%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(jit).

-export([
    stream/1,
    backend/2,
    beam_chunk_header/3,
    compile/9,
    decode_value64/1
]).

% NIFs
-export([
    stream_module/0,
    backend_module/0,
    variant/0
]).

-export_type([
    stream/0
]).

-export([
    small_integer_bounds/1,
    is_small_integer_range/3,
    can_inline_div_guarded/4
]).

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("default_atoms.hrl").
-include("opcodes.hrl").
-include("primitives.hrl").
-include("term.hrl").
-include("compact_term.hrl").

-ifdef(JIT_DWARF).
-compile({parse_transform, jit_dwarf_pt}).
-define(DWARF_LABEL(MMod, MSt, Label), MMod:dwarf_label(MSt, Label)).
-define(DWARF_FUNCTION(MMod, MSt, FunctionName, Arity),
    MMod:dwarf_function(MSt, (State0#state.atom_resolver)(FunctionName), Arity)
).
-define(DWARF_LINE(MMod, MSt, Line), MMod:dwarf_line(MSt, Line)).
-define(DWARF_VARIABLES(MMod, MSt, Vars), MMod:dwarf_variables(MSt, Vars)).
-else.
-define(DWARF_LABEL(_MMod, MSt, _Label), MSt).
-define(DWARF_FUNCTION(_MMod, MSt, _FunctionName, _Arity), MSt).
-define(DWARF_LINE(_MMod, MSt, _Line), MSt).
-define(DWARF_VARIABLES(_MMod, MSt, _Vars), MSt).
-endif.

-define(BOXED_FUN_SIZE, 3).
-define(FLOAT_SIZE_64, 2).
-define(FLOAT_SIZE_32, 3).

-define(INT32_MIN, -16#80000000).
-define(INT32_MAX, 16#7FFFFFFF).

-define(INT64_MIN, -16#8000000000000000).
-define(INT64_MAX, 16#7FFFFFFFFFFFFFFF).

-define(WAITING_TIMEOUT_EXPIRED, 2).

-define(BITSTRING_FLAG_LITTLE_ENDIAN, 16#2).
-define(BITSTRING_FLAG_SIGNED, 16#4).
-define(BITSTRING_FLAG_NATIVE_ENDIAN, 16#10).

-record(state, {
    line_offsets :: [{integer(), integer()}],
    current_line :: integer() | undefined,
    labels_count :: pos_integer(),
    atom_resolver :: fun((integer()) -> atom()),
    literal_resolver :: fun((integer()) -> any()),
    type_resolver :: fun((integer()) -> any()),
    import_resolver :: fun((integer()) -> {atom(), atom(), non_neg_integer()}),
    debug_info_resolver :: fun(
        (integer()) -> [{binary(), {x, integer()} | {y, integer()} | {value, any()}}] | false
    ),
    record_resolver ::
        fun(
            (atom()) ->
                #{
                    index := non_neg_integer(),
                    fields := [atom()],
                    is_exported := boolean()
                }
                | undefined
        ),
    %% Per-VM-register "this register holds a record of known type" tracking
    %% lives in the backend's `jit_regs' state, keyed by VM x/y reg. It is set
    %% by OP_IS_NATIVE_RECORD via `MMod:set_vm_record_type/3' and consumed by
    %% OP_GET_RECORD_FIELD/ELEMENTS and OP_IS_RECORD_ACCESSIBLE via
    %% `MMod:get_vm_record_type/2'. Invalidation is automatic: the backend
    %% drops the entry alongside its `regs' tracking on any write to the VM
    %% register, on C calls that clobber x regs, and at labels.
    tail_cache :: tail_cache()
}).

-type tail_cache() :: #{tuple() => non_neg_integer()} | disabled.
-type stream() :: any().

%%-define(TRACE(Fmt, Args), io:format(Fmt, Args)).
-define(TRACE(Fmt, Args), ok).

tail_cache_find(_Key, disabled) ->
    false;
tail_cache_find(Key, TC) ->
    case TC of
        #{Key := Value} -> {Key, Value};
        _ -> false
    end.

tail_cache_store(_Key, _Value, disabled) -> disabled;
tail_cache_store(Key, Value, TC) -> TC#{Key => Value}.

%%-define(ASSERT_ALL_NATIVE_FREE(St), MMod:assert_all_native_free(St)).
%%-define(ASSERT(Expr), true = Expr).
-define(ASSERT_ALL_NATIVE_FREE(St), ok).
-define(ASSERT(Expr), ok).

%%-----------------------------------------------------------------------------
%% @param   LabelsCount number of labels
%% @param   Arch code for the architecture
%% @param   Variant code for the JIT variant
%% @returns Beam chunk header
%% @doc     Create the beam chunk header for a single architecture/variant
%% @end
%%-----------------------------------------------------------------------------
beam_chunk_header(LabelsCount, Arch, Variant) ->
    Info = <<LabelsCount:32, ?JIT_FORMAT_VERSION:16, 1:16, Arch:16, Variant:16, 0:32>>,
    <<(byte_size(Info)):32, Info/binary>>.

%% Current variant supposes any entry point (labels or continuation pointer)
%% has the following signature
%% Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state, const ModuleNativeInterface *p)
compile(
    <<16:32, 0:32, OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, Opcodes/binary>>,
    AtomResolver,
    LiteralResolver,
    TypeResolver,
    ImportResolver,
    DebugInfoResolver,
    RecordResolver,
    MMod,
    MSt0
) when OpcodeMax =< ?OPCODE_MAX ->
    State0 = #state{
        line_offsets = [],
        current_line = undefined,
        labels_count = LabelsCount,
        atom_resolver = AtomResolver,
        literal_resolver = LiteralResolver,
        type_resolver = TypeResolver,
        import_resolver = ImportResolver,
        debug_info_resolver = DebugInfoResolver,
        record_resolver = RecordResolver,
        tail_cache =
            case erlang:function_exported(MMod, supports_tail_cache, 0) of
                true ->
                    case MMod:supports_tail_cache() of
                        true -> #{};
                        false -> disabled
                    end;
                false ->
                    #{}
            end
    },
    MSt1 = MMod:jump_table(MSt0, LabelsCount),
    {State1, MSt2} = first_pass(Opcodes, MMod, MSt1, State0),
    MSt3 = second_pass(MMod, MSt2, State1),
    MSt4 = MMod:flush(MSt3),
    {LabelsCount, MSt4};
compile(
    <<16:32, 0:32, OpcodeMax:32, _LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>>,
    _AtomResolver,
    _LiteralResolver,
    _TypeResolver,
    _ImportResolver,
    _DebugInfoResolver,
    _RecordResolver,
    _MMod,
    _MSt
) ->
    error(badarg, [OpcodeMax]);
compile(
    CodeChunk,
    _AtomResolver,
    _LiteralResolver,
    _TypeResolver,
    _ImportResolver,
    _DebugInfoResolver,
    _RecordResolver,
    _MMod,
    _MSt
) ->
    error(badarg, [CodeChunk]).

% 1
first_pass(
    <<?OP_LABEL, Rest0/binary>>, MMod, MSt0, State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_LABEL ~p\n", [Label]),
    MSt1 = ?DWARF_LABEL(MMod, MSt0, Label),
    MSt2 = MMod:add_label(MSt1, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    %% Record-type tracking lives in the backend's jit_regs state; the
    %% add_label above already cleared it alongside other per-register info.
    first_pass(Rest1, MMod, MSt2, State0);
% 2
first_pass(<<?OP_FUNC_INFO, Rest0/binary>>, MMod, MSt0, #state{tail_cache = TC} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_ModuleAtomIndex, Rest1} = decode_atom(Rest0),
    {FunctionAtomIndex, Rest2} = decode_atom(Rest1),
    {Arity, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_FUNC_INFO ~p, ~p, ~p\n", [_ModuleAtomIndex, FunctionAtomIndex, Arity]),
    Offset = MMod:offset(MSt0),
    {MSt1, OffsetReg} = MMod:move_to_native_register(MSt0, Offset),
    {MSt2, FunctionAtomIndexReg} = MMod:move_to_native_register(MSt1, FunctionAtomIndex),
    {MSt3, ArityReg} = MMod:move_to_native_register(MSt2, Arity),
    TailCacheKey =
        {call_primitive_last, ?PRIM_RAISE_ERROR_MFA, [OffsetReg, FunctionAtomIndexReg, ArityReg]},
    {MSt4, State1} =
        case tail_cache_find(TailCacheKey, TC) of
            false ->
                CacheOffset = MMod:offset(MSt3),
                MSt4a = MMod:call_primitive_last(MSt3, ?PRIM_RAISE_ERROR_MFA, [
                    ctx,
                    jit_state,
                    {free, OffsetReg},
                    {free, FunctionAtomIndexReg},
                    {free, ArityReg}
                ]),
                {MSt4a, State0#state{tail_cache = tail_cache_store(TailCacheKey, CacheOffset, TC)}};
            {TailCacheKey, CacheOffset} ->
                MSt4a = MMod:jump_to_offset(MSt3, CacheOffset),
                MSt4b = MMod:free_native_registers(MSt4a, [
                    OffsetReg, FunctionAtomIndexReg, ArityReg
                ]),
                {MSt4b, State0}
        end,
    MSt5 = ?DWARF_FUNCTION(MMod, MSt4, FunctionAtomIndex, Arity),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State1);
% 3
first_pass(
    <<?OP_INT_CALL_END>>, MMod, MSt0, #state{labels_count = LabelsCount} = State
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_INT_CALL_END\n", []),
    MSt1 = MMod:add_label(MSt0, LabelsCount),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_TERMINATE_CONTEXT, [
        ctx, jit_state
    ]),
    {State, MSt2};
% 4
first_pass(<<?OP_CALL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CALL ~p, ~p\n", [_Arity, Label]),
    MSt1 = MMod:call_or_schedule_next(MSt0, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State0);
% 5
first_pass(<<?OP_CALL_LAST, Rest0/binary>>, MMod, MSt0, #state{tail_cache = TC} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    {NWords, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_CALL_LAST ~p, ~p, ~p\n", [_Arity, Label, NWords]),
    TailCacheKey0 = {op_call_last, NWords, Label},
    case tail_cache_find(TailCacheKey0, TC) of
        false ->
            Offset0 = MMod:offset(MSt0),
            MSt1 = MMod:move_to_cp(MSt0, {y_reg, NWords}),
            MSt2 = MMod:increment_sp(MSt1, NWords + 1),
            TailCacheKey1 = {op_call_only, Label},
            case tail_cache_find(TailCacheKey1, TC) of
                false ->
                    Offset1 = MMod:offset(MSt2),
                    MSt3 = MMod:call_only_or_schedule_next(MSt2, Label),
                    State1 = State0#state{
                        tail_cache = tail_cache_store(
                            TailCacheKey1, Offset1, tail_cache_store(TailCacheKey0, Offset0, TC)
                        )
                    };
                {TailCacheKey1, Offset1} ->
                    MSt3 = MMod:jump_to_offset(MSt2, Offset1),
                    State1 = State0#state{
                        tail_cache = tail_cache_store(TailCacheKey0, Offset0, TC)
                    }
            end;
        {TailCacheKey0, Offset0} ->
            MSt3 = MMod:jump_to_offset(MSt0, Offset0),
            State1 = State0
    end,
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest3, MMod, MSt3, State1);
% 6
first_pass(<<?OP_CALL_ONLY, Rest0/binary>>, MMod, MSt0, #state{tail_cache = TC} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Arity, Rest1} = decode_literal(Rest0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CALL_ONLY ~p, ~p\n", [_Arity, Label]),
    TailCacheKey = {op_call_only, Label},
    case tail_cache_find(TailCacheKey, TC) of
        false ->
            Offset = MMod:offset(MSt0),
            MSt1 = MMod:call_only_or_schedule_next(MSt0, Label),
            State1 = State0#state{tail_cache = tail_cache_store(TailCacheKey, Offset, TC)};
        {TailCacheKey, Offset} ->
            MSt1 = MMod:jump_to_offset(MSt0, Offset),
            State1 = State0
    end,
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State1);
% 7
first_pass(<<?OP_CALL_EXT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_EXT ~p, ~p\n", [Arity, Index]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    State1 = record_continuation_line(MMod, MSt1, State0),
    MSt2 = MMod:call_primitive_with_cp(MSt1, ?PRIM_CALL_EXT, [
        ctx, jit_state, offset, Arity, Index, ?CALL_EXT_NO_DEALLOC_MFA
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State1);
% 8
first_pass(<<?OP_CALL_EXT_LAST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    {NWords, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_CALL_EXT_LAST ~p, ~p, ~p\n", [Arity, Index, NWords]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    State1 = record_continuation_line(MMod, MSt1, State0),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_CALL_EXT, [
        ctx, jit_state, offset, Arity, Index, NWords
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest3, MMod, MSt2, State1);
% 9
first_pass(<<?OP_BIF0, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Bif, Rest1} = decode_literal(Rest0),
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_BIF0 ~p, ~p\n", [Bif, Dest]),
    {MSt3, ResultReg} = MMod:call_func_ptr(MSt2, {free, FuncPtr}, [
        ctx
    ]),
    MSt4 = MMod:move_to_vm_register(MSt3, ResultReg, Dest),
    MSt5 = MMod:free_native_registers(MSt4, [Dest, ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest2, MMod, MSt5, State0);
% 10
first_pass(<<?OP_BIF1, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Bif, Rest2} = decode_literal(Rest1),
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, Arg, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    {MSt3, Dest, Rest4} = decode_dest(Rest3, MMod, MSt2),
    ?TRACE("OP_BIF1 ~p, ~p, ~p, ~p\n", [FailLabel, Bif, Arg, Dest]),
    {MSt4, ResultReg} = MMod:call_func_ptr(MSt3, {free, FuncPtr}, [
        ctx, FailLabel, {free, Arg}
    ]),
    MSt5 = bif_faillabel_test(FailLabel, MMod, MSt4, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 11
first_pass(
    <<?OP_BIF2, Rest0/binary>>, MMod, MSt0, #state{import_resolver = ImportResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Bif, Rest2} = decode_literal(Rest1),
    {BifModule, BifFunName, 2} = ImportResolver(Bif),
    {MSt1, Arg1, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, Arg2, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt1, State0),
    {MSt3, Dest, Rest5} = decode_dest(Rest4, MMod, MSt2),
    ?TRACE("OP_BIF2 ~p, ~p (~p:~p/2), ~p, ~p, ~p\n", [
        FailLabel, Bif, BifModule, BifFunName, Arg1, Arg2, Dest
    ]),
    MSt4 = op_bif2(MMod, MSt3, FailLabel, BifModule, BifFunName, Bif, Arg1, Arg2, Dest),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest5, MMod, MSt4, State0);
% 12
first_pass(<<?OP_ALLOCATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {StackNeed, Rest1} = decode_literal(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_ALLOCATE ~p, ~p\n", [StackNeed, Live]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_ALLOCATE, [
        ctx, jit_state, StackNeed, 0, Live
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 13
first_pass(<<?OP_ALLOCATE_HEAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {StackNeed, Rest1} = decode_literal(Rest0),
    {HeapNeed, Rest2} = decode_allocator_list(MMod, Rest1),
    {Live, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_ALLOCATE_HEAP ~p, ~p, ~p\n", [StackNeed, HeapNeed, Live]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_ALLOCATE, [
        ctx, jit_state, StackNeed, HeapNeed, Live
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest3, MMod, MSt2, State0);
% 16
first_pass(<<?OP_TEST_HEAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {HeapNeed, Rest1} = decode_allocator_list(MMod, Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_TEST_HEAP ~p, ~p\n", [HeapNeed, Live]),
    MSt2 = op_test_heap(MMod, MSt0, HeapNeed, Live),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 18
first_pass(<<?OP_DEALLOCATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {NWords, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_DEALLOCATE ~p\n", [NWords]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_DEALLOCATE, [
        ctx, jit_state, NWords
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 19
first_pass(<<?OP_RETURN, Rest/binary>>, MMod, MSt0, #state{tail_cache = TC} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_RETURN\n", []),
    % Optimized return: check if returning within same module
    {MSt1, CpReg0} = MMod:move_to_native_register(MSt0, cp),
    {MSt2, ModuleIndexReg} = MMod:get_module_index(MSt1),
    % Extract module index from cp (upper 8 bits: cp >> 24)
    {MSt3, CpReg1} = MMod:shift_right(MSt2, CpReg0, 24),
    % Compare extracted module index with current module index
    MSt4 = MMod:if_block(
        MSt3,
        {{free, CpReg1}, '==', {free, ModuleIndexReg}},
        % Same module: fast intra-module return
        fun(BSt0) ->
            % Mask to get lower 24 bits and shift right by 2 for offset
            {BSt1, CpReg0} = MMod:and_(BSt0, {free, CpReg0}, 16#FFFFFF),
            {BSt3, CPReg1} = MMod:shift_right(BSt1, {free, CpReg0}, 2),
            % Jump to continuation (this is a tail call)
            MMod:jump_to_continuation(BSt3, {free, CPReg1})
        end
    ),
    MSt5 = MMod:free_native_registers(MSt4, [CpReg0]),
    % Different module: use existing slow path
    TailCacheKey = {call_primitive_last, ?PRIM_RETURN},
    case tail_cache_find(TailCacheKey, TC) of
        false ->
            Offset = MMod:offset(MSt5),
            MSt6 = MMod:call_primitive_last(MSt5, ?PRIM_RETURN, [ctx, jit_state]),
            State1 = State0#state{tail_cache = tail_cache_store(TailCacheKey, Offset, TC)};
        {TailCacheKey, Offset} ->
            MSt6 = MMod:jump_to_offset(MSt5, Offset),
            State1 = State0
    end,
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest, MMod, MSt6, State1);
% 20
first_pass(<<?OP_SEND, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_SEND\n", []),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_SEND, [
        ctx, jit_state
    ]),
    MSt2 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest, MMod, MSt2, State0);
% 21
first_pass(<<?OP_REMOVE_MESSAGE, Rest/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_REMOVE_MESSAGE\n", []),
    {MSt1, Reg1} = MMod:call_primitive(MSt0, ?PRIM_CANCEL_TIMEOUT, [
        ctx
    ]),
    MSt2 = MMod:free_native_registers(MSt1, [Reg1]),
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt4 = MMod:return_if_not_equal_to_ctx(MSt3, {free, ResultReg}),
    {MSt5, Reg2} = MMod:call_primitive(MSt4, ?PRIM_MAILBOX_REMOVE_MESSAGE, [
        ctx
    ]),
    MSt6 = MMod:free_native_registers(MSt5, [Reg2]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest, MMod, MSt6, State0);
% 22
first_pass(<<?OP_TIMEOUT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_TIMEOUT\n", []),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TIMEOUT, [
        ctx
    ]),
    MSt2 = MMod:free_native_registers(MSt1, [ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest0, MMod, MSt2, State0);
% 23
first_pass(<<?OP_LOOP_REC, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt2 = MMod:return_if_not_equal_to_ctx(MSt1, {free, ResultReg}),
    {MSt3, Dest, Rest2} = decode_compact_term(Rest1, MMod, MSt2, State0),
    ?TRACE("OP_LOOP_REC ~p, ~p\n", [Label, Dest]),
    {MSt4, PeekResult} = MMod:call_primitive(MSt3, ?PRIM_MAILBOX_PEEK, [ctx]),
    MSt5 = cond_jump_to_label({PeekResult, '==', 0}, Label, MMod, MSt4),
    MSt6 = MMod:move_to_vm_register(MSt5, PeekResult, Dest),
    MSt7 = MMod:free_native_registers(MSt6, [PeekResult, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest2, MMod, MSt7, State0);
% 24
first_pass(<<?OP_LOOP_REC_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_LOOP_REC_END ~p\n", [Label]),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt2 = MMod:return_if_not_equal_to_ctx(MSt1, {free, ResultReg}),
    {MSt3, Reg1} = MMod:call_primitive(MSt2, ?PRIM_MAILBOX_NEXT, [
        ctx
    ]),
    MSt4 = MMod:free_native_registers(MSt3, [Reg1]),
    MSt5 = MMod:jump_to_label(MSt4, Label),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest1, MMod, MSt5, State0);
% 25
first_pass(<<?OP_WAIT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_WAIT ~p\n", [Label]),
    MSt1 = MMod:set_continuation_to_label(MSt0, Label),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 26
first_pass(<<?OP_WAIT_TIMEOUT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, OffsetRef0} = MMod:set_continuation_to_offset(MSt0),
    {MSt2, Timeout, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_WAIT_TIMEOUT ~p, ~p\n", [Label, Timeout]),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_WAIT_TIMEOUT, [
        ctx, jit_state, {free, Timeout}, Label
    ]),
    MSt4 = MMod:add_label(MSt3, OffsetRef0),
    MSt5 = MMod:continuation_entry_point(MSt4),
    {MSt6, ResultReg0} = MMod:call_primitive(MSt5, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    MSt7 = MMod:return_if_not_equal_to_ctx(MSt6, {free, ResultReg0}),
    {MSt8, ResultReg1} = MMod:call_primitive(MSt7, ?PRIM_CONTEXT_GET_FLAGS, [
        ctx, ?WAITING_TIMEOUT_EXPIRED
    ]),
    MSt9 = MMod:if_block(MSt8, {{free, ResultReg1}, '==', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_WAIT_TIMEOUT_TRAP_HANDLER, [
            ctx, jit_state, Label
        ])
    end),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest2, MMod, MSt9, State0);
% 39
first_pass(<<?OP_IS_LT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_LT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 = op_is_lt(MMod, MSt2, Label, Arg1, Arg2),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 40
first_pass(<<?OP_IS_GE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_GE ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 = op_is_ge(MMod, MSt2, Label, Arg1, Arg2),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 41
first_pass(<<?OP_IS_EQUAL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_EQUAL ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 = op_is_equal(MMod, MSt2, Label, Arg1, Arg2),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 42
first_pass(<<?OP_IS_NOT_EQUAL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_NOT_EQUAL ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 = op_is_not_equal(MMod, MSt2, Label, Arg1, Arg2),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 43
first_pass(<<?OP_IS_EQ_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_EQ_EXACT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 = op_is_eq_exact(MMod, MSt2, Label, Arg1, Arg2),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 44
first_pass(<<?OP_IS_NOT_EQ_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Arg2, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_NOT_EQ_EXACT ~p, ~p, ~p\n", [Label, Arg1, Arg2]),
    MSt5 = op_is_not_eq_exact(MMod, MSt2, Label, Arg1, Arg2),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest3, MMod, MSt5, State0);
% 45
first_pass(<<?OP_IS_INTEGER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_INTEGER ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_any_integer({free, Arg1}, Label, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 46
first_pass(<<?OP_IS_FLOAT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_FLOAT ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_FLOAT, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 47
first_pass(<<?OP_IS_NUMBER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NUMBER ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_immediate_or_boxed(
        {free, Arg1},
        ?TERM_INTEGER_TAG,
        ?TERM_BOXED_TAG_MASK_INTEGER_OR_FLOAT,
        ?TERM_BOXED_TAG_POSITIVE_INTEGER_OR_FLOAT,
        Label,
        MMod,
        MSt1
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 48
first_pass(<<?OP_IS_ATOM, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_ATOM ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {{free, Reg}, '&', ?TERM_IMMED2_TAG_MASK, '!=', ?TERM_IMMED2_ATOM},
        Label,
        MMod,
        MSt2
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 49
first_pass(<<?OP_IS_PID, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_PID ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_immediate_or_boxed(
        {free, Arg1}, ?TERM_PID_TAG, ?TERM_BOXED_EXTERNAL_PID, Label, MMod, MSt1
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 50
first_pass(<<?OP_IS_REFERENCE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_REFERENCE ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    {MSt6, Reg} = MMod:and_(MSt5, {free, Reg}, ?TERM_BOXED_TAG_MASK),
    MSt7 = cond_jump_to_label(
        {'and', [{Reg, '!=', ?TERM_BOXED_REF}, {Reg, '!=', ?TERM_BOXED_EXTERNAL_REF}]},
        Label,
        MMod,
        MSt6
    ),
    MSt8 = MMod:free_native_registers(MSt7, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State0);
% 51
first_pass(<<?OP_IS_PORT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_PORT ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_immediate_or_boxed(
        {free, Arg1}, ?TERM_PORT_TAG, ?TERM_BOXED_EXTERNAL_PORT, Label, MMod, MSt1
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 52
first_pass(<<?OP_IS_NIL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NIL ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label({Reg, '!=', ?TERM_NIL}, Label, MMod, MSt2),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 53
first_pass(<<?OP_IS_BINARY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BINARY ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_binary(Arg1, Label, MMod, MSt1),
    MSt3 = MMod:free_native_registers(MSt2, [Arg1]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 55
first_pass(<<?OP_IS_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_LIST ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {'and', [
            {Reg, '!=', ?TERM_NIL},
            {{free, Reg}, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_LIST}
        ]},
        Label,
        MMod,
        MSt2
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 56
first_pass(<<?OP_IS_NONEMPTY_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_NONEMPTY_LIST ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {{free, Reg}, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_LIST}, Label, MMod, MSt2
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 57
first_pass(<<?OP_IS_TUPLE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_TUPLE ~p, ~p\n", [Label, Arg1]),
    case try_fuse_tuple_ops(Rest2, Arg1, Label, MMod, MSt1, State0) of
        {fused, MStFused, RestFused} ->
            first_pass(RestFused, MMod, MStFused, State0);
        not_fused ->
            MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_TUPLE, MMod, MSt1),
            ?ASSERT_ALL_NATIVE_FREE(MSt2),
            first_pass(Rest2, MMod, MSt2, State0)
    end;
% 58
first_pass(<<?OP_TEST_ARITY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Arity, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_TEST_ARITY ~p, ~p, ~p\n", [Label, Arg1, Arity]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    {MSt3, Reg} = MMod:and_(MSt2, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    {MSt5, ArityReg} = MMod:shift_right(MSt4, {free, Reg}, 6),
    MSt6 = cond_jump_to_label({{free, ArityReg}, '!=', Arity}, Label, MMod, MSt5),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 59
first_pass(<<?OP_SELECT_VAL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_typed_compact_term(Rest0, MMod, MSt0, State0),
    {DefaultLabel, Rest2} = decode_label(Rest1),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_SELECT_VAL ~p, ~p", [SrcValue, DefaultLabel]),
    %% Load SrcValue once into a native register so we can reuse it across
    %% all comparisons (only valid when we use the inline cmp; the primitive
    %% path frees its args).
    {MSt2, Rest4} = op_select_val_loop(
        MMod, MSt1, SrcValue, Rest3, ListSize div 2, State0
    ),
    ?TRACE("\n", []),
    MSt3 = MMod:jump_to_label(MSt2, DefaultLabel),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 60
first_pass(<<?OP_SELECT_TUPLE_ARITY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {DefaultLabel, Rest2} = decode_label(Rest1),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_SELECT_TUPLE_ARITY ~p, ~p", [SrcValue, DefaultLabel]),
    {MSt2, Reg} = term_get_tuple_arity({free, SrcValue}, MMod, MSt1),
    {MSt3, Rest4} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {CmpValue, AccRest1} = decode_literal(AccRest0),
            {JmpLabel, AccRest2} = decode_label(AccRest1),
            ?TRACE(", ~p => ~p", [CmpValue, JmpLabel]),
            AccMSt1 = cond_jump_to_label({Reg, '==', CmpValue}, JmpLabel, MMod, AccMSt0),
            {AccMSt1, AccRest2}
        end,
        {MSt2, Rest3},
        lists:seq(0, (ListSize div 2) - 1)
    ),
    ?TRACE("\n", []),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    MSt5 = MMod:jump_to_label(MSt4, DefaultLabel),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 61
first_pass(<<?OP_JUMP, Rest0/binary>>, MMod, MSt0, #state{tail_cache = TC} = State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    ?TRACE("OP_JUMP ~p\n", [Label]),
    TailCacheKey = {op_call_only, Label},
    case tail_cache_find(TailCacheKey, TC) of
        false ->
            Offset = MMod:offset(MSt0),
            MSt1 = MMod:call_only_or_schedule_next(MSt0, Label),
            ?ASSERT_ALL_NATIVE_FREE(MSt1),
            first_pass(Rest1, MMod, MSt1, State0#state{
                tail_cache = tail_cache_store(TailCacheKey, Offset, TC)
            });
        {TailCacheKey, Offset} ->
            MSt1 = MMod:jump_to_offset(MSt0, Offset),
            ?ASSERT_ALL_NATIVE_FREE(MSt1),
            first_pass(Rest1, MMod, MSt1, State0)
    end;
% 62
% Same implementation as OP_TRY, to confirm.
first_pass(<<?OP_CATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_CATCH ~p, ~p\n", [Dest, Label]),
    MSt2 = term_from_catch_label(Dest, Label, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 63
first_pass(<<?OP_CATCH_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_CATCH_END ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_CATCH_END, [ctx, jit_state]),
    MSt5 = handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt4),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest1, MMod, MSt5, State0);
% 64
first_pass(<<?OP_MOVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_MOVE ~p, ~p\n", [Source, Dest]),
    MSt3 = MMod:move_to_vm_register(MSt2, Source, Dest),
    MSt4 = MMod:free_native_registers(MSt3, [Source, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 65
first_pass(<<?OP_GET_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, List, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, HeadDest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    {MSt3, TailDest, Rest3} = decode_dest(Rest2, MMod, MSt2),
    ?TRACE("OP_GET_LIST ~p, ~p, ~p\n", [List, HeadDest, TailDest]),
    {MSt4, Reg} = MMod:move_to_native_register(MSt3, List),
    {MSt5, Reg} = MMod:and_(MSt4, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt6 = MMod:move_array_element(MSt5, Reg, ?LIST_HEAD_INDEX, HeadDest),
    MSt7 = MMod:free_native_registers(MSt6, [HeadDest]),
    MSt8 = MMod:move_array_element(MSt7, Reg, ?LIST_TAIL_INDEX, TailDest),
    MSt9 = MMod:free_native_registers(MSt8, [Reg]),
    MSt10 = MMod:free_native_registers(MSt9, [TailDest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt10),
    first_pass(Rest3, MMod, MSt10, State0);
% 66
first_pass(<<?OP_GET_TUPLE_ELEMENT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {Element, Rest2} = decode_literal(Rest1),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    ?TRACE("OP_GET_TUPLE_ELEMENT ~p, ~p, ~p\n", [Source, Element, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, Source),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, Element + 1, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Reg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 67
first_pass(<<?OP_SET_TUPLE_ELEMENT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, NewElement, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Tuple, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    {Position, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_SET_TUPLE_ELEMENT ~p, ~p, ~p\n", [NewElement, Tuple, Position]),
    %% Done in C so that the generational GC write barrier is applied: the
    %% destructive update may store a young pointer into a promoted tuple.
    {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_SET_TUPLE_ELEMENT, [
        ctx, {free, Tuple}, Position, {free, NewElement}
    ]),
    MSt4 = MMod:free_native_registers(MSt3, [ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest3, MMod, MSt4, State0);
% 69
first_pass(<<?OP_PUT_LIST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Head, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Tail, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    {MSt3, Dest, Rest3} = decode_dest(Rest2, MMod, MSt2),
    ?TRACE("OP_PUT_LIST ~p, ~p, ~p\n", [Head, Tail, Dest]),
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_PUT_LIST, [
        ctx, {free, Head}, {free, Tail}
    ]),
    MSt5 = MMod:move_to_vm_register(MSt4, ResultReg, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [ResultReg]),
    MSt7 = MMod:free_native_registers(MSt6, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest3, MMod, MSt7, State0);
% 72
first_pass(<<?OP_BADMATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_BADMATCH ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADMATCH_ATOM, {free, Arg1}
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 73
first_pass(<<?OP_IF_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_IF_END\n", []),
    MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
        ctx, jit_state, offset, ?IF_CLAUSE_ATOM
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest0, MMod, MSt1, State0);
% 74
first_pass(<<?OP_CASE_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_CASE_END ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?CASE_CLAUSE_ATOM, {free, Arg1}
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 75
first_pass(<<?OP_CALL_FUN, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {ArgsCount, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_CALL_FUN ~p\n", [ArgsCount]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    State1a = record_continuation_line(MMod, MSt1, State0),
    {MSt2, FuncReg} = read_any_xreg(ArgsCount, MMod, MSt1),
    {MSt3, Reg} = verify_is_function(FuncReg, MMod, MSt2),
    MSt4 = MMod:call_primitive_with_cp(MSt3, ?PRIM_CALL_FUN, [
        ctx, jit_state, offset, {free, Reg}, ArgsCount
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest1, MMod, MSt4, State1a);
% 77
first_pass(<<?OP_IS_FUNCTION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_FUNCTION ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_FUN, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 78
first_pass(<<?OP_CALL_EXT_ONLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {Index, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_EXT_ONLY ~p, ~p\n", [Arity, Index]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    State1 = record_continuation_line(MMod, MSt1, State0),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_CALL_EXT, [
        ctx, jit_state, offset, Arity, Index, ?CALL_EXT_NO_DEALLOC
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State1);
% 96
first_pass(<<?OP_FMOVE, ?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FPRegIndex, Rest1} = decode_literal(Rest0),
    {MSt1, Dest, Rest2} = decode_dest(Rest1, MMod, MSt0),
    ?TRACE("OP_FMOVE {fp_reg, ~p}, ~p\n", [FPRegIndex, Dest]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_FROM_FLOAT, [ctx, FPRegIndex]),
    MSt3 = MMod:move_to_vm_register(MSt2, ResultReg, Dest),
    MSt4 = MMod:free_native_registers(MSt3, [ResultReg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
first_pass(
    <<?OP_FMOVE, ?COMPACT_EXTENDED_LITERAL, Rest0/binary>>,
    MMod,
    MSt0,
    #state{literal_resolver = LiteralResolver} = State0
) ->
    %% fmove of a float literal into an fp register. The float value is known
    %% at compile time, so on FPU backends store its IEEE-754 bits directly
    %% into fr[N] instead of loading the boxed literal term through
    %% PRIM_MODULE_LOAD_LITERAL, which re-parses the literal's external term
    %% format and allocates a heap fragment on every execution (and the
    %% fragment forces a GC at the next heap allocation).
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {LiteralIndex, Rest1} = decode_literal(Rest0),
    {{fp_reg, FPRegIndex} = FPReg, Rest2} = decode_fp_register(Rest1),
    ?TRACE("OP_FMOVE {literal, ~p}, ~p\n", [LiteralIndex, FPReg]),
    Float = LiteralResolver(LiteralIndex),
    MSt4 =
        case
            is_float(Float) andalso MMod:supports_fp(MSt0) andalso
                erlang:function_exported(MMod, move_float_to_fp_reg, 3)
        of
            true ->
                MSt1 = ensure_fpregs(MMod, MSt0),
                MMod:move_float_to_fp_reg(MSt1, Float, FPRegIndex);
            false ->
                {MSt1, SrcValue, Rest2} = decode_compact_term_module_literal(
                    LiteralIndex, MMod, MSt0, Rest2
                ),
                {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
                MSt3 = MMod:free_native_registers(MSt2, [ResultReg]),
                {MSt3b, Reg} = MMod:move_to_native_register(MSt3, SrcValue),
                {MSt3c, Reg} = MMod:and_(MSt3b, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
                MMod:move_to_vm_register(MSt3c, {free, {ptr, Reg, 1}}, FPReg)
        end,
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
first_pass(<<?OP_FMOVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {FPReg, Rest2} = decode_fp_register(Rest1),
    ?TRACE("OP_FMOVE ~p, ~p\n", [SrcValue, FPReg]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
    MSt3 = MMod:free_native_registers(MSt2, [ResultReg]),
    {MSt4, Reg} = MMod:move_to_native_register(MSt3, SrcValue),
    {MSt5, Reg} = MMod:and_(MSt4, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt6 = MMod:move_to_vm_register(MSt5, {free, {ptr, Reg, 1}}, FPReg),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest2, MMod, MSt6, State0);
% 97
first_pass(<<?OP_FCONV, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_typed_compact_term(Rest0, MMod, MSt0, State0),
    {{fp_reg, FPRegIndex}, Rest2} = decode_fp_register(Rest1),
    ?TRACE("OP_FCONV ~p, ~p\n", [SrcValue, {fp_reg, FPRegIndex}]),
    MSt2 = op_fconv(MMod, MSt1, SrcValue, FPRegIndex),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 98
first_pass(<<?OP_FADD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FADD, Rest0, MMod, MSt0, State0);
% 99
first_pass(<<?OP_FSUB, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FSUB, Rest0, MMod, MSt0, State0);
% 100
first_pass(<<?OP_FMUL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FMUL, Rest0, MMod, MSt0, State0);
% 101
first_pass(<<?OP_FDIV, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    first_pass_float3(?PRIM_FDIV, Rest0, MMod, MSt0, State0);
% 102
first_pass(<<?OP_FNEGATE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Label, Rest1} = decode_label(Rest0),
    {{fp_reg, FPRegIndex1}, Rest2} = decode_fp_register(Rest1),
    {{fp_reg, FPRegIndex2}, Rest3} = decode_fp_register(Rest2),
    ?TRACE("OP_FNEGATE ~p, ~p, ~p\n", [_Label, {fp_reg, FPRegIndex1}, {fp_reg, FPRegIndex2}]),
    {MSt1, Reg} = MMod:call_primitive(MSt0, ?PRIM_FNEGATE, [
        ctx, FPRegIndex1, FPRegIndex2
    ]),
    MSt2 = MMod:free_native_registers(MSt1, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest3, MMod, MSt2, State0);
% 104
first_pass(<<?OP_TRY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {Label, Rest2} = decode_label(Rest1),
    ?TRACE("OP_TRY ~p, ~p\n", [Dest, Label]),
    MSt2 = term_from_catch_label(Dest, Label, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 105
first_pass(<<?OP_TRY_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_TRY_END ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest1, MMod, MSt3, State0);
% 106
first_pass(<<?OP_TRY_CASE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_TRY_CASE ~p\n", [Dest]),
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_TRY_CASE, [ctx]),
    MSt5 = MMod:free_native_registers(MSt4, [ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest1, MMod, MSt5, State0);
% 107
first_pass(<<?OP_TRY_CASE_END, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_TRY_CASE_END ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?TRY_CLAUSE_ATOM, Arg1
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 108
first_pass(<<?OP_RAISE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Stacktrace, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, ExcValue, Rest2} = decode_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_RAISE ~p, ~p\n", [Stacktrace, ExcValue]),
    MSt3 = MMod:call_primitive_last(MSt2, ?PRIM_RAISE, [
        ctx, jit_state, Stacktrace, ExcValue
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 112
first_pass(<<?OP_APPLY, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_APPLY ~p\n", [Arity]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    State1a = record_continuation_line(MMod, MSt1, State0),
    {MSt2, Module} = read_any_xreg(Arity, MMod, MSt1),
    {MSt3, Function} = read_any_xreg(Arity + 1, MMod, MSt2),
    MSt4 = verify_is_atom(Module, 0, MMod, MSt3),
    MSt5 = verify_is_atom(Function, 0, MMod, MSt4),
    MSt6 = MMod:call_primitive_with_cp(MSt5, ?PRIM_APPLY, [
        ctx, jit_state, offset, {free, Module}, {free, Function}, Arity
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest1, MMod, MSt6, State1a);
% 113
first_pass(<<?OP_APPLY_LAST, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Arity, Rest1} = decode_literal(Rest0),
    {NWords, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_APPLY_LAST ~p, ~p\n", [Arity, NWords]),
    MSt1 = MMod:decrement_reductions_and_maybe_schedule_next(MSt0),
    State1 = record_continuation_line(MMod, MSt1, State0),
    {MSt2, Module} = read_any_xreg(Arity, MMod, MSt1),
    {MSt3, Function} = read_any_xreg(Arity + 1, MMod, MSt2),
    MSt4 = verify_is_atom(Module, 0, MMod, MSt3),
    MSt5 = verify_is_atom(Function, 0, MMod, MSt4),
    MSt6 = MMod:move_to_cp(MSt5, {y_reg, NWords}),
    MSt7 = MMod:increment_sp(MSt6, NWords + 1),
    MSt8 = MMod:call_primitive_last(MSt7, ?PRIM_APPLY, [
        ctx, jit_state, offset, {free, Module}, {free, Function}, Arity
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State1);
% 114
first_pass(<<?OP_IS_BOOLEAN, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BOOLEAN ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {'and', [{Reg, '!=', ?TRUE_ATOM}, {Reg, '!=', ?FALSE_ATOM}]}, Label, MMod, MSt2
    ),
    MSt4 = MMod:free_native_registers(MSt3, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest2, MMod, MSt4, State0);
% 115
first_pass(<<?OP_IS_FUNCTION2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, ArityTerm, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    ?TRACE("OP_IS_FUNCTION2 ~p,~p,~p\n", [Label, Arg1, ArityTerm]),
    {MSt3, FuncPtr} = term_is_boxed_with_tag_and_get_ptr(Label, Arg1, ?TERM_BOXED_FUN, MMod, MSt2),
    {MSt4, Arity} = term_to_int(ArityTerm, Label, MMod, MSt3),
    {MSt5, ModuleReg} = MMod:get_array_element(MSt4, FuncPtr, 1),
    {MSt6, IndexOrModuleReg} = MMod:get_array_element(MSt5, FuncPtr, 2),
    MSt7 = MMod:if_else_block(
        MSt6,
        {IndexOrModuleReg, '&', ?TERM_IMMED2_TAG_MASK, '!=', ?TERM_IMMED2_ATOM},
        fun(BSt0) ->
            {BSt1, IndexReg} = MMod:shift_right(BSt0, {free, IndexOrModuleReg}, 4),
            {BSt2, FunArity} = MMod:call_primitive(BSt1, ?PRIM_MODULE_GET_FUN_ARITY, [
                ModuleReg, {free, IndexReg}
            ]),
            cond_jump_to_label({'(int)', {free, FunArity}, '!=', Arity}, Label, MMod, BSt2)
        end,
        fun(BSt0) ->
            BSt1 = MMod:free_native_registers(BSt0, [IndexOrModuleReg]),
            {BSt2, FunArity} = MMod:get_array_element(BSt1, FuncPtr, 3),
            {BSt3, FunArityReg} = MMod:shift_right(BSt2, {free, FunArity}, 4),
            cond_jump_to_label({'(int)', {free, FunArityReg}, '!=', Arity}, Label, MMod, BSt3)
        end
    ),
    MSt8 = MMod:free_native_registers(MSt7, [FuncPtr, ModuleReg, Arity]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest3, MMod, MSt8, State0);
% 117
first_pass(<<?OP_BS_GET_INTEGER2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {_Live, Rest3} = decode_literal(Rest2),
    {MSt2, Size, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt1, State0),
    {Unit, Rest5} = decode_literal(Rest4),
    {FlagsValue, Rest6} = decode_literal(Rest5),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, SizeReg} = term_to_int(Size, Fail, MMod, MSt3),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt4, SizeReg * Unit};
            true ->
                MSt5 = MMod:mul(MSt4, SizeReg, Unit),
                {MSt5, SizeReg}
        end,
    {MSt7, BSBinaryReg} = MMod:get_array_element(MSt6, MatchStateRegPtr, 1),
    {MSt8, BSOffsetReg} = MMod:get_array_element(MSt7, MatchStateRegPtr, 2),
    {MSt9, Result} = MMod:call_primitive(MSt8, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, BSBinaryReg}, BSOffsetReg, NumBits, {free, FlagsValue}
    ]),
    MSt10 = handle_error_if({Result, '==', 0}, MMod, MSt9),
    MSt11 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt10),
    MSt12 = MMod:add(MSt11, BSOffsetReg, NumBits),
    MSt13 = MMod:free_native_registers(MSt12, [NumBits]),
    MSt14 = MMod:move_to_array_element(MSt13, BSOffsetReg, MatchStateRegPtr, 2),
    MSt15 = MMod:free_native_registers(MSt14, [BSOffsetReg, MatchStateRegPtr]),
    {MSt16, Dest, Rest7} = decode_dest(Rest6, MMod, MSt15),
    ?TRACE("OP_BS_GET_INTEGER2 ~p,~p,~p,~p,~p,~p,~p\n", [
        Fail, Src, _Live, Size, Unit, FlagsValue, Dest
    ]),
    MSt17 = MMod:move_to_vm_register(MSt16, Result, Dest),
    MSt18 = MMod:free_native_registers(MSt17, [Result]),
    ?ASSERT_ALL_NATIVE_FREE(MSt18),
    first_pass(Rest7, MMod, MSt18, State0);
% 118
first_pass(<<?OP_BS_GET_FLOAT2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {_Live, Rest3} = decode_literal(Rest2),
    {MSt2, Size, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt1, State0),
    {Unit, Rest5} = decode_literal(Rest4),
    {FlagsValue, Rest6} = decode_literal(Rest5),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, SizeReg} = term_to_int(Size, Fail, MMod, MSt3),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt4, SizeReg * Unit};
            true ->
                MSt5 = MMod:mul(MSt4, SizeReg, Unit),
                {MSt5, SizeReg}
        end,
    {MSt7, BSBinaryReg} = MMod:get_array_element(MSt6, MatchStateRegPtr, 1),
    {MSt8, BSOffsetReg} = MMod:get_array_element(MSt7, MatchStateRegPtr, 2),
    {MSt9, Result} = MMod:call_primitive(MSt8, ?PRIM_BITSTRING_EXTRACT_FLOAT, [
        ctx, {free, BSBinaryReg}, BSOffsetReg, NumBits, {free, FlagsValue}
    ]),
    MSt10 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt9),
    MSt11 = MMod:add(MSt10, BSOffsetReg, NumBits),
    MSt12 = MMod:free_native_registers(MSt11, [NumBits]),
    MSt13 = MMod:move_to_array_element(MSt12, BSOffsetReg, MatchStateRegPtr, 2),
    MSt14 = MMod:free_native_registers(MSt13, [BSOffsetReg, MatchStateRegPtr]),
    {MSt15, Dest, Rest7} = decode_dest(Rest6, MMod, MSt14),
    ?TRACE("OP_BS_GET_FLOAT2 ~p,~p,~p,~p,~p,~p,~p\n", [
        Fail, Src, _Live, Size, Unit, FlagsValue, Dest
    ]),
    MSt16 = MMod:move_to_vm_register(MSt15, Result, Dest),
    MSt17 = MMod:free_native_registers(MSt16, [Result]),
    ?ASSERT_ALL_NATIVE_FREE(MSt17),
    first_pass(Rest7, MMod, MSt17, State0);
% 119
first_pass(<<?OP_BS_GET_BINARY2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Live, Rest3} = decode_literal(Rest2),
    {MSt2, Size, Rest4} = decode_compact_term(Rest3, MMod, MSt1, State0),
    {Unit, Rest5} = decode_literal(Rest4),
    {FlagsValue, Rest6} = decode_literal(Rest5),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, BSBinaryReg0} = MMod:get_array_element(MSt3, MatchStateRegPtr, 1),
    {MSt5, BSOffsetReg0} = MMod:get_array_element(MSt4, MatchStateRegPtr, 2),
    MSt6 =
        if
            Unit =/= 8 ->
                MMod:call_primitive_last(MSt5, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                ]);
            FlagsValue =/= 0 ->
                MMod:call_primitive_last(MSt5, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                ]);
            true ->
                MSt5
        end,
    MSt7 = MMod:if_block(MSt6, {BSOffsetReg0, '&', 16#7, '!=', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [ctx, jit_state, offset, ?BADARG_ATOM])
    end),
    {MSt8, BSOffsetReg1} = MMod:shift_right(MSt7, {free, BSOffsetReg0}, 3),
    {MSt9, BSBinaryReg0} = MMod:and_(MSt8, {free, BSBinaryReg0}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt10, SizeReg} = MMod:get_array_element(MSt9, {free, BSBinaryReg0}, 1),
    {MSt13, SizeValue} =
        if
            Size =:= ?ALL_ATOM ->
                MSt11 = MMod:sub(MSt10, SizeReg, BSOffsetReg1),
                {MSt11, SizeReg};
            is_integer(Size) ->
                % SizeReg is binary size
                % Size is a tagged integer: (N bsl 4) bor 0xF
                % SizeBytes is the raw byte count
                SizeBytes = Size bsr 4,
                MSt11 = MMod:sub(MSt10, SizeReg, SizeBytes),
                MSt12 = cond_jump_to_label({{free, SizeReg}, '<', BSOffsetReg1}, Fail, MMod, MSt11),
                {MSt12, SizeBytes};
            true ->
                {MSt11, SizeValReg} = MMod:move_to_native_register(MSt10, Size),
                MSt12 = MMod:if_else_block(
                    MSt11,
                    {SizeValReg, '==', ?ALL_ATOM},
                    fun(BSt0) ->
                        BSt1 = MMod:sub(BSt0, SizeReg, BSOffsetReg1),
                        MMod:free_native_registers(BSt1, [SizeValReg])
                    end,
                    fun(BSt0) ->
                        {BSt1, SizeValReg} = term_to_int(SizeValReg, 0, MMod, BSt0),
                        BSt2 = MMod:sub(BSt1, SizeReg, SizeValReg),
                        BSt3 = cond_jump_to_label({SizeReg, '<', BSOffsetReg1}, Fail, MMod, BSt2),
                        BSt4 = MMod:move_to_native_register(BSt3, SizeValReg, SizeReg),
                        MMod:free_native_registers(BSt4, [SizeValReg])
                    end
                ),
                {MSt12, SizeReg}
        end,
    {MSt14, NewOffsetReg} = MMod:copy_to_native_register(MSt13, BSOffsetReg1),
    MSt15 = MMod:add(MSt14, NewOffsetReg, SizeValue),
    MSt16 = MMod:shift_left(MSt15, NewOffsetReg, 3),
    % Write new offset
    MSt17 = MMod:move_to_array_element(MSt16, NewOffsetReg, MatchStateRegPtr, 2),
    MSt18 = MMod:free_native_registers(MSt17, [NewOffsetReg]),
    {MSt19, TrimResultReg} = MMod:call_primitive(MSt18, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt20 = MMod:free_native_registers(MSt19, [TrimResultReg]),
    {MSt21, BSBinaryReg1} = MMod:get_array_element(MSt20, {free, MatchStateRegPtr}, 1),
    MSt22 = MMod:or_(MSt21, BSBinaryReg1, ?TERM_PRIMARY_BOXED),
    {MSt23, HeapSizeReg} = MMod:call_primitive(MSt22, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        BSBinaryReg1, SizeValue
    ]),
    {MSt24, BSBinaryReg2} = memory_ensure_free_with_extra_root(
        BSBinaryReg1, Live, {free, HeapSizeReg}, MMod, MSt23
    ),
    {MSt25, ResultTerm} = MMod:call_primitive(MSt24, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, {free, BSBinaryReg2}, {free, BSOffsetReg1}, {free, SizeValue}
    ]),
    {MSt26, Dest, Rest7} = decode_dest(Rest6, MMod, MSt25),
    ?TRACE("OP_BS_GET_BINARY2 ~p,~p,~p,~p,~p,~p,~p\n", [
        Fail, Src, Live, Size, Unit, FlagsValue, Dest
    ]),
    MSt27 = MMod:move_to_vm_register(MSt26, ResultTerm, Dest),
    MSt28 = MMod:free_native_registers(MSt27, [ResultTerm, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt28),
    first_pass(Rest7, MMod, MSt28, State0);
% 120
first_pass(<<?OP_BS_SKIP_BITS2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Size, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt1, State0),
    {Unit, Rest4} = decode_literal(Rest3),
    {_FlagsValue, Rest5} = decode_literal(Rest4),
    ?TRACE("OP_BS_SKIP_BITS2 ~p, ~p, ~p, ~p, ~p\n", [Fail, Src, Size, Unit, _FlagsValue]),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, SizeReg} = term_to_int(Size, Fail, MMod, MSt3),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt4, SizeReg * Unit};
            true ->
                MSt5 = MMod:mul(MSt4, SizeReg, Unit),
                {MSt5, SizeReg}
        end,
    {MSt7, BSBinaryReg} = MMod:get_array_element(MSt6, MatchStateRegPtr, 1),
    {MSt8, BSOffsetReg} = MMod:get_array_element(MSt7, MatchStateRegPtr, 2),
    MSt9 = MMod:add(MSt8, BSOffsetReg, NumBits),
    MSt10 = MMod:free_native_registers(MSt9, [NumBits]),
    {MSt11, BSBinarySize} = term_binary_size({free, BSBinaryReg}, MMod, MSt10),
    MSt12 = MMod:shift_left(MSt11, BSBinarySize, 3),
    MSt13 = cond_jump_to_label({{free, BSBinarySize}, '<', BSOffsetReg}, Fail, MMod, MSt12),
    MSt14 = MMod:move_to_array_element(MSt13, BSOffsetReg, MatchStateRegPtr, 2),
    MSt15 = MMod:free_native_registers(MSt14, [BSOffsetReg, MatchStateRegPtr]),
    ?ASSERT_ALL_NATIVE_FREE(MSt15),
    first_pass(Rest5, MMod, MSt15, State0);
% 121
first_pass(<<?OP_BS_TEST_TAIL2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Bits, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_TEST_TAIL2 ~p, ~p, ~p\n", [Fail, Src, Bits]),
    {MSt2, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt1, Src),
    {MSt3, BSBinaryReg} = MMod:get_array_element(MSt2, MatchStateRegPtr, 1),
    {MSt4, BSOffsetReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 2),
    MSt5 = MMod:free_native_registers(MSt4, [MatchStateRegPtr]),
    MSt6 = MMod:add(MSt5, BSOffsetReg, Bits),
    {MSt7, BSBinarySize} = term_binary_size({free, BSBinaryReg}, MMod, MSt6),
    MSt8 = MMod:shift_left(MSt7, BSBinarySize, 3),
    MSt9 = cond_jump_to_label({{free, BSBinarySize}, '!=', BSOffsetReg}, Fail, MMod, MSt8),
    MSt10 = MMod:free_native_registers(MSt9, [BSOffsetReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt10),
    first_pass(Rest3, MMod, MSt10, State0);
% 124
first_pass(
    <<?OP_GC_BIF1, Rest0/binary>>, MMod, MSt0, #state{import_resolver = ImportResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {Bif, Rest3} = decode_literal(Rest2),
    {MSt1, Arg, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt0, State0),
    {MSt2, Dest, Rest5} = decode_dest(Rest4, MMod, MSt1),
    {BifModule, BifFunName, 1} = ImportResolver(Bif),
    ?TRACE("OP_GC_BIF1 ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Live, Bif, Arg, Dest]),
    MSt3 = op_gc_bif1(MMod, MSt2, FailLabel, Live, Bif, BifModule, BifFunName, Arg, Dest),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest5, MMod, MSt3, State0);
% 125
first_pass(
    <<?OP_GC_BIF2, Rest0/binary>>, MMod, MSt0, #state{import_resolver = ImportResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    {Bif, Rest3} = decode_literal(Rest2),
    {MSt1, Arg1, Rest4} = decode_typed_compact_term(Rest3, MMod, MSt0, State0),
    {MSt2, Arg2, Rest5} = decode_typed_compact_term(Rest4, MMod, MSt1, State0),
    {MSt3, Dest, Rest6} = decode_dest(Rest5, MMod, MSt2),
    {BifModule, BifFunName, 2} = ImportResolver(Bif),
    ?TRACE("OP_GC_BIF2 ~p, ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Live, Bif, Arg1, Arg2, Dest]),
    MSt4 = op_gc_bif2(MMod, MSt3, FailLabel, Live, Bif, BifModule, BifFunName, Arg1, Arg2, Dest),
    ?ASSERT_ALL_NATIVE_FREE(MSt4),
    first_pass(Rest6, MMod, MSt4, State0);
% 129
first_pass(<<?OP_IS_BITSTR, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_BITSTR ~p, ~p\n", [Label, Arg1]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    {MSt6, Reg} = MMod:and_(MSt5, {free, Reg}, ?TERM_BOXED_TAG_MASK),
    MSt7 = cond_jump_to_label(
        {'and', [
            {Reg, '!=', ?TERM_BOXED_REFC_BINARY},
            {Reg, '!=', ?TERM_BOXED_HEAP_BINARY},
            {Reg, '!=', ?TERM_BOXED_SUB_BINARY}
        ]},
        Label,
        MMod,
        MSt6
    ),
    MSt8 = MMod:free_native_registers(MSt7, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest2, MMod, MSt8, State0);
% 131
first_pass(<<?OP_BS_TEST_UNIT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Unit, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_TEST_UNIT ~p, ~p, ~p\n", [Fail, Src, Unit]),
    {MSt2, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt1, Src),
    {MSt3, BSBinaryReg} = MMod:get_array_element(MSt2, MatchStateRegPtr, 1),
    {MSt4, BSOffsetReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 2),
    MSt5 = MMod:free_native_registers(MSt4, [MatchStateRegPtr]),
    {MSt6, BSBinarySize} = term_binary_size({free, BSBinaryReg}, MMod, MSt5),
    MSt7 = MMod:shift_left(MSt6, BSBinarySize, 3),
    % BSBinarySize = binary_size * 8
    MSt8 = MMod:sub(MSt7, BSBinarySize, BSOffsetReg),
    % BSBinarySize = (binary_size * 8) - offset = remaining bits
    MSt9 = MMod:free_native_registers(MSt8, [BSOffsetReg]),
    {MSt10, BSBinarySize1} = MMod:and_(MSt9, {free, BSBinarySize}, Unit - 1),
    MSt11 = cond_jump_to_label({{free, BSBinarySize1}, '!=', 0}, Fail, MMod, MSt10),
    ?ASSERT_ALL_NATIVE_FREE(MSt11),
    first_pass(Rest3, MMod, MSt11, State0);
% 132
first_pass(<<?OP_BS_MATCH_STRING, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt0, State0),
    {Bits, Rest3} = decode_literal(Rest2),
    {Offset, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_BS_MATCH_STRING ~p,~p,~p,~p\n", [Fail, Src, Bits, Offset]),
    {MSt2, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt1, Src),
    {MSt3, BSBinaryReg} = MMod:get_array_element(MSt2, MatchStateRegPtr, 1),
    {MSt4, BSOffsetReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 2),
    {MSt5, MatchResult} = MMod:call_primitive(MSt4, ?PRIM_BITSTRING_MATCH_MODULE_STR, [
        ctx, jit_state, {free, BSBinaryReg}, BSOffsetReg, Offset, Bits
    ]),
    MSt6 = cond_jump_to_label({'(bool)', {free, MatchResult}, '==', false}, Fail, MMod, MSt5),
    MSt7 = MMod:add(MSt6, BSOffsetReg, Bits),
    MSt8 = MMod:move_to_array_element(MSt7, BSOffsetReg, MatchStateRegPtr, 2),
    MSt9 = MMod:free_native_registers(MSt8, [BSOffsetReg, MatchStateRegPtr]),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest4, MMod, MSt9, State0);
% 133
first_pass(<<?OP_BS_INIT_WRITABLE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_BS_INIT_WRITABLE\n", []),
    HeapSize = term_binary_heap_size(0, MMod),
    {MSt1, MemoryEnsureFreeReg} = MMod:call_primitive(
        MSt0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
            ctx, jit_state, HeapSize, 0, ?MEMORY_CAN_SHRINK
        ]
    ),
    MSt2 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt1),
    {MSt3, CreatedBin} = MMod:call_primitive(MSt2, ?PRIM_TERM_CREATE_EMPTY_BINARY, [ctx, 0]),
    MSt4 = MMod:if_block(MSt3, {CreatedBin, '==', ?TERM_INVALID_TERM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
        ])
    end),
    MSt5 = MMod:set_bs(MSt4, CreatedBin),
    MSt6 = MMod:move_to_vm_register(MSt5, CreatedBin, {x_reg, 0}),
    MSt7 = MMod:free_native_registers(MSt6, [CreatedBin]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest0, MMod, MSt7, State0);
% 136
first_pass(<<?OP_TRIM, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {NWords, Rest1} = decode_literal(Rest0),
    {_NRemaining, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_TRIM ~p, ~p\n", [NWords, _NRemaining]),
    MSt1 = MMod:increment_sp(MSt0, NWords),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State0);
% 138
first_pass(<<?OP_BS_GET_UTF8, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    Rest4 = skip_compact_term(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}]),
    MSt3 = cond_jump_to_label({Value, '==', 0}, Fail, MMod, MSt2),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BS_GET_UTF8 ~p,~p,~p\n", [Fail, Src, Dest]),
    MSt5 = MMod:move_to_vm_register(MSt4, Value, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Value, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 139
first_pass(<<?OP_BS_SKIP_UTF8, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    Rest4 = skip_compact_term(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}]),
    MSt3 = cond_jump_to_label({{free, Value}, '==', 0}, Fail, MMod, MSt2),
    ?TRACE("OP_BS_SKIP_UTF8 ~p,~p\n", [Fail, Src]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 140
first_pass(<<?OP_BS_GET_UTF16, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF16, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({Value, '==', 0}, Fail, MMod, MSt2),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BS_GET_UTF16 ~p,~p,~p,~p\n", [Fail, Src, FlagsValue, Dest]),
    MSt5 = MMod:move_to_vm_register(MSt4, Value, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Value, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 141
first_pass(<<?OP_BS_SKIP_UTF16, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF16, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({{free, Value}, '==', 0}, Fail, MMod, MSt2),
    ?TRACE("OP_BS_SKIP_UTF16 ~p,~p,~p\n", [Fail, Src, FlagsValue]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 142
first_pass(<<?OP_BS_GET_UTF32, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF32, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({Value, '==', 0}, Fail, MMod, MSt2),
    {MSt4, Dest, Rest5} = decode_dest(Rest4, MMod, MSt3),
    ?TRACE("OP_BS_GET_UTF32 ~p,~p,~p,~p\n", [Fail, Src, FlagsValue, Dest]),
    MSt5 = MMod:move_to_vm_register(MSt4, Value, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Value, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0);
% 143
first_pass(<<?OP_BS_SKIP_UTF32, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    Rest3 = skip_compact_term(Rest2),
    {FlagsValue, Rest4} = decode_literal(Rest3),
    {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF32, [
        {free, Src}, {free, FlagsValue}
    ]),
    MSt3 = cond_jump_to_label({{free, Value}, '==', 0}, Fail, MMod, MSt2),
    ?TRACE("OP_BS_SKIP_UTF32 ~p,~p,~p\n", [Fail, Src, FlagsValue]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 152
first_pass(<<?OP_GC_BIF3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Live, Rest2} = decode_literal(Rest1),
    CappedLive =
        if
            Live > ?MAX_REG -> ?MAX_REG;
            true -> Live
        end,
    {Bif, Rest3} = decode_literal(Rest2),
    {MSt3, FuncPtr} = resolve_gcbif_func_ptr(MMod, MSt0, Live, Bif),
    {MSt4, Arg1, Rest4} = decode_compact_term(Rest3, MMod, MSt3, State0),
    {MSt5, Arg2, Rest5} = decode_compact_term(Rest4, MMod, MSt4, State0),
    {MSt6, Arg3, Rest6} = decode_compact_term(Rest5, MMod, MSt5, State0),
    {MSt7, Dest, Rest7} = decode_dest(Rest6, MMod, MSt6),
    ?TRACE("OP_GC_BIF3 ~p, ~p, ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Live, Bif, Arg1, Arg2, Arg3, Dest]),
    {MSt8, ResultReg} = MMod:call_func_ptr(MSt7, {free, FuncPtr}, [
        ctx, FailLabel, CappedLive, {free, Arg1}, {free, Arg2}, {free, Arg3}
    ]),
    MSt9 = bif_faillabel_test(FailLabel, MMod, MSt8, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest7, MMod, MSt9, State0);
% 153
first_pass(
    <<?OP_LINE, Rest0/binary>>,
    MMod,
    MSt,
    #state{line_offsets = AccLines} = State0
) ->
    {Line, Rest1} = decode_literal(Rest0),
    ?TRACE("OP_LINE ~p\n", [Line]),
    MSt0 = ?DWARF_LINE(MMod, MSt, Line),
    Offset = MMod:offset(MSt0),
    first_pass(Rest1, MMod, MSt0, State0#state{
        line_offsets = [{Line, Offset} | AccLines],
        current_line = Line
    });
% 154
first_pass(<<?OP_PUT_MAP_ASSOC, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    {Live, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_PUT_MAP_ASSOC ~p,~p,~p,~p,[", [_Label, Src, Dest, Live]),
    {ListSize, Rest5} = decode_extended_list_header(Rest4),
    {MSt3, NewEntriesReg} = MMod:move_to_native_register(MSt2, 0),
    % First iteration to compute size
    NumElements = ListSize div 2,
    {MSt4, Rest6} = lists:foldl(
        fun(_Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            ARest2 = skip_compact_term(ARest1),
            {ASt2, PosReg} = MMod:call_primitive(ASt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, Src, {free, Key}
            ]),
            ASt3 = MMod:if_block(ASt2, {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, fun(BSt0) ->
                MMod:add(BSt0, NewEntriesReg, 1)
            end),
            ASt4 = MMod:if_block(
                ASt3, {'(int)', {free, PosReg}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            {ASt4, ARest2}
        end,
        {MSt3, Rest5},
        lists:seq(1, NumElements)
    ),
    {MSt5, SrcSizeReg} = term_get_map_size(Src, MMod, MSt4),
    MSt6 = MMod:if_else_block(
        MSt5,
        {NewEntriesReg, '==', 0},
        fun(BSt0) ->
            MMod:add(BSt0, SrcSizeReg, 2)
        end,
        fun(BSt0) ->
            BSt1 = MMod:add(BSt0, SrcSizeReg, NewEntriesReg),
            BSt2 = MMod:shift_left(BSt1, SrcSizeReg, 1),
            MMod:add(BSt2, SrcSizeReg, 3)
        end
    ),
    {MSt7, TrimResultReg} = MMod:call_primitive(MSt6, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt8 = MMod:free_native_registers(MSt7, [TrimResultReg]),
    {MSt9, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, {free, SrcSizeReg}, MMod, MSt8
    ),
    % Second iteration to prepare KV pairs
    {MSt10, KVReg} = MMod:call_primitive(MSt9, ?PRIM_MALLOC, [
        ctx, jit_state, ListSize * MMod:word_size()
    ]),
    MSt11 = handle_error_if({KVReg, '==', 0}, MMod, MSt10),
    {MSt12, Rest6} = lists:foldl(
        fun(Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            {ASt2, Value, ARest2} = decode_compact_term(ARest1, MMod, ASt1, State0),
            ?TRACE("(~p,~p),", [Key, Value]),
            ASt3 = MMod:move_to_array_element(ASt2, Key, KVReg, Index * 2),
            ASt4 = MMod:move_to_array_element(ASt3, Value, KVReg, (Index * 2) + 1),
            ASt5 = MMod:free_native_registers(ASt4, [Key, Value]),
            {ASt5, ARest2}
        end,
        {MSt11, Rest5},
        lists:seq(0, NumElements - 1)
    ),
    ?TRACE("]\n", []),
    {MSt13, PutMapAssocReg} = MMod:call_primitive(MSt12, ?PRIM_PUT_MAP_ASSOC, [
        ctx, jit_state, {free, NewSrc}, {free, NewEntriesReg}, NumElements, KVReg
    ]),
    {MSt14, FreeReg} = MMod:call_primitive(MSt13, ?PRIM_FREE, [{free, KVReg}]),
    MSt15 = MMod:free_native_registers(MSt14, [FreeReg]),
    MSt16 = handle_error_if({PutMapAssocReg, '==', 0}, MMod, MSt15),
    MSt17 = MMod:move_to_vm_register(MSt16, PutMapAssocReg, Dest),
    MSt18 = MMod:free_native_registers(MSt17, [PutMapAssocReg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt18),
    first_pass(Rest6, MMod, MSt18, State0);
% 155
first_pass(<<?OP_PUT_MAP_EXACT, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    {Live, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_PUT_MAP_EXACT ~p,~p,~p,~p,[", [Label, Src, Dest, Live]),
    {ListSize, Rest5} = decode_extended_list_header(Rest4),
    % Make sure every key from list is in src
    NumElements = ListSize div 2,
    {MSt3, Rest6} = lists:foldl(
        fun(_Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            ARest2 = skip_compact_term(ARest1),
            {ASt2, PosReg} = MMod:call_primitive(ASt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, Src, {free, Key}
            ]),
            % A missing required key (:=) fails the guard when a fail label is
            % set (label /= 0); only raise badarg in body context (label == 0).
            ASt3 = cond_raise_badarg_or_jump_to_fail_label(
                {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, ASt2
            ),
            ASt4 = MMod:if_block(
                ASt3, {'(int)', {free, PosReg}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            {ASt4, ARest2}
        end,
        {MSt2, Rest5},
        lists:seq(1, NumElements)
    ),
    {MSt4, SrcSizeReg} = term_get_map_size(Src, MMod, MSt3),
    % shared
    MSt5 = MMod:add(MSt4, SrcSizeReg, 2),
    {MSt6, TrimResultReg} = MMod:call_primitive(MSt5, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt7 = MMod:free_native_registers(MSt6, [TrimResultReg]),
    {MSt8, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, {free, SrcSizeReg}, MMod, MSt7
    ),
    {MSt9, NewMapPtrReg} = MMod:call_primitive(MSt8, ?PRIM_TERM_COPY_MAP, [ctx, NewSrc]),
    {MSt10, NewMapPtrReg} = MMod:and_(MSt9, {free, NewMapPtrReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt11, Rest6} = lists:foldl(
        fun(_Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            {ASt2, Value, ARest2} = decode_compact_term(ARest1, MMod, ASt1, State0),
            ?TRACE("(~p,~p),", [Key, Value]),
            {ASt3, PosReg} = MMod:call_primitive(ASt2, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, NewSrc, Key
            ]),
            ASt4 = MMod:if_block(ASt3, {'(int)', PosReg, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(
                BSt0
            ) ->
                MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                ])
            end),
            ASt5 = term_set_map_assoc(
                NewMapPtrReg, {free, PosReg}, {free, Key}, {free, Value}, MMod, ASt4
            ),
            {ASt5, ARest2}
        end,
        {MSt10, Rest5},
        lists:seq(1, NumElements)
    ),
    ?TRACE("]\n", []),
    MSt12 = MMod:or_(MSt11, NewMapPtrReg, ?TERM_PRIMARY_BOXED),
    MSt13 = MMod:move_to_vm_register(MSt12, NewMapPtrReg, Dest),
    MSt14 = MMod:free_native_registers(MSt13, [NewMapPtrReg, Dest, NewSrc]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest6, MMod, MSt14, State0);
% 156
first_pass(<<?OP_IS_MAP, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_MAP ~p, ~p\n", [Label, Arg1]),
    MSt2 = verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_MAP, MMod, MSt1),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 157
first_pass(<<?OP_HAS_MAP_FIELDS, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_HAS_MAP_FIELDS ~p,~p,[", [Label, Src]),
    {MSt2, Key1, Rest4} = decode_compact_term(Rest3, MMod, MSt1, State0),
    ?TRACE("~p", [Key1]),
    {MSt3, PosReg1} = MMod:call_primitive(MSt2, ?PRIM_TERM_FIND_MAP_POS, [ctx, Src, {free, Key1}]),
    MSt4 = cond_jump_to_label({'(int)', PosReg1, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, MSt3),
    MSt5 = MMod:if_block(MSt4, {'(int)', {free, PosReg1}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(
        BSt0
    ) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
        ])
    end),
    {MSt6, Rest5} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Key, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE(",~p", [Key]),
            {AccMSt2, PosReg} = MMod:call_primitive(AccMSt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, Src, Key
            ]),
            AccMSt3 = cond_jump_to_label(
                {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, AccMSt2
            ),
            AccMSt4 = MMod:if_block(
                AccMSt3, {'(int)', {free, PosReg}, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    % TODO: previous implementation yielded a slightly smaller code as raise block was shared.
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            AccMSt5 = MMod:free_native_registers(AccMSt4, [Key]),
            {AccMSt5, AccRest1}
        end,
        {MSt5, Rest4},
        lists:seq(2, ListSize)
    ),
    ?TRACE("]\n", []),
    MSt7 = MMod:free_native_registers(MSt6, [Src]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest5, MMod, MSt7, State0);
% 158
first_pass(<<?OP_GET_MAP_ELEMENTS, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListSize, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_GET_MAP_ELEMENTS ~p,~p,[", [Label, Src]),
    {MSt2, Key1, Rest4} = decode_compact_term(Rest3, MMod, MSt1, State0),
    ?TRACE("~p", [Key1]),
    {MSt3, PosReg1} = MMod:call_primitive(MSt2, ?PRIM_TERM_FIND_MAP_POS, [ctx, Src, {free, Key1}]),
    MSt4 = cond_jump_to_label({'(int)', PosReg1, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, MSt3),
    MSt5 = MMod:if_block(MSt4, {'(int)', PosReg1, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
        ])
    end),
    {MSt6, SrcReg} = MMod:move_to_native_register(MSt5, Src),
    {MSt7, MapReg} = MMod:and_(MSt6, SrcReg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt8 = MMod:add(MSt7, MapReg, MMod:word_size() * 2),
    {MSt9, Dest1, Rest5} = decode_dest(Rest4, MMod, MSt8),
    ?TRACE(",~p", [Dest1]),
    MSt10 = MMod:move_array_element(MSt9, MapReg, {free, PosReg1}, Dest1),
    MSt11 = MMod:free_native_registers(MSt10, [Dest1]),
    {MSt12, Rest6} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Key, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE(",~p", [Key]),
            {AccMSt2, PosReg} = MMod:call_primitive(AccMSt1, ?PRIM_TERM_FIND_MAP_POS, [
                ctx, SrcReg, Key
            ]),
            AccMSt3 = cond_jump_to_label(
                {'(int)', PosReg, '==', ?TERM_MAP_NOT_FOUND}, Label, MMod, AccMSt2
            ),
            AccMSt4 = MMod:if_block(
                AccMSt3, {'(int)', PosReg, '==', ?TERM_MAP_MEMORY_ALLOC_FAIL}, fun(BSt0) ->
                    % TODO: previous implementation yielded a slightly smaller code as raise block was shared.
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end
            ),
            AccMSt5 = MMod:free_native_registers(AccMSt4, [Key]),
            {AccMSt6, Dest, AccRest2} = decode_dest(AccRest1, MMod, AccMSt5),
            ?TRACE(",~p", [Dest]),
            AccMSt7 = MMod:move_array_element(AccMSt6, MapReg, {free, PosReg}, Dest),
            AccMSt8 = MMod:free_native_registers(AccMSt7, [Dest]),
            {AccMSt8, AccRest2}
        end,
        {MSt11, Rest5},
        lists:seq(2, ListSize div 2)
    ),
    ?TRACE("]\n", []),
    MSt13 = MMod:free_native_registers(MSt12, [MapReg, SrcReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt13),
    first_pass(Rest6, MMod, MSt13, State0);
% 159
first_pass(
    <<?OP_IS_TAGGED_TUPLE, Rest0/binary>>, MMod, MSt0, #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Arg1, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Arity, Rest3} = decode_literal(Rest2),
    {AtomIndex, Rest4} = decode_atom(Rest3),
    ?TRACE("OP_IS_TAGGED_TUPLE ~p, ~p, ~p, ~p\n", [Label, Arg1, Arity, AtomIndex]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, TagReg0} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = cond_jump_to_label(
        {TagReg0, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_TUPLE}, Label, MMod, MSt5
    ),
    {MSt7, TagReg1} = MMod:shift_right(MSt6, {free, TagReg0}, 6),
    MSt8 = cond_jump_to_label({TagReg1, '!=', Arity}, Label, MMod, MSt7),
    MSt9 = MMod:free_native_registers(MSt8, [TagReg1]),
    MSt10 = MMod:move_array_element(MSt9, Reg, 1, Reg),
    {MSt11, AtomReg} =
        case maps:find(AtomResolver(AtomIndex), ?DEFAULT_ATOMS) of
            error ->
                MMod:call_primitive(
                    MSt10, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
                );
            {ok, Val} ->
                {MSt10, Val}
        end,
    MSt12 = cond_jump_to_label({Reg, '!=', AtomReg}, Label, MMod, MSt11),
    MSt13 = MMod:free_native_registers(MSt12, [Reg]),
    MSt14 = MMod:free_native_registers(MSt13, [AtomReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest4, MMod, MSt14, State0);
% 160
first_pass(<<?OP_BUILD_STACKTRACE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_STACKTRACE_BUILD, [ctx]),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, {x_reg, 0}),
    MSt3 = MMod:free_native_registers(MSt2, [ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest0, MMod, MSt3, State0);
% 161
first_pass(<<?OP_RAW_RAISE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ExClassReg} = MMod:move_to_native_register(MSt0, {x_reg, 0}),
    MSt2 = MMod:if_block(MSt1, {ExClassReg, '==', ?ERROR_ATOM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAW_RAISE, [ctx, jit_state])
    end),
    MSt3 = MMod:if_block(MSt2, {ExClassReg, '==', ?LOWERCASE_EXIT_ATOM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAW_RAISE, [ctx, jit_state])
    end),
    MSt4 = MMod:if_block(MSt3, {{free, ExClassReg}, '==', ?THROW_ATOM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAW_RAISE, [ctx, jit_state])
    end),
    MSt5 = MMod:move_to_vm_register(MSt4, ?BADARG_ATOM, {x_reg, 0}),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest0, MMod, MSt5, State0);
% 162
first_pass(<<?OP_GET_HD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_GET_HD ~p, ~p\n", [SrcValue, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, SrcValue),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, ?LIST_HEAD_INDEX, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Dest, Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 163
first_pass(<<?OP_GET_TL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, SrcValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest3} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_GET_TL ~p, ~p\n", [SrcValue, Dest]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, SrcValue),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, ?LIST_TAIL_INDEX, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [Dest, Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State0);
% 164
first_pass(<<?OP_PUT_TUPLE2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {ListSize, Rest2} = decode_extended_list_header(Rest1),
    ?TRACE("OP_PUT_TUPLE2 ~p, [", [Dest]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_ALLOC_TUPLE, [ctx, ListSize]),
    {MSt3, ResultReg} = MMod:and_(MSt2, {free, ResultReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Rest3} = lists:foldl(
        fun(Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Element, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE("~p,", [Element]),
            AccMSt2 = MMod:move_to_array_element(AccMSt1, Element, ResultReg, Index),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [Element]),
            {AccMSt3, AccRest1}
        end,
        {MSt3, Rest2},
        lists:seq(1, ListSize)
    ),
    ?TRACE("]\n", []),
    MSt5 = MMod:or_(MSt4, ResultReg, ?TERM_PRIMARY_BOXED),
    MSt6 = MMod:move_to_vm_register(MSt5, ResultReg, Dest),
    MSt7 = MMod:free_native_registers(MSt6, [Dest, ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest3, MMod, MSt7, State0);
% 165
first_pass(<<?OP_BS_GET_TAIL, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Src, Rest1} = decode_typed_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    {Live, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_GET_TAIL ~p, ~p, ~p\n", [Src, Dest, Live]),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, BSBinaryReg} = MMod:get_array_element(MSt3, MatchStateRegPtr, 1),
    {MSt5, BSOffsetReg} = MMod:get_array_element(MSt4, MatchStateRegPtr, 2),
    MSt6 = MMod:free_native_registers(MSt5, [MatchStateRegPtr]),
    {MSt7, BSBinaryReg} = MMod:and_(MSt6, {free, BSBinaryReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt8, ResultTerm, NewMatchState} = do_get_tail(
        Src, Live, BSOffsetReg, BSBinaryReg, MMod, MSt7
    ),
    MSt9 = MMod:free_native_registers(MSt8, [BSBinaryReg]),
    {MSt10, MatchStateReg1} = MMod:move_to_native_register(MSt9, NewMatchState),
    {MSt11, MatchStateReg1} = MMod:and_(MSt10, {free, MatchStateReg1}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt12 = MMod:move_to_array_element(MSt11, BSOffsetReg, MatchStateReg1, 2),
    MSt13 = MMod:move_to_vm_register(MSt12, ResultTerm, Dest),
    MSt14 = MMod:free_native_registers(MSt13, [MatchStateReg1, BSOffsetReg, ResultTerm, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt14),
    first_pass(Rest3, MMod, MSt14, State0);
% 166
first_pass(<<?OP_BS_START_MATCH3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Live, Rest3} = decode_literal(Rest2),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    ?TRACE("OP_BS_START_MATCH3 ~p, ~p, ~p, ~p\n", [Fail, Src, Live, Dest]),
    MSt3 = verify_is_binary_or_match_state(Fail, Src, MMod, MSt2),
    {MSt4, NewSrc} = term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt3),
    MSt5 = MMod:free_native_registers(MSt4, [NewSrc, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 167
first_pass(<<?OP_BS_GET_POSITION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Src, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Dest, Rest2} = decode_dest(Rest1, MMod, MSt1),
    {_Live, Rest3} = decode_literal(Rest2),
    ?TRACE("OP_BS_GET_POSITION ~p, ~p, ~p\n", [Src, Dest, _Live]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, Src),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 2, Reg),
    {MSt6, Reg} = term_from_int(Reg, MMod, MSt5),
    MSt7 = MMod:move_to_vm_register(MSt6, Reg, Dest),
    MSt8 = MMod:free_native_registers(MSt7, [Reg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt8),
    first_pass(Rest3, MMod, MSt8, State0);
% 168
first_pass(<<?OP_BS_SET_POSITION, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Src, Rest1} = decode_typed_compact_term(Rest0, MMod, MSt0, State0),
    {MSt2, Pos, Rest2} = decode_typed_compact_term(Rest1, MMod, MSt1, State0),
    ?TRACE("OP_BS_SET_POSITION ~p, ~p\n", [Src, Pos]),
    {MSt3, MatchStateRegPtr} = verify_is_match_state_and_get_ptr(MMod, MSt2, Src),
    {MSt4, PosVal} = term_to_int(Pos, 0, MMod, MSt3),
    MSt5 = MMod:move_to_array_element(MSt4, PosVal, MatchStateRegPtr, 2),
    MSt6 = MMod:free_native_registers(MSt5, [PosVal, MatchStateRegPtr]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest2, MMod, MSt6, State0);
% 169
first_pass(<<?OP_SWAP, Rest0/binary>>, MMod, MSt0, State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, ArgA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {MSt2, ArgB, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_SWAP ~p, ~p\n", [ArgA, ArgB]),
    {MSt3, Reg} = MMod:move_to_native_register(MSt2, ArgA),
    MSt4 = MMod:move_to_vm_register(MSt3, ArgB, ArgA),
    MSt5 = MMod:move_to_vm_register(MSt4, Reg, ArgB),
    MSt6 = MMod:free_native_registers(MSt5, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest2, MMod, MSt6, State);
% 170
first_pass(<<?OP_BS_START_MATCH4, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_atom_or_label(Rest0, State0),
    {Live, Rest2} = decode_literal(Rest1),
    {MSt1, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    ?TRACE("OP_BS_START_MATCH4 ~p, ~p, ~p, ~p\n", [Fail, Live, Src, Dest]),
    MSt3 =
        if
            is_integer(Fail) ->
                verify_is_binary_or_match_state(Fail, Src, MMod, MSt2);
            Fail =:= no_fail ->
                MSt2;
            Fail =:= resume ->
                MSt2
        end,
    {MSt4, NewSrc} = term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt3),
    MSt5 = MMod:free_native_registers(MSt4, [NewSrc, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(Rest4, MMod, MSt5, State0);
% 171
first_pass(<<?OP_MAKE_FUN3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FunIndex, Rest1} = decode_literal(Rest0),
    {MSt1, Dest, Rest2} = decode_dest(Rest1, MMod, MSt0),
    {NumFree, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_MAKE_FUN3 ~p, [", [Dest]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_ALLOC_FUN, [
        ctx, jit_state, FunIndex, NumFree
    ]),
    {MSt3, ResultReg} = MMod:and_(MSt2, {free, ResultReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Rest4} = lists:foldl(
        fun(Index, {AccMSt0, AccRest0}) ->
            {AccMSt1, Element, AccRest1} = decode_compact_term(AccRest0, MMod, AccMSt0, State0),
            ?TRACE("~p,", [Element]),
            AccMSt2 = MMod:move_to_array_element(AccMSt1, Element, ResultReg, Index),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [Element]),
            {AccMSt3, AccRest1}
        end,
        {MSt3, Rest3},
        lists:seq(3, NumFree + 2)
    ),
    ?TRACE("]\n", []),
    MSt5 = MMod:or_(MSt4, ResultReg, ?TERM_PRIMARY_BOXED),
    MSt6 = MMod:move_to_vm_register(MSt5, ResultReg, Dest),
    MSt7 = MMod:free_native_registers(MSt6, [Dest, ResultReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest4, MMod, MSt7, State0);
% 172
first_pass(<<?OP_INIT_YREGS, Rest0/binary>>, MMod, MSt0, State) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {ListSize, Rest1} = decode_extended_list_header(Rest0),
    ?TRACE("OP_INIT_YREGS ~p\n", [ListSize]),
    {MSt1, Rest2} = lists:foldl(
        fun(_, {AccMSt0, AccRest0}) ->
            {AccMSt1, Dest, AccRest1} = decode_dest(AccRest0, MMod, AccMSt0),
            AccMSt2 = MMod:move_to_vm_register(AccMSt1, ?TERM_NIL, Dest),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [Dest]),
            {AccMSt3, AccRest1}
        end,
        {MSt0, Rest1},
        lists:duplicate(ListSize, [])
    ),
    ?ASSERT_ALL_NATIVE_FREE(MSt1),
    first_pass(Rest2, MMod, MSt1, State);
% 173
first_pass(<<?OP_RECV_MARKER_BIND, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {MSt2, RegB, Rest2} = decode_dest(Rest1, MMod, MSt1),
    ?TRACE("OP_RECV_MARKER_BIND ~p, ~p\n", [RegA, RegB]),
    MSt3 = MMod:free_native_registers(MSt2, [RegA, RegB]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest2, MMod, MSt3, State0);
% 174
first_pass(<<?OP_RECV_MARKER_CLEAR, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_CLEAR ~p\n", [RegA]),
    MSt2 = MMod:free_native_registers(MSt1, [RegA]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 175
first_pass(<<?OP_RECV_MARKER_RESERVE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_RESERVE ~p\n", [Dest]),
    % Clear register to avoid any issue with GC
    MSt2 = MMod:move_to_vm_register(MSt1, ?TERM_NIL, Dest),
    MSt3 = MMod:free_native_registers(MSt2, [Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest1, MMod, MSt3, State0);
% 176
first_pass(<<?OP_RECV_MARKER_USE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, RegA, Rest1} = decode_dest(Rest0, MMod, MSt0),
    ?TRACE("OP_RECV_MARKER_USE ~p\n", [RegA]),
    MSt2 = MMod:free_native_registers(MSt1, [RegA]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 177
first_pass(
    <<?OP_BS_CREATE_BIN, Rest0/binary>>, MMod, MSt0, #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {Alloc, Rest2} = decode_allocator_list(MMod, Rest1),
    {Live, Rest3} = decode_literal(Rest2),
    {_Unit, Rest4} = decode_literal(Rest3),
    % TODO: add skip_dest and redecode when we need it
    {MSt1, Dest, Rest5} = decode_dest(Rest4, MMod, MSt0),
    ?TRACE("OP_BS_CREATE_BIN ~p, ~p, ~p, ~p, [", [Fail, Alloc, Live, _Unit]),
    {ListLen, Rest6} = decode_extended_list_header(Rest5),
    % Compute binary size and verify types in first iteration
    NBSegments = ListLen div 6,
    {Rest7, MSt2, BinaryLitSize, BinaryRegSize, State1, ReuseSourceBinary} = lists:foldl(
        fun(Index, {AccRest0, AccMSt0, AccLiteralSize0, AccSizeReg0, AccState0, AccReuseSrc}) ->
            {AtomTypeIndex, AccRest1} = decode_atom(AccRest0),
            AtomType = AtomResolver(AtomTypeIndex),
            {_Seg, AccRest2} = decode_literal(AccRest1),
            {SegmentUnit, AccRest3} = decode_literal(AccRest2),
            AccRest4 = skip_compact_term(AccRest3),
            {AccMSt1, Src, AccRest5} = decode_compact_term(AccRest4, MMod, AccMSt0, AccState0),
            {AccMSt2, Size, AccRest6} = decode_compact_term(AccRest5, MMod, AccMSt1, AccState0),
            {AccMSt3, AccLiteralSize1, AccSizeReg1, AccState1} = first_pass_bs_create_bin_compute_size(
                AtomType,
                Src,
                Size,
                SegmentUnit,
                Fail,
                AccLiteralSize0,
                AccSizeReg0,
                MMod,
                AccMSt2,
                AccState0
            ),
            NewReuseSrc =
                AccReuseSrc orelse
                    (Index =:= 1 andalso AtomType =:= private_append andalso Size =:= ?ALL_ATOM),
            AccMSt4 = MMod:free_native_registers(AccMSt3, [Src, Size]),
            {AccRest6, AccMSt4, AccLiteralSize1, AccSizeReg1, AccState1, NewReuseSrc}
        end,
        {Rest6, MSt1, 0, undefined, State0, false},
        lists:seq(1, NBSegments)
    ),
    {MSt4, BinaryTotalSize} =
        case {BinaryLitSize, BinaryRegSize} of
            {_, undefined} ->
                {MSt2, BinaryLitSize};
            {0, Reg} ->
                {MSt2, Reg};
            {_, _} ->
                MSt3 = MMod:add(MSt2, BinaryRegSize, BinaryLitSize),
                {MSt3, BinaryRegSize}
        end,
    MSt5 =
        if
            is_integer(BinaryTotalSize) andalso BinaryTotalSize band 16#7 =/= 0 ->
                MMod:call_primitive_last(MSt4, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                ]);
            is_integer(BinaryTotalSize) ->
                MSt4;
            true ->
                MMod:if_block(MSt4, {BinaryTotalSize, '&', 16#7, '!=', 0}, fun(BlockSt) ->
                    MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?UNSUPPORTED_ATOM
                    ])
                end)
        end,
    {MSt6, TrimResultReg} = MMod:call_primitive(MSt5, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt7 = MMod:free_native_registers(MSt6, [TrimResultReg]),
    {MSt12, BinaryTotalSizeInBytes, AllocSize} =
        if
            is_integer(BinaryTotalSize) ->
                {MSt7, (BinaryTotalSize div 8),
                    term_binary_heap_size((BinaryTotalSize div 8), MMod) + Alloc};
            true ->
                {MSt8, BinaryTotalSizeBytes} = MMod:shift_right(MSt7, {free, BinaryTotalSize}, 3),
                {MSt9, BinaryTotalSizeBytes0} = MMod:copy_to_native_register(
                    MSt8, BinaryTotalSizeBytes
                ),
                {MSt10, AllocSizeReg} = term_binary_heap_size(
                    {free, BinaryTotalSizeBytes0}, MMod, MSt9
                ),
                case Alloc of
                    0 ->
                        {MSt10, BinaryTotalSizeBytes, AllocSizeReg};
                    _ ->
                        MSt11 = MMod:add(MSt10, AllocSizeReg, Alloc),
                        {MSt11, BinaryTotalSizeBytes, AllocSizeReg}
                end
        end,
    {MSt13, MemoryEnsureFreeReg} = MMod:call_primitive(
        MSt12, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
            ctx, jit_state, {free, AllocSize}, Live, ?MEMORY_CAN_SHRINK
        ]
    ),
    MSt14 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt13),
    {MSt17, InitialCreatedBin} =
        case ReuseSourceBinary of
            false ->
                % No reuse - create the binary now
                {MSt15, CreatedBinResult} = MMod:call_primitive(
                    MSt14, ?PRIM_TERM_CREATE_EMPTY_BINARY, [
                        ctx, {free, BinaryTotalSizeInBytes}
                    ]
                ),
                MSt16 = MMod:if_block(MSt15, {CreatedBinResult, '==', ?TERM_INVALID_TERM}, fun(
                    BSt0
                ) ->
                    MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
                        ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
                    ])
                end),
                {MSt16, CreatedBinResult};
            true ->
                % Will reuse - defer creation until first segment
                {MSt14, {private_append, BinaryTotalSizeInBytes}}
        end,
    % We redo the decoding. Rest7 should still be equal to previous value.
    {Rest7, MSt18, FinalOffset, CreatedBin} = lists:foldl(
        fun(_Index, {AccRest0, AccMSt0, AccOffset0, AccCreatedBin}) ->
            {AtomTypeIndex, AccRest1} = decode_atom(AccRest0),
            AtomType = AtomResolver(AtomTypeIndex),
            {_Seg, AccRest2} = decode_literal(AccRest1),
            {SegmentUnit, AccRest3} = decode_literal(AccRest2),
            {AccMSt1, Flags, AccRest4} = decode_compact_term(AccRest3, MMod, AccMSt0, State1),
            {AccMSt2, Src, AccRest5} = decode_compact_term(AccRest4, MMod, AccMSt1, State1),
            {AccMSt3, Size, AccRest6} = decode_compact_term(AccRest5, MMod, AccMSt2, State1),
            ?TRACE("{~p,~p,~p,~p,~p,~p},", [AtomType, _Seg, SegmentUnit, Flags, Src, Size]),
            {AccMSt4, AccOffset1, AccCreatedBin1} = first_pass_bs_create_bin_insert_value(
                AtomType,
                Flags,
                Src,
                Size,
                SegmentUnit,
                Fail,
                AccCreatedBin,
                AccOffset0,
                MMod,
                AccMSt3
            ),
            AccMSt5 = MMod:free_native_registers(AccMSt4, [Flags, Src, Size]),
            {AccRest6, AccMSt5, AccOffset1, AccCreatedBin1}
        end,
        {Rest6, MSt17, 0, InitialCreatedBin},
        lists:seq(1, NBSegments)
    ),
    ?TRACE("]\n", []),
    MSt19 = MMod:free_native_registers(MSt18, [FinalOffset]),
    MSt20 = MMod:move_to_vm_register(MSt19, CreatedBin, Dest),
    MSt21 = MMod:free_native_registers(MSt20, [CreatedBin, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt21),
    first_pass(Rest7, MMod, MSt21, State1);
% 178
first_pass(<<?OP_CALL_FUN2, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Tag, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {ArgsCount, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_CALL_FUN2 ~p, ~p\n", [Tag, ArgsCount]),
    % We ignore Tag (could be literal 0 or atom unsafe)
    MSt2 = MMod:free_native_registers(MSt1, [Tag]),
    MSt3 = MMod:decrement_reductions_and_maybe_schedule_next(MSt2),
    State1 = record_continuation_line(MMod, MSt3, State0),
    {MSt4, Fun, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt3, State1),
    {MSt5, Reg} = verify_is_function(Fun, MMod, MSt4),
    MSt6 = MMod:call_primitive_with_cp(MSt5, ?PRIM_CALL_FUN, [
        ctx, jit_state, offset, {free, Reg}, ArgsCount
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest3, MMod, MSt6, State1);
% 179
first_pass(<<?OP_NIF_START, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    ?TRACE("OP_NIF_START\n", []),
    first_pass(Rest0, MMod, MSt0, State0);
% 180
first_pass(<<?OP_BADRECORD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, Arg1, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    ?TRACE("OP_BADRECORD ~p\n", [Arg1]),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADRECORD_ATOM, Arg1
    ]),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest1, MMod, MSt2, State0);
% 181
first_pass(
    <<?OP_UPDATE_RECORD, Rest0/binary>>, MMod, MSt0, #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {HintAtomIndex, Rest1} = decode_atom(Rest0),
    Hint = AtomResolver(HintAtomIndex),
    {Size, Rest2} = decode_literal(Rest1),
    {MSt1, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, SrcReg} = MMod:move_to_native_register(MSt1, Src),
    {MSt3, SrcReg} = MMod:and_(MSt2, {free, SrcReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Dest, Rest4} = decode_dest(Rest3, MMod, MSt3),
    {ListLen, Rest5} = decode_extended_list_header(Rest4),
    ?TRACE("OP_UPDATE_RECORD ~p, ~p, ~p, ~p, [", [Hint, Size, Src, Dest]),
    {MSt5, DestReg} = MMod:call_primitive(MSt4, ?PRIM_TERM_ALLOC_TUPLE, [ctx, Size]),
    {MSt6, DestReg} = MMod:and_(MSt5, {free, DestReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt7, ReuseReg} = MMod:move_to_native_register(
        MSt6,
        if
            Hint =:= reuse -> 1;
            true -> 0
        end
    ),
    MSt8 = lists:foldl(
        fun(Index, AccMSt0) ->
            {AccMSt1, SrcValue} = MMod:get_array_element(AccMSt0, SrcReg, Index),
            AccMSt2 = MMod:move_to_array_element(AccMSt1, SrcValue, DestReg, Index),
            MMod:free_native_registers(AccMSt2, [SrcValue])
        end,
        MSt7,
        lists:seq(1, Size)
    ),
    {MSt9, Rest6} = lists:foldl(
        fun(_Index, {AccMSt0, AccRest0}) ->
            {UpdateIx, AccRest1} = decode_literal(AccRest0),
            {AccMSt1, UpdateValue, AccRest2} = decode_compact_term(AccRest1, MMod, AccMSt0, State0),
            AccMSt2 = MMod:if_else_block(
                AccMSt1,
                {'(bool)', ReuseReg, '!=', false},
                fun(BSt0) ->
                    {BSt1, OldValueReg} = MMod:get_array_element(BSt0, DestReg, UpdateIx),
                    {BSt2, ResultReg} = MMod:call_primitive(BSt1, ?PRIM_TERM_COMPARE, [
                        ctx, jit_state, {free, OldValueReg}, UpdateValue, ?TERM_COMPARE_EXACT
                    ]),
                    BSt3 = handle_error_if(
                        {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, BSt2
                    ),
                    MMod:if_block(BSt3, {'(int)', {free, ResultReg}, '!=', ?TERM_EQUALS}, fun(ESt0) ->
                        ESt1 = MMod:move_to_array_element(ESt0, UpdateValue, DestReg, UpdateIx),
                        MMod:move_to_native_register(ESt1, 0, ReuseReg)
                    end)
                end,
                fun(BSt0) ->
                    MMod:move_to_array_element(BSt0, UpdateValue, DestReg, UpdateIx)
                end
            ),
            AccMSt3 = MMod:free_native_registers(AccMSt2, [UpdateValue]),
            {AccMSt3, AccRest2}
        end,
        {MSt8, Rest5},
        lists:seq(1, ListLen div 2)
    ),
    ?TRACE("]\n", []),
    MSt10 = MMod:if_else_block(
        MSt9,
        {'(bool)', {free, ReuseReg}, '!=', false},
        fun(BSt0) ->
            BSt1 = MMod:or_(BSt0, SrcReg, ?TERM_PRIMARY_BOXED),
            MMod:move_to_vm_register(BSt1, SrcReg, Dest)
        end,
        fun(BSt0) ->
            BSt1 = MMod:or_(BSt0, DestReg, ?TERM_PRIMARY_BOXED),
            MMod:move_to_vm_register(BSt1, DestReg, Dest)
        end
    ),
    MSt11 = MMod:free_native_registers(MSt10, [DestReg, SrcReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt11),
    first_pass(Rest6, MMod, MSt11, State0);
% 182
first_pass(<<?OP_BS_MATCH, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, MatchState, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListLen, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_BS_MATCH ~p, ~p, [", [Fail, MatchState]),
    {MSt2, MatchStateReg0} = MMod:move_to_native_register(MSt1, MatchState),
    {MSt3, MatchStateReg1} = MMod:and_(MSt2, MatchStateReg0, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, BSBinaryReg} = MMod:get_array_element(MSt3, MatchStateReg1, 1),
    {MSt5, BSOffsetReg} = MMod:get_array_element(MSt4, MatchStateReg1, 2),
    MSt6 = MMod:free_native_registers(MSt5, [MatchStateReg1]),
    {MSt7, BSBinaryReg} = MMod:and_(MSt6, {free, BSBinaryReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt8, Rest4, MatchStateReg2, NewBSOffsetReg} = first_pass_bs_match(
        Fail, MatchStateReg0, BSBinaryReg, BSOffsetReg, ListLen, Rest3, MMod, MSt7, State0
    ),
    ?TRACE("]\n", []),
    MSt9 = MMod:free_native_registers(MSt8, [BSBinaryReg, NewBSOffsetReg, MatchStateReg2]),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    first_pass(Rest4, MMod, MSt9, State0);
% 183
first_pass(<<?OP_EXECUTABLE_LINE, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {MSt1, {literal, _Location}, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {_LineNum, Rest2} = decode_literal(Rest1),
    ?TRACE("OP_EXECUTABLE_LINE ~p, ~p\n", [_Location, _LineNum]),
    MSt2 = ?DWARF_LINE(MMod, MSt1, _Location),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest2, MMod, MSt2, State0);
% 184
first_pass(
    <<?OP_DEBUG_LINE, Rest0/binary>>,
    MMod,
    MSt0,
    #state{debug_info_resolver = DebugInfoResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    Rest1 = skip_compact_term(Rest0),
    {MSt1, {literal, _Location}, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Index, Rest3} = decode_literal(Rest2),
    {_Live, Rest4} = decode_literal(Rest3),
    ?TRACE("OP_DEBUG_LINE ~p, ~p, ~p\n", [_Location, Index, _Live]),
    MSt2 = ?DWARF_LINE(MMod, MSt1, _Location),
    MSt3 =
        case DebugInfoResolver(Index) of
            false ->
                MSt2;
            _VarMappings ->
                ?DWARF_VARIABLES(MMod, MSt2, _VarMappings)
        end,
    ?ASSERT_ALL_NATIVE_FREE(MSt3),
    first_pass(Rest4, MMod, MSt3, State0);
% 185
first_pass(<<?OP_BIF3, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {Bif, Rest2} = decode_literal(Rest1),
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, Arg1, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    {MSt3, Arg2, Rest4} = decode_compact_term(Rest3, MMod, MSt2, State0),
    {MSt4, Arg3, Rest5} = decode_compact_term(Rest4, MMod, MSt3, State0),
    {MSt5, Dest, Rest6} = decode_dest(Rest5, MMod, MSt4),
    ?TRACE("OP_BIF3 ~p, ~p, ~p, ~p, ~p, ~p\n", [FailLabel, Bif, Arg1, Arg2, Arg3, Dest]),
    {MSt6, ResultReg} = MMod:call_func_ptr(MSt5, {free, FuncPtr}, [
        ctx, FailLabel, {free, Arg1}, {free, Arg2}, {free, Arg3}
    ]),
    MSt7 = bif_faillabel_test(FailLabel, MMod, MSt6, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest6, MMod, MSt7, State0);
% 186
first_pass(<<?OP_IS_ANY_NATIVE_RECORD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    ?TRACE("OP_IS_ANY_NATIVE_RECORD ~p, ~p\n", [Label, Src]),
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Src),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, TagReg} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = cond_jump_to_label(
        {TagReg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_RECORD}, Label, MMod, MSt5
    ),
    MSt7 = MMod:free_native_registers(MSt6, [Reg, TagReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt7),
    first_pass(Rest2, MMod, MSt7, State0);
% 187
first_pass(
    <<?OP_IS_NATIVE_RECORD, Rest0/binary>>,
    MMod,
    MSt0,
    #state{atom_resolver = AtomResolver, record_resolver = RecordResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ModAtomIndex, Rest3} = decode_atom(Rest2),
    {NameAtomIndex, Rest4} = decode_atom(Rest3),
    ?TRACE("OP_IS_NATIVE_RECORD ~p, ~p, ~p, ~p\n", [Label, Src, ModAtomIndex, NameAtomIndex]),
    {MSt2, ModAtom} =
        case maps:find(AtomResolver(ModAtomIndex), ?DEFAULT_ATOMS) of
            error ->
                MMod:call_primitive(
                    MSt1, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, ModAtomIndex]
                );
            {ok, ModVal} ->
                {MSt1, ModVal}
        end,
    {MSt3, NameAtom} =
        case maps:find(AtomResolver(NameAtomIndex), ?DEFAULT_ATOMS) of
            error ->
                MMod:call_primitive(
                    MSt2, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, NameAtomIndex]
                );
            {ok, NameVal} ->
                {MSt2, NameVal}
        end,
    {MSt4, ResultReg} = MMod:call_primitive(MSt3, ?PRIM_IS_RECORD_OF, [
        {free, Src}, {free, ModAtom}, {free, NameAtom}
    ]),
    MSt5 = cond_jump_to_label({{free, ResultReg}, '==', 0}, Label, MMod, MSt4),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    %% On the fall-through edge, src is proven to be a record of (Mod, Name).
    %% If the record is module-local, mark src so subsequent GET_RECORD_* /
    %% IS_RECORD_ACCESSIBLE opcodes can specialize via JIT-time offsets. The
    %% backend's `set_vm_record_type' overwrites any prior assertion for src.
    MSt6 = maybe_track_record_type(
        Src, ModAtomIndex, NameAtomIndex, AtomResolver, RecordResolver, MMod, MSt5
    ),
    first_pass(Rest4, MMod, MSt6, State0);
% 188
first_pass(
    <<?OP_GET_RECORD_ELEMENTS, Rest0/binary>>,
    MMod,
    MSt0,
    State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Fail, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ListLen, Rest3} = decode_extended_list_header(Rest2),
    ?TRACE("OP_GET_RECORD_ELEMENTS ~p, ~p, ~p\n", [Fail, Src, ListLen]),
    NumPairs = ListLen div 2,
    case MMod:get_vm_record_type(MSt1, Src) of
        #{fields := FieldAtoms} ->
            get_record_elements_resolved(
                Src, FieldAtoms, Fail, NumPairs, Rest3, MMod, MSt1, State0
            );
        undefined ->
            get_record_elements_generic(
                Src, Fail, NumPairs, Rest3, MMod, MSt1, State0
            )
    end;
% 189
first_pass(<<?OP_PUT_RECORD, Rest0/binary>>, MMod, MSt0, State0) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {_Fail, Rest1} = decode_label(Rest0),
    case peek_local_record_id(Rest1, State0) of
        {ok, RecInfo, Rest2Local} ->
            put_record_resolved(Rest2Local, RecInfo, MMod, MSt0, State0);
        not_local ->
            put_record_generic(Rest1, MMod, MSt0, State0)
    end;
% 190
first_pass(
    <<?OP_IS_RECORD_ACCESSIBLE, Rest0/binary>>,
    MMod,
    MSt0,
    #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {Label, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {ScopeAtomIndex, Rest3} = decode_atom(Rest2),
    ?TRACE("OP_IS_RECORD_ACCESSIBLE ~p, ~p, ~p\n", [Label, Src, ScopeAtomIndex]),
    case MMod:get_vm_record_type(MSt1, Src) of
        #{is_exported := IsExported} ->
            %% Flow-proven module-local record: def->module is the current
            %% module, so jit_is_record_accessible passes iff the record is
            %% exported OR scope is not 'external'. Both inputs are known at
            %% JIT time — constant-fold.
            ScopeAtom = AtomResolver(ScopeAtomIndex),
            case IsExported orelse ScopeAtom =/= external of
                true ->
                    %% Statically passes — no code, no jump.
                    ?ASSERT_ALL_NATIVE_FREE(MSt1),
                    first_pass(Rest3, MMod, MSt1, State0);
                false ->
                    %% Statically fails — unconditional jump to Label.
                    MSt2 = MMod:jump_to_label(MSt1, Label),
                    ?ASSERT_ALL_NATIVE_FREE(MSt2),
                    first_pass(Rest3, MMod, MSt2, State0)
            end;
        undefined ->
            {MSt2, ScopeAtom} =
                case maps:find(AtomResolver(ScopeAtomIndex), ?DEFAULT_ATOMS) of
                    error ->
                        MMod:call_primitive(
                            MSt1, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, ScopeAtomIndex]
                        );
                    {ok, ScopeVal} ->
                        {MSt1, ScopeVal}
                end,
            {MSt3, ResultReg} = MMod:call_primitive(MSt2, ?PRIM_IS_RECORD_ACCESSIBLE, [
                ctx, jit_state, {free, Src}, {free, ScopeAtom}
            ]),
            MSt4 = cond_jump_to_label({{free, ResultReg}, '==', 0}, Label, MMod, MSt3),
            ?ASSERT_ALL_NATIVE_FREE(MSt4),
            first_pass(Rest3, MMod, MSt4, State0)
    end;
% 191
first_pass(
    <<?OP_GET_RECORD_FIELD, Rest0/binary>>,
    MMod,
    MSt0,
    #state{atom_resolver = AtomResolver} = State0
) ->
    ?ASSERT_ALL_NATIVE_FREE(MSt0),
    {FailLabel, Rest1} = decode_label(Rest0),
    {MSt1, Src, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    case MMod:get_vm_record_type(MSt1, Src) of
        #{fields := FieldAtoms} ->
            %% Flow-proven type — skip the runtime record check and the
            %% per-field name lookup. Discard Id and FailLabel (the
            %% record-type guard is statically satisfied; no field can be
            %% missing because the JIT verified the layout at compile time).
            Rest3 = skip_compact_term(Rest2),
            {FieldAtomIndex, Rest4} = decode_atom(Rest3),
            {MSt2, Dest, Rest5} = decode_dest(Rest4, MMod, MSt1),
            FieldAtom = AtomResolver(FieldAtomIndex),
            case maps:find(FieldAtom, field_position_map(FieldAtoms)) of
                {ok, Position} ->
                    {MSt3, SrcReg} = MMod:move_to_native_register(MSt2, Src),
                    {MSt4, SrcReg} = MMod:and_(MSt3, {free, SrcReg}, ?TERM_PRIMARY_CLEAR_MASK),
                    MSt5 = MMod:move_array_element(MSt4, SrcReg, Position, Dest),
                    MSt6 = MMod:free_native_registers(MSt5, [SrcReg, Dest]),
                    ?ASSERT_ALL_NATIVE_FREE(MSt6),
                    first_pass(Rest5, MMod, MSt6, State0);
                error ->
                    %% Field name not present in the tracked layout — fall back
                    %% to the generic primitive path, which does its own runtime
                    %% type check. Re-decode from Rest2 (Id, FieldAtomIndex,
                    %% Dest) using the generic decoder.
                    get_record_field_generic(
                        FailLabel, Src, Rest2, MMod, MSt1, State0
                    )
            end;
        undefined ->
            get_record_field_generic(FailLabel, Src, Rest2, MMod, MSt1, State0)
    end.

%% @doc Generic OP_GET_RECORD_FIELD path: src isn't tracked (or tracking was
%% stale), so the runtime PRIM_GET_RECORD_FIELD primitive does its own type
%% check against `Id' and resolves the field offset by name.
get_record_field_generic(
    FailLabel,
    Src,
    Rest2,
    MMod,
    MSt1,
    #state{atom_resolver = AtomResolver} = State0
) ->
    {MSt2, Id, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    {FieldAtomIndex, Rest4} = decode_atom(Rest3),
    {MSt3, Dest, Rest5} = decode_dest(Rest4, MMod, MSt2),
    ?TRACE("OP_GET_RECORD_FIELD ~p, ~p, ~p, ~p, ~p\n", [
        FailLabel, Src, Id, FieldAtomIndex, Dest
    ]),
    {MSt4, FieldAtom} =
        case maps:find(AtomResolver(FieldAtomIndex), ?DEFAULT_ATOMS) of
            error ->
                MMod:call_primitive(
                    MSt3, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, FieldAtomIndex]
                );
            {ok, FieldVal} ->
                {MSt3, FieldVal}
        end,
    {MSt5, ResultReg} = MMod:call_primitive(MSt4, ?PRIM_GET_RECORD_FIELD, [
        ctx, FailLabel, {free, Src}, {free, Id}, {free, FieldAtom}
    ]),
    MSt6 = bif_faillabel_test(FailLabel, MMod, MSt5, {free, ResultReg}, {free, Dest}),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest5, MMod, MSt6, State0).

first_pass_bs_create_bin_compute_size(
    AtomType, Src, _Size, _SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when AtomType =:= utf8 orelse AtomType =:= utf16 ->
    {MSt1, SrcValue} = term_to_int(Src, Fail, MMod, MSt0),
    {MSt2, ResultReg} =
        case AtomType of
            utf8 ->
                MMod:call_primitive(MSt1, ?PRIM_BITSTRING_UTF8_SIZE, [{free, SrcValue}]);
            utf16 ->
                MMod:call_primitive(MSt1, ?PRIM_BITSTRING_UTF16_SIZE, [{free, SrcValue}])
        end,
    MSt3 = cond_raise_badarg_or_jump_to_fail_label(
        {ResultReg, '==', 0}, Fail, MMod, MSt2
    ),
    MSt4 = MMod:shift_left(MSt3, ResultReg, 3),
    case AccSizeReg0 of
        undefined ->
            {MSt4, AccLiteralSize0, ResultReg, State0};
        _ ->
            MSt5 = MMod:add(MSt4, AccSizeReg0, ResultReg),
            MSt6 = MMod:free_native_registers(MSt5, [ResultReg]),
            {MSt6, AccLiteralSize0, AccSizeReg0, State0}
    end;
first_pass_bs_create_bin_compute_size(
    utf32, Src, _Size, _SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) ->
    MSt1 = verify_is_integer(Src, Fail, MMod, MSt0),
    {MSt1, AccLiteralSize0 + 32, AccSizeReg0, State0};
first_pass_bs_create_bin_compute_size(
    float, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) ->
    MSt1 = verify_is_number(Src, Fail, MMod, MSt0),
    {MSt2, SizeValue} = term_to_int(Size, Fail, MMod, MSt1),
    % The field width is Size * Unit bits; account for the product (the insert
    % primitive validates it is 16/32/64), so a valid unit allocates the right
    % size and the offset advances correctly.
    if
        is_integer(SizeValue) ->
            {MSt2, AccLiteralSize0 + (SizeValue * SegmentUnit), AccSizeReg0, State0};
        is_atom(SizeValue) ->
            % Validate the product Size * Unit, not Size alone.
            MSt3 = MMod:mul(MSt2, SizeValue, SegmentUnit),
            MSt4 = cond_raise_badarg_or_jump_to_fail_label(
                {'and', [
                    {SizeValue, '!=', 16},
                    {SizeValue, '!=', 32},
                    {SizeValue, '!=', 64}
                ]},
                Fail,
                MMod,
                MSt3
            ),
            case AccSizeReg0 of
                undefined ->
                    {MSt4, AccLiteralSize0, SizeValue, State0};
                _ ->
                    MSt5 = MMod:add(MSt4, AccSizeReg0, SizeValue),
                    MSt6 = MMod:free_native_registers(MSt5, [SizeValue]),
                    {MSt6, AccLiteralSize0, AccSizeReg0, State0}
            end
    end;
first_pass_bs_create_bin_compute_size(
    integer, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) ->
    MSt1 = verify_is_any_integer(Src, Fail, MMod, MSt0),
    MSt2 = verify_is_integer(Size, Fail, MMod, MSt1),
    {MSt3, SizeValue} = term_to_int(Size, 0, MMod, MSt2),
    MSt5 =
        if
            is_integer(SizeValue) andalso SizeValue > 0 ->
                MSt3;
            is_integer(SizeValue) andalso Fail =:= 0 ->
                MMod:call_primitive_last(MSt3, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARG_ATOM
                ]);
            is_integer(SizeValue) andalso Fail =/= 0 ->
                MMod:jump_to_label(MSt3, Fail);
            true ->
                cond_raise_badarg_or_jump_to_fail_label(
                    {SizeValue, '<', 0}, Fail, MMod, MSt3
                )
        end,
    if
        is_integer(SizeValue) ->
            {MSt5, AccLiteralSize0 + (SizeValue * SegmentUnit), AccSizeReg0, State0};
        true ->
            MSt6 = MMod:mul(MSt5, SizeValue, SegmentUnit),
            case AccSizeReg0 of
                undefined ->
                    {MSt6, AccLiteralSize0, SizeValue, State0};
                _ ->
                    MSt7 = MMod:add(MSt6, AccSizeReg0, SizeValue),
                    MSt8 = MMod:free_native_registers(MSt7, [SizeValue]),
                    {MSt8, AccLiteralSize0, AccSizeReg0, State0}
            end
    end;
first_pass_bs_create_bin_compute_size(
    string, _Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt1, State0
) ->
    MSt2 = verify_is_integer(Size, Fail, MMod, MSt1),
    {MSt3, SizeValue} = term_to_int(Size, 0, MMod, MSt2),
    MSt5 =
        if
            is_integer(SizeValue) andalso SizeValue > 0 ->
                MSt3;
            is_integer(SizeValue) andalso Fail =:= 0 ->
                MMod:call_primitive_last(MSt3, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARG_ATOM
                ]);
            is_integer(SizeValue) andalso Fail =/= 0 ->
                MMod:jump_to_label(MSt3, Fail)
        end,
    {MSt5, AccLiteralSize0 + (SizeValue * SegmentUnit), AccSizeReg0, State0};
first_pass_bs_create_bin_compute_size(
    AtomType, Src, ?ALL_ATOM, _SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when AtomType =:= binary orelse AtomType =:= append orelse AtomType =:= private_append ->
    MSt1 = verify_is_binary(Src, Fail, MMod, MSt0),
    {MSt2, Reg} = MMod:copy_to_native_register(MSt1, Src),
    {MSt3, Reg} = MMod:and_(MSt2, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 1, Reg),
    MSt5 = MMod:shift_left(MSt4, Reg, 3),
    case AccSizeReg0 of
        undefined ->
            {MSt5, AccLiteralSize0, Reg, State0};
        _ ->
            MSt6 = MMod:add(MSt5, AccSizeReg0, Reg),
            MSt7 = MMod:free_native_registers(MSt6, [Reg]),
            {MSt7, AccLiteralSize0, AccSizeReg0, State0}
    end;
first_pass_bs_create_bin_compute_size(
    AtomType, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when
    (AtomType =:= binary orelse AtomType =:= append orelse
        AtomType =:= private_append) andalso is_integer(Size) andalso Size > 0
->
    MSt1 = verify_is_binary(Src, Fail, MMod, MSt0),
    {MSt2, SizeValue} = term_to_int(Size, 0, MMod, MSt1),
    RequiredBits = SizeValue * SegmentUnit,
    % Verify the source binary is large enough to provide RequiredBits bits.
    % Without this check the insert step would read past the end of the source
    % (memcpy out of bounds). The emulator performs the same check, and the
    % register-size clause below does it for non-literal sizes.
    {MSt3, SrcBitsReg} = MMod:copy_to_native_register(MSt2, Src),
    {MSt4, SrcBitsReg} = MMod:and_(MSt3, {free, SrcBitsReg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, SrcBitsReg, 1, SrcBitsReg),
    MSt6 = MMod:shift_left(MSt5, SrcBitsReg, 3),
    MSt7 = cond_raise_badarg_or_jump_to_fail_label(
        {SrcBitsReg, '<', RequiredBits}, Fail, MMod, MSt6
    ),
    MSt8 = MMod:free_native_registers(MSt7, [SrcBitsReg]),
    {MSt8, AccLiteralSize0 + RequiredBits, AccSizeReg0, State0};
first_pass_bs_create_bin_compute_size(
    AtomType, Src, Size, SegmentUnit, Fail, AccLiteralSize0, AccSizeReg0, MMod, MSt0, State0
) when AtomType =:= binary orelse AtomType =:= append orelse AtomType =:= private_append ->
    MSt1 = verify_is_binary(Src, Fail, MMod, MSt0),
    {MSt2, Reg0} = MMod:copy_to_native_register(MSt1, Size),
    {MSt3, Reg1} = MMod:copy_to_native_register(MSt2, Src),
    {MSt4, Reg1} = MMod:and_(MSt3, {free, Reg1}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg1, 1, Reg1),
    MSt6 = MMod:shift_left(MSt5, Reg1, 3),
    MSt7 = MMod:if_block(MSt6, {{free, Reg0}, '!=', ?ALL_ATOM}, fun(BSt0) ->
        {BSt1, SizeReg} = term_to_int(Size, Fail, MMod, BSt0),
        BSt2 = cond_raise_badarg_or_jump_to_fail_label(
            {SizeReg, '<', 0}, Fail, MMod, BSt1
        ),
        BSt3 = MMod:mul(BSt2, SizeReg, SegmentUnit),
        BSt4 = cond_raise_badarg_or_jump_to_fail_label(
            {Reg1, '<', SizeReg}, Fail, MMod, BSt3
        ),
        BSt5 = MMod:move_to_native_register(BSt4, SizeReg, Reg1),
        MMod:free_native_registers(BSt5, [SizeReg])
    end),
    case AccSizeReg0 of
        undefined ->
            {MSt7, AccLiteralSize0, Reg1, State0};
        _ ->
            MSt8 = MMod:add(MSt7, AccSizeReg0, Reg1),
            MSt9 = MMod:free_native_registers(MSt8, [Reg1]),
            {MSt9, AccLiteralSize0, AccSizeReg0, State0}
    end.

first_pass_bs_create_bin_insert_value(
    utf8, _Flags, Src, _Size, _SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, SrcValue} = utf_term_to_int(Src, Fail, MMod, MSt0),
    {MSt2, Size} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_INSERT_UTF8, [
        CreatedBin, Offset, {free, SrcValue}
    ]),
    {MSt3, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt2, Offset, Size, 8
    ),
    {MSt3, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    utf16, Flags, Src, _Size, _SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, FlagsValue} = decode_flags_list(Flags, MMod, MSt0),
    {MSt2, SrcValue} = utf_term_to_int(Src, Fail, MMod, MSt1),
    {MSt3, Size} = MMod:call_primitive(MSt2, ?PRIM_BITSTRING_INSERT_UTF16, [
        CreatedBin, Offset, {free, SrcValue}, {free, FlagsValue}
    ]),
    {MSt4, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt3, Offset, Size, 8
    ),
    {MSt4, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    utf32, Flags, Src, _Size, _SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, FlagsValue} = decode_flags_list(Flags, MMod, MSt0),
    {MSt2, SrcValue} = utf_term_to_int(Src, Fail, MMod, MSt1),
    {MSt3, BoolResult} = MMod:call_primitive(MSt2, ?PRIM_BITSTRING_INSERT_UTF32, [
        CreatedBin, Offset, {free, SrcValue}, {free, FlagsValue}
    ]),
    MSt4 = cond_raise_badarg_or_jump_to_fail_label(
        {'(bool)', {free, BoolResult}, '==', false}, Fail, MMod, MSt3
    ),
    {MSt5, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt4, Offset, 4, 8
    ),
    {MSt5, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    integer, Flags, Src, Size, SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    % term_to_int can raise a badarg and use a temp register for this, start
    % with it.
    {MSt1, SizeValue0} = term_to_int(Size, Fail, MMod, MSt0),
    % Because we're calling a function without ctx as an arg, we need to move
    % the value now to a register
    {MSt2, SrcReg} = MMod:move_to_native_register(MSt1, Src),
    {MSt3, FlagsValue} = decode_flags_list(Flags, MMod, MSt2),
    {MSt4, SizeValue} =
        if
            % Literal size: compute size*unit directly. mul/3 cannot take a
            % literal in its register slot (it would dispatch to shift_left/3
            % and crash), and an integer is immutable so the in-place product
            % below would be lost anyway.
            is_integer(SizeValue0) ->
                {MSt3, SizeValue0 * SegmentUnit};
            true ->
                % mul mutates the register in place to hold size*unit.
                {MMod:mul(MSt3, SizeValue0, SegmentUnit), SizeValue0}
        end,
    {MSt5, BoolResult} = MMod:call_primitive(MSt4, ?PRIM_BITSTRING_INSERT_INTEGER, [
        CreatedBin, Offset, {free, SrcReg}, SizeValue, {free, FlagsValue}
    ]),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'(bool)', {free, BoolResult}, '==', false}, Fail, MMod, MSt5
    ),
    {MSt7, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt6, Offset, SizeValue, 1
    ),
    {MSt7, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    float, Flags, Src, Size, SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, SrcReg} = MMod:move_to_native_register(MSt0, Src),
    {MSt2, FlagsValue} = decode_flags_list(Flags, MMod, MSt1),
    {MSt3, SizeValue} = term_to_int(Size, Fail, MMod, MSt2),
    % The field width is Size * Unit bits; pass the product (validated as
    % 16/32/64 by the primitive), not Size alone, and advance by it.
    {MSt4, TotalSize} =
        if
            is_integer(SizeValue) ->
                {MSt3, SizeValue * SegmentUnit};
            true ->
                MSt3b = MMod:mul(MSt3, SizeValue, SegmentUnit),
                {MSt3b, SizeValue}
        end,
    % Call single primitive with size parameter
    {MSt5, BoolResult} = MMod:call_primitive(MSt4, ?PRIM_BITSTRING_INSERT_FLOAT, [
        CreatedBin, Offset, {free, SrcReg}, TotalSize, {free, FlagsValue}
    ]),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'(bool)', {free, BoolResult}, '==', false}, Fail, MMod, MSt5
    ),
    {MSt7, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt6, Offset, TotalSize, 1
    ),
    {MSt7, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    string, _Flags, Src, Size, SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) ->
    {MSt1, SrcValue} = term_to_int(Src, Fail, MMod, MSt0),
    {MSt2, SizeValue} = term_to_int(Size, Fail, MMod, MSt1),
    true = is_integer(SizeValue) andalso is_integer(SegmentUnit),
    BitSize = SizeValue * SegmentUnit,
    {MSt4, VoidResult} = MMod:call_primitive(MSt2, ?PRIM_BITSTRING_COPY_MODULE_STR, [
        ctx, jit_state, CreatedBin, Offset, {free, SrcValue}, BitSize
    ]),
    MSt5 = MMod:free_native_registers(MSt4, [VoidResult]),
    {MSt6, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt5, Offset, BitSize, 1
    ),
    {MSt6, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    private_append,
    _Flags,
    Src,
    _Size,
    _SegmentUnit,
    _Fail,
    {private_append, BinaryTotalSizeInBytes},
    Offset,
    MMod,
    MSt0
) ->
    % Special case: first segment is private_append with undefined CreatedBin
    % Get original size before reusing
    {MSt1, OriginalSize} = term_binary_size(Src, MMod, MSt0),
    % Reuse the source binary (content is already there, no need to copy)
    {MSt2, CreatedBin} = MMod:call_primitive(MSt1, ?PRIM_TERM_REUSE_BINARY, [
        ctx, {free, Src}, {free, BinaryTotalSizeInBytes}
    ]),
    MSt3 = MMod:if_block(MSt2, {CreatedBin, '==', ?TERM_INVALID_TERM}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?OUT_OF_MEMORY_ATOM
        ])
    end),
    % Convert original size to bits and update offset
    MSt4 = MMod:shift_left(MSt3, OriginalSize, 3),
    {MSt5, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt4, Offset, OriginalSize, 1
    ),
    {MSt5, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    AtomType, _Flags, Src, Size, SegmentUnit, Fail, CreatedBin, Offset, MMod, MSt0
) when AtomType =:= binary orelse AtomType =:= append orelse AtomType =:= private_append ->
    {MSt4, SizeInBits} =
        if
            is_integer(Size) andalso Size band 16#F =:= ?TERM_INTEGER_TAG ->
                {MSt0, term_from_int(SegmentUnit * (Size bsr 4))};
            Size =:= ?ALL_ATOM ->
                {MSt0, Size};
            SegmentUnit =:= 1 ->
                {MSt1, SizeReg} = MMod:move_to_native_register(MSt0, Size),
                MSt2 = cond_raise_badarg_or_jump_to_fail_label(
                    {SizeReg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, Fail, MMod, MSt1
                ),
                {MSt2, {free, SizeReg}};
            true ->
                {MSt1, SizeReg} = term_to_int(Size, Fail, MMod, MSt0),
                MSt2 = MMod:mul(MSt1, SizeReg, SegmentUnit),
                {MSt3, SizeReg} = term_from_int(SizeReg, MMod, MSt2),
                {MSt3, {free, SizeReg}}
        end,
    {MSt5, SizeValue} = MMod:call_primitive(MSt4, ?PRIM_BITSTRING_COPY_BINARY, [
        ctx, jit_state, CreatedBin, Offset, Src, SizeInBits
    ]),
    MSt6 = MMod:if_block(MSt5, {SizeValue, '<', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_HANDLE_ERROR, [
            ctx, jit_state, offset
        ])
    end),
    {MSt7, NewOffset} = first_pass_bs_create_bin_insert_value_increment_offset(
        MMod, MSt6, Offset, SizeValue, 1
    ),
    {MSt7, NewOffset, CreatedBin};
first_pass_bs_create_bin_insert_value(
    _OtherType, _Flag, _Src, _Size, _SegmentUnit, _Fail, CreatedBin, Offset, _MMod, MSt0
) ->
    {MSt0, Offset, CreatedBin}.

first_pass_bs_create_bin_insert_value_increment_offset(_MMod, MSt0, Offset, Size, Unit) when
    is_integer(Offset) andalso is_integer(Size) andalso is_integer(Unit)
->
    {MSt0, Offset + (Size * Unit)};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, 0, Size, 8) when is_atom(Size) ->
    MSt1 = MMod:shift_left(MSt0, Size, 3),
    {MSt1, Size};
first_pass_bs_create_bin_insert_value_increment_offset(_MMod, MSt0, 0, Size, 1) ->
    {MSt0, Size};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 8) when
    is_integer(Offset) andalso is_atom(Size)
->
    MSt1 = MMod:shift_left(MSt0, Size, 3),
    MSt2 = MMod:add(MSt1, Size, Offset),
    {MSt2, Size};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 1) when
    is_integer(Offset)
->
    MSt1 = MMod:add(MSt0, Size, Offset),
    {MSt1, Size};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, Unit) when
    is_integer(Size) andalso is_integer(Unit)
->
    MSt1 = MMod:add(MSt0, Offset, Size * Unit),
    {MSt1, Offset};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 8) when
    is_atom(Size)
->
    MSt1 = MMod:shift_left(MSt0, Size, 3),
    MSt2 = MMod:add(MSt1, Offset, Size),
    MSt3 = MMod:free_native_registers(MSt2, [Size]),
    {MSt3, Offset};
first_pass_bs_create_bin_insert_value_increment_offset(MMod, MSt0, Offset, Size, 1) ->
    MSt1 = MMod:add(MSt0, Offset, Size),
    MSt2 = MMod:free_native_registers(MSt1, [Size]),
    {MSt2, Offset}.

first_pass_bs_match(_Fail, MatchState, _BSBinaryReg, BSOffsetReg, 0, Rest, _MMod, MSt, _State) ->
    {MSt, Rest, MatchState, BSOffsetReg};
first_pass_bs_match(
    Fail,
    MatchState,
    BSBinaryReg,
    BSOffsetReg,
    J0,
    Rest0,
    MMod,
    MSt0,
    #state{atom_resolver = AtomResolver} = State0
) ->
    {CommandAtomIndex, Rest1} = decode_atom(Rest0),
    Command = AtomResolver(CommandAtomIndex),
    J1 = J0 - 1,
    {J2, Rest2, NewMatchState, NewBSOffsetReg, MSt1} =
        case Command of
            ensure_at_least ->
                first_pass_bs_match_ensure_at_least(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            ensure_exactly ->
                first_pass_bs_match_ensure_exactly(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            integer ->
                first_pass_bs_match_integer(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            binary ->
                first_pass_bs_match_binary(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0, State0
                );
            get_tail ->
                first_pass_bs_match_get_tail(
                    MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            '=:=' ->
                first_pass_bs_match_equal_colon_equal(
                    Fail, MatchState, BSBinaryReg, BSOffsetReg, J1, Rest1, MMod, MSt0
                );
            skip ->
                first_pass_bs_match_skip(MatchState, BSOffsetReg, J1, Rest1, MMod, MSt0)
        end,
    % offset needs to be updated in the loop
    {MSt2, MatchStateReg1} = MMod:and_(MSt1, NewMatchState, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_to_array_element(MSt2, NewBSOffsetReg, MatchStateReg1, 2),
    MSt4 = MMod:free_native_registers(MSt3, [MatchStateReg1]),
    first_pass_bs_match(
        Fail, NewMatchState, BSBinaryReg, NewBSOffsetReg, J2, Rest2, MMod, MSt4, State0
    ).

first_pass_bs_match_ensure_at_least(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0
) ->
    {Stride, Rest1} = decode_literal(Rest0),
    if
        Stride < 0 ->
            MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, offset, ?BADARG_ATOM
            ]),
            {J0, Rest0, MatchState, BSOffsetReg, MSt1};
        true ->
            {Unit, Rest2} = decode_literal(Rest1),
            ?TRACE("{ensure_at_least,~p,~p},", [Stride, Unit]),
            {MSt1, Reg} = MMod:get_array_element(MSt0, BSBinaryReg, 1),
            MSt2 = MMod:shift_left(MSt1, Reg, 3),
            % Reg is bs_bin_size * 8
            MSt3 = MMod:sub(MSt2, Reg, BSOffsetReg),
            % Reg is (bs_bin_size * 8) - bs_offset = remaining bits
            MSt4 = cond_jump_to_label({Reg, '<', Stride}, Fail, MMod, MSt3),
            % Also check unit alignment: (remaining - stride) % unit == 0
            MSt7 =
                if
                    Unit > 1 ->
                        MSt4b = MMod:sub(MSt4, Reg, Stride),
                        {MSt5, UnitReg} = MMod:and_(MSt4b, {free, Reg}, Unit - 1),
                        MSt6 = cond_jump_to_label({{free, UnitReg}, '!=', 0}, Fail, MMod, MSt5),
                        MSt6;
                    true ->
                        MMod:free_native_registers(MSt4, [Reg])
                end,
            {J0 - 2, Rest2, MatchState, BSOffsetReg, MSt7}
    end.

first_pass_bs_match_ensure_exactly(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0
) ->
    {Stride, Rest1} = decode_literal(Rest0),
    if
        Stride < 0 ->
            MSt1 = MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                ctx, jit_state, offset, ?BADARG_ATOM
            ]),
            {J0, Rest0, MatchState, BSOffsetReg, MSt1};
        true ->
            ?TRACE("{ensure_exactly,~p},", [Stride]),
            {MSt1, Reg} = MMod:get_array_element(MSt0, BSBinaryReg, 1),
            MSt2 = MMod:shift_left(MSt1, Reg, 3),
            % Reg is bs_bin_size * 8 (use unit instead ??)
            MSt3 = MMod:sub(MSt2, Reg, BSOffsetReg),
            % Reg is (bs_bin_size * 8) - bs_offset
            MSt4 = cond_jump_to_label({Reg, '!=', Stride}, Fail, MMod, MSt3),
            MSt5 = MMod:free_native_registers(MSt4, [Reg]),
            {J0 - 1, Rest1, MatchState, BSOffsetReg, MSt5}
    end.

first_pass_bs_match_integer(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0, State0
) ->
    {_Live, Rest1} = decode_literal(Rest0),
    {Flags, Rest2} = decode_compile_time_literal(Rest1, State0),
    {MSt1, FlagsValue} = decode_flags_list(Flags, MMod, MSt0),
    {MSt2, Size, Rest3} = decode_typed_compact_term(Rest2, MMod, MSt0, State0),
    {Unit, Rest4} = decode_literal(Rest3),
    ?TRACE("{integer,~p,~p,~p, ", [Flags, Size, Unit]),
    {MSt3, SizeReg} = term_to_int(Size, 0, MMod, MSt1),
    {MSt6, NumBits} =
        if
            is_integer(SizeReg) ->
                {MSt2, SizeReg * Unit};
            true ->
                MSt3 = MMod:mul(SizeReg, Unit),
                {MSt3, SizeReg}
        end,
    {MSt7, Result} = MMod:call_primitive(MSt6, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, BSBinaryReg, BSOffsetReg, NumBits, {free, FlagsValue}
    ]),
    MSt8 = handle_error_if({Result, '==', 0}, MMod, MSt7),
    MSt9 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt8),
    MSt10 =
        case MMod:available_regs(MSt9) of
            [] ->
                MMod:free_native_registers(MSt9, [BSOffsetReg]);
            _ ->
                MSt9
        end,
    {MSt11, Dest, Rest5} = decode_dest(Rest4, MMod, MSt10),
    ?TRACE("~p},", [Dest]),
    MSt12 = MMod:move_to_vm_register(MSt11, Result, Dest),
    MSt13 = MMod:free_native_registers(MSt12, [Result, Dest]),
    case MMod:available_regs(MSt9) of
        [] ->
            {MSt14, MatchState} = MMod:and_(MSt13, {free, MatchState}, ?TERM_PRIMARY_CLEAR_MASK),
            {MSt15, NewBSOffsetReg} = MMod:get_array_element(MSt14, MatchState, 2),
            MSt16 = MMod:or_(MSt15, MatchState, ?TERM_PRIMARY_BOXED),
            MSt17 = MMod:add(MSt16, NewBSOffsetReg, NumBits),
            MSt18 = MMod:free_native_registers(MSt17, [NumBits]),
            {J0 - 5, Rest5, MatchState, NewBSOffsetReg, MSt18};
        _ ->
            MSt14 = MMod:add(MSt13, BSOffsetReg, NumBits),
            MSt15 = MMod:free_native_registers(MSt14, [NumBits]),
            {J0 - 5, Rest5, MatchState, BSOffsetReg, MSt15}
    end.

first_pass_bs_match_binary(
    Fail,
    MatchState,
    BSBinaryReg,
    BSOffsetReg,
    J0,
    Rest0,
    MMod,
    MSt0,
    State0
) ->
    {Live, Rest1} = decode_literal(Rest0),
    {_Flags, Rest2} = decode_compile_time_literal(Rest1, State0),
    %   {_FlagsValue, MSt1} = decode_flags_list(Flags, MMod, MSt0),
    {Size, Rest3} = decode_literal(Rest2),
    {Unit, Rest4} = decode_literal(Rest3),
    ?TRACE("{binary,~p,~p,~p,~p", [Live, _Flags, Size, Unit]),
    MatchedBits = Size * Unit,
    MSt1 =
        if
            MatchedBits rem 8 =:= 0 ->
                cond_raise_badarg({BSOffsetReg, '&', 2#111, '!=', 0}, MMod, MSt0);
            true ->
                MMod:call_primitive_last(MSt0, ?PRIM_RAISE_ERROR, [
                    ctx, jit_state, offset, ?BADARG_ATOM
                ])
        end,
    MatchedBytes = MatchedBits div 8,
    {MSt2, BSOffseBytesReg} = MMod:shift_right(MSt1, BSOffsetReg, 3),
    {MSt3, RemainingBytesReg} = MMod:get_array_element(MSt2, BSBinaryReg, 1),
    MSt4 = MMod:sub(MSt3, RemainingBytesReg, BSOffseBytesReg),
    MSt5 = cond_jump_to_label({RemainingBytesReg, '<', MatchedBytes}, Fail, MMod, MSt4),
    MSt6 = MMod:free_native_registers(MSt5, [RemainingBytesReg]),
    {MSt7, HeapSizeReg} = MMod:call_primitive(MSt6, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        BSBinaryReg, MatchedBytes
    ]),
    {MSt8, NewMatchState} = memory_ensure_free_with_extra_root(
        MatchState, Live, {free, HeapSizeReg}, MMod, MSt7
    ),
    % Restore BSBinaryReg as it may have been gc'd as well
    {MSt9, MatchStateReg0} = MMod:copy_to_native_register(MSt8, NewMatchState),
    {MSt10, MatchStateReg0} = MMod:and_(MSt9, {free, MatchStateReg0}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt11 = MMod:move_array_element(MSt10, MatchStateReg0, 1, BSBinaryReg),
    MSt12 = MMod:free_native_registers(MSt11, [MatchStateReg0]),
    {MSt13, ResultTerm} = MMod:call_primitive(MSt12, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, BSBinaryReg, {free, BSOffseBytesReg}, MatchedBytes
    ]),
    {MSt14, BSBinaryReg} = MMod:and_(MSt13, {free, BSBinaryReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt15, Dest, Rest5} = decode_dest(Rest4, MMod, MSt14),
    ?TRACE("~p},", [Dest]),
    MSt16 = MMod:move_to_vm_register(MSt15, ResultTerm, Dest),
    MSt17 = MMod:free_native_registers(MSt16, [ResultTerm]),
    MSt18 = MMod:add(MSt17, BSOffsetReg, MatchedBits),
    {J0 - 5, Rest5, NewMatchState, BSOffsetReg, MSt18}.

first_pass_bs_match_get_tail(MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0) ->
    {Live, Rest1} = decode_literal(Rest0),
    {_Unit, Rest2} = decode_literal(Rest1),
    ?TRACE("{get_tail,~p,~p,", [Live, _Unit]),
    {MSt1, ResultTerm, NewMatchState} = do_get_tail(
        MatchState, Live, BSOffsetReg, BSBinaryReg, MMod, MSt0
    ),
    % This is get_tail, we don't need to fix BSBinaryReg by doing an and with ?TERM_PRIMARY_CLEAR_MASK
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    ?TRACE("~p},", [Dest]),
    MSt3 = MMod:move_to_vm_register(MSt2, ResultTerm, Dest),
    MSt4 = MMod:free_native_registers(MSt3, [ResultTerm, Dest]),
    {J0 - 3, Rest3, NewMatchState, BSOffsetReg, MSt4}.

do_get_tail(
    MatchState, Live, BSOffsetReg, BSBinaryReg, MMod, MSt0
) ->
    MSt1 = cond_raise_badarg({BSOffsetReg, '&', 2#111, '!=', 0}, MMod, MSt0),
    {MSt2, BSOffseBytesReg} = MMod:shift_right(MSt1, BSOffsetReg, 3),
    {MSt3, TailBytesReg0} = MMod:get_array_element(MSt2, BSBinaryReg, 1),
    MSt4 = MMod:sub(MSt3, TailBytesReg0, BSOffseBytesReg),
    {MSt5, HeapSizeReg} = MMod:call_primitive(MSt4, ?PRIM_TERM_SUB_BINARY_HEAP_SIZE, [
        BSBinaryReg, {free, TailBytesReg0}
    ]),
    {MSt6, NewMatchState} = memory_ensure_free_with_extra_root(
        MatchState, Live, {free, HeapSizeReg}, MMod, MSt5
    ),
    % Restore BSBinaryReg as it may have been gc'd as well
    {MSt7, MatchStateReg0} = MMod:copy_to_native_register(MSt6, NewMatchState),
    {MSt8, MatchStateReg0} = MMod:and_(MSt7, {free, MatchStateReg0}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt9 = MMod:move_array_element(MSt8, MatchStateReg0, 1, BSBinaryReg),
    MSt10 = MMod:free_native_registers(MSt9, [MatchStateReg0]),
    {MSt11, BSBinaryReg} = MMod:and_(MSt10, {free, BSBinaryReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt12, TailBytesReg1} = MMod:get_array_element(MSt11, BSBinaryReg, 1),
    MSt13 = MMod:sub(MSt12, TailBytesReg1, BSOffseBytesReg),
    MSt14 = MMod:add(MSt13, BSBinaryReg, ?TERM_PRIMARY_BOXED),
    {MSt15, ResultTerm} = MMod:call_primitive(MSt14, ?PRIM_TERM_MAYBE_CREATE_SUB_BINARY, [
        ctx, BSBinaryReg, {free, BSOffseBytesReg}, {free, TailBytesReg1}
    ]),
    {MSt15, ResultTerm, NewMatchState}.

first_pass_bs_match_equal_colon_equal(
    Fail, MatchState, BSBinaryReg, BSOffsetReg, J0, Rest0, MMod, MSt0
) ->
    % genot.tab says Live, but compiler always put nil
    Rest1 = decode_nil(Rest0),
    {Size, Rest2} = decode_literal(Rest1),
    {PatternValue, Rest3} = decode_literal(Rest2),
    ?TRACE("{'=:=',[],~p,~p},", [Size, PatternValue]),
    {MSt1, Result} = MMod:call_primitive(MSt0, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, BSBinaryReg, BSOffsetReg, Size, 0
    ]),
    MSt2 = handle_error_if({Result, '==', 0}, MMod, MSt1),
    MSt3 = cond_jump_to_label({Result, '==', ?FALSE_ATOM}, Fail, MMod, MSt2),
    MSt6 =
        case MMod:word_size() of
            4 when PatternValue bsr 28 > 0 ->
                % PatternValue doesn't match on immediate integer, so unbox Result for comparison
                MMod:if_block(
                    MSt3, {Result, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
                        MMod:jump_to_label(BSt0, Fail)
                    end
                ),
                {MSt4, Result} = MMod:and_(MSt3, {free, Result}, ?TERM_PRIMARY_CLEAR_MASK),
                {MSt5, IntValue} = MMod:get_array_element(MSt4, {free, Result}, 1),
                cond_jump_to_label({{free, IntValue}, '!=', PatternValue}, Fail, MMod, MSt5);
            _ ->
                {MSt4, ResultInt} = MMod:shift_right(MSt3, {free, Result}, 4),
                cond_jump_to_label({{free, ResultInt}, '!=', PatternValue}, Fail, MMod, MSt4)
        end,
    MSt7 = MMod:add(MSt6, BSOffsetReg, Size),
    {J0 - 3, Rest3, MatchState, BSOffsetReg, MSt7}.

first_pass_bs_match_skip(MatchState, BSOffsetReg, J0, Rest0, MMod, MSt0) ->
    {Stride, Rest1} = decode_literal(Rest0),
    MSt1 = MMod:add(MSt0, BSOffsetReg, Stride),
    ?TRACE("{skip,~p},", [Stride]),
    {J0 - 1, Rest1, MatchState, BSOffsetReg, MSt1}.

% OP_BIF2 dispatch (Module, Function known at compile time):
% inline fast paths for a few hot BIFs, otherwise fall back to a generic
% indirect call.
op_bif2(MMod, MSt0, FailLabel, erlang, element, _Bif, Index, Tuple, Dest) ->
    op_bif2_element(MMod, MSt0, FailLabel, Index, Tuple, Dest);
op_bif2(MMod, MSt0, FailLabel, _Module, _Function, Bif, Arg1, Arg2, Dest) ->
    op_bif2_default(MMod, MSt0, FailLabel, Bif, unwrap_typed(Arg1), unwrap_typed(Arg2), Dest).

op_bif2_default(MMod, MSt0, FailLabel, Bif, Arg1, Arg2, Dest) ->
    {MSt1, FuncPtr} = MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_BIF, [
        jit_state, Bif
    ]),
    {MSt2, ResultReg} = MMod:call_func_ptr(MSt1, {free, FuncPtr}, [
        ctx, FailLabel, {free, Arg1}, {free, Arg2}
    ]),
    bif_faillabel_test(FailLabel, MMod, MSt2, {free, ResultReg}, {free, Dest}).

%% Inline erlang:element/2: verify Index is a small int and Tuple is a boxed
%% tuple with arity >= Index >= 1, then read the element directly.  Any check
%% failure jumps to FailLabel (or raises badarg if FailLabel=0).
%%
%% Type-driven simplification: if Tuple is statically known to be a tuple,
%% skip the boxed-primary check and the boxed-tuple-tag check.
op_bif2_element(MMod, MSt0, FailLabel, Index, Tuple, Dest) ->
    TupleIsTuple = is_known_tuple(Tuple),
    Index1 = unwrap_typed(Index),
    Tuple1 = unwrap_typed(Tuple),
    {MSt1, IndexReg} = MMod:move_to_native_register(MSt0, Index1),
    MSt2 = cond_raise_badarg_or_jump_to_fail_label(
        {IndexReg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        FailLabel,
        MMod,
        MSt1
    ),
    {MSt3, TupleReg} = MMod:move_to_native_register(MSt2, Tuple1),
    MSt4 =
        case TupleIsTuple of
            true ->
                MSt3;
            false ->
                cond_raise_badarg_or_jump_to_fail_label(
                    {TupleReg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED},
                    FailLabel,
                    MMod,
                    MSt3
                )
        end,
    %% Strip primary tag bits to get the heap pointer
    {MSt5, TupleReg} = MMod:and_(MSt4, {free, TupleReg}, ?TERM_PRIMARY_CLEAR_MASK),
    %% Allocate a fresh register for the header so we can keep TupleReg
    %% alive for the final indexed read.
    {MSt6, HeaderReg} = MMod:copy_to_native_register(MSt5, TupleReg),
    MSt7 = MMod:move_array_element(MSt6, HeaderReg, 0, HeaderReg),
    MSt8 =
        case TupleIsTuple of
            true ->
                MSt7;
            false ->
                cond_raise_badarg_or_jump_to_fail_label(
                    {HeaderReg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_TUPLE},
                    FailLabel,
                    MMod,
                    MSt7
                )
        end,
    %% Convert the term-encoded Index to a raw integer (>> 4)
    {MSt9, IndexInt} = MMod:shift_right(MSt8, {free, IndexReg}, 4),
    MSt10 = cond_raise_badarg_or_jump_to_fail_label(
        {IndexInt, '<', 1}, FailLabel, MMod, MSt9
    ),
    %% Arity = header >> 6
    {MSt11, ArityReg} = MMod:shift_right(MSt10, {free, HeaderReg}, 6),
    %% IndexInt > Arity means out-of-range; encoded as (Arity < IndexInt)
    MSt12 = cond_raise_badarg_or_jump_to_fail_label(
        {{free, ArityReg}, '<', IndexInt}, FailLabel, MMod, MSt11
    ),
    %% Read TupleReg[IndexInt] (1-based, so byte offset IndexInt*8)
    MSt13 = MMod:move_array_element(MSt12, TupleReg, {free, IndexInt}, Dest),
    MSt14 = MMod:free_native_registers(MSt13, [TupleReg, Dest]),
    MSt14.

% byte_size on a known binary - inline
op_gc_bif1(MMod, MSt0, FailLabel, Live, Bif, erlang, 'byte_size', Arg, Dest) ->
    case is_known_binary(MMod, MSt0, Arg) of
        true ->
            op_gc_bif1_byte_size_binary(MMod, MSt0, unwrap_typed(Arg), Dest);
        false ->
            op_gc_bif1_default(MMod, MSt0, FailLabel, Live, Bif, unwrap_typed(Arg), Dest)
    end;
% Default: call BIF via function pointer
op_gc_bif1(MMod, MSt0, FailLabel, Live, Bif, _Module, _Function, Arg, Dest) ->
    op_gc_bif1_default(MMod, MSt0, FailLabel, Live, Bif, unwrap_typed(Arg), Dest).

is_known_tuple({typed, _Arg, t_tuple}) -> true;
is_known_tuple(_) -> false.

op_gc_bif1_default(MMod, MSt0, FailLabel, Live, Bif, Arg, Dest) ->
    CappedLive =
        if
            Live > ?MAX_REG -> ?MAX_REG;
            true -> Live
        end,
    {MSt3, FuncPtr} = resolve_gcbif_func_ptr(MMod, MSt0, Live, Bif),
    {MSt4, ResultReg} = MMod:call_func_ptr(MSt3, {free, FuncPtr}, [
        ctx, FailLabel, CappedLive, {free, Arg}
    ]),
    bif_faillabel_test(FailLabel, MMod, MSt4, {free, ResultReg}, {free, Dest}).

% Inline byte_size for a known binary
% Binary layout: boxed_value[0] = header, boxed_value[1] = byte size (raw integer)
op_gc_bif1_byte_size_binary(MMod, MSt0, Arg, Dest) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg),
    % Strip primary tag to get raw pointer
    {MSt2, Reg} = MMod:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    % Read byte size from boxed_value[1]
    MSt3 = MMod:move_array_element(MSt2, Reg, 1, Reg),
    % Encode as tagged integer: (size << 4) | 0xF
    MSt4 = MMod:shift_left(MSt3, Reg, 4),
    MSt5 = MMod:or_(MSt4, Reg, ?TERM_INTEGER_TAG),
    MSt6 = MMod:move_to_vm_register(MSt5, Reg, Dest),
    MMod:free_native_registers(MSt6, [Reg, Dest]).

is_known_binary(_MMod, _MSt, {typed, _Arg, {t_bs_matchable, Unit}}) when Unit rem 8 =:= 0 ->
    true;
is_known_binary(_MMod, _MSt, _) ->
    false.

op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    '+',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_add(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod, MSt0, FailLabel, Live, Bif, erlang, '+', {typed, Arg1, {t_integer, Range1}}, Arg2, Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    % Arg2 is a small integer literal, extract its value and create a range
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_add(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    '-',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_sub(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod, MSt0, FailLabel, Live, Bif, erlang, '-', {typed, Arg1, {t_integer, Range1}}, Arg2, Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    % Arg2 is a small integer literal, extract its value and create a range
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_sub(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% band - both typed integers with range: inline if proven small
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'band',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_band(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'band',
    {typed, Arg1, {t_integer, Range1}},
    Arg2,
    Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_band(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% bor - both typed integers with range: inline if proven small
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'bor',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_bor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'bor',
    {typed, Arg1, {t_integer, Range1}},
    Arg2,
    Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_bor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% bxor - both typed integers with range: inline if proven small, XOR zeroes tag
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'bxor',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_bxor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'bxor',
    {typed, Arg1, {t_integer, Range1}},
    Arg2,
    Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_bxor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% mul - both typed integers with range: inline if proven small
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    '*',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_mul(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    '*',
    {typed, Arg1, {t_integer, Range1}},
    Arg2,
    Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_mul(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% div - both typed integers: inline if divisor provably non-zero and result fits
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'div',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_div(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod, MSt0, FailLabel, Live, Bif, erlang, 'div', {typed, Arg1, {t_integer, Range1}}, Arg2, Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_div(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% rem - both typed integers: inline if divisor provably non-zero and result fits
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'rem',
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}},
    Dest
) ->
    op_gc_bif2_rem(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
op_gc_bif2(
    MMod, MSt0, FailLabel, Live, Bif, erlang, 'rem', {typed, Arg1, {t_integer, Range1}}, Arg2, Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    Range2 = {Arg2Value, Arg2Value},
    op_gc_bif2_rem(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2);
% bsl - typed integer with literal shift amount: inline if result fits
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'bsl',
    {typed, Arg1, {t_integer, Range1}},
    Arg2,
    Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    op_gc_bif2_bsl(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Arg2Value);
% bsr - typed integer with literal shift amount: inline if non-negative and small
op_gc_bif2(
    MMod,
    MSt0,
    FailLabel,
    Live,
    Bif,
    erlang,
    'bsr',
    {typed, Arg1, {t_integer, Range1}},
    Arg2,
    Dest
) when is_integer(Arg2), Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG ->
    Arg2Value = Arg2 bsr 4,
    op_gc_bif2_bsr(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Arg2Value);
% Runtime small-integer fast path for + and - on operands not statically known
% to be small. Only when the backend exposes overflow-checked arithmetic and
% both operands are reloadable VM locations/literals (so the fallback can
% re-read them); otherwise the default BIF call.
op_gc_bif2(MMod, MSt0, FailLabel, Live, Bif, erlang, Op, Arg1, Arg2, Dest) when
    (Op =:= '+' orelse Op =:= '-')
->
    op_gc_bif2_addsub_fallback(MMod, MSt0, FailLabel, Live, Bif, Op, Arg1, Arg2, Dest);
% Runtime small-integer fast path for *. Same gating as +/-; the backend
% mul_overflow op computes the tagged product and flags whether it overflowed
% the small-integer range.
op_gc_bif2(MMod, MSt0, FailLabel, Live, Bif, erlang, '*', Arg1, Arg2, Dest) ->
    op_gc_bif2_mul_fallback(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest);
% Runtime small-integer fast path for div/rem by a POSITIVE small-int literal,
% when the dividend's type is not known to be a small integer.
op_gc_bif2(MMod, MSt0, FailLabel, Live, Bif, erlang, Op, Arg1, Arg2, Dest) when
    (Op =:= 'div' orelse Op =:= 'rem'),
    is_integer(Arg2),
    Arg2 band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG,
    (Arg2 bsr 4) >= 1
->
    BackendOp =
        case Op of
            'div' -> div_;
            'rem' -> rem_
        end,
    case
        erlang:function_exported(MMod, supports_div, 1) andalso MMod:supports_div(MSt0) andalso
            addsub_fastpath_reloadable(Arg1)
    of
        true ->
            op_gc_bif2_divrem_lit_runtime(
                MMod, MSt0, FailLabel, Live, Bif, BackendOp, Arg1, Arg2, Dest
            );
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, unwrap_typed(Arg1), Arg2, Dest)
    end;
% Default case
op_gc_bif2(
    MMod, MSt0, FailLabel, Live, Bif, _Module, _Function, {typed, Arg1, _}, {typed, Arg2, _}, Dest
) ->
    op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest);
op_gc_bif2(MMod, MSt0, FailLabel, Live, Bif, _Module, _Function, {typed, Arg1, _}, Arg2, Dest) ->
    op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest);
op_gc_bif2(MMod, MSt0, FailLabel, Live, Bif, _Module, _Function, Arg1, {typed, Arg2, _}, Dest) ->
    op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest);
op_gc_bif2(MMod, MSt0, FailLabel, Live, Bif, _Module, _Function, Arg1, Arg2, Dest) ->
    op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest).

%% Runtime small-integer fast-path dispatch for + and -, used both when no
%% compile-time type information is available and when the range-based
%% inlining could not prove the absence of overflow (e.g. an unbounded
%% t_integer range). Backends with flag-based overflow detection
%% (aarch64/x86_64/arm32/armv6m) implement add_overflow/3; flagless backends
%% (riscv/wasm/xtensa) implement add_overflow_check/3. Without either, or with
%% non-reloadable operands, fall back to the plain BIF call.
op_gc_bif2_addsub_fallback(MMod, MSt0, FailLabel, Live, Bif, Op, Arg1, Arg2, Dest) ->
    Reloadable = addsub_fastpath_reloadable(Arg1) andalso addsub_fastpath_reloadable(Arg2),
    case Reloadable andalso erlang:function_exported(MMod, add_overflow, 3) of
        true ->
            op_gc_bif2_addsub_runtime(MMod, MSt0, FailLabel, Live, Bif, Op, Arg1, Arg2, Dest);
        false ->
            case Reloadable andalso erlang:function_exported(MMod, add_overflow_check, 3) of
                true ->
                    op_gc_bif2_addsub_runtime_nf(
                        MMod, MSt0, FailLabel, Live, Bif, Op, Arg1, Arg2, Dest
                    );
                false ->
                    op_gc_bif2_default(
                        MMod,
                        MSt0,
                        FailLabel,
                        Live,
                        Bif,
                        unwrap_typed(Arg1),
                        unwrap_typed(Arg2),
                        Dest
                    )
            end
    end.

%% Same dispatch for *: runtime fast path if the backend supports it,
%% plain BIF call otherwise.
op_gc_bif2_mul_fallback(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest) ->
    Reloadable = addsub_fastpath_reloadable(Arg1) andalso addsub_fastpath_reloadable(Arg2),
    case Reloadable andalso erlang:function_exported(MMod, mul_overflow, 3) of
        true ->
            op_gc_bif2_mul_runtime(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest);
        false ->
            case Reloadable andalso erlang:function_exported(MMod, mul_overflow_check, 3) of
                true ->
                    op_gc_bif2_mul_runtime_nf(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest);
                false ->
                    op_gc_bif2_default(
                        MMod,
                        MSt0,
                        FailLabel,
                        Live,
                        Bif,
                        unwrap_typed(Arg1),
                        unwrap_typed(Arg2),
                        Dest
                    )
            end
    end.

op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest) ->
    CappedLive =
        if
            Live > ?MAX_REG -> ?MAX_REG;
            true -> Live
        end,
    {MSt3, FuncPtr} = resolve_gcbif_func_ptr(MMod, MSt0, Live, Bif),
    {MSt4, ResultReg} = MMod:call_func_ptr(MSt3, {free, FuncPtr}, [
        ctx, FailLabel, CappedLive, {free, Arg1}, {free, Arg2}
    ]),
    bif_faillabel_test(FailLabel, MMod, MSt4, {free, ResultReg}, {free, Dest}).

%% An operand is "reloadable" for the add/sub fast path if its source can be
%% read again (for the BIF fallback) after we have loaded it: VM registers and
%% small-integer literals qualify; transient {free, _} temporaries do not.
addsub_fastpath_reloadable({typed, Arg, _}) -> addsub_fastpath_reloadable(Arg);
addsub_fastpath_reloadable({x_reg, _}) -> true;
addsub_fastpath_reloadable({y_reg, _}) -> true;
addsub_fastpath_reloadable(Arg) when is_integer(Arg) -> true;
addsub_fastpath_reloadable(_) -> false.

%% Runtime small-integer fast path for + / -. See the dispatch clause above.
%% Register ownership: R1/R2 hold the loaded operands and are freed on EVERY
%% path exactly once (so the if_else_block merge is consistent). The inline
%% computation uses private copies (Res/Tmp) freed within the inline path. The
%% fallback re-reads the original VM operands (Arg1/Arg2 are reloadable).
%%
%% Correctness of the tagged arithmetic: small ints are (v << 4) |
%% TERM_INTEGER_TAG. Strip the second operand's tag and add/subtract into a copy
%% of the first (which keeps its tag), giving a correctly tagged result. Signed
%% overflow of the tagged op (V flag) occurs exactly when the untagged result
%% leaves the small-integer range, selecting the bignum fallback.
op_gc_bif2_addsub_runtime(MMod, MSt0, FailLabel, Live, Bif, Op, Arg1, Arg2, Dest) ->
    UArg1 = unwrap_typed(Arg1),
    UArg2 = unwrap_typed(Arg2),
    {MSt1, R1} = MMod:move_to_native_register(MSt0, UArg1),
    {MSt2, R2} = MMod:move_to_native_register(MSt1, UArg2),
    %% Free the loaded operand registers, then re-read the originals for the BIF.
    Fallback = fun(BSt0) ->
        BSt1 = MMod:free_native_registers(BSt0, [R1, R2]),
        op_gc_bif2_default(MMod, BSt1, FailLabel, Live, Bif, UArg1, UArg2, Dest)
    end,
    MMod:if_else_block(
        MSt2,
        {R1, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        Fallback,
        fun(BSt0) ->
            MMod:if_else_block(
                BSt0,
                {R2, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                Fallback,
                fun(BSt1) ->
                    %% Compute in place into R1 so the result ends up in the
                    %% same register the non-fast path would use (downstream
                    %% code may read the result register directly). Strip
                    %% operand 2's tag in place with {free, R2}: this both frees
                    %% R2 and invalidates any VM-location it was caching. That
                    %% invalidation is essential when Arg2 aliases Dest (e.g.
                    %% `X = A - X`): once the result overwrites Dest, R2 would
                    %% otherwise stay cached as that VM register while holding
                    %% the stale pre-op value, and a later read would pick it up.
                    %% R1 keeps its tag so the result is correctly tagged. On
                    %% overflow R1 is clobbered, but the fallback re-reads the
                    %% original VM operands (it does not need R1/R2's values).
                    {BSt3, TmpS} = MMod:and_(BSt1, {free, R2}, bnot (?TERM_IMMED_TAG_MASK)),
                    BSt4 =
                        case Op of
                            '+' -> MMod:add_overflow(BSt3, R1, TmpS);
                            '-' -> MMod:sub_overflow(BSt3, R1, TmpS)
                        end,
                    BSt5 = MMod:free_native_registers(BSt4, [TmpS]),
                    MMod:if_else_block(
                        BSt5,
                        overflow_set,
                        %% Overflow: R1 clobbered; fallback re-reads VM operands.
                        Fallback,
                        %% In range: R1 holds the tagged result; store to Dest.
                        fun(NSt0) ->
                            NSt1 = MMod:move_to_vm_register(NSt0, R1, Dest),
                            MMod:free_native_registers(NSt1, [R1, Dest])
                        end
                    )
                end
            )
        end
    ).

%% Flagless variant of op_gc_bif2_addsub_runtime for backends without hardware
%% condition flags (riscv/wasm/xtensa). add_overflow_check/sub_overflow_check
%% leave the result shifted into the value field of R1 (untagged) and return a
%% CheckReg that is nonzero iff the result left the small-integer range; the
%% overflow is then branched on with the existing {CheckReg, '!=', 0} condition.
op_gc_bif2_addsub_runtime_nf(MMod, MSt0, FailLabel, Live, Bif, Op, Arg1, Arg2, Dest) ->
    UArg1 = unwrap_typed(Arg1),
    UArg2 = unwrap_typed(Arg2),
    {MSt1, R1} = MMod:move_to_native_register(MSt0, UArg1),
    {MSt2, R2} = MMod:move_to_native_register(MSt1, UArg2),
    Fallback = fun(BSt0) ->
        BSt1 = MMod:free_native_registers(BSt0, [R1, R2]),
        op_gc_bif2_default(MMod, BSt1, FailLabel, Live, Bif, UArg1, UArg2, Dest)
    end,
    MMod:if_else_block(
        MSt2,
        {R1, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        Fallback,
        fun(BSt0) ->
            MMod:if_else_block(
                BSt0,
                {R2, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                Fallback,
                fun(BSt1) ->
                    %% R1 := (R1 Op R2) shifted into the value field (untagged);
                    %% CheckReg != 0 iff the result overflows the small range.
                    {BSt2, CheckReg} =
                        case Op of
                            '+' -> MMod:add_overflow_check(BSt1, R1, R2);
                            '-' -> MMod:sub_overflow_check(BSt1, R1, R2)
                        end,
                    MMod:if_else_block(
                        BSt2,
                        {{free, CheckReg}, '!=', 0},
                        %% Overflow: R1 clobbered; fallback re-reads VM operands.
                        Fallback,
                        %% In range: add the small-integer tag and store to Dest.
                        fun(NSt0) ->
                            NSt1 = MMod:or_(NSt0, R1, ?TERM_INTEGER_TAG),
                            NSt2 = MMod:move_to_vm_register(NSt1, R1, Dest),
                            MMod:free_native_registers(NSt2, [R1, R2, Dest])
                        end
                    )
                end
            )
        end
    ).

%% Flagless variant of op_gc_bif2_mul_runtime (riscv: has a high-multiply).
%% mul_overflow_check leaves the product shifted into the value field of R1
%% (untagged) and returns CheckReg != 0 iff it overflowed the small range.
op_gc_bif2_mul_runtime_nf(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest) ->
    UArg1 = unwrap_typed(Arg1),
    UArg2 = unwrap_typed(Arg2),
    {MSt1, R1} = MMod:move_to_native_register(MSt0, UArg1),
    {MSt2, R2} = MMod:move_to_native_register(MSt1, UArg2),
    Fallback = fun(BSt0) ->
        BSt1 = MMod:free_native_registers(BSt0, [R1, R2]),
        op_gc_bif2_default(MMod, BSt1, FailLabel, Live, Bif, UArg1, UArg2, Dest)
    end,
    MMod:if_else_block(
        MSt2,
        {R1, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        Fallback,
        fun(BSt0) ->
            MMod:if_else_block(
                BSt0,
                {R2, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                Fallback,
                fun(BSt1) ->
                    {BSt2, CheckReg} = MMod:mul_overflow_check(BSt1, R1, R2),
                    MMod:if_else_block(
                        BSt2,
                        {{free, CheckReg}, '!=', 0},
                        Fallback,
                        fun(NSt0) ->
                            NSt1 = MMod:or_(NSt0, R1, ?TERM_INTEGER_TAG),
                            NSt2 = MMod:move_to_vm_register(NSt1, R1, Dest),
                            MMod:free_native_registers(NSt2, [R1, R2, Dest])
                        end
                    )
                end
            )
        end
    ).

%% Runtime small-integer fast path for *. Mirrors op_gc_bif2_addsub_runtime:
%% load both operands, check both small, then mul_overflow computes the tagged
%% product in place in R1 and flags whether it overflowed the small range. On
%% overflow R1 is clobbered, so the fallback re-reads the original VM operands.
op_gc_bif2_mul_runtime(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest) ->
    UArg1 = unwrap_typed(Arg1),
    UArg2 = unwrap_typed(Arg2),
    {MSt1, R1} = MMod:move_to_native_register(MSt0, UArg1),
    {MSt2, R2} = MMod:move_to_native_register(MSt1, UArg2),
    Fallback = fun(BSt0) ->
        BSt1 = MMod:free_native_registers(BSt0, [R1, R2]),
        op_gc_bif2_default(MMod, BSt1, FailLabel, Live, Bif, UArg1, UArg2, Dest)
    end,
    MMod:if_else_block(
        MSt2,
        {R1, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        Fallback,
        fun(BSt0) ->
            MMod:if_else_block(
                BSt0,
                {R2, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                Fallback,
                fun(BSt1) ->
                    %% R1 = (R1 * R2) shifted into the value field (not yet
                    %% tagged); flags set so mul_overflow_set is true iff the
                    %% product does not fit in a small integer.
                    BSt2 = MMod:mul_overflow(BSt1, R1, R2),
                    MMod:if_else_block(
                        BSt2,
                        mul_overflow_set,
                        %% Overflow: R1 clobbered; fallback re-reads VM operands.
                        Fallback,
                        %% In range: add the small-integer tag, store to Dest.
                        fun(NSt0) ->
                            NSt1 = MMod:or_(NSt0, R1, ?TERM_INTEGER_TAG),
                            NSt2 = MMod:move_to_vm_register(NSt1, R1, Dest),
                            MMod:free_native_registers(NSt2, [R1, R2, Dest])
                        end
                    )
                end
            )
        end
    ).

%% Runtime small-integer fast path for `div'/`rem' by a positive small-int
%% literal Arg2Tagged. If the dividend is a small integer, untag it IN PLACE in
%% R1, do the native signed div/rem by the (untagged) literal divisor, re-tag in
%% place, and store to Dest. The result is left in R1 (the register the
%% non-fast path would use) so downstream code that reads the result register
%% directly sees the right value. On the not-small path R1 is untouched and the
%% BIF re-reads the original VM dividend (so the positive literal divisor still
%% guarantees no divide-by-zero / MIN-by-(-1) overflow on the small path).
op_gc_bif2_divrem_lit_runtime(MMod, MSt0, FailLabel, Live, Bif, BackendOp, Arg1, Arg2Tagged, Dest) ->
    UArg1 = unwrap_typed(Arg1),
    DivisorValue = Arg2Tagged bsr 4,
    {MSt1, R1} = MMod:move_to_native_register(MSt0, UArg1),
    MMod:if_else_block(
        MSt1,
        {R1, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        %% Not a small integer: re-read the original dividend for the BIF.
        fun(BSt0) ->
            BSt1 = MMod:free_native_registers(BSt0, [R1]),
            op_gc_bif2_default(MMod, BSt1, FailLabel, Live, Bif, UArg1, Arg2Tagged, Dest)
        end,
        %% Small integer: untag R1 in place, native div/rem, re-tag in place.
        fun(BSt0) ->
            {BSt1, R1} = MMod:shift_right_arith(BSt0, {free, R1}, 4),
            {BSt2, Divisor} = MMod:move_to_native_register(BSt1, DivisorValue),
            {BSt3, ResReg} = MMod:BackendOp(BSt2, R1, Divisor),
            BSt4 = MMod:shift_left(BSt3, ResReg, 4),
            BSt5 = MMod:or_(BSt4, ResReg, ?TERM_INTEGER_TAG),
            BSt6 = MMod:move_to_vm_register(BSt5, ResReg, Dest),
            MMod:free_native_registers(BSt6, [ResReg, Divisor, Dest])
        end
    ).

%% Resolve the imported gc_bif function pointer. Backends may inline the
%% resolution (avoiding the out-of-line PRIM_GET_IMPORTED_GCBIF call) by
%% exporting move_imported_gcbif_to_native_register/3; otherwise fall back to
%% the primitive.
resolve_gcbif_func_ptr(MMod, MSt0, Live, Bif) ->
    case erlang:function_exported(MMod, move_imported_gcbif_to_native_register, 3) of
        true ->
            MMod:move_imported_gcbif_to_native_register(MSt0, Live, Bif);
        false ->
            MMod:call_primitive(MSt0, ?PRIM_GET_IMPORTED_GCBIF, [ctx, jit_state, Live, Bif])
    end.

%% OP_TEST_HEAP: measured 2026-06-11 on x86_64, inlining the free-space
%% corridor check (read_avail_heap_memory + if_block, with the C helper as
%% slow path) is a net LOSS on the AOT benchmark: the helper call is cheap
%% (hot and well-predicted) while the per-site inline check and duplicated
%% slow-path call sites cost icache (pingpong +10-25%, total +3-4%, both for
%% the corridor variant and a slimmed GC-direction-only variant). Keep the
%% plain call.
op_test_heap(MMod, MSt0, HeapNeed, Live) ->
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TEST_HEAP, [
        ctx, jit_state, HeapNeed, Live
    ]),
    handle_error_if({'(bool)', {free, ResultReg}, '==', false}, MMod, MSt1).

% Platform-specific bounds for small integers
small_integer_bounds(MMod) ->
    case MMod:word_size() of
        % 32-bit
        4 -> {-(1 bsl 27), (1 bsl 27) - 1};
        % 64-bit
        8 -> {-(1 bsl 59), (1 bsl 59) - 1}
    end.

% Check if addition can overflow based on type ranges
% Returns true if the result is guaranteed to fit in a small integer
can_inline_add(Range1, Range2, MMod) ->
    {MinSafe, MaxSafe} = small_integer_bounds(MMod),
    case {Range1, Range2} of
        {{Min1, Max1}, {Min2, Max2}} when
            is_integer(Min1),
            is_integer(Max1),
            is_integer(Min2),
            is_integer(Max2)
        ->
            % Calculate min and max possible results
            MinResult = Min1 + Min2,
            MaxResult = Max1 + Max2,
            MinResult >= MinSafe andalso MaxResult =< MaxSafe;
        _ ->
            false
    end.

% Optimized addition with compile-time range checking
op_gc_bif2_add(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    case can_inline_add(Range1, Range2, MMod) of
        true ->
            % Safe to inline - no overflow possible
            {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
            MSt2 = MMod:add(MSt1, Reg, Arg2 band (bnot (?TERM_IMMED_TAG_MASK))),
            MSt3 = MMod:move_to_vm_register(MSt2, Reg, Dest),
            MMod:free_native_registers(MSt3, [Reg, Dest]);
        false ->
            % Cannot prove the result stays small: try the runtime
            % overflow-checked fast path before the BIF call.
            op_gc_bif2_addsub_fallback(MMod, MSt0, FailLabel, Live, Bif, '+', Arg1, Arg2, Dest)
    end;
op_gc_bif2_add(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case can_inline_add(Range1, Range2, MMod) of
        true ->
            % Safe to inline both arguments
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            % Strip tag from Reg2 using AND, then add to Reg1 (Reg1 keeps its tag)
            {MSt3, Reg2Stripped} = MMod:and_(MSt2, {free, Reg2}, bnot (?TERM_IMMED_TAG_MASK)),
            MSt4 = MMod:add(MSt3, Reg1, Reg2Stripped),
            MSt5 = MMod:move_to_vm_register(MSt4, Reg1, Dest),
            MMod:free_native_registers(MSt5, [Reg1, Reg2Stripped, Dest]);
        false ->
            % Cannot prove the result stays small: try the runtime
            % overflow-checked fast path before the BIF call.
            op_gc_bif2_addsub_fallback(MMod, MSt0, FailLabel, Live, Bif, '+', Arg1, Arg2, Dest)
    end.

% Check if subtraction can overflow based on type ranges
% Returns true if the result is guaranteed to fit in a small integer
can_inline_sub(Range1, Range2, MMod) ->
    {MinSafe, MaxSafe} = small_integer_bounds(MMod),
    case {Range1, Range2} of
        {{Min1, Max1}, {Min2, Max2}} when
            is_integer(Min1),
            is_integer(Max1),
            is_integer(Min2),
            is_integer(Max2)
        ->
            % Min result: Min1 - Max2, Max result: Max1 - Min2
            MinResult = Min1 - Max2,
            MaxResult = Max1 - Min2,
            MinResult >= MinSafe andalso MaxResult =< MaxSafe;
        _ ->
            false
    end.

% Optimized subtraction with compile-time range checking
op_gc_bif2_sub(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    case can_inline_sub(Range1, Range2, MMod) of
        true ->
            % Safe to inline - no overflow possible
            {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
            MSt2 = MMod:sub(MSt1, Reg, Arg2 band (bnot (?TERM_IMMED_TAG_MASK))),
            MSt3 = MMod:move_to_vm_register(MSt2, Reg, Dest),
            MMod:free_native_registers(MSt3, [Reg, Dest]);
        false ->
            % Cannot prove the result stays small: try the runtime
            % overflow-checked fast path before the BIF call.
            op_gc_bif2_addsub_fallback(MMod, MSt0, FailLabel, Live, Bif, '-', Arg1, Arg2, Dest)
    end;
op_gc_bif2_sub(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case can_inline_sub(Range1, Range2, MMod) of
        true ->
            % Safe to inline both arguments
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            % Strip tag from Reg2 using AND, then subtract from Reg1 (Reg1 keeps its tag)
            {MSt3, Reg2Stripped} = MMod:and_(MSt2, {free, Reg2}, bnot (?TERM_IMMED_TAG_MASK)),
            MSt4 = MMod:sub(MSt3, Reg1, Reg2Stripped),
            MSt5 = MMod:move_to_vm_register(MSt4, Reg1, Dest),
            MMod:free_native_registers(MSt5, [Reg1, Reg2Stripped, Dest]);
        false ->
            % Cannot prove the result stays small: try the runtime
            % overflow-checked fast path before the BIF call.
            op_gc_bif2_addsub_fallback(MMod, MSt0, FailLabel, Live, Bif, '-', Arg1, Arg2, Dest)
    end.

% Check if both ranges are guaranteed to be small integers
% Sufficient for bitwise ops where result magnitude cannot exceed input magnitudes
is_small_integer_range(Range1, Range2, MMod) ->
    {MinSafe, MaxSafe} = small_integer_bounds(MMod),
    case {Range1, Range2} of
        {{Min1, Max1}, {Min2, Max2}} when
            is_integer(Min1),
            is_integer(Max1),
            is_integer(Min2),
            is_integer(Max2)
        ->
            Min1 >= MinSafe andalso Max1 =< MaxSafe andalso
                Min2 >= MinSafe andalso Max2 =< MaxSafe;
        _ ->
            false
    end.

op_gc_bif2_band(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg1} = MMod:and_(MSt1, {free, Reg1}, Arg2),
            MSt3 = MMod:move_to_vm_register(MSt2, Reg1, Dest),
            MMod:free_native_registers(MSt3, [Reg1, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end;
op_gc_bif2_band(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            {MSt3, Reg1} = MMod:and_(MSt2, {free, Reg1}, Reg2),
            MSt4 = MMod:move_to_vm_register(MSt3, Reg1, Dest),
            MMod:free_native_registers(MSt4, [Reg1, Reg2, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

op_gc_bif2_bor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            MSt2 = MMod:or_(MSt1, Reg1, Arg2),
            MSt3 = MMod:move_to_vm_register(MSt2, Reg1, Dest),
            MMod:free_native_registers(MSt3, [Reg1, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end;
op_gc_bif2_bor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            MSt3 = MMod:or_(MSt2, Reg1, Reg2),
            MSt4 = MMod:move_to_vm_register(MSt3, Reg1, Dest),
            MMod:free_native_registers(MSt4, [Reg1, Reg2, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

op_gc_bif2_bxor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            %% XOR with tag-stripped literal preserves Arg1's tag bits in one op:
            %% (a*16+15) XOR (b*16) = (a XOR b)*16 + 15.
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            MSt2 = MMod:xor_(MSt1, Reg1, Arg2 band (bnot (?TERM_IMMED_TAG_MASK))),
            MSt3 = MMod:move_to_vm_register(MSt2, Reg1, Dest),
            MMod:free_native_registers(MSt3, [Reg1, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end;
op_gc_bif2_bxor(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            MSt3 = MMod:xor_(MSt2, Reg1, Reg2),
            MSt4 = MMod:or_(MSt3, Reg1, ?TERM_INTEGER_TAG),
            MSt5 = MMod:move_to_vm_register(MSt4, Reg1, Dest),
            MMod:free_native_registers(MSt5, [Reg1, Reg2, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

% Check if multiplication can be inlined based on type ranges
% Returns true if the result is guaranteed to fit in a small integer
can_inline_mul(Range1, Range2, MMod) ->
    {MinSafe, MaxSafe} = small_integer_bounds(MMod),
    case {Range1, Range2} of
        {{Min1, Max1}, {Min2, Max2}} when
            is_integer(Min1),
            is_integer(Max1),
            is_integer(Min2),
            is_integer(Max2)
        ->
            % For multiplication, all four corner products must be checked
            Products = [Min1 * Min2, Min1 * Max2, Max1 * Min2, Max1 * Max2],
            MinResult = lists:min(Products),
            MaxResult = lists:max(Products),
            MinResult >= MinSafe andalso MaxResult =< MaxSafe;
        _ ->
            false
    end.

% Optimized multiplication with compile-time range checking
op_gc_bif2_mul(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    case can_inline_mul(Range1, Range2, MMod) of
        true ->
            Arg2Value = Arg2 bsr 4,
            case Arg2Value of
                C when C > 1 ->
                    %% Multiply the tagged value by C, then subtract 15*(C-1)
                    %% to fix the tag bits in one step.
                    %%   (a*16+15) * C = (a*C)*16 + 15*C
                    %%   target        = (a*C)*16 + 15
                    %%   diff          = 15*(C - 1)
                    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
                    MSt2 = MMod:mul(MSt1, Reg, C),
                    Diff = ?TERM_IMMED_TAG_MASK * (C - 1),
                    MSt3 = MMod:sub(MSt2, Reg, Diff),
                    MSt4 = MMod:move_to_vm_register(MSt3, Reg, Dest),
                    MMod:free_native_registers(MSt4, [Reg, Dest]);
                _ ->
                    % 0 or 1 would need special handling (0 produces wrong
                    % tag, 1 is identity), and negative constants require
                    % sign-aware logic. The compiler typically folds these,
                    % but fall back defensively.
                    op_gc_bif2_mul_fallback(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
            end;
        false ->
            % Cannot prove the result stays small: try the runtime
            % overflow-checked fast path before the BIF call.
            op_gc_bif2_mul_fallback(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end;
op_gc_bif2_mul(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case can_inline_mul(Range1, Range2, MMod) of
        true ->
            % Both operands in registers. Untag each operand with an arithmetic
            % shift (preserving the sign of negative small integers), multiply,
            % then shift the product back into the value field and add the tag.
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            {MSt5, ResReg, FreeRegs} =
                case Reg1 =:= Reg2 of
                    true ->
                        % Same VM register on both sides (e.g. X * X): untag the
                        % single register ONCE, then square it. Untagging twice
                        % (or untagging one side and shifting the other) would
                        % shift the same register repeatedly and corrupt it.
                        {MSt3, Reg1} = MMod:shift_right_arith(MSt2, {free, Reg1}, 4),
                        MSt4 = MMod:mul(MSt3, Reg1, Reg1),
                        {MSt4, Reg1, [Reg1, Dest]};
                    false ->
                        % Distinct registers: untag both, then multiply.
                        {MSt3, Reg1} = MMod:shift_right_arith(MSt2, {free, Reg1}, 4),
                        {MSt4, Reg2} = MMod:shift_right_arith(MSt3, {free, Reg2}, 4),
                        {MMod:mul(MSt4, Reg1, Reg2), Reg1, [Reg1, Reg2, Dest]}
                end,
            % Shift the product into the value field and add the tag:
            % ResReg = (value1 * value2) << 4 | TERM_INTEGER_TAG
            MSt6 = MMod:shift_left(MSt5, ResReg, 4),
            MSt7 = MMod:or_(MSt6, ResReg, ?TERM_INTEGER_TAG),
            MSt8 = MMod:move_to_vm_register(MSt7, ResReg, Dest),
            MMod:free_native_registers(MSt8, FreeRegs);
        false ->
            % Cannot prove the result stays small: try the runtime
            % overflow-checked fast path before the BIF call.
            op_gc_bif2_mul_fallback(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

% Check if left shift can be inlined based on type range and shift amount
can_inline_bsl(Range1, ShiftAmount, MMod) ->
    {MinSafe, MaxSafe} = small_integer_bounds(MMod),
    case Range1 of
        {Min1, Max1} when
            is_integer(Min1),
            is_integer(Max1),
            ShiftAmount >= 0
        ->
            MinResult = Min1 bsl ShiftAmount,
            MaxResult = Max1 bsl ShiftAmount,
            MinResult >= MinSafe andalso MaxResult =< MaxSafe;
        _ ->
            false
    end.

% Optimized bsl with compile-time range checking
op_gc_bif2_bsl(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, ShiftAmount) ->
    case can_inline_bsl(Range1, ShiftAmount, MMod) of
        true ->
            case ShiftAmount of
                0 ->
                    % No shift - just copy
                    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
                    MSt2 = MMod:move_to_vm_register(MSt1, Reg, Dest),
                    MMod:free_native_registers(MSt2, [Reg, Dest]);
                _ ->
                    % Shift tagged value left, then subtract 15*(2^N - 1) to fix
                    % the tag bits. Saves the explicit tag-strip:
                    %   (a*16+15) << N = (a<<N)*16 + 15*2^N
                    %   target = (a<<N)*16 + 15
                    %   diff = 15*(2^N - 1)
                    %
                    % This is 2 instructions (lsl + sub) vs 3 (and + lsl + orr)
                    % for small N where the immediate fits. For large N or
                    % unencodable immediates, op_imm falls back to mov+sub which
                    % matches the original count.
                    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
                    MSt2 = MMod:shift_left(MSt1, Reg, ShiftAmount),
                    Diff = ?TERM_IMMED_TAG_MASK * ((1 bsl ShiftAmount) - 1),
                    MSt3 = MMod:sub(MSt2, Reg, Diff),
                    MSt4 = MMod:move_to_vm_register(MSt3, Reg, Dest),
                    MMod:free_native_registers(MSt4, [Reg, Dest])
            end;
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

% Check if right shift can be inlined
% Only safe for non-negative inputs (the generated native code uses logical
% shift right, which does not preserve sign for negative values)
can_inline_bsr(Range1, ShiftAmount, MMod) ->
    {_MinSafe, MaxSafe} = small_integer_bounds(MMod),
    % Ensure (ShiftAmount + 4) does not exceed register width
    % (would be undefined behavior in native shift)
    WordBits = MMod:word_size() * 8,
    case Range1 of
        {Min1, Max1} when
            is_integer(Min1),
            is_integer(Max1),
            Min1 >= 0,
            ShiftAmount >= 0,
            ShiftAmount + 4 < WordBits
        ->
            % Non-negative input: right shift can only reduce magnitude
            Max1 =< MaxSafe;
        _ ->
            false
    end.

% Optimized bsr with compile-time range checking
op_gc_bif2_bsr(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, ShiftAmount) ->
    case can_inline_bsr(Range1, ShiftAmount, MMod) of
        true ->
            case ShiftAmount of
                0 ->
                    % No shift - just copy
                    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
                    MSt2 = MMod:move_to_vm_register(MSt1, Reg, Dest),
                    MMod:free_native_registers(MSt2, [Reg, Dest]);
                _ ->
                    % For non-negative values: shift right by (S+4), shift left by 4, re-tag.
                    % This avoids a separate tag-stripping instruction: the combined
                    % shift (S+4) removes both the 4 tag bits and applies the S-bit
                    % shift in one operation. The tag bits get shifted away since S+4 >= 5.
                    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Arg1),
                    {MSt2, Reg} = MMod:shift_right(MSt1, {free, Reg}, ShiftAmount + 4),
                    MSt3 = MMod:shift_left(MSt2, Reg, 4),
                    MSt4 = MMod:or_(MSt3, Reg, ?TERM_INTEGER_TAG),
                    MSt5 = MMod:move_to_vm_register(MSt4, Reg, Dest),
                    MMod:free_native_registers(MSt5, [Reg, Dest])
            end;
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

can_inline_div(Range1, Range2, MMod, MSt) ->
    case MMod:supports_div(MSt) of
        false ->
            false;
        true ->
            {MinSafe, MaxSafe} =
                case MMod:word_size() of
                    4 -> {-(1 bsl 27), (1 bsl 27) - 1};
                    8 -> {-(1 bsl 59), (1 bsl 59) - 1}
                end,
            case {Range1, Range2} of
                {{Min1, Max1}, {Min2, Max2}} when
                    is_integer(Min1),
                    is_integer(Max1),
                    is_integer(Min2),
                    is_integer(Max2),
                    Min1 >= MinSafe,
                    Max1 =< MaxSafe,
                    Min2 >= MinSafe,
                    Max2 =< MaxSafe,
                    (Min2 > 0 orelse Max2 < 0)
                ->
                    % Guard against MinSafe div -1 = -MinSafe which overflows
                    not (Min1 =:= MinSafe andalso Min2 =< -1 andalso Max2 >= -1);
                _ ->
                    false
            end
    end.

% Compute log2 of a power of 2
log2_pow2(1) -> 0;
log2_pow2(N) when N > 0 -> 1 + log2_pow2(N bsr 1).

% Check if we can use power-of-2 shift optimization for div
% Requires: divisor is power of 2, dividend is non-negative, fits in small integer
can_inline_pow2_div({Min1, Max1}, Arg2Value, MMod) when
    is_integer(Min1),
    is_integer(Max1),
    Min1 >= 0,
    is_integer(Arg2Value),
    Arg2Value > 0,
    Arg2Value band (Arg2Value - 1) =:= 0
->
    {_MinSafe, MaxSafe} = small_integer_bounds(MMod),
    Max1 =< MaxSafe;
can_inline_pow2_div(_, _, _) ->
    false.

% Check if we can use power-of-2 AND optimization for rem
% Same requirements as pow2 div
can_inline_pow2_rem({Min1, Max1}, Arg2Value, MMod) when
    is_integer(Min1),
    is_integer(Max1),
    Min1 >= 0,
    is_integer(Arg2Value),
    Arg2Value > 0,
    Arg2Value band (Arg2Value - 1) =:= 0
->
    {_MinSafe, MaxSafe} = small_integer_bounds(MMod),
    Max1 =< MaxSafe;
can_inline_pow2_rem(_, _, _) ->
    false.

op_gc_bif2_div(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    Arg2Value = Arg2 bsr 4,
    case can_inline_pow2_div(Range1, Arg2Value, MMod) of
        true ->
            % Power-of-2 division: X div 2^k = X >> k for non-negative X
            % The shift by (4+Shift) discards both the 4 tag bits and applies
            % the division shift in one operation, so no AND to strip tags is needed.
            Shift = log2_pow2(Arg2Value),
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg1} = MMod:shift_right_arith(MSt1, {free, Reg1}, 4 + Shift),
            MSt3 = MMod:shift_left(MSt2, Reg1, 4),
            MSt4 = MMod:or_(MSt3, Reg1, ?TERM_INTEGER_TAG),
            MSt5 = MMod:move_to_vm_register(MSt4, Reg1, Dest),
            MMod:free_native_registers(MSt5, [Reg1, Dest]);
        false ->
            case can_inline_div(Range1, Range2, MMod, MSt0) of
                true ->
                    {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
                    % Shift right by 4 discards the tag bits
                    {MSt2, Reg1} = MMod:shift_right_arith(MSt1, {free, Reg1}, 4),
                    {MSt3, Reg2} = MMod:move_to_native_register(MSt2, Arg2Value),
                    {MSt4, QuotientReg} = MMod:div_(MSt3, Reg1, Reg2),
                    MSt5 = MMod:shift_left(MSt4, QuotientReg, 4),
                    MSt6 = MMod:or_(MSt5, QuotientReg, ?TERM_INTEGER_TAG),
                    MSt7 = MMod:move_to_vm_register(MSt6, QuotientReg, Dest),
                    MMod:free_native_registers(MSt7, [QuotientReg, Reg1, Reg2, Dest]);
                false ->
                    op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
            end
    end;
op_gc_bif2_div(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case can_inline_div(Range1, Range2, MMod, MSt0) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            % Shift right by 4 discards the tag bits
            {MSt3, Reg1} = MMod:shift_right_arith(MSt2, {free, Reg1}, 4),
            {MSt4, Reg2} = MMod:shift_right_arith(MSt3, {free, Reg2}, 4),
            {MSt5, QuotientReg} = MMod:div_(MSt4, Reg1, Reg2),
            MSt6 = MMod:shift_left(MSt5, QuotientReg, 4),
            MSt7 = MMod:or_(MSt6, QuotientReg, ?TERM_INTEGER_TAG),
            MSt8 = MMod:move_to_vm_register(MSt7, QuotientReg, Dest),
            MMod:free_native_registers(MSt8, [QuotientReg, Reg1, Reg2, Dest]);
        false ->
            op_gc_bif2_div_rem_guarded(
                div_, MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2
            )
    end.

op_gc_bif2_rem(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) when
    is_integer(Arg2)
->
    Arg2Value = Arg2 bsr 4,
    case can_inline_pow2_rem(Range1, Arg2Value, MMod) of
        true ->
            % Power-of-2 remainder: X rem 2^k = X AND (2^k - 1) for non-negative X
            % Applied directly on tagged value: AND with ((2^k - 1) << 4 | tag)
            Mask = ((Arg2Value - 1) bsl 4) bor ?TERM_INTEGER_TAG,
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg1} = MMod:and_(MSt1, {free, Reg1}, Mask),
            MSt3 = MMod:move_to_vm_register(MSt2, Reg1, Dest),
            MMod:free_native_registers(MSt3, [Reg1, Dest]);
        false ->
            case can_inline_div(Range1, Range2, MMod, MSt0) of
                true ->
                    {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
                    % Shift right by 4 discards the tag bits
                    {MSt2, Reg1} = MMod:shift_right_arith(MSt1, {free, Reg1}, 4),
                    {MSt3, Reg2} = MMod:move_to_native_register(MSt2, Arg2Value),
                    {MSt4, RemReg} = MMod:rem_(MSt3, Reg1, Reg2),
                    MSt5 = MMod:shift_left(MSt4, RemReg, 4),
                    MSt6 = MMod:or_(MSt5, RemReg, ?TERM_INTEGER_TAG),
                    MSt7 = MMod:move_to_vm_register(MSt6, RemReg, Dest),
                    MMod:free_native_registers(MSt7, [RemReg, Reg1, Reg2, Dest]);
                false ->
                    op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
            end
    end;
op_gc_bif2_rem(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case can_inline_div(Range1, Range2, MMod, MSt0) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            % Shift right by 4 discards the tag bits
            {MSt3, Reg1} = MMod:shift_right_arith(MSt2, {free, Reg1}, 4),
            {MSt4, Reg2} = MMod:shift_right_arith(MSt3, {free, Reg2}, 4),
            {MSt5, RemReg} = MMod:rem_(MSt4, Reg1, Reg2),
            MSt6 = MMod:shift_left(MSt5, RemReg, 4),
            MSt7 = MMod:or_(MSt6, RemReg, ?TERM_INTEGER_TAG),
            MSt8 = MMod:move_to_vm_register(MSt7, RemReg, Dest),
            MMod:free_native_registers(MSt8, [RemReg, Reg1, Reg2, Dest]);
        false ->
            op_gc_bif2_div_rem_guarded(
                rem_, MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2
            )
    end.

%% @doc Inline div/rem when both operands are proven integers (any range) and
%% the divisor is proven positive (so it is never 0 and never -1, ruling out
%% div-by-zero and the MIN div -1 overflow). The operands may still be bignums
%% at runtime, so guard on both having the small-integer tag: if both are small
%% the inline native sdiv/rem path runs, otherwise fall back to the BIF (which
%% handles bignums). This removes the out-of-line call from the common small-int
%% case (e.g. `X rem I` in a loop where the compiler proves I >= 1 but unbounded
%% above). Op is rem_ or div_.
op_gc_bif2_div_rem_guarded(Op, MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest, Range1, Range2) ->
    case can_inline_div_guarded(Range1, Range2, MMod, MSt0) of
        true ->
            {MSt1, Reg1} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Reg2} = MMod:move_to_native_register(MSt1, Arg2),
            %% Both small ints iff (Reg1 band Reg2) has all tag bits set, since
            %% the small-integer tag is all four low bits (?TERM_INTEGER_TAG).
            {MSt3, TagReg} = MMod:copy_to_native_register(MSt2, Reg1),
            {MSt4, TagReg} = MMod:and_(MSt3, {free, TagReg}, Reg2),
            %% The masked-compare condition only supports '!=', so the TRUE
            %% block is the bignum (slow) path and the FALSE block is the
            %% both-small (fast) path.
            MSt5 = MMod:if_else_block(
                MSt4,
                {{free, TagReg}, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                fun(BSt0) ->
                    %% Slow path: at least one bignum. Call the BIF.
                    op_gc_bif2_default(MMod, BSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
                end,
                fun(BSt0) ->
                    %% Fast path: both small ints. Untag, native op, retag.
                    {BSt1, R1} = MMod:copy_to_native_register(BSt0, Reg1),
                    {BSt2, R1} = MMod:shift_right_arith(BSt1, {free, R1}, 4),
                    {BSt3, R2} = MMod:copy_to_native_register(BSt2, Reg2),
                    {BSt4, R2} = MMod:shift_right_arith(BSt3, {free, R2}, 4),
                    {BSt5, ResReg} = MMod:Op(BSt4, R1, R2),
                    BSt6 = MMod:shift_left(BSt5, ResReg, 4),
                    BSt7 = MMod:or_(BSt6, ResReg, ?TERM_INTEGER_TAG),
                    BSt8 = MMod:move_to_vm_register(BSt7, ResReg, Dest),
                    MMod:free_native_registers(BSt8, [ResReg, R1, R2])
                end
            ),
            MMod:free_native_registers(MSt5, [Reg1, Reg2, Dest]);
        false ->
            op_gc_bif2_default(MMod, MSt0, FailLabel, Live, Bif, Arg1, Arg2, Dest)
    end.

%% @doc Like can_inline_div but for the runtime-guarded path: requires the
%% backend to support native div, and the divisor range to prove the divisor is
%% strictly positive (Min2 >= 1). The dividend may be any integer (the small-int
%% guard at runtime catches bignums); we only need to rule out divide-by-zero
%% and the MIN/-1 overflow, both of which a positive divisor guarantees.
can_inline_div_guarded(_Range1, Range2, MMod, MSt) ->
    case MMod:supports_div(MSt) of
        false ->
            false;
        true ->
            case Range2 of
                {Min2, _Max2} when is_integer(Min2), Min2 >= 1 -> true;
                _ -> false
            end
    end.

% Helper to unwrap typed arguments
unwrap_typed({typed, Arg, _Type}) -> Arg;
unwrap_typed(Arg) -> Arg.

% Optimized >= comparison for typed integers
% Test if Arg1 >= Arg2, jump to Label if false (i.e., if Arg1 < Arg2)
op_is_ge(
    MMod,
    MSt0,
    Label,
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}}
) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            %% Both known small integers: tagged-value comparison works because
            %% both have the same tag bits (15) in the low 4 positions, so the
            %% relative order matches the integer order.
            {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Arg2Reg} = MMod:move_to_native_register(MSt1, Arg2),
            MSt3 = cond_jump_to_label(
                {{free, Arg1Reg}, '<', Arg2Reg}, Label, MMod, MSt2
            ),
            MMod:free_native_registers(MSt3, [Arg2Reg]);
        false ->
            op_is_ge_default(MMod, MSt0, Label, Arg1, Arg2)
    end;
op_is_ge(MMod, MSt0, Label, Arg1, {typed, Arg2, {t_integer, _Range}}) when is_integer(Arg1) ->
    % Arg1 is integer literal (already tagged by decode_compact_term), Arg2 is typed integer
    % If Arg2 is boxed (bignum), the comparison result depends on the sign
    {MSt1, Arg2Reg} = MMod:move_to_native_register(MSt0, Arg2),
    % Check if Arg2 is a small integer (tagged with 0xF)
    MSt2 = MMod:if_block(MSt1, {Arg2Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(
        BSt0
    ) ->
        {BSt1, BoxedReg} = MMod:and_(BSt0, Arg2Reg, bnot (?TERM_PRIMARY_MASK)),
        BSt2 = MMod:move_array_element(BSt1, BoxedReg, 0, BoxedReg),
        {BSt3, TagReg} = MMod:and_(BSt2, {free, BoxedReg}, ?TERM_BOXED_TAG_MASK),
        BSt4 = cond_jump_to_label(
            {{free, TagReg}, '==', ?TERM_BOXED_POSITIVE_INTEGER}, Label, MMod, BSt3
        ),
        % Negative bignum falls through here: set Arg2Reg to Arg1 so
        % the subsequent Arg1 < Arg2Reg comparison is false (no jump)
        MMod:move_to_native_register(BSt4, Arg1, Arg2Reg)
    end),
    % If we're here, Arg2 is a small integer - do inline comparison
    % is_ge tests Arg1 >= Arg2, jump to Label if Arg1 < Arg2
    % Arg1 is already tagged, use it directly
    cond_jump_to_label({Arg1, '<', {free, Arg2Reg}}, Label, MMod, MSt2);
op_is_ge(MMod, MSt0, Label, {typed, Arg1, {t_integer, _Range}}, Arg2) when is_integer(Arg2) ->
    % Arg1 is typed integer, Arg2 is integer literal (already tagged by decode_compact_term)
    % If Arg1 is boxed (bignum), the comparison result depends on the sign
    {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
    % Check if Arg1 is a small integer (tagged with 0xF)
    MSt2 = MMod:if_block(MSt1, {Arg1Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(
        BSt0
    ) ->
        {BSt1, BoxedReg} = MMod:and_(BSt0, Arg1Reg, bnot (?TERM_PRIMARY_MASK)),
        BSt2 = MMod:move_array_element(BSt1, BoxedReg, 0, BoxedReg),
        {BSt3, TagReg} = MMod:and_(BSt2, {free, BoxedReg}, ?TERM_BOXED_TAG_MASK),
        BSt4 = cond_jump_to_label(
            {{free, TagReg}, '!=', ?TERM_BOXED_POSITIVE_INTEGER}, Label, MMod, BSt3
        ),
        % Positive bignum falls through here: set Arg1Reg to Arg2 so
        % the subsequent Arg1Reg < Arg2 comparison is false (no jump)
        MMod:move_to_native_register(BSt4, Arg2, Arg1Reg)
    end),
    % If we're here, Arg1 is a small integer - do inline comparison
    % is_ge tests Arg1 >= Arg2, jump to Label if Arg1 < Arg2
    % Arg2 is already tagged, use it directly
    cond_jump_to_label({{free, Arg1Reg}, '<', Arg2}, Label, MMod, MSt2);
% Fallback: use term_compare
op_is_ge(MMod, MSt0, Label, Arg1, Arg2) ->
    op_is_ge_default(MMod, MSt0, Label, Arg1, Arg2).

op_is_ge_default(MMod, MSt0, Label, Arg1, Arg2) ->
    %% is_ge jumps to Label when Arg1 < Arg2 (i.e. NOT >=).
    emit_smallint_compare_fastpath(
        MMod,
        MSt0,
        Label,
        Arg1,
        Arg2,
        ?TERM_COMPARE_NO_OPTS,
        fun(BSt0, A1, A2) -> cond_jump_to_label({A1, '<', A2}, Label, MMod, BSt0) end,
        ?TERM_LESS_THAN
    ).

%% Optimized < comparison for typed integers.
%% Semantic: jump to Label if Arg1 >= Arg2 (i.e., NOT(Arg1 < Arg2)).
%%
%% Both-small-integer case: tagged cmp + if_else_block to invert.
op_is_lt(
    MMod,
    MSt0,
    Label,
    {typed, Arg1, {t_integer, Range1}},
    {typed, Arg2, {t_integer, Range2}}
) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Arg2Reg} = MMod:move_to_native_register(MSt1, Arg2),
            MSt3 = MMod:if_else_block(
                MSt2,
                {{free, Arg1Reg}, '<', Arg2Reg},
                fun(BSt0) -> BSt0 end,
                fun(BSt0) -> MMod:jump_to_label(BSt0, Label) end
            ),
            MMod:free_native_registers(MSt3, [Arg2Reg]);
        false ->
            op_is_lt_default(MMod, MSt0, Label, Arg1, Arg2)
    end;
%% Fallback: use term_compare. The literal-first and typed-literal-second
%% cases never reach here: OTP's beam_jump rewrites `is_lt` with a typed
%% integer arg and an integer literal arg to `is_ge` with args swapped.
op_is_lt(MMod, MSt0, Label, Arg1, Arg2) ->
    op_is_lt_default(MMod, MSt0, Label, Arg1, Arg2).

op_is_lt_default(MMod, MSt0, Label, Arg1, Arg2) ->
    %% is_lt jumps to Label when Arg1 >= Arg2 (i.e. NOT <). There is no direct
    %% ">=" condition, so express it as if_else_block on "<": the false branch
    %% (Arg1 >= Arg2) jumps.
    emit_smallint_compare_fastpath(
        MMod,
        MSt0,
        Label,
        Arg1,
        Arg2,
        ?TERM_COMPARE_NO_OPTS,
        fun(BSt0, A1, A2) ->
            MMod:if_else_block(
                BSt0,
                {A1, '<', A2},
                fun(B0) -> B0 end,
                fun(B0) -> MMod:jump_to_label(B0, Label) end
            )
        end,
        ?TERM_GREATER_THAN + ?TERM_EQUALS
    ).

op_is_not_equal(MMod, MSt0, Label, Arg1, Arg2) ->
    {MSt1, ResultReg} = MMod:call_primitive(MSt0, ?PRIM_TERM_COMPARE, [
        ctx,
        jit_state,
        {free, unwrap_typed(Arg1)},
        {free, unwrap_typed(Arg2)},
        ?TERM_COMPARE_NO_OPTS
    ]),
    MSt2 = handle_error_if({'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt1),
    cond_jump_to_label({'(int)', {free, ResultReg}, '==', ?TERM_EQUALS}, Label, MMod, MSt2).

%% Optimized =:= comparison for typed args.
%% is_eq_exact Label, Arg1, Arg2: jump to Label if Arg1 =/= Arg2.
%%
%% For typed small-int + typed small-int (both ranges fit small_integer_bounds),
%% inline as a direct cmp on tagged values: equal iff tagged values equal.
op_is_eq_exact(
    MMod, MSt0, Label, {typed, Arg1, {t_integer, Range1}}, {typed, Arg2, {t_integer, Range2}}
) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Arg2Reg} = MMod:move_to_native_register(MSt1, Arg2),
            MSt3 = cond_jump_to_label(
                {{free, Arg1Reg}, '!=', Arg2Reg}, Label, MMod, MSt2
            ),
            MMod:free_native_registers(MSt3, [Arg2Reg]);
        false ->
            op_is_eq_exact_default(MMod, MSt0, Label, Arg1, Arg2)
    end;
op_is_eq_exact(MMod, MSt0, Label, {typed, Arg1, {t_integer, _Range1}}, Arg2) when
    is_integer(Arg2)
->
    %% Arg1 typed small or bignum, Arg2 small int literal.
    %% Same as op_is_equal's typed+literal case: check Arg1 is small int.
    {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
    MSt2 = MMod:if_block(MSt1, {Arg1Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(
        BSt0
    ) ->
        MMod:jump_to_label(BSt0, Label)
    end),
    cond_jump_to_label({{free, Arg1Reg}, '!=', Arg2}, Label, MMod, MSt2);
%% No literal-first clause: OTP's beam_ssa_codegen always emits is_eq_exact
%% with the literal as the second argument.
op_is_eq_exact(MMod, MSt0, Label, Arg1, Arg2) when is_integer(Arg2) ->
    %% Plain immediate Arg2.
    {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, unwrap_typed(Arg1)),
    cond_jump_to_label({{free, Arg1Reg}, '!=', Arg2}, Label, MMod, MSt1);
op_is_eq_exact(MMod, MSt0, Label, Arg1, Arg2) ->
    op_is_eq_exact_default(MMod, MSt0, Label, Arg1, Arg2).

op_is_eq_exact_default(MMod, MSt0, Label, Arg1, Arg2) ->
    %% Runtime fast path: if both operands are small integers, equality is a
    %% direct tagged compare (equal iff tagged values equal), avoiding the
    %% term_compare C call. Otherwise fall back to term_compare. is_eq_exact
    %% jumps to Label when the operands are NOT equal.
    emit_smallint_compare_fastpath(
        MMod,
        MSt0,
        Label,
        Arg1,
        Arg2,
        ?TERM_COMPARE_EXACT,
        %% both-small fast path: jump to Label if tagged values differ.
        fun(BSt0, A1, A2) -> cond_jump_to_label({A1, '!=', A2}, Label, MMod, BSt0) end,
        %% term_compare result mask for "jump": LESS or GREATER (i.e. not equal).
        ?TERM_LESS_THAN + ?TERM_GREATER_THAN
    ).

%% Emit a runtime small-integer fast path wrapping a term_compare fallback,
%% mirroring how BEAM's JIT inlines tagged-small-int comparison before calling
%% the generic comparator. If BOTH operands carry the small-integer tag at
%% runtime, FastFn emits the native tagged comparison; otherwise term_compare
%% is called and the result is masked with JumpMask to decide the jump.
%%
%% Correctness: two small integers are encoded as (v << 4) | TERM_INTEGER_TAG
%% with TERM_INTEGER_TAG == TERM_IMMED_TAG_MASK, so a signed comparison of the
%% tagged values has the same ordering as the integers, and they are equal iff
%% the tagged values are equal. For any other type combination we must use the
%% full comparator. FastFn is called with (State, Arg1Reg, Arg2Reg) and both
%% registers are free to consume.
emit_smallint_compare_fastpath(MMod, MSt0, Label, Arg1, Arg2, CompareOpts, FastFn, JumpMask) ->
    {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, unwrap_typed(Arg1)),
    {MSt2, Arg2Reg} = MMod:move_to_native_register(MSt1, unwrap_typed(Arg2)),
    %% Outer test on Arg1's tag; inner test on Arg2's tag. Only when both are
    %% small integers do we reach FastFn; every other path uses term_compare.
    Fallback = fun(BSt0) ->
        {BSt1, ResultReg} = MMod:call_primitive(BSt0, ?PRIM_TERM_COMPARE, [
            ctx, jit_state, {free, Arg1Reg}, {free, Arg2Reg}, CompareOpts
        ]),
        BSt2 = handle_error_if(
            {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, BSt1
        ),
        cond_jump_to_label({{free, ResultReg}, '&', JumpMask, '!=', 0}, Label, MMod, BSt2)
    end,
    MMod:if_else_block(
        MSt2,
        {Arg1Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        %% Arg1 not a small integer: fall back.
        Fallback,
        %% Arg1 is a small integer: test Arg2.
        fun(BSt0) ->
            MMod:if_else_block(
                BSt0,
                {Arg2Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                %% Arg2 not a small integer: fall back.
                Fallback,
                %% Both small integers: native tagged comparison.
                fun(BSt1) ->
                    BSt2 = FastFn(BSt1, {free, Arg1Reg}, Arg2Reg),
                    MMod:free_native_registers(BSt2, [Arg2Reg])
                end
            )
        end
    ).

%% Optimized =/= comparison for typed args. Mirror of op_is_eq_exact.
op_is_not_eq_exact(
    MMod, MSt0, Label, {typed, Arg1, {t_integer, Range1}}, {typed, Arg2, {t_integer, Range2}}
) ->
    case is_small_integer_range(Range1, Range2, MMod) of
        true ->
            {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
            {MSt2, Arg2Reg} = MMod:move_to_native_register(MSt1, Arg2),
            MSt3 = cond_jump_to_label(
                {{free, Arg1Reg}, '==', Arg2Reg}, Label, MMod, MSt2
            ),
            MMod:free_native_registers(MSt3, [Arg2Reg]);
        false ->
            op_is_not_eq_exact_default(MMod, MSt0, Label, Arg1, Arg2)
    end;
op_is_not_eq_exact(MMod, MSt0, Label, {typed, Arg1, {t_integer, _Range1}}, Arg2) when
    is_integer(Arg2)
->
    %% Arg1 typed integer, Arg2 small int literal.
    %% is_not_eq_exact L, A, B: jump to L if A == B.
    %% If Arg1 is bignum: A != B (different tags) → don't jump.
    %% If Arg1 is small int: do tagged cmp; jump if equal.
    %%
    %% if_else_block: condition TRUE = bignum → block_true (no-op).
    %%                condition FALSE = small int → block_false (cmp + maybe jump).
    {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, Arg1),
    MMod:if_else_block(
        MSt1,
        {Arg1Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        fun(BSt0) -> MMod:free_native_registers(BSt0, [Arg1Reg]) end,
        fun(BSt0) ->
            cond_jump_to_label({{free, Arg1Reg}, '==', Arg2}, Label, MMod, BSt0)
        end
    );
%% No literal-first clause: OTP's beam_ssa_codegen always emits is_ne_exact
%% with the literal as the second argument.
op_is_not_eq_exact(MMod, MSt0, Label, Arg1, Arg2) when is_integer(Arg2) ->
    {MSt1, Arg1Reg} = MMod:move_to_native_register(MSt0, unwrap_typed(Arg1)),
    cond_jump_to_label({{free, Arg1Reg}, '==', Arg2}, Label, MMod, MSt1);
op_is_not_eq_exact(MMod, MSt0, Label, Arg1, Arg2) ->
    op_is_not_eq_exact_default(MMod, MSt0, Label, Arg1, Arg2).

op_is_not_eq_exact_default(MMod, MSt0, Label, Arg1, Arg2) ->
    %% is_ne_exact jumps to Label when the operands ARE equal.
    emit_smallint_compare_fastpath(
        MMod,
        MSt0,
        Label,
        Arg1,
        Arg2,
        ?TERM_COMPARE_EXACT,
        fun(BSt0, A1, A2) -> cond_jump_to_label({A1, '==', A2}, Label, MMod, BSt0) end,
        ?TERM_EQUALS
    ).

op_is_equal(MMod, MSt0, Label, Arg1, Arg2) ->
    %% is_equal (==) jumps to Label when the operands are NOT equal. For two
    %% small integers == agrees with =:=, so the tagged compare is valid.
    emit_smallint_compare_fastpath(
        MMod,
        MSt0,
        Label,
        Arg1,
        Arg2,
        ?TERM_COMPARE_NO_OPTS,
        fun(BSt0, A1, A2) -> cond_jump_to_label({A1, '!=', A2}, Label, MMod, BSt0) end,
        ?TERM_LESS_THAN + ?TERM_GREATER_THAN
    ).

%% OP_SELECT_VAL loop: emit a chain of cmp/branch comparisons. When both
%% sides are statically known to be immediates (atoms/small ints), we can
%% compare tagged values directly with cmp + b.eq. Otherwise fall back to
%% PRIM_TERM_COMPARE.
op_select_val_loop(MMod, MSt0, SrcValue, Rest0, N, State) when N > 0 ->
    case can_inline_select_val_src(SrcValue) of
        true ->
            op_select_val_inline_loop(MMod, MSt0, SrcValue, Rest0, N, State);
        false ->
            op_select_val_default_loop(MMod, MSt0, SrcValue, Rest0, N, State)
    end;
op_select_val_loop(_MMod, MSt0, _SrcValue, Rest, 0, _State) ->
    {MSt0, Rest}.

op_select_val_inline_loop(_MMod, MSt0, _SrcValue, Rest, 0, _State) ->
    {MSt0, Rest};
op_select_val_inline_loop(MMod, MSt0, SrcValue, Rest0, N, State) ->
    {MSt1, CmpValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State),
    {JmpLabel, Rest2} = decode_label(Rest1),
    ?TRACE(", ~p => ~p", [CmpValue, JmpLabel]),
    %% Load SrcValue into a register (hits cache after the first iteration).
    {MSt2, SrcReg} = MMod:move_to_native_register(MSt1, unwrap_typed(SrcValue)),
    MSt3 =
        case CmpValue of
            Imm when is_integer(Imm) ->
                MSta = cond_jump_to_label({SrcReg, '==', Imm}, JmpLabel, MMod, MSt2),
                %% SrcReg is still reserved; free it so the next iteration's
                %% move_to_native_register sees it as available and reuses cache.
                MMod:free_native_registers(MSta, [SrcReg]);
            _ ->
                Cmp = unwrap_typed(CmpValue),
                {MSt2a, CmpReg} = MMod:move_to_native_register(MSt2, Cmp),
                %% Compare two registers; both get freed by if_block. The cache
                %% in #regs.contents preserves the SrcValue → SrcReg mapping so
                %% the next iteration can recover it without re-loading.
                cond_jump_to_label({{free, CmpReg}, '==', {free, SrcReg}}, JmpLabel, MMod, MSt2a)
        end,
    op_select_val_inline_loop(MMod, MSt3, SrcValue, Rest2, N - 1, State).

op_select_val_default_loop(_MMod, MSt0, _SrcValue, Rest, 0, _State) ->
    {MSt0, Rest};
op_select_val_default_loop(MMod, MSt0, SrcValue, Rest0, N, State) ->
    {MSt1, CmpValue, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State),
    {JmpLabel, Rest2} = decode_label(Rest1),
    ?TRACE(", ~p => ~p", [CmpValue, JmpLabel]),
    {MSt2, ResultReg} = MMod:call_primitive(MSt1, ?PRIM_TERM_COMPARE, [
        ctx, jit_state, {free, unwrap_typed(CmpValue)}, unwrap_typed(SrcValue), ?TERM_COMPARE_EXACT
    ]),
    MSt3 = handle_error_if(
        {'(int)', ResultReg, '==', ?TERM_COMPARE_MEMORY_ALLOC_FAIL}, MMod, MSt2
    ),
    MSt4 = cond_jump_to_label(
        {'(int)', {free, ResultReg}, '==', ?TERM_EQUALS}, JmpLabel, MMod, MSt3
    ),
    op_select_val_default_loop(MMod, MSt4, SrcValue, Rest2, N - 1, State).

%% A SrcValue is select_val-inline-able when it's known to be an immediate
%% term (atom, pid, or a small integer). Bignums use a boxed representation
%% so tagged-value comparison is not safe — fall back to PRIM_TERM_COMPARE.
can_inline_select_val_src({typed, _, t_atom}) -> true;
can_inline_select_val_src({typed, _, pid}) -> true;
can_inline_select_val_src(_) -> false.

term_alloc_bin_match_state(Live, Src, Dest, MMod, MSt0) ->
    {MSt1, TrimResultReg} = MMod:call_primitive(MSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt2 = MMod:free_native_registers(MSt1, [TrimResultReg]),
    % Write Src to x_reg to have it as a gc root
    {MSt3, NewSrc} = memory_ensure_free_with_extra_root(
        Src, Live, ?TERM_BOXED_BIN_MATCH_STATE_SIZE, MMod, MSt2
    ),
    {MSt4, AllocMatchStateReg} = MMod:call_primitive(MSt3, ?PRIM_TERM_ALLOC_BIN_MATCH_STATE, [
        ctx, NewSrc, 0
    ]),
    MSt5 = MMod:move_to_vm_register(MSt4, AllocMatchStateReg, Dest),
    MSt6 = MMod:free_native_registers(MSt5, [AllocMatchStateReg]),
    {MSt6, NewSrc}.

term_from_catch_label(Dest, Label, MMod, MSt1) ->
    {MSt2, Reg} = MMod:get_module_index(MSt1),
    MSt3 = MMod:shift_left(MSt2, Reg, 24),
    MSt4 = MMod:or_(MSt3, Reg, (Label bsl ?TERM_IMMED2_TAG_SIZE) bor ?TERM_IMMED2_CATCH),
    MSt5 = MMod:move_to_vm_register(MSt4, Reg, Dest),
    MMod:free_native_registers(MSt5, [Reg, Dest]).

term_is_boxed_with_tag_and_get_ptr(Label, Arg1, BoxedTag, MMod, MSt1) ->
    {MSt2, Reg} = MMod:move_to_native_register(MSt1, Arg1),
    MSt3 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, MMod, MSt2
    ),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt5, BoxTagReg} = MMod:get_array_element(MSt4, Reg, 0),
    MSt6 = cond_jump_to_label(
        {{free, BoxTagReg}, '&', ?TERM_BOXED_TAG_MASK, '!=', BoxedTag}, Label, MMod, MSt5
    ),
    {MSt6, Reg}.

%%-----------------------------------------------------------------------------
%% @doc Raise tuple {badfun, Arg} if Arg is not a function
%% @param Arg element to test
%% @param MMod backend module
%% @param MSt0 backend state
%% @return new backend state
%%-----------------------------------------------------------------------------
verify_is_function({typed, Func, t_fun}, MMod, MSt0) ->
    MMod:move_to_native_register(MSt0, Func);
verify_is_function({typed, Func, any}, MMod, MSt0) ->
    verify_is_function(Func, MMod, MSt0);
verify_is_function({typed, Func, _Other}, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Func),
    MSt2 = MMod:call_primitive_last(MSt1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADFUN_ATOM, Reg
    ]),
    {MSt2, Reg};
verify_is_function(Func, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Func),
    MSt2 = MMod:if_block(MSt1, {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
            ctx, jit_state, offset, ?BADFUN_ATOM, Reg
        ])
    end),
    {MSt3, BoxedPtrReg} = MMod:and_(MSt2, Reg, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, BoxedPtrReg, 0, BoxedPtrReg),
    MSt5 = MMod:if_block(
        MSt4, {BoxedPtrReg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FUN}, fun(BSt0) ->
            MMod:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, Reg
            ])
        end
    ),
    MSt6 = MMod:free_native_registers(MSt5, [BoxedPtrReg]),
    {MSt6, Reg}.

verify_is_binary_or_match_state(Label, Src, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Src),
    MSt2 = verify_is_boxed(MMod, MSt1, Reg, Label),
    {MSt3, Reg} = MMod:and_(MSt2, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    {MSt5, Reg} = MMod:and_(MSt4, {free, Reg}, ?TERM_BOXED_TAG_MASK),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'and', [
            {Reg, '!=', ?TERM_BOXED_REFC_BINARY},
            {Reg, '!=', ?TERM_BOXED_HEAP_BINARY},
            {Reg, '!=', ?TERM_BOXED_SUB_BINARY},
            {Reg, '!=', ?TERM_BOXED_BIN_MATCH_STATE}
        ]},
        Label,
        MMod,
        MSt5
    ),
    MMod:free_native_registers(MSt6, [Reg]).

verify_is_boxed_with_tag(Label, Arg1, BoxedTag, MMod, MSt0) ->
    verify_is_boxed_with_tag(Label, Arg1, ?TERM_BOXED_TAG_MASK, BoxedTag, MMod, MSt0).

verify_is_boxed_with_tag(Label, {free, Reg}, BoxedMask, BoxedTag, MMod, MSt0) when is_atom(Reg) ->
    MSt1 = verify_is_boxed(MMod, MSt0, Reg, Label),
    {MSt2, Reg} = MMod:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, Reg, 0, Reg),
    cond_raise_badarg_or_jump_to_fail_label(
        {{free, Reg}, '&', BoxedMask, '!=', BoxedTag}, Label, MMod, MSt3
    );
verify_is_boxed_with_tag(Label, Arg1, BoxedMask, BoxedTag, MMod, MSt1) ->
    {MSt2, Reg} = MMod:copy_to_native_register(MSt1, Arg1),
    MSt3 = verify_is_boxed(MMod, MSt2, Reg, Label),
    {MSt4, Reg} = MMod:and_(MSt3, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt5 = MMod:move_array_element(MSt4, Reg, 0, Reg),
    cond_raise_badarg_or_jump_to_fail_label(
        {{free, Reg}, '&', BoxedMask, '!=', BoxedTag}, Label, MMod, MSt5
    ).

verify_is_boxed(MMod, MSt0, Reg) ->
    verify_is_boxed(MMod, MSt0, Reg, 0).

verify_is_boxed(MMod, MSt0, Reg, FailLabel) ->
    cond_raise_badarg_or_jump_to_fail_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, FailLabel, MMod, MSt0
    ).

%% Fuse is_tuple + test_arity + get_tuple_element(s) on the same register.
%% Avoids redundant register loads, tag stripping, and header loading.
try_fuse_tuple_ops(<<?OP_TEST_ARITY, Rest0/binary>>, Arg1, IsTupleLabel, MMod, MSt0, State0) ->
    {TestArityLabel, Rest1} = decode_label(Rest0),
    {MSt1, TestArityArg, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {Arity, Rest3} = decode_literal(Rest2),
    case TestArityArg =:= unwrap_typed(Arg1) of
        true ->
            ?TRACE("FUSE: is_tuple + test_arity ~p, ~p\n", [TestArityLabel, Arity]),
            {GetElements, Rest4, MSt2} =
                collect_get_tuple_elements(Rest3, TestArityArg, MMod, MSt1, State0),
            MStFused = emit_fused_tuple_ops(
                IsTupleLabel, TestArityLabel, Arg1, Arity, GetElements, MMod, MSt2
            ),
            {fused, MStFused, Rest4};
        false ->
            not_fused
    end;
try_fuse_tuple_ops(_Rest, _Arg1, _IsTupleLabel, _MMod, _MSt, _State) ->
    not_fused.

collect_get_tuple_elements(
    <<?OP_GET_TUPLE_ELEMENT, Rest0/binary>> = FullBin, SrcArg, MMod, MSt0, State0
) ->
    {MSt1, Source, Rest1} = decode_compact_term(Rest0, MMod, MSt0, State0),
    {Element, Rest2} = decode_literal(Rest1),
    {MSt2, Dest, Rest3} = decode_dest(Rest2, MMod, MSt1),
    case Source =:= SrcArg of
        true ->
            ?TRACE("FUSE: + get_tuple_element ~p, ~p, ~p\n", [Source, Element, Dest]),
            case Dest =:= SrcArg of
                true ->
                    %% This get_tuple_element overwrites the source register.
                    %% Include it but stop collecting: subsequent get_tuple_elements
                    %% would read from the new value, not the original tuple.
                    {[{Element, Dest}], Rest3, MSt2};
                false ->
                    {MoreElements, RestN, MStN} =
                        collect_get_tuple_elements(Rest3, SrcArg, MMod, MSt2, State0),
                    {[{Element, Dest} | MoreElements], RestN, MStN}
            end;
        false ->
            {[], FullBin, MSt0}
    end;
collect_get_tuple_elements(Rest, _SrcArg, _MMod, MSt, _State0) ->
    {[], Rest, MSt}.

emit_fused_tuple_ops(IsTupleLabel, TestArityLabel, Arg1, Arity, GetElements, MMod, MSt0) ->
    %% The BEAM Types chunk (versions 2-4, up to OTP 29) does not encode tuple
    %% arity, so {typed, _, t_tuple} never carries an arity to specialize on.
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, unwrap_typed(Arg1)),
    MSt2 = cond_jump_to_label(
        {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED},
        IsTupleLabel,
        MMod,
        MSt1
    ),
    {MSt3, Reg} = MMod:and_(MSt2, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, HeaderReg} = MMod:get_array_element(MSt3, Reg, 0),
    MSt4a = cond_jump_to_label(
        {HeaderReg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_TUPLE},
        IsTupleLabel,
        MMod,
        MSt4
    ),
    {MSt4b, ArityReg} = MMod:shift_right(MSt4a, {free, HeaderReg}, 6),
    MSt5 = cond_jump_to_label({{free, ArityReg}, '!=', Arity}, TestArityLabel, MMod, MSt4b),
    MSt8 = lists:foldl(
        fun({Element, Dest}, AccMSt0) ->
            AccMSt1 = MMod:move_array_element(AccMSt0, Reg, Element + 1, Dest),
            MMod:free_native_registers(AccMSt1, [Dest])
        end,
        MSt5,
        GetElements
    ),
    MSt9 = MMod:free_native_registers(MSt8, [Reg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt9),
    MSt9.

%% @doc verify_match_state and return the term_ptr for Reg.
%% Actually, this means Reg isn't restored with OR ?TERM_PRIMARY_BOXED
verify_is_match_state_and_get_ptr(MMod, MSt0, {typed, Src, {t_bs_matchable, _Unit}}) ->
    %% If Src is of type t_bs_matchable, it means it's boxed but we need to check
    %% if it is a bin_match_state (OTP27 type had a bs_context type but it's
    %% gone with OTP28)
    {MSt1, SrcReg} = MMod:move_to_native_register(MSt0, Src),
    verify_is_match_state_and_get_ptr0(MMod, MSt1, SrcReg);
verify_is_match_state_and_get_ptr(MMod, MSt0, {typed, Src, _}) ->
    verify_is_match_state_and_get_ptr(MMod, MSt0, Src);
verify_is_match_state_and_get_ptr(MMod, MSt0, Src) ->
    % Default case is to check it's boxed
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Src),
    MSt2 = verify_is_boxed(MMod, MSt1, Reg),
    verify_is_match_state_and_get_ptr0(MMod, MSt2, Reg).

verify_is_match_state_and_get_ptr0(MMod, MSt0, Reg) ->
    {MSt1, Reg} = MMod:and_(MSt0, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt2, BoxTag} = MMod:get_array_element(MSt1, Reg, 0),
    MSt3 = cond_raise_badarg(
        {{free, BoxTag}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_BIN_MATCH_STATE}, MMod, MSt2
    ),
    {MSt3, Reg}.

verify_is_immediate(Arg1, ImmediateTag, FailLabel, MMod, MSt0) ->
    verify_is_immediate(Arg1, ?TERM_IMMED_TAG_MASK, ImmediateTag, FailLabel, MMod, MSt0).

verify_is_immediate(Arg1, ImmediateMask, ImmediateTag, _FailLabel, _MMod, MSt0) when
    is_integer(Arg1) andalso Arg1 band ImmediateMask =:= ImmediateTag
->
    MSt0;
verify_is_immediate(Arg1, ImmediateMask, ImmediateTag, 0, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    cond_raise_badarg(
        {{free, Reg}, '&', ImmediateMask, '!=', ImmediateTag}, MMod, MSt1
    );
verify_is_immediate(Arg1, ImmediateMask, ImmediateTag, FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    cond_jump_to_label(
        {{free, Reg}, '&', ImmediateMask, '!=', ImmediateTag}, FailLabel, MMod, MSt1
    ).

verify_is_integer(Arg1, Fail, MMod, MSt0) ->
    verify_is_immediate(Arg1, ?TERM_INTEGER_TAG, Fail, MMod, MSt0).

verify_is_atom(Arg1, Fail, MMod, MSt0) ->
    verify_is_immediate(Arg1, ?TERM_IMMED2_TAG_MASK, ?TERM_IMMED2_ATOM, Fail, MMod, MSt0).

verify_is_immediate_or_boxed(Arg1, ImmediateTag, BoxedTag, FailLabel, MMod, MSt0) ->
    verify_is_immediate_or_boxed(
        Arg1, ImmediateTag, ?TERM_BOXED_TAG_MASK, BoxedTag, FailLabel, MMod, MSt0
    ).

verify_is_immediate_or_boxed(
    Arg1, ImmediateTag, _BoxedMask, _BoxedTag, _FailLabel, _MMod, MSt0
) when
    is_integer(Arg1) andalso Arg1 band ?TERM_IMMED_TAG_MASK =:= ImmediateTag
->
    MSt0;
verify_is_immediate_or_boxed(
    {free, Arg1}, ImmediateTag, _BoxedMask, _BoxedTag, _FailLabel, _MMod, MSt0
) when
    is_integer(Arg1) andalso Arg1 band ?TERM_IMMED_TAG_MASK =:= ImmediateTag
->
    MSt0;
verify_is_immediate_or_boxed(
    ArgOrTuple, ImmediateTag, BoxedMask, BoxedTag, Label, MMod, MSt0
) ->
    {MSt1, Reg} =
        case ArgOrTuple of
            {free, Arg} -> MMod:move_to_native_register(MSt0, Arg);
            _ -> MMod:copy_to_native_register(MSt0, ArgOrTuple)
        end,
    MSt2 = MMod:if_block(MSt1, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ImmediateTag}, fun(BSt0) ->
        verify_is_boxed_with_tag(Label, {free, Reg}, BoxedMask, BoxedTag, MMod, BSt0)
    end),
    MMod:free_native_registers(MSt2, [Reg]).

verify_is_any_integer(Arg1, Fail, MMod, MSt0) ->
    verify_is_immediate_or_boxed(
        Arg1,
        ?TERM_INTEGER_TAG,
        ?TERM_BOXED_TAG_MASK_NO_SIGN,
        ?TERM_BOXED_POSITIVE_INTEGER,
        Fail,
        MMod,
        MSt0
    ).

verify_is_number(Arg1, Fail, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    {MSt2, IsNumber} = MMod:call_primitive(MSt1, ?PRIM_TERM_IS_NUMBER, [{free, Reg}]),
    cond_raise_badarg_or_jump_to_fail_label(
        {'(bool)', {free, IsNumber}, '==', false}, Fail, MMod, MSt2
    ).

%%-----------------------------------------------------------------------------
%% @doc Test if Arg1 is a binary, jump to FailLabel if it isn't or raise
%% badarg if FailLabel is 0
%% @param Arg1 element to test
%% @param FailLabel label to jump to if Arg1 is not a binary or 0 to raise
%% @param MMod backend module
%% @param MSt0 backend state
%% @return new backend state
%%-----------------------------------------------------------------------------
verify_is_binary(Arg1, FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:copy_to_native_register(MSt0, Arg1),
    MSt2 = verify_is_boxed(MMod, MSt1, Reg, FailLabel),
    {MSt3, Reg} = MMod:and_(MSt2, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt4 = MMod:move_array_element(MSt3, Reg, 0, Reg),
    {MSt5, Reg} = MMod:and_(MSt4, {free, Reg}, ?TERM_BOXED_TAG_MASK),
    MSt6 = cond_raise_badarg_or_jump_to_fail_label(
        {'and', [
            {Reg, '!=', ?TERM_BOXED_REFC_BINARY},
            {Reg, '!=', ?TERM_BOXED_HEAP_BINARY},
            {Reg, '!=', ?TERM_BOXED_SUB_BINARY}
        ]},
        FailLabel,
        MMod,
        MSt5
    ),
    MMod:free_native_registers(MSt6, [Reg]).

cond_raise_badarg(Cond, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?BADARG_ATOM
        ])
    end).

cond_raise_badarg_or_jump_to_fail_label(Cond, 0, MMod, MSt0) ->
    cond_raise_badarg(Cond, MMod, MSt0);
cond_raise_badarg_or_jump_to_fail_label(Cond, FailLabel, MMod, MSt0) when FailLabel > 0 ->
    cond_jump_to_label(Cond, FailLabel, MMod, MSt0).

% Like term_to_int/4 but for utf segment sources, which must be integer code
% points. term_to_int's first clause assumes a bare-integer argument is an
% already-tagged integer term and just shifts it, but decode_compact_term
% returns the term value of a constant non-integer (e.g. the atom 'false',
% folded from a boolean expression) as a bare integer too. Shifting that
% silently turns its tag bits into a bogus code point (the JIT-only bug). Here
% a constant term that is not integer-tagged is a compile-time type error:
% raise badarg / jump to the fail label, matching the emulator's
% VERIFY_IS_INTEGER. All other source shapes (registers, typed registers,
% literals) reuse term_to_int, whose register path already type-checks.
utf_term_to_int(Term, _FailLabel, _MMod, MSt0) when
    is_integer(Term), Term band ?TERM_IMMED_TAG_MASK =:= ?TERM_INTEGER_TAG
->
    {MSt0, Term bsr 4};
utf_term_to_int(Term, FailLabel, MMod, MSt0) when is_integer(Term) ->
    % Constant non-integer term value (e.g. the atom 'false'): materialize it
    % and run the same tag check as the register path below, which always fails
    % here and raises badarg / jumps to the fail label.
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    MSt2 = cond_raise_badarg_or_jump_to_fail_label(
        {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, FailLabel, MMod, MSt1
    ),
    {MSt3, IntReg} = MMod:shift_right(MSt2, {free, Reg}, 4),
    {MSt3, IntReg};
utf_term_to_int(Term, FailLabel, MMod, MSt0) ->
    term_to_int(Term, FailLabel, MMod, MSt0).

term_to_int(Term, _FailLabel, _MMod, MSt0) when is_integer(Term) ->
    {MSt0, Term bsr 4};
term_to_int({literal, Val}, _FailLabel, _MMod, MSt0) when is_integer(Val) ->
    {MSt0, Val};
% Optimized case: when we have type information showing this is an integer, skip the type check
term_to_int({typed, Term, {t_integer, _Range}}, _FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    {MSt2, IntReg} = MMod:shift_right(MSt1, {free, Reg}, 4),
    {MSt2, IntReg};
term_to_int({typed, Term, _NonIntegerType}, FailLabel, MMod, MSt0) ->
    % Type information shows it's not an integer, fall back to generic path
    term_to_int(Term, FailLabel, MMod, MSt0);
term_to_int(Term, FailLabel, MMod, MSt0) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    MSt2 = cond_raise_badarg_or_jump_to_fail_label(
        {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, FailLabel, MMod, MSt1
    ),
    {MSt3, IntReg} = MMod:shift_right(MSt2, {free, Reg}, 4),
    {MSt3, IntReg}.

%% @doc Peek at the OP_PUT_RECORD `Id' argument and try to resolve it to a
%% record defined in the current module. Returns `{ok, RecInfo, Rest}' on
%% success, where `Rest' points past the consumed Id bytes, or `not_local' if
%% the id is unresolvable or points to a record defined elsewhere.
peek_local_record_id(<<0:4, ?COMPACT_ATOM:4, _/binary>>, _State) ->
    %% Atom index 0 is NIL — never a record id.
    not_local;
peek_local_record_id(<<_:4, ?COMPACT_ATOM:4, _/binary>> = Bin, #state{
    atom_resolver = AR, record_resolver = RR
}) ->
    {AtomIndex, Rest} = decode_atom(Bin),
    case RR(AR(AtomIndex)) of
        undefined -> not_local;
        Info -> {ok, Info, Rest}
    end;
peek_local_record_id(<<_:4, ?COMPACT_LARGE_ATOM:4, _/binary>> = Bin, #state{
    atom_resolver = AR, record_resolver = RR
}) ->
    {AtomIndex, Rest} = decode_atom(Bin),
    case RR(AR(AtomIndex)) of
        undefined -> not_local;
        Info -> {ok, Info, Rest}
    end;
peek_local_record_id(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>, #state{
    atom_resolver = AR, literal_resolver = LR, record_resolver = RR
}) ->
    {LitIndex, Rest} = decode_literal(Rest0),
    case LR(LitIndex) of
        Atom when is_atom(Atom) ->
            case RR(Atom) of
                undefined -> not_local;
                Info -> {ok, Info, Rest}
            end;
        {Mod, Name} when is_atom(Mod), is_atom(Name) ->
            case AR(1) of
                Mod ->
                    case RR(Name) of
                        undefined -> not_local;
                        Info -> {ok, Info, Rest}
                    end;
                _ ->
                    not_local
            end;
        _ ->
            not_local
    end;
peek_local_record_id(_Bin, _State) ->
    not_local.

%% @doc Specialized OP_PUT_RECORD path for records defined in the current
%% module. The record-def lookup and per-field name-scan are resolved at JIT
%% compile time; the runtime only sees a primitive call that takes the record
%% index and a (position, value) array.
put_record_resolved(Rest2, #{index := RecIdx, fields := FieldAtoms}, MMod, MSt0, State0) ->
    NumFields = length(FieldAtoms),
    HeapNeed = NumFields + 2,
    FieldPos = field_position_map(FieldAtoms),

    {MSt1, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt0, State0),
    {MSt2, Dest, Rest4} = decode_dest(Rest3, MMod, MSt1),
    {Live, Rest5} = decode_literal(Rest4),
    {ListLen, Rest6} = decode_extended_list_header(Rest5),
    ?TRACE("OP_PUT_RECORD (resolved idx=~p) Src=~p Dest=~p Live=~p\n", [RecIdx, Src, Dest, Live]),
    NumPairs = ListLen div 2,

    {MSt3, TrimReg} = MMod:call_primitive(MSt2, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt4 = MMod:free_native_registers(MSt3, [TrimReg]),
    {MSt5, NewSrc} = memory_ensure_free_with_extra_root(Src, Live, HeapNeed, MMod, MSt4),
    {MSt6, KVReg} =
        if
            NumPairs > 0 ->
                MMod:call_primitive(MSt5, ?PRIM_MALLOC, [
                    ctx, jit_state, NumPairs * 2 * MMod:word_size()
                ]);
            true ->
                MMod:move_to_native_register(MSt5, 0)
        end,
    MSt7 =
        if
            NumPairs > 0 -> handle_error_if({KVReg, '==', 0}, MMod, MSt6);
            true -> MSt6
        end,
    {MSt8, Rest7} = lists:foldl(
        fun(Index, {ASt0, ARest0}) ->
            {KeyAtomIndex, ARest1} = decode_atom(ARest0),
            #state{atom_resolver = AR} = State0,
            KeyAtom = AR(KeyAtomIndex),
            Position =
                case maps:find(KeyAtom, FieldPos) of
                    {ok, Pos} -> Pos;
                    error -> error({jit, unknown_record_field, KeyAtom})
                end,
            {ASt1, Value, ARest2} = decode_compact_term(ARest1, MMod, ASt0, State0),
            ASt2 = MMod:move_to_array_element(ASt1, Position, KVReg, Index * 2),
            ASt3 = MMod:move_to_array_element(ASt2, Value, KVReg, (Index * 2) + 1),
            ASt4 = MMod:free_native_registers(ASt3, [Value]),
            {ASt4, ARest2}
        end,
        {MSt7, Rest6},
        lists:seq(0, NumPairs - 1)
    ),
    {MSt9, ResultReg} = MMod:call_primitive(MSt8, ?PRIM_PUT_RECORD_RESOLVED, [
        ctx, jit_state, RecIdx, {free, NewSrc}, NumPairs, KVReg
    ]),
    MSt10 =
        if
            NumPairs > 0 ->
                {Ms, FreeReg} = MMod:call_primitive(MSt9, ?PRIM_FREE, [{free, KVReg}]),
                MMod:free_native_registers(Ms, [FreeReg]);
            true ->
                MMod:free_native_registers(MSt9, [KVReg])
        end,
    MSt11 = handle_error_if({ResultReg, '==', 0}, MMod, MSt10),
    MSt12 = MMod:move_to_vm_register(MSt11, ResultReg, Dest),
    MSt13 = MMod:free_native_registers(MSt12, [ResultReg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt13),
    first_pass(Rest7, MMod, MSt13, State0).

%% @doc Specialized OP_GET_RECORD_ELEMENTS — src is known to be a record of a
%% module-local type, so every field name resolves to a JIT-time-known boxed
%% offset and the per-field PRIM_RECORD_FIELD_POS call is eliminated.
%%
%% A pre-pass validates that every requested field exists in the tracked
%% record's layout. If any doesn't (the tracking was stale — src was rewritten
%% with a different record type between IS_NATIVE_RECORD and here), fall back
%% to the generic primitive path which does its own runtime type check.
get_record_elements_resolved(
    Src,
    FieldAtoms,
    Fail,
    NumPairs,
    Rest3,
    MMod,
    MSt0,
    #state{atom_resolver = AR} = State0
) ->
    FieldPos = field_position_map(FieldAtoms),
    case resolve_field_positions(NumPairs, Rest3, AR, FieldPos, []) of
        {ok, Positions, RestAfterList} ->
            emit_get_record_elements_resolved(
                Src, NumPairs, Rest3, Positions, RestAfterList, MMod, MSt0, State0
            );
        stale ->
            get_record_elements_generic(
                Src, Fail, NumPairs, Rest3, MMod, MSt0, State0
            )
    end.

%% Walk the field-name list at JIT time, resolving each to its boxed position
%% via the tracked record's layout. Returns `{ok, [Pos], RestAfterList}' on
%% success, or `stale' if any field isn't in the tracked layout.
resolve_field_positions(0, Rest, _AR, _FieldPos, Acc) ->
    {ok, lists:reverse(Acc), Rest};
resolve_field_positions(N, Bin0, AR, FieldPos, Acc) ->
    {AtomIndex, Bin1} = decode_atom(Bin0),
    %% Skip the dest register slot — we'll re-decode it during emission.
    Bin2 = skip_dest(Bin1),
    case maps:find(AR(AtomIndex), FieldPos) of
        {ok, P} ->
            resolve_field_positions(N - 1, Bin2, AR, FieldPos, [P | Acc]);
        error ->
            stale
    end.

%% Skip past one DEST register encoding.
skip_dest(<<_RegIndex:4, ?COMPACT_XREG:4, Rest/binary>>) -> Rest;
skip_dest(<<_RegIndex:4, ?COMPACT_YREG:4, Rest/binary>>) -> Rest;
skip_dest(<<_:3, 0:1, ?COMPACT_LARGE_XREG:4, _, Rest/binary>>) -> Rest;
skip_dest(<<_:3, 0:1, ?COMPACT_LARGE_YREG:4, _, Rest/binary>>) -> Rest.

emit_get_record_elements_resolved(
    Src,
    NumPairs,
    Rest3,
    Positions,
    RestAfterList,
    MMod,
    MSt0,
    State0
) ->
    {MSt1, SrcReg} = MMod:move_to_native_register(MSt0, Src),
    {MSt2, SrcPtrReg} = MMod:copy_to_native_register(MSt1, SrcReg),
    {MSt3, SrcPtrReg} = MMod:and_(MSt2, {free, SrcPtrReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, _} = lists:foldl(
        fun({_Idx, Position}, {ASt0, ARest0}) ->
            {_AtomIndex, ARest1} = decode_atom(ARest0),
            {ASt1, Dest, ARest2} = decode_dest(ARest1, MMod, ASt0),
            ASt2 = MMod:move_array_element(ASt1, SrcPtrReg, Position, Dest),
            ASt3 = MMod:free_native_registers(ASt2, [Dest]),
            {ASt3, ARest2}
        end,
        {MSt3, Rest3},
        lists:zip(lists:seq(1, NumPairs), Positions)
    ),
    MSt5 = MMod:free_native_registers(MSt4, [SrcReg, SrcPtrReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt5),
    first_pass(RestAfterList, MMod, MSt5, State0).

%% @doc Generic OP_GET_RECORD_ELEMENTS — src record type isn't tracked, so
%% each field name is resolved at runtime via PRIM_RECORD_FIELD_POS and the
%% bytecode-supplied fail label is used for missing-field errors.
get_record_elements_generic(
    Src,
    Fail,
    NumPairs,
    Rest3,
    MMod,
    MSt0,
    #state{atom_resolver = AtomResolver} = State0
) ->
    {MSt1, SrcReg} = MMod:move_to_native_register(MSt0, Src),
    {MSt2, SrcPtrReg} = MMod:copy_to_native_register(MSt1, SrcReg),
    {MSt3, SrcPtrReg} = MMod:and_(MSt2, {free, SrcPtrReg}, ?TERM_PRIMARY_CLEAR_MASK),
    {MSt4, Rest4} = lists:foldl(
        fun(_Idx, {ASt0, ARest0}) ->
            {AtomIndex, ARest1} = decode_atom(ARest0),
            {ASt1, Dest, ARest2} = decode_dest(ARest1, MMod, ASt0),
            ASt2 = MMod:free_native_registers(ASt1, [Dest]),
            {ASt3, FieldName} =
                case maps:find(AtomResolver(AtomIndex), ?DEFAULT_ATOMS) of
                    error ->
                        MMod:call_primitive(
                            ASt2, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
                        );
                    {ok, Val} ->
                        {ASt2, Val}
                end,
            {ASt4, PosReg} = MMod:call_primitive(ASt3, ?PRIM_RECORD_FIELD_POS, [
                SrcReg, {free, FieldName}
            ]),
            ASt5 = cond_jump_to_label({{free, PosReg}, '==', 0}, Fail, MMod, ASt4),
            {ASt5, ARest2}
        end,
        {MSt3, Rest3},
        lists:seq(1, NumPairs)
    ),
    {MSt5, _} = lists:foldl(
        fun(_Idx, {ASt0, ARest0}) ->
            {AtomIndex, ARest1} = decode_atom(ARest0),
            {ASt1, Dest, ARest2} = decode_dest(ARest1, MMod, ASt0),
            {ASt2, FieldName} =
                case maps:find(AtomResolver(AtomIndex), ?DEFAULT_ATOMS) of
                    error ->
                        MMod:call_primitive(
                            ASt1, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
                        );
                    {ok, Val} ->
                        {ASt1, Val}
                end,
            {ASt3, PosReg} = MMod:call_primitive(ASt2, ?PRIM_RECORD_FIELD_POS, [
                SrcReg, {free, FieldName}
            ]),
            ASt4 = MMod:move_array_element(ASt3, SrcPtrReg, {free, PosReg}, Dest),
            ASt5 = MMod:free_native_registers(ASt4, [Dest]),
            {ASt5, ARest2}
        end,
        {MSt4, Rest3},
        lists:seq(1, NumPairs)
    ),
    MSt6 = MMod:free_native_registers(MSt5, [SrcReg, SrcPtrReg]),
    ?ASSERT_ALL_NATIVE_FREE(MSt6),
    first_pass(Rest4, MMod, MSt6, State0).

%% Record the assertion that `Src' holds a value of the module-local record
%% type identified by `(ModAtomIndex, NameAtomIndex)'. The backend's jit_regs
%% state holds the assertion and invalidates it on writes to Src, on C calls
%% clobbering x regs, and at labels.
maybe_track_record_type({x_reg, _} = Src, ModAtomIndex, NameAtomIndex, AR, RR, MMod, MSt) ->
    track_if_local(Src, ModAtomIndex, NameAtomIndex, AR, RR, MMod, MSt);
maybe_track_record_type({y_reg, _} = Src, ModAtomIndex, NameAtomIndex, AR, RR, MMod, MSt) ->
    track_if_local(Src, ModAtomIndex, NameAtomIndex, AR, RR, MMod, MSt);
maybe_track_record_type(_Src, _ModAtomIndex, _NameAtomIndex, _AR, _RR, _MMod, MSt) ->
    MSt.

track_if_local(Src, ModAtomIndex, NameAtomIndex, AR, RR, MMod, MSt) ->
    ModAtom = AR(ModAtomIndex),
    case AR(1) of
        ModAtom ->
            case RR(AR(NameAtomIndex)) of
                undefined ->
                    MSt;
                Info ->
                    MMod:set_vm_record_type(MSt, Src, Info)
            end;
        _ ->
            MSt
    end.

%% Build {FieldAtom => Position} where Position is the 1-based boxed-array
%% index (counting the def-pointer slot), matching jit_record_field_pos return
%% values: first declared field is position 2.
field_position_map(FieldAtoms) ->
    {Map, _} = lists:foldl(
        fun(Atom, {Acc, Pos}) -> {Acc#{Atom => Pos}, Pos + 1} end,
        {#{}, 2},
        FieldAtoms
    ),
    Map.

%% @doc Generic OP_PUT_RECORD path (cross-module records or records not
%% resolvable at JIT compile time). Falls back to runtime def lookup via
%% PRIM_RECORD_DEF_ARITY + PRIM_PUT_RECORD.
put_record_generic(Rest1, MMod, MSt0, State0) ->
    {MSt1, Id, Rest2} = decode_compact_term(Rest1, MMod, MSt0, State0),
    {MSt2, Src, Rest3} = decode_compact_term(Rest2, MMod, MSt1, State0),
    {MSt3, Dest, Rest4} = decode_dest(Rest3, MMod, MSt2),
    {Live, Rest5} = decode_literal(Rest4),
    {ListLen, Rest6} = decode_extended_list_header(Rest5),
    ?TRACE("OP_PUT_RECORD ~p, ~p, ~p, ~p\n", [Id, Src, Dest, Live]),
    NumPairs = ListLen div 2,
    {MSt4, ArityReg} = MMod:call_primitive(MSt3, ?PRIM_RECORD_DEF_ARITY, [
        ctx, jit_state, {free, Id}
    ]),
    MSt6 = MMod:add(MSt4, ArityReg, 2),
    {MSt7, TrimReg} = MMod:call_primitive(MSt6, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
    MSt8 = MMod:free_native_registers(MSt7, [TrimReg]),
    {MSt8a, NewSrc} = memory_ensure_free_with_extra_root(Src, Live, {free, ArityReg}, MMod, MSt8),
    {MSt9, NewId, _} = decode_compact_term(Rest1, MMod, MSt8a, State0),
    {MSt10, KVReg} =
        if
            NumPairs > 0 ->
                MMod:call_primitive(MSt9, ?PRIM_MALLOC, [
                    ctx, jit_state, NumPairs * 2 * MMod:word_size()
                ]);
            true ->
                MMod:move_to_native_register(MSt9, 0)
        end,
    MSt11 =
        if
            NumPairs > 0 -> handle_error_if({KVReg, '==', 0}, MMod, MSt10);
            true -> MSt10
        end,
    {MSt12, Rest7} = lists:foldl(
        fun(Index, {ASt0, ARest0}) ->
            {ASt1, Key, ARest1} = decode_compact_term(ARest0, MMod, ASt0, State0),
            {ASt2, Value, ARest2} = decode_compact_term(ARest1, MMod, ASt1, State0),
            ASt3 = MMod:move_to_array_element(ASt2, Key, KVReg, Index * 2),
            ASt4 = MMod:move_to_array_element(ASt3, Value, KVReg, (Index * 2) + 1),
            ASt5 = MMod:free_native_registers(ASt4, [Key, Value]),
            {ASt5, ARest2}
        end,
        {MSt11, Rest6},
        lists:seq(0, NumPairs - 1)
    ),
    {MSt13, ResultReg} = MMod:call_primitive(MSt12, ?PRIM_PUT_RECORD, [
        ctx, jit_state, {free, NewId}, {free, NewSrc}, NumPairs, KVReg
    ]),
    MSt14 =
        if
            NumPairs > 0 ->
                {Ms, FreeReg} = MMod:call_primitive(MSt13, ?PRIM_FREE, [{free, KVReg}]),
                MMod:free_native_registers(Ms, [FreeReg]);
            true ->
                MMod:free_native_registers(MSt13, [KVReg])
        end,
    MSt15 = handle_error_if({ResultReg, '==', 0}, MMod, MSt14),
    MSt16 = MMod:move_to_vm_register(MSt15, ResultReg, Dest),
    MSt17 = MMod:free_native_registers(MSt16, [ResultReg, Dest]),
    ?ASSERT_ALL_NATIVE_FREE(MSt17),
    first_pass(Rest7, MMod, MSt17, State0).

%% OP_FCONV: convert a term to a double in fr[FPRegIndex], raising badarith if
%% the term is not a number.
%%
%% Type information from the compiler lets us specialise: an integer- or
%% number-typed source is provably a number, so the runtime term_is_number
%% guard is unnecessary. On FPU backends an integer-typed source additionally
%% gets an inline fast path for the common small-immediate-integer case
%% (untag + int->double), only calling the C term_conv_to_float for boxed
%% integers / bignums.
op_fconv(MMod, MSt0, {typed, Term, {t_integer, _Range}}, FPRegIndex) ->
    %% Provably an integer (small immediate or bignum). On FPU backends inline
    %% the common immediate-integer case; otherwise use the C conversion.
    case MMod:supports_fp(MSt0) of
        true -> op_fconv_int_inline(MMod, MSt0, Term, FPRegIndex);
        false -> op_fconv_number(MMod, MSt0, Term, FPRegIndex)
    end;
op_fconv(MMod, MSt0, {typed, Term, {t_number, _Range}}, FPRegIndex) ->
    %% Provably a number (integer or float). On FPU backends inline the
    %% immediate-integer case; a boxed float or bignum goes through the C
    %% conversion (which handles both), avoiding a boxed-header tag test here.
    case MMod:supports_fp(MSt0) of
        true -> op_fconv_int_inline(MMod, MSt0, Term, FPRegIndex);
        false -> op_fconv_number(MMod, MSt0, Term, FPRegIndex)
    end;
op_fconv(MMod, MSt0, {typed, Term, {t_float, _Range}}, FPRegIndex) ->
    %% Provably a float, hence always a boxed float: unbox inline on FPU backends.
    case MMod:supports_fp(MSt0) of
        true ->
            {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
            MSt2 = ensure_fpregs(MMod, MSt1),
            MMod:float_conv_float(MSt2, {free, Reg}, FPRegIndex);
        false ->
            op_fconv_number(MMod, MSt0, Term, FPRegIndex)
    end;
op_fconv(MMod, MSt0, {typed, Term, _OtherType}, FPRegIndex) ->
    %% A non-number static type would be a badarith at runtime; keep the guarded
    %% generic path rather than assuming.
    op_fconv_guarded(MMod, MSt0, Term, FPRegIndex);
op_fconv(MMod, MSt0, SrcValue, FPRegIndex) ->
    op_fconv_guarded(MMod, MSt0, SrcValue, FPRegIndex).

%% Inline conversion of an integer-typed source: immediate small integer is
%% untagged and converted in registers; a boxed integer / bignum falls back to
%% the C term_conv_to_float.
op_fconv_int_inline(MMod, MSt0, Term, FPRegIndex) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    MSt2 = ensure_fpregs(MMod, MSt1),
    %% Test the immediate tag with '!=' (the only '&'-mask form the backends
    %% support), so the boxed/bignum fallback is the "then" branch and the
    %% inline immediate-int conversion is the "else".
    MSt3 = MMod:if_else_block(
        MSt2,
        {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        %% Boxed integer / bignum: fall back to the C conversion.
        fun(BSt0) ->
            {BSt1, ConvReg} = MMod:call_primitive(BSt0, ?PRIM_TERM_CONV_TO_FLOAT, [
                ctx, Reg, FPRegIndex
            ]),
            MMod:free_native_registers(BSt1, [ConvReg])
        end,
        %% Immediate small integer: untag (arithmetic shift to preserve the sign
        %% of negative values) and convert inline.
        fun(BSt0) ->
            {BSt1, IntReg} = MMod:shift_right_arith(BSt0, Reg, 4),
            BSt2 = MMod:float_conv_int(BSt1, IntReg, FPRegIndex),
            MMod:free_native_registers(BSt2, [IntReg])
        end
    ),
    MMod:free_native_registers(MSt3, [Reg]).

%% Ensure the fp register array is allocated. context_ensure_fpregs only does
%% a lazy malloc on the first call, so on FPU backends test ctx->fr inline and
%% make the C call (with its register spill) only when the array has not been
%% allocated yet; in the steady state (the array already exists) this is just a
%% load + branch. Backends without inline FP support call the primitive
%% directly.
ensure_fpregs(MMod, MSt0) ->
    case MMod:supports_fp(MSt0) of
        true ->
            {MSt1, FpRegsPtr} = MMod:read_fp_regs_ptr(MSt0),
            MMod:if_block(MSt1, {{free, FpRegsPtr}, '==', 0}, fun(BSt0) ->
                {BSt1, EnsureReg} = MMod:call_primitive(BSt0, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
                MMod:free_native_registers(BSt1, [EnsureReg])
            end);
        false ->
            {MSt1, EnsureReg} = MMod:call_primitive(MSt0, ?PRIM_CONTEXT_ENSURE_FPREGS, [ctx]),
            MMod:free_native_registers(MSt1, [EnsureReg])
    end.

%% Convert a value already known to be a number (no term_is_number guard) using
%% the C term_conv_to_float, which handles small int, boxed int/bignum and float.
op_fconv_number(MMod, MSt0, Term, FPRegIndex) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, Term),
    MSt2 = ensure_fpregs(MMod, MSt1),
    {MSt3, ConvReg} = MMod:call_primitive(MSt2, ?PRIM_TERM_CONV_TO_FLOAT, [
        ctx, {free, Reg}, FPRegIndex
    ]),
    MMod:free_native_registers(MSt3, [ConvReg]).

%% Generic path with the runtime term_is_number check (raises badarith on a
%% non-number), used when no type information proves the source is a number.
op_fconv_guarded(MMod, MSt0, SrcValue, FPRegIndex) ->
    {MSt1, Reg} = MMod:move_to_native_register(MSt0, SrcValue),
    {MSt2, IsNumber} = MMod:call_primitive(MSt1, ?PRIM_TERM_IS_NUMBER, [Reg]),
    MSt3 = MMod:if_block(MSt2, {'(bool)', {free, IsNumber}, '==', false}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?BADARITH_ATOM
        ])
    end),
    MSt4 = ensure_fpregs(MMod, MSt3),
    {MSt5, ConvReg} = MMod:call_primitive(MSt4, ?PRIM_TERM_CONV_TO_FLOAT, [
        ctx, {free, Reg}, FPRegIndex
    ]),
    MMod:free_native_registers(MSt5, [ConvReg]).

first_pass_float3(Primitive, Rest0, MMod, MSt0, State0) ->
    %% The Erlang compiler always emits fadd/fsub/fmul/fdiv with fail label 0
    %% (beam_validator asserts ?EXCEPTION_LABEL = Fail), and the BEAM loader's
    %% ops.tab rewrites only match the `p` (label-0) form. A non-zero fail
    %% label is therefore unreachable from any loadable BEAM file.
    {0, Rest1} = decode_label(Rest0),
    {{fp_reg, FPRegIndex1}, Rest2} = decode_fp_register(Rest1),
    {{fp_reg, FPRegIndex2}, Rest3} = decode_fp_register(Rest2),
    {{fp_reg, FPRegIndex3}, Rest4} = decode_fp_register(Rest3),
    ?TRACE("OP_F3*~p ~p, ~p, ~p\n", [
        Primitive, {fp_reg, FPRegIndex1}, {fp_reg, FPRegIndex2}, {fp_reg, FPRegIndex3}
    ]),
    %% Backends with a hardware FPU inline the operation and a finiteness check;
    %% the others fall back to the C primitive. Both yield a register that is
    %% false (0) iff the result is non-finite, so badarith handling is shared.
    {MSt1, Reg} =
        case MMod:supports_fp(MSt0) of
            true ->
                MMod:float_op(MSt0, Primitive, FPRegIndex1, FPRegIndex2, FPRegIndex3);
            false ->
                MMod:call_primitive(MSt0, Primitive, [
                    ctx, FPRegIndex1, FPRegIndex2, FPRegIndex3
                ])
        end,
    MSt2 = MMod:if_block(MSt1, {'(bool)', {free, Reg}, '==', false}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_RAISE_ERROR, [
            ctx, jit_state, offset, ?BADARITH_ATOM
        ])
    end),
    ?ASSERT_ALL_NATIVE_FREE(MSt2),
    first_pass(Rest4, MMod, MSt2, State0).

bif_faillabel_test(FailLabel, MMod, MSt0, {free, ResultReg}, {free, Dest}) when FailLabel > 0 ->
    MSt1 = cond_jump_to_label({ResultReg, '==', 0}, FailLabel, MMod, MSt0),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, Dest),
    MMod:free_native_registers(MSt2, [ResultReg, Dest]);
bif_faillabel_test(0, MMod, MSt0, {free, ResultReg}, {free, Dest}) ->
    MSt1 = handle_error_if({ResultReg, '==', 0}, MMod, MSt0),
    MSt2 = MMod:move_to_vm_register(MSt1, ResultReg, Dest),
    MMod:free_native_registers(MSt2, [ResultReg, Dest]).

memory_ensure_free_with_extra_root({x_reg, N} = ExtraRoot, Live, Size, MMod, MSt0) when N < Live ->
    {MSt1, MemoryEnsureFreeReg} = MMod:call_primitive(MSt0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live, ?MEMORY_CAN_SHRINK
    ]),
    MSt2 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt1),
    {MSt2, ExtraRoot};
memory_ensure_free_with_extra_root(ExtraRoot, Live, Size, MMod, MSt0) when is_atom(ExtraRoot) ->
    ExtraRootXReg =
        if
            Live < ?MAX_REG ->
                {x_reg, Live};
            true ->
                {x_reg, extra}
        end,
    MSt1 = MMod:move_to_vm_register(MSt0, ExtraRoot, ExtraRootXReg),
    MSt2 = MMod:free_native_registers(MSt1, [ExtraRoot]),
    {MSt3, MemoryEnsureFreeReg} = MMod:call_primitive(MSt2, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live + 1, ?MEMORY_CAN_SHRINK
    ]),
    MSt4 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt3),
    MMod:move_to_native_register(MSt4, ExtraRootXReg);
memory_ensure_free_with_extra_root(ExtraRoot, Live, Size, MMod, MSt0) when is_integer(ExtraRoot) ->
    {MSt1, MemoryEnsureFreeReg} = MMod:call_primitive(MSt0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live, ?MEMORY_CAN_SHRINK
    ]),
    MSt2 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt1),
    {MSt2, ExtraRoot};
memory_ensure_free_with_extra_root(ExtraRoot, Live, Size, MMod, MSt0) when is_tuple(ExtraRoot) ->
    ExtraRootXReg =
        if
            Live < ?MAX_REG ->
                {x_reg, Live};
            true ->
                {x_reg, extra}
        end,
    MSt1 = MMod:move_to_vm_register(MSt0, ExtraRoot, ExtraRootXReg),
    {MSt2, MemoryEnsureFreeReg} = MMod:call_primitive(MSt1, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, Size, Live + 1, ?MEMORY_CAN_SHRINK
    ]),
    MSt3 = handle_error_if({'(bool)', {free, MemoryEnsureFreeReg}, '==', false}, MMod, MSt2),
    MSt4 = MMod:move_to_vm_register(MSt3, ExtraRootXReg, ExtraRoot),
    {MSt4, ExtraRoot}.

%% Record the current line at the current offset, so that continuation labels
%% created by function-splitting operations (WASM) have line info.
%% On native backends this is a no-op as the offset is within the existing range.
record_continuation_line(_MMod, _MSt, #state{current_line = undefined} = State) ->
    State;
record_continuation_line(MMod, MSt, #state{current_line = Line, line_offsets = AccLines} = State) ->
    Offset = MMod:offset(MSt),
    State#state{line_offsets = [{Line, Offset} | AccLines]}.

second_pass(MMod, MSt0, #state{line_offsets = Lines}) ->
    ?TRACE("SECOND PASS -- ~B lines\n", [length(Lines)]),
    % Add extra function that returns labels and line information
    MSt1 = MMod:add_label(MSt0, 0),
    SortedLines = lists:keysort(2, Lines),
    MSt2 = MMod:return_labels_and_lines(MSt1, SortedLines),
    MMod:update_branches(MSt2).

decode_literal(<<_Value:5, ?COMPACT_LITERAL:3, _Rest/binary>> = Binary) ->
    decode_value64(Binary);
decode_literal(<<_:4, ?COMPACT_LARGE_LITERAL:4, _Rest/binary>> = Bin) ->
    decode_value64(Bin).

decode_label(<<_Value:5, ?COMPACT_LABEL:3, _Rest/binary>> = Binary) ->
    decode_value64(Binary).

decode_atom(<<_Value:5, ?COMPACT_ATOM:3, _Rest/binary>> = Binary) ->
    decode_value64(Binary).

decode_atom_or_label(<<_Value:5, ?COMPACT_ATOM:3, _Rest/binary>> = Binary, #state{
    atom_resolver = AtomResolver
}) ->
    {AtomIndex, Rest1} = decode_value64(Binary),
    {AtomResolver(AtomIndex), Rest1};
decode_atom_or_label(Binary, _State0) ->
    decode_label(Binary).

decode_nil(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>) ->
    Rest.

decode_value64(<<Val:4, 0:1, _:3, Rest/binary>>) -> {Val, Rest};
decode_value64(<<Val:3, 1:2, _:3, NextByte, Rest/binary>>) -> {(Val bsl 8) bor NextByte, Rest};
decode_value64(<<Size0:3, 3:2, _:3, Value:(8 * (Size0 + 2)), Rest/binary>>) -> {Value, Rest}.

% @doc Decode a compact term to an integer or to a register tuple, building
% code if it needs to be decoded at runtime.
-spec decode_compact_term(binary(), module(), any(), #state{}) ->
    {
        any(),
        integer()
        | {x_reg, non_neg_integer()}
        | {y_reg, non_neg_integer()}
        | {atom, integer(), atom()}
        | {ptr, any()},
        binary()
    }.
decode_compact_term(<<_:4, ?COMPACT_INTEGER:4, _Rest/binary>> = Bin, _MMod, MSt, _State) ->
    {Value, Rest} = decode_value64(Bin),
    {MSt, term_from_int(Value), Rest};
decode_compact_term(
    <<Val:3, ?COMPACT_LARGE_INTEGER_11BITS:5, NextByte, Rest/binary>>, _MMod, MSt, _State
) ->
    {MSt, term_from_int((Val bsl 8) bor NextByte), Rest};
decode_compact_term(
    <<7:3, ?COMPACT_LARGE_INTEGER_NBITS:5, Rest/binary>>,
    MMod,
    MSt,
    _State
) ->
    {DecodedLen, Rest1} = decode_literal(Rest),
    % 7 actually means 7 + 2, that means an integer that is >= 9 bytes
    IntegerByteLen = DecodedLen + 9,
    <<Value:(IntegerByteLen * 8)/signed-big-integer, Rest2/binary>> = Rest1,
    decode_compact_term_big_integer(Value, MMod, MSt, Rest2);
decode_compact_term(
    <<Size0:3, ?COMPACT_LARGE_INTEGER_NBITS:5, Value:(8 * (Size0 + 2))/signed, Rest/binary>>,
    MMod,
    MSt,
    _State
) ->
    decode_compact_term_integer(Value, MMod, MSt, Rest);
decode_compact_term(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>, _MMod, MSt, _State) ->
    {MSt, ?TERM_NIL, Rest};
decode_compact_term(<<_:4, ?COMPACT_ATOM:4, _Rest/binary>> = Bin, MMod, MSt, State) ->
    {Value, Rest} = decode_value64(Bin),
    decode_compact_term_atom(Value, MMod, MSt, Rest, State);
decode_compact_term(<<_:4, ?COMPACT_LARGE_ATOM:4, _Rest/binary>> = Bin, MMod, MSt, State) ->
    {Value, Rest} = decode_value64(Bin),
    decode_compact_term_atom(Value, MMod, MSt, Rest, State);
decode_compact_term(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>, MMod, MSt, _State) ->
    {Value, Rest1} = decode_literal(Rest0),
    decode_compact_term_module_literal(Value, MMod, MSt, Rest1);
decode_compact_term(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>, MMod, MSt0, _State) ->
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {_Type, Rest2} = decode_literal(Rest1),
    {MSt1, Dest, Rest2};
decode_compact_term(<<_Value:5, ?COMPACT_LABEL:3, _Rest/binary>> = Binary, _MMod, MSt0, _State) ->
    {Value, Rest} = decode_label(Binary),
    {MSt0, {label, Value}, Rest};
decode_compact_term(<<_Value:5, ?COMPACT_LITERAL:3, _Rest/binary>> = Binary, _MMod, MSt0, _State) ->
    {Value, Rest} = decode_value64(Binary),
    {MSt0, {literal, Value}, Rest};
decode_compact_term(Other, MMod, MSt, _State) ->
    decode_dest(Other, MMod, MSt).

% Decode compact term with type information awareness
decode_typed_compact_term(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>, MMod, MSt0, #state{
    type_resolver = TypeResover
}) ->
    {MSt1, Dest, Rest1} = decode_dest(Rest0, MMod, MSt0),
    {TypeIx, Rest2} = decode_literal(Rest1),
    Type = TypeResover(TypeIx),
    {MSt1, {typed, Dest, Type}, Rest2};
decode_typed_compact_term(Other, MMod, MSt, State) ->
    decode_compact_term(Other, MMod, MSt, State).

skip_compact_term(<<_:4, ?COMPACT_INTEGER:4, _Rest/binary>> = Bin) ->
    {_Value, Rest} = decode_value64(Bin),
    Rest;
skip_compact_term(<<_Val:3, ?COMPACT_LARGE_INTEGER_11BITS:5, _NextByte, Rest/binary>>) ->
    Rest;
skip_compact_term(<<7:3, ?COMPACT_LARGE_INTEGER_NBITS:5, Rest0/binary>>) ->
    {DecodedLen, Rest1} = decode_literal(Rest0),
    % 7 actually means 7 + 2, that means an integer that is >= 9 bytes
    IntegerByteLen = DecodedLen + 9,
    <<_Value:IntegerByteLen/binary, Rest2/binary>> = Rest1,
    Rest2;
skip_compact_term(
    <<Size0:3, ?COMPACT_LARGE_INTEGER_NBITS:5, _Value:(8 * (Size0 + 2))/signed, Rest/binary>>
) ->
    Rest;
skip_compact_term(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_:4, ?COMPACT_ATOM:4, _Rest/binary>> = Bin) ->
    {_Value, Rest} = decode_value64(Bin),
    Rest;
skip_compact_term(<<_:4, ?COMPACT_LARGE_ATOM:4, _Rest/binary>> = Bin) ->
    {_Value, Rest} = decode_value64(Bin),
    Rest;
skip_compact_term(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>) ->
    {_Value, Rest1} = decode_literal(Rest0),
    Rest1;
skip_compact_term(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>) ->
    Rest1 = skip_compact_term(Rest0),
    Rest2 = decode_literal(Rest1),
    Rest2;
skip_compact_term(<<_ValueH:5, ?COMPACT_LABEL:3, _Rest/binary>> = Binary) ->
    {_Value, Rest} = decode_label(Binary),
    Rest;
skip_compact_term(<<_ValueH:5, ?COMPACT_LITERAL:3, _Rest/binary>> = Binary) ->
    {_Value, Rest} = decode_value64(Binary),
    Rest;
skip_compact_term(<<_RegIndex:4, ?COMPACT_XREG:4, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_RegIndex:4, ?COMPACT_YREG:4, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_RegIndexH:3, 0:1, ?COMPACT_LARGE_XREG:4, _RegIndexL, Rest/binary>>) ->
    Rest;
skip_compact_term(<<_RegIndexH:3, 0:1, ?COMPACT_LARGE_YREG:4, _RegIndexL, Rest/binary>>) ->
    Rest.

decode_compile_time_literal(<<0:4, ?COMPACT_ATOM:4, Rest/binary>>, _State) ->
    {[], Rest};
decode_compile_time_literal(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>, #state{
    literal_resolver = Resolver
}) ->
    {LiteralIndex, Rest1} = decode_literal(Rest0),
    LiteralTerm = Resolver(LiteralIndex),
    {LiteralTerm, Rest1}.

decode_flags_list(L, _MMod, MSt) when is_list(L) ->
    % compile time decoding
    Value = decode_flags_list0(L, 0),
    {MSt, Value};
decode_flags_list(L, MMod, MSt0) ->
    % run-time decoding
    {MSt1, FlagsValue} = MMod:call_primitive(MSt0, ?PRIM_DECODE_FLAGS_LIST, [ctx, jit_state, L]),
    MSt2 = MMod:if_block(MSt1, {FlagsValue, '<', 0}, fun(BlockSt) ->
        MMod:call_primitive_last(BlockSt, ?PRIM_HANDLE_ERROR, [
            ctx, jit_state, offset
        ])
    end),
    {MSt2, FlagsValue}.

decode_flags_list0([], Val) ->
    Val;
decode_flags_list0([little | T], Val) ->
    decode_flags_list0(T, Val bor ?BITSTRING_FLAG_LITTLE_ENDIAN);
decode_flags_list0([signed | T], Val) ->
    decode_flags_list0(T, Val bor ?BITSTRING_FLAG_SIGNED);
decode_flags_list0([native | T], Val) ->
    decode_flags_list0(T, Val bor ?BITSTRING_FLAG_NATIVE_ENDIAN).

decode_compact_term_atom(AtomIndex, MMod, MSt0, Rest, #state{atom_resolver = Resolver}) ->
    Atom = Resolver(AtomIndex),
    case maps:find(Atom, ?DEFAULT_ATOMS) of
        error ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_MODULE_GET_ATOM_TERM_BY_ID, [jit_state, AtomIndex]
            ),
            ?TRACE("(get_atom_term_by_id(~p) => ~p)", [AtomIndex, Reg]),
            {MSt1, Reg, Rest};
        {ok, DefaultAtomIndex} ->
            {MSt0, DefaultAtomIndex, Rest}
    end.

decode_compact_term_module_literal(LiteralIndex, MMod, MSt0, Rest) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_MODULE_LOAD_LITERAL, [ctx, jit_state, LiteralIndex]
    ),
    ?TRACE("(module_load_literal(~p) => ~p)", [LiteralIndex, Reg]),
    {MSt1, Reg, Rest}.

decode_compact_term_integer(Value, _MMod, MSt, Rest) when
    Value >= (?INT32_MIN bsr 4) andalso Value =< (?INT32_MAX bsr 4)
->
    {MSt, term_from_int(Value), Rest};
decode_compact_term_integer(Value, MMod, MSt0, Rest) when
    Value >= (?INT64_MIN bsr 4) andalso Value =< (?INT64_MAX bsr 4)
->
    case MMod:word_size() of
        4 ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [ctx, {avm_int64_t, Value}]
            ),
            ?TRACE("(alloc_boxed_integer_fragment(~p) => ~p)", [Value, Reg]),
            {MSt1, Reg, Rest};
        8 ->
            {MSt0, term_from_int(Value), Rest}
    end;
decode_compact_term_integer(Value, MMod, MSt0, Rest) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [ctx, {avm_int64_t, Value}]
    ),
    ?TRACE("(alloc_boxed_integer_fragment(~p) => ~p)", [Value, Reg]),
    {MSt1, Reg, Rest}.

decode_compact_term_big_integer(Value, MMod, MSt0, Rest) ->
    Sign =
        case Value of
            Pos when Pos >= 0 -> ?TERM_POSITIVE_INTEGER;
            _Neg -> ?TERM_NEGATIVE_INTEGER
        end,
    AbsValue = abs(Value),
    % Len is in intn_digit_t units, not words/term unit
    Len = count_big_int_digits(AbsValue, 0),
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_ALLOC_BIG_INTEGER_FRAGMENT, [ctx, Len, Sign]
    ),
    {MSt2, Reg} = MMod:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    WordSize = MMod:word_size(),
    % Do not write at Index 0, since it contains boxed header, start from 1 instead
    MSt3 = put_digits(AbsValue, 1, MSt2, Reg, WordSize, MMod),
    MSt4 = MMod:or_(MSt3, Reg, ?TERM_PRIMARY_BOXED),
    {MSt4, Reg, Rest}.

% Assuming 32-bit digits, this code has to be kept in sync when changing intn_digit_t size.
count_big_int_digits(0, Acc) ->
    Acc;
count_big_int_digits(N, Acc) ->
    count_big_int_digits(N bsr 32, Acc + 1).

% put_digits puts 32-bit digits (intn_digit_t) inside a boxed big integer.
%
% Big integers are encoded starting from the least significant digit to the most significant digit.
% Each 32-bit digit is a regular native integer internally encoded with native endianess,
% but since digits order is from least to most significant, it means that we can cast a pair of
% digits to uint64 only on little endian platforms.
%
% After the most significant there might be an additional 0 (as padding) on 64-bit platforms.
%
% Value must be an absolute value, sign is kept in boxed header.
%
% This code has to be kept in sync when changing intn_digit_t size.
put_digits(0, _Index, Mst0, _Reg, _WordSize, _MMod) ->
    Mst0;
put_digits(Value, Index, MSt0, Reg, 4, MMod) ->
    Digit = Value band 16#FFFFFFFF,
    MSt1 = MMod:move_to_array_element(MSt0, Digit, Reg, Index),
    put_digits(Value bsr 32, Index + 1, MSt1, Reg, 4, MMod);
put_digits(Value, Index, MSt0, Reg, 8, MMod) ->
    % Assuming little endian, see above for more info about encoding
    Word = Value band 16#FFFFFFFFFFFFFFFF,
    MSt1 = MMod:move_to_array_element(MSt0, Word, Reg, Index),
    put_digits(Value bsr 64, Index + 1, MSt1, Reg, 8, MMod).

decode_dest(<<RegIndex:4, ?COMPACT_XREG:4, Rest/binary>>, _MMod, MSt) ->
    {MSt, {x_reg, RegIndex}, Rest};
decode_dest(<<RegIndex:4, ?COMPACT_YREG:4, Rest/binary>>, _MMod, MSt) ->
    {MSt, {y_reg, RegIndex}, Rest};
decode_dest(<<RegIndexH:3, 0:1, ?COMPACT_LARGE_XREG:4, RegIndexL, Rest/binary>>, MMod, MSt0) ->
    RegIndex = (RegIndexH bsl 8) bor RegIndexL,
    if
        RegIndex < ?MAX_REG ->
            {MSt0, {x_reg, RegIndex}, Rest};
        true ->
            {MSt1, Reg} = MMod:call_primitive(
                MSt0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, RegIndex]
            ),
            ?TRACE("(extended_register_ptr(~p) => ~p)", [RegIndex, Reg]),
            {MSt1, {ptr, Reg}, Rest}
    end;
decode_dest(<<RegIndexH:3, 0:1, ?COMPACT_LARGE_YREG:4, RegIndexL, Rest/binary>>, _MMod, MSt) ->
    {MSt, {y_reg, (RegIndexH bsl 8) bor RegIndexL}, Rest}.

decode_fp_register(<<?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>) ->
    {FPRegIndex, Rest1} = decode_literal(Rest0),
    {{fp_reg, FPRegIndex}, Rest1}.

read_any_xreg(RegIndex, _MMod, MSt0) when RegIndex < ?MAX_REG ->
    {MSt0, {x_reg, RegIndex}};
read_any_xreg(RegIndex, MMod, MSt0) ->
    {MSt1, Reg} = MMod:call_primitive(
        MSt0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, RegIndex]
    ),
    ?TRACE("extended_register_ptr(~p) => ~p\n", [RegIndex, Reg]),
    {MSt1, {ptr, Reg}}.

decode_extended_list_header(<<?COMPACT_EXTENDED_LIST, Rest0/binary>>) ->
    decode_literal(Rest0).

decode_allocator_list(MMod, <<?COMPACT_EXTENDED_ALLOCATION_LIST, Rest0/binary>>) ->
    {ListSize, Rest1} = decode_literal(Rest0),
    decode_allocator_list0(MMod, 0, ListSize, Rest1);
decode_allocator_list(_MMod, Bin) ->
    decode_literal(Bin).

decode_allocator_list0(_MMod, Need, 0, Rest) ->
    {Need, Rest};
decode_allocator_list0(MMod, AccNeed, Remaining, Rest0) ->
    {AllocatorTag, Rest1} = decode_literal(Rest0),
    {AllocatorSize, Rest2} = decode_literal(Rest1),
    NeedIncrement =
        case AllocatorTag of
            ?COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FLOATS ->
                case MMod:word_size() of
                    4 ->
                        AllocatorSize * ?FLOAT_SIZE_32;
                    8 ->
                        AllocatorSize * ?FLOAT_SIZE_64
                end;
            ?COMPACT_EXTENDED_ALLOCATOR_LIST_TAG_FUNS ->
                AllocatorSize * ?BOXED_FUN_SIZE;
            _ ->
                AllocatorSize
        end,
    decode_allocator_list0(MMod, AccNeed + NeedIncrement, Remaining - 1, Rest2).

term_from_int(Int) when is_integer(Int) ->
    (Int bsl 4) bor ?TERM_INTEGER_TAG.

term_from_int(Reg, MMod, MSt0) when is_atom(Reg) ->
    MSt1 = MMod:shift_left(MSt0, Reg, 4),
    MSt2 = MMod:or_(MSt1, Reg, ?TERM_INTEGER_TAG),
    {MSt2, Reg}.

term_get_tuple_arity(Tuple, MMod, MSt0) ->
    {MSt1, Reg} =
        case Tuple of
            {free, TupleReg} -> MMod:move_to_native_register(MSt0, TupleReg);
            _ -> MMod:copy_to_native_register(MSt0, Tuple)
        end,
    {MSt2, Reg} = MMod:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, Reg, 0, Reg),
    {MSt4, ArityReg} = MMod:shift_right(MSt3, {free, Reg}, 6),
    {MSt4, ArityReg}.

term_get_map_size(Map, MMod, MSt0) ->
    {MSt1, MapKeys} = term_get_map_keys(Map, MMod, MSt0),
    term_get_tuple_arity({free, MapKeys}, MMod, MSt1).

term_get_map_keys(Map, MMod, MSt0) ->
    {MSt1, Reg} =
        case Map of
            {free, MapReg} -> MMod:move_to_native_register(MSt0, MapReg);
            _ -> MMod:copy_to_native_register(MSt0, Map)
        end,
    {MSt2, Reg} = MMod:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, Reg, 1, Reg),
    {MSt3, Reg}.

handle_error_if(Cond, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

term_binary_heap_size(Size, MMod) when is_integer(Size) ->
    case MMod:word_size() of
        4 when Size < ?REFC_BINARY_MIN_32 ->
            ((Size + 3) bsr 2) + 1 + ?BINARY_HEADER_SIZE;
        8 when Size < ?REFC_BINARY_MIN_64 ->
            ((Size + 7) bsr 3) + 1 + ?BINARY_HEADER_SIZE;
        _ ->
            ?TERM_BOXED_REFC_BINARY_SIZE
    end.

term_binary_heap_size({free, Immediate}, MMod, MSt0) when is_integer(Immediate) ->
    {MSt0, term_binary_heap_size(Immediate, MMod)};
term_binary_heap_size({free, Reg}, MMod, MSt0) ->
    MSt1 =
        case MMod:word_size() of
            4 ->
                MMod:if_else_block(
                    MSt0,
                    {Reg, '<', ?REFC_BINARY_MIN_32},
                    fun(BSt0) ->
                        BSt1 = MMod:add(BSt0, Reg, 3),
                        {BSt2, Reg} = MMod:shift_right(BSt1, {free, Reg}, 2),
                        MMod:add(BSt2, Reg, 1 + ?BINARY_HEADER_SIZE)
                    end,
                    fun(BSt0) ->
                        % pretty sure  + ?BINARY_HEADER_SIZE is too much, same issue in opcodeswitch
                        MMod:move_to_native_register(
                            BSt0, ?TERM_BOXED_REFC_BINARY_SIZE + ?BINARY_HEADER_SIZE, Reg
                        )
                    end
                );
            8 ->
                MMod:if_else_block(
                    MSt0,
                    {Reg, '<', ?REFC_BINARY_MIN_64},
                    fun(BSt0) ->
                        BSt1 = MMod:add(BSt0, Reg, 7),
                        {BSt2, Reg} = MMod:shift_right(BSt1, {free, Reg}, 3),
                        MMod:add(BSt2, Reg, 1 + ?BINARY_HEADER_SIZE)
                    end,
                    fun(BSt0) ->
                        MMod:move_to_native_register(
                            BSt0, ?TERM_BOXED_REFC_BINARY_SIZE + ?BINARY_HEADER_SIZE, Reg
                        )
                    end
                )
        end,
    {MSt1, Reg}.

term_binary_size({free, BinReg}, MMod, MSt0) ->
    {MSt1, BinReg} = MMod:and_(MSt0, {free, BinReg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt2 = MMod:move_array_element(MSt1, BinReg, 1, BinReg),
    {MSt2, BinReg};
term_binary_size(Src, MMod, MSt0) ->
    {MSt1, SrcReg} = MMod:move_to_native_register(MSt0, Src),
    {MSt2, SrcReg} = MMod:and_(MSt1, {free, SrcReg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt3 = MMod:move_array_element(MSt2, SrcReg, 1, SrcReg),
    {MSt3, SrcReg}.

term_set_map_assoc(MapPtrReg, {free, PosReg}, {free, Key}, {free, Value}, MMod, MSt0) ->
    {MSt1, MapKeysReg} = MMod:get_array_element(MSt0, MapPtrReg, 1),
    MSt2 = term_put_tuple_element({free, MapKeysReg}, PosReg, {free, Key}, MMod, MSt1),
    MSt3 = MMod:move_to_array_element(MSt2, Value, MapPtrReg, PosReg, 2),
    MMod:free_native_registers(MSt3, [PosReg, Value]).

term_put_tuple_element({free, TupleReg}, PosReg, {free, Value}, MMod, MSt0) ->
    {MSt1, TupleReg} = MMod:and_(MSt0, {free, TupleReg}, ?TERM_PRIMARY_CLEAR_MASK),
    MSt2 = MMod:move_to_array_element(MSt1, Value, TupleReg, PosReg, 1),
    MMod:free_native_registers(MSt2, [TupleReg, Value]).

%% @doc Get the stream module
%% @return The stream module for jit on this platform
-spec stream_module() -> module().
stream_module() ->
    erlang:nif_error(undefined).

%% @doc Return a new stream for this platform
%% @param MaxSize estimation of the maximum size of the stream
%% @return A tuple with the stream module and the stream resource for this platform
-spec stream(MaxSize :: pos_integer()) -> {module(), stream()}.
stream(MaxSize) ->
    StreamModule = ?MODULE:stream_module(),
    {StreamModule, StreamModule:new(MaxSize)}.

%% @doc Get the backend module
%% @return The backend module for jit on this platform
-spec backend_module() -> module().
backend_module() ->
    erlang:nif_error(undefined).

%% @doc Get the JIT variant suitable for runtime compilation
%% @return The JIT variant for this platform and float precision
-spec variant() -> non_neg_integer().
variant() ->
    erlang:nif_error(undefined).

%% @doc Instantiate backend for this platform
%% @return A tuple with the backend module and the backend state for this platform
backend(StreamModule, Stream) ->
    BackendModule = ?MODULE:backend_module(),
    Variant = ?MODULE:variant(),
    BackendState = BackendModule:new(Variant, StreamModule, Stream),
    {BackendModule, BackendState}.
