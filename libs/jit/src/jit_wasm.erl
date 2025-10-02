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

-module(jit_wasm).

-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    flush/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/1,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_equal_to_ctx/2,
    jump_to_label/2,
    jump_to_continuation/2,
    jump_to_offset/2,
    if_block/3,
    if_else_block/4,
    shift_right/3,
    shift_left/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_native_register/3,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    move_to_array_element/5,
    set_bs/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    get_module_index/1,
    and_/3,
    or_/3,
    add/3,
    sub/3,
    mul/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3
]).

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-define(ASSERT(Expr), true = Expr).

%% WASM32 uses 32-bit pointers
%% Local allocation strategy:
%%   Local 0: ctx (i32 pointer) - parameter
%%   Local 1: jit_state (i32 pointer) - parameter
%%   Local 2: native_interface (i32 pointer) - parameter
%%   Local 3-8: Scratch locals (6 total, matching ARMv6-M)
%%
%% Function signature: (param i32 i32 i32) (result i32)

-type wasm_local() :: {local, 0..8}.

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    available_locals :: [wasm_local()],
    used_locals :: [wasm_local()],
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    control_stack :: [wasm_label()]
}).

-type state() :: #state{}.
-type wasm_label() :: {block | loop | if_, non_neg_integer()}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, wasm_local()}.
-type value() :: immediate() | vm_register() | wasm_local() | {ptr, wasm_local()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_wasm_local() ::
    {free, wasm_local()} | wasm_local().

-type condition() ::
    {wasm_local(), '<', integer()}
    | {maybe_free_wasm_local(), '<', wasm_local()}
    | {maybe_free_wasm_local(), '==', integer()}
    | {maybe_free_wasm_local(), '!=', wasm_local() | integer()}
    | {'(int)', maybe_free_wasm_local(), '==', integer()}
    | {'(int)', maybe_free_wasm_local(), '!=', wasm_local() | integer()}
    | {'(bool)', maybe_free_wasm_local(), '==', false}
    | {'(bool)', maybe_free_wasm_local(), '!=', false}
    | {maybe_free_wasm_local(), '&', non_neg_integer(), '!=', integer()}
    | {{free, wasm_local()}, '==', {free, wasm_local()}}.

% Memory layout (same as other backends)
-define(CTX_LOCAL, {local, 0}).
-define(JITSTATE_LOCAL, {local, 1}).
-define(NATIVE_INTERFACE_LOCAL, {local, 2}).

% Context structure offsets (match ARMv6-M)
-define(Y_REGS_OFFSET, 16#14).
-define(X_REG_OFFSET(N), 16#18 + (N * 4)).
-define(CP_OFFSET, 16#5C).
-define(FP_REGS_OFFSET, 16#60).
-define(BS_OFFSET, 16#64).
-define(BS_OFFSET_OFFSET, 16#68).

% JITState structure offsets
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).

-define(JUMP_TABLE_ENTRY_SIZE, 12).

% Available scratch locals (6 total, matching ARMv6-M)
-define(AVAILABLE_LOCALS, [
    {local, 3},
    {local, 4},
    {local, 5},
    {local, 6},
    {local, 7},
    {local, 8},
    {local, 9},
    {local, 10},
    {local, 11},
    {local, 12},
    {local, 13},
    {local, 14},
    {local, 15},
    {local, 16},
    {local, 17},
    {local, 18},
    {local, 19},
    {local, 20}
]).

%%-----------------------------------------------------------------------------
%% @doc Return the word size in bytes, i.e. the sizeof(term) i.e.
%% sizeof(uintptr_t)
%%
%% For WASM32, this is 4 bytes (32-bit pointers)
%%
%% @end
%% @return Word size in bytes
%%-----------------------------------------------------------------------------
-spec word_size() -> 4 | 8.
word_size() -> 4.

%%-----------------------------------------------------------------------------
%% @doc Create a new backend state for provided variant, module and stream.
%% @end
%% @param Variant JIT variant to use (currently ?JIT_VARIANT_PIC)
%% @param StreamModule module to stream instructions
%% @param Stream stream state
%% @return New backend state
%%-----------------------------------------------------------------------------
-spec new(any(), module(), stream()) -> state().
new(Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        offset = StreamModule:offset(Stream),
        available_locals = ?AVAILABLE_LOCALS,
        used_locals = [],
        labels = [],
        variant = Variant,
        control_stack = []
    }.

%%-----------------------------------------------------------------------------
%% @doc Access the stream object.
%% @end
%% @param State current backend state
%% @return The stream object
%%-----------------------------------------------------------------------------
-spec stream(state()) -> stream().
stream(#state{stream = Stream}) ->
    Stream.

%%-----------------------------------------------------------------------------
%% @doc Get the current offset in the stream
%% @end
%% @param State current backend state
%% @return The current offset
%%-----------------------------------------------------------------------------
-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @doc Flush the stream.
%% @end
%% @param State current backend state
%% @return The new state
%%-----------------------------------------------------------------------------
-spec flush(state()) -> stream().
flush(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a debugger breakpoint instruction.
%% For WASM, we use the unreachable instruction.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec debugger(state()) -> state().
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    % WASM unreachable instruction (opcode 0x00)
    Stream1 = StreamModule:append(Stream0, <<16#00>>),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native locals.
%% @end
%% @param State current backend state
%% @return The list of used locals
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [wasm_local()].
used_regs(#state{used_locals = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch locals.
%% @end
%% @param State current backend state
%% @return The list of available locals
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [wasm_local()].
available_regs(#state{available_locals = Available}) -> Available.

%%-----------------------------------------------------------------------------
%% @doc Free native locals. The passed list can contain locals, pointers to
%% locals or other values that are ignored.
%% @end
%% @param State current backend state
%% @param Locals list of locals or other values
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Local | Rest]) ->
    State1 = free_native_register(State, Local),
    free_native_registers(State1, Rest).

%% Private helper to free a single register
-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_locals = Available, used_locals = Used} = State, {local, _} = Local
) ->
    State#state{
        available_locals = [Local | Available],
        used_locals = lists:delete(Local, Used)
    };
free_native_register(State, {ptr, Local}) ->
    free_native_register(State, Local);
free_native_register(State, _Other) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Assert that all native registers are free. This is used for debugging.
%% @end
%% @param State current backend state
%% @return true if all registers are free
%%-----------------------------------------------------------------------------
-spec assert_all_native_free(state()) -> true.
assert_all_native_free(#state{used_locals = []}) ->
    true;
assert_all_native_free(#state{used_locals = Used}) ->
    error({used_locals_not_freed, Used}).

%%-----------------------------------------------------------------------------
%% Stubs for remaining functions - to be implemented in subsequent commits
%%-----------------------------------------------------------------------------

-spec jump_table(state(), non_neg_integer()) -> state().
jump_table(State, LabelsCount) ->
    jump_table0(State, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    N,
    LabelsCount
) ->
    % In WASM, jump table entries are function indices in the function table
    % Each entry is a placeholder that will be patched during update_branches
    % We store a placeholder function index (0 for now)
    % The actual function index will be resolved when labels are known
    PlaceholderIndex = 0,
    Code = jit_wasm_asm:i32_const(PlaceholderIndex),
    Stream1 = StreamModule:append(Stream0, Code),

    % Note: We may need to add relocation info here for later patching
    % For now, just generate the placeholder

    jump_table0(State#state{stream = Stream1}, N + 1, LabelsCount).

-spec update_branches(state()) -> state().
update_branches(State) ->
    % TODO: implement branch resolution
    State.

-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), wasm_local()}.
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State,
    PrimIndex,
    _Args
) ->
    % Call primitive using call_indirect
    % 1. Push standard arguments (ctx, jit_state, native_interface)
    % 2. Load function pointer from native_interface->functions[PrimIndex]
    % 3. Call indirectly through function table
    % 4. Store result in a local and return it
    Code = <<
        % Arguments for the call: ctx, jit_state, native_interface

        % ctx (local 0)
        (jit_wasm_asm:local_get(0))/binary,
        % jit_state (local 1)
        (jit_wasm_asm:local_get(1))/binary,
        % native_interface (local 2)
        (jit_wasm_asm:local_get(2))/binary,
        % Load function pointer from native_interface->functions[PrimIndex]

        % native_interface base address
        (jit_wasm_asm:local_get(2))/binary,
        % Load functions[PrimIndex]
        (jit_wasm_asm:i32_load(2, PrimIndex * 4))/binary,
        % Call indirectly (type 0 = (i32,i32,i32)->i32)
        (jit_wasm_asm:call_indirect(0))/binary,
        % Store result in local
        (jit_wasm_asm:local_set(element(2, Local)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, used_locals = [Local | Used], available_locals = AvailT}, Local}.

-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_last(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    PrimIndex,
    _Args
) ->
    % Tail call primitive using call_indirect followed by return
    % WASM doesn't have native tail calls yet, so we call and return
    Code = <<
        % Arguments for the call: ctx, jit_state, native_interface

        % ctx (local 0)
        (jit_wasm_asm:local_get(0))/binary,
        % jit_state (local 1)
        (jit_wasm_asm:local_get(1))/binary,
        % native_interface (local 2)
        (jit_wasm_asm:local_get(2))/binary,
        % Load function pointer from native_interface->functions[PrimIndex]

        % native_interface base address
        (jit_wasm_asm:local_get(2))/binary,
        % Load functions[PrimIndex]
        (jit_wasm_asm:i32_load(2, PrimIndex * 4))/binary,
        % Call indirectly (type 0 = (i32,i32,i32)->i32)
        (jit_wasm_asm:call_indirect(0))/binary,
        % Return the result immediately (tail call)
        (jit_wasm_asm:return_())/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec call_primitive_with_cp(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_with_cp(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    _Index,
    _Args
) ->
    % Call primitive with continuation point - placeholder
    Code = jit_wasm_asm:unreachable(),
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec return_if_not_equal_to_ctx(state(), wasm_local()) -> state().
return_if_not_equal_to_ctx(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, LocalIdx}
) ->
    % Return if local != ctx - placeholder
    % Compare local with ctx (local 0), if not equal, return
    Code = <<
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        % ctx
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_ne())/binary,
        (jit_wasm_asm:if_(empty))/binary,
        % return the local value
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:return_())/binary,
        (jit_wasm_asm:end_())/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec jump_to_label(state(), integer() | reference()) -> state().
jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, labels = Labels, branches = Branches} =
        State,
    Label
) ->
    LabelLookupResult = lists:keyfind(Label, 1, Labels),
    CurrentOffset = StreamModule:offset(Stream0),
    case LabelLookupResult of
        {Label, TargetOffset} when TargetOffset =< CurrentOffset ->
            % Backward branch: implement using loop + br
            % For now, use unreachable as placeholder
            Code = jit_wasm_asm:unreachable(),
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1};
        _ ->
            % Forward branch: record it for later resolution
            % Emit br instruction with placeholder depth 0
            Code = jit_wasm_asm:br(0),
            Stream1 = StreamModule:append(Stream0, Code),
            BranchOffset = CurrentOffset,
            NewBranches = [{Label, BranchOffset, 1} | Branches],
            State#state{stream = Stream1, branches = NewBranches}
    end.

-spec jump_to_continuation(state(), maybe_free_wasm_local()) -> state().
% Handle {free, Local} - use it and free it (we're jumping away)
jump_to_continuation(#state{stream_module = StreamModule, stream = Stream0} = State0, {free, Local}) ->
    % Jump to continuation stored in jit_state->continuation
    % Load continuation and call indirectly (same pattern as primitives)
    Code = <<
        % Standard arguments
        (jit_wasm_asm:local_get(0))/binary,  % ctx
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:local_get(2))/binary,  % native_interface
        % Load continuation from jit_state->continuation (offset 4)
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:i32_load(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
        % Call indirectly (type 0 = (i32,i32,i32)->i32)
        (jit_wasm_asm:call_indirect(0))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    % Free the register after use
    free_native_register(State1, Local);
jump_to_continuation(
    #state{stream_module = StreamModule, stream = Stream0} = State, {local, _Idx}
) ->
    % Jump to continuation stored in jit_state->continuation
    % Load continuation and call indirectly (same pattern as primitives)
    Code = <<
        % Standard arguments

        % ctx
        (jit_wasm_asm:local_get(0))/binary,
        % jit_state
        (jit_wasm_asm:local_get(1))/binary,
        % native_interface
        (jit_wasm_asm:local_get(2))/binary,
        % Load continuation from jit_state->continuation (offset 4)

        % jit_state
        (jit_wasm_asm:local_get(1))/binary,
        (jit_wasm_asm:i32_load(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
        % Call indirectly (type 0 = (i32,i32,i32)->i32)
        (jit_wasm_asm:call_indirect(0))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec jump_to_offset(state(), non_neg_integer()) -> state().
jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    % Jump to function at a byte offset in the label table
    % This combines loading the function pointer and calling it
    Code = <<
        % Standard arguments

        % ctx
        (jit_wasm_asm:local_get(0))/binary,
        % jit_state
        (jit_wasm_asm:local_get(1))/binary,
        % native_interface
        (jit_wasm_asm:local_get(2))/binary,
        % Load function pointer from native_interface (label table) + offset

        % native_interface base
        (jit_wasm_asm:local_get(2))/binary,
        % Byte offset
        (jit_wasm_asm:i32_const(TargetOffset))/binary,
        % Calculate address
        (jit_wasm_asm:i32_add())/binary,
        % Load function pointer
        (jit_wasm_asm:i32_load(2, 0))/binary,
        % Call indirectly
        (jit_wasm_asm:call_indirect(0))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit condition code for if/if_else blocks
%% Returns {State, ConditionCode} where ConditionCode evaluates to i32 on stack
%% @end
%%-----------------------------------------------------------------------------
-spec emit_condition(state(), condition()) -> {state(), binary()}.
emit_condition(State, {Local, '==', Value}) when is_integer(Value) ->
    {LocalIdx, Code1} = get_local_idx(Local),
    Code = <<
        Code1/binary,
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:i32_const(Value))/binary,
        (jit_wasm_asm:i32_eq())/binary
    >>,
    {State, Code};
emit_condition(State, {Local, '!=', Value}) when is_integer(Value) ->
    {LocalIdx, Code1} = get_local_idx(Local),
    Code = <<
        Code1/binary,
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:i32_const(Value))/binary,
        (jit_wasm_asm:i32_ne())/binary
    >>,
    {State, Code};
% Handle typed comparisons like {'(bool)', Local, '==', Value}
emit_condition(State, {'(bool)', Local, Op, Value}) ->
    % Strip the type annotation and handle the comparison
    emit_condition(State, {Local, Op, Value});
% Handle comparison with false (0 in WASM) - must come before general == clause
emit_condition(State, {Local, '==', false}) ->
    {LocalIdx, Code1} = get_local_idx(Local),
    Code = <<
        Code1/binary,
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:i32_const(0))/binary,  % false = 0
        (jit_wasm_asm:i32_eq())/binary
    >>,
    {State, Code};
emit_condition(State, {Local1, '==', Local2}) ->
    {LocalIdx1, Code1} = get_local_idx(Local1),
    {LocalIdx2, Code2} = get_local_idx(Local2),
    Code = <<
        Code1/binary,
        (jit_wasm_asm:local_get(LocalIdx1))/binary,
        Code2/binary,
        (jit_wasm_asm:local_get(LocalIdx2))/binary,
        (jit_wasm_asm:i32_eq())/binary
    >>,
    {State, Code};
emit_condition(State, {Local, '<', Value}) when is_integer(Value) ->
    {LocalIdx, Code1} = get_local_idx(Local),
    Code = <<
        Code1/binary,
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:i32_const(Value))/binary,
        (jit_wasm_asm:i32_lt_s())/binary
    >>,
    {State, Code};
emit_condition(State, Condition) ->
    % For other conditions, use unreachable as placeholder
    io:format("Warning: Unimplemented condition: ~p~n", [Condition]),
    {State, <<(jit_wasm_asm:i32_const(1))/binary>>}.

%%-----------------------------------------------------------------------------
%% @doc Helper to extract local index from maybe_free_wasm_local
%% @end
%%-----------------------------------------------------------------------------
-spec get_local_idx(maybe_free_wasm_local()) -> {non_neg_integer(), binary()}.
get_local_idx({free, {local, Idx}}) -> {Idx, <<>>};
get_local_idx({local, Idx}) -> {Idx, <<>>}.

-spec if_block(state(), condition(), fun((state()) -> state())) -> state().
if_block(
    #state{stream_module = StreamModule, stream = Stream0, control_stack = ControlStack} = State0,
    Condition,
    ThenFun
) ->
    % Emit condition code
    {State1, CondCode} = emit_condition(State0, Condition),
    % Emit if instruction
    IfCode = jit_wasm_asm:if_(empty),
    Stream1 = StreamModule:append(Stream0, <<CondCode/binary, IfCode/binary>>),
    Offset1 = StreamModule:offset(Stream1),
    State2 = State1#state{
        stream = Stream1, offset = Offset1, control_stack = [{if_, Offset1} | ControlStack]
    },
    % Execute then block
    State3 = ThenFun(State2),
    % Emit end instruction
    EndCode = jit_wasm_asm:end_(),
    Stream3 = StreamModule:append(State3#state.stream, EndCode),
    Offset3 = StreamModule:offset(Stream3),
    [{if_, _} | RestStack] = State3#state.control_stack,
    State3#state{stream = Stream3, offset = Offset3, control_stack = RestStack}.

-spec if_else_block(
    state(), condition(), fun((state()) -> state()), fun((state()) -> state())
) -> state().
if_else_block(
    #state{stream_module = StreamModule, stream = Stream0, control_stack = ControlStack} = State0,
    Condition,
    ThenFun,
    ElseFun
) ->
    % Emit condition code
    {State1, CondCode} = emit_condition(State0, Condition),
    % Emit if instruction
    IfCode = jit_wasm_asm:if_(empty),
    Stream1 = StreamModule:append(Stream0, <<CondCode/binary, IfCode/binary>>),
    Offset1 = StreamModule:offset(Stream1),
    State2 = State1#state{
        stream = Stream1, offset = Offset1, control_stack = [{if_, Offset1} | ControlStack]
    },
    % Execute then block
    State3 = ThenFun(State2),
    % Emit else instruction
    ElseCode = jit_wasm_asm:else_(),
    Stream3 = StreamModule:append(State3#state.stream, ElseCode),
    Offset3 = StreamModule:offset(Stream3),
    State4 = State3#state{stream = Stream3, offset = Offset3},
    % Execute else block
    State5 = ElseFun(State4),
    % Emit end instruction
    EndCode = jit_wasm_asm:end_(),
    Stream5 = StreamModule:append(State5#state.stream, EndCode),
    Offset5 = StreamModule:offset(Stream5),
    [{if_, _} | RestStack] = State5#state.control_stack,
    State5#state{stream = Stream5, offset = Offset5, control_stack = RestStack}.

%%-----------------------------------------------------------------------------
%% @doc Shift right: local = local >> shift (unsigned)
%% @end
%%-----------------------------------------------------------------------------
-spec shift_right(state(), maybe_free_wasm_local(), non_neg_integer()) -> {state(), wasm_local()}.
% Handle {free, Local} - use it and then free it
shift_right(#state{stream_module = StreamModule, stream = Stream0} = State0, {free, {local, Idx} = Local}, Shift) ->
    Code = <<
        (jit_wasm_asm:local_get(Idx))/binary,
        (jit_wasm_asm:i32_const(Shift))/binary,
        (jit_wasm_asm:i32_shr_u())/binary,
        (jit_wasm_asm:local_set(Idx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    % Free the register after use
    State2 = free_native_register(State1, Local),
    {State2, Local};
shift_right(#state{stream_module = StreamModule, stream = Stream0} = State, {local, Idx} = Local, Shift) ->
    Code = <<
        (jit_wasm_asm:local_get(Idx))/binary,
        (jit_wasm_asm:i32_const(Shift))/binary,
        (jit_wasm_asm:i32_shr_u())/binary,
        (jit_wasm_asm:local_set(Idx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1}, Local}.

%%-----------------------------------------------------------------------------
%% @doc Shift left: local = local << shift
%% @end
%%-----------------------------------------------------------------------------
-spec shift_left(state(), wasm_local(), non_neg_integer()) -> {state(), wasm_local()}.
shift_left(#state{stream_module = StreamModule, stream = Stream0} = State, {local, Idx} = Local, Shift) ->
    Code = <<
        (jit_wasm_asm:local_get(Idx))/binary,
        (jit_wasm_asm:i32_const(Shift))/binary,
        (jit_wasm_asm:i32_shl())/binary,
        (jit_wasm_asm:local_set(Idx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1}, Local}.

%%-----------------------------------------------------------------------------
%% @doc Move a value from a native local to a VM register (x_reg or y_reg).
%% @end
%%-----------------------------------------------------------------------------
-spec move_to_vm_register(state(), value(), vm_register()) -> state().
% Move immediate value to X register
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, Value, {x_reg, X}
) when is_integer(Value) ->
    Offset = ?X_REG_OFFSET(X),
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_const(Value))/binary,
        (jit_wasm_asm:i32_store(2, Offset))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
% Move immediate value to Y register
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, Value, {y_reg, Y}
) when is_integer(Value) ->
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?Y_REGS_OFFSET))/binary,
        (jit_wasm_asm:i32_const(Value))/binary,
        (jit_wasm_asm:i32_store(2, Y * 4))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
% Move from X register to Y register
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, SrcX}, {y_reg, Y}
) ->
    SrcOffset = ?X_REG_OFFSET(SrcX),
    Code = <<
        % Load Y regs array base
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?Y_REGS_OFFSET))/binary,
        % Load value from X register
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, SrcOffset))/binary,
        % Store to Y register
        (jit_wasm_asm:i32_store(2, Y * 4))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
% Move from Y register to X register
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, SrcY}, {x_reg, X}
) ->
    DestOffset = ?X_REG_OFFSET(X),
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?Y_REGS_OFFSET))/binary,
        (jit_wasm_asm:i32_load(2, SrcY * 4))/binary,
        (jit_wasm_asm:i32_store(2, DestOffset))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
% Move from local to X register
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {local, SrcIdx}, {x_reg, X}
) ->
    Offset = ?X_REG_OFFSET(X),
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_store(2, Offset))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
% Move from local to Y register
move_to_vm_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {local, SrcIdx}, {y_reg, Y}
) ->
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?Y_REGS_OFFSET))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_store(2, Y * 4))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Move a value to a native local, allocating a new local if needed.
%% Returns the state and the local containing the value.
%% @end
%%-----------------------------------------------------------------------------
-spec move_to_native_register(state(), value()) -> {state(), wasm_local()}.
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State,
    cp
) ->
    % Load continuation pointer from context
    % ctx->cp is at offset ?CP_OFFSET
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?CP_OFFSET))/binary,
        (jit_wasm_asm:local_set(element(2, Local)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, used_locals = [Local | Used], available_locals = AvailT}, Local};
move_to_native_register(State, {local, _} = Local) ->
    % Already a local, return as-is
    {State, Local};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, {local, Idx}}
) ->
    % Dereference pointer (load from memory address in local)
    Code = <<
        (jit_wasm_asm:local_get(Idx))/binary,
        (jit_wasm_asm:i32_load(2, 0))/binary,
        (jit_wasm_asm:local_set(Idx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1}, {local, Idx}};
move_to_native_register(
    #state{
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State0,
    Imm
) when is_integer(Imm) ->
    State1 = State0#state{used_locals = [Local | Used], available_locals = AvailT},
    {move_to_native_register(State1, Imm, Local), Local};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State,
    {x_reg, X}
) ->
    % Load X register from context
    Offset = ?X_REG_OFFSET(X),
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, Offset))/binary,
        (jit_wasm_asm:local_set(element(2, Local)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, used_locals = [Local | Used], available_locals = AvailT}, Local};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State,
    {y_reg, Y}
) ->
    % Load Y register from context
    % Y regs are stored as an array at offset ?Y_REGS_OFFSET
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?Y_REGS_OFFSET))/binary,
        (jit_wasm_asm:i32_load(2, Y * 4))/binary,
        (jit_wasm_asm:local_set(element(2, Local)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, used_locals = [Local | Used], available_locals = AvailT}, Local}.

%%-----------------------------------------------------------------------------
%% @doc Move a value to a specific native local.
%% @end
%%-----------------------------------------------------------------------------
-spec move_to_native_register(state(), value(), wasm_local()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {local, SrcIdx}, {local, DstIdx}
) ->
    % Copy from one local to another
    Code = <<
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:local_set(DstIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_native_register(State, Imm, {local, DstIdx}) when is_integer(Imm) ->
    % Load immediate constant
    mov_immediate(State, {local, DstIdx}, Imm);
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {ptr, {local, SrcIdx}},
    {local, DstIdx}
) ->
    % Dereference pointer
    Code = <<
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_load(2, 0))/binary,
        (jit_wasm_asm:local_set(DstIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, {local, DstIdx}
) ->
    % Load X register
    Offset = ?X_REG_OFFSET(X),
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, Offset))/binary,
        (jit_wasm_asm:local_set(DstIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, Y}, {local, DstIdx}
) ->
    % Load Y register
    Code = <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:i32_load(2, ?Y_REGS_OFFSET))/binary,
        (jit_wasm_asm:i32_load(2, Y * 4))/binary,
        (jit_wasm_asm:local_set(DstIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%% Helper to move immediate to local
-spec mov_immediate(state(), wasm_local(), integer()) -> state().
mov_immediate(#state{stream_module = StreamModule, stream = Stream0} = State, {local, DstIdx}, Imm) ->
    Code = <<
        (jit_wasm_asm:i32_const(Imm))/binary,
        (jit_wasm_asm:local_set(DstIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec move_to_cp(state(), wasm_local()) -> state().
move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, LocalIdx}
) ->
    % Move to continuation pointer: store local to ctx->cp (offset 0x5C)
    Code = <<
        % ctx
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:i32_store(2, ?CP_OFFSET))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec move_array_element(state(), wasm_local(), wasm_local(), non_neg_integer()) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    {local, BaseIdx},
    Offset
) ->
    % Load from array and store to local: dest = base[offset]
    Code = <<
        (jit_wasm_asm:local_get(BaseIdx))/binary,
        (jit_wasm_asm:i32_load(2, Offset * 4))/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec move_to_array_element(state(), wasm_local(), wasm_local(), non_neg_integer()) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, SrcIdx},
    {local, BaseIdx},
    Offset
) ->
    % Store to array: base[offset] = src
    Code = <<
        (jit_wasm_asm:local_get(BaseIdx))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_store(2, Offset * 4))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec move_to_array_element(
    state(), wasm_local(), wasm_local(), wasm_local(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, SrcIdx},
    {local, BaseIdx},
    {local, IndexIdx},
    Scale
) ->
    % Store to array with index: base[index * scale] = src
    % Need to compute offset: index * scale
    Code = <<
        (jit_wasm_asm:local_get(BaseIdx))/binary,
        (jit_wasm_asm:local_get(IndexIdx))/binary,
        (jit_wasm_asm:i32_const(Scale))/binary,
        (jit_wasm_asm:i32_mul())/binary,
        (jit_wasm_asm:i32_add())/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_store(2, 0))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec set_bs(state(), wasm_local()) -> state().
set_bs(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, LocalIdx}
) ->
    % Set binary state: store local to ctx->bs (offset 0x64)
    Code = <<
        % ctx
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:local_get(LocalIdx))/binary,
        (jit_wasm_asm:i32_store(2, ?BS_OFFSET))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Copy a value to a new native local. Allocates a new local and copies
%% the value from the source local.
%% @end
%%-----------------------------------------------------------------------------
-spec copy_to_native_register(state(), wasm_local()) -> {state(), wasm_local()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [NewLocal | AvailT],
        used_locals = Used
    } = State,
    {local, SrcIdx}
) ->
    Code = <<
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:local_set(element(2, NewLocal)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_locals = AvailT, used_locals = [NewLocal | Used]},
        NewLocal
    }.

-spec get_array_element(state(), wasm_local(), non_neg_integer()) -> {state(), wasm_local()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [ElemLocal | AvailT],
        used_locals = Used
    } = State,
    {local, BaseIdx},
    Offset
) ->
    % Load from array: base[offset]
    % WASM: local.get base, i32.load align=2 offset=(Offset*4), local.set elem
    Code = <<
        (jit_wasm_asm:local_get(BaseIdx))/binary,
        (jit_wasm_asm:i32_load(2, Offset * 4))/binary,
        (jit_wasm_asm:local_set(element(2, ElemLocal)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_locals = AvailT, used_locals = [ElemLocal | Used]},
        ElemLocal
    }.

-spec increment_sp(state(), integer()) -> state().
increment_sp(State, _Amount) ->
    % Increment stack pointer - for WASM this is likely a no-op or placeholder
    % as WASM manages its own stack. For compatibility, just return state unchanged.
    % In the future, this might adjust a stack pointer in the context.
    State.

-spec set_continuation_to_label(state(), integer() | reference()) -> state().
set_continuation_to_label(
    #state{stream_module = StreamModule, stream = Stream0} = State, Label
) when is_integer(Label) ->
    % Set continuation to a label (function index in WASM)
    % Store label index to jit_state->continuation (offset 4)
    % In WASM, labels are function indices that can be stored as i32
    Code = <<
        % jit_state
        (jit_wasm_asm:local_get(1))/binary,
        % Label as function index
        (jit_wasm_asm:i32_const(Label))/binary,
        (jit_wasm_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
set_continuation_to_label(State, _Label) ->
    % Reference-based labels need runtime resolution
    % For now, placeholder (no-op)
    State.

-spec set_continuation_to_offset(state()) -> {state(), reference()}.
set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches
    } = State
) ->
    % Set continuation to the current code position
    % Create a reference that will be resolved to a label
    % The label will point to the code following this instruction
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),

    % For WASM, we store a placeholder function index that will be resolved later
    % When add_label is called with OffsetRef, it will associate it with a function
    % For now, we store the offset as a temporary placeholder value
    % This will need to be patched when the label is resolved
    Code = <<
        % jit_state
        (jit_wasm_asm:local_get(1))/binary,
        % Temporary: store offset as placeholder (will be patched to function index)
        (jit_wasm_asm:i32_const(Offset))/binary,
        (jit_wasm_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),

    % Record this for later resolution (similar to a branch that needs fixing)
    % The 'continuation' tag indicates this needs special handling
    Reloc = {OffsetRef, Offset, continuation},

    {State#state{stream = Stream1, branches = [Reloc | Branches]}, OffsetRef}.

-spec continuation_entry_point(state()) -> state().
continuation_entry_point(State) ->
    % Entry point for continuation - placeholder
    State.

-spec get_module_index(state()) -> {state(), wasm_local()}.
get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State
) ->
    % Get module index from jit_state
    % Load from jit_state->module (offset 0)
    Code = <<
        % jit_state
        (jit_wasm_asm:local_get(1))/binary,
        (jit_wasm_asm:i32_load(2, ?JITSTATE_MODULE_OFFSET))/binary,
        (jit_wasm_asm:local_set(element(2, Local)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_locals = AvailT, used_locals = [Local | Used]}, Local}.

%%-----------------------------------------------------------------------------
%% @doc Bitwise AND: dest = dest & src
%% @end
%%-----------------------------------------------------------------------------
-spec and_(state(), wasm_local(), wasm_local() | integer()) -> state().
% AND with immediate value
and_(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    Imm
) when is_integer(Imm) ->
    Code = <<
        (jit_wasm_asm:local_get(DestIdx))/binary,
        (jit_wasm_asm:i32_const(Imm))/binary,
        (jit_wasm_asm:i32_and())/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
% AND with another local
and_(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    {local, SrcIdx}
) ->
    Code = <<
        (jit_wasm_asm:local_get(DestIdx))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_and())/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Bitwise OR: dest = dest | src
%% @end
%%-----------------------------------------------------------------------------
-spec or_(state(), wasm_local(), wasm_local()) -> state().
or_(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    {local, SrcIdx}
) ->
    Code = <<
        (jit_wasm_asm:local_get(DestIdx))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_or())/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Addition: dest = dest + src
%% @end
%%-----------------------------------------------------------------------------
-spec add(state(), wasm_local(), wasm_local()) -> state().
add(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    {local, SrcIdx}
) ->
    Code = <<
        (jit_wasm_asm:local_get(DestIdx))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_add())/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Subtraction: dest = dest - src
%% @end
%%-----------------------------------------------------------------------------
-spec sub(state(), wasm_local(), wasm_local()) -> state().
sub(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    {local, SrcIdx}
) ->
    Code = <<
        (jit_wasm_asm:local_get(DestIdx))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_sub())/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Multiplication: dest = dest * src
%% @end
%%-----------------------------------------------------------------------------
-spec mul(state(), wasm_local(), wasm_local()) -> state().
mul(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    {local, DestIdx},
    {local, SrcIdx}
) ->
    Code = <<
        (jit_wasm_asm:local_get(DestIdx))/binary,
        (jit_wasm_asm:local_get(SrcIdx))/binary,
        (jit_wasm_asm:i32_mul())/binary,
        (jit_wasm_asm:local_set(DestIdx))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0} = State
) ->
    % Decrement reductions counter and potentially yield - placeholder
    % Full implementation requires cooperation with scheduler
    Code = jit_wasm_asm:unreachable(),
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec call_or_schedule_next(state(), integer() | reference()) -> state().
call_or_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    _Label
) ->
    % Call or schedule next - placeholder
    Code = jit_wasm_asm:unreachable(),
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec call_only_or_schedule_next(state(), integer() | reference()) -> state().
call_only_or_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    _Label
) ->
    % Tail call or schedule next - placeholder
    Code = jit_wasm_asm:unreachable(),
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec call_func_ptr(state(), wasm_local(), [arg()]) -> {state(), wasm_local()}.
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State,
    _FuncPtr,
    _Args
) ->
    % Indirect function call - placeholder
    % Full implementation requires call_indirect with function table
    % For now, use unreachable and return a result local
    Code = jit_wasm_asm:unreachable(),
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, used_locals = [Local | Used], available_locals = AvailT}, Local}.

-spec return_labels_and_lines(state(), [{integer(), integer()}]) ->
    {state(), wasm_local(), [{integer(), integer()}]}.
return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_locals = [Local | AvailT],
        used_locals = Used
    } = State,
    LabelsAndLines
) ->
    % Return labels and lines for debug info - placeholder
    % For now, just allocate a local and return unchanged labels
    Code = <<
        (jit_wasm_asm:i32_const(0))/binary,
        (jit_wasm_asm:local_set(element(2, Local)))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_locals = AvailT, used_locals = [Local | Used]},
        Local,
        LabelsAndLines
    }.

-spec add_label(state(), integer() | reference()) -> state().
add_label(#state{stream_module = StreamModule, stream = Stream0} = State0, Label) ->
    Offset = StreamModule:offset(Stream0),
    add_label(State0, Label, Offset).

-spec add_label(state(), integer() | reference(), integer()) -> state().
add_label(#state{labels = Labels} = State, Label, Offset) ->
    State#state{labels = [{Label, Offset} | Labels]}.
