%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_wasm32).

%% WASM32 JIT backend for AtomVM.
%%
%% This backend generates WebAssembly bytecode instead of native machine code.
%% Each BEAM label compiles to a separate WASM function with the standard
%% ModuleNativeEntryPoint signature:
%%   Context* (*)(Context* ctx, JITState* jit_state, const ModuleNativeInterface* p)
%%
%% All WASM function parameters and local variables are passed through
%% Emscripten's linear memory. The three parameters (ctx, jit_state, p) are
%% wasm32 pointers (i32 indices into linear memory).
%%
%% Intra-module branches (jump_to_label, cond_jump_to_label) set the
%% jit_state->continuation field and return to the C dispatch loop, which
%% then re-enters the target label's WASM function.
%%
%% The "jump table" is an array of function pointers (i32 table indices)
%% stored as data, not executable code.
%%
%% WASM local variables:
%%   local 0: ctx (i32) - parameter
%%   local 1: jit_state (i32) - parameter
%%   local 2: native_interface (i32) - parameter
%%   local 3+: scratch locals for temporary values

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
    cond_jump_to_label/3,
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
    add_label/3,
    get_regs_tracking/1,
    xor_/3,
    shift_right_arith/3,
    div_reg/3,
    rem_reg/3
]).

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-include("jit_backend_dwarf_impl.hrl").

-define(ASSERT(Expr), true = Expr).

%% WASM32 uses 32-bit pointers in Emscripten linear memory.
%% The three function parameters are:
%%   local 0: ctx pointer (i32)
%%   local 1: jit_state pointer (i32)
%%   local 2: native_interface pointer (i32)
%% Additional locals are used as scratch space.

-define(CTX_LOCAL, local0).
-define(JITSTATE_LOCAL, local1).
-define(NATIVE_INTERFACE_LOCAL, local2).

%% Scratch locals start at index 3
-define(FIRST_SCRATCH_LOCAL, 3).
-define(NUM_SCRATCH_LOCALS, 8).

%% Context struct offsets (32-bit architecture, same as armv6m/riscv32)
-define(CTX_E_OFFSET, 16#14).
-define(CTX_X_OFFSET, 16#18).
-define(CTX_CP_OFFSET, 16#5C).
-define(CTX_FR_OFFSET, 16#60).
-define(CTX_BS_OFFSET, 16#64).
-define(CTX_BS_OFFSET_OFFSET, 16#68).

%% JITState struct offsets
-define(JITSTATE_MODULE_OFFSET, 16#0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).

%% Jump table entry size: sizeof(uint32_t) for a function pointer
-define(JUMP_TABLE_ENTRY_SIZE, 4).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    jump_table_start :: non_neg_integer(),
    %% WASM uses local variables instead of registers.
    %% available_regs/used_regs track scratch local indices (bitmask).
    available_regs :: non_neg_integer(),
    used_regs :: non_neg_integer(),
    %% High-water mark of scratch locals allocated. When the initial pool
    %% of NUM_SCRATCH_LOCALS is exhausted, new locals are allocated at
    %% index FIRST_SCRATCH_LOCAL + max_scratch and max_scratch is incremented.
    max_scratch :: non_neg_integer(),
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    regs :: jit_regs:regs(),
    %% WASM-specific: accumulated function bodies (in reverse order).
    %% Each entry is {Label, InstructionBytes} for a label's WASM code.
    func_bodies :: [{integer(), binary()}],
    %% Current function body being built (accumulated instructions)
    current_body :: binary(),
    %% Current label being built (or undefined)
    current_label :: integer() | reference() | undefined,
    %% Number of labels in the module
    labels_count :: non_neg_integer()
}).

-type state() :: #state{}.
-type wasm_local() :: non_neg_integer().
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, wasm_local()}.
-type value() :: immediate() | vm_register() | wasm_local() | {ptr, wasm_local()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_local() ::
    {free, wasm_local()} | wasm_local().

-type condition() ::
    {wasm_local(), '<', integer()}
    | {maybe_free_local(), '<', wasm_local()}
    | {integer(), '<', maybe_free_local()}
    | {maybe_free_local(), '==', integer()}
    | {maybe_free_local(), '!=', wasm_local() | integer()}
    | {'(int)', maybe_free_local(), '==', integer()}
    | {'(int)', maybe_free_local(), '!=', wasm_local() | integer()}
    | {'(bool)', maybe_free_local(), '==', false}
    | {'(bool)', maybe_free_local(), '!=', false}
    | {maybe_free_local(), '&', non_neg_integer(), '!=', integer()}
    | {{free, wasm_local()}, '==', {free, wasm_local()}}.

%% Bitmask for scratch local availability.
%% We have 8 scratch locals (indices 3-10).
-define(AVAILABLE_REGS_MASK,
    (16#FF)
).

%%=============================================================================
%% Backend API implementation
%%=============================================================================

-spec word_size() -> 4 | 8.
word_size() -> 4.

-spec new(any(), module(), stream()) -> state().
new(Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        jump_table_start = 0,
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        max_scratch = ?NUM_SCRATCH_LOCALS,
        labels = [],
        variant = Variant,
        regs = jit_regs:new(),
        func_bodies = [],
        current_body = <<>>,
        current_label = undefined,
        labels_count = 0
    }.

-spec stream(state()) -> stream().
stream(#state{stream = Stream}) ->
    Stream.

-spec offset(state()) -> non_neg_integer().
offset(#state{current_label = Label, labels = Labels, jump_table_start = JTStart}) ->
    %% For WASM, the "offset" is the label's jump table entry position.
    %% This encodes the label number so jit_return() can recover it:
    %%   label = (offset) / JUMP_TABLE_ENTRY_SIZE
    case Label of
        undefined ->
            JTStart;
        _ when is_integer(Label) -> JTStart + Label * ?JUMP_TABLE_ENTRY_SIZE;
        _ ->
            %% Reference labels - find if already registered
            case lists:keyfind(Label, 1, Labels) of
                {Label, Offset} -> Offset;
                false -> JTStart
            end
    end.

-spec flush(state()) -> state().
flush(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
    State#state{stream = Stream1}.

-spec debugger(state()) -> state().
debugger(State) ->
    %% Emit unreachable as a debug trap in WASM
    emit(State, jit_wasm32_asm:unreachable()).

-spec used_regs(state()) -> [wasm_local()].
used_regs(#state{used_regs = Used}) -> mask_to_locals(Used).

-spec available_regs(state()) -> [wasm_local()].
available_regs(#state{available_regs = Available}) -> mask_to_locals(Available).

-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Val | Rest]) ->
    State1 = free_native_register(State, Val),
    free_native_registers(State1, Rest).

-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_regs = Available0, used_regs = Used0} = State,
    Local
) when is_atom(Local) ->
    LocalIdx = jit_wasm32_asm:local_index(Local),
    case LocalIdx >= ?FIRST_SCRATCH_LOCAL of
        true ->
            Bit = local_bit(LocalIdx),
            State#state{
                available_regs = Available0 bor Bit, used_regs = Used0 band (bnot Bit)
            };
        false ->
            State
    end;
free_native_register(State, {ptr, Local}) ->
    free_native_register(State, Local);
free_native_register(State, _Other) ->
    State.

-spec assert_all_native_free(state()) -> ok.
assert_all_native_free(#state{max_scratch = MS} = State) ->
    0 = State#state.used_regs,
    AllFree = (1 bsl MS) - 1,
    AllFree = State#state.available_regs,
    ok.

%%=============================================================================
%% Jump table - for WASM this is a data array of function pointers
%%=============================================================================

-spec jump_table(state(), pos_integer()) -> state().
jump_table(#state{stream_module = StreamModule, stream = Stream0} = State, LabelsCount) ->
    NumEntries = LabelsCount + 1,
    %% Stream header:
    %%   Bytes 0..3:  num_entries (uint32_t LE)
    %%   Bytes 4..7:  wasm_offset placeholder (uint32_t LE, patched in return_labels_and_lines)
    %%   Bytes 8..8+num_entries*4-1: Reserved jump table area (not used by C side for WASM,
    %%     but kept for compatibility with the jump table patching logic)
    Stream1 = StreamModule:append(Stream0, <<NumEntries:32/little, 0:32/little>>),
    JumpTableStart = StreamModule:offset(Stream1),
    Placeholder = <<0:32/little>>,
    Stream2 = emit_n_times(StreamModule, Stream1, Placeholder, NumEntries),
    State#state{
        stream = Stream2,
        jump_table_start = JumpTableStart,
        labels_count = LabelsCount
    }.

emit_n_times(_StreamModule, Stream, _Binary, 0) ->
    Stream;
emit_n_times(StreamModule, Stream, Binary, N) when N > 0 ->
    Stream1 = StreamModule:append(Stream, Binary),
    emit_n_times(StreamModule, Stream1, Binary, N - 1).

%%=============================================================================
%% Branch patching
%%=============================================================================

-spec update_branches(state()) -> state().
update_branches(#state{branches = []} = State) ->
    State;
update_branches(State) ->
    %% For WASM, branches are resolved at label-add time.
    %% Cross-label jumps set jit_state->continuation and return to C.
    %% Any remaining unresolved branches are reference-labeled continuations
    %% that will be handled by the WASM module structure.
    State#state{branches = []}.

%%=============================================================================
%% Primitive calls
%%
%% In WASM, calling a primitive means:
%% 1. Push arguments onto the WASM operand stack
%% 2. Load the function pointer from the native interface table
%% 3. Use call_indirect to invoke it
%%
%% However, since WASM runs in Emscripten and all C functions are available,
%% we emit WASM code that:
%% - Loads arguments from the Context/JITState via i32.load
%% - Calls the primitive C function through call_indirect
%% - Stores results back
%%
%% For the initial implementation, we delegate all work to the C dispatch loop
%% by setting continuation and returning. This avoids the complexity of
%% emitting call_indirect for every primitive.
%%=============================================================================

-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), wasm_local()}.
call_primitive(State0, Primitive, Args) ->
    %% Allocate a result local
    {State1, ResultLocal} = alloc_local(State0),
    %% Emit code to call the primitive through the native interface table:
    %% 1. Load function pointer from interface[Primitive]
    %% 2. Push arguments
    %% 3. call_indirect
    %% 4. Store result in local
    State2 = emit_call_primitive(State1, Primitive, Args, ResultLocal, false),
    %% After C function call, all register tracking is invalidated (callee may GC)
    Regs1 = jit_regs:invalidate_all(State2#state.regs),
    {State2#state{regs = Regs1}, ResultLocal}.

-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_last(State0, Primitive, Args) ->
    %% Tail call: emit the call and return the result directly
    State1 = emit_call_primitive(State0, Primitive, Args, none, true),
    AllFree = (1 bsl State1#state.max_scratch) - 1,
    State1#state{
        available_regs = AllFree,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State1#state.regs)
    }.

call_primitive_with_cp(State0, Primitive, Args) ->
    %% Set CP before calling the primitive
    State1 = emit_set_cp(State0),
    call_primitive_last(State1, Primitive, Args).

%%=============================================================================
%% Control flow
%%=============================================================================

return_if_not_equal_to_ctx(
    #state{available_regs = Available0, used_regs = Used0} = State0,
    {free, Local}
) ->
    %% if (Local != ctx) return Local;
    Code = <<
        %% Push Local and ctx, compare
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_ne())/binary,
        (jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void()))/binary,
        %% Return the value (not equal to ctx)
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:return())/binary,
        (jit_wasm32_asm:end_())/binary
    >>,
    Bit = local_bit(Local),
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{
        available_regs = Available0 bor Bit,
        used_regs = Used0 band (bnot Bit),
        regs = Regs1
    }.

jump_to_label(State0, Label) ->
    %% In WASM, jumping to a label means:
    %% 1. Look up the label's function table index
    %% 2. Store it in jit_state->continuation
    %% 3. Return ctx (let the C dispatch loop re-enter)
    State1 = emit_set_continuation_for_label(State0, Label),
    %% Return ctx
    State2 = emit(State1, <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:return())/binary
    >>),
    %% After unconditional jump, register tracking is dead until next label
    State2#state{regs = jit_regs:invalidate_all(State2#state.regs)}.

jump_to_offset(State0, TargetOffset) ->
    %% Jump to a specific offset - find the corresponding label
    %% For WASM, offsets encode label positions in the jump table
    case lists:keyfind(TargetOffset, 2, State0#state.labels) of
        {Label, TargetOffset} when is_integer(Label) ->
            jump_to_label(State0, Label);
        _ ->
            %% Offset not found - return ctx (let C dispatch loop handle it)
            State1 = emit(State0, <<
                (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                (jit_wasm32_asm:return())/binary
            >>),
            State1#state{regs = jit_regs:invalidate_all(State1#state.regs)}
    end.

cond_jump_to_label(State, Cond, Label) ->
    if_block(State, Cond, fun(S) -> jump_to_label(S, Label) end).

jump_to_continuation(State0, {free, OffsetLocal}) ->
    %% Jump to the address stored in OffsetLocal (a byte offset in the jump table)
    %% Convert to (label + 1) format expected by the C dispatch loop:
    %%   label = offset / JUMP_TABLE_ENTRY_SIZE
    %%   continuation = label + 1
    Code = <<
        %% Store (OffsetLocal / 4 + 1) into jit_state->continuation
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:local_get(OffsetLocal))/binary,
        (jit_wasm32_asm:i32_const(?JUMP_TABLE_ENTRY_SIZE))/binary,
        (jit_wasm32_asm:i32_div_u())/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
        %% Return ctx
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:return())/binary
    >>,
    State1 = emit(State0, Code),
    AllFree = (1 bsl State1#state.max_scratch) - 1,
    State1#state{available_regs = AllFree, used_regs = 0}.

%%=============================================================================
%% Conditional blocks
%%=============================================================================

-spec if_block(state(), condition() | {'and', [condition()]}, fun((state()) -> state())) -> state().
if_block(State0, {'and', CondList}, BlockFn) ->
    %% For AND conditions, emit all conditions and AND them together
    State1 = emit_and_conditions(State0, CondList),
    %% Now emit: if ... block ... end
    State2 = emit(State1, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State3 = BlockFn(State2),
    State4 = emit(State3, jit_wasm32_asm:end_()),
    MergedRegs = jit_regs:merge(State1#state.regs, State4#state.regs),
    merge_used_regs(State4#state{regs = MergedRegs}, State1#state.used_regs);
if_block(State0, Cond, BlockFn) ->
    State1 = emit_condition(State0, Cond),
    State2 = emit(State1, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State3 = BlockFn(State2),
    State4 = emit(State3, jit_wasm32_asm:end_()),
    MergedRegs = jit_regs:merge(State1#state.regs, State4#state.regs),
    merge_used_regs(State4#state{regs = MergedRegs}, State1#state.used_regs).

-spec if_else_block(state(), condition(), fun((state()) -> state()), fun((state()) -> state())) ->
    state().
if_else_block(State0, Cond, BlockTrueFn, BlockFalseFn) ->
    State1 = emit_condition(State0, Cond),
    State2 = emit(State1, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State3 = BlockTrueFn(State2),
    State4 = emit(State3, jit_wasm32_asm:else_()),
    StateElse = State4#state{
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs
    },
    State5 = BlockFalseFn(StateElse),
    State6 = emit(State5, jit_wasm32_asm:end_()),
    MergedRegs = jit_regs:merge(State3#state.regs, State5#state.regs),
    merge_used_regs(State6#state{regs = MergedRegs}, State3#state.used_regs).

%%=============================================================================
%% Arithmetic and bitwise operations
%%=============================================================================

-spec shift_right(state(), maybe_free_local(), non_neg_integer()) ->
    {state(), wasm_local()}.
shift_right(State0, {free, Local}, Shift) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_u())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
shift_right(State0, Local, Shift) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_u())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

shift_left(State0, Local, Shift) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

shift_right_arith(State0, {free, Local}, Shift) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_s())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
shift_right_arith(State0, Local, Shift) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:i32_const(Shift))/binary,
        (jit_wasm32_asm:i32_shr_s())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

and_(State0, {free, Local}, Mask) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(Mask))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
and_(State0, Local, Mask) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(Mask))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

or_(State0, {free, Local}, ValOrReg) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_or())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
or_(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_or())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

xor_(State0, {free, Local}, ValOrReg) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_xor())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
xor_(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_xor())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

add(State0, {free, Local}, ValOrReg) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
add(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

sub(State0, {free, Local}, ValOrReg) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
sub(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(State0, {free, Local}, ValOrReg) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_mul())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
mul(State0, Local, ValOrReg) when is_atom(Local) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(ValOrReg))/binary,
        (jit_wasm32_asm:i32_mul())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

div_reg(State0, {free, Local}, Divisor) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_div_s())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
div_reg(State0, Local, Divisor) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_div_s())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

rem_reg(State0, {free, Local}, Divisor) ->
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_rem_s())/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    {State1#state{regs = Regs1}, Local};
rem_reg(State0, Local, Divisor) when is_atom(Local) ->
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (jit_wasm32_asm:local_get(Divisor))/binary,
        (jit_wasm32_asm:i32_rem_s())/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

%%=============================================================================
%% Memory access (VM registers, context fields)
%%=============================================================================

move_to_vm_register(State0, Value, {ptr, Local}) ->
    %% Store value to memory at [Local]
    Code = <<
        (jit_wasm32_asm:local_get(Local))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code);
move_to_vm_register(State0, Value, {x_reg, N}) ->
    %% Store value to ctx->x[N]
    %% ctx->x is at CTX_X_OFFSET, each x register is 4 bytes (word_size)
    Offset = ?CTX_X_OFFSET + N * 4,
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, Offset))/binary
    >>,
    emit(State0, Code);
move_to_vm_register(State0, Value, {y_reg, N}) ->
    %% Store value to ctx->e[N]
    %% ctx->e is a pointer at CTX_E_OFFSET, then index by N * 4
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        %% Load ctx->e (stack pointer)
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        %% Store value at e[N]
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, N * 4))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal);
move_to_vm_register(State0, {free, {ptr, Reg, WordOffset}}, {fp_reg, N}) ->
    %% Store boxed float data to ctx->fr[N] (8 bytes per double)
    DataOffset = WordOffset * 4,
    {State1, FpRegsLocal} = alloc_local(State0),
    Code = <<
        %% Load ctx->fr pointer
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_FR_OFFSET))/binary,
        (jit_wasm32_asm:local_set(FpRegsLocal))/binary,
        %% Copy first 4 bytes of the double
        (jit_wasm32_asm:local_get(FpRegsLocal))/binary,
        (jit_wasm32_asm:local_get(Reg))/binary,
        (jit_wasm32_asm:i32_load(2, DataOffset))/binary,
        (jit_wasm32_asm:i32_store(2, N * 8))/binary,
        %% Copy second 4 bytes of the double
        (jit_wasm32_asm:local_get(FpRegsLocal))/binary,
        (jit_wasm32_asm:local_get(Reg))/binary,
        (jit_wasm32_asm:i32_load(2, DataOffset + 4))/binary,
        (jit_wasm32_asm:i32_store(2, N * 8 + 4))/binary
    >>,
    State2 = free_native_register(State1, Reg),
    State3 = free_native_register(State2, FpRegsLocal),
    emit(State3, Code).

move_to_native_register(State, Reg) when is_atom(Reg) ->
    %% Check if it's already a scratch local (localN where N >= FIRST_SCRATCH_LOCAL)
    case is_scratch_local(Reg) of
        true -> {State, Reg};
        false -> move_to_native_register_alloc(State, Reg)
    end;
move_to_native_register(State0, Value) ->
    move_to_native_register_alloc(State0, Value).

move_to_native_register_alloc(State0, Value) ->
    {State1, Local} = alloc_local(State0),
    Code = <<
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, Local}.

move_to_native_register(State0, Value, Local) ->
    Code = <<
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State1 = emit(State0, Code),
    State1#state{regs = Regs1}.

move_to_cp(State0, {y_reg, Y}) ->
    %% Load y register and store to ctx->cp
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        %% Load ctx->e
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        %% Load e[Y]
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Y * 4))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        %% Store to ctx->cp
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_CP_OFFSET))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal).

move_array_element(State0, Base, Index, {ptr, Dest}) when is_integer(Index) ->
    %% Load word at Base[Index] and store into memory at [Dest]
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(Dest))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, Index, {x_reg, N}) when is_integer(Index) ->
    %% Load word at Base[Index] and store into ctx->x[N]
    BaseLocal = unwrap_local(Base),
    Offset = ?CTX_X_OFFSET + N * 4,
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:i32_store(2, Offset))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, Index, {y_reg, N}) when is_integer(Index) ->
    %% Load word at Base[Index] and store into ctx->e[N]
    BaseLocal = unwrap_local(Base),
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:i32_store(2, N * 4))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal);
move_array_element(State0, Base, Index, Dest) when is_integer(Index), is_atom(Dest) ->
    %% Load word at Base[Index] and store into local Dest
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:local_set(Dest))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, {free, IndexReg}, {ptr, Dest}) ->
    %% Load word at Base[IndexReg * 4] and store into memory at [Dest]
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(Dest))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, {free, IndexReg}, {x_reg, N}) ->
    %% Load word at Base[IndexReg * 4] and store into ctx->x[N]
    BaseLocal = unwrap_local(Base),
    Offset = ?CTX_X_OFFSET + N * 4,
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:i32_store(2, Offset))/binary
    >>,
    emit(State0, Code);
move_array_element(State0, Base, {free, IndexReg}, {y_reg, N}) ->
    %% Load word at Base[IndexReg * 4] and store into ctx->e[N]
    BaseLocal = unwrap_local(Base),
    {State1, TempLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:local_set(TempLocal))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:i32_store(2, N * 4))/binary
    >>,
    State2 = emit(State1, Code),
    free_native_register(State2, TempLocal);
move_array_element(State0, Base, {free, IndexReg}, Dest) when is_atom(Dest) ->
    %% Load word at Base[IndexReg * 4] and store into local Dest
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexReg))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:local_set(Dest))/binary
    >>,
    emit(State0, Code).

move_to_array_element(State0, Value, Base, Index) ->
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, Index * 4))/binary
    >>,
    emit(State0, Code).

move_to_array_element(State0, Value, Base, IndexLocal, _WordSize) ->
    %% Store value at Base + IndexLocal * 4
    BaseLocal = unwrap_local(Base),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:local_get(IndexLocal))/binary,
        (jit_wasm32_asm:i32_const(2))/binary,
        (jit_wasm32_asm:i32_shl())/binary,
        (jit_wasm32_asm:i32_add())/binary,
        (emit_value_to_stack(Value))/binary,
        (jit_wasm32_asm:i32_store(2, 0))/binary
    >>,
    emit(State0, Code).

set_bs(State0, TermLocal) ->
    Code = <<
        %% Store term to ctx->bs
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TermLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_BS_OFFSET))/binary,
        %% Store 0 to ctx->bs_offset
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_const(0))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_BS_OFFSET_OFFSET))/binary
    >>,
    emit(State0, Code).

copy_to_native_register(State0, Value) ->
    move_to_native_register(State0, Value).

get_array_element(State0, Base, Index) ->
    BaseLocal = unwrap_local(Base),
    {State1, ResultLocal} = alloc_local(State0),
    Code = <<
        (jit_wasm32_asm:local_get(BaseLocal))/binary,
        (jit_wasm32_asm:i32_load(2, Index * 4))/binary,
        (jit_wasm32_asm:local_set(ResultLocal))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, ResultLocal),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, ResultLocal}.

increment_sp(State0, Offset) ->
    %% ctx->e += Offset * 4
    Code = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        %% Load current ctx->e
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        %% Add offset
        (jit_wasm32_asm:i32_const(Offset * 4))/binary,
        (jit_wasm32_asm:i32_add())/binary,
        %% Store back
        (jit_wasm32_asm:i32_store(2, ?CTX_E_OFFSET))/binary
    >>,
    emit(State0, Code).

%%=============================================================================
%% Continuation and module index
%%=============================================================================

set_continuation_to_label(State0, Label) ->
    emit_set_continuation_for_label(State0, Label).

set_continuation_to_offset(State0) ->
    %% Create a unique reference for this continuation point.
    %% For WASM, continuations are label-based. This reference will be
    %% registered as a label via add_label/2 when the continuation target
    %% is defined (typically the next BEAM label).
    OffsetRef = make_ref(),
    {State0, OffsetRef}.

-spec continuation_entry_point(state()) -> state().
continuation_entry_point(State) ->
    %% No-op for WASM, same as riscv32
    State.

get_module_index(State0) ->
    {State1, Local} = alloc_local(State0),
    %% Load jit_state->module, then load module->module_index
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?JITSTATE_MODULE_OFFSET))/binary,
        (jit_wasm32_asm:i32_load(2, 0))/binary,
        (jit_wasm32_asm:local_set(Local))/binary
    >>,
    Regs1 = jit_regs:invalidate_reg(State0#state.regs, Local),
    State2 = emit(State1, Code),
    {State2#state{regs = Regs1}, Local}.

%%=============================================================================
%% Scheduling and reductions
%%=============================================================================

decrement_reductions_and_maybe_schedule_next(State0) ->
    {State1, TempLocal} = alloc_local(State0),
    %% Load, decrement, store reductions
    %% If zero, call schedule_next_cp and return
    Code = <<
        %% Load reduction count
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_tee(TempLocal))/binary,
        %% Store decremented value back
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        %% Check if zero
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_eqz())/binary
    >>,
    State2 = emit(State1, Code),
    %% If reductions == 0, schedule next
    State3 = emit(State2, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State4 = call_primitive_last(State3, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    State5 = emit(State4, jit_wasm32_asm:end_()),
    State6 = free_native_register(State5, TempLocal),
    MergedRegs = jit_regs:merge(State2#state.regs, State5#state.regs),
    State6#state{regs = MergedRegs}.

call_or_schedule_next(State0, Label) ->
    %% WASM: Split the function at the call site. The return from the called
    %% function lands in a new WASM function (continuation label).

    %% Allocate a new label number for the continuation
    ContLabel = State0#state.labels_count + 1,
    State1 = State0#state{labels_count = ContLabel},

    %% Set CP to point to the continuation label
    State2 = emit_set_cp_for_label(State1, ContLabel),

    %% Do the call (sets continuation for scheduling, then jumps)
    State3 = call_only_or_schedule_next(State2, Label),

    %% Split: close current function, open continuation function
    #state{
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        jump_table_start = JumpTableStart,
        labels = Labels
    } = State3,
    FinalizedBody =
        <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
            (jit_wasm32_asm:return())/binary>>,
    NewFuncBodies = [{PrevLabel, FinalizedBody} | FuncBodies],
    ContLabelOff = JumpTableStart + ContLabel * ?JUMP_TABLE_ENTRY_SIZE,
    %% The continuation function starts fresh but must declare enough locals
    %% to cover any that jit.erl might allocate subsequently.
    %% Preserve max_scratch from State3 (the high-water mark so far).
    %% Reset used/available so all scratch locals are available.
    AllFree = (1 bsl State3#state.max_scratch) - 1,
    State3#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = ContLabel,
        labels = [{ContLabel, ContLabelOff} | Labels],
        available_regs = AllFree,
        used_regs = 0,
        regs = jit_regs:invalidate_all(State3#state.regs)
    }.

call_only_or_schedule_next(State0, Label) ->
    {State1, TempLocal} = alloc_local(State0),
    %% Decrement reductions
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_sub())/binary,
        (jit_wasm32_asm:local_tee(TempLocal))/binary,
        %% Store back
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_REDUCTIONCOUNT_OFFSET))/binary,
        %% Check if zero
        (jit_wasm32_asm:local_get(TempLocal))/binary,
        (jit_wasm32_asm:i32_eqz())/binary
    >>,
    State2 = emit(State1, Code),
    %% If zero: set continuation and schedule
    State3 = emit(State2, jit_wasm32_asm:if_(jit_wasm32_asm:blocktype_void())),
    State4 = emit_set_continuation_for_label(State3, Label),
    State5 = call_primitive_last(State4, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    State6 = emit(State5, jit_wasm32_asm:end_()),
    State7 = free_native_register(State6, TempLocal),
    %% If not zero: jump to label
    jump_to_label(State7, Label).

%%=============================================================================
%% Function pointer calls (for BIFs)
%%=============================================================================

call_func_ptr(State0, FuncPtrTuple, Args) ->
    %% For WASM, function pointer calls go through call_indirect.
    %% For BIFs loaded via get_imported_bif, the pointer is a WASM table index.
    %%
    %% Emit: push args, push func ptr, call_indirect
    {State1, ResultLocal} = alloc_local(State0),
    State2 = emit_push_args(State1, Args),
    %% Push the function pointer onto the stack
    FuncPtrCode =
        case FuncPtrTuple of
            {free, Local} ->
                jit_wasm32_asm:local_get(Local);
            {primitive, Primitive} ->
                <<
                    (jit_wasm32_asm:local_get(?NATIVE_INTERFACE_LOCAL))/binary,
                    (jit_wasm32_asm:i32_load(2, Primitive * 4))/binary
                >>
        end,
    State3 = emit(State2, FuncPtrCode),
    %% call_indirect with the appropriate type for the argument count
    WasmArgCount = count_wasm_args(Args),
    TypeIdx = WasmArgCount - 1,
    State4 = emit(State3, jit_wasm32_asm:call_indirect(TypeIdx, 0)),
    %% Store result
    State5 = emit(State4, jit_wasm32_asm:local_set(ResultLocal)),
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    State6 = free_func_ptr(State5, FuncPtrTuple),
    {State6#state{regs = Regs1}, ResultLocal}.

%%=============================================================================
%% Labels and metadata
%%=============================================================================

return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        func_bodies = FuncBodies0,
        current_body = CurrentBody,
        current_label = CurLabel,
        labels_count = LabelsCount,
        max_scratch = MaxScratch
    } = State,
    _SortedLines
) ->
    %% Save the last label's body
    FuncBodies1 =
        case CurLabel of
            undefined ->
                FuncBodies0;
            _ ->
                FinalizedBody =
                    <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                        (jit_wasm32_asm:return())/binary>>,
                [{CurLabel, FinalizedBody} | FuncBodies0]
        end,
    %% Sort func_bodies by label number (ascending)
    SortedBodies = lists:keysort(1, [{L, B} || {L, B} <- FuncBodies1, is_integer(L)]),

    %% Build a complete WASM module binary
    WasmModule = assemble_wasm_module(SortedBodies, LabelsCount, MaxScratch),

    %% Append the WASM module binary to the stream
    WasmOffset = StreamModule:offset(Stream0),
    Stream1 = StreamModule:append(Stream0, WasmModule),

    %% Patch the jump table header
    %% The jump table data starts at JumpTableStart - 8 in the stream
    JumpTableDataStart = State#state.jump_table_start - 8,
    %% Patch num_entries (bytes 0-3) to include continuation labels
    NumEntries = LabelsCount + 1,
    Stream2 = StreamModule:replace(Stream1, JumpTableDataStart, <<NumEntries:32/little>>),
    %% Patch wasm_offset (bytes 4-7) relative to the jump table data start
    WasmOffsetPos = JumpTableDataStart + 4,
    RelativeWasmOffset = WasmOffset - JumpTableDataStart,
    Stream3 = StreamModule:replace(Stream2, WasmOffsetPos, <<RelativeWasmOffset:32/little>>),

    State#state{
        stream = Stream3,
        current_body = <<>>,
        current_label = undefined
    }.

-spec add_label(state(), integer() | reference()) -> state().
add_label(State0, Label) ->
    add_label(State0, Label, undefined).

-spec add_label(state(), integer() | reference(), integer() | undefined) -> state().
add_label(
    #state{
        jump_table_start = JumpTableStart,
        func_bodies = FuncBodies,
        current_body = CurrentBody,
        current_label = PrevLabel,
        labels = Labels
    } = State,
    Label,
    _LabelOffset
) when is_integer(Label) ->
    %% Save the previous label's body (if any)
    NewFuncBodies =
        case PrevLabel of
            undefined ->
                FuncBodies;
            _ ->
                %% Finalize previous body: add return ctx and end instruction
                FinalizedBody =
                    <<CurrentBody/binary, (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
                        (jit_wasm32_asm:return())/binary>>,
                [{PrevLabel, FinalizedBody} | FuncBodies]
        end,
    %% Compute label offset (= position in jump table data)
    LabelOff = JumpTableStart + Label * ?JUMP_TABLE_ENTRY_SIZE,
    Regs1 = jit_regs:invalidate_all(State#state.regs),
    State#state{
        func_bodies = NewFuncBodies,
        current_body = <<>>,
        current_label = Label,
        labels = [{Label, LabelOff} | Labels],
        regs = Regs1
    };
add_label(
    #state{
        jump_table_start = JumpTableStart,
        labels = Labels
    } = State,
    Label,
    _Offset
) ->
    %% Reference labels - record offset for branch resolution
    %% Use a computed offset based on the current label
    LabelOff =
        case State#state.current_label of
            undefined ->
                JumpTableStart;
            CurLabel when is_integer(CurLabel) ->
                JumpTableStart + CurLabel * ?JUMP_TABLE_ENTRY_SIZE;
            _ ->
                JumpTableStart
        end,
    State#state{labels = [{Label, LabelOff} | Labels]}.

%% @doc Get the register tracking state.
get_regs_tracking(#state{regs = Regs}) -> Regs.

%%=============================================================================
%% Internal helpers
%%=============================================================================

%% Emit bytecode to the current label's body (accumulated per-label)
emit(#state{current_body = Body} = State, Code) ->
    State#state{current_body = <<Body/binary, Code/binary>>}.

%% Allocate a scratch local variable.
%% When the initial pool is exhausted, dynamically extend with new locals.
alloc_local(#state{available_regs = 0, used_regs = Used, max_scratch = MaxScratch} = State) ->
    LocalIdx = ?FIRST_SCRATCH_LOCAL + MaxScratch,
    Bit = 1 bsl MaxScratch,
    LocalAtom = index_to_local(LocalIdx),
    {
        State#state{
            used_regs = Used bor Bit,
            max_scratch = MaxScratch + 1
        },
        LocalAtom
    };
alloc_local(#state{available_regs = Available, used_regs = Used} = State) ->
    LocalIdx = first_avail_local(Available),
    Bit = local_bit(LocalIdx),
    LocalAtom = index_to_local(LocalIdx),
    {
        State#state{
            available_regs = Available band (bnot Bit),
            used_regs = Used bor Bit
        },
        LocalAtom
    }.

%% Get the first available local from bitmask
first_avail_local(Mask) ->
    first_avail_local(Mask, 0).
first_avail_local(Mask, N) ->
    case Mask band (1 bsl N) of
        0 -> first_avail_local(Mask, N + 1);
        _ -> N + ?FIRST_SCRATCH_LOCAL
    end.

%% Convert bitmask to list of local indices
mask_to_locals(Mask) ->
    mask_to_locals(Mask, 0, []).
mask_to_locals(0, _N, Acc) ->
    lists:reverse(Acc);
mask_to_locals(Mask, N, Acc) ->
    case Mask band 1 of
        1 -> mask_to_locals(Mask bsr 1, N + 1, [index_to_local(N + ?FIRST_SCRATCH_LOCAL) | Acc]);
        0 -> mask_to_locals(Mask bsr 1, N + 1, Acc)
    end.

%% Check if an atom is a scratch local (local3, local4, ...)
is_scratch_local(local3) ->
    true;
is_scratch_local(local4) ->
    true;
is_scratch_local(local5) ->
    true;
is_scratch_local(local6) ->
    true;
is_scratch_local(local7) ->
    true;
is_scratch_local(local8) ->
    true;
is_scratch_local(local9) ->
    true;
is_scratch_local(local10) ->
    true;
is_scratch_local(Atom) when is_atom(Atom) ->
    case atom_to_list(Atom) of
        "local" ++ Rest ->
            case catch list_to_integer(Rest) of
                N when is_integer(N), N >= ?FIRST_SCRATCH_LOCAL -> true;
                _ -> false
            end;
        _ ->
            false
    end;
is_scratch_local(_) ->
    false.

%% Unwrap a local reference (stripping {ptr, _} and {free, _} wrappers)
unwrap_local({ptr, L}) -> unwrap_local(L);
unwrap_local({free, L}) -> unwrap_local(L);
unwrap_local(L) when is_atom(L) -> L.

%% Convert a WASM local integer index to an atom name
index_to_local(0) ->
    local0;
index_to_local(1) ->
    local1;
index_to_local(2) ->
    local2;
index_to_local(3) ->
    local3;
index_to_local(4) ->
    local4;
index_to_local(5) ->
    local5;
index_to_local(6) ->
    local6;
index_to_local(7) ->
    local7;
index_to_local(8) ->
    local8;
index_to_local(9) ->
    local9;
index_to_local(10) ->
    local10;
index_to_local(N) when is_integer(N), N > 10 ->
    list_to_atom("local" ++ integer_to_list(N)).

%% Get bit position for a scratch local (takes integer index or atom)
local_bit(LocalAtom) when is_atom(LocalAtom) ->
    local_bit(jit_wasm32_asm:local_index(LocalAtom));
local_bit(LocalIdx) when is_integer(LocalIdx), LocalIdx >= ?FIRST_SCRATCH_LOCAL ->
    1 bsl (LocalIdx - ?FIRST_SCRATCH_LOCAL).

%% Merge used registers from two paths
merge_used_regs(#state{used_regs = UR, max_scratch = MS} = State, OtherUR) ->
    MergedUR = UR bor OtherUR,
    AllFree = (1 bsl MS) - 1,
    MergedAvail = AllFree band (bnot MergedUR),
    State#state{used_regs = MergedUR, available_regs = MergedAvail}.

%% Push a value onto the WASM operand stack.
%% Atom names are WASM local variable references (from alloc_local).
%% Bare integers are immediate constants (tagged term values from jit.erl).
emit_value_to_stack(cp) ->
    <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_CP_OFFSET))/binary
    >>;
emit_value_to_stack({free, Local}) when is_atom(Local) ->
    jit_wasm32_asm:local_get(Local);
emit_value_to_stack({free, Imm}) when is_integer(Imm) ->
    %% Immediate tagged value (e.g., from decode_compact_term for small integers)
    %% Truncate to 32 bits since host Erlang may produce 64-bit terms
    jit_wasm32_asm:i32_const(to_i32(Imm));
emit_value_to_stack({x_reg, N}) ->
    %% Load ctx->x[N]
    Offset = ?CTX_X_OFFSET + N * 4,
    <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, Offset))/binary
    >>;
emit_value_to_stack({y_reg, N}) ->
    %% Load ctx->e[N]
    <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, ?CTX_E_OFFSET))/binary,
        (jit_wasm32_asm:i32_load(2, N * 4))/binary
    >>;
emit_value_to_stack({ptr, Local}) ->
    jit_wasm32_asm:local_get(Local);
emit_value_to_stack(Local) when is_atom(Local) ->
    jit_wasm32_asm:local_get(Local);
emit_value_to_stack(Imm) when is_integer(Imm) ->
    %% Immediate constant (tagged term value)
    %% Truncate to signed 32-bit range since host Erlang uses 64-bit terms
    jit_wasm32_asm:i32_const(to_i32(Imm)).

%% Emit code to set jit_state->continuation to a label's entry point.
%% Stores (Label + 1) in continuation to distinguish from NULL (0).
%% The C dispatch loop subtracts 1 and calls module_get_native_entry_point
%% to convert the label number to an actual function pointer.
emit_set_continuation_for_label(#state{} = State, Label) when is_integer(Label) ->
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_const(Label + 1))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>,
    emit(State, Code);
emit_set_continuation_for_label(State, _RefLabel) ->
    %% Reference labels: store 0 (NULL). The reference will be resolved
    %% when the target label is added, but for WASM cross-label jumps
    %% always go through the C dispatch loop anyway.
    Code = <<
        (jit_wasm32_asm:local_get(?JITSTATE_LOCAL))/binary,
        (jit_wasm32_asm:i32_const(0))/binary,
        (jit_wasm32_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>,
    emit(State, Code).

%% Emit code to set up CP (continuation pointer for returns).
%% CP format: (module_index << 24) | (label_offset << 2)
%% where label_offset = label * JUMP_TABLE_ENTRY_SIZE (relative to jump_table_start).
%% On return, jit_return() computes:
%%   label = ((cp & 0xFFFFFF) >> 2) / JUMP_TABLE_ENTRY_SIZE
%% and uses module_get_native_entry_point(mod, label) to get the function pointer.
%%
%% At the time emit_set_cp is called, we don't know the exact return label.
%% We use the current label's offset as the CP target. The caller (jit.erl)
%% ensures that the return point is the immediately following label.
emit_set_cp(State0) ->
    LabelOffset =
        case State0#state.current_label of
            undefined -> 0;
            CurLabel -> (CurLabel + 1) * ?JUMP_TABLE_ENTRY_SIZE
        end,
    emit_set_cp_for_offset(State0, LabelOffset).

emit_set_cp_for_label(State0, Label) ->
    LabelOffset = Label * ?JUMP_TABLE_ENTRY_SIZE,
    emit_set_cp_for_offset(State0, LabelOffset).

emit_set_cp_for_offset(State0, LabelOffset) ->
    {State1, ModIdxLocal} = get_module_index(State0),
    State2 = shift_left(State1, ModIdxLocal, 24),
    {State3, OffsetLocal} = alloc_local(State2),
    Code = <<
        (jit_wasm32_asm:i32_const(LabelOffset bsl 2))/binary,
        (jit_wasm32_asm:local_set(OffsetLocal))/binary
    >>,
    State4 = emit(State3, Code),
    {State5, _CPLocal} = or_(State4, {free, ModIdxLocal}, OffsetLocal),
    State6 = free_native_register(State5, OffsetLocal),
    %% Store CP to ctx->cp
    Code2 = <<
        (jit_wasm32_asm:local_get(?CTX_LOCAL))/binary,
        (jit_wasm32_asm:local_get(ModIdxLocal))/binary,
        (jit_wasm32_asm:i32_store(2, ?CTX_CP_OFFSET))/binary
    >>,
    State7 = emit(State6, Code2),
    free_native_register(State7, ModIdxLocal).

%% Emit code to call a primitive function
emit_call_primitive(State0, Primitive, Args, ResultLocal, IsTailCall) ->
    %% Count WASM-level arguments (avm_int64_t counts as 2 i32 args)
    WasmArgCount = count_wasm_args(Args),
    %% Push arguments onto the WASM stack
    State1 = emit_push_args(State0, Args),
    %% Load the function pointer from the native interface table
    Code = <<
        (jit_wasm32_asm:local_get(?NATIVE_INTERFACE_LOCAL))/binary,
        (jit_wasm32_asm:i32_load(2, Primitive * 4))/binary
    >>,
    State2 = emit(State1, Code),
    %% call_indirect with appropriate type index for the argument count
    %% Type index = ArgCount - 1 (type 0 = 1 arg, type 1 = 2 args, etc.)
    TypeIdx = WasmArgCount - 1,
    State3 = emit(State2, jit_wasm32_asm:call_indirect(TypeIdx, 0)),
    case IsTailCall of
        true ->
            %% Return the result directly
            State4 = emit(State3, jit_wasm32_asm:return()),
            State4;
        false ->
            %% Store the result in a local
            State4 = emit(State3, jit_wasm32_asm:local_set(ResultLocal)),
            State4
    end.

%% Emit code to push function arguments onto the WASM stack
emit_push_args(State, []) ->
    State;
emit_push_args(State0, [Arg | Rest]) ->
    Code =
        case Arg of
            ctx ->
                jit_wasm32_asm:local_get(?CTX_LOCAL);
            jit_state ->
                jit_wasm32_asm:local_get(?JITSTATE_LOCAL);
            jit_state_tail_call ->
                jit_wasm32_asm:local_get(?JITSTATE_LOCAL);
            offset ->
                LabelOffset =
                    case State0#state.current_label of
                        undefined -> 0;
                        CurLabel -> CurLabel * ?JUMP_TABLE_ENTRY_SIZE
                    end,
                jit_wasm32_asm:i32_const(LabelOffset);
            stack ->
                jit_wasm32_asm:i32_const(0);
            {free, {ptr, Local}} ->
                jit_wasm32_asm:local_get(Local);
            {free, {x_reg, N}} ->
                emit_value_to_stack({x_reg, N});
            {free, {y_reg, N}} ->
                emit_value_to_stack({y_reg, N});
            {free, Local} when is_atom(Local) ->
                jit_wasm32_asm:local_get(Local);
            {free, Imm} when is_integer(Imm) ->
                jit_wasm32_asm:i32_const(to_i32(Imm));
            {ptr, Local} ->
                jit_wasm32_asm:local_get(Local);
            {avm_int64_t, Val} ->
                %% For 32-bit WASM, pass i64 as two i32 values
                %% Low 32 bits first, then high 32 bits
                <<
                    (jit_wasm32_asm:i32_const(Val band 16#FFFFFFFF))/binary,
                    (jit_wasm32_asm:i32_const((Val bsr 32) band 16#FFFFFFFF))/binary
                >>;
            {x_reg, N} ->
                emit_value_to_stack({x_reg, N});
            {y_reg, N} ->
                emit_value_to_stack({y_reg, N});
            Local when is_atom(Local) -> jit_wasm32_asm:local_get(Local);
            Local when is_integer(Local) -> jit_wasm32_asm:i32_const(to_i32(Local))
        end,
    State1 = emit(State0, Code),
    emit_push_args(State1, Rest).

%% Emit a condition test, leaving the result (i32 0 or 1) on the stack
emit_condition(State0, {Local, '<', 0}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(0))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '<', Val}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(Val))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '<', OtherLocal}) when is_atom(OtherLocal) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:local_get(OtherLocal))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Val, '<', Local}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (jit_wasm32_asm:i32_const(Val))/binary,
        (jit_wasm32_asm:local_get(L))/binary,
        (jit_wasm32_asm:i32_lt_s())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '==', Val}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(Val))/binary,
        (jit_wasm32_asm:i32_eq())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '!=', Val}) when is_integer(Val) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(Val))/binary,
        (jit_wasm32_asm:i32_ne())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '!=', OtherLocal}) when is_atom(OtherLocal) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:local_get(OtherLocal))/binary,
        (jit_wasm32_asm:i32_ne())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {'(int)', Local, '==', Val}) ->
    emit_condition(State0, {Local, '==', Val});
emit_condition(State0, {'(int)', Local, '!=', Val}) ->
    emit_condition(State0, {Local, '!=', Val});
emit_condition(State0, {'(bool)', Local, '==', false}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:i32_eqz())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {'(bool)', Local, '!=', false}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(1))/binary,
        (jit_wasm32_asm:i32_and())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {Local, '&', Mask, '!=', Val}) ->
    L = unwrap_free(Local),
    Code = <<
        (emit_unwrapped_to_stack(L))/binary,
        (jit_wasm32_asm:i32_const(Mask))/binary,
        (jit_wasm32_asm:i32_and())/binary,
        (jit_wasm32_asm:i32_const(Val))/binary,
        (jit_wasm32_asm:i32_ne())/binary
    >>,
    State1 = maybe_free(State0, Local),
    emit(State1, Code);
emit_condition(State0, {{free, L1}, '==', {free, L2}}) ->
    Code = <<
        (emit_unwrapped_to_stack(L1))/binary,
        (emit_unwrapped_to_stack(L2))/binary,
        (jit_wasm32_asm:i32_eq())/binary
    >>,
    State1 = free_native_register(State0, L1),
    State2 = free_native_register(State1, L2),
    emit(State2, Code).

emit_and_conditions(State0, []) ->
    %% Push 1 (true) if no conditions
    emit(State0, jit_wasm32_asm:i32_const(1));
emit_and_conditions(State0, [Cond]) ->
    emit_condition(State0, Cond);
emit_and_conditions(State0, [Cond | Rest]) ->
    State1 = emit_condition(State0, Cond),
    State2 = emit_and_conditions(State1, Rest),
    emit(State2, jit_wasm32_asm:i32_and()).

unwrap_free({free, L}) -> L;
unwrap_free(L) -> L.

%% Truncate an Erlang integer to signed 32-bit range for WASM i32.const.
%% Host Erlang may use 64-bit terms, so we must truncate to 32 bits.
to_i32(Val) when is_integer(Val) ->
    Masked = Val band 16#FFFFFFFF,
    case Masked > 16#7FFFFFFF of
        true -> Masked - 16#100000000;
        false -> Masked
    end.

%% Push an unwrapped value to the WASM stack.
%% Atom locals use local_get; integers are immediate constants.
emit_unwrapped_to_stack(L) when is_atom(L) ->
    jit_wasm32_asm:local_get(L);
emit_unwrapped_to_stack(Imm) when is_integer(Imm) ->
    jit_wasm32_asm:i32_const(to_i32(Imm)).

%% Count WASM-level i32 arguments (avm_int64_t counts as 2)
count_wasm_args(Args) ->
    lists:foldl(
        fun
            ({avm_int64_t, _}, Acc) -> Acc + 2;
            (_, Acc) -> Acc + 1
        end,
        0,
        Args
    ).

maybe_free(State, {free, Local}) -> free_native_register(State, Local);
maybe_free(State, _) -> State.

free_func_ptr(State, {free, Local}) -> free_native_register(State, Local);
free_func_ptr(State, _) -> State.

%%=============================================================================
%% WASM module assembly
%%
%% Produces a complete, valid WebAssembly module binary from the accumulated
%% per-label function bodies.
%%
%% Module structure:
%%   - Type section: function signatures
%%   - Import section: Emscripten's memory and indirect function table
%%   - Function section: type indices for each function
%%   - Export section: "f0", "f1", ... for each label function
%%   - Code section: function bodies
%%
%% Type indices:
%%   0: (i32, i32, i32) -> i32  [entry point signature, used for all label functions]
%%
%% Import indices:
%%   0: env.memory (memory)
%%   1: env.__indirect_function_table (table funcref)
%%
%% Note: The initial implementation uses type 0 for all call_indirect calls.
%% This works for primitives with the entry point signature (3 args -> i32)
%% but will need refinement for primitives with different signatures.
%%=============================================================================

assemble_wasm_module(SortedBodies, LabelsCount, MaxScratch) ->
    Asm = jit_wasm32_asm,

    %% Type section: define function types for different arities
    %% Type N: (i32 x (N+1)) -> i32
    %% Type 0: 1 arg, Type 1: 2 args, ..., Type 7: 8 args
    %% The entry point type (3 args) is Type 2
    Types = [
        Asm:encode_func_type(lists:duplicate(N, Asm:type_i32()), [Asm:type_i32()])
     || N <- lists:seq(1, 8)
    ],
    TypeSection = Asm:encode_type_section(Types),

    %% Import section:
    %%   import 0: "env" "memory" memory {min: 256}
    %%   import 1: "env" "__indirect_function_table" table funcref {min: 0}
    MemoryImport = <<
        (Asm:encode_name("env"))/binary,
        (Asm:encode_name("memory"))/binary,
        %% import kind: memory
        16#02,
        %% limits: shared + has max (required for -pthread Emscripten builds)
        16#03,
        %% min pages
        (Asm:encode_uleb128(256))/binary,
        %% max pages (65536 = 4GB max)
        (Asm:encode_uleb128(65536))/binary
    >>,
    TableImport = <<
        (Asm:encode_name("env"))/binary,
        (Asm:encode_name("__indirect_function_table"))/binary,
        %% import kind: table
        16#01,
        (Asm:type_funcref())/binary,
        %% limits: min only
        16#00,
        %% min entries
        (Asm:encode_uleb128(0))/binary
    >>,
    ImportSection = Asm:encode_section(2, Asm:encode_vector([MemoryImport, TableImport])),

    %% Function section: all label functions use type 2 (3 args: ctx, jit_state, native_interface)
    NumFunctions = LabelsCount + 1,
    FunctionTypeIndices = lists:duplicate(NumFunctions, 2),
    FunctionSection = Asm:encode_function_section(FunctionTypeIndices),

    %% Export section: export each label function as "f0", "f1", ...
    Exports = lists:map(
        fun(LabelIdx) ->
            FuncName = "f" ++ integer_to_list(LabelIdx),
            %% Function index = LabelIdx (since imports don't count as function indices
            %% in the function section, but they DO count in the function index space)
            %% Import functions: 0 = memory, 1 = table (these are NOT function imports)
            %% So function indices start at 0 for our defined functions.
            <<
                (Asm:encode_name(FuncName))/binary,
                %% export kind: function
                16#00,
                %% function index
                (Asm:encode_uleb128(LabelIdx))/binary
            >>
        end,
        lists:seq(0, NumFunctions - 1)
    ),
    ExportSection = Asm:encode_export_section(Exports),

    %% Code section: function bodies
    %% Each function has 3 params (i32 each) + MaxScratch i32 locals
    ScratchLocals = [{MaxScratch, Asm:type_i32()}],
    FuncBodiesEncoded = build_func_bodies(SortedBodies, 0, NumFunctions, ScratchLocals, Asm),
    CodeSection = Asm:encode_code_section(FuncBodiesEncoded),

    %% Assemble the complete module
    <<
        (Asm:wasm_magic())/binary,
        (Asm:wasm_version())/binary,
        TypeSection/binary,
        ImportSection/binary,
        FunctionSection/binary,
        ExportSection/binary,
        CodeSection/binary
    >>.

%% Build encoded function bodies for all labels (0 to NumFunctions-1).
%% Labels that don't have compiled code get a stub that returns ctx.
build_func_bodies(SortedBodies, Idx, NumFunctions, ScratchLocals, Asm) ->
    build_func_bodies(SortedBodies, Idx, NumFunctions, ScratchLocals, Asm, []).

build_func_bodies(_SortedBodies, Idx, NumFunctions, _ScratchLocals, _Asm, Acc) when
    Idx >= NumFunctions
->
    lists:reverse(Acc);
build_func_bodies(SortedBodies, Idx, NumFunctions, ScratchLocals, Asm, Acc) ->
    Body =
        case lists:keyfind(Idx, 1, SortedBodies) of
            {Idx, InstrBytes} ->
                %% Real function body: locals + instructions + end
                Expr = <<InstrBytes/binary, (Asm:end_())/binary>>,
                Asm:encode_func_body(ScratchLocals, Expr);
            false ->
                %% Stub: return ctx (local 0)
                StubExpr = <<
                    (Asm:local_get(0))/binary,
                    (Asm:end_())/binary
                >>,
                Asm:encode_func_body(ScratchLocals, StubExpr)
        end,
    build_func_bodies(SortedBodies, Idx + 1, NumFunctions, ScratchLocals, Asm, [Body | Acc]).
