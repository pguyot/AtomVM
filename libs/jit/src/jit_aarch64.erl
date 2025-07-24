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

-module(jit_aarch64).

-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/2,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_equal_to_ctx/2,
    jump_to_label/2,
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
    return_labels_and_lines/3
]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-define(ASSERT(Expr), true = Expr).

%% AArch64 ABI: r0-r7 are used for argument passing and return value.
%% r8 is the indirect result location register (platform-specific),
%% r9-r15 are caller-saved scratch registers (used by JIT),
%% r16-r17 are intra-procedure-call scratch registers,
%% r18 is platform register (reserved),
%% r19-r28 are callee-saved,
%% r29 is frame pointer, r30 is link register, r31 is stack pointer/zero.
%% d0-d7 are used for FP argument passing and return value.
%% d8-d15 are callee-saved FP registers.
%%
%% See: Arm® Architecture Procedure Call Standard (AAPCS64)
%% https://developer.arm.com/documentation/ihi0055/latest/
%%
%% Registers used by the JIT backend:
%%   - Scratch GPRs: r9-r15
%%   - Argument/return: r0-r7, d0-d7
%%   - Stack pointer: r31 (sp)
%%   - Frame pointer: r29
%%   - Link register: r30
%%   - Indirect result: r8
%%
%% Note: r18 is reserved for platform use and must not be used.
%%
%% For more details, refer to the AArch64 Procedure Call Standard.

-type aarch64_register() ::
    r0
    | r1
    | r2
    | r3
    | r4
    | r5
    | r6
    | r7
    | r8
    | r9
    | r10
    | r11
    | r12
    | r13
    | r14
    | r15
    | d0
    | d1
    | d2
    | d3
    | d4
    | d5
    | d6
    | d7.

-define(IS_GPR(Reg),
    (Reg =:= r0 orelse Reg =:= r1 orelse Reg =:= r2 orelse Reg =:= r3 orelse Reg =:= r4 orelse
        Reg =:= r5 orelse Reg =:= r6 orelse Reg =:= r7 orelse Reg =:= r8 orelse Reg =:= r9 orelse
        Reg =:= r10 orelse Reg =:= r11 orelse Reg =:= r12 orelse Reg =:= r13 orelse Reg =:= r14 orelse
        Reg =:= r15)
).
-define(IS_FPR(Reg),
    (Reg =:= d0 orelse Reg =:= d1 orelse Reg =:= d2 orelse Reg =:= d3 orelse Reg =:= d4 orelse
        Reg =:= d5 orelse Reg =:= d6 orelse Reg =:= d7)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    available_regs :: [aarch64_register()],
    available_fpregs :: [aarch64_register()],
    used_regs :: [aarch64_register()]
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, aarch64_register()}.
-type value() :: immediate() | vm_register() | aarch64_register() | {ptr, aarch64_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()}.

-type maybe_free_aarch64_register() ::
    {free, aarch64_register()} | aarch64_register().

-type condition() ::
    {aarch64_register(), '<', 0}
    | {maybe_free_aarch64_register(), '==', 0}
    | {maybe_free_aarch64_register(), '!=', integer()}
    | {'(uint8_t)', maybe_free_aarch64_register(), '==', false}
    | {'(uint8_t)', maybe_free_aarch64_register(), '!=', false}
    | {maybe_free_aarch64_register(), '&', non_neg_integer(), '!=', 0}.

% ctx->e is 0x28
% ctx->x is 0x30
-define(CTX_REG, r0).
-define(JITSTATE_REG, r1).
-define(NATIVE_INTERFACE_REG, r2).
-define(Y_REGS, {?CTX_REG, 16#28}).
-define(X_REG(N), {?CTX_REG, 16#30 + (N * 8)}).
-define(CP, {?CTX_REG, 16#B8}).
-define(FP_REGS, {?CTX_REG, 16#C0}).
-define(BS, {?CTX_REG, 16#C8}).
-define(BS_OFFSET, {?CTX_REG, 16#D0}).
-define(JITSTATE_MODULE, {?JITSTATE_REG, 0}).
-define(JITSTATE_CONTINUATION, {?JITSTATE_REG, 16#8}).
-define(JITSTATE_REDUCTIONCOUNT, {?JITSTATE_REG, 16#10}).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * 8}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).

% aarch64 ABI specific
-define(LR_REG, r30).
-define(IP0_REG, r16).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(AVAILABLE_REGS, [r7, r8, r9, r10, r11, r12, r13, r14, r15, r3, r4, r5, r6]).
-define(AVAILABLE_FPREGS, [d0, d1, d2, d3, d4, d5, d6, d7]).
-define(PARAMETER_REGS, [r0, r1, r2, r3, r4, r5]).
-define(PARAMETER_FPREGS, [d0, d1, d2, d3, d4, d5]).

%%-----------------------------------------------------------------------------
%% @doc Return the word size in bytes, i.e. the sizeof(term) i.e.
%% sizeof(uintptr_t)
%%
%% C code equivalent is:
%% #if UINTPTR_MAX == UINT32_MAX
%%    #define TERM_BYTES 4
%% #elif UINTPTR_MAX == UINT64_MAX
%%    #define TERM_BYTES 8
%% #else
%%    #error "Term size must be either 32 bit or 64 bit."
%% #endif
%%
%% @end
%% @return Word size in bytes
%%-----------------------------------------------------------------------------
-spec word_size() -> 4 | 8.
word_size() -> 8.

%%-----------------------------------------------------------------------------
%% @doc Create a new backend state for provided variant, module and stream.
%% @end
%% @param Variant JIT variant to use (currently ?JIT_VARIANT_PIC)
%% @param StreamModule module to stream instructions
%% @param Stream stream state
%% @return New backend state
%%-----------------------------------------------------------------------------
-spec new(any(), module(), stream()) -> state().
new(_Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS,
        available_fpregs = ?AVAILABLE_FPREGS,
        used_regs = []
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
%% @doc Emit a debugger of breakpoint instruction. This is used for debugging
%% and not in production.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec debugger(state()) -> state().
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:brk(0)),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native registers. This is used for
%% debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of used registers
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [aarch64_register()].
used_regs(#state{used_regs = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch registers. This
%% is used for debugging and not in production.
%% @end
%% @param State current backend state
%% @return The list of available registers
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [aarch64_register()].
available_regs(#state{available_regs = Available}) -> Available.

%%-----------------------------------------------------------------------------
%% @doc Free native registers. The passed list of registers can contain
%% registers, pointer to registers or other values that are ignored.
%% @end
%% @param State current backend state
%% @param Regs list of registers or other values
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Reg | Rest]) ->
    State1 = free_native_register(State, Reg),
    free_native_registers(State1, Rest).

-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_regs = Available0, available_fpregs = AvailableFP0, used_regs = Used0} = State,
    Reg
) when
    is_atom(Reg)
->
    {Available1, AvailableFP1, Used1} = free_reg(Available0, AvailableFP0, Used0, Reg),
    State#state{available_regs = Available1, available_fpregs = AvailableFP1, used_regs = Used1};
free_native_register(State, {ptr, Reg}) ->
    free_native_register(State, Reg);
free_native_register(State, _Other) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Assert that all native scratch registers are available. This is used
%% for debugging and not in production.
%% @end
%% @param State current backend state
%% @return ok
%%-----------------------------------------------------------------------------
-spec assert_all_native_free(state()) -> ok.
assert_all_native_free(#state{
    available_regs = ?AVAILABLE_REGS, available_fpregs = ?AVAILABLE_FPREGS, used_regs = []
}) ->
    ok.

%%-----------------------------------------------------------------------------
%% @doc Emit the jump table at the beginning of the module. Branches will be
%% updated afterwards with update_branches/2. Emit branches for labels from
%% 0 (special entry for lines and labels information) to LabelsCount included
%% (special entry for OP_INT_CALL_END).
%% @end
%% @param State current backend state
%% @param LabelsCount number of labels in the module.
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_table(state(), pos_integer()) -> state().
jump_table(State, LabelsCount) ->
    jump_table0(State, 0, LabelsCount).

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State,
    N,
    LabelsCount
) ->
    Offset = StreamModule:offset(Stream0),
    BranchInstr = jit_aarch64_asm:b(0),
    Reloc = {N, Offset, b},
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    jump_table0(State#state{stream = Stream1, branches = [Reloc | Branches]}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Rewrite stream to update all branches for labels.
%% @end
%% @param State current backend state
%% @param Labels list of tuples with label, offset and size of the branch in bits
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec update_branches(state(), [{non_neg_integer(), non_neg_integer()}]) -> state().
update_branches(#state{branches = []} = State, _Labels) ->
    State;
update_branches(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = [{Label, Offset, Type} | BranchesT]
    } = State,
    Labels
) ->
    {Label, LabelOffset} = lists:keyfind(Label, 1, Labels),
    Rel = LabelOffset - Offset,
    NewInstr =
        case Type of
            {bcc, CC} -> jit_aarch64_asm:bcc(CC, Rel);
            {adr, Reg} -> jit_aarch64_asm:adr(Reg, Rel);
            b -> jit_aarch64_asm:b(Rel)
        end,
    Stream1 = StreamModule:replace(Stream0, Offset, NewInstr),
    update_branches(State#state{stream = Stream1, branches = BranchesT}, Labels).

%%-----------------------------------------------------------------------------
%% @doc Emit a call (call with return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention. It also saves scratch registers we need to preserve.
%% @end
%% @param State current backend state
%% @param Primitive index to the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), aarch64_register()}.
call_primitive(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    Primitive,
    Args
) ->
    PrepCall =
        case Primitive of
            0 ->
                jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, 0});
            N ->
                jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * 8})
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    StateCall = State#state{stream = Stream1},
    call_func_ptr(StateCall, {free, ?IP0_REG}, Args).

%%-----------------------------------------------------------------------------
%% @doc Emit a jump (call without return) to a primitive with arguments. This
%% function converts arguments and pass them following the backend ABI
%% convention.
%% @end
%% @param State current backend state
%% @param Primitive index to the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
call_primitive_last(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    Primitive,
    Args
) ->
    % We need a register for the function pointer that should not be used as a parameter
    % Since we're not returning, we can use all scratch registers except
    % registers used for parameters
    ParamRegs = lists:sublist(?PARAMETER_REGS, length(Args)),
    ArgsRegs = args_regs(Args),
    ScratchRegs = ?AVAILABLE_REGS -- ArgsRegs -- ParamRegs,
    [Temp | AvailableRegs1] = ScratchRegs,
    UsedRegs = ?AVAILABLE_REGS -- AvailableRegs1,
    PrepCall =
        case Primitive of
            0 ->
                jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, 0});
            N ->
                jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, N * 8})
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    State1 = set_args(
        State0#state{
            stream = Stream1, available_regs = AvailableRegs1, used_regs = UsedRegs
        },
        Args
    ),
    #state{stream = Stream2} = State1,
    Call = jit_aarch64_asm:br(Temp),
    Stream3 = StreamModule:append(Stream2, Call),
    State1#state{stream = Stream3, available_regs = ?AVAILABLE_REGS, used_regs = []}.

%%-----------------------------------------------------------------------------
%% @doc Emit a return of a value if it's not equal to ctx.
%% This logic is used to break out to the scheduler, typically after signal
%% messages have been processed.
%% @end
%% @param State current backend state
%% @param Reg register to compare to (should be {free, Reg} as it's always freed)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
return_if_not_equal_to_ctx(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        available_fpregs = AvailableFPRegs0,
        used_regs = UsedRegs0
    } = State,
    {free, Reg}
) ->
    I1 = jit_aarch64_asm:cmp(Reg, ?CTX_REG),
    I3 =
        case Reg of
            % Return value is already in r0
            r0 -> <<>>;
            % Move to r0 (return register)
            _ -> jit_aarch64_asm:orr(r0, xzr, Reg)
        end,
    I4 = jit_aarch64_asm:ret(),
    I2 = jit_aarch64_asm:bcc(eq, byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg
    ),
    State#state{
        stream = Stream1,
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a jump to a label. The offset of the relocation is saved and will
%% be updated with `update_branches/2`.
%% @end
%% @param State current backend state
%% @param Label to jump to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches} = State, Label
) ->
    Offset = StreamModule:offset(Stream0),
    % Placeholder offset, will be patched
    I1 = jit_aarch64_asm:b(0),
    Reloc = {Label, Offset, b},
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, branches = [Reloc | AccBranches]}.

%%-----------------------------------------------------------------------------
%% @doc Emit an if block, i.e. emit a test of a condition and conditionnally
%% execute a block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockFn function to emit the block that may be executed
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec if_block(state(), condition() | {'and', [condition()]}, fun((state()) -> state())) -> state().
if_block(
    #state{stream_module = StreamModule} = State0,
    {'and', CondList},
    BlockFn
) ->
    {Replacements, State1} = lists:foldl(
        fun(Cond, {AccReplacements, AccState}) ->
            Offset = StreamModule:offset(AccState#state.stream),
            {NewAccState, CC, ReplaceDelta} = if_block_cond(AccState, Cond),
            {[{Offset + ReplaceDelta, CC} | AccReplacements], NewAccState}
        end,
        {[], State0},
        CondList
    ),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, CC}, AccStream) ->
            BranchOffset = OffsetAfter - ReplacementOffset,
            NewBranchInstr = jit_aarch64_asm:bcc(CC, BranchOffset),
            StreamModule:replace(AccStream, ReplacementOffset, NewBranchInstr)
        end,
        Stream2,
        Replacements
    ),
    merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs);
if_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = BlockFn(State1),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the conditional branch instruction to jump to the end of the block
    BranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = jit_aarch64_asm:bcc(CC, BranchOffset),
    Stream3 = StreamModule:replace(Stream2, Offset + BranchInstrOffset, NewBranchInstr),
    merge_used_regs(State2#state{stream = Stream3}, State1#state.used_regs).

%%-----------------------------------------------------------------------------
%% @doc Emit an if else block, i.e. emit a test of a condition and
%% conditionnally execute a block or another block.
%% @end
%% @param State current backend state
%% @param Cond condition to test
%% @param BlockTrueFn function to emit the block that is executed if condition is true
%% @param BlockFalseFn function to emit the block that is executed if condition is false
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec if_else_block(state(), condition(), fun((state()) -> state()), fun((state()) -> state())) ->
    state().
if_else_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockTrueFn,
    BlockFalseFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = BlockTrueFn(State1),
    Stream2 = State2#state.stream,
    %% Emit unconditional branch to skip the else block (will be replaced)
    ElseJumpOffset = StreamModule:offset(Stream2),
    ElseJumpInstr = jit_aarch64_asm:b(0),
    Stream3 = StreamModule:append(Stream2, ElseJumpInstr),
    %% Else block starts here.
    OffsetAfter = StreamModule:offset(Stream3),
    %% Patch the conditional branch to jump to the else block
    ElseBranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = jit_aarch64_asm:bcc(CC, ElseBranchOffset),
    Stream4 = StreamModule:replace(Stream3, Offset + BranchInstrOffset, NewBranchInstr),
    %% Build the else block
    StateElse = State2#state{
        stream = Stream4,
        used_regs = State1#state.used_regs,
        available_regs = State1#state.available_regs,
        available_fpregs = State1#state.available_fpregs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional branch to jump to the end
    FinalJumpOffset = OffsetFinal - ElseJumpOffset,
    NewElseJumpInstr = jit_aarch64_asm:b(FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    merge_used_regs(State3#state{stream = Stream6}, State2#state.used_regs).

-spec if_block_cond(state(), condition()) -> {state(), jit_aarch64_asm:cc(), non_neg_integer()}.
if_block_cond(#state{stream_module = StreamModule, stream = Stream0} = State0, {Reg, '<', 0}) ->
    I1 = jit_aarch64_asm:tst(Reg, Reg),
    % pl = positive or zero (>=0)
    I2 = jit_aarch64_asm:bcc(pl, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, pl, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegA, '<', RegB}
) when ?IS_GPR(RegA) ->
    I1 = jit_aarch64_asm:cmp(RegA, RegB),
    % ge = greater than or equal
    I2 = jit_aarch64_asm:bcc(ge, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, ge, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:tst(Reg, Reg),
    % ne = not equal
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {'(int)', RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:tst32(Reg, Reg),
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 =
        case Val of
            V when is_integer(V) -> jit_aarch64_asm:cmp(Reg, V);
            V when is_atom(V) -> jit_aarch64_asm:cmp(Reg, V)
        end,
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 =
        case Val of
            V when is_integer(V) -> jit_aarch64_asm:cmp32(Reg, V);
            V when is_atom(V) -> jit_aarch64_asm:cmp32(Reg, V)
        end,
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 =
        case Val of
            V when is_integer(V) -> jit_aarch64_asm:cmp(Reg, V);
            V when is_atom(V) -> jit_aarch64_asm:cmp(Reg, V)
        end,
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 =
        case Val of
            V when is_integer(V) -> jit_aarch64_asm:cmp32(Reg, V);
            V when is_atom(V) -> jit_aarch64_asm:cmp32(Reg, V)
        end,
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(uint8_t)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test low 8 bits
    I1 = jit_aarch64_asm:tst32(Reg, 16#FF),
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(uint8_t)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test low 8 bits
    I1 = jit_aarch64_asm:tst32(Reg, 16#FF),
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    I1 = jit_aarch64_asm:and_(Temp, Reg, Mask),
    % Compare with value
    I2 = jit_aarch64_asm:cmp(Temp, Val),
    I3 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1},
    {State1, eq, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    I1 = jit_aarch64_asm:and_(Reg, Reg, Mask),
    % Compare with value
    I2 = jit_aarch64_asm:cmp(Reg, Val),
    I3 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary,
        I3/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1) + byte_size(I2)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {RegOrTuple, '&', Val}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test bits
    I1 = jit_aarch64_asm:tst(Reg, Val),
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {'(uint8_t)', RegOrTuple, '&', Val}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test 8-bit value
    I1 = jit_aarch64_asm:tst32(Reg, Val),
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, eq, byte_size(I1)}.

-spec if_block_free_reg(aarch64_register() | {free, aarch64_register()}, state()) -> state().
if_block_free_reg({free, Reg}, State0) ->
    #state{available_regs = AvR0, available_fpregs = AvFR0, used_regs = UR0} = State0,
    {AvR1, AvFR1, UR1} = free_reg(AvR0, AvFR0, UR0, Reg),
    State0#state{
        available_regs = AvR1,
        available_fpregs = AvFR1,
        used_regs = UR1
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

-spec merge_used_regs(state(), [aarch64_register()]) -> state().
merge_used_regs(#state{used_regs = UR0, available_regs = AvR0, available_fpregs = AvFR0} = State, [
    Reg | T
]) ->
    case lists:member(Reg, UR0) of
        true ->
            merge_used_regs(State, T);
        false ->
            AvR1 = lists:delete(Reg, AvR0),
            AvFR1 = lists:delete(Reg, AvFR0),
            UR1 = [Reg | UR0],
            merge_used_regs(
                State#state{used_regs = UR1, available_regs = AvR1, available_fpregs = AvFR1}, T
            )
    end;
merge_used_regs(State, []) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register right by a fixed number of bits, effectively
%% dividing it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
shift_right(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_aarch64_asm:lsr(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register left by a fixed number of bits, effectively
%% multiplying it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
shift_left(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Shift) when
    is_atom(Reg)
->
    I = jit_aarch64_asm:lsl(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a function pointer with arguments. This function converts
%% arguments and passes them following the backend ABI convention.
%% @end
%% @param State current backend state
%% @param FuncPtrTuple either {free, Reg} or {primitive, PrimitiveIndex}
%% @param Args arguments to pass to the function
%% @return Updated backend state and return register
%%-----------------------------------------------------------------------------
-spec call_func_ptr(state(), {free, aarch64_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), aarch64_register()}.
call_func_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        available_fpregs = AvailableFP0,
        used_regs = UsedRegs0
    } = State0,
    FuncPtrTuple,
    Args
) ->
    FreeRegs = lists:flatmap(
        fun
            ({free, ?IP0_REG}) -> [];
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    UsedRegs1 = UsedRegs0 -- FreeRegs,
    SavedRegs = [?LR_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | UsedRegs1],
    {SavedRegsOdd, Stream1} = push_registers(SavedRegs, StreamModule, Stream0),

    % Set up arguments following AArch64 calling convention
    State1 = set_args(State0#state{stream = Stream1}, Args),
    #state{stream = Stream2} = State1,

    {FuncPtrReg, Stream3} =
        case FuncPtrTuple of
            {free, Reg} ->
                {Reg, Stream2};
            {primitive, Primitive} ->
                % We use r16 for the address.
                PrepCall =
                    case Primitive of
                        0 ->
                            jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, 0});
                        N ->
                            jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * 8})
                    end,
                {?IP0_REG, StreamModule:append(Stream2, PrepCall)}
        end,

    % Call the function pointer (using BLR for call with return)
    Call = jit_aarch64_asm:blr(FuncPtrReg),
    Stream4 = StreamModule:append(Stream3, Call),

    % If r0 is in used regs, save it to another temporary register
    AvailableRegs1 = FreeRegs ++ AvailableRegs0,
    {Stream5, ResultReg} =
        case lists:member(r0, SavedRegs) of
            true ->
                [Temp | _] = AvailableRegs1,
                {StreamModule:append(Stream4, jit_aarch64_asm:mov(Temp, r0)), Temp};
            false ->
                {Stream4, r0}
        end,

    Stream6 = pop_registers(SavedRegsOdd, lists:reverse(SavedRegs), StreamModule, Stream5),

    AvailableRegs2 = lists:delete(ResultReg, AvailableRegs1),
    AvailableRegs3 = ?AVAILABLE_REGS -- (?AVAILABLE_REGS -- AvailableRegs2),
    AvailableFP1 = FreeRegs ++ AvailableFP0,
    AvailableFP2 = lists:delete(ResultReg, AvailableFP1),
    AvailableFP3 = ?AVAILABLE_FPREGS -- (?AVAILABLE_FPREGS -- AvailableFP2),
    UsedRegs2 = [ResultReg | UsedRegs1],
    {
        State1#state{
            stream = Stream6,
            available_regs = AvailableRegs3,
            available_fpregs = AvailableFP3,
            used_regs = UsedRegs2
        },
        ResultReg
    }.

push_registers([RegA, RegB | Tail], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:stp(RegA, RegB, {sp, -16}, '!')),
    push_registers(Tail, StreamModule, Stream1);
push_registers([], _StreamModule, Stream0) ->
    {false, Stream0};
push_registers([RegA], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:str(RegA, {sp, -16}, '!')),
    {true, Stream1}.

pop_registers(true, [Reg | Tail], StreamModule, Stream0) ->
    % Odd number of registers, pop the last one first
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:ldr(Reg, {sp}, 16)),
    pop_registers(false, Tail, StreamModule, Stream1);
pop_registers(false, [], _StreamModule, Stream0) ->
    Stream0;
pop_registers(false, [RegB, RegA | Tail], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:ldp(RegA, RegB, {sp}, 16)),
    pop_registers(false, Tail, StreamModule, Stream1).

-spec set_args(state(), [arg()]) -> state().
set_args(
    #state{stream = Stream0, stream_module = StreamModule, used_regs = UsedRegs} = State0, Args
) ->
    ParamRegs = parameter_regs(Args),
    ArgsRegs = args_regs(Args),
    AvailableScratchGP =
        [rdi, rsi, rdx, rcx, r8, r9, r10, r11] -- ParamRegs -- ArgsRegs -- UsedRegs,
    AvailableScratchFP = ?AVAILABLE_FPREGS -- ParamRegs -- ArgsRegs -- UsedRegs,
    Offset = StreamModule:offset(Stream0),
    Args1 = [
        case Arg of
            offset -> Offset;
            _ -> Arg
        end
     || Arg <- Args
    ],
    SetArgsCode = set_args0(Args1, ArgsRegs, ParamRegs, AvailableScratchGP, AvailableScratchFP, []),
    Stream1 = StreamModule:append(Stream0, SetArgsCode),
    NewUsedRegs = lists:foldl(
        fun
            ({free, {ptr, Reg}}, AccUsed) -> lists:delete(Reg, AccUsed);
            ({free, Reg}, AccUsed) -> lists:delete(Reg, AccUsed);
            (_, AccUsed) -> AccUsed
        end,
        UsedRegs,
        Args
    ),
    State0#state{
        stream = Stream1,
        available_regs = ?AVAILABLE_REGS -- ParamRegs -- NewUsedRegs,
        available_fpregs = ?AVAILABLE_FPREGS -- ParamRegs -- NewUsedRegs,
        used_regs = ParamRegs ++ (NewUsedRegs -- ParamRegs)
    }.

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, ?PARAMETER_FPREGS, []).

parameter_regs0([], _, _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([Special | T], [GPReg | GPRegsT], FPRegs, Acc) when
    Special =:= ctx orelse Special =:= jit_state orelse Special =:= offset
->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([{free, Free} | T], GPRegs, FPRegs, Acc) ->
    parameter_regs0([Free | T], GPRegs, FPRegs, Acc);
parameter_regs0([{ptr, Reg} | T], [GPReg | GPRegsT], FPRegs, Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([Reg | T], [GPReg | GPRegsT], FPRegs, Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([Reg | T], GPRegs, [FPReg | FPRegsT], Acc) when ?IS_FPR(Reg) ->
    parameter_regs0(T, GPRegs, FPRegsT, [FPReg | Acc]);
parameter_regs0([{x_reg, _} | T], [GPReg | GPRegsT], FPRegs, Acc) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([{y_reg, _} | T], [GPReg | GPRegsT], FPRegs, Acc) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]);
parameter_regs0([{fp_reg, _} | T], GPRegs, [FPReg | FPRegsT], Acc) ->
    parameter_regs0(T, GPRegs, FPRegsT, [FPReg | Acc]);
parameter_regs0([Int | T], [GPReg | GPRegsT], FPRegs, Acc) when is_integer(Int) ->
    parameter_regs0(T, GPRegsT, FPRegs, [GPReg | Acc]).

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

set_args0([], [], [], _AvailGP, _AvailFP, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc) ->
    set_args0([FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc);
set_args0([ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, AvailFP, Acc) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc);
set_args0(
    [jit_state | ArgsT],
    [?JITSTATE_REG | ArgsRegs],
    [?JITSTATE_REG | ParamRegs],
    AvailGP,
    AvailFP,
    Acc
) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, Acc);
set_args0(
    [jit_state | ArgsT], [?JITSTATE_REG | ArgsRegs], [ParamReg | ParamRegs], AvailGP, AvailFP, Acc
) ->
    false = lists:member(ParamReg, ArgsRegs),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, [
        jit_aarch64_asm:mov(ParamReg, ?JITSTATE_REG) | Acc
    ]);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0([Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, AvailFP, Acc) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    J = set_args1(Arg, ?CTX_REG),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, [J | Acc]);
set_args0(
    [Arg | ArgsT],
    [_ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    [Avail | AvailGPT] = AvailGP,
    AvailFP,
    Acc
) ->
    J = set_args1(Arg, ParamReg),
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, AvailFP, [J | Acc]);
        true ->
            I = jit_aarch64_asm:mov(Avail, ParamReg),
            NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
            set_args0(NewArgsT, ArgsRegs, ParamRegs, AvailGPT, AvailFP, [J, I | Acc])
    end.

set_args1(Reg, Reg) ->
    [];
set_args1({x_reg, extra}, Reg) ->
    jit_aarch64_asm:ldr(Reg, ?X_REG(?MAX_REG));
set_args1({x_reg, X}, Reg) ->
    jit_aarch64_asm:ldr(Reg, ?X_REG(X));
set_args1({ptr, Source}, Reg) ->
    jit_aarch64_asm:ldr(Reg, {Source, 0});
set_args1({y_reg, X}, Reg) ->
    [
        jit_aarch64_asm:mov(Reg, ?Y_REGS),
        jit_aarch64_asm:mov(Reg, {X * 8, Reg})
    ];
set_args1(ArgReg, Reg) when ?IS_GPR(ArgReg) ->
    jit_aarch64_asm:mov(Reg, ArgReg);
set_args1(Arg, Reg) when is_integer(Arg) andalso Arg >= -16#80000000 andalso Arg < 16#80000000 ->
    jit_aarch64_asm:mov(Reg, Arg);
set_args1(Arg, Reg) when is_integer(Arg) ->
    %% For large immediates, we need a more complex sequence in AArch64
    %% For now, just use the immediate (may need expansion later)
    jit_aarch64_asm:mov(Reg, Arg).

%%-----------------------------------------------------------------------------
%% @doc Emit a move to a vm register (x_reg, y_reg, fpreg or a pointer on x_reg)
%% from an immediate, a native register or another vm register.
%% @end
%% @param State current backend state
%% @param Src value to move to vm register
%% @param Dest vm register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_vm_register(state(), Src :: value() | vm_register(), Dest :: vm_register()) ->
    state().
% Native register to VM register
move_to_vm_register(State0, Src, {x_reg, extra}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {x_reg, X}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register(#state{available_regs = [Temp | _]} = State0, Src, {y_reg, Y}) when
    is_atom(Src)
->
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:str(Src, {Temp, Y * 8}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1};
% Source is an integer
move_to_vm_register(State, 0, Dest) ->
    move_to_vm_register(State, xzr, Dest);
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, N, Dest) when
    is_integer(N)
->
    I1 = jit_aarch64_asm:mov(Temp, N),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
% Source is a VM register
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, extra}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {x_reg, X}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {ptr, Reg}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(#state{available_regs = [Temp | AT] = AR0} = State0, {y_reg, Y}, Dest) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp, {Temp, Y * 8}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    State1 = move_to_vm_register(State0#state{stream = Stream1, available_regs = AT}, Temp, Dest),
    State1#state{available_regs = AR0};
move_to_vm_register(
    #state{stream_module = StreamModule, available_regs = [Temp | _], stream = Stream0} = State,
    Reg,
    {fp_reg, F}
) when is_atom(Reg) ->
    I1 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I2 = jit_aarch64_asm:str(Reg, {Temp, F * 8}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of an array element (reg[x]) to a vm or a native register.
%% @end
%% @param State current backend state
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @param Dest vm or native register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_array_element(
    state(),
    aarch64_register(),
    non_neg_integer() | aarch64_register(),
    vm_register() | aarch64_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Reg,
    Index,
    {ptr, Dest}
) when is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, {0, Dest}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp1, Temp2 | _]} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Temp1, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp2, {Reg, Index * 8}),
    I3 = jit_aarch64_asm:str(Temp2, {Temp1, Y * 8}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq({Index * 8, Reg}, Reg),
    I3 = jit_x86_64_asm_unimplemented:movq(Reg, {Y * 8, Temp}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Dest, {Reg, Index * 8}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = AvailableRegs0,
        used_regs = UsedRegs0,
        available_fpregs = AvailableFPRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    I1 = jit_x86_64_asm_unimplemented:shlq(3, IndexReg),
    I2 = jit_x86_64_asm_unimplemented:addq(Reg, IndexReg),
    I3 = jit_x86_64_asm_unimplemented:movq({0, IndexReg}, IndexReg),
    I4 = jit_x86_64_asm_unimplemented:movq(IndexReg, ?X_REG(X)),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, IndexReg
    ),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    State#state{
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _] = AvailableRegs0,
        used_regs = UsedRegs0,
        available_fpregs = AvailableFPRegs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:shlq(3, IndexReg),
    I3 = jit_x86_64_asm_unimplemented:addq(Reg, IndexReg),
    I4 = jit_x86_64_asm_unimplemented:movq({0, IndexReg}, IndexReg),
    I5 = jit_x86_64_asm_unimplemented:movq(IndexReg, {Y * 8, Temp}),
    {AvailableRegs1, AvailableFPRegs1, UsedRegs1} = free_reg(
        AvailableRegs0, AvailableFPRegs0, UsedRegs0, IndexReg
    ),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>
    ),
    State#state{
        available_regs = AvailableRegs1,
        available_fpregs = AvailableFPRegs1,
        used_regs = UsedRegs1,
        stream = Stream1
    }.

%% @doc move reg[x] to a vm or native register
-spec get_array_element(state(), aarch64_register(), non_neg_integer()) ->
    {state(), aarch64_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [ElemReg | AvailableT],
        used_regs = UsedRegs0
    } = State,
    Reg,
    Index
) ->
    I1 = jit_x86_64_asm_unimplemented:movq({Index * 8, Reg}, ElemReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    {
        State#state{
            stream = Stream1, available_regs = AvailableT, used_regs = [ElemReg | UsedRegs0]
        },
        ElemReg
    }.

%% @doc move an integer, a vm or native register to reg[x]
-spec move_to_array_element(
    state(), integer() | vm_register() | aarch64_register(), aarch64_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    Reg,
    Index
) when X < ?MAX_REG andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    Reg,
    IndexReg
) when X < ?MAX_REG andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, {0, Reg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {ptr, Source},
    Reg,
    Index
) ->
    I1 = jit_x86_64_asm_unimplemented:movq({0, Source}, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {y_reg, Y},
    Reg,
    Index
) when ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm_unimplemented:movq(Temp, {Index * 8, Reg}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} =
        State,
    {y_reg, Y},
    Reg,
    IndexReg
) when ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm_unimplemented:movq(Temp, {0, Reg, IndexReg, 8}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_GPR(Source) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_SINT32_T(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    Source,
    Reg,
    Index
) when is_integer(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm_unimplemented:movabsq(Source, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1}.

move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {x_reg, X},
    BaseReg,
    IndexReg,
    Offset
) when X < ?MAX_REG andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State,
    {y_reg, Y},
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:movq({Y * 8, Temp}, Temp),
    I3 = jit_x86_64_asm_unimplemented:movq(Temp, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_GPR(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset)
->
    I1 = jit_x86_64_asm_unimplemented:movq(Source, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_SINT32_T(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso
        is_integer(Offset)
->
    I1 = jit_x86_64_asm_unimplemented:movq(Source, {Offset, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1 / binary),
    State#state{stream = Stream1};
move_to_array_element(
    State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when is_integer(IndexReg) andalso is_integer(Offset) andalso Offset div 8 =:= 0 ->
    move_to_array_element(State, Source, BaseReg, IndexReg + (Offset div 8)).

-spec move_to_native_register(state(), value()) -> {state(), aarch64_register()}.
move_to_native_register(State, Reg) when is_atom(Reg) ->
    {State, Reg};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm_unimplemented:movq({0, Reg}, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    Imm
) when
    is_integer(Imm)
->
    I1 = jit_x86_64_asm_unimplemented:movq(Imm, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, used_regs = [Reg | Used], available_regs = AvailT}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    {x_reg, X}
) when
    X < ?MAX_REG
->
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, used_regs = [Reg | Used], available_regs = AvailT}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailT],
        used_regs = Used
    } = State,
    {y_reg, Y}
) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, Reg),
    I2 = jit_x86_64_asm_unimplemented:movq({Y * 8, Reg}, Reg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [Reg | Used]}, Reg};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        available_fpregs = [FPReg | AvailFT],
        used_regs = Used
    } = State,
    {fp_reg, F}
) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?FP_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:movsd({F * 8, Temp}, FPReg),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, available_fpregs = AvailFT, used_regs = [FPReg | Used]}, FPReg}.

-spec move_to_native_register(state(), value(), aarch64_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, RegSrc, RegDst
) when is_atom(RegSrc) orelse is_integer(RegSrc) ->
    I = jit_x86_64_asm_unimplemented:movq(RegSrc, RegDst),
    Stream1 = StreamModule:append(Stream0, I),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {ptr, Reg}, RegDst
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm_unimplemented:movq({0, Reg}, RegDst),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    I1 = jit_x86_64_asm_unimplemented:movq(?X_REG(X), RegDst),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0} = State, {y_reg, Y}, RegDst
) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?Y_REGS, RegDst),
    I2 = jit_x86_64_asm_unimplemented:movq({Y * 8, RegDst}, RegDst),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1};
move_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _]
    } = State,
    {fp_reg, F},
    RegDst
) ->
    I1 = jit_x86_64_asm_unimplemented:movq(?FP_REGS, Temp),
    I2 = jit_x86_64_asm_unimplemented:movsd({F * 8, Temp}, RegDst),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

-spec copy_to_native_register(state(), value()) -> {state(), aarch64_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    Reg
) when is_atom(Reg) ->
    I1 = jit_aarch64_asm:mov(SaveReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [SaveReg | AvailT],
        used_regs = Used
    } = State,
    {ptr, Reg}
) when is_atom(Reg) ->
    I1 = jit_x86_64_asm_unimplemented:movq({0, Reg}, SaveReg),
    Stream1 = StreamModule:append(Stream0, I1),
    {State#state{stream = Stream1, available_regs = AvailT, used_regs = [SaveReg | Used]}, SaveReg};
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    {y_reg, Y}
) ->
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Y * 8}),
    I3 = jit_aarch64_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Reg | _]} = State,
    Offset
) ->
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:add(Reg, Reg, Offset * 8),
    I3 = jit_aarch64_asm:str(Reg, ?Y_REGS),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        branches = Branches
    } = State,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    I1 = jit_aarch64_asm:adr(Temp, 0),
    Reloc = {Label, Offset, {adr, Temp}},
    I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, branches = [Reloc | Branches]}.

set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Temp | _],
        branches = Branches
    } = State
) ->
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    {RewriteLEAOffset, I1} = jit_x86_64_asm_unimplemented:leaq_rel32({-4, rip}, Temp),
    Reloc = {OffsetRef, Offset + RewriteLEAOffset, 32},
    I2 = jit_x86_64_asm_unimplemented:movq(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {State#state{stream = Stream1, branches = [Reloc | Branches]}, OffsetRef}.

get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        available_regs = [Reg | AvailableT],
        used_regs = UsedRegs0
    } = State
) ->
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_MODULE),
    I2 = jit_aarch64_asm:ldr_w(Reg, ?MODULE_INDEX(Reg)),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    {
        State#state{stream = Stream1, available_regs = AvailableT, used_regs = [Reg | UsedRegs0]},
        Reg
    }.

and_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_aarch64_asm:and_(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

or_(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm_unimplemented:orq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

add(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm_unimplemented:addq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

sub(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm_unimplemented:subq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(#state{stream_module = StreamModule, stream = Stream0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm_unimplemented:imulq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, available_regs = [Temp | _]} = State0
) ->
    % Load reduction count
    I1 = jit_aarch64_asm:ldr(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str(Temp, ?JITSTATE_REDUCTIONCOUNT),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch if reduction count is not zero
    I4 = jit_aarch64_asm:bcc(ne, 0),
    % Set continuation to the next instruction
    ADROffset = BNEOffset + byte_size(I4),
    I5 = jit_aarch64_asm:adr(Temp, 0),
    I6 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    % Append the instructions to the stream
    Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary, I6/binary>>),
    State1 = State0#state{stream = Stream2},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite the branch and adr instructions
    #state{stream = Stream3} = State2,
    NewOffset = StreamModule:offset(Stream3),
    NewI4 = jit_aarch64_asm:bcc(ne, NewOffset - BNEOffset),
    NewI5 = jit_aarch64_asm:adr(Temp, NewOffset - ADROffset),
    Stream4 = StreamModule:replace(
        Stream3, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    merge_used_regs(State2#state{stream = Stream4}, State1#state.used_regs).

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset).

call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        available_regs = [Temp | _]
    } = State0,
    Label
) ->
    % Load reduction count
    I1 = jit_aarch64_asm:ldr(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str(Temp, ?JITSTATE_REDUCTIONCOUNT),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),
    % Branch to label if reduction count is not zero
    I4 = jit_aarch64_asm:bcc(ne, 0),
    Reloc1 = {Label, BNEOffset, {bcc, ne}},
    Stream2 = StreamModule:append(Stream1, I4),
    State1 = State0#state{stream = Stream2, branches = [Reloc1 | Branches]},
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset).

-spec set_cp(state()) -> {state(), non_neg_integer()}.
set_cp(State0) ->
    % get module index (dynamically)
    {#state{stream_module = StreamModule, stream = Stream0} = State1, Reg} = get_module_index(
        State0
    ),
    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_aarch64_asm:lsl(Reg, Reg, 24),
    I2 = jit_aarch64_asm:mov(?IP0_REG, 0),
    MOVOffset = Offset + byte_size(I1),
    I3 = jit_aarch64_asm:orr(Reg, Reg, ?IP0_REG),
    I4 = jit_aarch64_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, MOVOffset}.

-spec rewrite_cp_offset(state(), non_neg_integer()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    NewMoveInstr = jit_aarch64_asm:mov(?IP0_REG, NewOffset bsl 2),
    ?ASSERT(byte_size(NewMoveInstr) =:= 4),
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, NewMoveInstr),
    State0#state{stream = Stream1}.

set_bs(#state{stream_module = StreamModule, stream = Stream0} = State0, TermReg) ->
    I1 = jit_x86_64_asm_unimplemented:movq(TermReg, ?BS),
    I2 = jit_x86_64_asm_unimplemented:movq(0, ?BS_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param State current state
%% @param SortedLabels labels information, sorted by offset
%% @param SortedLines line information, sorted by offset
%% @doc Build labels and line tables and encode a function that returns it.
%% In this case, the function returns the effective address of what immediately
%% follows.
%% @end
%% @return New state
%%-----------------------------------------------------------------------------
return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State,
    SortedLabels,
    SortedLines
) ->
    I2 = jit_x86_64_asm_unimplemented:retq(),
    {_RewriteLEAOffset, I1} = jit_x86_64_asm_unimplemented:leaq_rel32({byte_size(I2), rip}, rax),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

free_reg(AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg) when ?IS_GPR(Reg) ->
    AvailableRegs1 = free_reg0(?AVAILABLE_REGS, AvailableRegs0, Reg, []),
    true = lists:member(Reg, UsedRegs0),
    UsedRegs1 = lists:delete(Reg, UsedRegs0),
    {AvailableRegs1, AvailableFPRegs0, UsedRegs1};
free_reg(AvailableRegs0, AvailableFPRegs0, UsedRegs0, Reg) when ?IS_FPR(Reg) ->
    AvailableFPRegs1 = free_reg0(?AVAILABLE_FPREGS, AvailableFPRegs0, Reg, []),
    true = lists:member(Reg, UsedRegs0),
    UsedRegs1 = lists:delete(Reg, UsedRegs0),
    {AvailableRegs0, AvailableFPRegs1, UsedRegs1}.

free_reg0([Reg | _SortedT], PrevRegs0, Reg, Acc) ->
    lists:reverse(Acc, [Reg | PrevRegs0]);
free_reg0([PrevReg | SortedT], [PrevReg | PrevT], Reg, Acc) ->
    free_reg0(SortedT, PrevT, Reg, [PrevReg | Acc]);
free_reg0([_Other | SortedT], PrevRegs, Reg, Acc) ->
    free_reg0(SortedT, PrevRegs, Reg, Acc).

args_regs(Args) ->
    lists:map(
        fun
            ({free, {ptr, Reg}}) -> Reg;
            ({free, Reg}) when is_atom(Reg) -> Reg;
            ({free, Imm}) when is_integer(Imm) -> imm;
            (offset) -> imm;
            (ctx) -> ?CTX_REG;
            (jit_state) -> ?JITSTATE_REG;
            (Reg) when is_atom(Reg) -> Reg;
            (Imm) when is_integer(Imm) -> imm;
            ({ptr, Reg}) -> Reg;
            ({x_reg, _}) -> ?CTX_REG;
            ({y_reg, _}) -> ?CTX_REG;
            ({fp_reg, _}) -> ?CTX_REG;
            ({free, {x_reg, _}}) -> ?CTX_REG;
            ({free, {y_reg, _}}) -> ?CTX_REG;
            ({free, {fp_reg, _}}) -> ?CTX_REG
        end,
        Args
    ).
