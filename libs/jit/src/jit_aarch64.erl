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
    move_imported_gcbif_to_native_register/3,
    and_/3,
    or_/3,
    add/3,
    add_overflow/3,
    sub/3,
    sub_overflow/3,
    mul/3,
    mul_overflow/3,
    div_/3,
    rem_/3,
    supports_div/1,
    supports_fp/1,
    float_op/5,
    float_conv_int/3,
    float_conv_float/3,
    move_float_to_fp_reg/3,
    read_fp_regs_ptr/1,
    shift_right_arith/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3,
    xor_/3,
    set_vm_record_type/3,
    get_vm_record_type/2
]).

-export([dwarf_x_reg_offset/0]).

-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2,
    dwarf_variables/2,
    dwarf_ctx_register/0
]).
-endif.

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

%-define(ASSERT(Expr), true = Expr).
-define(ASSERT(_Expr), ok).

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
%% https://github.com/ARM-software/abi-aa/blob/main/aapcs64/aapcs64.rst
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
    | r15.

-define(IS_GPR(Reg),
    (Reg =:= r0 orelse Reg =:= r1 orelse Reg =:= r2 orelse Reg =:= r3 orelse Reg =:= r4 orelse
        Reg =:= r5 orelse Reg =:= r6 orelse Reg =:= r7 orelse Reg =:= r8 orelse Reg =:= r9 orelse
        Reg =:= r10 orelse Reg =:= r11 orelse Reg =:= r12 orelse Reg =:= r13 orelse Reg =:= r14 orelse
        Reg =:= r15)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: #{integer() | reference() => [{non_neg_integer(), non_neg_integer()}]},
    jump_table_start :: non_neg_integer(),
    labels :: #{integer() | reference() => integer()},
    variant :: non_neg_integer(),
    regs :: jit_regs:regs()
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, aarch64_register()}.
-type value() :: immediate() | vm_register() | aarch64_register() | {ptr, aarch64_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_aarch64_register() ::
    {free, aarch64_register()} | aarch64_register().

-type condition() ::
    {aarch64_register(), '<', integer()}
    | {maybe_free_aarch64_register(), '<', aarch64_register()}
    | {integer(), '<', maybe_free_aarch64_register()}
    | {maybe_free_aarch64_register(), '==', aarch64_register() | integer()}
    | {maybe_free_aarch64_register(), '!=', aarch64_register() | integer()}
    | {'(int)', maybe_free_aarch64_register(), '==', integer()}
    | {'(int)', maybe_free_aarch64_register(), '!=', aarch64_register() | integer()}
    | {'(bool)', maybe_free_aarch64_register(), '==', false}
    | {'(bool)', maybe_free_aarch64_register(), '!=', false}
    | {maybe_free_aarch64_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, aarch64_register()}, '==', {free, aarch64_register()}}.

% ctx->e is 0x50
% ctx->x is 0x58
-define(WORD_SIZE, 8).
-define(CTX_REG, r0).
-define(JITSTATE_REG, r1).
-define(NATIVE_INTERFACE_REG, r2).
-define(Y_REGS, {?CTX_REG, 16#50}).
-define(X_REG(N), {?CTX_REG, 16#58 + (N * ?WORD_SIZE)}).
-define(CP, {?CTX_REG, 16#E0}).
-define(FP_REGS, {?CTX_REG, 16#E8}).
-define(FP_REG_OFFSET(State, F),
    (F *
        case (State)#state.variant band ?JIT_VARIANT_FLOAT32 of
            0 -> 8;
            _ -> 4
        end)
).
-define(BS, {?CTX_REG, 16#F0}).
-define(BS_OFFSET, {?CTX_REG, 16#F8}).
-define(JITSTATE_MODULE, {?JITSTATE_REG, 0}).
-define(JITSTATE_CONTINUATION, {?JITSTATE_REG, 16#8}).
-define(JITSTATE_REDUCTIONCOUNT, {?JITSTATE_REG, 16#10}).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).
% Offsets for inlining the imported-BIF pointer resolution at gc_bif call sites.
% Kept in sync with src/libAtomVM/jit.c via _Static_assert.
-define(MODULE_IMPORTED_FUNCS, 16#90).
-define(CTX_EXTENDED_X_REGS, 16#100).
% struct Bif { struct ExportedFunction base; union { BifImpl0 bif0_ptr; ... }; }
% base is at offset 0, so EXPORTED_FUNCTION_TO_BIF(f) == f and bif0_ptr is here.
-define(BIF_BIF0_PTR, 16#8).

% aarch64 ABI specific
-define(LR_REG, r30).
-define(IP0_REG, r16).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(PARAMETER_REGS, [r0, r1, r2, r3, r4, r5]).

-define(REG_BIT_R0, (1 bsl 0)).
-define(REG_BIT_R1, (1 bsl 1)).
-define(REG_BIT_R2, (1 bsl 2)).
-define(REG_BIT_R3, (1 bsl 3)).
-define(REG_BIT_R4, (1 bsl 4)).
-define(REG_BIT_R5, (1 bsl 5)).
-define(REG_BIT_R6, (1 bsl 6)).
-define(REG_BIT_R7, (1 bsl 7)).
-define(REG_BIT_R8, (1 bsl 8)).
-define(REG_BIT_R9, (1 bsl 9)).
-define(REG_BIT_R10, (1 bsl 10)).
-define(REG_BIT_R11, (1 bsl 11)).
-define(REG_BIT_R12, (1 bsl 12)).
-define(REG_BIT_R13, (1 bsl 13)).
-define(REG_BIT_R14, (1 bsl 14)).
-define(REG_BIT_R15, (1 bsl 15)).
-define(REG_BIT_R16, (1 bsl 16)).
-define(REG_BIT_R17, (1 bsl 17)).

-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_R7 bor ?REG_BIT_R8 bor ?REG_BIT_R9 bor ?REG_BIT_R10 bor ?REG_BIT_R11 bor
        ?REG_BIT_R12 bor ?REG_BIT_R13 bor ?REG_BIT_R14 bor ?REG_BIT_R15 bor
        ?REG_BIT_R3 bor ?REG_BIT_R4 bor ?REG_BIT_R5 bor ?REG_BIT_R6)
).
-define(SCRATCH_REGS_MASK,
    (?REG_BIT_R7 bor ?REG_BIT_R8 bor ?REG_BIT_R9 bor ?REG_BIT_R10 bor ?REG_BIT_R11 bor
        ?REG_BIT_R12 bor ?REG_BIT_R13 bor ?REG_BIT_R14 bor ?REG_BIT_R15 bor
        ?REG_BIT_R3 bor ?REG_BIT_R4 bor ?REG_BIT_R5 bor ?REG_BIT_R6 bor ?REG_BIT_R17)
).

-include("jit_backend_dwarf_impl.hrl").

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
word_size() -> ?WORD_SIZE.

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
        branches = #{},
        jump_table_start = 0,
        offset = StreamModule:offset(Stream),
        labels = #{},
        variant = Variant,
        regs = jit_regs:new(?AVAILABLE_REGS_MASK, 0)
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
%% @doc Flush the current state (unused on aarch64)
%% @end
%% @param State current backend state
%% @return The flushed state
%%-----------------------------------------------------------------------------
-spec flush(state()) -> state().
flush(#state{} = State) ->
    State.

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

%% Native-register allocation bookkeeping (used_regs/1, available_regs/1,
%% free_native_registers/2, free_native_register/2, assert_all_native_free/1,
%% first_avail/1, mask_to_list/1, args_regs/1, prepare_call_scratch/1) is shared
%% across the register-based backends and flows through jit_regs.
-define(FIRST_AVAIL_REGS, [r7, r8, r9, r10, r11, r12, r13, r14, r15, r3, r4, r5, r6]).
-define(MASK_TO_LIST_REGS, ?FIRST_AVAIL_REGS).
-define(JITSTATE_ARG_REG, ?JITSTATE_REG).
-include("jit_backend_regs_impl.hrl").

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
jump_table(#state{stream_module = StreamModule, stream = Stream0} = State, LabelsCount) ->
    JumpTableStart = StreamModule:offset(Stream0),
    jump_table0(State#state{jump_table_start = JumpTableStart}, 0, LabelsCount).

-spec jump_table0(state(), non_neg_integer(), pos_integer()) -> state().
jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    N,
    LabelsCount
) ->
    % Placeholder jumps to next entry
    BranchInstr = jit_aarch64_asm:b(4),
    Stream1 = StreamModule:append(Stream0, BranchInstr),
    jump_table0(State#state{stream = Stream1}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Patch a single branch in the stream
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param Offset offset of the branch to patch
%% @param Type type of the branch
%% @param LabelOffset target label offset
%% @return Updated stream
%%-----------------------------------------------------------------------------
-spec patch_branch(module(), stream(), non_neg_integer(), any(), non_neg_integer()) -> stream().
patch_branch(StreamModule, Stream, Offset, Type, LabelOffset) ->
    Rel = LabelOffset - Offset,
    NewInstr =
        case Type of
            {bcc, CC} -> jit_aarch64_asm:bcc(CC, Rel);
            {adr, Reg} -> adr_far(Reg, Rel);
            b -> jit_aarch64_asm:b(Rel)
        end,
    StreamModule:replace(Stream, Offset, NewInstr).

%% @private
%% @doc Materialize PC + Rel into Reg with a fixed-size two-instruction
%% sequence (adr + add/sub lsl #12), covering ±16MB. Placeholder sites that
%% are patched later must always emit this form so the patch fits in place.
-spec adr_far(jit_aarch64_asm:aarch64_gpr_register(), integer()) -> binary().
adr_far(Reg, Rel) ->
    AdrImm = Rel rem 4096,
    Hi = Rel div 4096,
    Adr = jit_aarch64_asm:adr(Reg, AdrImm),
    Adj =
        if
            Hi >= 0 -> jit_aarch64_asm:add(Reg, Reg, Hi, {lsl, 12});
            true -> jit_aarch64_asm:sub(Reg, Reg, -Hi, {lsl, 12})
        end,
    <<Adr/binary, Adj/binary>>.

%%-----------------------------------------------------------------------------
%% @doc Patch all branches targeting a specific label and return remaining branches
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param TargetLabel label to patch branches for
%% @param LabelOffset offset of the target label
%% @param Branches list of pending branches
%% @return {UpdatedStream, RemainingBranches}
%%-----------------------------------------------------------------------------
-spec patch_branches_for_label(
    module(),
    stream(),
    integer() | reference(),
    non_neg_integer(),
    #{integer() | reference() => [{non_neg_integer(), non_neg_integer()}]}
) ->
    {stream(), #{integer() | reference() => [{non_neg_integer(), non_neg_integer()}]}}.
patch_branches_for_label(StreamModule, Stream, TargetLabel, LabelOffset, Branches) ->
    case Branches of
        #{TargetLabel := BrList} ->
            Stream1 = lists:foldl(
                fun({Offset, Type}, AccStream) ->
                    patch_branch(StreamModule, AccStream, Offset, Type, LabelOffset)
                end,
                Stream,
                BrList
            ),
            {Stream1, maps:remove(TargetLabel, Branches)};
        _ ->
            {Stream, Branches}
    end.

%%-----------------------------------------------------------------------------
%% @doc Rewrite stream to update all branches for labels.
%% @end
%% @param State current backend state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec update_branches(state()) -> state().
update_branches(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels
    } = State
) ->
    Stream1 = maps:fold(
        fun(Label, BrList, AccStream) ->
            #{Label := LabelOffset} = Labels,
            lists:foldl(
                fun({Offset, Type}, AccStream2) ->
                    patch_branch(StreamModule, AccStream2, Offset, Type, LabelOffset)
                end,
                AccStream,
                BrList
            )
        end,
        Stream0,
        Branches
    ),
    State#state{stream = Stream1, branches = #{}}.

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
                jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
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
-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
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
    #{temp := Temp, available_mask := AvailableRegs1, used_mask := UsedRegs} =
        prepare_call_scratch(Args),
    PrepCall =
        case Primitive of
            0 ->
                jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, 0});
            N ->
                jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
        end,
    Stream1 = StreamModule:append(Stream0, PrepCall),
    State1 = set_args(
        State0#state{
            stream = Stream1,
            regs = jit_regs:set_masks(
                jit_regs:invalidate_reg(State0#state.regs, Temp), AvailableRegs1, UsedRegs
            )
        },
        Args
    ),
    #state{stream = Stream2} = State1,
    Call = jit_aarch64_asm:br(Temp),
    Stream3 = StreamModule:append(Stream2, Call),
    State1#state{
        stream = Stream3,
        regs = jit_regs:set_masks(
            jit_regs:unreachable(State1#state.regs), ?AVAILABLE_REGS_MASK, 0
        )
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a return of a value if it's not equal to ctx.
%% This logic is used to break out to the scheduler, typically after signal
%% messages have been processed.
%% @end
%% @param State current backend state
%% @param Reg register to compare to (should be {free, Reg} as it's always freed)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec return_if_not_equal_to_ctx(state(), {free, aarch64_register()}) -> state().
return_if_not_equal_to_ctx(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
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
    I2 = jit_aarch64_asm:bcc(eq, 4 + byte_size(I3) + byte_size(I4)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Bit = reg_bit(Reg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs0, Bit)
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a jump to a label. The offset of the relocation is saved and will
%% be updated with `update_branches/2`.
%% @end
%% @param State current backend state
%% @param Label to jump to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_to_label(state(), integer() | reference()) -> state().
jump_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches, labels = Labels} =
        State,
    Label
) ->
    Offset = StreamModule:offset(Stream0),
    case Labels of
        #{Label := LabelOffset} ->
            % Label is already known, emit direct branch without relocation
            Rel = LabelOffset - Offset,
            I1 = jit_aarch64_asm:b(Rel),
            Stream1 = StreamModule:append(Stream0, I1),
            State#state{stream = Stream1, regs = jit_regs:unreachable(State#state.regs)};
        _ ->
            % Label not yet known, emit placeholder and add relocation
            I1 = jit_aarch64_asm:b(0),
            BrEntry = {Offset, b},
            ExistingBrs = maps:get(Label, AccBranches, []),
            Stream1 = StreamModule:append(Stream0, I1),
            State#state{
                stream = Stream1,
                branches = AccBranches#{Label => [BrEntry | ExistingBrs]},
                regs = jit_regs:unreachable(State#state.regs)
            }
    end.

jump_to_offset(#state{stream_module = StreamModule, stream = Stream0} = State, TargetOffset) ->
    Offset = StreamModule:offset(Stream0),
    Rel = TargetOffset - Offset,
    I1 = jit_aarch64_asm:b(Rel),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, regs = jit_regs:unreachable(State#state.regs)}.

%%-----------------------------------------------------------------------------
%% @doc Jump to a continuation address stored in a register.
%% This is used for optimized intra-module returns.
%% @end
%% @param State current backend state
%% @param OffsetReg register containing the continuation offset
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_continuation(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        offset = BaseOffset,
        regs = Regs0
    } = State,
    {free, OffsetReg}
) ->
    Available = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Available),
    % Calculate absolute address: native_code_base + target_offset
    % where native_code_base = current_pc + (BaseOffset - CurrentStreamOffset)
    CurrentStreamOffset = StreamModule:offset(Stream0),
    NetOffset = BaseOffset - CurrentStreamOffset,

    % Get native code base address into temporary register
    I1 =
        if
            NetOffset >= -1048576 andalso NetOffset =< 1048572 ->
                jit_aarch64_asm:adr(TempReg, NetOffset);
            NetOffset < 0 ->
                % Beyond ADR's ±1MB range: take the current PC, then apply
                % the displacement (sub/add immediates cover up to 16MB)
                Adr = jit_aarch64_asm:adr(TempReg, 0),
                Sub = jit_aarch64_asm:sub(TempReg, TempReg, -NetOffset),
                <<Adr/binary, Sub/binary>>;
            true ->
                Adr = jit_aarch64_asm:adr(TempReg, 0),
                Add = jit_aarch64_asm:add(TempReg, TempReg, NetOffset),
                <<Adr/binary, Add/binary>>
        end,
    % Add target offset to get final absolute address
    I2 = jit_aarch64_asm:add(TempReg, TempReg, OffsetReg),
    % Indirect branch to the calculated absolute address
    I3 = jit_aarch64_asm:br(TempReg),

    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free all registers since this is a tail jump
    State#state{
        stream = Stream1,
        regs = jit_regs:set_masks(
            jit_regs:unreachable(Regs0), ?AVAILABLE_REGS_MASK, 0
        )
    }.

%% @private
-spec rewrite_branch_instruction(
    jit_aarch64_asm:cc() | {tbz | tbnz, atom(), 0..63} | {cbz, atom()}, integer()
) -> binary().
rewrite_branch_instruction({cbnz, Reg}, Offset) ->
    jit_aarch64_asm:cbnz(Reg, Offset);
rewrite_branch_instruction({cbnz_w, Reg}, Offset) ->
    jit_aarch64_asm:cbnz_w(Reg, Offset);
rewrite_branch_instruction({cbz, Reg}, Offset) ->
    jit_aarch64_asm:cbz(Reg, Offset);
rewrite_branch_instruction({cbz_w, Reg}, Offset) ->
    jit_aarch64_asm:cbz_w(Reg, Offset);
rewrite_branch_instruction({tbz, Reg, Bit}, Offset) ->
    jit_aarch64_asm:tbz(Reg, Bit, Offset);
rewrite_branch_instruction({tbnz, Reg, Bit}, Offset) ->
    jit_aarch64_asm:tbnz(Reg, Bit, Offset);
rewrite_branch_instruction(CC, Offset) when is_atom(CC) ->
    jit_aarch64_asm:bcc(CC, Offset).

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
    MergedRegs = jit_regs:merge(
        State1#state.regs, State2#state.regs, ?AVAILABLE_REGS_MASK
    ),
    State2#state{stream = Stream3, regs = MergedRegs};
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
    NewBranchInstr = rewrite_branch_instruction(CC, BranchOffset),
    Stream3 = StreamModule:replace(Stream2, Offset + BranchInstrOffset, NewBranchInstr),
    MergedRegs = jit_regs:merge(
        State1#state.regs, State2#state.regs, ?AVAILABLE_REGS_MASK
    ),
    State2#state{stream = Stream3, regs = MergedRegs}.

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
    NewBranchInstr = rewrite_branch_instruction(CC, ElseBranchOffset),
    Stream4 = StreamModule:replace(Stream3, Offset + BranchInstrOffset, NewBranchInstr),
    %% Build the else block
    StateElse = State2#state{
        stream = Stream4,
        regs = State1#state.regs
    },
    State3 = BlockFalseFn(StateElse),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional branch to jump to the end
    FinalJumpOffset = OffsetFinal - ElseJumpOffset,
    NewElseJumpInstr = jit_aarch64_asm:b(FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    MergedRegs = jit_regs:merge(
        State2#state.regs, State3#state.regs, ?AVAILABLE_REGS_MASK
    ),
    State3#state{stream = Stream6, regs = MergedRegs}.

%% @private
-spec if_block_cond(state(), condition()) ->
    {
        state(),
        jit_aarch64_asm:cc() | {tbz | tbnz, atom(), 0..63} | {cbz, atom()},
        non_neg_integer()
    }.
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '<', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:tbz(Reg, 63, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {tbz, Reg, 63}, 0};
% Handle {Val, '<', Reg} - means Val < Reg, jump if false (i.e., if Val >= Reg or Reg <= Val)
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {Val, '<', RegOrTuple}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    % le = less than or equal
    I2 = jit_aarch64_asm:bcc(le, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, le, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', Val}
) when is_integer(Val), Val =/= 0 ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    % ge = greater than or equal
    I2 = jit_aarch64_asm:bcc(ge, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ge, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '<', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, RegB),
    % ge = greater than or equal
    I2 = jit_aarch64_asm:bcc(ge, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ge, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:cbnz(Reg, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {cbnz, Reg}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0, {'(int)', RegOrTuple, '==', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:cbnz_w(Reg, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {cbnz_w, Reg}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '==', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp_w(Reg, Val),
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
    {RegOrTuple, '!=', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:cbz(Reg, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {cbz, Reg}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
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
    {'(int)', RegOrTuple, '!=', 0}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I = jit_aarch64_asm:cbz_w(Reg, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {cbz_w, Reg}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(int)', RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp_w(Reg, Val),
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
    I1 = jit_aarch64_asm:cmp(Reg, Val),
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
    {{free, Reg1}, '==', {free, Reg2}}
) ->
    % Compare two free registers
    I1 = jit_aarch64_asm:cmp(Reg1, Reg2),
    I2 = jit_aarch64_asm:bcc(ne, 0),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free both registers
    State1 = if_block_free_reg({free, Reg1}, State0),
    State2 = if_block_free_reg({free, Reg2}, State1),
    State3 = State2#state{stream = Stream1},
    {State3, ne, byte_size(I1)};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test lowest bit
    I = jit_aarch64_asm:tbnz(Reg, 0, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {tbnz, Reg, 0}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    % Test lowest bit
    I = jit_aarch64_asm:tbz(Reg, 0, 0),
    Stream1 = StreamModule:append(Stream0, I),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, {tbz, Reg, 0}, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    overflow_set
) ->
    %% Flags set by a preceding adds/subs. Execute the block when V (signed
    %% overflow) is set; branch over it (skip) when overflow is clear.
    I = jit_aarch64_asm:bcc(vc, 0),
    Stream1 = StreamModule:append(Stream0, I),
    {State0#state{stream = Stream1}, vc, 0};
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    mul_overflow_set
) ->
    %% Flags set by a preceding mul_overflow (cmp hi, sign): Z=1 iff the product
    %% fits in a small integer. Execute the block (bignum fallback) when it does
    %% NOT fit; branch over it (skip, eq) when it fits.
    I = jit_aarch64_asm:bcc(eq, 0),
    Stream1 = StreamModule:append(Stream0, I),
    {State0#state{stream = Stream1}, eq, 0};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    {RegOrTuple, '&', Val, '!=', 0}
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    Temp = first_avail(Available),
    % Test bits
    TestCode =
        try
            jit_aarch64_asm:tst(Reg, Val)
        catch
            error:{unencodable_immediate, Val} ->
                TestCode0 = jit_aarch64_asm:mov(Temp, Val),
                TestCode1 = jit_aarch64_asm:tst(Reg, Temp),
                <<TestCode0/binary, TestCode1/binary>>
        end,
    I2 = jit_aarch64_asm:bcc(eq, 0),
    Code = <<
        TestCode/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream1, regs = Regs1},
    {State2, eq, byte_size(TestCode)};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    {Reg, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = op_imm(State0, and_, Temp, Reg, Mask),
    Stream1 = State1#state.stream,
    % Compare with value
    I2 = jit_aarch64_asm:cmp(Temp, Val),
    Stream2 = StreamModule:append(Stream1, I2),
    OffsetAfter = StreamModule:offset(Stream2),
    I3 = jit_aarch64_asm:bcc(eq, 0),
    Stream3 = StreamModule:append(Stream2, I3),
    Regs1b = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream3, regs = Regs1b},
    {State2, eq, OffsetAfter - OffsetBefore};
if_block_cond(
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0,
    {{free, Reg} = RegTuple, '&', Mask, '!=', Val}
) when ?IS_GPR(Reg) ->
    % AND with mask
    OffsetBefore = StreamModule:offset(Stream0),
    {State1, Reg} = and_(State0, RegTuple, Mask),
    Stream1 = State1#state.stream,
    % Compare with value
    I2 = jit_aarch64_asm:cmp(Reg, Val),
    Stream2 = StreamModule:append(Stream1, I2),
    OffsetAfter = StreamModule:offset(Stream2),
    I3 = jit_aarch64_asm:bcc(eq, 0),
    Stream3 = StreamModule:append(Stream2, I3),
    State3 = State1#state{stream = Stream3},
    State4 = if_block_free_reg(RegTuple, State3),
    {State4, eq, OffsetAfter - OffsetBefore}.

%% @private
-spec if_block_free_reg(aarch64_register() | {free, aarch64_register()}, state()) -> state().
if_block_free_reg({free, Reg}, State0) ->
    #state{regs = Regs0} = State0,
    Bit = reg_bit(Reg),
    State0#state{
        regs = jit_regs:free_reg(Regs0, Bit)
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

%% @private

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register right by a fixed number of bits, effectively
%% dividing it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
-spec shift_right(#state{}, maybe_free_aarch64_register(), non_neg_integer()) ->
    {#state{}, aarch64_register()}.
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_aarch64_asm:lsr(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    Available = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Available),
    Bit = reg_bit(ResultReg),
    I = jit_aarch64_asm:lsr(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        ResultReg
    }.

-spec shift_right_arith(#state{}, maybe_free_aarch64_register(), non_neg_integer()) ->
    {#state{}, aarch64_register()}.
shift_right_arith(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_aarch64_asm:asr(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
shift_right_arith(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    Available = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Available),
    Bit = reg_bit(ResultReg),
    I = jit_aarch64_asm:asr(ResultReg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        ResultReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register left by a fixed number of bits, effectively
%% multiplying it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
-spec shift_left(state(), aarch64_register(), non_neg_integer()) -> state().
shift_left(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Shift
) when
    is_atom(Reg)
->
    I = jit_aarch64_asm:lsl(Reg, Reg, Shift),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

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
        regs = Regs0
    } = State0,
    FuncPtrTuple,
    Args
) ->
    AvailableRegs0 = jit_regs:available_regs(Regs0),
    UsedRegs0 = jit_regs:used_regs(Regs0),
    FreeRegs = lists:flatmap(
        fun
            ({free, ?IP0_REG}) -> [];
            ({free, {ptr, Reg}}) -> [Reg];
            ({free, Reg}) when is_atom(Reg) -> [Reg];
            (_) -> []
        end,
        [FuncPtrTuple | Args]
    ),
    FreeMask = jit_regs:regs_to_mask(FreeRegs, fun reg_bit/1),
    UsedRegs1 = UsedRegs0 band (bnot FreeMask),
    SavedRegs = [?LR_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | mask_to_list(UsedRegs1)],
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
                            jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
                    end,
                {?IP0_REG, StreamModule:append(Stream2, PrepCall)}
        end,

    % Call the function pointer (using BLR for call with return)
    Call = jit_aarch64_asm:blr(FuncPtrReg),
    Stream4 = StreamModule:append(Stream3, Call),

    % If r0 is in used regs, save it to another temporary register
    FreeGPMask = FreeMask band ?AVAILABLE_REGS_MASK,
    AvailableRegs1 = AvailableRegs0 bor FreeGPMask,
    {Stream5, ResultReg} =
        case lists:member(r0, SavedRegs) of
            true ->
                Temp = first_avail(AvailableRegs1),
                {StreamModule:append(Stream4, jit_aarch64_asm:mov(Temp, r0)), Temp};
            false ->
                {Stream4, r0}
        end,

    Stream6 = pop_registers(SavedRegsOdd, lists:reverse(SavedRegs), StreamModule, Stream5),

    ResultBit = reg_bit(ResultReg),
    AvailableRegs2 = AvailableRegs1 band (bnot ResultBit),
    AvailableRegs3 = AvailableRegs2 band ?AVAILABLE_REGS_MASK,
    Regs1 = jit_regs:invalidate_all(Regs0),
    UsedRegs2 = UsedRegs1 bor ResultBit,
    {
        State1#state{
            stream = Stream6,
            regs = jit_regs:set_masks(Regs1, AvailableRegs3, UsedRegs2)
        },
        ResultReg
    }.

%% @private
-spec push_registers([aarch64_register()], module(), stream()) -> {boolean(), stream()}.
push_registers([RegA, RegB | Tail], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:stp(RegA, RegB, {sp, -16}, '!')),
    push_registers(Tail, StreamModule, Stream1);
push_registers([], _StreamModule, Stream0) ->
    {false, Stream0};
push_registers([RegA], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:str(RegA, {sp, -16}, '!')),
    {true, Stream1}.

%% @private
-spec pop_registers(boolean(), [aarch64_register()], module(), stream()) -> stream().
pop_registers(true, [Reg | Tail], StreamModule, Stream0) ->
    % Odd number of registers, pop the last one first
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:ldr(Reg, {sp}, 16)),
    pop_registers(false, Tail, StreamModule, Stream1);
pop_registers(false, [], _StreamModule, Stream0) ->
    Stream0;
pop_registers(false, [RegB, RegA | Tail], StreamModule, Stream0) ->
    Stream1 = StreamModule:append(Stream0, jit_aarch64_asm:ldp(RegA, RegB, {sp}, 16)),
    pop_registers(false, Tail, StreamModule, Stream1).

%% @private
-spec set_args(state(), [arg()]) -> state().
set_args(
    #state{stream = Stream0, stream_module = StreamModule, regs = Regs0} = State0, Args
) ->
    UsedRegs = jit_regs:used_regs(Regs0),
    ParamRegs = parameter_regs(Args),
    ArgsRegs = args_regs(Args),
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ArgsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    AvailableScratchMask =
        ?SCRATCH_REGS_MASK band (bnot (ParamMask bor ArgsMask bor UsedRegs)),
    AvailableScratchGP = mask_to_list(AvailableScratchMask),
    Offset = StreamModule:offset(Stream0),
    Args1 = [
        case Arg of
            offset -> Offset;
            _ -> Arg
        end
     || Arg <- Args
    ],
    SetArgsCode = set_args0(Args1, ArgsRegs, ParamRegs, AvailableScratchGP, #{}, []),
    Stream1 = StreamModule:append(Stream0, SetArgsCode),
    NewUsedMask = lists:foldl(
        fun
            ({free, {ptr, Reg}}, AccUsed) -> AccUsed band (bnot reg_bit(Reg));
            ({free, Reg}, AccUsed) when is_atom(Reg) -> AccUsed band (bnot reg_bit(Reg));
            (_, AccUsed) -> AccUsed
        end,
        UsedRegs,
        Args
    ),
    State0#state{
        stream = Stream1,
        regs = jit_regs:set_masks(
            Regs0,
            ?AVAILABLE_REGS_MASK band (bnot (ParamMask bor NewUsedMask)),
            ParamMask bor NewUsedMask
        )
    }.

%% @private
-spec parameter_regs([arg()]) -> [aarch64_register()].
parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

%% @private
-spec parameter_regs0([arg()], [aarch64_register()], [aarch64_register()]) -> [aarch64_register()].
parameter_regs0([], _, Acc) ->
    lists:reverse(Acc);
parameter_regs0([Special | T], [GPReg | GPRegsT], Acc) when
    Special =:= ctx orelse Special =:= jit_state orelse Special =:= offset
->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{free, Free} | T], GPRegs, Acc) ->
    parameter_regs0([Free | T], GPRegs, Acc);
parameter_regs0([{ptr, Reg} | T], [GPReg | GPRegsT], Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([Reg | T], [GPReg | GPRegsT], Acc) when ?IS_GPR(Reg) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{x_reg, _} | T], [GPReg | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{y_reg, _} | T], [GPReg | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([Int | T], [GPReg | GPRegsT], Acc) when is_integer(Int) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]);
parameter_regs0([{avm_int64_t, _} | T], [GPReg | GPRegsT], Acc) ->
    parameter_regs0(T, GPRegsT, [GPReg | Acc]).

%% @private
-spec replace_reg([arg()], aarch64_register(), aarch64_register()) -> [arg()].
replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

%% @private
-spec replace_reg0([arg()], aarch64_register(), aarch64_register(), [arg()]) -> [arg()].
replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

%% @private
-spec set_args0(
    [arg()], [aarch64_register() | imm], [aarch64_register()], [aarch64_register()], map(), [
        binary()
    ]
) -> binary().
set_args0([], [], [], _AvailGP, _LoadedImm, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc) ->
    set_args0([FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
set_args0([ctx | ArgsT], [?CTX_REG | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, LoadedImm, Acc) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
set_args0(
    [jit_state | ArgsT],
    [?JITSTATE_REG | ArgsRegs],
    [?JITSTATE_REG | ParamRegs],
    AvailGP,
    LoadedImm,
    Acc
) ->
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
set_args0(
    [jit_state | ArgsT], [?JITSTATE_REG | ArgsRegs], [ParamReg | ParamRegs], AvailGP, LoadedImm, Acc
) ->
    false = lists:member(ParamReg, ArgsRegs),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [
        jit_aarch64_asm:mov(ParamReg, ?JITSTATE_REG) | Acc
    ]);
% ctx is special as we need it to access x_reg/y_reg/fp_reg
set_args0([Arg | ArgsT], [_ArgReg | ArgsRegs], [?CTX_REG | ParamRegs], AvailGP, LoadedImm, Acc) ->
    false = lists:member(?CTX_REG, ArgsRegs),
    J = set_args1(Arg, ?CTX_REG),
    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [J | Acc]);
set_args0(
    [Arg | ArgsT],
    [_ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    [Avail | AvailGPT] = AvailGP,
    LoadedImm,
    Acc
) ->
    case is_integer(Arg) andalso maps:find(Arg, LoadedImm) of
        {ok, CachedReg} ->
            J = jit_aarch64_asm:mov(ParamReg, CachedReg),
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [J | Acc]);
        _ ->
            J = set_args1(Arg, ParamReg),
            NewLoadedImm =
                case is_integer(Arg) of
                    true -> LoadedImm#{Arg => ParamReg};
                    false -> LoadedImm
                end,
            case lists:member(ParamReg, ArgsRegs) of
                false ->
                    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, NewLoadedImm, [J | Acc]);
                true ->
                    I = jit_aarch64_asm:mov(Avail, ParamReg),
                    NewArgsT = replace_reg(ArgsT, ParamReg, Avail),
                    set_args0(NewArgsT, ArgsRegs, ParamRegs, AvailGPT, NewLoadedImm, [J, I | Acc])
            end
    end.

%% @private
-spec set_args1(arg(), aarch64_register()) -> binary() | [binary()].
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
        jit_aarch64_asm:ldr(Reg, ?Y_REGS),
        jit_aarch64_asm:ldr(Reg, {Reg, X * ?WORD_SIZE})
    ];
set_args1(ArgReg, Reg) when ?IS_GPR(ArgReg) ->
    jit_aarch64_asm:mov(Reg, ArgReg);
set_args1(Arg, Reg) when is_integer(Arg) ->
    jit_aarch64_asm:mov(Reg, Arg);
set_args1({avm_int64_t, Value}, Reg) when is_integer(Value) ->
    jit_aarch64_asm:mov(Reg, Value).

%%-----------------------------------------------------------------------------
%% @doc Emit a move to a vm register (x_reg, y_reg, fpreg or a pointer on x_reg)
%% from an immediate, a native register or another vm register.
%% @end
%% @param State current backend state
%% @param Src value to move to vm register
%% @param Dest vm register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_vm_register
    (state(), Src :: value() | vm_register(), Dest :: vm_register()) -> state();
    (state(), Src :: {free, {ptr, aarch64_register(), 1}}, Dest :: {fp_reg, non_neg_integer()}) ->
        state().
move_to_vm_register(#state{regs = Regs0} = State, Src, Dest) ->
    VmLoc = jit_regs:vm_dest_to_contents(Dest, ?MAX_REG),
    Regs1 =
        case VmLoc of
            unknown -> Regs0;
            _ -> jit_regs:invalidate_vm_loc(Regs0, VmLoc)
        end,
    State1 = move_to_vm_register_emit(State#state{regs = Regs1}, Src, Dest),
    case {Src, VmLoc} of
        {Reg, Contents} when is_atom(Reg), Contents =/= unknown ->
            #state{regs = Regs2} = State1,
            State1#state{regs = jit_regs:set_contents(Regs2, Reg, Contents)};
        _ ->
            State1
    end.

% Native register to VM register
move_to_vm_register_emit(State0, Src, {x_reg, extra}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(?MAX_REG)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {x_reg, X}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, ?X_REG(X)),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(State0, Src, {ptr, Reg}) when is_atom(Src) ->
    I1 = jit_aarch64_asm:str(Src, {Reg, 0}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, I1),
    State0#state{stream = Stream1};
move_to_vm_register_emit(
    #state{regs = Regs0} = State0, Src, {y_reg, Y}
) when
    is_atom(Src)
->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:str(Src, {Temp, Y * ?WORD_SIZE}),
    Stream1 = (State0#state.stream_module):append(State0#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1};
% Source is an integer
move_to_vm_register_emit(State, 0, Dest) ->
    move_to_vm_register_emit(State, xzr, Dest);
move_to_vm_register_emit(#state{regs = Regs0} = State0, N, Dest) when
    is_integer(N)
->
    with_temp(State0, Dest, fun(Temp) ->
        {jit_aarch64_asm:mov(Temp, N), jit_regs:set_contents(Regs0, Temp, {imm, N})}
    end);
% Source is a VM register
move_to_vm_register_emit(#state{regs = Regs0} = State0, {x_reg, extra}, Dest) ->
    with_temp(State0, Dest, fun(Temp) ->
        {
            jit_aarch64_asm:ldr(Temp, ?X_REG(?MAX_REG)),
            jit_regs:set_contents(Regs0, Temp, {x_reg, ?MAX_REG})
        }
    end);
move_to_vm_register_emit(#state{regs = Regs0} = State0, {x_reg, X}, Dest) ->
    with_temp(State0, Dest, fun(Temp) ->
        {jit_aarch64_asm:ldr(Temp, ?X_REG(X)), jit_regs:set_contents(Regs0, Temp, {x_reg, X})}
    end);
move_to_vm_register_emit(#state{regs = Regs0} = State0, {ptr, Reg}, Dest) ->
    with_temp(State0, Dest, fun(Temp) ->
        {jit_aarch64_asm:ldr(Temp, {Reg, 0}), jit_regs:invalidate_reg(Regs0, Temp)}
    end);
move_to_vm_register_emit(#state{regs = Regs0} = State0, {y_reg, Y}, Dest) ->
    with_temp(State0, Dest, fun(Temp) ->
        I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
        I2 = jit_aarch64_asm:ldr(Temp, {Temp, Y * ?WORD_SIZE}),
        {<<I1/binary, I2/binary>>, jit_regs:set_contents(Regs0, Temp, {y_reg, Y})}
    end);
% term_to_float
move_to_vm_register_emit(
    #state{stream_module = StreamModule, regs = Regs0, stream = Stream0} = State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) ->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, ?WORD_SIZE}),
    I2 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I3 = jit_aarch64_asm:str(Reg, {Temp, ?FP_REG_OFFSET(State0, F)}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = free_native_register(State0, Reg),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State1#state{stream = Stream1, regs = Regs1}.

-spec with_temp(
    state(),
    vm_register(),
    fun((aarch64_register()) -> {binary(), jit_regs:regs()})
) -> state().
with_temp(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Dest, EmitFn
) ->
    AR0 = jit_regs:available_regs(Regs0),
    Temp = first_avail(AR0),
    TempBit = reg_bit(Temp),
    {Code, Regs1} = EmitFn(Temp),
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = move_to_vm_register_emit(
        State0#state{
            stream = Stream1,
            regs = jit_regs:set_available_regs(Regs1, AR0 band (bnot TempBit))
        },
        Temp,
        Dest
    ),
    State1#state{regs = jit_regs:set_available_regs(State1#state.regs, AR0)}.

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
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:set_contents(Regs1, Temp, {x_reg, X}),
    State#state{stream = Stream1, regs = Regs2};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_atom(Reg) andalso is_integer(Index) ->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, {Dest, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_atom(Reg) andalso is_integer(Index) ->
    Available = jit_regs:available_regs(Regs0),
    Temp1 = first_avail(Available),
    Bit1 = reg_bit(Temp1),
    Avail1 = Available band (bnot Bit1),
    Temp2 = first_avail(Avail1),
    I1 = jit_aarch64_asm:ldr(Temp1, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Temp2, {Reg, Index * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Temp2, {Temp1, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp1),
    Regs3 = jit_regs:invalidate_reg(Regs2, Temp2),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    {free, Reg},
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Index * ?WORD_SIZE}),
    I3 = jit_aarch64_asm:str(Reg, {Temp, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Reg),
    Regs3 = jit_regs:invalidate_reg(Regs2, Temp),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:ldr(Dest, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Dest),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, ?X_REG(X)),
    Bit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, IndexReg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs2, Bit)
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {ptr, PtrReg}
) when is_atom(IndexReg) ->
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, {PtrReg, 0}),
    Bit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, IndexReg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs1, Bit)
    };
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    {free, IndexReg},
    {y_reg, Y}
) when ?IS_GPR(IndexReg) ->
    AvailableRegs0 = jit_regs:available_regs(Regs0),
    Temp = first_avail(AvailableRegs0),
    I1 = jit_aarch64_asm:ldr(Temp, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I3 = jit_aarch64_asm:str(IndexReg, {Temp, Y * ?WORD_SIZE}),
    Bit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary>>
    ),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs2 = jit_regs:invalidate_reg(Regs1, Temp),
    Regs3 = jit_regs:invalidate_reg(Regs2, IndexReg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs3, Bit)
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of an array element (reg[x]) to a new native register.
%% @end
%% @param State current backend state
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec get_array_element(
    state(), aarch64_register() | {free, aarch64_register()}, non_neg_integer()
) ->
    {state(), aarch64_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Index
) ->
    Available = jit_regs:available_regs(Regs0),
    ElemReg = first_avail(Available),
    Bit = reg_bit(ElemReg),
    I1 = jit_aarch64_asm:ldr(ElemReg, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ElemReg),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        ElemReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of a value (integer, vm register or native register) to an
%% array element (reg[x])
%% @end
%% @param State current backend state
%% @param Value value to move
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_array_element(
    state(), integer() | vm_register() | aarch64_register(), aarch64_register(), non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    Index
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_aarch64_asm:str(ValueReg, {Reg, Index * ?WORD_SIZE}),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    ValueReg,
    Reg,
    IndexReg
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(Reg) andalso ?IS_GPR(IndexReg) ->
    I1 = jit_aarch64_asm:str(ValueReg, {Reg, IndexReg, lsl, 3}),
    Stream1 = StreamModule:append(Stream0, I1),
    State0#state{stream = Stream1};
move_to_array_element(
    State0,
    Value,
    Reg,
    Index
) ->
    {State1, Temp} = copy_to_native_register(State0, Value),
    State2 = move_to_array_element(State1, Temp, Reg, Index),
    free_native_register(State2, Temp).

%%-----------------------------------------------------------------------------
%% @doc Emit a move of a value (integer, vm register or native register) to an
%% array element (reg[x+offset])
%% @end
%% @param State current backend state
%% @param Value value to move
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @param Offset additional offset
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_array_element(
    state(), value(), aarch64_register(), aarch64_register() | non_neg_integer(), integer()
) -> state().
move_to_array_element(
    State,
    Value,
    BaseReg,
    IndexVal,
    Offset
) when is_integer(IndexVal) andalso is_integer(Offset) ->
    move_to_array_element(State, Value, BaseReg, IndexVal + Offset);
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    ValueReg,
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(ValueReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:add(Temp, IndexReg, Offset),
    I2 = jit_aarch64_asm:str(ValueReg, {BaseReg, Temp, lsl, 3}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    State0,
    Value,
    BaseReg,
    IndexReg,
    Offset
) ->
    {State1, ValueReg} = copy_to_native_register(State0, Value),
    Temp = first_avail(jit_regs:available_regs(State1#state.regs)),
    I1 = jit_aarch64_asm:add(Temp, IndexReg, Offset),
    I2 = jit_aarch64_asm:str(ValueReg, {BaseReg, Temp, lsl, 3}),
    Stream1 = (State1#state.stream_module):append(State1#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(State1#state.regs, Temp),
    State2 = State1#state{stream = Stream1, regs = Regs1},
    free_native_register(State2, ValueReg).

%%-----------------------------------------------------------------------------
%% @doc Move a value (integer, vm register, pointer or native register) to a
%% native register. This allocates a new native register from the available
%% pool if needed.
%% @end
%% @param State current backend state
%% @param Value value to move (can be an immediate, vm register, pointer, or native register)
%% @return Tuple of {Updated backend state, Native register containing the value}
%%-----------------------------------------------------------------------------
-spec move_to_native_register(state(), value() | cp) -> {state(), aarch64_register()}.
move_to_native_register(State, Reg) when ?IS_GPR(Reg) ->
    {State, Reg};
move_to_native_register(#state{regs = Regs} = State, Value) ->
    Contents = jit_regs:value_to_contents(Value, ?MAX_REG),
    case Contents =/= unknown andalso jit_regs:find_reg_with_contents(Regs, Contents) of
        {ok, CachedReg} ->
            Bit = reg_bit(CachedReg),
            CurUsed = jit_regs:used_regs(Regs),
            CurAvail = jit_regs:available_regs(Regs),
            case CurUsed band Bit of
                0 ->
                    case CurAvail band Bit of
                        0 ->
                            move_to_native_register_emit(State, Value, Contents);
                        _ ->
                            {
                                State#state{
                                    regs = jit_regs:alloc_reg(Regs, Bit)
                                },
                                CachedReg
                            }
                    end;
                _ ->
                    {State, CachedReg}
            end;
        _ ->
            move_to_native_register_emit(State, Value, Contents)
    end.

move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    cp,
    Contents
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?CP),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    };
move_to_native_register_emit(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {ptr, Reg},
    _Contents
) when is_atom(Reg) ->
    I1 = jit_aarch64_asm:ldr(Reg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Imm,
    Contents
) when
    is_integer(Imm)
->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:mov(Reg, Imm),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {x_reg, extra},
    Contents
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {x_reg, X},
    Contents
) when
    X < ?MAX_REG
->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    };
move_to_native_register_emit(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {y_reg, Y},
    Contents
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    }.

%%-----------------------------------------------------------------------------
%% @doc Move a value (integer, vm register, pointer or native register) to a
%% specific native register.
%% @end
%% @param State current backend state
%% @param Value value to move (can be an immediate, vm register, pointer, or native register)
%% @param TargetReg the specific native register to move the value to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_native_register(state(), value(), aarch64_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_atom(RegSrc) ->
    I = jit_aarch64_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    SrcContents = jit_regs:get_contents(Regs0, RegSrc),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, SrcContents),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_integer(RegSrc) ->
    I = jit_aarch64_asm:mov(RegDst, RegSrc),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {imm, RegSrc}),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {ptr, Reg}, RegDst
) when ?IS_GPR(Reg) ->
    I1 = jit_aarch64_asm:ldr(RegDst, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, RegDst),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {x_reg, extra},
    RegDst
) ->
    I1 = jit_aarch64_asm:ldr(RegDst, ?X_REG(?MAX_REG)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {x_reg, extra}),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    I1 = jit_aarch64_asm:ldr(RegDst, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {x_reg, X}),
    State#state{stream = Stream1, regs = Regs1};
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {y_reg, Y}, RegDst
) ->
    I1 = jit_aarch64_asm:ldr(RegDst, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(RegDst, {RegDst, Y * ?WORD_SIZE}),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, RegDst, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Copy a value to a native register, allocating a new register from the
%% available pool. Unlike move_to_native_register, this always allocates a new
%% register and copies the value (preserving the source if it's a register).
%% @end
%% @param State current backend state
%% @param Value value to copy (can be an immediate, vm register, pointer, or native register)
%% @return Tuple of {Updated backend state, Native register containing the copied value}
%%-----------------------------------------------------------------------------
-spec copy_to_native_register(state(), value()) -> {state(), aarch64_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg
) when is_atom(Reg) ->
    Available = jit_regs:available_regs(Regs0),
    SaveReg = first_avail(Available),
    Bit = reg_bit(SaveReg),
    I1 = jit_aarch64_asm:mov(SaveReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    SrcContents = jit_regs:get_contents(Regs0, Reg),
    Regs1 = jit_regs:set_contents(Regs0, SaveReg, SrcContents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        SaveReg
    };
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {ptr, Reg}
) when is_atom(Reg) ->
    Available = jit_regs:available_regs(Regs0),
    SaveReg = first_avail(Available),
    Bit = reg_bit(SaveReg),
    I1 = jit_aarch64_asm:ldr(SaveReg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, SaveReg),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        SaveReg
    };
copy_to_native_register(State, Reg) ->
    move_to_native_register(State, Reg).

%%-----------------------------------------------------------------------------
%% @doc Move a VM register value to the continuation pointer (CP).
%% @end
%% @param State current backend state
%% @param VMReg VM register to move to CP
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec move_to_cp(state(), vm_register()) -> state().
move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {y_reg, Y}
) ->
    Avail = jit_regs:available_regs(Regs0),
    %% Use two temp registers: BaseReg keeps y_regs_base alive after this op
    %% so a subsequent increment_sp / y_reg access can reuse it. BaseReg is
    %% reserved (marked used) and released by the next op that consumes it
    %% (currently increment_sp).
    BaseReg = first_avail(Avail),
    BaseBit = reg_bit(BaseReg),
    Avail1 = Avail band (bnot BaseBit),
    case Avail1 of
        0 ->
            %% Only one register available, fall back to single-temp version.
            I1 = jit_aarch64_asm:ldr(BaseReg, ?Y_REGS),
            I2 = jit_aarch64_asm:ldr(BaseReg, {BaseReg, Y * ?WORD_SIZE}),
            I3 = jit_aarch64_asm:str(BaseReg, ?CP),
            Code = <<I1/binary, I2/binary, I3/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            Regs1 = jit_regs:set_contents(Regs0, BaseReg, {y_reg, Y}),
            State#state{stream = Stream1, regs = Regs1};
        _ ->
            ValReg = first_avail(Avail1),
            I1 = jit_aarch64_asm:ldr(BaseReg, ?Y_REGS),
            I2 = jit_aarch64_asm:ldr(ValReg, {BaseReg, Y * ?WORD_SIZE}),
            I3 = jit_aarch64_asm:str(ValReg, ?CP),
            Code = <<I1/binary, I2/binary, I3/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            %% Reserve BaseReg with y_regs_base contents so it isn't reused.
            Regs1 = jit_regs:set_contents(Regs0, BaseReg, y_regs_base),
            State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, BaseBit)}
    end.

%%-----------------------------------------------------------------------------
%% @doc Increment the stack pointer (SP) by a given offset.
%% @end
%% @param State current backend state
%% @param Offset offset to add to SP (in words, will be multiplied by 8)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec increment_sp(state(), integer()) -> state().
increment_sp(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Offset
) ->
    %% If a previous move_to_cp reserved y_regs_base in a register, reuse it
    %% and release the reservation.
    case jit_regs:find_reg_with_contents(Regs0, y_regs_base) of
        {ok, CachedBase} ->
            I1 = jit_aarch64_asm:add(CachedBase, CachedBase, Offset * ?WORD_SIZE),
            I2 = jit_aarch64_asm:str(CachedBase, ?Y_REGS),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            Bit = reg_bit(CachedBase),
            Regs1 = jit_regs:free_reg(jit_regs:invalidate_reg(Regs0, CachedBase), Bit),
            State#state{stream = Stream1, regs = Regs1};
        none ->
            Avail = jit_regs:available_regs(Regs0),
            Reg = first_avail(Avail),
            I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
            I2 = jit_aarch64_asm:add(Reg, Reg, Offset * ?WORD_SIZE),
            I3 = jit_aarch64_asm:str(Reg, ?Y_REGS),
            Code = <<I1/binary, I2/binary, I3/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
            State#state{stream = Stream1, regs = Regs1}
    end.

%%-----------------------------------------------------------------------------
%% @doc Set the continuation address to point to a specific label. The actual
%% address will be resolved during branch update.
%% @end
%% @param State current backend state
%% @param Label label to set as continuation target
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec set_continuation_to_label(state(), integer() | reference()) -> state().
set_continuation_to_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels,
        regs = Regs0
    } = State,
    Label
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Offset = StreamModule:offset(Stream0),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    case Labels of
        #{Label := LabelOffset} ->
            Rel = LabelOffset - Offset,
            I1 =
                if
                    Rel >= -1048576 andalso Rel =< 1048572 ->
                        jit_aarch64_asm:adr(Temp, Rel);
                    true ->
                        adr_far(Temp, Rel)
                end,
            I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1, regs = Regs1};
        _ ->
            % Placeholder must have the same size as any later patch
            I1 = adr_far(Temp, 0),
            BrEntry = {Offset, {adr, Temp}},
            I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            ExistingBrs = maps:get(Label, Branches, []),
            State#state{
                stream = Stream1,
                branches = Branches#{Label => [BrEntry | ExistingBrs]},
                regs = Regs1
            }
    end.

%%-----------------------------------------------------------------------------
%% @doc Set the continuation address to the current offset, creating a
%% reference for later resolution. Returns a reference that can be used
%% to add the label at the target location.
%% @end
%% @param State current backend state
%% @return Tuple of {Updated backend state, Reference for the continuation offset}
%%-----------------------------------------------------------------------------
-spec set_continuation_to_offset(state()) -> {state(), reference()}.
set_continuation_to_offset(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        regs = Regs0
    } = State
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    OffsetRef = make_ref(),
    Offset = StreamModule:offset(Stream0),
    % Placeholder must have the same size as any later patch
    I1 = adr_far(Temp, 0),
    BrEntry = {Offset, {adr, Temp}},
    I2 = jit_aarch64_asm:str(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    {
        State#state{
            stream = Stream1,
            branches = Branches#{OffsetRef => [BrEntry]},
            regs = Regs1
        },
        OffsetRef
    }.

%%-----------------------------------------------------------------------------
%% @doc Implement a continuation entry point. On AArch64 this is a nop
%% as we don't need to save any register.
%% @end
%% @param State current backend state
%% @return Updated backend state (unchanged on AArch64)
%%-----------------------------------------------------------------------------
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(State) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Resolve the imported BIF function pointer for a gc_bif call site inline,
%% instead of through the PRIM_GET_IMPORTED_GCBIF primitive call. Equivalent to
%% jit_get_imported_gcbif in jit.c: first drop dead extended registers (only if
%% any exist — the common case has none, so the cleanup call is skipped), then
%% load module->imported_funcs[Bif]->bif0_ptr. Returns the pointer register.
%% @end
-spec move_imported_gcbif_to_native_register(state(), integer(), non_neg_integer()) ->
    {state(), aarch64_register()}.
move_imported_gcbif_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    Live,
    Bif
) ->
    %% Inline extended-register cleanup: skip the call when the list is empty
    %% (ListHead.next == &list, i.e. equals the list address). list address is
    %% ctx + ?CTX_EXTENDED_X_REGS.
    Avail = jit_regs:available_regs(Regs0),
    AddrReg = first_avail(Avail),
    NextReg = first_avail(Avail band (bnot reg_bit(AddrReg))),
    I1 = jit_aarch64_asm:add(AddrReg, ?CTX_REG, ?CTX_EXTENDED_X_REGS),
    I2 = jit_aarch64_asm:ldr(NextReg, {AddrReg, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(
        jit_regs:invalidate_reg(Regs0, AddrReg), NextReg
    ),
    State1 = State0#state{stream = Stream1, regs = Regs1},
    State2 = if_block(State1, {{free, NextReg}, '!=', AddrReg}, fun(BSt0) ->
        {BSt1, R} = call_primitive(BSt0, ?PRIM_TRIM_LIVE_REGS, [ctx, Live]),
        free_native_registers(BSt1, [R])
    end),
    %% Now load the BIF pointer: module = [jit_state+0]; funcs = [module+IMP];
    %% exported = [funcs + Bif*8]; bif0_ptr = [exported + BIF0].
    Stream2 = State2#state.stream,
    Avail2 = jit_regs:available_regs(State2#state.regs),
    PtrReg = first_avail(Avail2),
    J1 = jit_aarch64_asm:ldr(PtrReg, ?JITSTATE_MODULE),
    J2 = jit_aarch64_asm:ldr(PtrReg, {PtrReg, ?MODULE_IMPORTED_FUNCS}),
    J3 = jit_aarch64_asm:ldr(PtrReg, {PtrReg, Bif * ?WORD_SIZE}),
    J4 = jit_aarch64_asm:ldr(PtrReg, {PtrReg, ?BIF_BIF0_PTR}),
    Stream3 = StreamModule:append(Stream2, <<J1/binary, J2/binary, J3/binary, J4/binary>>),
    Bit = reg_bit(PtrReg),
    Regs2 = jit_regs:alloc_reg(jit_regs:invalidate_reg(State2#state.regs, PtrReg), Bit),
    {
        State2#state{stream = Stream3, regs = Regs2},
        PtrReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Get the module index from the JIT state and load it into a native
%% register.
%% @end
%% @param State current backend state
%% @return Tuple of {Updated backend state, Native register containing module index}
%%-----------------------------------------------------------------------------
-spec get_module_index(state()) -> {state(), aarch64_register()}.
get_module_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_MODULE),
    I2 = jit_aarch64_asm:ldr_w(Reg, ?MODULE_INDEX(Reg)),
    Code = <<I1/binary, I2/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, module_index),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    }.

%% @private
-spec op_imm(state(), atom(), aarch64_register(), aarch64_register(), integer()) -> state().
op_imm(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Op, Reg, Reg, Val
) ->
    Result =
        try
            I = jit_aarch64_asm:Op(Reg, Reg, Val),
            {ok, StreamModule:append(Stream0, I), Regs0}
        catch
            error:{unencodable_immediate, Val} ->
                Temp = first_avail(jit_regs:available_regs(State#state.regs)),
                I1 = jit_aarch64_asm:mov(Temp, Val),
                I2 = jit_aarch64_asm:Op(Reg, Reg, Temp),
                NewStream = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
                %% Temp was reused as scratch — invalidate any cached contents.
                {ok, NewStream, jit_regs:invalidate_reg(Regs0, Temp)}
        end,
    {ok, Stream1, Regs1} = Result,
    State#state{stream = Stream1, regs = Regs1};
op_imm(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Op,
    RegA,
    RegB,
    Val
) ->
    Result =
        try
            I = jit_aarch64_asm:Op(RegA, RegB, Val),
            {ok, StreamModule:append(Stream0, I), Regs0}
        catch
            error:{unencodable_immediate, Val} ->
                MoveI = jit_aarch64_asm:mov(RegA, Val),
                AndI = jit_aarch64_asm:Op(RegA, RegB, RegA),
                NewStream = StreamModule:append(Stream0, <<MoveI/binary, AndI/binary>>),
                %% RegA was used as scratch for the immediate before the op overwrote it.
                {ok, NewStream, jit_regs:invalidate_reg(Regs0, RegA)}
        end,
    {ok, Stream1, Regs1} = Result,
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Perform bitwise AND of a register with an immediate value.
%% @end
%% @param State current backend state
%% @param Reg register to AND with value
%% @param Val immediate value to AND
%% @return Updated backend state
%%-----------------------------------------------------------------------------
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {free, Reg},
    SrcReg
) when
    is_atom(SrcReg)
->
    I1 = jit_aarch64_asm:and_(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
and_(#state{regs = Regs0} = State, {free, Reg}, Val) ->
    NewState = op_imm(State, and_, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {NewState#state{regs = Regs1}, Reg};
and_(
    #state{regs = Regs0} = State,
    Reg,
    Val
) ->
    Avail = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Avail),
    Bit = reg_bit(ResultReg),
    NewState = op_imm(
        State#state{
            regs = jit_regs:alloc_reg(Regs0, Bit)
        },
        and_,
        ResultReg,
        Reg,
        Val
    ),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    NewRegs = jit_regs:set_masks(
        Regs1,
        jit_regs:available_regs(NewState#state.regs),
        jit_regs:used_regs(NewState#state.regs)
    ),
    {NewState#state{regs = NewRegs}, ResultReg}.

%%-----------------------------------------------------------------------------
%% @doc Perform bitwise OR of a register with an immediate value.
%% @end
%% @param State current backend state
%% @param Reg register to OR with value
%% @param Val immediate value to OR
%% @return Updated backend state
%%-----------------------------------------------------------------------------
or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I1 = jit_aarch64_asm:orr(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
or_(#state{regs = Regs0} = State, Reg, Val) ->
    NewState = op_imm(State, orr, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1}.

xor_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I1 = jit_aarch64_asm:eor(Reg, Reg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
xor_(#state{regs = Regs0} = State, Reg, Val) ->
    NewState = op_imm(State, eor, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Add an immediate value to a register.
%% @end
%% @param State current backend state
%% @param Reg register to add to
%% @param Val immediate value to add
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add(state(), aarch64_register(), integer()) -> state().
add(#state{regs = Regs0} = State, Reg, Val) ->
    NewState = op_imm(State, add, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Subtract an immediate value from a register.
%% @end
%% @param State current backend state
%% @param Reg register to subtract from
%% @param Val immediate value to subtract
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec sub(state(), aarch64_register(), integer() | aarch64_register()) -> state().
sub(#state{regs = Regs0} = State, Reg, Val) when is_integer(Val) ->
    NewState = op_imm(State, sub, Reg, Reg, Val),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    NewState#state{regs = Regs1};
sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    is_atom(Val)
->
    I1 = jit_aarch64_asm:sub(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Add Val to Reg in place, setting condition flags (V on signed
%% overflow), testable with the `overflow_set' if-condition.
%% @end
%%-----------------------------------------------------------------------------
-spec add_overflow(state(), aarch64_register(), aarch64_register()) -> state().
add_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_atom(Val) ->
    I1 = jit_aarch64_asm:adds(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Subtract Val from Reg in place, setting condition flags. See
%% add_overflow/3.
%% @end
%%-----------------------------------------------------------------------------
-spec sub_overflow(state(), aarch64_register(), aarch64_register()) -> state().
sub_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_atom(Val) ->
    I1 = jit_aarch64_asm:subs(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Multiply two tagged small integers Reg and Val, leaving the product
%% shifted into the value field of Reg but WITHOUT the small-integer tag (low
%% bits zero); the caller re-tags on the no-overflow path. Condition flags are
%% set so the `mul_overflow_set' if-condition is true iff the result does NOT
%% fit in a small integer (and the bignum fallback must run).
%%
%% Both operands are (v << 4) | TERM_INTEGER_TAG. We untag both (arithmetic
%% shift right by 4), compute the low 64 bits (mul) and high 64 bits (smulh) of
%% the signed product, shift the low word back into the value field (Reg =
%% lo << 4), then test that it fits: the high word must equal the sign-extension
%% of the low word beyond the small-integer value range (asr by 59 on a 64-bit
%% build, i.e. SMALL value bits - 1). The final cmp sets Z=1 when it fits; the
%% shift happens before the cmp so it does not clobber the flags.
%% @end
%%-----------------------------------------------------------------------------
-spec mul_overflow(state(), aarch64_register(), aarch64_register()) -> state().
mul_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Reg,
    Val
) when is_atom(Val) ->
    %% Scratch registers for a, b/lo, hi, sign. first_avail does not consume
    %% Reg/Val (they are marked used), so these are distinct.
    Avail = jit_regs:available_regs(Regs0),
    A = first_avail(Avail),
    Avail1 = Avail band (bnot reg_bit(A)),
    Lo = first_avail(Avail1),
    Avail2 = Avail1 band (bnot reg_bit(Lo)),
    Hi = first_avail(Avail2),
    Avail3 = Avail2 band (bnot reg_bit(Hi)),
    Sign = first_avail(Avail3),
    Code = <<
        %% a = Reg >> 4 ; b = Val >> 4 (Lo temporarily holds b)
        (jit_aarch64_asm:asr(A, Reg, 4))/binary,
        (jit_aarch64_asm:asr(Lo, Val, 4))/binary,
        %% hi = smulh(a, b) ; lo = a * b
        (jit_aarch64_asm:smulh(Hi, A, Lo))/binary,
        (jit_aarch64_asm:mul(Lo, A, Lo))/binary,
        %% shift low word into the value field: Reg = lo << 4 (tag added later)
        (jit_aarch64_asm:lsl(Reg, Lo, 4))/binary,
        %% fits-in-small test: sign = asr(lo, 59); cmp hi, sign (Z=1 iff fits)
        (jit_aarch64_asm:asr(Sign, Lo, 59))/binary,
        (jit_aarch64_asm:cmp(Hi, Sign))/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Multiply a register by a constant value. Uses optimized instruction
%% sequences for common multipliers (powers of 2, small values).
%% @end
%% @param State current backend state
%% @param Reg register to multiply
%% @param Val multiplier (an integer constant or a register)
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec mul(state(), aarch64_register(), integer() | aarch64_register()) -> state().
mul(State, _Reg, 1) ->
    State;
mul(State, Reg, 2) ->
    shift_left(State, Reg, 1);
mul(#state{regs = Regs0} = State, Reg, 3) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 1),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 4) ->
    shift_left(State, Reg, 2);
mul(#state{regs = Regs0} = State, Reg, 5) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 2),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 6) ->
    State1 = mul(State0, Reg, 3),
    mul(State1, Reg, 2);
mul(#state{regs = Regs0} = State, Reg, 7) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 3),
    I2 = jit_aarch64_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 8) ->
    shift_left(State, Reg, 3);
mul(#state{regs = Regs0} = State, Reg, 9) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 3),
    I2 = jit_aarch64_asm:add(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State0, Reg, 10) ->
    State1 = mul(State0, Reg, 5),
    mul(State1, Reg, 2);
mul(#state{regs = Regs0} = State, Reg, 15) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:lsl(Temp, Reg, 4),
    I2 = jit_aarch64_asm:sub(Reg, Temp, Reg),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(State, Reg, 16) ->
    shift_left(State, Reg, 4);
mul(State, Reg, 32) ->
    shift_left(State, Reg, 5);
mul(State, Reg, 64) ->
    shift_left(State, Reg, 6);
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Reg,
    Val
) when is_integer(Val) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_aarch64_asm:mov(Temp, Val),
    I2 = jit_aarch64_asm:mul(Reg, Reg, Temp),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, DestReg, SrcReg
) when is_atom(SrcReg) ->
    I1 = jit_aarch64_asm:mul(DestReg, DestReg, SrcReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, DestReg),
    State#state{stream = Stream1, regs = Regs1}.

-spec div_(state(), aarch64_register(), aarch64_register()) -> {state(), aarch64_register()}.
div_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I1 = jit_aarch64_asm:sdiv(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

-spec rem_(state(), aarch64_register(), aarch64_register()) -> {state(), aarch64_register()}.
rem_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    DividendReg,
    DivisorReg
) ->
    Avail = jit_regs:available_regs(Regs0),
    %% rem = dividend - (dividend / divisor) * divisor
    %% Use msub: Rd = Ra - (Rn * Rm)
    %% First sdiv into a temp, then msub
    TempReg = first_avail(Avail band (bnot reg_bit(DividendReg)) band (bnot reg_bit(DivisorReg))),
    I1 = jit_aarch64_asm:sdiv(TempReg, DividendReg, DivisorReg),
    I2 = jit_aarch64_asm:msub(DividendReg, TempReg, DivisorReg, DividendReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

%% aarch64 always supports native sdiv.
-spec supports_div(state()) -> boolean().
supports_div(_State) -> true.

%% aarch64 has a hardware FPU, so it can inline double-precision fadd/fsub/fmul/
%% fdiv. The single-precision (FLOAT32) variant stores 4-byte floats in the fp
%% register array and is not handled inline here, so it falls back to the C
%% primitive.
-spec supports_fp(state()) -> boolean().
supports_fp(#state{variant = Variant}) ->
    Variant band ?JIT_VARIANT_FLOAT32 =:= 0.

%% Inline a double-precision binary float op fr[F3] = fr[F1] <op> fr[F2], and
%% return a register that is 0 iff the result is non-finite (so the caller can
%% raise badarith with the same test used for the C primitive's boolean result).
-spec float_op(state(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {state(), aarch64_register()}.
float_op(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    Primitive,
    F1,
    F2,
    F3
) ->
    Avail0 = jit_regs:available_regs(Regs0),
    Op =
        case Primitive of
            ?PRIM_FADD -> fun jit_aarch64_asm:fadd/3;
            ?PRIM_FSUB -> fun jit_aarch64_asm:fsub/3;
            ?PRIM_FMUL -> fun jit_aarch64_asm:fmul/3;
            ?PRIM_FDIV -> fun jit_aarch64_asm:fdiv/3
        end,
    CheckReg = first_avail(Avail0),
    Temp = first_avail(Avail0 band (bnot reg_bit(CheckReg))),
    %% Load the fp register array pointer (ctx->fr), compute the operation in
    %% d0, store it back to fr[F3], then test the result's exponent bits: a
    %% value is non-finite (inf/nan) iff all exponent bits are set. cset turns
    %% that into the clean 0/1 boolean the caller's badarith test expects.
    I1 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I2 = jit_aarch64_asm:ldr_d(d0, {Temp, ?FP_REG_OFFSET(State0, F1)}),
    I3 = jit_aarch64_asm:ldr_d(d1, {Temp, ?FP_REG_OFFSET(State0, F2)}),
    I4 = Op(d0, d0, d1),
    I5 = jit_aarch64_asm:str_d(d0, {Temp, ?FP_REG_OFFSET(State0, F3)}),
    I6 = jit_aarch64_asm:fmov(CheckReg, d0),
    I7 = jit_aarch64_asm:movz(Temp, 16#7FF0, 48),
    I8 = jit_aarch64_asm:and_(CheckReg, CheckReg, Temp),
    I9 = jit_aarch64_asm:cmp(CheckReg, Temp),
    I10 = jit_aarch64_asm:cset(CheckReg, ne),
    Code =
        <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary, I7/binary, I8/binary,
            I9/binary, I10/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    CheckBit = reg_bit(CheckReg),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Temp), CheckReg),
    {
        State0#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, CheckBit)},
        CheckReg
    }.

%% Convert an untagged signed integer (already in IntReg, i.e. the small-int
%% term shifted right past its tag) to a double and store it in fr[FPRegIndex].
%% Used by the inline fconv fast path; integer-to-double can never be
%% non-finite, so there is nothing to check and no register is returned.
-spec float_conv_int(state(), aarch64_register(), non_neg_integer()) -> state().
float_conv_int(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    IntReg,
    FPRegIndex
) ->
    Avail0 = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail0),
    I1 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I2 = jit_aarch64_asm:scvtf(d0, IntReg),
    I3 = jit_aarch64_asm:str_d(d0, {Temp, ?FP_REG_OFFSET(State0, FPRegIndex)}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1}.

%% Unbox a boxed float term (in BoxedReg) and store its double value into
%% fr[FPRegIndex]. The double lives just past the boxed header word, i.e. at
%% offset one word from the untagged boxed pointer. BoxedReg is consumed.
-spec float_conv_float(state(), aarch64_register(), non_neg_integer()) -> state().
float_conv_float(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    BoxedReg,
    FPRegIndex
) ->
    Avail0 = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail0 band (bnot reg_bit(BoxedReg))),
    %% Clear the 2 primary (boxed) tag bits (mask 0x3) to get the boxed pointer,
    %% load the double from boxed_ptr[1], load fr base, store to fr[FPRegIndex].
    I1 = jit_aarch64_asm:and_(BoxedReg, BoxedReg, bnot 16#3),
    I2 = jit_aarch64_asm:ldr_d(d0, {BoxedReg, ?WORD_SIZE}),
    I3 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I4 = jit_aarch64_asm:str_d(d0, {Temp, ?FP_REG_OFFSET(State0, FPRegIndex)}),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State0#state{stream = Stream1, regs = Regs1}.

%% Store a compile-time float constant directly into fr[FPRegIndex] as its
%% IEEE-754 double bits, avoiding any literal-table access at runtime. Only
%% called when supports_fp/1 returns true, i.e. the double-precision variant.
-spec move_float_to_fp_reg(state(), float(), non_neg_integer()) -> state().
move_float_to_fp_reg(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    Float,
    FPRegIndex
) ->
    <<Bits:64/unsigned-little>> = <<Float:64/float-little>>,
    Avail0 = jit_regs:available_regs(Regs0),
    BitsReg = first_avail(Avail0),
    BaseReg = first_avail(Avail0 band (bnot reg_bit(BitsReg))),
    I1 = jit_aarch64_asm:mov(BitsReg, Bits),
    I2 = jit_aarch64_asm:ldr(BaseReg, ?FP_REGS),
    I3 = jit_aarch64_asm:str(BitsReg, {BaseReg, ?FP_REG_OFFSET(State0, FPRegIndex)}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, BitsReg), BaseReg),
    State0#state{stream = Stream1, regs = Regs1}.

%% Load the fp register array pointer (ctx->fr) into a freshly allocated
%% register and return it, so the caller can test it for NULL and only call
%% context_ensure_fpregs (the malloc) when it has not been allocated yet.
-spec read_fp_regs_ptr(state()) -> {state(), aarch64_register()}.
read_fp_regs_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?FP_REGS),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, Bit)},
        Reg
    }.

%%-----------------------------------------------------------------------------
%% @doc Decrement the reduction count and schedule the next process if it
%% reaches zero. If reductions remain, execution continues; otherwise, the
%% continuation is set and the scheduler is invoked.
%% @end
%% @param State current backend state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State0
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    % Load reduction count
    I1 = jit_aarch64_asm:ldr_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
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
    State1 = State0#state{stream = Stream2, regs = Regs1},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite the branch and adr instructions
    #state{stream = Stream3} = State2,
    NewOffset = StreamModule:offset(Stream3),
    NewI4 = jit_aarch64_asm:bcc(ne, NewOffset - BNEOffset),
    NewI5 = jit_aarch64_asm:adr(Temp, NewOffset - ADROffset),
    Stream4 = StreamModule:replace(
        Stream3, BNEOffset, <<NewI4/binary, NewI5/binary>>
    ),
    %% schedule_next clobbers caller-saved regs; invalidate cache at continuation.
    State2#state{stream = Stream4, regs = jit_regs:invalidate_all(State1#state.regs)}.

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a label with automatic scheduling. Decrements reductions
%% and calls the label if reductions remain, otherwise schedules the next
%% process. Sets the continuation pointer before the call.
%% @end
%% @param State current backend state
%% @param Label label to call
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset, RewriteSize} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset, RewriteSize).

%%-----------------------------------------------------------------------------
%% @doc Emit a tail call to a label with automatic scheduling. Decrements
%% reductions and jumps to the label if reductions remain, otherwise schedules
%% the next process. Does not set a new continuation pointer (tail call).
%% @end
%% @param State current backend state
%% @param Label label to jump to
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_only_or_schedule_next(state(), non_neg_integer()) -> state().
call_only_or_schedule_next(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels,
        regs = Regs0
    } = State0,
    Label
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    % Load reduction count
    I1 = jit_aarch64_asm:ldr_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    BNEOffset = StreamModule:offset(Stream1),

    case Labels of
        #{Label := LabelOffset} when
            LabelOffset - BNEOffset >= -1048576 andalso LabelOffset - BNEOffset < 1048576
        ->
            % Label is already known and in bcc range, emit direct branch
            Rel = LabelOffset - BNEOffset,
            I4 = jit_aarch64_asm:bcc(ne, Rel),
            Stream2 = StreamModule:append(Stream1, I4),
            State1 = State0#state{stream = Stream2};
        #{Label := LabelOffset} ->
            % Label is beyond bcc's ±1MB range: skip over an unconditional
            % branch (±128MB) with the inverted condition
            I4 = jit_aarch64_asm:bcc(eq, 8),
            I5 = jit_aarch64_asm:b(LabelOffset - (BNEOffset + 4)),
            Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary>>),
            State1 = State0#state{stream = Stream2};
        _ ->
            % Label not yet known: emit the far-capable pair so the patch
            % fits whatever the final distance is
            I4 = jit_aarch64_asm:bcc(eq, 8),
            I5 = jit_aarch64_asm:b(0),
            BrEntry = {BNEOffset + 4, b},
            ExistingBrs = maps:get(Label, Branches, []),
            Stream2 = StreamModule:append(Stream1, <<I4/binary, I5/binary>>),
            State1 = State0#state{
                stream = Stream2,
                branches = Branches#{Label => [BrEntry | ExistingBrs]}
            }
    end,
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

%%-----------------------------------------------------------------------------
%% @doc Emit a call to a primitive with continuation pointer setup. This is
%% used for primitives that may not return directly (e.g., those that can
%% trap or reschedule). Sets CP before calling the primitive.
%% @end
%% @param State current backend state
%% @param Primitive index of the primitive to call
%% @param Args arguments to pass to the primitive
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec call_primitive_with_cp(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset, RewriteSize} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset, RewriteSize).

%% @private
-spec set_cp(state()) -> {state(), non_neg_integer(), 4 | 8}.
set_cp(State0) ->
    % get module index (dynamically)
    {#state{stream_module = StreamModule, stream = Stream0} = State1, Reg} = get_module_index(
        State0
    ),
    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_aarch64_asm:lsl(Reg, Reg, 24),
    if
        Offset >= 16250 ->
            I2 = jit_aarch64_asm:nop(),
            I3 = jit_aarch64_asm:nop(),
            RewriteSize = 8;
        true ->
            I2 = jit_aarch64_asm:nop(),
            I3 = <<>>,
            RewriteSize = 4
    end,
    MOVOffset = Offset + byte_size(I1),
    I4 = jit_aarch64_asm:orr(Reg, Reg, ?IP0_REG),
    I5 = jit_aarch64_asm:str(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, MOVOffset, RewriteSize}.

%% @private
-spec rewrite_cp_offset(state(), non_neg_integer(), 4 | 8) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset,
    _RewriteSize
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    NewMoveInstr = jit_aarch64_asm:mov(?IP0_REG, NewOffset bsl 2),
    ?ASSERT(byte_size(NewMoveInstr) =< _RewriteSize),
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, NewMoveInstr),
    %% Execution resumes here when the callee returns: registers are
    %% clobbered and, crucially, code is reachable again.
    State0#state{stream = Stream1, regs = jit_regs:invalidate_all(State0#state.regs)}.

%%-----------------------------------------------------------------------------
%% @doc Set the binary state (BS) register to point to a term and reset the
%% BS offset to zero. Used for binary matching operations.
%% @end
%% @param State current backend state
%% @param TermReg register containing the term to set as binary state
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec set_bs(state(), aarch64_register()) -> state().
set_bs(#state{stream_module = StreamModule, stream = Stream0} = State0, TermReg) ->
    I1 = jit_aarch64_asm:str(TermReg, ?BS),
    I2 = jit_aarch64_asm:str(xzr, ?BS_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @param State current state
%% @param SortedLines line information, sorted by offset
%% @doc Build labels and line tables and encode a function that returns it.
%% In this case, the function returns the effective address of what immediately
%% follows.
%% @end
%% @return New state
%%-----------------------------------------------------------------------------
-spec return_labels_and_lines(state(), [{non_neg_integer(), non_neg_integer()}]) -> state().
return_labels_and_lines(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        labels = Labels
    } = State,
    SortedLines
) ->
    SortedLabels = lists:keysort(2, [
        {Label, LabelOffset}
     || {Label, LabelOffset} <- maps:to_list(Labels), is_integer(Label)
    ]),

    I1 = jit_aarch64_asm:adr(r0, 8),
    I2 = jit_aarch64_asm:ret(),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

reg_bit(r0) -> ?REG_BIT_R0;
reg_bit(r1) -> ?REG_BIT_R1;
reg_bit(r2) -> ?REG_BIT_R2;
reg_bit(r3) -> ?REG_BIT_R3;
reg_bit(r4) -> ?REG_BIT_R4;
reg_bit(r5) -> ?REG_BIT_R5;
reg_bit(r6) -> ?REG_BIT_R6;
reg_bit(r7) -> ?REG_BIT_R7;
reg_bit(r8) -> ?REG_BIT_R8;
reg_bit(r9) -> ?REG_BIT_R9;
reg_bit(r10) -> ?REG_BIT_R10;
reg_bit(r11) -> ?REG_BIT_R11;
reg_bit(r12) -> ?REG_BIT_R12;
reg_bit(r13) -> ?REG_BIT_R13;
reg_bit(r14) -> ?REG_BIT_R14;
reg_bit(r15) -> ?REG_BIT_R15;
reg_bit(r16) -> ?REG_BIT_R16;
reg_bit(r17) -> ?REG_BIT_R17.

%%-----------------------------------------------------------------------------
%% @doc Add a label at the current offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference()) -> state().
add_label(#state{stream_module = StreamModule, stream = Stream, regs = Regs0} = State, Label) ->
    Offset = StreamModule:offset(Stream),
    Regs1 = jit_regs:invalidate_all(Regs0),
    add_label(State#state{regs = Regs1}, Label, Offset).

%%-----------------------------------------------------------------------------
%% @doc Add a label at a specific offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @param Offset the explicit offset for this label
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference(), integer()) -> state().
add_label(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        jump_table_start = JumpTableStart,
        branches = Branches,
        labels = Labels
    } = State,
    Label,
    LabelOffset
) when is_integer(Label) ->
    % Patch the jump table entry immediately
    % Each b instruction is 4 bytes
    JumpTableEntryOffset = JumpTableStart + Label * 4,
    RelativeOffset = LabelOffset - JumpTableEntryOffset,
    BranchInstr = jit_aarch64_asm:b(RelativeOffset),
    Stream1 = StreamModule:replace(Stream0, JumpTableEntryOffset, BranchInstr),

    % Eagerly patch any branches targeting this label
    {Stream2, RemainingBranches} = patch_branches_for_label(
        StreamModule,
        Stream1,
        Label,
        LabelOffset,
        Branches
    ),

    State#state{
        stream = Stream2, branches = RemainingBranches, labels = Labels#{Label => LabelOffset}
    };
add_label(#state{labels = Labels} = State, Label, Offset) ->
    State#state{labels = Labels#{Label => Offset}}.

%% @doc Byte offset of the `x' register array within the Context struct.
%% Derived from ?X_REG so it tracks the codegen offset.
-spec dwarf_x_reg_offset() -> non_neg_integer().
dwarf_x_reg_offset() ->
    element(2, ?X_REG(0)).

%% @doc Record a type assertion for a VM x/y register. The assertion is
%% invalidated automatically by the same hooks that invalidate `regs` tracking
%% (writes to the VM register, C calls clobbering x regs, labels).
set_vm_record_type(#state{regs = Regs} = State, VmLoc, Type) ->
    State#state{regs = jit_regs:set_vm_type(Regs, VmLoc, Type)}.

%% @doc Look up the type assertion previously recorded for a VM x/y register.
get_vm_record_type(#state{regs = Regs}, VmLoc) ->
    jit_regs:get_vm_type(Regs, VmLoc).

-ifdef(JIT_DWARF).
%%-----------------------------------------------------------------------------
%% @doc Return the DWARF register number for the ctx parameter
%% @returns The DWARF register number where ctx is passed (x0/r0 in aarch64)
%% @end
%%-----------------------------------------------------------------------------
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    ?DWARF_X0_REG_AARCH64.

-spec dwarf_register_number(atom()) -> non_neg_integer().
dwarf_register_number(r0) -> 0;
dwarf_register_number(r1) -> 1;
dwarf_register_number(r2) -> 2;
dwarf_register_number(r3) -> 3;
dwarf_register_number(r4) -> 4;
dwarf_register_number(r5) -> 5;
dwarf_register_number(r6) -> 6;
dwarf_register_number(r7) -> 7;
dwarf_register_number(r8) -> 8;
dwarf_register_number(r9) -> 9;
dwarf_register_number(r10) -> 10;
dwarf_register_number(r11) -> 11;
dwarf_register_number(r12) -> 12;
dwarf_register_number(r13) -> 13;
dwarf_register_number(r14) -> 14;
dwarf_register_number(r15) -> 15;
dwarf_register_number(r16) -> 16;
dwarf_register_number(r17) -> 17.
-endif.
