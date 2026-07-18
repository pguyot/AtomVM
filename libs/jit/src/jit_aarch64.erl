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
    relocations/1,
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
    add_deferred_raise/5,
    take_deferred_raises/1,
    reset_regs_fresh/1,
    jump_to_label_cond/3,
    call_primitive_with_cp/3,
    call_primitive_with_cp_direct/3,
    call_primitive_direct/3,
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
    load_be_unsigned/3,
    set_bs/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    get_module_index/1,
    get_cp_base/1,
    get_module_atom_index/2,
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
    set_live_masks/2,
    supports_loop_residency/0,
    heap_bump_alloc/2,
    jump_table_range_check/4,
    jump_table_dispatch/1,
    shift_right_arith_reg/3,
    shift_left_reg/3,
    read_avail_heap_memory/1,
    read_heap_fragments/1,
    read_shrink_probe_mismatch/1,
    allocate_frame_fast/2,
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
-include("term.hrl").

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
    regs :: jit_regs:regs(),
    %% Primitive-call relocations recorded in JIT_VARIANT_RELOC mode: a list of
    %% {ByteOffset, PrimitiveIndex}. The loader patches the b/bl imm26 at each
    %% ByteOffset to reach module_native_interface[PrimitiveIndex] at run time.
    relocations = [] :: [{non_neg_integer(), non_neg_integer()}],
    %% Deferred x-register store elision (pass B of jit_liveness): per-label
    %% live-in masks, pending stores (x index -> {stream offset of the str,
    %% store width in bytes, cond depth}), and the current conditional-emission
    %% depth. The pending machinery lives in jit_backend_pending_impl.hrl.
    live_masks = undefined :: undefined | #{non_neg_integer() => non_neg_integer()},
    pending_x = #{} ::
        #{non_neg_integer() => {non_neg_integer(), non_neg_integer(), non_neg_integer()}},
    cond_depth = 0 :: non_neg_integer(),
    %% Loop-header register residency: labels that are direct call targets
    %% (from jit_liveness pass A) get a cold-entry preload of their live-in
    %% x registers; loop_entries records Label => {HotOffset, [{X, Reg}]}
    %% so backward call_only sites can reconcile and enter past the loads.
    call_targets = #{} :: #{integer() => true},
    loop_entries = #{} :: #{integer() => {non_neg_integer(), [{non_neg_integer(), atom()}]}},
    %% Hot-capable call_only blocks are shared across sites by jit.erl's
    %% tail cache; the block entry offset maps to {Label, SharedOffset}
    %% (start of the register-state-independent part) so jump_to_offset
    %% can emit a site-specific reconciliation before entering the block.
    recon_blocks = #{} :: #{non_neg_integer() => {integer(), non_neg_integer()}},
    %% call_only blocks emitted before their target label existed branch to
    %% the cold entry; when a later site would reuse one and the label has
    %% since gained a hot entry, a fresh block is emitted instead.
    cold_call_blocks = #{} :: #{non_neg_integer() => integer()},
    %% Deferred (outlined) raise blocks: {StubRef, SiteOffset, Prim, ExtraArgs}.
    %% The raise site branches to StubRef (happy path falls through); the actual
    %% tail-calling raise is emitted at the module tail by flush_deferred_raises,
    %% deduped per {Prim, ExtraArgs} since ctx/jit_state are pinned (x0/x1) and
    %% the per-site offset is reloaded from a fresh register at each stub.
    deferred_raises = [] :: [{reference(), non_neg_integer(), non_neg_integer(), [arg()]}]
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
    | {maybe_free_aarch64_register(), '<u', aarch64_register()}
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
-define(HEAP_PTR, {?CTX_REG, 16#18}).
-define(X_REG(N), {?CTX_REG, 16#58 + (N * ?WORD_SIZE)}).
-define(CP, {?CTX_REG, 16#E0}).
-define(FP_REGS, {?JITSTATE_REG, 16#18}).
-define(FP_REG_OFFSET(State, F),
    (F *
        case (State)#state.variant band ?JIT_VARIANT_FLOAT32 of
            0 -> 8;
            _ -> 4
        end)
).
-define(BS, {?CTX_REG, 16#E8}).
-define(BS_OFFSET, {?CTX_REG, 16#F0}).
-define(JITSTATE_MODULE, {?JITSTATE_REG, 0}).
-define(JITSTATE_CONTINUATION, {?JITSTATE_REG, 16#8}).
-define(JITSTATE_REDUCTIONCOUNT, {?JITSTATE_REG, 16#10}).
%% module_index << 24, maintained by jit_state_set_module in jit.c
%% (_Static_assert pins the offset).
-define(JITSTATE_CPBASE, {?JITSTATE_REG, 16#28}).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).
% module->local_atoms_to_global_table (see _Static_assert in jit.c).
-define(MODULE_LOCAL_ATOMS_TABLE(ModuleReg), {ModuleReg, 16#D8}).
% Offsets for inlining the imported-BIF pointer resolution at gc_bif call sites.
% Kept in sync with src/libAtomVM/jit.c via _Static_assert.
-define(MODULE_IMPORTED_FUNCS, 16#90).
-define(CTX_EXTENDED_X_REGS, 16#F8).
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
        relocations = [],
        regs = jit_regs:new(avail_mask_for_variant(Variant), 0)
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
%% @doc Primitive-call relocations recorded in JIT_VARIANT_RELOC mode, as a list
%% of {ByteOffset, PrimitiveIndex} (ByteOffset relative to the stream start,
%% i.e. including the chunk info header). Empty in non-reloc mode.
%% @end
%%-----------------------------------------------------------------------------
-spec relocations(state()) -> [{non_neg_integer(), non_neg_integer()}].
relocations(#state{relocations = Relocations}) ->
    Relocations.

%% Registers the allocator may use. In JIT_VARIANT_RELOC mode primitive calls are
%% direct, so the native-interface register (r2) is never live between calls and
%% becomes an extra general-purpose register (it is already a parameter register,
%% so it behaves exactly like r3-r5 around calls).
-spec avail_mask(state()) -> non_neg_integer().
avail_mask(#state{variant = Variant}) ->
    avail_mask_for_variant(Variant).

avail_mask_for_variant(Variant) ->
    case (Variant band ?JIT_VARIANT_RELOC) =/= 0 of
        true -> ?AVAILABLE_REGS_MASK bor ?REG_BIT_R2;
        false -> ?AVAILABLE_REGS_MASK
    end.

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
jump_table(#state{} = StateP, LabelsCount) ->
    #state{stream_module = StreamModule, stream = Stream0} = State = pending_clear_all(StateP),
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
    case (State#state.variant band ?JIT_VARIANT_RELOC) =/= 0 of
        true ->
            %% Direct, loader-relocated call: no table load, emit the branch in
            %% call_func_ptr from the {primitive, _} form.
            call_func_ptr(State, {primitive, Primitive}, Args);
        false ->
            PrepCall =
                case Primitive of
                    0 ->
                        jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, 0});
                    N ->
                        jit_aarch64_asm:ldr(?IP0_REG, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
                end,
            Stream1 = StreamModule:append(Stream0, PrepCall),
            StateCall = State#state{stream = Stream1},
            call_func_ptr(StateCall, {free, ?IP0_REG}, Args)
    end.

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
    #state{} = StateP,
    Primitive,
    Args
) ->
    %% Tail call into C: argument setup below reads x registers from the
    %% context (set_args), so pending stores must persist.
    #state{
        stream_module = StreamModule,
        stream = Stream0
    } = State0 = pending_clear_all(StateP),
    % We need a register for the function pointer that should not be used as a parameter
    % Since we're not returning, we can use all scratch registers except
    % registers used for parameters
    Reloc = (State0#state.variant band ?JIT_VARIANT_RELOC) =/= 0,
    #{temp := Temp, available_mask := AvailableRegs1, used_mask := UsedRegs} =
        prepare_call_scratch(Args),
    Stream1 =
        case Reloc of
            true ->
                %% No table load: the tail branch below is loader-relocated.
                Stream0;
            false ->
                PrepCall =
                    case Primitive of
                        0 ->
                            jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, 0});
                        N ->
                            jit_aarch64_asm:ldr(Temp, {?NATIVE_INTERFACE_REG, N * ?WORD_SIZE})
                    end,
                StreamModule:append(Stream0, PrepCall)
        end,
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
    {Stream3, Relocations1} =
        case Reloc of
            true ->
                %% Tail call: single direct branch, loader-bound to the primitive
                %% or its in-module veneer.
                BOffset = StreamModule:offset(Stream2),
                {StreamModule:append(Stream2, jit_aarch64_asm:b(0)), [
                    {BOffset, Primitive} | State1#state.relocations
                ]};
            false ->
                {StreamModule:append(Stream2, jit_aarch64_asm:br(Temp)), State1#state.relocations}
        end,
    State1#state{
        stream = Stream3,
        relocations = Relocations1,
        regs = jit_regs:set_masks(
            jit_regs:unreachable(State1#state.regs), avail_mask(State1), 0
        )
    }.

%%-----------------------------------------------------------------------------
%% @doc Record a deferred (outlined) raise. The raise site has already branched
%% to `StubRef' when the error condition holds; the tail-calling raise itself is
%% emitted at the module tail by `jit:flush_deferred_raises/2'. `SiteOffset' is
%% the site's native offset, used to reconstruct the exception's line number.
%% @end
%%-----------------------------------------------------------------------------
-spec add_deferred_raise(state(), reference(), non_neg_integer(), non_neg_integer(), [arg()]) ->
    state().
add_deferred_raise(#state{deferred_raises = DR} = State, StubRef, SiteOffset, Prim, ExtraArgs) ->
    State#state{deferred_raises = [{StubRef, SiteOffset, Prim, ExtraArgs} | DR]}.

%%-----------------------------------------------------------------------------
%% @doc Return the recorded deferred raises (in emission order) and clear them.
%% @end
%%-----------------------------------------------------------------------------
-spec take_deferred_raises(state()) ->
    {[{reference(), non_neg_integer(), non_neg_integer(), [arg()]}], state()}.
take_deferred_raises(#state{deferred_raises = DR} = State) ->
    {lists:reverse(DR), State#state{deferred_raises = []}}.

%%-----------------------------------------------------------------------------
%% @doc Reset the tracked register state to fresh (all scratch available, no
%% contents), used at the head of each outlined raise stub: the stub is entered
%% only by its site's branch and does nothing but reload its offset and
%% tail-call, so it may clobber freely -- and a deterministic fresh state makes
%% the offset register identical across stubs, so same-shape stubs dedup.
%% @end
%%-----------------------------------------------------------------------------
-spec reset_regs_fresh(state()) -> state().
reset_regs_fresh(#state{regs = Regs} = State) ->
    State#state{regs = jit_regs:set_masks(jit_regs:invalidate_all(Regs), avail_mask(State), 0)}.

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
    #state{} = StateP,
    Label
) ->
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches, labels = Labels} =
        State = pending_filter_label(StateP, Label),
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

%%-----------------------------------------------------------------------------
%% @doc Emit a single conditional branch to `Label' when `Cond' holds; the happy
%% path falls through. This is the branch-if-true dual of `if_block' (which
%% branches over its body when the condition is false), so a guard's fail-jump
%% costs one taken-only branch instead of a skip + unconditional jump, and the
%% not-taken fall-through is the common, well-predicted path. For the register
%% test forms (tbz/cbz), which `update_branches' cannot patch to a label, keep
%% the placeholder as a skip over an unconditional branch (the previous
%% two-branch shape). Pending x-register stores the label needs are committed
%% first, exactly as `jump_to_label' does, so both edges see a consistent heap.
%% @end
%%-----------------------------------------------------------------------------
-spec jump_to_label_cond(state(), tuple(), integer() | reference()) -> state().
jump_to_label_cond(StateP, {'and', _} = Cond, Label) ->
    %% Compound AND: a single conditional branch cannot express "all hold", so
    %% fall back to the block form (each sub-condition skips past the branch),
    %% which if_block already handles.
    if_block(StateP, Cond, fun(BSt) -> jump_to_label(BSt, Label) end);
jump_to_label_cond(StateP, Cond, Label) ->
    #state{stream_module = SM, labels = Labels} = State = pending_filter_label(StateP, Label),
    Offset0 = SM:offset(State#state.stream),
    {State1, CC, BranchInstrOffset} = if_block_cond(State, Cond),
    BranchOffset = Offset0 + BranchInstrOffset,
    %% A `bcc' reaches only +/-1MB (imm19). For a backward label the exact
    %% distance is known. For a forward BEAM label the byte offset is not known
    %% yet, so estimate it from the running bytes-per-label density and the
    %% number of labels still ahead ((target - emitted) * avg): guard fail-jumps
    %% are intra-function, so this tracks the real distance closely, and a
    %% generous margin (well under the +/-1MB `bcc' reach) absorbs the estimate's
    %% imprecision. When over budget -- or for the far module-tail stub refs --
    %% keep the full-range two-branch form (bcc skip + `b'), the prior behaviour.
    InBccRange =
        case Labels of
            #{Label := LabelOffset} ->
                abs(LabelOffset - BranchOffset) < 16#F0000;
            _ when is_integer(Label) ->
                Emitted = maps:size(Labels),
                Emitted > 0 andalso
                    (Label - Emitted) * (BranchOffset div Emitted) < 16#60000;
            _ ->
                false
        end,
    case is_atom(CC) andalso InBccRange of
        true ->
            record_label_branch(State1, Label, BranchOffset, {bcc, invert_cc(CC)});
        false ->
            #state{stream = Stream1} = State1,
            BOffset = SM:offset(Stream1),
            Stream2 = SM:append(Stream1, jit_aarch64_asm:b(0)),
            AfterOffset = SM:offset(Stream2),
            CondSkip = rewrite_branch_instruction(CC, AfterOffset - BranchOffset),
            Stream3 = SM:replace(Stream2, BranchOffset, CondSkip),
            record_label_branch(State1#state{stream = Stream3}, Label, BOffset, b)
    end.

%% Record (or, if the label offset is already known, immediately patch) a branch
%% of the given type to Label. Unlike jump_to_label the fall-through remains
%% reachable, so the register state is left as-is (not marked unreachable).
record_label_branch(
    #state{stream_module = SM, stream = Stream, branches = Branches, labels = Labels} = State,
    Label,
    BrOffset,
    Type
) ->
    case Labels of
        #{Label := LabelOffset} ->
            State#state{stream = patch_branch(SM, Stream, BrOffset, Type, LabelOffset)};
        _ ->
            Existing = maps:get(Label, Branches, []),
            State#state{branches = Branches#{Label => [{BrOffset, Type} | Existing]}}
    end.

%% Invert an aarch64 condition code (if_block_cond returns the branch-if-false
%% code; jump_to_label_cond needs branch-if-true).
invert_cc(eq) -> ne;
invert_cc(ne) -> eq;
invert_cc(ge) -> lt;
invert_cc(lt) -> ge;
invert_cc(le) -> gt;
invert_cc(gt) -> le;
invert_cc(ls) -> hi;
invert_cc(hi) -> ls;
invert_cc(lo) -> hs;
invert_cc(hs) -> lo;
invert_cc(cc) -> cs;
invert_cc(cs) -> cc;
invert_cc(mi) -> pl;
invert_cc(pl) -> mi;
invert_cc(vs) -> vc;
invert_cc(vc) -> vs.

jump_to_offset(#state{cold_call_blocks = CB, loop_entries = LE} = StateP, TargetOffset) when
    is_map_key(TargetOffset, CB) andalso is_map_key(map_get(TargetOffset, CB), LE)
->
    %% The shared block branches to the target label's cold entry, but the
    %% label has a hot entry now: emit a fresh call_only block (site
    %% reconciliation + reduction + hot branch) instead of reusing it.
    call_only_or_schedule_next(StateP, map_get(TargetOffset, CB));
jump_to_offset(#state{recon_blocks = RB} = StateP, TargetOffset) when
    is_map_key(TargetOffset, RB)
->
    %% Entering a shared hot-capable call_only block: its register-state-
    %% independent part starts at SharedOffset; emit this site's own
    %% reconciliation of the loop-entry bindings first.
    #{TargetOffset := {Label, SharedOffset}} = RB,
    State0 = pending_filter_label(StateP, Label),
    #state{loop_entries = #{Label := {_HotOffset, Bindings}}} = State0,
    State1 = emit_backedge_recon(State0, Bindings),
    #state{stream_module = StreamModule, stream = Stream1} = State1,
    Rel = SharedOffset - StreamModule:offset(Stream1),
    Stream2 = StreamModule:append(Stream1, jit_aarch64_asm:b(Rel)),
    State1#state{stream = Stream2, regs = jit_regs:unreachable(State1#state.regs)};
jump_to_offset(#state{} = StateP, TargetOffset) ->
    #state{stream_module = StreamModule, stream = Stream0} = State = pending_clear_all(StateP),
    Offset = StreamModule:offset(Stream0),
    Rel = TargetOffset - Offset,
    I1 = jit_aarch64_asm:b(Rel),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, regs = jit_regs:unreachable(State#state.regs)}.

%% Emit loads/moves so each loop-entry binding register holds its x value,
%% using the current contents cache to elide loads that are already in
%% place. Bindings are materialized in order; a register already consumed
%% as a reconciliation target is not trusted as a move source.
emit_backedge_recon(#state{stream_module = StreamModule, regs = Regs} = State, Bindings) ->
    {Stream1, _Done} = lists:foldl(
        fun({X, Reg}, {StAcc, Done}) ->
            I =
                case jit_regs:find_reg_with_contents(Regs, {x_reg, X}) of
                    {ok, Reg} ->
                        <<>>;
                    {ok, Other} ->
                        case lists:member(Other, Done) of
                            false -> jit_aarch64_asm:mov(Reg, Other);
                            true -> jit_aarch64_asm:ldr(Reg, ?X_REG(X))
                        end;
                    _ ->
                        jit_aarch64_asm:ldr(Reg, ?X_REG(X))
                end,
            {StreamModule:append(StAcc, I), [Reg | Done]}
        end,
        {State#state.stream, []},
        Bindings
    ),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Jump to a continuation address stored in a register.
%% This is used for optimized intra-module returns.
%% @end
%% @param State current backend state
%% @param OffsetReg register containing the continuation offset
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_continuation(
    #state{} = StateP,
    {free, OffsetReg}
) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        offset = BaseOffset,
        regs = Regs0
    } = State = pending_clear_all(StateP),
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
            jit_regs:unreachable(Regs0), avail_mask(State), 0
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
    State2 = pending_exit_cond(BlockFn(pending_enter_cond(State1))),
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
        State1#state.regs, State2#state.regs, avail_mask(State2)
    ),
    State2#state{stream = Stream3, regs = MergedRegs};
if_block(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Cond,
    BlockFn
) ->
    Offset = StreamModule:offset(Stream0),
    {State1, CC, BranchInstrOffset} = if_block_cond(State0, Cond),
    State2 = pending_exit_cond(BlockFn(pending_enter_cond(State1))),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    %% Patch the conditional branch instruction to jump to the end of the block
    BranchOffset = OffsetAfter - (Offset + BranchInstrOffset),
    NewBranchInstr = rewrite_branch_instruction(CC, BranchOffset),
    Stream3 = StreamModule:replace(Stream2, Offset + BranchInstrOffset, NewBranchInstr),
    MergedRegs = jit_regs:merge(
        State1#state.regs, State2#state.regs, avail_mask(State2)
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
    State2 = pending_exit_cond(BlockTrueFn(pending_enter_cond(State1))),
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
    State3 = pending_exit_cond(BlockFalseFn(pending_enter_cond(StateElse))),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    %% Patch the unconditional branch to jump to the end
    FinalJumpOffset = OffsetFinal - ElseJumpOffset,
    NewElseJumpInstr = jit_aarch64_asm:b(FinalJumpOffset),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset, NewElseJumpInstr),
    MergedRegs = jit_regs:merge(
        State2#state.regs, State3#state.regs, avail_mask(State3)
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
%% Unsigned above: jump over the block when Reg <= Val (unsigned). Used for
%% two-sided corridor checks folded into one compare via unsigned wrap.
if_block_cond(
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    {RegOrTuple, '(uint)>', Val}
) when is_integer(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, Val),
    I2 = jit_aarch64_asm:bcc(ls, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, ls, byte_size(I1)};
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
    {RegOrTuple, '<u', RegB}
) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_aarch64_asm:cmp(Reg, RegB),
    % cs (aka hs) = carry set = greater than or equal, unsigned
    I2 = jit_aarch64_asm:bcc(cs, 0),
    Code = <<
        I1/binary,
        I2/binary
    >>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = if_block_free_reg(RegOrTuple, State0),
    State2 = State1#state{stream = Stream1},
    {State2, cs, byte_size(I1)};
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
    %% Mask into the scratch register rather than clobbering Reg in place:
    %% Reg frequently caches a VM register's value (tag tests right after a
    %% load), and keeping it intact lets the next read of that VM register
    %% hit the jit_regs contents cache instead of reloading from the
    %% context. Freeing below retains the contents.
    OffsetBefore = StreamModule:offset(Stream0),
    State1 = op_imm(State0, and_, ?IP0_REG, Reg, Mask),
    Stream1 = State1#state.stream,
    % Compare with value
    I2 = jit_aarch64_asm:cmp(?IP0_REG, Val),
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

%% Load NumBits (8, 16 or 32) bits big-endian, zero-extended, from the address
%% in AddrReg into AddrReg itself. AArch64 loads are little-endian, so the 16-
%% and 32-bit cases byte-reverse after loading.
-spec load_be_unsigned(#state{}, aarch64_register(), 8 | 16 | 32) -> #state{}.
load_be_unsigned(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, AddrReg, NumBits
) when
    ?IS_GPR(AddrReg)
->
    Code =
        case NumBits of
            8 ->
                jit_aarch64_asm:ldrb(AddrReg, {AddrReg, 0});
            16 ->
                I1 = jit_aarch64_asm:ldrh(AddrReg, {AddrReg, 0}),
                I2 = jit_aarch64_asm:rev16(AddrReg, AddrReg),
                <<I1/binary, I2/binary>>;
            32 ->
                I1 = jit_aarch64_asm:ldr_w(AddrReg, {AddrReg, 0}),
                I2 = jit_aarch64_asm:rev32_w(AddrReg, AddrReg),
                <<I1/binary, I2/binary>>
        end,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, AddrReg),
    State#state{stream = Stream1, regs = Regs1}.

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
    #state{} = StateP,
    FuncPtrTuple,
    Args
) ->
    %% The callee can read any x register from ctx (and clobbers the
    %% register cache): all pending stores must persist.
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0 = pending_clear_all(StateP),
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
    %% In JIT_VARIANT_RELOC mode primitive calls are direct branches, so the
    %% native-interface table register is never used and need not be preserved.
    Reloc = (State0#state.variant band ?JIT_VARIANT_RELOC) =/= 0,
    SavedRegs =
        case Reloc of
            true ->
                [?LR_REG, ?CTX_REG, ?JITSTATE_REG | mask_to_list(UsedRegs1)];
            false ->
                [?LR_REG, ?CTX_REG, ?JITSTATE_REG, ?NATIVE_INTERFACE_REG | mask_to_list(UsedRegs1)]
        end,
    {SavedRegsOdd, Stream1} = push_registers(SavedRegs, StreamModule, Stream0),

    % Set up arguments following AArch64 calling convention
    State1 = set_args(State0#state{stream = Stream1}, Args),
    #state{stream = Stream2} = State1,

    {FuncPtrReg, Stream3} =
        case FuncPtrTuple of
            {free, Reg} ->
                {Reg, Stream2};
            {primitive, Primitive} when Reloc ->
                %% No table load: the call below is a direct, loader-relocated
                %% branch to the primitive.
                {{reloc, Primitive}, Stream2};
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

    % Call the function pointer: a direct BL (loader-relocated) in reloc mode,
    % otherwise BLR through the loaded pointer.
    {Stream4, Relocations1} =
        case FuncPtrReg of
            {reloc, PrimIdx} ->
                %% Single direct call. The loader binds it to the primitive when
                %% in branch range, otherwise to a per-primitive in-module veneer.
                BlOffset = StreamModule:offset(Stream3),
                {StreamModule:append(Stream3, jit_aarch64_asm:bl(0)), [
                    {BlOffset, PrimIdx} | State1#state.relocations
                ]};
            _ ->
                {
                    StreamModule:append(Stream3, jit_aarch64_asm:blr(FuncPtrReg)),
                    State1#state.relocations
                }
        end,

    % If r0 is in used regs, save it to another temporary register
    FreeGPMask = FreeMask band avail_mask(State0),
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
    AvailableRegs3 = AvailableRegs2 band avail_mask(State0),
    Regs1 = jit_regs:invalidate_all(Regs0),
    UsedRegs2 = UsedRegs1 bor ResultBit,
    {
        State1#state{
            stream = Stream6,
            relocations = Relocations1,
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
            avail_mask(State0) band (bnot (ParamMask bor NewUsedMask)),
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
move_to_vm_register(#state{regs = Regs0} = State0, Src, Dest) ->
    %% Pending-store bookkeeping: an x-register source may be re-read from
    %% memory by the emit below (cache misses are decided there), so its
    %% pending store must persist; an x-register destination supersedes a
    %% same-depth pending store to the same slot.
    StateA =
        case Src of
            {x_reg, SrcX} when is_integer(SrcX) -> pending_clear_x(State0, SrcX);
            _ -> State0
        end,
    State =
        case Dest of
            {x_reg, DestX} when is_integer(DestX) -> pending_elide_prev(StateA, DestX);
            _ -> StateA
        end,
    VmLoc = jit_regs:vm_dest_to_contents(Dest, ?MAX_REG),
    Regs1 =
        case VmLoc of
            unknown -> Regs0;
            _ -> jit_regs:invalidate_vm_loc(Regs0, VmLoc)
        end,
    State1 = move_to_vm_register_emit(State#state{regs = Regs1}, Src, Dest),
    State2 =
        case Dest of
            {x_reg, DestX2} when is_integer(DestX2) -> pending_note_store(State1, DestX2);
            _ -> State1
        end,
    case {Src, VmLoc} of
        {Reg, Contents} when is_atom(Reg), Contents =/= unknown ->
            #state{regs = Regs2} = State2,
            State2#state{regs = jit_regs:set_contents(Regs2, Reg, Contents)};
        _ ->
            State2
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
move_to_vm_register_emit(#state{} = StateP, {x_reg, X}, Dest) ->
    #state{regs = Regs0} = State0 = pending_clear_x(StateP, X),
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
    #state{} =
        State0,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(Reg) andalso is_integer(Index) ->
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State =
        pending_elide_prev(State0, X),
    Available = jit_regs:available_regs(Regs0),
    Temp = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Temp, {Reg, Index * ?WORD_SIZE}),
    I2 = jit_aarch64_asm:str(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:set_contents(Regs1, Temp, {x_reg, X}),
    pending_note_store(State#state{stream = Stream1, regs = Regs2}, X);
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
    #state{} = State0,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State = pending_elide_prev(State0, X),
    I1 = jit_aarch64_asm:ldr(IndexReg, {Reg, IndexReg, lsl, 3}),
    I2 = jit_aarch64_asm:str(IndexReg, ?X_REG(X)),
    Bit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, IndexReg),
    pending_note_store(
        State#state{
            stream = Stream1,
            regs = jit_regs:free_reg(Regs2, Bit)
        },
        X
    );
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
    #state{} = StateP,
    {x_reg, X},
    Contents
) when
    X < ?MAX_REG
->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State = pending_clear_x(StateP, X),
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
    #state{} = StateP, {x_reg, X}, RegDst
) when
    X < ?MAX_REG
->
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State =
        pending_clear_x(StateP, X),
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
            %% ValReg was free but may cache stale contents (a following
            %% loop back-edge reconciliation reads the cache).
            Regs0b = jit_regs:invalidate_reg(Regs0, ValReg),
            %% Reserve BaseReg with y_regs_base contents so it isn't reused.
            Regs1 = jit_regs:set_contents(Regs0b, BaseReg, y_regs_base),
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
    #state{} = StateP,
    Label
) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels,
        regs = Regs0
    } = State = pending_clear_all(StateP),
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
    #state{} = StateP
) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        regs = Regs0
    } = State = pending_clear_all(StateP),
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
    %% Execution can resume here from the scheduler loop: pending stores
    %% made before this point must persist, and native registers are dead
    %% on the resume path — cached contents must not be trusted past this
    %% point (loop back-edge reconciliation reads the cache).
    State1 = pending_clear_all(State),
    State1#state{regs = jit_regs:invalidate_all(State1#state.regs)}.

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
%% @doc Load jit_state->cp_base (module_index << 24) into a fresh register.
%% One load instead of get_module_index's dependent module->index chain;
%% used by the intra-module return check.
-spec get_cp_base(state()) -> {state(), aarch64_register()}.
get_cp_base(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_CPBASE),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, Bit)}, Reg}.

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

%% @doc Load the 32-bit global atom index for a module-local atom id, i.e.
%% jit_state->module->local_atoms_to_global_table[AtomIndex], into a fresh
%% register. The shared jit:get_module_atom_term/3 applies the term tag. This is
%% hot (every non-default atom literal access; hundreds of millions of times in
%% the compiler), so inlining these loads avoids the primitive-call overhead
%% (table load + indirect branch + register save/restore) per access.
-spec get_module_atom_index(state(), non_neg_integer()) -> {state(), aarch64_register()}.
get_module_atom_index(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    AtomIndex
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    %% Reg = jit_state->module
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_MODULE),
    %% Reg = module->local_atoms_to_global_table
    I2 = jit_aarch64_asm:ldr(Reg, ?MODULE_LOCAL_ATOMS_TABLE(Reg)),
    %% Reg = local_atoms_to_global_table[AtomIndex] (a 32-bit global atom index,
    %% zero-extended into the 64-bit register). The entries are 4 bytes wide; the
    %% scaled LDR (32-bit) immediate reaches an offset of 16380, beyond which the
    %% offset is added to the base first.
    Offset = AtomIndex * 4,
    LoadGid =
        case Offset =< 16380 of
            true ->
                jit_aarch64_asm:ldr_w(Reg, {Reg, Offset});
            false ->
                <<
                    (jit_aarch64_asm:add(Reg, Reg, Offset))/binary,
                    (jit_aarch64_asm:ldr_w(Reg, {Reg, 0}))/binary
                >>
        end,
    Code = <<I1/binary, I2/binary, LoadGid/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, {atom_index, AtomIndex}),
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
-spec add_overflow(state(), aarch64_register(), aarch64_register() | 0..4095) -> state().
add_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_atom(Val) ->
    I1 = jit_aarch64_asm:adds(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
add_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_integer(Val), Val >= 0, Val =< 4095 ->
    I1 = jit_aarch64_asm:adds(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc Subtract Val from Reg in place, setting condition flags. See
%% add_overflow/3.
%% @end
%%-----------------------------------------------------------------------------
-spec sub_overflow(state(), aarch64_register(), aarch64_register() | 0..4095) -> state().
sub_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_atom(Val) ->
    I1 = jit_aarch64_asm:subs(Reg, Reg, Val),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
sub_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_integer(Val), Val >= 0, Val =< 4095 ->
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
    %% Load the fp register array pointer (jit_state->fr), compute the operation in
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
%% offset one word from the untagged boxed pointer. BoxedReg is clobbered by
%% the in-place untag, so it must be passed as {free, Reg}: it is invalidated
%% (any cached vm-register association would be stale) and returned to the
%% pool.
-spec float_conv_float(state(), {free, aarch64_register()}, non_neg_integer()) -> state().
float_conv_float(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    {free, BoxedReg},
    FPRegIndex
) ->
    Avail0 = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail0 band (bnot reg_bit(BoxedReg))),
    %% Clear the 2 primary (boxed) tag bits to get the boxed pointer, load the
    %% double from boxed_ptr[1], load fr base, store to fr[FPRegIndex].
    I1 = jit_aarch64_asm:and_(BoxedReg, BoxedReg, bnot ?TERM_PRIMARY_MASK),
    I2 = jit_aarch64_asm:ldr_d(d0, {BoxedReg, ?WORD_SIZE}),
    I3 = jit_aarch64_asm:ldr(Temp, ?FP_REGS),
    I4 = jit_aarch64_asm:str_d(d0, {Temp, ?FP_REG_OFFSET(State0, FPRegIndex)}),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    Regs2 = jit_regs:invalidate_reg(Regs1, BoxedReg),
    Regs3 = jit_regs:free_reg(Regs2, reg_bit(BoxedReg)),
    State0#state{stream = Stream1, regs = Regs3}.

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

%%-----------------------------------------------------------------------------
%% Deferred x-register store elision (jit_liveness pass B).
%%
%% Every store to ctx->x[N] is recorded as "pending". When a NEW store to
%% the same slot is emitted at the same conditional depth and nothing in
%% between could have observed the slot in memory, the earlier str is
%% rewritten to a nop: all intermediate consumers provably took the value
%% from the register cache. Observation points conservatively drop
%% pendings: memory reads of the slot, any call (callees read ctx->x),
%% labels (unknown predecessors), backward jumps, and branches to labels
%% whose live-in mask (from jit_liveness) contains the register.
%% Conditional bodies (if_block) only elide stores made at their own depth.
%%-----------------------------------------------------------------------------
-spec set_live_masks(
    state(), {#{non_neg_integer() => non_neg_integer()}, #{non_neg_integer() => true}}
) -> state().
set_live_masks(State, {Masks, CallTargets}) ->
    State#state{live_masks = Masks, call_targets = CallTargets}.

%% This backend opens call_only blocks with loop-header register residency
%% (site-specific reconciliation); jit.erl must not share whole op_call_last
%% blocks across sites (see OP_CALL_LAST's HotCapable).
-spec supports_loop_residency() -> boolean().
supports_loop_residency() -> true.

%% aarch64 stores are always one 4-byte instruction.
-include("jit_backend_pending_impl.hrl").

%% The store is the last emitted 4-byte instruction, so it started 4 bytes
%% before the current offset.
pending_note_store(#state{stream_module = SM, stream = St} = State, X) ->
    pending_note_store(State, X, SM:offset(St) - 4).

pending_nop_bytes(4) ->
    jit_aarch64_asm:nop().

%% Load the fp register array pointer (jit_state->fr) into a freshly allocated
%% register and return it, so the caller can test it for NULL and only call
%% the ensure_fpregs primitive (the malloc) when it has not been allocated yet.
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

%% First half of a dense select_val jump table: IP0 = Src - MinTagged; when
%% (unsigned) above Bound the next (4-byte) instruction runs — the caller
%% emits the branch to the default label there — otherwise it is skipped
%% and execution continues at jump_table_dispatch.
-spec jump_table_range_check(state(), aarch64_register(), non_neg_integer(), 0..4095) -> state().
jump_table_range_check(
    #state{stream_module = StreamModule} = State0, SrcReg, MinTagged, Bound
) ->
    State1 = op_imm(State0, sub, ?IP0_REG, SrcReg, MinTagged),
    I2 = jit_aarch64_asm:cmp(?IP0_REG, Bound),
    I3 = jit_aarch64_asm:bcc(ls, 8),
    State1#state{stream = StreamModule:append(State1#state.stream, <<I2/binary, I3/binary>>)}.

%% Second half: computed branch into the table of 4-byte b instructions the
%% caller emits right after this. IP0 holds the tagged difference (a small
%% int delta is value * 16); each table slot is 4 bytes, so the byte offset
%% is IP0 >> 2.
-spec jump_table_dispatch(state()) -> state().
jump_table_dispatch(#state{stream_module = StreamModule, stream = Stream0} = State0) ->
    I1 = jit_aarch64_asm:lsr(?IP0_REG, ?IP0_REG, 2),
    I2 = jit_aarch64_asm:adr(r17, 12),
    I3 = jit_aarch64_asm:add(r17, r17, ?IP0_REG),
    I4 = jit_aarch64_asm:br(r17),
    State0#state{
        stream = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>)
    }.

%% In-place variable shifts (amount in a register, callers bound-check it:
%% the hardware takes the amount mod 64).
-spec shift_right_arith_reg(state(), aarch64_register(), aarch64_register()) -> state().
shift_right_arith_reg(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, ShiftReg
) ->
    I1 = jit_aarch64_asm:asrv(Reg, Reg, ShiftReg),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = StreamModule:append(Stream0, I1), regs = Regs1}.

-spec shift_left_reg(state(), aarch64_register(), aarch64_register()) -> state().
shift_left_reg(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, ShiftReg
) ->
    I1 = jit_aarch64_asm:lslv(Reg, Reg, ShiftReg),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = StreamModule:append(Stream0, I1), regs = Regs1}.

%% Bump-allocate NWords terms from the context heap, returning a freshly
%% allocated register holding the pointer to the first allocated word. The
%% space is already reserved by the preceding test_heap/allocate (BEAM
%% bytecode guarantees it), so this is memory_heap_alloc inlined: no bounds
%% check, just a heap_ptr load/add/store.
-spec heap_bump_alloc(state(), pos_integer()) -> {state(), aarch64_register()}.
heap_bump_alloc(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    NWords
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Tmp = first_avail(Available band (bnot reg_bit(Reg))),
    I1 = jit_aarch64_asm:ldr(Reg, ?HEAP_PTR),
    I2 = jit_aarch64_asm:add(Tmp, Reg, NWords * ?WORD_SIZE),
    I3 = jit_aarch64_asm:str(Tmp, ?HEAP_PTR),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Tmp),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

%% Available heap memory in bytes (ctx->e - ctx->heap.heap_ptr), in a freshly
%% allocated register, for inline allocate/test_heap room checks.
-spec read_avail_heap_memory(state()) -> {state(), aarch64_register()}.
read_avail_heap_memory(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Tmp = first_avail(Available band (bnot reg_bit(Reg))),
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:ldr(Tmp, ?HEAP_PTR),
    I3 = jit_aarch64_asm:sub(Reg, Reg, Tmp),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Tmp),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

%% Load ctx->heap.root->next into a freshly allocated register, so deallocate
%% can test for pending heap fragments inline and only call the primitive
%% (which compacts them) when there are any.
-spec read_heap_fragments(state()) -> {state(), aarch64_register()}.
read_heap_fragments(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    I1 = jit_aarch64_asm:ldr(Reg, {?CTX_REG, 16#8}),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, 0}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

%% ctx->heap.root->next | (ctx->heap.heap_end ^ ctx->shrink_probe_heap_end),
%% in a freshly allocated register: zero exactly when no heap fragments are
%% pending and the shrink probe already ran for this root block, i.e. when a
%% test_heap whose free space exceeds the shrink corridor can skip the
%% primitive call entirely (mirrors the probe short-circuit in jit_test_heap).
-spec read_shrink_probe_mismatch(state()) -> {state(), aarch64_register()}.
read_shrink_probe_mismatch(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Tmp1 = first_avail(Available band (bnot reg_bit(Reg))),
    Tmp2 = first_avail(Available band (bnot (reg_bit(Reg) bor reg_bit(Tmp1)))),
    I1 = jit_aarch64_asm:ldr(Reg, {?CTX_REG, 16#8}),
    I2 = jit_aarch64_asm:ldr(Reg, {Reg, 0}),
    I3 = jit_aarch64_asm:ldr(Tmp1, {?CTX_REG, 16#20}),
    I4 = jit_aarch64_asm:ldr(Tmp2, {?CTX_REG, 16#1A0}),
    I5 = jit_aarch64_asm:eor(Tmp1, Tmp1, Tmp2),
    I6 = jit_aarch64_asm:orr(Reg, Reg, Tmp1),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary>>
    ),
    Regs1 = jit_regs:invalidate_reg(
        jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Tmp1), Tmp2
    ),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

%% Push a stack frame, the fast path of the allocate opcode once the room
%% check passed: e -= (StackNeed + 1) words; e[StackNeed] = ctx->cp. The
%% 64-bit cp occupies a single stack slot.
-spec allocate_frame_fast(state(), non_neg_integer()) -> state().
allocate_frame_fast(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    StackNeed
) ->
    Available = jit_regs:available_regs(Regs0),
    Reg = first_avail(Available),
    Tmp = first_avail(Available band (bnot reg_bit(Reg))),
    I1 = jit_aarch64_asm:ldr(Reg, ?Y_REGS),
    I2 = jit_aarch64_asm:sub(Reg, Reg, (StackNeed + 1) * ?WORD_SIZE),
    I3 = jit_aarch64_asm:str(Reg, ?Y_REGS),
    I4 = jit_aarch64_asm:ldr(Tmp, ?CP),
    I5 = jit_aarch64_asm:str(Tmp, {Reg, StackNeed * ?WORD_SIZE}),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>
    ),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Tmp),
    State#state{stream = Stream1, regs = Regs1}.

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
    #state{} = StateP
) ->
    %% The out-of-reductions path leaves through the scheduler; pending
    %% stores must persist.
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State0 = pending_clear_all(StateP),
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
    #state{} = StateP,
    Label
) ->
    %% Control transfers into the callee, which reads exactly the x
    %% registers in its entry label's live-in mask (its arguments): other
    %% pending stores survive the call — the callee neither reads nor
    %% roots them (they are beyond its Live counts).
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels,
        regs = Regs0
    } = StateF = pending_filter_label(StateP, Label),
    %% Loop-header hot entry: emit a site-specific reconciliation of the
    %% recorded bindings BEFORE the shared part of the block. jit.erl's
    %% tail cache reuses this block for other call_only/call_last/jump
    %% sites via jump_to_offset, which intercepts the entry offset (see
    %% recon_blocks) and emits its own reconciliation — so everything
    %% from SharedOffset on must not depend on this site's register state.
    EntryOffset = StreamModule:offset(Stream0),
    {TargetOffset, State0} =
        case StateF#state.loop_entries of
            #{Label := {HotOffset, Bindings}} ->
                StateR = emit_backedge_recon(StateF, Bindings),
                SharedOffset = StreamModule:offset(StateR#state.stream),
                {HotOffset, StateR#state{
                    recon_blocks = (StateR#state.recon_blocks)#{
                        EntryOffset => {Label, SharedOffset}
                    }
                }};
            _ ->
                {maps:get(Label, Labels, unknown), StateF}
        end,
    %% On hot paths the reduction scratch must not clobber a reconciled
    %% binding register (available_regs includes free-but-cached regs and
    %% Temp is not invalidated below): use IP0 there.
    Temp =
        case StateF#state.loop_entries of
            #{Label := _} -> ?IP0_REG;
            _ -> first_avail(jit_regs:available_regs(Regs0))
        end,
    % Load reduction count
    I1 = jit_aarch64_asm:ldr_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    % Decrement reduction count
    I2 = jit_aarch64_asm:subs(Temp, Temp, 1),
    % Store back the decremented value
    I3 = jit_aarch64_asm:str_w(Temp, ?JITSTATE_REDUCTIONCOUNT),
    StreamR = StreamModule:append(
        State0#state.stream, <<I1/binary, I2/binary, I3/binary>>
    ),
    BNEOffset = StreamModule:offset(StreamR),
    case TargetOffset of
        LabelOffset when
            is_integer(LabelOffset) andalso
                LabelOffset - BNEOffset >= -1048576 andalso LabelOffset - BNEOffset < 1048576
        ->
            % Label is already known and in bcc range, emit direct branch
            Rel = LabelOffset - BNEOffset,
            I4 = jit_aarch64_asm:bcc(ne, Rel),
            Stream2 = StreamModule:append(StreamR, I4),
            State1 = State0#state{stream = Stream2};
        LabelOffset when is_integer(LabelOffset) ->
            % Label is beyond bcc's ±1MB range: skip over an unconditional
            % branch (±128MB) with the inverted condition
            I4 = jit_aarch64_asm:bcc(eq, 8),
            I5 = jit_aarch64_asm:b(LabelOffset - (BNEOffset + 4)),
            Stream2 = StreamModule:append(StreamR, <<I4/binary, I5/binary>>),
            State1 = State0#state{stream = Stream2};
        unknown ->
            % Label not yet known: emit the far-capable pair so the patch
            % fits whatever the final distance is
            I4 = jit_aarch64_asm:bcc(eq, 8),
            I5 = jit_aarch64_asm:b(0),
            BrEntry = {BNEOffset + 4, b},
            ExistingBrs = maps:get(Label, Branches, []),
            Stream2 = StreamModule:append(StreamR, <<I4/binary, I5/binary>>),
            State1 = State0#state{
                stream = Stream2,
                branches = Branches#{Label => [BrEntry | ExistingBrs]},
                %% Forward block: targets the cold entry even if the label
                %% later gains a hot one — record so reuse sites can emit
                %% a fresh hot block instead (see jump_to_offset).
                cold_call_blocks = (State0#state.cold_call_blocks)#{EntryOffset => Label}
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

%% Call a resolving primitive that returns either the callee's native entry
%% point with bit 0 set — branch to it directly, skipping the scheduler-loop
%% round trip — or a Context * (bit 0 clear) to return to the scheduler loop
%% (the saved lr still points there; primitives preserve it). cp is set to
%% the instruction after the dispatch sequence, like call_primitive_with_cp.
-spec call_primitive_with_cp_direct(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_with_cp_direct(State0, Primitive, Args) ->
    {State1, RewriteOffset, RewriteSize} = set_cp(State0),
    {State2, ResultReg} = call_primitive(State1, Primitive, Args),
    #state{stream_module = StreamModule, stream = Stream2} = State2,
    %% Dispatch on the primitive's tagged result:
    %%   bit 0 clear: Context * — return to the scheduler loop
    %%   value 3 (STAY): the continuation is this site's cp target — fall
    %%     through (skips the callee's cp->native-pc resolution entirely)
    %%   bit 0 set: tagged native entry — branch to it
    I1 = jit_aarch64_asm:tbnz(ResultReg, 0, 12),
    I2 = jit_aarch64_asm:mov(r0, ResultReg),
    I3 = jit_aarch64_asm:ret(),
    I4 = jit_aarch64_asm:tbnz(ResultReg, 1, 12),
    I5 = jit_aarch64_asm:and_(ResultReg, ResultReg, bnot 3),
    I6 = jit_aarch64_asm:br(ResultReg),
    Stream3 = StreamModule:append(
        Stream2, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary>>
    ),
    State3 = free_native_register(State2#state{stream = Stream3}, ResultReg),
    rewrite_cp_offset(State3, RewriteOffset, RewriteSize).

%% Tail-position variant of call_primitive_with_cp_direct: no cp is set (the
%% callee returns to the caller's caller), but the branch-or-return dispatch
%% on the primitive's tagged result is the same. Code after this is
%% unreachable from this site.
-spec call_primitive_direct(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_direct(State0, Primitive, Args) ->
    {State1, ResultReg} = call_primitive(State0, Primitive, Args),
    #state{stream_module = StreamModule, stream = Stream1} = State1,
    I1 = jit_aarch64_asm:tbnz(ResultReg, 0, 12),
    I2 = jit_aarch64_asm:mov(r0, ResultReg),
    I3 = jit_aarch64_asm:ret(),
    I4 = jit_aarch64_asm:and_(ResultReg, ResultReg, bnot 1),
    I5 = jit_aarch64_asm:br(ResultReg),
    Stream2 = StreamModule:append(
        Stream1, <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>
    ),
    State2 = free_native_register(State1#state{stream = Stream2}, ResultReg),
    State2#state{regs = jit_regs:invalidate_all(State2#state.regs)}.

%% @private
-spec set_cp(state()) -> {state(), non_neg_integer(), 4 | 8}.
set_cp(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Offset = StreamModule:offset(Stream0),
    % cp = jit_state->cp_base (module_index << 24) | (return offset << 2);
    % the offset mov is rewritten once the return point is known.
    I1 = jit_aarch64_asm:ldr(Reg, ?JITSTATE_CPBASE),
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
    %% Reg was free but may cache stale contents; it now holds a transient
    %% cp value.
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State0#state{stream = Stream1, regs = Regs1}, MOVOffset, RewriteSize}.

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
add_label(#state{} = StateP, Label) ->
    #state{stream_module = StreamModule, stream = Stream, regs = Regs0} =
        State =
        pending_flush_label(StateP, Label),
    Offset = StreamModule:offset(Stream),
    Regs1 = jit_regs:invalidate_all(Regs0),
    State1 = add_label(State#state{regs = Regs1}, Label, Offset),
    maybe_emit_loop_preload(State1, Label).

%% Loop-header residency (cold entry): when a label is a direct call
%% target (function entry / loop header, per pass A), its live-in set is
%% small and no forward branch targets it, preload those x registers right
%% after the label. The body then compiles against the cached bindings and
%% backward call_only sites can reconcile and branch past the loads (hot
%% entry). Yield resumes and jump-table entries use the label itself, so
%% memory stays authoritative on every other path.
maybe_emit_loop_preload(#state{live_masks = undefined} = State, _Label) ->
    State;
maybe_emit_loop_preload(
    #state{live_masks = Masks, call_targets = CT, branches = Branches} = State0, Label
) when is_integer(Label) ->
    Mask = maps:get(Label, Masks, -1),
    Eligible =
        is_map_key(Label, CT) andalso
            Mask > 0 andalso Mask < 16#10000 andalso
            not maps:is_key(Label, Branches),
    Xs = [X || X <- lists:seq(0, 15), Mask band (1 bsl X) =/= 0],
    case Eligible andalso length(Xs) =< 3 of
        true ->
            {State1, RevBindings} = lists:foldl(
                fun(X, {Acc0, Bs}) ->
                    {Acc1, Reg} = move_to_native_register(Acc0, {x_reg, X}),
                    {Acc1, [{X, Reg} | Bs]}
                end,
                {State0, []},
                Xs
            ),
            Bindings = lists:reverse(RevBindings),
            State2 = free_native_registers(State1, [Reg || {_X, Reg} <- Bindings]),
            #state{stream_module = SM, stream = Stream1, loop_entries = LE} = State2,
            HotOffset = SM:offset(Stream1),
            State2#state{loop_entries = LE#{Label => {HotOffset, Bindings}}};
        false ->
            State0
    end;
maybe_emit_loop_preload(State, _Label) ->
    State.

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
    #state{} = StateP,
    Label,
    LabelOffset
) when is_integer(Label) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        jump_table_start = JumpTableStart,
        branches = Branches,
        labels = Labels
    } = State = pending_clear_all(StateP),
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
add_label(#state{} = StateP, Label, Offset) ->
    #state{labels = Labels} = State = pending_clear_all(StateP),
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
