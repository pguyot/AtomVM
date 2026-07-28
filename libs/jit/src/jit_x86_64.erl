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

-module(jit_x86_64).

-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    flush/1,
    set_live_masks/2,
    supports_loop_residency/0,
    supports_inline_tuple2_eq/0,
    add_deferred_raise/5,
    take_deferred_raises/1,
    take_deferred_stubs/1,
    map_get_stub_call/3,
    compare_stub_call/3,
    reset_regs_fresh/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    jump_table_range_check/4,
    jump_table_dispatch/1,
    update_branches/1,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    call_primitive_direct/3,
    call_ext_with_cp_direct/4,
    call_ext_last_direct/5,
    call_fun_with_cp_direct/3,
    return_if_not_equal_to_ctx/2,
    return_cross_module/2,
    jump_to_label/2,
    jump_to_label_cond/3,
    jump_to_continuation/2,
    jump_to_offset/2,
    if_block/3,
    if_else_block/4,
    shift_right/3,
    shift_right_arith/3,
    shift_right_arith_reg/3,
    shift_left/3,
    shift_left_reg/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_native_register/3,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    move_to_array_element/5,
    load_be_unsigned/3,
    store_be/4,
    set_bs/2,
    get_bs/1,
    get_bs_offset/1,
    set_bs_offset/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    move_imported_gcbif_to_native_register/3,
    move_imported_bif_to_native_register/2,
    get_cp_base/1,
    get_module_index/1,
    get_module_atom_index/2,
    get_list_head_tail/4,
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
    read_avail_heap_memory/1,
    heap_bump_alloc/2,
    read_heap_fragments/1,
    allocate_frame_fast/2,
    term_from_float_inline/2,
    supports_vm_reg_cond/0,
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

-define(ASSERT(Expr), true = Expr).

%% System V X86_64 calling conventions which we apply here.
%% (Integer) parameters : rdi, rsi, rdx, rcx, r8, r9
%% (Integer) result : rax
%%
%% Function is called as (Context *, JITState *, ModuleNativeInterface *) so:
%% Context * is rdi
%% JITState * is rsi
%% ModuleNativeInterface * is rdx
%%
%% rax, r11, r10, r9, r8 and rcx can be used as scratch registers.
%% rdi / rsi / rdx are pushed to stack before calling a primitive and popped back.
%% when returning (some push call pop push call pop sequences could be optimized)

-type x86_64_register() ::
    rax
    | rcx
    | rdx
    | rsi
    | rdi
    | r8
    | r9
    | r10
    | r11.

-define(IS_GPR(Reg),
    (Reg =:= rax orelse Reg =:= rcx orelse Reg =:= rdx orelse Reg =:= rsi orelse Reg =:= r8 orelse
        Reg =:= r9 orelse Reg =:= r10 orelse Reg =:= r11)
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
    %% Write-through x-store elision (jit_backend_pending_impl.hrl): per-label
    %% live-in masks (jit_liveness pass A, via set_live_masks/2), pending
    %% stores (x index -> {stream offset, store width, cond depth}) and the
    %% current conditional-emission depth.
    live_masks = undefined :: undefined | #{non_neg_integer() => non_neg_integer()},
    pending_x = #{} ::
        #{non_neg_integer() => {non_neg_integer(), non_neg_integer(), non_neg_integer()}},
    cond_depth = 0 :: non_neg_integer(),
    %% Outlined raise blocks, newest first (see add_deferred_raise/5); emitted
    %% at the module tail by jit:flush_deferred_raises/2.
    deferred_raises = [] :: [{reference(), non_neg_integer(), non_neg_integer(), [arg()]}],
    %% select_val jump table under construction: the index register, the
    %% offset of the range check's jbe (patched by jump_table_dispatch/1) and
    %% the slot count. jump_table_slots counts down the slots still to emit,
    %% which jump_to_label/2 must emit at a fixed width.
    jump_table_index = undefined ::
        undefined | {x86_64_register(), non_neg_integer(), pos_integer()},
    jump_table_slots = 0 :: non_neg_integer(),
    %% Per-module callable stubs: Key -> label reference (so every site shares
    %% one body), plus the bodies still to emit at the module tail.
    stubs = #{} :: #{term() => reference()},
    deferred_stubs = [] :: [{reference(), fun((state()) -> state())}]
}).

-type state() :: #state{}.
-type vm_register() ::
    {x_reg, non_neg_integer()}
    | {x_reg, extra}
    | {y_reg, non_neg_integer()}
    | {ptr, x86_64_register()}
    | {fp_reg, non_neg_integer()}.
-type value() :: integer() | vm_register() | x86_64_register() | {ptr, x86_64_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_x86_64_register() :: x86_64_register() | {free, x86_64_register()}.

-type condition() ::
    {x86_64_register(), '<', integer()}
    | {maybe_free_x86_64_register(), '<', x86_64_register()}
    | {maybe_free_x86_64_register(), '<u', x86_64_register()}
    | {integer(), '<', maybe_free_x86_64_register()}
    | {maybe_free_x86_64_register(), '==', x86_64_register() | integer()}
    | {maybe_free_x86_64_register(), '!=', x86_64_register() | integer()}
    | {'(int)', maybe_free_x86_64_register(), '==', integer()}
    | {'(int)', maybe_free_x86_64_register(), '!=', x86_64_register() | integer()}
    | {'(bool)', maybe_free_x86_64_register(), '==', false}
    | {'(bool)', maybe_free_x86_64_register(), '!=', false}
    | {maybe_free_x86_64_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, x86_64_register()}, '==', {free, x86_64_register()}}.

-define(WORD_SIZE, 8).

% Following offsets are verified with static asserts in jit.c
% ctx->e is 0x50
% ctx->x is 0x58
% ctx->cp is 0xE0
% ctx->bs is 0xE8
% ctx->bs_offset is 0xF0
% jit_state->fr is 0x18
%% Pinned-register convention: ctx, jit_state, the primitives table and
%% ctx->heap.heap_ptr / ctx->e live in callee-saved registers, seeded once
%% per C->native crossing by the dispatch loop (opcodesswitch.h). C
%% primitives preserve them per the SysV ABI, so generated code never saves,
%% restores or reloads them around calls; rdi/rsi/rdx become scratch.
%% Base-register assignment follows ModRM cost: rbx/r14/r15 encode with a
%% plain ModRM byte and serve the frequent bases (table, ctx, e); r13 forces
%% a disp8 at offset 0 and serves the rare jit_state base; r12 would need a
%% SIB byte but hp is never used as a base.
-define(CTX_REG, r14).
-define(JITSTATE_REG, r13).
-define(NATIVE_INTERFACE_REG, rbx).
%% hp/e mutate (inline allocs, allocate/deallocate, GC): written back to ctx
%% before every C call and reloaded after calls that return, except around
%% primitives listed in jit_prim_pure.hrl.
-define(HP_REG, r12).
-define(E_REG, r15).
-define(Y_REGS, {16#50, ?CTX_REG}).
-define(X_REG(N), {16#58 + (N * ?WORD_SIZE), ?CTX_REG}).
-define(CP, {16#E0, ?CTX_REG}).
-define(FP_REGS, {16#18, ?JITSTATE_REG}).
-define(HEAP_PTR, {16#18, ?CTX_REG}).
-define(FP_REG_OFFSET(State, F),
    (F *
        case (State)#state.variant band ?JIT_VARIANT_FLOAT32 of
            0 -> 8;
            _ -> 4
        end)
).
-define(BS, {16#E8, ?CTX_REG}).
-define(BS_OFFSET, {16#F0, ?CTX_REG}).
-define(JITSTATE_MODULE, {0, ?JITSTATE_REG}).
-define(JITSTATE_CONTINUATION, {16#8, ?JITSTATE_REG}).
-define(JITSTATE_REMAINING_REDUCTIONS, {16#10, ?JITSTATE_REG}).
% jit_state->cp_base: module_index << 24, precomputed by the C side wherever
% jit_state->module is set (see _Static_assert in jit.c), so building a cp or
% a catch term is one load instead of load-module/load-index/shift.
-define(JITSTATE_CPBASE, {16#28, ?JITSTATE_REG}).
-define(PRIMITIVE(N), {N * ?WORD_SIZE, ?NATIVE_INTERFACE_REG}).
-define(MODULE_INDEX(ModuleReg), {0, ModuleReg}).
% module->native_code (see _Static_assert in jit.c); used by the inline
% cross-module return fast path.
-define(MODULE_NATIVE_CODE, 16#78).
% module->fun_table (see _Static_assert in jit.c); used by the inline
% call_fun fast path. Entries are 24 bytes after a 12-byte header, holding
% big-endian 32-bit fields: arity (+16), label (+20), n_freeze (+28) -- see
% module_get_fun in module.h.
-define(MODULE_FUN_TABLE, 16#30).
-define(FUN_TABLE_ARITY, 16).
-define(FUN_TABLE_LABEL, 20).
-define(FUN_TABLE_N_FREEZE, 28).
% Bytes per entry of the module jump table (one jmp rel32 per label), which is
% emitted at offset 0 of the module's native code (see jump_table/2).
-define(JUMP_TABLE_ENTRY_SIZE, 5).
% global->atom_table, atom_table->index_to_node, HNode->sort_key (see
% _Static_assert in jit.c); used by the inline atom-vs-atom compare-stub fast
% path. atom_table is the second field of GlobalContext on purpose, so this
% offset holds in every build configuration.
-define(GLOBAL_ATOM_TABLE, 16#8).
-define(ATOM_TABLE_INDEX_TO_NODE, 16#20).
-define(HNODE_SORT_KEY, 16#10).
-define(MODULE_LOCAL_ATOMS_TABLE(ModuleReg), {16#D8, ModuleReg}).
% Offsets for inlining the imported-BIF pointer resolution at gc_bif call sites.
% Kept in sync with src/libAtomVM/jit.c via _Static_assert.
-define(MODULE_IMPORTED_FUNCS, 16#90).
-define(CTX_EXTENDED_X_REGS, 16#F8).
% struct Bif { struct ExportedFunction base; union { BifImpl0 bif0_ptr; ... }; }
% base is at offset 0, so EXPORTED_FUNCTION_TO_BIF(f) == f and bif0_ptr is here.
-define(BIF_BIF0_PTR, 16#8).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-define(PARAMETER_REGS, [rdi, rsi, rdx, rcx, r8, r9]).

-define(REG_BIT_RAX, (1 bsl 0)).
-define(REG_BIT_RCX, (1 bsl 1)).
-define(REG_BIT_RDX, (1 bsl 2)).
-define(REG_BIT_RSI, (1 bsl 3)).
-define(REG_BIT_RDI, (1 bsl 4)).
-define(REG_BIT_R8, (1 bsl 5)).
-define(REG_BIT_R9, (1 bsl 6)).
-define(REG_BIT_R10, (1 bsl 7)).
-define(REG_BIT_R11, (1 bsl 8)).
%% Callee-saved pinned registers: never in the available/used masks; the bits
%% exist so args_regs/regs_to_mask can pass over them.
-define(REG_BIT_RBX, (1 bsl 9)).
-define(REG_BIT_R12, (1 bsl 10)).
-define(REG_BIT_R13, (1 bsl 11)).
-define(REG_BIT_R14, (1 bsl 12)).
-define(REG_BIT_R15, (1 bsl 13)).

-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_RAX bor ?REG_BIT_R11 bor ?REG_BIT_R10 bor ?REG_BIT_R9 bor ?REG_BIT_R8 bor
        ?REG_BIT_RCX bor ?REG_BIT_RDX bor ?REG_BIT_RSI bor ?REG_BIT_RDI)
).
-define(SCRATCH_REGS_MASK,
    (?REG_BIT_RDI bor ?REG_BIT_RSI bor ?REG_BIT_RDX bor ?REG_BIT_RCX bor ?REG_BIT_R8 bor
        ?REG_BIT_R9 bor ?REG_BIT_R10 bor ?REG_BIT_R11)
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
%% @doc Flush the current state (unused on x86-64)
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
    Stream1 = StreamModule:append(Stream0, <<16#CC>>),
    State#state{stream = Stream1}.

%% Native-register allocation bookkeeping (used_regs/1, available_regs/1,
%% free_native_registers/2, free_native_register/2, assert_all_native_free/1,
%% first_avail/1, mask_to_list/1, args_regs/1, prepare_call_scratch/1) is shared
%% across the register-based backends and flows through jit_regs.
-define(FIRST_AVAIL_REGS, [rax, r11, r10, r9, r8, rcx, rdx, rsi, rdi]).
-define(MASK_TO_LIST_REGS, [rcx, r8, r9, r10, r11, rax, rdx, rsi, rdi]).
-define(JITSTATE_ARG_REG, ?JITSTATE_REG).
-include("jit_backend_regs_impl.hrl").
-include("jit_backend_pending_impl.hrl").
-include("jit_prim_pure.hrl").

%%-----------------------------------------------------------------------------
%% @doc Receive the per-label live-in x-register masks (jit_liveness pass A)
%% and activate write-through store elision. The call-target set is unused on
%% x86-64 (no loop-header residency).
%% @end
%%-----------------------------------------------------------------------------
-spec set_live_masks(
    state(), {#{non_neg_integer() => non_neg_integer()}, #{non_neg_integer() => true}}
) -> state().
set_live_masks(State, {Masks, _CallTargets}) ->
    State#state{live_masks = Masks}.

%% x86-64 has no loop-header register residency: residency needs a bank of
%% callee-saved registers to hold VM x0-x3 across the loop back-edge, and all
%% but one of the six SysV callee-saved registers are already pinned (rbx,
%% r12-r15; rbp is the frame pointer). So jit.erl may share whole
%% op_call_last blocks across sites.
-spec supports_loop_residency() -> boolean().
supports_loop_residency() -> false.

%% Capability marker: the generic layer inlines exact 2-tuple (in)equality
%% and the 2-tuple ordering fast path (see jit:emit_tuple2_exact_eq/7 and
%% jit:emit_tuple2_order_fastpath/6) on backends exporting this, and also
%% short-circuits identical operands in the ordering ops. Both helpers are
%% written against the generic backend API, so this is a pure opt-in.
-spec supports_inline_tuple2_eq() -> true.
supports_inline_tuple2_eq() ->
    true.

%%-----------------------------------------------------------------------------
%% @doc Get-or-register the per-module callable stub for Key. The body is
%% emitted once at the module tail (jit:flush_deferred_stubs/2); every site
%% reaches it with a plain `call' and the body ends in `ret', so unlike
%% aarch64 no link-register bookkeeping is needed.
%% @end
%%-----------------------------------------------------------------------------
-spec stub_ref(state(), term(), fun((state()) -> state())) -> {state(), reference()}.
stub_ref(#state{stubs = Stubs, deferred_stubs = DS} = State, Key, BodyFun) ->
    case Stubs of
        #{Key := Ref} ->
            {State, Ref};
        _ ->
            Ref = make_ref(),
            {
                State#state{
                    stubs = Stubs#{Key => Ref},
                    deferred_stubs = [{Ref, BodyFun} | DS]
                },
                Ref
            }
    end.

-spec take_deferred_stubs(state()) -> {[{reference(), fun((state()) -> state())}], state()}.
take_deferred_stubs(#state{deferred_stubs = DS} = State) ->
    {lists:reverse(DS), State#state{deferred_stubs = []}}.

%% @private Emit a `call' to a (possibly not yet emitted) label; patched like
%% any other 32-bit branch when the label is added.
-spec call_to_label(state(), reference()) -> state().
call_to_label(
    #state{stream_module = StreamModule, stream = Stream0, branches = Branches} = State, Label
) ->
    Offset = StreamModule:offset(Stream0),
    {RelocOffset, I} = jit_x86_64_asm:callq_rel32(1),
    Stream1 = StreamModule:append(Stream0, I),
    BrEntry = {Offset + RelocOffset, 32},
    Branches1 = maps:update_with(Label, fun(L) -> [BrEntry | L] end, [BrEntry], Branches),
    State#state{stream = Stream1, branches = Branches1}.

%% @private Assemble a stub body with local labels. Unlike a fixed-width ISA,
%% x86-64 branch displacements depend on the sizes of everything in between, so
%% the body is described as a list of items and the labels are resolved in two
%% passes. Every branch uses the 32-bit form so the sizes are known up front and
%% one sizing pass suffices; the few wasted bytes are paid once per module, not
%% per call site.
%%
%% Items: a binary (emitted verbatim), {label, Name}, {jcc, Fun, Name} where
%% Fun is one of the jit_x86_64_asm rel32 conditional jumps, or {jmp, Name}.
-spec stub_assemble([binary() | tuple()]) -> binary().
stub_assemble(Items) ->
    Labels = stub_scan(Items, 0, #{}),
    stub_emit(Items, 0, Labels, []).

stub_scan([], _Offset, Labels) ->
    Labels;
stub_scan([{label, Name} | Rest], Offset, Labels) ->
    stub_scan(Rest, Offset, Labels#{Name => Offset});
stub_scan([Item | Rest], Offset, Labels) ->
    stub_scan(Rest, Offset + stub_item_size(Item), Labels).

stub_item_size(Bin) when is_binary(Bin) -> byte_size(Bin);
%% 0F 8x rel32
stub_item_size({jcc, _, _}) -> 6;
%% E9 rel32
stub_item_size({jmp, _}) -> 5.

stub_emit([], _Offset, _Labels, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
stub_emit([{label, _} | Rest], Offset, Labels, Acc) ->
    stub_emit(Rest, Offset, Labels, Acc);
stub_emit([{jcc, Fun, Name} | Rest], Offset, Labels, Acc) ->
    #{Name := Target} = Labels,
    {_RelocOffset, Bin} = Fun(Target - Offset),
    stub_emit(Rest, Offset + 6, Labels, [Bin | Acc]);
stub_emit([{jmp, Name} | Rest], Offset, Labels, Acc) ->
    #{Name := Target} = Labels,
    {_RelocOffset, Bin} = jit_x86_64_asm:jmp_rel32(Target - Offset),
    stub_emit(Rest, Offset + 5, Labels, [Bin | Acc]);
stub_emit([Bin | Rest], Offset, Labels, Acc) when is_binary(Bin) ->
    stub_emit(Rest, Offset + byte_size(Bin), Labels, [Bin | Acc]).

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
%% @doc Factorized inline map get: call the per-module flat-map stub.
%% ABI: in rax = map term, rcx = key term; out rax = value (on hit),
%% rcx = status (0 hit, 1 not-found, 2 unsupported). The stub preserves every
%% other register (it pushes its own scratch) and clobbers flags. The
%% not-found verdict is sound only for immediate keys on flat maps, which is
%% exactly what the stub handles -- everything else reports unsupported and
%% the caller takes the C path.
%%
%% x86-64 has no register outside the allocatable pool to use as the ABI
%% (aarch64 has ip0/ip1), so rax/rcx are saved around the call whenever they
%% currently hold something live, and the two results are moved out before
%% those saves are restored. Pushing the arguments and popping them into
%% rax/rcx sidesteps every aliasing case between the sources and the ABI pair.
%% @end
%%-----------------------------------------------------------------------------
-spec map_get_stub_call(state(), x86_64_register(), x86_64_register()) ->
    {state(), x86_64_register(), x86_64_register()}.
map_get_stub_call(#state{} = State0, SrcReg, KeyReg) ->
    {State1, Ref} = stub_ref(State0, map_get_flat_imm, fun emit_map_get_stub_body/1),
    #state{stream_module = StreamModule, stream = Stream1, regs = Regs1} = State1,
    Used = jit_regs:used_regs(Regs1),
    Saved = [R || R <- [rax, rcx], Used band reg_bit(R) =/= 0],
    Pre = [
        [jit_x86_64_asm:pushq(R) || R <- Saved],
        jit_x86_64_asm:pushq(SrcReg),
        jit_x86_64_asm:pushq(KeyReg),
        jit_x86_64_asm:popq(rcx),
        jit_x86_64_asm:popq(rax)
    ],
    Stream2 = StreamModule:append(Stream1, iolist_to_binary(Pre)),
    State2 = call_to_label(State1#state{stream = Stream2}, Ref),
    #state{stream = Stream3, regs = Regs3} = State2,
    %% The value register is deliberately NOT allocated (the generic layer does
    %% not free it): it must be consumed by the immediately following emission,
    %% exactly like the aarch64 contract. The status register is allocated, as
    %% the condition machinery keeps it live across several blocks.
    Avail = jit_regs:available_regs(Regs3),
    ValReg = first_avail(Avail),
    StatusReg = first_avail(Avail band (bnot reg_bit(ValReg))),
    Post = [
        move_reg_if_different(rax, ValReg),
        move_reg_if_different(rcx, StatusReg),
        [jit_x86_64_asm:popq(R) || R <- lists:reverse(Saved)]
    ],
    Stream4 = StreamModule:append(Stream3, iolist_to_binary(Post)),
    Regs4 = lists:foldl(
        fun(R, Acc) -> jit_regs:invalidate_reg(Acc, R) end,
        Regs3,
        [rax, rcx, ValReg, StatusReg]
    ),
    Regs5 = jit_regs:alloc_reg(Regs4, reg_bit(StatusReg)),
    {State2#state{stream = Stream4, regs = Regs5}, ValReg, StatusReg}.

%% @private
move_reg_if_different(Reg, Reg) -> <<>>;
move_reg_if_different(From, To) -> jit_x86_64_asm:movq(From, To).

%%-----------------------------------------------------------------------------
%% @doc Factorized term-order resolution: call the per-module compare stub.
%% ABI: in rax = left term, rcx = right term; out rcx = status, either a
%% TermCompareResult (?TERM_EQUALS / ?TERM_LESS_THAN / ?TERM_GREATER_THAN) or 0
%% when the stub could not decide and the site must call the C comparator.
%% Both operands stay live in their own registers (the site still needs them
%% for the fallback), which the stub guarantees by preserving everything except
%% rax/rcx; see map_get_stub_call/3 for why those two are saved around the call.
%% @end
%%-----------------------------------------------------------------------------
-spec compare_stub_call(state(), x86_64_register(), x86_64_register()) ->
    {state(), x86_64_register()}.
compare_stub_call(#state{} = State0, LeftReg, RightReg) ->
    {State1, Ref} = stub_ref(State0, term_compare_order, fun emit_compare_stub_body/1),
    #state{stream_module = StreamModule, stream = Stream1, regs = Regs1} = State1,
    Used = jit_regs:used_regs(Regs1),
    Saved = [R || R <- [rax, rcx], Used band reg_bit(R) =/= 0],
    Pre = [
        [jit_x86_64_asm:pushq(R) || R <- Saved],
        jit_x86_64_asm:pushq(LeftReg),
        jit_x86_64_asm:pushq(RightReg),
        jit_x86_64_asm:popq(rcx),
        jit_x86_64_asm:popq(rax)
    ],
    Stream2 = StreamModule:append(Stream1, iolist_to_binary(Pre)),
    State2 = call_to_label(State1#state{stream = Stream2}, Ref),
    #state{stream = Stream3, regs = Regs3} = State2,
    %% The status is copied into an allocated register: the generic condition
    %% machinery keeps it live across both arms of an if_else_block.
    StatusReg = first_avail(jit_regs:available_regs(Regs3)),
    Post = [
        move_reg_if_different(rcx, StatusReg),
        [jit_x86_64_asm:popq(R) || R <- lists:reverse(Saved)]
    ],
    Stream4 = StreamModule:append(Stream3, iolist_to_binary(Post)),
    Regs4 = lists:foldl(
        fun(R, Acc) -> jit_regs:invalidate_reg(Acc, R) end,
        Regs3,
        [rax, rcx, StatusReg]
    ),
    Regs5 = jit_regs:alloc_reg(Regs4, reg_bit(StatusReg)),
    {State2#state{stream = Stream4, regs = Regs5}, StatusReg}.

%% @private The term-order resolution body. See compare_stub_call/3 for the
%% ABI. Iterative only (no stack): list tails loop back to the top, list heads
%% and tuple elements resolve as scalars or give up. Everything it decides is
%% decided soundly for TERM_COMPARE_NO_OPTS, _EXACT and _EQUAL_ONLY alike:
%%   - identical words are equal under every mode;
%%   - two (unequal) small integers order by signed tagged compare;
%%   - number ranks below every other type, and every immediate type ranks
%%     below list, so small-int vs other-immediate and immediate vs list decide
%%     on type alone (boxed operands never decide by type: a boxed float/bignum
%%     is a number, a boxed binary ranks above list);
%%   - tuples order by arity first, then leftmost unequal element pair;
%%   - list order is decided by the leftmost unequal head pair, or by the tails
%%     (nil is an immediate, so exhausted/improper tails fall out of the loop
%%     into the scalar rules above);
%%   - two atoms order by their cached 8-byte name sort_key (ctx->global
%%     ->atom_table->index_to_node[idx]->sort_key, read directly -- see
%%     ?GLOBAL_ATOM_TABLE et al.), matching atom_table_cmp_using_atom_index's
%%     fast path; a sort_key tie (shared 8-byte name prefix) is rare and
%%     reports unresolved so the C comparator's memcmp fallback decides.
%% Undecidable pairs (anything boxed-vs-scalar, non-tuple boxes, pids, ports,
%% references, funs) report 0 and the site calls the C comparator.
%%
%% Register plan: rax = left cursor, rcx = right cursor (status at exit);
%% rdx/rsi = box pointers, reused in the atom tail as left/right working
%% registers (index -> node -> sort_key); rdi/r8 = primary tags -> headers/
%% arity -> scan end / list tails; r9/r10 = scalar pair under test.
%% rdx/rsi/rdi/r8/r9/r10 are saved.
-spec emit_compare_stub_body(state()) -> state().
emit_compare_stub_body(#state{stream_module = StreamModule, stream = Stream0} = State0) ->
    Scratch = [rdx, rsi, rdi, r8, r9, r10],
    Body = stub_assemble(
        lists:flatten([
            [jit_x86_64_asm:pushq(R) || R <- Scratch],
            {label, top},
            %% identity, then dispatch on the primary tags
            jit_x86_64_asm:cmpq(rax, rcx),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_equals},
            jit_x86_64_asm:movq(rax, rdi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, rdi),
            jit_x86_64_asm:movq(rcx, r8),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_LIST, rdi),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, left_list},
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, rdi),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, both_boxed},
            %% left is immediate: below any list, undecided vs boxed, and two
            %% immediates resolve as a scalar pair
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_LIST, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_less},
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_undecided},
            jit_x86_64_asm:movq(rax, r9),
            jit_x86_64_asm:movq(rcx, r10),
            {jmp, scalar},
            {label, left_list},
            %% left is a list: above any immediate, undecided vs boxed
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_LIST, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, both_lists},
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_IMMED, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_greater},
            {jmp, v_undecided},
            {label, both_boxed},
            %% both boxed: tuples order by arity, everything else undecided
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, r8),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_undecided},
            jit_x86_64_asm:movq(rax, rdx),
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rdx),
            jit_x86_64_asm:movq({0, rdx}, rdi),
            jit_x86_64_asm:movq(rdi, r10),
            jit_x86_64_asm:andq(?TERM_BOXED_TAG_MASK, r10),
            jit_x86_64_asm:cmpq(?TERM_BOXED_TUPLE, r10),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_undecided},
            jit_x86_64_asm:movq(rcx, rsi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rsi),
            jit_x86_64_asm:movq({0, rsi}, r8),
            jit_x86_64_asm:movq(r8, r10),
            jit_x86_64_asm:andq(?TERM_BOXED_TAG_MASK, r10),
            jit_x86_64_asm:cmpq(?TERM_BOXED_TUPLE, r10),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_undecided},
            jit_x86_64_asm:shrq(6, rdi),
            jit_x86_64_asm:shrq(6, r8),
            %% arities are unsigned; above decides, otherwise a non-zero
            %% remainder means below and equality falls into the element scan
            jit_x86_64_asm:cmpq(r8, rdi),
            {jcc, fun jit_x86_64_asm:ja_rel32/1, v_greater},
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_less},
            %% equal arity: scan for the leftmost unequal element pair, with
            %% rdi the address of the last element of the left tuple
            jit_x86_64_asm:shlq(3, rdi),
            jit_x86_64_asm:addq(rdx, rdi),
            jit_x86_64_asm:addq(8, rdx),
            jit_x86_64_asm:addq(8, rsi),
            {label, tuple_loop},
            jit_x86_64_asm:cmpq(rdi, rdx),
            {jcc, fun jit_x86_64_asm:ja_rel32/1, v_equals},
            jit_x86_64_asm:movq({0, rdx}, r9),
            jit_x86_64_asm:addq(8, rdx),
            jit_x86_64_asm:movq({0, rsi}, r10),
            jit_x86_64_asm:addq(8, rsi),
            jit_x86_64_asm:cmpq(r10, r9),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, tuple_loop},
            {jmp, scalar},
            {label, both_lists},
            %% both lists: the leftmost unequal head pair decides as a scalar
            %% pair; equal heads loop on the tails
            jit_x86_64_asm:movq(rax, rdx),
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rdx),
            jit_x86_64_asm:movq(rcx, rsi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rsi),
            jit_x86_64_asm:movq({?LIST_TAIL_INDEX * 8, rdx}, rdi),
            jit_x86_64_asm:movq({?LIST_HEAD_INDEX * 8, rdx}, r9),
            jit_x86_64_asm:movq({?LIST_TAIL_INDEX * 8, rsi}, r8),
            jit_x86_64_asm:movq({?LIST_HEAD_INDEX * 8, rsi}, r10),
            jit_x86_64_asm:cmpq(r10, r9),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, scalar},
            jit_x86_64_asm:movq(rdi, rax),
            jit_x86_64_asm:movq(r8, rcx),
            {jmp, top},
            {label, scalar},
            %% an unequal pair in r9/r10: two small ints order by signed
            %% compare, a small int against another immediate is below, two
            %% atoms fall to the tail, anything else is undecided
            jit_x86_64_asm:movq(r9, rdi),
            jit_x86_64_asm:andq(r10, rdi),
            jit_x86_64_asm:movq(rdi, r8),
            jit_x86_64_asm:andq(?TERM_IMMED_TAG_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_INTEGER_TAG, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, both_small_ints},
            jit_x86_64_asm:movq(rdi, r8),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_IMMED, r8),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_undecided},
            jit_x86_64_asm:movq(r9, r8),
            jit_x86_64_asm:andq(?TERM_IMMED_TAG_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_INTEGER_TAG, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_less},
            jit_x86_64_asm:movq(r10, r8),
            jit_x86_64_asm:andq(?TERM_IMMED_TAG_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_INTEGER_TAG, r8),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_greater},
            %% neither is a small int: only an atom pair goes any further
            jit_x86_64_asm:movq(r9, r8),
            jit_x86_64_asm:andq(?TERM_IMMED2_TAG_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_IMMED2_ATOM, r8),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_undecided},
            {jmp, atom_tail},
            {label, v_equals},
            jit_x86_64_asm:movq(?TERM_EQUALS, rcx),
            {jmp, epilogue},
            {label, v_less},
            jit_x86_64_asm:movq(?TERM_LESS_THAN, rcx),
            {jmp, epilogue},
            {label, v_greater},
            jit_x86_64_asm:movq(?TERM_GREATER_THAN, rcx),
            {jmp, epilogue},
            {label, v_undecided},
            jit_x86_64_asm:movq(0, rcx),
            {label, epilogue},
            [jit_x86_64_asm:popq(R) || R <- lists:reverse(Scratch)],
            jit_x86_64_asm:retq(),
            {label, atom_tail},
            %% Reached with r9/r10 confirmed TERM_PRIMARY_IMMED, not
            %% TERM_INTEGER_TAG, and r9 confirmed an atom. Check r10 too, then
            %% compare both atoms' cached sort_key, reached by the
            %% address-dependency chain ctx -> global -> atom_table ->
            %% index_to_node[idx] -> sort_key (same idiom, and same soundness
            %% argument, as the plain-load chain in return_cross_module/2: each
            %% load's address depends on the previous load's value, and an
            %% atom_index already held in a live term cannot have been
            %% published more recently than whatever channel delivered that
            %% term to this scheduler -- no acquire needed, and no count bound
            %% check needed since the index is already known valid).
            jit_x86_64_asm:movq(r10, r8),
            jit_x86_64_asm:andq(?TERM_IMMED2_TAG_MASK, r8),
            jit_x86_64_asm:cmpq(?TERM_IMMED2_ATOM, r8),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, v_undecided},
            jit_x86_64_asm:movq(r9, rdx),
            jit_x86_64_asm:shrq(?TERM_IMMED2_TAG_SIZE, rdx),
            jit_x86_64_asm:movq(r10, rsi),
            jit_x86_64_asm:shrq(?TERM_IMMED2_TAG_SIZE, rsi),
            jit_x86_64_asm:movq({0, ?CTX_REG}, rdi),
            jit_x86_64_asm:movq({?GLOBAL_ATOM_TABLE, rdi}, rdi),
            jit_x86_64_asm:movq({?ATOM_TABLE_INDEX_TO_NODE, rdi}, r8),
            jit_x86_64_asm:movq({0, r8, rdx, 8}, rdx),
            jit_x86_64_asm:movq({0, r8, rsi, 8}, rsi),
            jit_x86_64_asm:movq({?HNODE_SORT_KEY, rdx}, rdx),
            jit_x86_64_asm:movq({?HNODE_SORT_KEY, rsi}, rsi),
            %% sort keys are unsigned; a tie is left to the C comparator
            jit_x86_64_asm:cmpq(rsi, rdx),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, v_undecided},
            {jcc, fun jit_x86_64_asm:ja_rel32/1, v_greater},
            {jmp, v_less},
            {label, both_small_ints},
            %% tagged small integers compare as signed words
            jit_x86_64_asm:cmpq(r10, r9),
            {jcc, fun jit_x86_64_asm:jl_rel32/1, v_less},
            {jmp, v_greater}
        ])
    ),
    State0#state{stream = StreamModule:append(Stream0, Body)}.

%% @private The flat-map lookup body. See map_get_stub_call/3 for the ABI.
%% Layout facts encoded here (asserted in jit.c / term.h): flat map =
%% boxed [header | keys tuple | V0..Vn-1]; keys tuple = boxed
%% [header | K0..Kn-1]; a tree map's boxed[1] is non-boxed. Two probe modes,
%% both with a CONCLUSIVE not-found:
%%   - immediate key (primary bits 2#11): exactly equal only to its identical
%%     word;
%%   - 2-tuple of immediates (#b_var{}-style compiler keys): a tuple is exactly
%%     equal only to a same-arity tuple with exactly-equal elements, and an
%%     immediate element equals only its identical word (boxed small integers
%%     do not exist; 1 =:= 1.0 is false), so word compares on the header and
%%     both elements decide either way.
%% Anything else reports unsupported and the site takes the C path.
%%
%% Register plan: rax = map term -> arity -> scan end -> value; rcx = key
%% (status at exit); rdx = map ptr -> value-address base delta (map ptr - keys
%% ptr: value_addr = delta + cursor_after_hit, since values start 16 bytes into
%% the map box and the cursor has advanced 16 bytes past the keys base at a
%% hit); rsi = keys ptr -> cursor; rdi = probe classification scratch -> loaded
%% stored key -> its ptr; r8/r9 = probe elements; r10 = stored-key
%% header/element scratch. rdx/rsi/rdi/r8/r9/r10 are saved.
-spec emit_map_get_stub_body(state()) -> state().
emit_map_get_stub_body(#state{stream_module = StreamModule, stream = Stream0} = State0) ->
    Tuple2Header = (2 bsl 6) bor ?TERM_BOXED_TUPLE,
    Scratch = [rdx, rsi, rdi, r8, r9, r10],
    Body = stub_assemble(
        lists:flatten([
            [jit_x86_64_asm:pushq(R) || R <- Scratch],
            %% the map must be boxed
            jit_x86_64_asm:movq(rax, rdx),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, rdx),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, rdx),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, unsupported},
            %% its boxed header must be a map
            jit_x86_64_asm:movq(rax, rdx),
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rdx),
            jit_x86_64_asm:movq({0, rdx}, rsi),
            jit_x86_64_asm:andq(?TERM_BOXED_TAG_MASK, rsi),
            jit_x86_64_asm:cmpq(?TERM_BOXED_MAP, rsi),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, unsupported},
            %% flat form: boxed[1] is the (boxed) keys tuple
            jit_x86_64_asm:movq({8, rdx}, rsi),
            jit_x86_64_asm:movq(rsi, rdi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, rdi),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, rdi),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, unsupported},
            %% keys ptr, arity, size cap
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rsi),
            jit_x86_64_asm:movq({0, rsi}, rax),
            jit_x86_64_asm:shrq(6, rax),
            jit_x86_64_asm:cmpq(64, rax),
            {jcc, fun jit_x86_64_asm:ja_rel32/1, unsupported},
            %% base delta, scan end (address of the last key), cursor (first key)
            jit_x86_64_asm:subq(rsi, rdx),
            jit_x86_64_asm:shlq(3, rax),
            jit_x86_64_asm:addq(rsi, rax),
            jit_x86_64_asm:addq(8, rsi),
            %% probe: an immediate key takes the short loop
            jit_x86_64_asm:movq(rcx, rdi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, rdi),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_IMMED, rdi),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, imm_loop},
            %% otherwise it must be boxed...
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, rdi),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, unsupported},
            %% ...with a 2-tuple header...
            jit_x86_64_asm:movq(rcx, rdi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rdi),
            jit_x86_64_asm:movq({0, rdi}, r8),
            jit_x86_64_asm:cmpq(Tuple2Header, r8),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, unsupported},
            %% ...whose two elements are both immediates
            jit_x86_64_asm:movq({8, rdi}, r8),
            jit_x86_64_asm:movq({16, rdi}, r9),
            jit_x86_64_asm:movq(r8, rdi),
            jit_x86_64_asm:andq(r9, rdi),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, rdi),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_IMMED, rdi),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, unsupported},
            {label, tuple2_loop},
            jit_x86_64_asm:cmpq(rax, rsi),
            {jcc, fun jit_x86_64_asm:ja_rel32/1, not_found},
            jit_x86_64_asm:movq({0, rsi}, rdi),
            jit_x86_64_asm:addq(8, rsi),
            jit_x86_64_asm:cmpq(rcx, rdi),
            {jcc, fun jit_x86_64_asm:jz_rel32/1, hit},
            %% a stored key must be boxed (primary 2#10) to possibly be a tuple
            jit_x86_64_asm:movq(rdi, r10),
            jit_x86_64_asm:andq(?TERM_PRIMARY_MASK, r10),
            jit_x86_64_asm:cmpq(?TERM_PRIMARY_BOXED, r10),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, tuple2_loop},
            jit_x86_64_asm:andq(?TERM_PRIMARY_CLEAR_MASK, rdi),
            jit_x86_64_asm:movq({0, rdi}, r10),
            jit_x86_64_asm:cmpq(Tuple2Header, r10),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, tuple2_loop},
            jit_x86_64_asm:movq({8, rdi}, r10),
            jit_x86_64_asm:cmpq(r8, r10),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, tuple2_loop},
            jit_x86_64_asm:movq({16, rdi}, r10),
            jit_x86_64_asm:cmpq(r9, r10),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, tuple2_loop},
            {jmp, hit},
            {label, imm_loop},
            jit_x86_64_asm:cmpq(rax, rsi),
            {jcc, fun jit_x86_64_asm:ja_rel32/1, not_found},
            jit_x86_64_asm:movq({0, rsi}, rdi),
            jit_x86_64_asm:addq(8, rsi),
            jit_x86_64_asm:cmpq(rcx, rdi),
            {jcc, fun jit_x86_64_asm:jnz_rel32/1, imm_loop},
            {label, hit},
            %% value = *(delta + cursor), the cursor having advanced past the
            %% matching key; rdx is dead from here on
            jit_x86_64_asm:addq(rsi, rdx),
            jit_x86_64_asm:movq({0, rdx}, rax),
            jit_x86_64_asm:movq(0, rcx),
            {jmp, epilogue},
            {label, not_found},
            jit_x86_64_asm:movq(1, rcx),
            {jmp, epilogue},
            {label, unsupported},
            jit_x86_64_asm:movq(2, rcx),
            {label, epilogue},
            [jit_x86_64_asm:popq(R) || R <- lists:reverse(Scratch)],
            jit_x86_64_asm:retq()
        ])
    ),
    State0#state{stream = StreamModule:append(Stream0, Body)}.

%%-----------------------------------------------------------------------------
%% @doc Return the recorded deferred raises (in emission order) and clear them.
%% @end
%%-----------------------------------------------------------------------------
-spec take_deferred_raises(state()) ->
    {[{reference(), non_neg_integer(), non_neg_integer(), [arg()]}], state()}.
take_deferred_raises(#state{deferred_raises = DR} = State) ->
    {lists:reverse(DR), State#state{deferred_raises = []}}.

%%-----------------------------------------------------------------------------
%% @doc Reset the register file to the state a freshly entered block sees: no
%% tracked contents, nothing allocated. Outlined blocks are reached from
%% arbitrary sites, so they may not inherit the emitting site's allocation.
%% @end
%%-----------------------------------------------------------------------------
-spec reset_regs_fresh(state()) -> state().
reset_regs_fresh(#state{regs = Regs} = State) ->
    State#state{
        regs = jit_regs:set_masks(jit_regs:invalidate_all(Regs), ?AVAILABLE_REGS_MASK, 0)
    }.

%% Nop encoding of a given store width. x86-64 x-stores are 4 bytes
%% (`mov reg, disp8(%rdi)') or 7 bytes (disp32); the assembler provides
%% canonical multi-byte nops.
pending_nop_bytes(Width) ->
    jit_x86_64_asm:nop(Width).

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

jump_table0(State, N, LabelsCount) when N > LabelsCount ->
    State;
jump_table0(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    N,
    LabelsCount
) ->
    % Placeholder, encodes with 0xffffffff
    {_RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(4),
    Stream1 = StreamModule:append(Stream0, I1),
    jump_table0(State#state{stream = Stream1}, N + 1, LabelsCount).

%%-----------------------------------------------------------------------------
%% @doc Patch a single branch in the stream
%% @end
%% @param StreamModule stream module
%% @param Stream stream state
%% @param Offset offset of the branch to patch
%% @param Size size of the branch in bits
%% @param LabelOffset target label offset
%% @return Updated stream
%%-----------------------------------------------------------------------------
-spec patch_branch(module(), stream(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    stream().
patch_branch(StreamModule, Stream, Offset, Size, LabelOffset) ->
    StreamModule:map(Stream, Offset, Size div 8, fun(<<Delta:Size/signed-little>>) ->
        <<(Delta + LabelOffset - Offset):Size/little>>
    end).

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
                fun({Offset, Size}, AccStream) ->
                    patch_branch(StreamModule, AccStream, Offset, Size, LabelOffset)
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
                fun({Offset, Size}, AccStream2) ->
                    patch_branch(StreamModule, AccStream2, Offset, Size, LabelOffset)
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
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> {state(), x86_64_register()}.
call_primitive(StateP, Primitive, Args0) ->
    %% Pinned-register convention: primitives read ctx and jit_state from
    %% r14/r13 and do not take them as parameters. (BIF/computed-pointer
    %% calls go through call_func_ptr directly and keep their ctx argument.)
    %% The function pointer is loaded from the pinned table after argument
    %% setup, directly in call_func_ptr0.
    Args = [A || A <- Args0, A =/= ctx, A =/= jit_state],
    Reload =
        case {prim_pure(Primitive), prim_returns_context(Primitive)} of
            {true, _} -> none;
            {_, true} -> deferred;
            _ -> here
        end,
    call_func_ptr0(StateP, {primitive, Primitive}, Args, Reload).

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
call_primitive_last(StateP, Primitive, Args0) ->
    %% Control leaves through the primitive, which reads ctx->x: any pending
    %% x store must stay. Pinned-register convention: primitives read ctx and
    %% jit_state from r14/r13, drop them from the argument list.
    Args = [A || A <- Args0, A =/= ctx, A =/= jit_state],
    call_primitive_last0(pending_clear_all(StateP), Primitive, Args).

call_primitive_last0(
    #state{
        stream_module = StreamModule
    } = State0,
    Primitive,
    Args
) ->
    %% The table is pinned in callee-saved rbx, which is never a parameter
    %% register and survives argument setup: a memory-indirect tail jump
    %% works for any argument count, no temp load of the function pointer.
    #{
        available_mask := AvailableRegs1,
        used_mask := UsedRegs,
        param_regs := ParamRegs,
        args_regs := ArgsRegs,
        param_mask := ParamMask,
        args_mask := ArgsMask
    } = prepare_call_scratch(Args),
    State1 = set_args2(
        State0#state{
            regs = jit_regs:set_masks(State0#state.regs, AvailableRegs1, UsedRegs)
        },
        Args,
        ParamRegs,
        ArgsRegs,
        ParamMask,
        ArgsMask
    ),
    #state{stream = Stream1} = State1,
    %% Write hp/e back to ctx before tail-calling into C (see call_func_ptr0).
    WB = <<
        (jit_x86_64_asm:movq(?HP_REG, ?HEAP_PTR))/binary,
        (jit_x86_64_asm:movq(?E_REG, ?Y_REGS))/binary
    >>,
    PrimAddr =
        case Primitive of
            0 -> {0, ?NATIVE_INTERFACE_REG};
            N -> ?PRIMITIVE(N)
        end,
    Call = jit_x86_64_asm:jmpq(PrimAddr),
    Stream2 = StreamModule:append(Stream1, <<WB/binary, Call/binary>>),
    State1#state{
        stream = Stream2,
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
return_if_not_equal_to_ctx(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {free, Reg}
) ->
    I1 = jit_x86_64_asm:cmpq(?CTX_REG, Reg),
    I3 =
        case Reg of
            rax -> <<>>;
            _ -> jit_x86_64_asm:movq(Reg, rax)
        end,
    I4 = jit_x86_64_asm:retq(),
    I2 = jit_x86_64_asm:jz(byte_size(I3) + byte_size(I4) + 2),
    %% Falling through means the primitive stayed with this context, so it is
    %% still alive and hp/e can be reloaded from it -- which the call itself
    %% deferred, precisely because the other edge may have freed it.
    RL = reload_hp_e_code(),
    Stream1 = StreamModule:append(
        Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary, RL/binary>>
    ),
    RegBit = reg_bit(Reg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs0, RegBit)
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
    StateP,
    Label
) ->
    %% Pendings whose register the target may read from memory (its live-in
    %% mask) keep their store; the rest stay pending for a possible flush.
    #state{stream_module = StreamModule, stream = Stream0, branches = AccBranches, labels = Labels} =
        State = pending_filter_label(StateP, Label),
    Offset = StreamModule:offset(Stream0),
    %% Inside a select_val jump table every slot must be exactly one 5-byte
    %% `jmp rel32', because the computed branch indexes the table by a fixed
    %% stride; the short form is only allowed outside one.
    Fixed = State#state.jump_table_slots > 0,
    State1 =
        case Fixed of
            true -> State#state{jump_table_slots = State#state.jump_table_slots - 1};
            false -> State
        end,
    case Labels of
        #{Label := LabelOffset} when not Fixed ->
            % Label is already known, emit direct branch without relocation

            % Calculate relative offset (assembler will adjust for instruction size)
            RelOffset = LabelOffset - Offset,
            I1 = jit_x86_64_asm:jmp(RelOffset),
            Stream1 = StreamModule:append(Stream0, I1),
            %% After unconditional jump, register tracking is dead until next label
            State#state{stream = Stream1, regs = jit_regs:unreachable(State#state.regs)};
        #{Label := LabelOffset} ->
            {_RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(LabelOffset - Offset),
            Stream1 = StreamModule:append(Stream0, I1),
            State1#state{stream = Stream1, regs = jit_regs:unreachable(State1#state.regs)};
        _ ->
            % Label not yet known, emit placeholder and add relocation
            {RelocOffset, I1} = jit_x86_64_asm:jmp_rel32(1),
            Stream1 = StreamModule:append(Stream0, I1),
            BrEntry = {Offset + RelocOffset, 32},
            ExistingBrs = maps:get(Label, AccBranches, []),
            State1#state{
                stream = Stream1,
                branches = AccBranches#{Label => [BrEntry | ExistingBrs]},
                regs = jit_regs:unreachable(State1#state.regs)
            }
    end.

%%-----------------------------------------------------------------------------
%% @doc First half of a dense select_val jump table: compute the index
%% `Src - MinTagged' and, when it is (unsigned) at most `Bound', skip the next
%% instruction -- the caller emits the branch to the default label there.
%% Otherwise execution falls into that branch. The forward displacement is
%% patched by jump_table_dispatch/1, since the default branch's width is not
%% known here.
%% @end
%%-----------------------------------------------------------------------------
-spec jump_table_range_check(state(), x86_64_register(), non_neg_integer(), non_neg_integer()) ->
    state().
jump_table_range_check(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    SrcReg,
    MinTagged,
    Bound
) ->
    %% SrcReg is allocated, so it cannot be picked here. The index register
    %% stays allocated until jump_table_dispatch/1 consumes it; the only thing
    %% emitted in between is the default branch, and jump_to_label/2 preserves
    %% the allocation masks.
    IdxReg = first_avail(jit_regs:available_regs(Regs0)),
    I1 = jit_x86_64_asm:leaq({-MinTagged, SrcReg}, IdxReg),
    I2 = jit_x86_64_asm:cmpq(Bound, IdxReg),
    {_RelocOffset, I3} = jit_x86_64_asm:jbe_rel32(0),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    JbeOffset = StreamModule:offset(Stream0) + byte_size(I1) + byte_size(I2),
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:alloc_reg(jit_regs:invalidate_reg(Regs0, IdxReg), reg_bit(IdxReg)),
    State0#state{
        stream = Stream1,
        regs = Regs1,
        jump_table_index = {IdxReg, JbeOffset, Bound div 16 + 1}
    }.

%%-----------------------------------------------------------------------------
%% @doc Second half: patch the range check to land here, then branch into the
%% table of 5-byte `jmp rel32' slots the caller emits right after. The index
%% register holds the tagged difference (a small int delta is value * 16), so
%% the byte offset into the table is (delta >> 4) * 5.
%% @end
%%-----------------------------------------------------------------------------
-spec jump_table_dispatch(state()) -> state().
jump_table_dispatch(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0,
        jump_table_index = {IdxReg, JbeOffset, Slots}
    } = State0
) ->
    %% The jbe emitted by jump_table_range_check/4 skips everything up to here.
    {_RelocOffset, Jbe} = jit_x86_64_asm:jbe_rel32(StreamModule:offset(Stream0) - JbeOffset),
    Stream1 = StreamModule:replace(Stream0, JbeOffset, Jbe),
    %% IdxReg is allocated; pick a second scratch for the table base.
    TmpReg = first_avail(jit_regs:available_regs(Regs0) band (bnot reg_bit(IdxReg))),
    I1 = jit_x86_64_asm:shrq(4, IdxReg),
    I2 = jit_x86_64_asm:imulq(5, IdxReg),
    I4 = jit_x86_64_asm:addq(IdxReg, TmpReg),
    I5 = jit_x86_64_asm:jmpq({TmpReg}),
    %% The rip base is the address following the lea, so the table starts
    %% byte_size(I4) + byte_size(I5) further on.
    I3 = jit_x86_64_asm:leaq({rip, byte_size(I4) + byte_size(I5)}, TmpReg),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
    Stream2 = StreamModule:append(Stream1, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, IdxReg), TmpReg),
    State0#state{
        stream = Stream2,
        regs = jit_regs:free_reg(Regs1, reg_bit(IdxReg)),
        jump_table_index = undefined,
        jump_table_slots = Slots
    }.

jump_to_offset(StateP, TargetOffset) ->
    %% Entering a shared (tail-cached) block that reads ctx->x: pending
    %% stores must stay.
    #state{stream_module = StreamModule, stream = Stream0} =
        State = pending_clear_all(StateP),
    Offset = StreamModule:offset(Stream0),
    RelOffset = TargetOffset - Offset,
    I1 = jit_x86_64_asm:jmp(RelOffset),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1, regs = jit_regs:unreachable(State#state.regs)}.

%%-----------------------------------------------------------------------------
%% @doc Emit a conditional jump straight to a label. For the fused condition
%% shapes this is a single jcc to the label instead of if_block's
%% inverted-jcc-over-a-jmp (5 fewer bytes per site and one branch instead of
%% two on the taken path — every type-test failure edge). Condition shapes
%% without a fused form fall back to the if_block emission.
%% @end
%% @param StateP current backend state
%% @param Cond condition to jump on (same forms as if_block)
%% @param Label label to jump to when Cond holds
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec jump_to_label_cond(state(), any(), integer() | reference()) -> state().
jump_to_label_cond(StateP, Cond, Label) ->
    case cond_direct_jcc(StateP, Cond) of
        unsupported ->
            if_block(StateP, Cond, fun(BSt0) -> jump_to_label(BSt0, Label) end);
        {State0, CmpCode} ->
            %% Same pending bookkeeping as jump_to_label: the taken edge
            %% must see stores whose register is in the label's live-in mask.
            #state{
                stream_module = StreamModule,
                stream = Stream0,
                branches = AccBranches,
                labels = Labels
            } =
                State = pending_filter_label(State0, Label),
            Offset = StreamModule:offset(Stream0),
            JccOffset = Offset + byte_size(CmpCode),
            case Labels of
                #{Label := LabelOffset} ->
                    Rel = LabelOffset - JccOffset,
                    I1 =
                        if
                            Rel >= -126 andalso Rel =< 129 ->
                                jit_x86_64_asm:jnz(Rel);
                            true ->
                                {_, I1Rel32} = jit_x86_64_asm:jnz_rel32(Rel),
                                I1Rel32
                        end,
                    Stream1 = StreamModule:append(Stream0, <<CmpCode/binary, I1/binary>>),
                    State#state{stream = Stream1};
                _ ->
                    {RelocOffset, I1} = jit_x86_64_asm:jnz_rel32(2),
                    Stream1 = StreamModule:append(Stream0, <<CmpCode/binary, I1/binary>>),
                    BrEntry = {JccOffset + RelocOffset, 32},
                    ExistingBrs = maps:get(Label, AccBranches, []),
                    State#state{
                        stream = Stream1,
                        branches = AccBranches#{Label => [BrEntry | ExistingBrs]}
                    }
            end
    end.

%% @private
%% Compare emission for jump_to_label_cond's fused shapes: returns the state
%% (condition operands freed) and the compare code whose jnz takes the jump
%% exactly when the condition holds. Non-fused shapes return `unsupported'.
cond_direct_jcc(State0, {RegOrTuple, '&', Mask, '!=', 0}) when ?IS_UINT8_T(Mask) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Mask, Reg),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, I1};
cond_direct_jcc(#state{regs = Regs0} = State0, {{free, Reg} = RegTuple, '&', Mask, '!=', Val}) when
    ?IS_UINT8_T(Mask)
->
    I1 = jit_x86_64_asm:andb(Mask, Reg),
    I2 = jit_x86_64_asm:cmpb(Val, Reg),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State1 = if_block_free_reg(RegTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary>>};
cond_direct_jcc(#state{regs = Regs0} = State0, {Reg, '&', Mask, '!=', Val}) when
    is_atom(Reg) andalso ?IS_UINT8_T(Mask)
->
    case jit_regs:available_regs(Regs0) of
        0 ->
            %% See the matching if_block_cond0 clause: pushq/popq preserve
            %% EFLAGS, so the cmpb result survives the restore.
            I1 = jit_x86_64_asm:pushq(Reg),
            I2 = jit_x86_64_asm:andb(Mask, Reg),
            I3 = jit_x86_64_asm:cmpb(Val, Reg),
            I4 = jit_x86_64_asm:popq(Reg),
            {State0, <<I1/binary, I2/binary, I3/binary, I4/binary>>};
        Avail ->
            Temp = first_avail(Avail),
            I1 = jit_x86_64_asm:movq(Reg, Temp),
            I2 = jit_x86_64_asm:andb(Mask, Temp),
            I3 = jit_x86_64_asm:cmpb(Val, Temp),
            Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
            {State0#state{regs = Regs1}, <<I1/binary, I2/binary, I3/binary>>}
    end;
cond_direct_jcc(_State0, _Cond) ->
    unsupported.

%%-----------------------------------------------------------------------------
%% @doc Cross-module return fast path: resolve the caller's module from the cp
%% in CpReg (ctx->global->modules_by_index[cp >> 24], offsets pinned by
%% _Static_asserts in jit.c), and when it has native code, update
%% jit_state->module / cp_base and branch straight to native_code + offset --
%% the work PRIM_RETURN does in C, minus the call round trip. Falls through
%% (with CpReg freed) when the target module has no native code (emulated),
%% for the caller to emit the C fallback.
%% @end
%% @param State current backend state
%% @param CpReg register holding the full cp value, consumed
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec return_cross_module(state(), {free, x86_64_register()}) -> state().
return_cross_module(#state{} = StateP, {free, CpReg}) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State = pending_clear_all(StateP),
    Avail0 = jit_regs:available_regs(Regs0),
    IdxReg = first_avail(Avail0),
    Avail1 = Avail0 band (bnot reg_bit(IdxReg)),
    ModReg = first_avail(Avail1),
    TargetReg = first_avail(Avail1 band (bnot reg_bit(ModReg))),
    I1 = jit_x86_64_asm:movq(CpReg, IdxReg),
    I2 = jit_x86_64_asm:shrq(24, IdxReg),
    % ctx->global, then global->modules_by_index (both at offset 0).
    I3 = jit_x86_64_asm:movq({0, ?CTX_REG}, ModReg),
    I4 = jit_x86_64_asm:movq({0, ModReg}, ModReg),
    % modules_by_index[idx]: scale the index in place, so it also serves as
    % the cp_base source below (idx << 24 is then idx * 8 << 21).
    I5 = jit_x86_64_asm:shlq(3, IdxReg),
    I6 = jit_x86_64_asm:addq(IdxReg, ModReg),
    I7 = jit_x86_64_asm:movq({0, ModReg}, ModReg),
    I8 = jit_x86_64_asm:movq({?MODULE_NATIVE_CODE, ModReg}, TargetReg),
    I9 = jit_x86_64_asm:testq(TargetReg, TargetReg),
    % jit_state_set_module: module and cp_base (module_index << 24).
    I11 = jit_x86_64_asm:movq(ModReg, ?JITSTATE_MODULE),
    I12 = jit_x86_64_asm:shlq(21, IdxReg),
    I13 = jit_x86_64_asm:movq(IdxReg, ?JITSTATE_CPBASE),
    % native_code + ((cp & 0xFFFFFF) >> 2)
    I14 = jit_x86_64_asm:andq(16#FFFFFF, CpReg),
    I15 = jit_x86_64_asm:shrq(2, CpReg),
    I16 = jit_x86_64_asm:addq(CpReg, TargetReg),
    I17 = jit_x86_64_asm:jmpq({TargetReg}),
    Tail =
        <<I11/binary, I12/binary, I13/binary, I14/binary, I15/binary, I16/binary, I17/binary>>,
    % No native code (emulated target): fall through to the C fallback.
    I10 = jit_x86_64_asm:jz(byte_size(Tail) + 2),
    Code =
        <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary, I7/binary, I8/binary,
            I9/binary, I10/binary, Tail/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = lists:foldl(
        fun(R, Acc) -> jit_regs:invalidate_reg(Acc, R) end,
        Regs0,
        [IdxReg, ModReg, TargetReg]
    ),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs1, reg_bit(CpReg))
    }.

%%-----------------------------------------------------------------------------
%% @doc Jump to a continuation address stored in a register.
%% This is used for optimized intra-module returns.
%% @end
%% @param State current backend state
%% @param OffsetReg register containing the continuation offset
%% @return Updated backend state
%%-----------------------------------------------------------------------------
jump_to_continuation(
    StateP,
    {free, OffsetReg}
) ->
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        offset = BaseOffset,
        regs = Regs0
    } = State = pending_clear_all(StateP),
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    % Calculate absolute address: native_code_base + target_offset
    % where native_code_base = current_pc + (BaseOffset - CurrentStreamOffset)
    % Similar to aarch64 approach but using leaq for PC-relative addressing
    CurrentStreamOffset = StreamModule:offset(Stream0),
    NetOffset = BaseOffset - CurrentStreamOffset - 7,

    % Get native code base address using PC-relative lea: leaq NetOffset(%rip), TempReg
    I1 = jit_x86_64_asm:leaq({rip, NetOffset}, TempReg),
    7 = byte_size(I1),
    % Add target offset to get final absolute address: addq OffsetReg, TempReg
    I2 = jit_x86_64_asm:addq(OffsetReg, TempReg),
    % Indirect jump to the calculated absolute address: jmpq *TempReg
    I3 = jit_x86_64_asm:jmpq({TempReg}),

    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    % Free all registers since this is a tail jump
    State#state{
        stream = Stream1,
        regs = jit_regs:set_masks(
            jit_regs:unreachable(Regs0), ?AVAILABLE_REGS_MASK, 0
        )
    }.

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
            {NewAccState, ReplaceDelta} = if_block_cond(AccState, Cond),
            OffsetAfterCond = StreamModule:offset(NewAccState#state.stream),
            {
                [
                    {Offset + ReplaceDelta, OffsetAfterCond, cond_skip_disp_width(Cond)}
                    | AccReplacements
                ],
                NewAccState
            }
        end,
        {[], State0},
        CondList
    ),
    State2 = pending_exit_cond(BlockFn(pending_enter_cond(State1))),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = lists:foldl(
        fun({ReplacementOffset, OffsetAfterCond, Width}, AccStream) ->
            patch_cond_skip(
                StreamModule, AccStream, ReplacementOffset, OffsetAfter - OffsetAfterCond, Width
            )
        end,
        Stream2,
        Replacements
    ),
    %% At the merge point, only keep register tracking that is consistent
    %% in both the taken (State2) and not-taken (State1) paths
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
    {State1, ReplaceDelta} = if_block_cond(State0, Cond),
    OffsetAfterCond = StreamModule:offset(State1#state.stream),
    State2 = pending_exit_cond(BlockFn(pending_enter_cond(State1))),
    Stream2 = State2#state.stream,
    OffsetAfter = StreamModule:offset(Stream2),
    Stream3 = patch_cond_skip(
        StreamModule,
        Stream2,
        Offset + ReplaceDelta,
        OffsetAfter - OffsetAfterCond,
        cond_skip_disp_width(Cond)
    ),
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
    {State1, ReplaceDelta} = if_block_cond(State0, Cond),
    OffsetAfterCond = StreamModule:offset(State1#state.stream),
    State2 = pending_exit_cond(BlockTrueFn(pending_enter_cond(State1))),
    Stream2 = State2#state.stream,
    ElseJumpOffset = StreamModule:offset(Stream2),
    %% Skip-the-false-block jump: use a rel32 jump because the false block can
    %% exceed 127 bytes (e.g. nested inline-arithmetic blocks). A rel8 jump here
    %% silently wraps the displacement and lands in the middle of an instruction.
    {RelocJMPOffset, I} = jit_x86_64_asm:jmp_rel32(1),
    Stream3 = StreamModule:append(Stream2, I),
    OffsetAfter = StreamModule:offset(Stream3),
    Stream4 = patch_cond_skip(
        StreamModule,
        Stream3,
        Offset + ReplaceDelta,
        OffsetAfter - OffsetAfterCond,
        cond_skip_disp_width(Cond)
    ),
    StateElse = State2#state{
        stream = Stream4,
        regs = State1#state.regs
    },
    State3 = pending_exit_cond(BlockFalseFn(pending_enter_cond(StateElse))),
    Stream5 = State3#state.stream,
    OffsetFinal = StreamModule:offset(Stream5),
    Stream6 = StreamModule:replace(Stream5, ElseJumpOffset + RelocJMPOffset, <<
        (OffsetFinal - OffsetAfter):32/little
    >>),
    %% Merge register tracking from both branches (true=State2, false=State3)
    MergedRegs = jit_regs:merge(
        State2#state.regs, State3#state.regs, ?AVAILABLE_REGS_MASK
    ),
    State3#state{stream = Stream6, regs = MergedRegs}.

%% Displacement width (bytes) of the conditional skip-the-block jump emitted by
%% if_block_cond0 for a given condition. The conditions used to guard the bignum
%% fallback in the inline-arith fast paths — the small-integer tag checks
%% ({Reg,'&',Mask,'!=',_}) and overflow_set / mul_overflow_set — must skip a
%% block that routinely exceeds the rel8 +127 range, so if_block_cond0 emits a
%% rel32 jump (4-byte displacement) for them; all other conditions use rel8.
-spec cond_skip_disp_width(condition()) -> 1 | 4.
cond_skip_disp_width(overflow_set) -> 4;
cond_skip_disp_width(mul_overflow_set) -> 4;
cond_skip_disp_width({_, '&', _, '!=', _}) -> 4;
%% select_val binary-search tree splits guard whole subtrees.
cond_skip_disp_width({_, '(uint)>', _}) -> 4;
%% Explicitly marked by the caller as guarding a large block.
cond_skip_disp_width({'(wide)', _}) -> 4;
cond_skip_disp_width(_) -> 1.

%% Patch the forward (skip-the-block) displacement of a conditional jump at byte
%% At, writing Disp in the width matching the emitted jump (rel8 or rel32).
patch_cond_skip(StreamModule, Stream, At, Disp, 1) ->
    ?ASSERT(Disp >= 0 andalso Disp < 16#80),
    StreamModule:replace(Stream, At, <<Disp>>);
patch_cond_skip(StreamModule, Stream, At, Disp, 4) ->
    StreamModule:replace(Stream, At, <<Disp:32/little>>).

-spec if_block_cond(state(), condition()) -> {state(), non_neg_integer()}.
if_block_cond(#state{stream_module = StreamModule} = State0, Cond) ->
    {State1, Code, ReplaceDelta} = if_block_cond0(State0, Cond),
    Stream1 = StreamModule:append(State1#state.stream, Code),
    State2 = State1#state{stream = Stream1},
    {State2, ReplaceDelta}.

-spec if_block_cond0(state(), condition()) -> {state(), binary(), non_neg_integer()}.
if_block_cond0(State0, Cond) when Cond =:= overflow_set orelse Cond =:= mul_overflow_set ->
    %% Flags were set by a preceding flag-setting instruction: addq/subq for
    %% overflow_set, imulq for mul_overflow_set. Both set the OF flag on signed
    %% overflow, so the block (bignum fallback) runs when OF is set; skip it
    %% (jump over) when overflow is clear. Use a rel32 jno: the skipped block is
    %% the bignum fallback (gc_bif call) which routinely exceeds the rel8 +127
    %% range — a rel8 jump there silently wraps and corrupts control flow.
    {RelocJNOOffset, I1} = jit_x86_64_asm:jno_rel32(1),
    {State0, I1, RelocJNOOffset};
if_block_cond0(State0, {RegOrTuple, '<', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJGEOffset};
% Handle {Value, '<', Reg} - means Value < Reg, jump if false (i.e., if Value >= Reg or Reg <= Value)
if_block_cond0(State0, {Value, '<', RegOrTuple}) when ?IS_SINT32_T(Value) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Value, Reg),
    {RelocJLEOffset, I2} = jit_x86_64_asm:jle_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJLEOffset};
% Catch-all for large values outside SINT32_T range
if_block_cond0(
    #state{regs = Regs0} = State0, {Value, '<', RegOrTuple}
) when is_integer(Value) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Value, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJLEOffset, I3} = jit_x86_64_asm:jle_rel8(1),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, Value}),
    State1 = if_block_free_reg(RegOrTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJLEOffset};
if_block_cond0(State0, {RegOrTuple, '<', Value}) when ?IS_SINT32_T(Value) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Value, Reg),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJGEOffset};
if_block_cond0(State0, {RegOrTuple, '<u', RegB}) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(RegB, Reg),
    {RelocJAEOffset, I2} = jit_x86_64_asm:jae_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJAEOffset};
if_block_cond0(State0, {RegOrTuple, '<', RegB}) when is_atom(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(RegB, Reg),
    {RelocJGEOffset, I2} = jit_x86_64_asm:jge_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJGEOffset};
% Catch-all for large values outside SINT32_T range
if_block_cond0(
    #state{regs = Regs0} = State0, {RegOrTuple, '<', Value}
) when is_integer(Value) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Value, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJGEOffset, I3} = jit_x86_64_asm:jge_rel8(1),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, Value}),
    State1 = if_block_free_reg(RegOrTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJGEOffset};
%% Unsigned above: skip the block when Reg <= Value (unsigned). Used for
%% two-sided corridor checks folded into one compare via unsigned wrap and
%% for the select_val binary-search tree splits. The skip is rel32: tree
%% splits guard whole subtrees, which routinely exceed the rel8 +127 range
%% (see cond_skip_disp_width).
if_block_cond0(State0, {RegOrTuple, '(uint)>', Value}) when
    is_integer(Value), ?IS_SINT32_T(Value)
->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Value, Reg),
    {RelocJBEOffset, I2} = jit_x86_64_asm:jbe_rel32(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJBEOffset};
% Catch-all for large values outside SINT32_T range
if_block_cond0(
    #state{regs = Regs0} = State0, {RegOrTuple, '(uint)>', Value}
) when is_integer(Value) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Value, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJBEOffset, I3} = jit_x86_64_asm:jbe_rel32(1),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, Value}),
    State1 = if_block_free_reg(RegOrTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJBEOffset};
if_block_cond0(State0, {RegOrTuple, '==', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(State0, {'(int)', RegOrTuple, '==', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testl(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(#state{regs = Regs0} = State0, {{x_reg, X}, Op, Val}) when
    (Op =:= '!=' orelse Op =:= '=='), X < ?MAX_REG, ?IS_SINT32_T(Val)
->
    %% Compare an x register against an immediate. When the register is
    %% cached in a native register compare that; otherwise fuse the load into
    %% a memory-operand compare (cmp imm, x_reg slot), saving an instruction
    %% and a temporary.
    case jit_regs:find_reg_with_contents(Regs0, {x_reg, X}) of
        {ok, CachedReg} ->
            if_block_cond0(State0, {CachedReg, Op, Val});
        none ->
            %% Memory-operand compare reads ctx->x[X]: keep its pending store.
            State1 = pending_clear_x(State0, X),
            I1 = jit_x86_64_asm:cmpq(Val, ?X_REG(X)),
            {RelocOffset, I2} =
                case Op of
                    '!=' -> jit_x86_64_asm:jz_rel8(1);
                    '==' -> jit_x86_64_asm:jnz_rel8(1)
                end,
            {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocOffset}
    end;
if_block_cond0(State0, {RegOrTuple, '!=', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testq(Reg, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(State0, {'(int)', RegOrTuple, '!=', 0}) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testl(Reg, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
%% '(wide)': the caller knows the guarded block is far past the rel8 +127
%% range, so the skip is emitted with a 32-bit displacement (see
%% cond_skip_disp_width). Unmarked conditions of the same shape keep the rel8
%% form below -- they guard a handful of instructions.
if_block_cond0(State0, {'(wide)', {RegOrTuple, '!=', RegB}}) when ?IS_GPR(RegB) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(RegB, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel32(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    State0,
    {RegOrTuple, '!=', Val}
) when ?IS_SINT32_T(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    #state{regs = Regs0} = State0,
    {RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Val, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jz_rel8(1),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, Val}),
    State1 = if_block_free_reg(RegOrTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond0(
    State0,
    {'(int)', RegOrTuple, '!=', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    State0,
    {RegOrTuple, '==', Val}
) when ?IS_SINT32_T(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpq(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    #state{regs = Regs0} = State0,
    {RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:movabsq(Val, Temp),
    I2 = jit_x86_64_asm:cmpq(Temp, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jnz_rel8(1),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, Val}),
    State1 = if_block_free_reg(RegOrTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond0(State0, {{free, Reg1}, '==', {free, Reg2}}) ->
    % Compare two free registers
    I1 = jit_x86_64_asm:cmpq(Reg2, Reg1),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    % Free both registers
    State1 = if_block_free_reg({free, Reg1}, State0),
    State2 = if_block_free_reg({free, Reg2}, State1),
    {State2, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(
    State0,
    {'(int)', RegOrTuple, '==', Val}
) when is_integer(Val) orelse ?IS_GPR(Val) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:cmpl(Val, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(
    State0,
    {'(bool)', RegOrTuple, '==', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    {RelocJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJNZOffset};
if_block_cond0(
    State0,
    {'(bool)', RegOrTuple, '!=', false}
) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Reg, Reg),
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel8(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(State0, {RegOrTuple, '&', Mask, '!=', 0}) when ?IS_UINT8_T(Mask) ->
    Reg =
        case RegOrTuple of
            {free, Reg0} -> Reg0;
            RegOrTuple -> RegOrTuple
        end,
    I1 = jit_x86_64_asm:testb(Mask, Reg),
    %% rel32: this condition guards the bignum fallback in the inline-arith fast
    %% paths, whose block exceeds the rel8 +127 range (see cond_skip_disp_width).
    {RelocJZOffset, I2} = jit_x86_64_asm:jz_rel32(1),
    State1 = if_block_free_reg(RegOrTuple, State0),
    {State1, <<I1/binary, I2/binary>>, byte_size(I1) + RelocJZOffset};
if_block_cond0(#state{regs = Regs0} = State0, {{free, Reg} = RegTuple, '&', Mask, '!=', Val}) when
    ?IS_UINT8_T(Mask)
->
    I1 = jit_x86_64_asm:andb(Mask, Reg),
    I2 = jit_x86_64_asm:cmpb(Val, Reg),
    {RelocJZOffset, I3} = jit_x86_64_asm:jz_rel32(1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State1 = if_block_free_reg(RegTuple, State0#state{regs = Regs1}),
    {State1, <<I1/binary, I2/binary, I3/binary>>, byte_size(I1) + byte_size(I2) + RelocJZOffset};
if_block_cond0(#state{regs = Regs0} = State0, {Reg, '&', Mask, '!=', Val}) when ?IS_UINT8_T(Mask) ->
    case jit_regs:available_regs(Regs0) of
        0 ->
            %% No scratch register is free to hold a copy of `Reg', which must
            %% survive the test (it is not `{free, _}'). Save it on the native
            %% stack, run the destructive AND/CMP on its low byte, then restore
            %% it with popq. Neither pushq nor popq touches EFLAGS, so the ZF
            %% set by cmpb survives the pop and drives the skip jump.
            I1 = jit_x86_64_asm:pushq(Reg),
            I2 = jit_x86_64_asm:andb(Mask, Reg),
            I3 = jit_x86_64_asm:cmpb(Val, Reg),
            I4 = jit_x86_64_asm:popq(Reg),
            {RelocJZOffset, I5} = jit_x86_64_asm:jz_rel32(1),
            {
                State0,
                <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
                byte_size(I1) + byte_size(I2) + byte_size(I3) + byte_size(I4) + RelocJZOffset
            };
        Avail ->
            Temp = first_avail(Avail),
            I1 = jit_x86_64_asm:movq(Reg, Temp),
            I2 = jit_x86_64_asm:andb(Mask, Temp),
            I3 = jit_x86_64_asm:cmpb(Val, Temp),
            {RelocJZOffset, I4} = jit_x86_64_asm:jz_rel32(1),
            Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
            {
                State0#state{regs = Regs1},
                <<I1/binary, I2/binary, I3/binary, I4/binary>>,
                byte_size(I1) + byte_size(I2) + byte_size(I3) + RelocJZOffset
            }
    end.

-spec if_block_free_reg(x86_64_register() | {free, x86_64_register()}, state()) -> state().
if_block_free_reg({free, Reg}, #state{regs = Regs0} = State0) ->
    Bit = reg_bit(Reg),
    State0#state{
        regs = jit_regs:free_reg(Regs0, Bit)
    };
if_block_free_reg(Reg, State0) when ?IS_GPR(Reg) ->
    State0.

%%-----------------------------------------------------------------------------
%% @doc Emit a shift register right by a fixed number of bits, effectively
%% dividing it by 2^Shift
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
%% Load NumBits (8, 16 or 32) bits big-endian, zero-extended, from the address
%% in AddrReg, into AddrReg itself.
-spec load_be_unsigned(#state{}, x86_64_register(), 8 | 16 | 32) -> #state{}.
load_be_unsigned(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, AddrReg, NumBits
) when
    ?IS_GPR(AddrReg)
->
    Code =
        case NumBits of
            8 ->
                jit_x86_64_asm:movzbq({0, AddrReg}, AddrReg);
            16 ->
                I1 = jit_x86_64_asm:movzwq({0, AddrReg}, AddrReg),
                I2 = jit_x86_64_asm:bswapl(AddrReg),
                I3 = jit_x86_64_asm:shrq(16, AddrReg),
                <<I1/binary, I2/binary, I3/binary>>;
            32 ->
                I1 = jit_x86_64_asm:movl({0, AddrReg}, AddrReg),
                I2 = jit_x86_64_asm:bswapl(AddrReg),
                <<I1/binary, I2/binary>>
        end,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, AddrReg),
    State#state{stream = Stream1, regs = Regs1}.

%% Store the low NumBits (8, 16 or 32) bits of ValReg big-endian to the address
%% in AddrReg. ValReg is clobbered (byte-swapped in place for 16/32 bits).
-spec store_be(#state{}, x86_64_register(), x86_64_register(), 8 | 16 | 32) -> #state{}.
store_be(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    AddrReg,
    ValReg,
    NumBits
) when
    ?IS_GPR(AddrReg) andalso ?IS_GPR(ValReg)
->
    Code =
        case NumBits of
            8 ->
                jit_x86_64_asm:movb_store(ValReg, {0, AddrReg});
            16 ->
                I1 = jit_x86_64_asm:rolw(8, ValReg),
                I2 = jit_x86_64_asm:movw_store(ValReg, {0, AddrReg}),
                <<I1/binary, I2/binary>>;
            32 ->
                I1 = jit_x86_64_asm:bswapl(ValReg),
                I2 = jit_x86_64_asm:movl_store(ValReg, {0, AddrReg}),
                <<I1/binary, I2/binary>>
        end,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, ValReg),
    State#state{stream = Stream1, regs = Regs1}.

-spec shift_right(#state{}, maybe_free_x86_64_register(), non_neg_integer()) ->
    {#state{}, x86_64_register()}.
shift_right(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_x86_64_asm:shrq(Shift, Reg),
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
    Avail = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Avail),
    Bit = reg_bit(ResultReg),
    I1 = jit_x86_64_asm:movq(Reg, ResultReg),
    I2 = jit_x86_64_asm:shrq(Shift, ResultReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        ResultReg
    }.

%%-----------------------------------------------------------------------------
%% @doc Emit an arithmetic shift right by a fixed number of bits (sign-preserving).
%% @param State current state
%% @param Reg register to shift
%% @param Shift number of bits to shift
%% @return new state
%%-----------------------------------------------------------------------------
-spec shift_right_arith(#state{}, maybe_free_x86_64_register(), non_neg_integer()) ->
    {#state{}, x86_64_register()}.
shift_right_arith(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Shift
) when
    ?IS_GPR(Reg) andalso is_integer(Shift)
->
    I = jit_x86_64_asm:sarq(Shift, Reg),
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
    Avail = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Avail),
    Bit = reg_bit(ResultReg),
    I1 = jit_x86_64_asm:movq(Reg, ResultReg),
    I2 = jit_x86_64_asm:sarq(Shift, ResultReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
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
shift_left(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Shift
) when
    is_atom(Reg)
->
    I = jit_x86_64_asm:shlq(Shift, Reg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%%-----------------------------------------------------------------------------
%% @doc In-place variable shift: shift `Reg' by the amount in `ShiftReg'
%% (callers bound-check the amount; the hardware takes it mod 64).
%%
%% x86-64 takes a variable shift count only in `%cl', so unless the amount
%% already is in rcx the count has to be moved there. rcx is an ordinary
%% member of the scratch pool, so three shapes are possible: it is free (use
%% it directly), it holds the value being shifted (swap the two registers
%% around the shift, which leaves the count where the caller expects it -- the
%% bsl path reuses it), or it is live for something else (save and restore it
%% around the shift). The BMI2 shlx/sarx forms would avoid all of this, but
%% AOT-precompiled code has to run on pre-Haswell CPUs too.
%% @end
%% @param State current backend state
%% @param Reg register holding the value to shift, updated in place
%% @param ShiftReg register holding the shift amount, preserved
%% @return new state
%%-----------------------------------------------------------------------------
-spec shift_right_arith_reg(state(), x86_64_register(), x86_64_register()) -> state().
shift_right_arith_reg(State, Reg, ShiftReg) ->
    shift_by_cl(State, fun jit_x86_64_asm:sarq/2, Reg, ShiftReg).

-spec shift_left_reg(state(), x86_64_register(), x86_64_register()) -> state().
shift_left_reg(State, Reg, ShiftReg) ->
    shift_by_cl(State, fun jit_x86_64_asm:shlq/2, Reg, ShiftReg).

shift_by_cl(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, ShiftFun, Reg, rcx
) when Reg =/= rcx ->
    Stream1 = StreamModule:append(Stream0, ShiftFun(cl, Reg)),
    State#state{stream = Stream1, regs = jit_regs:invalidate_reg(Regs0, Reg)};
shift_by_cl(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    ShiftFun,
    rcx,
    ShiftReg
) when ShiftReg =/= rcx ->
    %% The value is in rcx and the count is not: swap them, shift in place,
    %% swap back. The second xchg restores the count to ShiftReg and leaves
    %% the result in rcx.
    Xchg = jit_x86_64_asm:xchgq(rcx, ShiftReg),
    Code = <<Xchg/binary, (ShiftFun(cl, ShiftReg))/binary, Xchg/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, regs = jit_regs:invalidate_reg(Regs0, rcx)};
shift_by_cl(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    ShiftFun,
    Reg,
    ShiftReg
) when Reg =/= rcx, ShiftReg =/= rcx ->
    Mov = jit_x86_64_asm:movq(ShiftReg, rcx),
    Shift = ShiftFun(cl, Reg),
    Code =
        case jit_regs:available_regs(Regs0) band ?REG_BIT_RCX of
            0 ->
                Save = jit_x86_64_asm:pushq(rcx),
                Restore = jit_x86_64_asm:popq(rcx),
                <<Save/binary, Mov/binary, Shift/binary, Restore/binary>>;
            _ ->
                <<Mov/binary, Shift/binary>>
        end,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), rcx),
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
-spec call_func_ptr(state(), {free, x86_64_register()} | {primitive, non_neg_integer()}, [arg()]) ->
    {state(), x86_64_register()}.
call_func_ptr(StateP, FuncPtrTuple, Args) ->
    call_func_ptr0(StateP, FuncPtrTuple, Args, false).

%% `Reload' says what to do about the hp/e pinned registers after the call:
%% `none' for a pure primitive (they stay authoritative across it), `here' to
%% reload them from ctx right after the call, and `deferred' for a primitive
%% that may return a different Context -- reading ctx then would touch a
%% process this call just terminated, so the dispatch that follows reloads on
%% the paths where ctx is still alive.
-spec call_func_ptr0(
    state(),
    {free, x86_64_register()} | {primitive, non_neg_integer()},
    [arg()],
    none | here | deferred | boolean()
) ->
    {state(), x86_64_register()}.
call_func_ptr0(
    StateP,
    FuncPtrTuple,
    Args,
    Pure
) ->
    %% The callee reads ctx->x: any pending x store must stay. ctx/jit_state
    %% args are NOT filtered here: BIFs and computed function pointers take
    %% ctx explicitly (set_args materializes it from r14); primitive calls
    %% are filtered in call_primitive/call_primitive_last.
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0 = pending_clear_all(StateP),
    AvailableRegs0 = jit_regs:available_regs(Regs0),
    UsedRegs0 = jit_regs:used_regs(Regs0),
    FreeMask = lists:foldl(
        fun
            ({free, {ptr, Reg}}, Acc) -> Acc bor reg_bit(Reg);
            ({free, Reg}, Acc) when is_atom(Reg) -> Acc bor reg_bit(Reg);
            (_, Acc) -> Acc
        end,
        0,
        [FuncPtrTuple | Args]
    ),
    UsedRegs1 = UsedRegs0 band (bnot FreeMask),
    %% ctx/jit_state/table (r14/r13/rbx) are callee-saved: the callee
    %% preserves them, so only live scratch regs need saving.
    SavedRegs = mask_to_list(UsedRegs1),
    PushBin = iolist_to_binary([jit_x86_64_asm:pushq(R) || R <- SavedRegs]),
    PushOdds = length(SavedRegs) rem 2,
    % x86-64 stack must be 16-byte aligned at the callq: entry left it
    % misaligned by 8, so the total push count must be odd. The filler is
    % popped into r11 after the call (rax carries the result).
    AlignBin =
        case PushOdds of
            1 -> <<>>;
            0 -> jit_x86_64_asm:pushq(rax)
        end,
    %% A {free, Reg} function pointer living in a parameter register would be
    %% overwritten by argument setup: park it on the stack and pop it into
    %% rax after set_args. The pair is balanced before the call, so it does
    %% not affect alignment.
    ParamMask0 = jit_regs:regs_to_mask(parameter_regs(Args), fun reg_bit/1),
    {FuncPtrTuple1, ParkBin} =
        case FuncPtrTuple of
            {free, FPReg0} when is_atom(FPReg0) ->
                case reg_bit(FPReg0) band ParamMask0 of
                    0 -> {FuncPtrTuple, <<>>};
                    _ -> {parked, jit_x86_64_asm:pushq(FPReg0)}
                end;
            _ ->
                {FuncPtrTuple, <<>>}
        end,
    Stream1 = StreamModule:append(Stream0, <<PushBin/binary, AlignBin/binary, ParkBin/binary>>),
    State1 = set_args(State0#state{stream = Stream1}, Args),
    #state{stream = Stream5} = State1,
    %% Write hp/e back to ctx: the callee — and any GC it triggers — must see
    %% a coherent heap/stack state. Skipped for pure primitives (prim_pure/1).
    WB =
        case Pure of
            Skip when Skip =:= true orelse Skip =:= none ->
                <<>>;
            _ ->
                <<
                    (jit_x86_64_asm:movq(?HP_REG, ?HEAP_PTR))/binary,
                    (jit_x86_64_asm:movq(?E_REG, ?Y_REGS))/binary
                >>
        end,
    Call =
        case FuncPtrTuple1 of
            {free, FuncPtrReg} ->
                jit_x86_64_asm:callq({FuncPtrReg});
            parked ->
                Call0 = jit_x86_64_asm:popq(rax),
                Call1 = jit_x86_64_asm:callq({rax}),
                <<Call0/binary, Call1/binary>>;
            {primitive, Primitive} ->
                %% The table is pinned in rbx: load the pointer after argument
                %% setup, into rax (never a parameter register).
                PrepCall =
                    case Primitive of
                        0 -> jit_x86_64_asm:movq({0, ?NATIVE_INTERFACE_REG}, rax);
                        N -> jit_x86_64_asm:movq(?PRIMITIVE(N), rax)
                    end,
                <<PrepCall/binary, (jit_x86_64_asm:callq({rax}))/binary>>
        end,
    % Unalign stack: combine call + unalign-pop into one append when needed
    CallAndUnalign =
        case PushOdds of
            1 -> <<WB/binary, Call/binary>>;
            0 -> <<WB/binary, Call/binary, (jit_x86_64_asm:popq(r11))/binary>>
        end,
    Stream7 = StreamModule:append(Stream5, CallAndUnalign),
    % If rax is in used regs, save it to another temporary register
    AvailableRegs1 = AvailableRegs0 bor FreeMask,
    {Stream8, ResultReg} =
        case UsedRegs1 band ?REG_BIT_RAX of
            0 ->
                {Stream7, rax};
            _ ->
                Temp = first_avail(AvailableRegs1),
                {StreamModule:append(Stream7, jit_x86_64_asm:movq(rax, Temp)), Temp}
        end,
    PopBin = iolist_to_binary([jit_x86_64_asm:popq(R) || R <- lists:reverse(SavedRegs)]),
    %% Reload hp/e: the callee (or a GC it triggered) may have moved them.
    RL =
        case Pure of
            true -> <<>>;
            none -> <<>>;
            deferred -> <<>>;
            _ -> reload_hp_e_code()
        end,
    Stream9 = StreamModule:append(Stream8, <<PopBin/binary, RL/binary>>),
    ResultBit = reg_bit(ResultReg),
    AvailableRegs2 = (AvailableRegs1 band (bnot ResultBit)) band ?AVAILABLE_REGS_MASK,
    UsedRegs2 = UsedRegs1 bor ResultBit,
    Regs1 = jit_regs:invalidate_all(State0#state.regs),
    {
        State1#state{
            stream = Stream9,
            regs = jit_regs:set_masks(Regs1, AvailableRegs2, UsedRegs2)
        },
        ResultReg
    }.

%% @private
%% Refresh the pinned hp/e registers from the (live) context.
reload_hp_e_code() ->
    <<
        (jit_x86_64_asm:movq(?HEAP_PTR, ?HP_REG))/binary,
        (jit_x86_64_asm:movq(?Y_REGS, ?E_REG))/binary
    >>.

-spec set_args(state(), [arg()]) -> state().
set_args(State0, Args) ->
    ParamRegs = parameter_regs(Args),
    ArgsRegs = args_regs(Args),
    ParamMask = jit_regs:regs_to_mask(ParamRegs, fun reg_bit/1),
    ArgsMask = jit_regs:regs_to_mask(ArgsRegs, fun reg_bit/1),
    set_args2(State0, Args, ParamRegs, ArgsRegs, ParamMask, ArgsMask).

set_args2(
    #state{stream = Stream0, stream_module = StreamModule, regs = Regs0} = State0,
    Args,
    ParamRegs,
    ArgsRegs,
    ParamMask,
    ArgsMask
) ->
    UsedRegs = jit_regs:used_regs(Regs0),
    AvailableScratchGP =
        ?SCRATCH_REGS_MASK band (bnot (ParamMask bor ArgsMask bor UsedRegs)),
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
            ({free, {ptr, Reg}}, AccMask) -> AccMask band (bnot reg_bit(Reg));
            ({free, Reg}, AccMask) when is_atom(Reg) -> AccMask band (bnot reg_bit(Reg));
            (_, AccMask) -> AccMask
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

parameter_regs(Args) ->
    parameter_regs0(Args, ?PARAMETER_REGS, []).

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

replace_reg(Args, Reg1, Reg2) ->
    replace_reg0(Args, Reg1, Reg2, []).

replace_reg0([Reg | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
replace_reg0([{free, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [Replacement | T]);
%% Pointer arguments reference the register too: the dereference happens off
%% the replacement register.
replace_reg0([{ptr, Reg} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [{ptr, Replacement} | T]);
replace_reg0([{free, {ptr, Reg}} | T], Reg, Replacement, Acc) ->
    lists:reverse(Acc, [{free, {ptr, Replacement}} | T]);
replace_reg0([Other | T], Reg, Replacement, Acc) ->
    replace_reg0(T, Reg, Replacement, [Other | Acc]).

% Exchange registers in both Args and ArgsRegs lists
exchange_reg(Args, ArgsRegs, Reg1, Reg2) ->
    NewArgs = replace_reg(Args, Reg1, Reg2),
    NewArgsRegs = lists:map(
        fun
            (R) when R =:= Reg1 -> Reg2;
            (R) -> R
        end,
        ArgsRegs
    ),
    {NewArgs, NewArgsRegs}.

set_args0([], [], [], _AvailGP, _LoadedImm, Acc) ->
    list_to_binary(lists:reverse(Acc));
set_args0([{free, FreeVal} | ArgsT], ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc) ->
    set_args0([FreeVal | ArgsT], ArgsRegs, ParamRegs, AvailGP, LoadedImm, Acc);
%% ctx/jit_state materialize from the pinned registers (r14/r13): like
%% immediates they have no swappable source register, so when the parameter
%% register is still occupied by a later argument, defer to the end of the
%% queue instead of xchg-ing (which would clobber a pinned register).
set_args0(
    [Special | ArgsT], [SpecialReg | ArgsRegs], [ParamReg | ParamRegs], AvailGP, LoadedImm, Acc
) when
    Special =:= ctx orelse Special =:= jit_state
->
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            J = set_args1(Special, ParamReg),
            set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [J | Acc]);
        true ->
            set_args0(
                ArgsT ++ [Special],
                ArgsRegs ++ [SpecialReg],
                ParamRegs ++ [ParamReg],
                AvailGP,
                LoadedImm,
                Acc
            )
    end;
set_args0(
    [Arg | ArgsT],
    [ArgReg | ArgsRegs],
    [ParamReg | ParamRegs],
    AvailGP,
    LoadedImm,
    Acc
) ->
    case lists:member(ParamReg, ArgsRegs) of
        false ->
            % Normal case: ParamReg is free, just move Arg to ParamReg
            case is_integer(Arg) andalso maps:find(Arg, LoadedImm) of
                {ok, SourceReg} ->
                    J = jit_x86_64_asm:movq(SourceReg, ParamReg),
                    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, LoadedImm, [J | Acc]);
                _ ->
                    J = set_args1(Arg, ParamReg),
                    NewLoadedImm =
                        case is_integer(Arg) of
                            true -> LoadedImm#{Arg => ParamReg};
                            false -> LoadedImm
                        end,
                    set_args0(ArgsT, ArgsRegs, ParamRegs, AvailGP, NewLoadedImm, [J | Acc])
            end;
        true when ArgReg =:= imm ->
            % The argument is an immediate (or offset): it has no source register
            % to xchg with. ParamReg is still occupied by a later argument, so
            % defer this immediate to the end of the queue. Once that later
            % argument has been moved to its own destination, ParamReg is free
            % and the immediate is loaded directly. Immediates never participate
            % in a register cycle, so the queue always drains.
            set_args0(
                ArgsT ++ [Arg],
                ArgsRegs ++ [ArgReg],
                ParamRegs ++ [ParamReg],
                AvailGP,
                LoadedImm,
                Acc
            );
        true ->
            % ParamReg is occupied by another argument that will go elsewhere
            % Use xchg to swap ArgReg and ParamReg
            % After xchg, the value from Arg (which was in ArgReg) is now in ParamReg
            I = jit_x86_64_asm:xchgq(ArgReg, ParamReg),
            {NewArgsT, NewArgsRegs} = exchange_reg(ArgsT, ArgsRegs, ParamReg, ArgReg),
            set_args0(NewArgsT, NewArgsRegs, ParamRegs, AvailGP, LoadedImm, [I | Acc])
    end.

set_args1(Reg, Reg) ->
    [];
set_args1(ctx, Reg) ->
    jit_x86_64_asm:movq(?CTX_REG, Reg);
set_args1(jit_state, Reg) ->
    jit_x86_64_asm:movq(?JITSTATE_REG, Reg);
set_args1({x_reg, extra}, Reg) ->
    jit_x86_64_asm:movq(?X_REG(?MAX_REG), Reg);
set_args1({x_reg, X}, Reg) ->
    jit_x86_64_asm:movq(?X_REG(X), Reg);
set_args1({ptr, Source}, Reg) ->
    jit_x86_64_asm:movq({0, Source}, Reg);
set_args1({y_reg, X}, Reg) ->
    jit_x86_64_asm:movq({X * 8, ?E_REG}, Reg);
set_args1(ArgReg, Reg) when ?IS_GPR(ArgReg) ->
    jit_x86_64_asm:movq(ArgReg, Reg);
set_args1(0, Reg) ->
    jit_x86_64_asm:xorl(Reg, Reg);
set_args1(Arg, Reg) when is_integer(Arg), Arg >= 0, Arg =< 16#FFFFFFFF ->
    jit_x86_64_asm:movl(Arg, Reg);
set_args1(Arg, Reg) when is_integer(Arg) andalso Arg >= -16#80000000 andalso Arg < 16#80000000 ->
    jit_x86_64_asm:movq(Arg, Reg);
set_args1(Arg, Reg) when is_integer(Arg) ->
    jit_x86_64_asm:movabsq(Arg, Reg);
set_args1({avm_int64_t, Value}, Reg) when is_integer(Value) ->
    jit_x86_64_asm:movabsq(Value, Reg).

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
    (state(), Src :: {free, {ptr, x86_64_register(), 1}}, Dest :: {fp_reg, non_neg_integer()}) ->
        state().
move_to_vm_register(#state{regs = Regs0} = State0, Src, Dest) ->
    %% Pending-store bookkeeping (jit_backend_pending_impl.hrl): an x-register
    %% source may be re-read from memory by the emit below, so its pending
    %% store must persist; an x-register destination supersedes a same-depth
    %% pending store to the same slot.
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
    %% Invalidate any CPU register tracking the old value of the destination VM register
    VmLoc = vm_dest_to_contents(Dest),
    Regs1 =
        case VmLoc of
            unknown -> Regs0;
            _ -> jit_regs:invalidate_vm_loc(Regs0, VmLoc)
        end,
    State1 = move_to_vm_register_emit(State#state{regs = Regs1}, Src, Dest),
    %% The x-store is the last instruction emitted; record it as pending so a
    %% later superseding store or a dead-at-label can neutralize it in place.
    State2 =
        case Dest of
            {x_reg, DestX2} when is_integer(DestX2) ->
                Width = x_store_width(Src, DestX2),
                StartOffset = (State1#state.stream_module):offset(State1#state.stream) - Width,
                pending_note_store(State1, DestX2, StartOffset);
            _ ->
                State1
        end,
    %% After storing a native register to a VM register, the native reg still holds
    %% the VM register's value. Record this so subsequent loads can be skipped.
    case {Src, VmLoc} of
        {Reg, Contents} when is_atom(Reg), Contents =/= unknown ->
            #state{regs = Regs2} = State2,
            State2#state{regs = jit_regs:set_contents(Regs2, Reg, Contents)};
        _ ->
            State2
    end.

%% Byte width of the x-slot store emitted by move_to_vm_register_emit for a
%% given source form and (integer) destination slot. The store is always the
%% final instruction: `andq $0, x[X]' for a zero source, `movq imm32, x[X]'
%% for a small immediate, else `movq reg, x[X]' (register width is
%% independent of which GP register — REX is always present).
x_store_width(0, X) ->
    byte_size(jit_x86_64_asm:andq(0, ?X_REG(X)));
x_store_width(N, X) when is_integer(N), ?IS_SINT32_T(N) ->
    byte_size(jit_x86_64_asm:movq(N, ?X_REG(X)));
x_store_width(_Src, X) ->
    byte_size(jit_x86_64_asm:movq(rax, ?X_REG(X))).

%% Convert a VM register destination to a contents descriptor.
vm_dest_to_contents(Dest) -> jit_regs:vm_dest_to_contents(Dest, ?MAX_REG).

% Src = 0, we can andq as an optimization
move_to_vm_register_emit(State, 0, {x_reg, X}) when X < ?MAX_REG ->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(X)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register_emit(State, 0, {x_reg, extra}) ->
    I1 = jit_x86_64_asm:andq(0, ?X_REG(?MAX_REG)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register_emit(State, 0, {ptr, Reg}) ->
    I1 = jit_x86_64_asm:andq(0, {0, Reg}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register_emit(#state{} = State, 0, {y_reg, Y}) ->
    I2 = jit_x86_64_asm:andq(0, {Y * 8, ?E_REG}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I2),
    State#state{stream = Stream1};
% ?IS_SINT32_T(Src), we can use movq to set the value
move_to_vm_register_emit(State, N, {x_reg, X}) when X < ?MAX_REG andalso ?IS_SINT32_T(N) ->
    Stream1 = (State#state.stream_module):append(
        State#state.stream, jit_x86_64_asm:movq(N, ?X_REG(X))
    ),
    State#state{stream = Stream1};
move_to_vm_register_emit(State, N, {x_reg, extra}) when ?IS_SINT32_T(N) ->
    Stream1 = (State#state.stream_module):append(
        State#state.stream, jit_x86_64_asm:movq(N, ?X_REG(?MAX_REG))
    ),
    State#state{stream = Stream1};
move_to_vm_register_emit(State, N, {ptr, Reg}) when ?IS_SINT32_T(N) ->
    Stream1 = (State#state.stream_module):append(
        State#state.stream, jit_x86_64_asm:movq(N, {0, Reg})
    ),
    State#state{stream = Stream1};
move_to_vm_register_emit(#state{} = State, N, {y_reg, Y}) when
    ?IS_SINT32_T(N)
->
    I2 = jit_x86_64_asm:movq(N, {Y * 8, ?E_REG}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I2),
    State#state{stream = Stream1};
% ?is_integer(Src), we need to use movabsq
move_to_vm_register_emit(#state{regs = Regs0} = State, N, {x_reg, X}) when
    X < ?MAX_REG andalso is_integer(N)
->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, N}),
    State#state{stream = Stream1, regs = Regs1};
move_to_vm_register_emit(
    #state{regs = Regs0} = State, N, {x_reg, extra}
) when
    is_integer(N)
->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(?MAX_REG)),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, N}),
    State#state{stream = Stream1, regs = Regs1};
move_to_vm_register_emit(#state{regs = Regs0} = State, N, {ptr, Reg}) when
    is_integer(N)
->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(N, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Reg}),
    Stream1 = (State#state.stream_module):append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, N}),
    State#state{stream = Stream1, regs = Regs1};
move_to_vm_register_emit(#state{regs = Regs0} = State, N, {y_reg, Y}) when
    is_integer(N)
->
    Avail = jit_regs:available_regs(Regs0),
    Temp2 = first_avail(Avail),
    I2 = jit_x86_64_asm:movabsq(N, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, ?E_REG}),
    Stream1 = (State#state.stream_module):append(
        State#state.stream, <<I2/binary, I3/binary>>
    ),
    Regs1 = jit_regs:set_contents(Regs0, Temp2, {imm, N}),
    State#state{stream = Stream1, regs = Regs1};
% is_atom(Src) (native register)
move_to_vm_register_emit(State, Reg, {x_reg, X}) when is_atom(Reg) andalso X < ?MAX_REG ->
    I1 = jit_x86_64_asm:movq(Reg, ?X_REG(X)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register_emit(State, Reg, {x_reg, extra}) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, ?X_REG(?MAX_REG)),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register_emit(State, Reg, {ptr, Dest}) when is_atom(Reg) ->
    I1 = jit_x86_64_asm:movq(Reg, {0, Dest}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I1),
    State#state{stream = Stream1};
move_to_vm_register_emit(#state{} = State, Reg, {y_reg, Y}) when
    is_atom(Reg)
->
    I2 = jit_x86_64_asm:movq(Reg, {Y * 8, ?E_REG}),
    Stream1 = (State#state.stream_module):append(State#state.stream, I2),
    State#state{stream = Stream1};
% Src is x_reg, store in temporary register and call move_to_vm_register_emit for the four cases
move_to_vm_register_emit(
    #state{regs = Regs0} = State0, {x_reg, X}, Dest
) when
    X < ?MAX_REG
->
    with_temp(State0, Dest, fun(Temp) ->
        {jit_x86_64_asm:movq(?X_REG(X), Temp), jit_regs:set_contents(Regs0, Temp, {x_reg, X})}
    end);
move_to_vm_register_emit(
    #state{regs = Regs0} = State0, {x_reg, extra}, Dest
) ->
    with_temp(State0, Dest, fun(Temp) ->
        {
            jit_x86_64_asm:movq(?X_REG(?MAX_REG), Temp),
            jit_regs:set_contents(Regs0, Temp, {x_reg, ?MAX_REG})
        }
    end);
move_to_vm_register_emit(#state{regs = Regs0} = State0, {ptr, Reg}, Dest) ->
    with_temp(State0, Dest, fun(Temp) ->
        {jit_x86_64_asm:movq({0, Reg}, Temp), jit_regs:invalidate_reg(Regs0, Temp)}
    end);
move_to_vm_register_emit(#state{regs = Regs0} = State0, {y_reg, Y}, Dest) ->
    with_temp(State0, Dest, fun(Temp) ->
        I2 = jit_x86_64_asm:movq({Y * 8, ?E_REG}, Temp),
        {I2, jit_regs:set_contents(Regs0, Temp, {y_reg, Y})}
    end);
% term_to_float
move_to_vm_register_emit(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State0,
    {free, {ptr, Reg, 1}},
    {fp_reg, F}
) when is_atom(Reg) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movq({8, Reg}, Reg),
    I2 = jit_x86_64_asm:movq(?FP_REGS, Temp),
    I3 = jit_x86_64_asm:movq(Reg, {?FP_REG_OFFSET(State0, F), Temp}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State1 = free_native_register(State0#state{regs = Regs1}, Reg),
    State1#state{stream = Stream1}.

-spec with_temp(
    state(),
    vm_register(),
    fun((x86_64_register()) -> {binary(), jit_regs:regs()})
) -> state().
with_temp(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0, Dest, EmitFn
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    TempBit = reg_bit(Temp),
    {Code, Regs1} = EmitFn(Temp),
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = move_to_vm_register_emit(
        State0#state{
            stream = Stream1,
            regs = jit_regs:set_available_regs(Regs1, Avail band (bnot TempBit))
        },
        Temp,
        Dest
    ),
    State1#state{regs = jit_regs:set_available_regs(State1#state.regs, Avail)}.

%%-----------------------------------------------------------------------------
%% @doc Emit a move of an array element (reg[x]) to a vm or a native register.
%% @end
%% @param State current backend state
%% @param Reg base register of the array
%% @param Index index in the array, as an integer or a native register
%% @param Dest vm or native register to move to
%% @return Updated backend state
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @doc Load both cells of a cons into DISTINCT native registers, each tracked
%% as holding its destination x register, so a following read of either is
%% served from the register instead of reloading from memory (the pervasive
%% tail-recursive list loop reads head and tail right after the match). The
%% generic two-call path reuses one temp and so evicts the head when it loads
%% the tail. `ListReg' (the already-unboxed cons pointer) is consumed.
%% @end
%% @param State current backend state
%% @param ListReg native register holding the untagged cons pointer, consumed
%% @param HeadDest destination for the head cell
%% @param TailDest destination for the tail cell
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec get_list_head_tail(state(), {free, x86_64_register()}, vm_register(), vm_register()) ->
    state().
get_list_head_tail(State0, {free, ListReg}, {x_reg, H}, {x_reg, T}) when
    H < ?MAX_REG, T < ?MAX_REG, H =/= T
->
    #state{stream_module = StreamModule, regs = Regs0} =
        State1 = pending_elide_prev(pending_elide_prev(State0, H), T),
    %% ListReg is allocated, so it is not in the available mask and cannot
    %% collide with either destination; the two destinations are distinct by
    %% construction.
    Avail0 = jit_regs:available_regs(Regs0),
    HeadReg = first_avail(Avail0),
    TailReg = first_avail(Avail0 band (bnot reg_bit(HeadReg))),
    Loads = <<
        (jit_x86_64_asm:movq({?LIST_HEAD_INDEX * 8, ListReg}, HeadReg))/binary,
        (jit_x86_64_asm:movq({?LIST_TAIL_INDEX * 8, ListReg}, TailReg))/binary
    >>,
    StreamA = StreamModule:append(State1#state.stream, Loads),
    %% Write-through stores, noted one at a time so each pending store records
    %% the offset of its own `mov'.
    StH = jit_x86_64_asm:movq(HeadReg, ?X_REG(H)),
    StreamB = StreamModule:append(StreamA, StH),
    StateB = pending_note_store(
        State1#state{stream = StreamB}, H, StreamModule:offset(StreamB) - byte_size(StH)
    ),
    StT = jit_x86_64_asm:movq(TailReg, ?X_REG(T)),
    StreamC = StreamModule:append(StateB#state.stream, StT),
    StateC = pending_note_store(
        StateB#state{stream = StreamC}, T, StreamModule:offset(StreamC) - byte_size(StT)
    ),
    %% ListReg is dead; head and tail are now tracked in their own registers.
    Regs1 = jit_regs:free_reg(StateC#state.regs, reg_bit(ListReg)),
    Regs2 = jit_regs:invalidate_vm_loc(Regs1, {x_reg, H}),
    Regs3 = jit_regs:invalidate_vm_loc(Regs2, {x_reg, T}),
    Regs4 = jit_regs:set_contents(Regs3, HeadReg, {x_reg, H}),
    Regs5 = jit_regs:set_contents(Regs4, TailReg, {x_reg, T}),
    StateC#state{regs = Regs5};
get_list_head_tail(State0, {free, ListReg}, HeadDest, TailDest) ->
    %% Fallback (y_reg / ptr destinations): the generic two-load form.
    State1 = move_array_element(State0, ListReg, ?LIST_HEAD_INDEX, HeadDest),
    State2 = free_native_registers(State1, [HeadDest]),
    State3 = move_array_element(State2, ListReg, ?LIST_TAIL_INDEX, TailDest),
    State4 = free_native_registers(State3, [ListReg]),
    free_native_registers(State4, [TailDest]).

-spec move_array_element(
    State :: state(),
    Reg :: x86_64_register(),
    Index :: non_neg_integer() | {free, x86_64_register()},
    Dest :: vm_register() | x86_64_register()
) -> state().
move_array_element(
    #state{stream_module = StreamModule, stream = _Stream0, regs = Regs0} =
        State0,
    Reg,
    Index,
    {x_reg, X}
) when X < ?MAX_REG andalso is_integer(Index) ->
    %% A same-depth pending store to x[X] is superseded by this one.
    State = pending_elide_prev(State0, X),
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, ?X_REG(X)),
    Stream1 = StreamModule:append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:set_contents(Regs1, Temp, {x_reg, X}),
    State1 = State#state{stream = Stream1, regs = Regs2},
    %% The x-store is the final instruction (I2).
    pending_note_store(State1, X, StreamModule:offset(Stream1) - byte_size(I2));
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Reg,
    Index,
    {ptr, Dest}
) when is_integer(Index) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {0, Dest}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Reg,
    Index,
    {y_reg, Y}
) when is_integer(Index) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp2 = first_avail(Avail),
    I2 = jit_x86_64_asm:movq({Index * 8, Reg}, Temp2),
    I3 = jit_x86_64_asm:movq(Temp2, {Y * 8, ?E_REG}),
    Code = <<I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs3 = jit_regs:invalidate_reg(Regs1, Temp2),
    State#state{stream = Stream1, regs = Regs3};
move_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Index, Dest
) when is_atom(Dest) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Dest),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Dest),
    State#state{stream = Stream1, regs = Regs1};
move_array_element(
    #state{
        stream_module = StreamModule,
        stream = _Stream0,
        regs = Regs0
    } = State0,
    Reg,
    {free, IndexReg},
    {x_reg, X}
) when X < ?MAX_REG andalso is_atom(IndexReg) ->
    State = pending_elide_prev(State0, X),
    I1 = jit_x86_64_asm:shlq(3, IndexReg),
    I2 = jit_x86_64_asm:addq(Reg, IndexReg),
    I3 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I4 = jit_x86_64_asm:movq(IndexReg, ?X_REG(X)),
    IndexBit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(
        State#state.stream, <<I1/binary, I2/binary, I3/binary, I4/binary>>
    ),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {x_reg, X}),
    Regs2 = jit_regs:invalidate_reg(Regs1, IndexReg),
    State1 = State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs2, IndexBit)
    },
    pending_note_store(State1, X, StreamModule:offset(Stream1) - byte_size(I4));
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
    I1 = jit_x86_64_asm:shlq(3, IndexReg),
    I2 = jit_x86_64_asm:addq(Reg, IndexReg),
    I3 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I4 = jit_x86_64_asm:movq(IndexReg, {0, PtrReg}),
    IndexBit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary, I3/binary, I4/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, IndexReg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs1, IndexBit)
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
    I2 = jit_x86_64_asm:shlq(3, IndexReg),
    I3 = jit_x86_64_asm:addq(Reg, IndexReg),
    I4 = jit_x86_64_asm:movq({0, IndexReg}, IndexReg),
    I5 = jit_x86_64_asm:movq(IndexReg, {Y * 8, ?E_REG}),
    IndexBit = reg_bit(IndexReg),
    Stream1 = StreamModule:append(
        Stream0, <<I2/binary, I3/binary, I4/binary, I5/binary>>
    ),
    Regs1 = jit_regs:invalidate_vm_loc(Regs0, {y_reg, Y}),
    Regs3 = jit_regs:invalidate_reg(Regs1, IndexReg),
    State#state{
        stream = Stream1,
        regs = jit_regs:free_reg(Regs3, IndexBit)
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
    State :: state(),
    Reg :: x86_64_register() | {free, x86_64_register()},
    Index :: non_neg_integer()
) ->
    {state(), x86_64_register()}.
get_array_element(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    {free, Reg},
    Index
) ->
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, Reg),
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
    Avail = jit_regs:available_regs(Regs0),
    ElemReg = first_avail(Avail),
    Bit = reg_bit(ElemReg),
    I1 = jit_x86_64_asm:movq({Index * 8, Reg}, ElemReg),
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
    State :: state(),
    Value :: integer() | vm_register() | x86_64_register(),
    Reg :: x86_64_register(),
    Index :: non_neg_integer()
) -> state().
move_to_array_element(
    #state{stream_module = StreamModule, stream = _Stream0, regs = Regs0} =
        State0,
    {x_reg, X},
    Reg,
    Index
) when X < ?MAX_REG andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    %% Reads ctx->x[X] from memory: keep its pending store.
    State = pending_clear_x(State0, X),
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {x_reg, X}),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    {ptr, Source},
    Reg,
    Index
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movq({0, Source}, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    {y_reg, Y},
    Reg,
    Index
) when ?IS_GPR(Reg) andalso is_integer(Index) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I2 = jit_x86_64_asm:movq({Y * 8, ?E_REG}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Code = <<I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_GPR(Source) andalso ?IS_GPR(Reg) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State, Source, Reg, Index
) when ?IS_SINT32_T(Source) andalso is_integer(Index) ->
    I1 = jit_x86_64_asm:movq(Source, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    Source,
    Reg,
    Index
) when is_integer(Source) andalso is_integer(Index) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Source, Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Index * 8, Reg}),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {imm, Source}),
    State#state{stream = Stream1, regs = Regs1}.

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
move_to_array_element(
    State,
    Source,
    BaseReg,
    Index,
    Offset
) when is_integer(Index) andalso is_integer(Offset) ->
    move_to_array_element(State, Source, BaseReg, Index + Offset);
move_to_array_element(
    #state{stream_module = StreamModule, stream = _Stream0, regs = Regs0} =
        State0,
    {x_reg, X},
    BaseReg,
    IndexReg,
    Offset
) when X < ?MAX_REG andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    %% Reads ctx->x[X] from memory: keep its pending store.
    State = pending_clear_x(State0, X),
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I1 = jit_x86_64_asm:movq(?X_REG(X), Temp),
    I2 = jit_x86_64_asm:movq(Temp, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(State#state.stream, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {x_reg, X}),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    {y_reg, Y},
    BaseReg,
    IndexReg,
    Offset
) when ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    I2 = jit_x86_64_asm:movq({Y * 8, ?E_REG}, Temp),
    I3 = jit_x86_64_asm:movq(Temp, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, <<I2/binary, I3/binary>>),
    Regs1 = jit_regs:set_contents(Regs0, Temp, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs1};
move_to_array_element(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Source,
    BaseReg,
    IndexReg,
    Offset
) when
    ?IS_GPR(Source) andalso ?IS_GPR(BaseReg) andalso ?IS_GPR(IndexReg) andalso is_integer(Offset)
->
    I1 = jit_x86_64_asm:movq(Source, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
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
    I1 = jit_x86_64_asm:movq(Source, {Offset * ?WORD_SIZE, BaseReg, IndexReg, 8}),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

-spec move_to_native_register(state(), value() | cp) -> {state(), x86_64_register()}.
move_to_native_register(State, Reg) when ?IS_GPR(Reg) ->
    {State, Reg};
move_to_native_register(#state{regs = Regs} = State, Value) ->
    Contents = jit_regs:value_to_contents(Value, ?MAX_REG),
    case Contents =/= unknown andalso jit_regs:find_reg_with_contents(Regs, Contents) of
        {ok, CachedReg} ->
            Bit = reg_bit(CachedReg),
            Used = jit_regs:used_regs(Regs),
            case Used band Bit of
                0 ->
                    Avail = jit_regs:available_regs(Regs),
                    case Avail band Bit of
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
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_x86_64_asm:movq(?CP, Reg),
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
    I1 = jit_x86_64_asm:movq({0, Reg}, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    %% After dereferencing a pointer, contents tracking for this reg is invalidated
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
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 =
        if
            Imm =:= 0 -> jit_x86_64_asm:xorl(Reg, Reg);
            Imm >= 0, Imm =< 16#FFFFFFFF -> jit_x86_64_asm:movl(Imm, Reg);
            ?IS_SINT32_T(Imm) -> jit_x86_64_asm:movq(Imm, Reg);
            true -> jit_x86_64_asm:movabsq(Imm, Reg)
        end,
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
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_x86_64_asm:movq(?X_REG(?MAX_REG), Reg),
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
    StateP,
    {x_reg, X},
    Contents
) when
    X < ?MAX_REG
->
    %% Cache miss: this loads ctx->x[X] from memory, so keep its pending store.
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State = pending_clear_x(StateP, X),
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_x86_64_asm:movq(?X_REG(X), Reg),
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
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    Code = jit_x86_64_asm:movq({Y * 8, ?E_REG}, Reg),
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, Contents),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs1, Bit)
        },
        Reg
    }.

-spec move_to_native_register(state(), integer() | x86_64_register(), x86_64_register()) -> state().
move_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, RegSrc, RegDst
) when is_atom(RegSrc) orelse is_integer(RegSrc) ->
    I =
        if
            is_atom(RegSrc) -> jit_x86_64_asm:movq(RegSrc, RegDst);
            RegSrc =:= 0 -> jit_x86_64_asm:xorl(RegDst, RegDst);
            RegSrc >= 0, RegSrc =< 16#FFFFFFFF -> jit_x86_64_asm:movl(RegSrc, RegDst);
            ?IS_SINT32_T(RegSrc) -> jit_x86_64_asm:movq(RegSrc, RegDst);
            true -> jit_x86_64_asm:movabsq(RegSrc, RegDst)
        end,
    Stream1 = StreamModule:append(Stream0, I),
    %% Copy the source's tracking to the destination, or set imm if integer
    Regs1 =
        case is_atom(RegSrc) of
            true ->
                SrcContents = jit_regs:get_contents(Regs0, RegSrc),
                jit_regs:set_contents(Regs0, RegDst, SrcContents);
            false when is_integer(RegSrc) ->
                jit_regs:set_contents(Regs0, RegDst, {imm, RegSrc})
        end,
    State#state{stream = Stream1, regs = Regs1}.

-spec copy_to_native_register(state(), value()) -> {state(), x86_64_register()}.
copy_to_native_register(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg
) when is_atom(Reg) ->
    Avail = jit_regs:available_regs(Regs0),
    SaveReg = first_avail(Avail),
    Bit = reg_bit(SaveReg),
    I1 = jit_x86_64_asm:movq(Reg, SaveReg),
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
    Avail = jit_regs:available_regs(Regs0),
    SaveReg = first_avail(Avail),
    Bit = reg_bit(SaveReg),
    I1 = jit_x86_64_asm:movq({0, Reg}, SaveReg),
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

move_to_cp(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    {y_reg, Y}
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    I2 = jit_x86_64_asm:movq({Y * 8, ?E_REG}, Reg),
    I3 = jit_x86_64_asm:movq(Reg, ?CP),
    Code = <<I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:set_contents(Regs0, Reg, {y_reg, Y}),
    State#state{stream = Stream1, regs = Regs1}.

increment_sp(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    Offset
) ->
    %% e is pinned: bump the register directly.
    I1 = jit_x86_64_asm:addq(Offset * 8, ?E_REG),
    Stream1 = StreamModule:append(Stream0, I1),
    State#state{stream = Stream1}.

set_continuation_to_label(
    StateP,
    Label
) ->
    %% The continuation is a re-entry point with unknown x-cache state:
    %% pending x stores must be committed.
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
            % Label is already known, emit direct leaq without relocation
            % leaq instruction is 7 bytes, RIP points to next instruction
            RelOffset = LabelOffset - (Offset + 7),
            I1 = jit_x86_64_asm:leaq({rip, RelOffset}, Temp),
            I2 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State#state{stream = Stream1, regs = Regs1};
        _ ->
            % Label not yet known, emit placeholder and add relocation
            {RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({-4, rip}, Temp),
            BrEntry = {Offset + RewriteLEAOffset, 32},
            I2 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
            Code = <<I1/binary, I2/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            ExistingBrs = maps:get(Label, Branches, []),
            State#state{
                stream = Stream1,
                branches = Branches#{Label => [BrEntry | ExistingBrs]},
                regs = Regs1
            }
    end.

set_continuation_to_offset(
    StateP
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
    {RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({-4, rip}, Temp),
    BrEntry = {Offset + RewriteLEAOffset, 32},
    I2 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
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

%% @doc Implement a continuation entry point. On x86-64 this is a nop
%% as we don't need to save any register.
-spec continuation_entry_point(#state{}) -> #state{}.
continuation_entry_point(State) ->
    %% Resumption target: code may re-enter here with unknown x-cache state.
    pending_clear_all(State).

%%-----------------------------------------------------------------------------
%% @doc Resolve the imported BIF function pointer for a gc_bif call site inline,
%% instead of through the PRIM_GET_IMPORTED_GCBIF primitive call. Equivalent to
%% jit_get_imported_gcbif in jit.c: first drop dead extended registers (only if
%% any exist — the common case has none, so the cleanup call is skipped), then
%% load module->imported_funcs[Bif]->bif0_ptr. Returns the pointer register.
%% @end
-spec move_imported_gcbif_to_native_register(state(), integer(), non_neg_integer()) ->
    {state(), x86_64_register()}.
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
    I1 = jit_x86_64_asm:leaq({?CTX_EXTENDED_X_REGS, ?CTX_REG}, AddrReg),
    I2 = jit_x86_64_asm:movq({0, AddrReg}, NextReg),
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
    J1 = jit_x86_64_asm:movq(?JITSTATE_MODULE, PtrReg),
    J2 = jit_x86_64_asm:movq({?MODULE_IMPORTED_FUNCS, PtrReg}, PtrReg),
    J3 = jit_x86_64_asm:movq({Bif * ?WORD_SIZE, PtrReg}, PtrReg),
    J4 = jit_x86_64_asm:movq({?BIF_BIF0_PTR, PtrReg}, PtrReg),
    Stream3 = StreamModule:append(Stream2, <<J1/binary, J2/binary, J3/binary, J4/binary>>),
    Bit = reg_bit(PtrReg),
    Regs2 = jit_regs:alloc_reg(jit_regs:invalidate_reg(State2#state.regs, PtrReg), Bit),
    {
        State2#state{stream = Stream3, regs = Regs2},
        PtrReg
    }.

%% Load an imported plain-BIF function pointer into a freshly allocated
%% register: the same 4-load chain as the gcbif variant, without the
%% extended-register cleanup prelude (plain BIFs cannot GC). Replaces the
%% PRIM_GET_IMPORTED_BIF primitive call per bif0/1/2 site.
-spec move_imported_bif_to_native_register(state(), non_neg_integer()) ->
    {state(), x86_64_register()}.
move_imported_bif_to_native_register(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0,
    Bif
) ->
    Avail = jit_regs:available_regs(Regs0),
    PtrReg = first_avail(Avail),
    J1 = jit_x86_64_asm:movq(?JITSTATE_MODULE, PtrReg),
    J2 = jit_x86_64_asm:movq({?MODULE_IMPORTED_FUNCS, PtrReg}, PtrReg),
    J3 = jit_x86_64_asm:movq({Bif * ?WORD_SIZE, PtrReg}, PtrReg),
    J4 = jit_x86_64_asm:movq({?BIF_BIF0_PTR, PtrReg}, PtrReg),
    Stream1 = StreamModule:append(Stream0, <<J1/binary, J2/binary, J3/binary, J4/binary>>),
    Bit = reg_bit(PtrReg),
    Regs1 = jit_regs:alloc_reg(jit_regs:invalidate_reg(Regs0, PtrReg), Bit),
    {State0#state{stream = Stream1, regs = Regs1}, PtrReg}.

%% module_index << 24 (the high half of a cp or a catch term) in a freshly
%% allocated register: one load of the precomputed jit_state->cp_base.
-spec get_cp_base(state()) -> {state(), x86_64_register()}.
get_cp_base(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State
) ->
    Reg = first_avail(jit_regs:available_regs(Regs0)),
    I1 = jit_x86_64_asm:movq(?JITSTATE_CPBASE, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

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
    I1 = jit_x86_64_asm:movq(?JITSTATE_MODULE, Reg),
    I2 = jit_x86_64_asm:movl(?MODULE_INDEX(Reg), Reg),
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
%% register. The shared jit:get_module_atom_term/3 applies the term tag (the
%% shift_left/add in jit.erl), so this returns the raw, zero-extended 32-bit
%% global atom index. This is hot (every non-default atom literal access), so
%% inlining these loads avoids the primitive-call overhead per access.
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
    I1 = jit_x86_64_asm:movq(?JITSTATE_MODULE, Reg),
    %% Reg = module->local_atoms_to_global_table
    I2 = jit_x86_64_asm:movq(?MODULE_LOCAL_ATOMS_TABLE(Reg), Reg),
    %% Reg = local_atoms_to_global_table[AtomIndex] (a 32-bit global atom index,
    %% zero-extended into the 64-bit register). The entries are uint32_t (4 bytes
    %% wide). movl/2 only has a zero-displacement memory-source form in
    %% jit_x86_64_asm, so fold the AtomIndex*4 displacement into the base pointer
    %% first (the offset is a 32-bit immediate so any AtomIndex fits), then do
    %% the 32-bit movl which zero-extends into the full 64-bit register.
    Offset = AtomIndex * 4,
    LoadGid =
        case Offset of
            0 ->
                jit_x86_64_asm:movl({0, Reg}, Reg);
            _ ->
                <<
                    (jit_x86_64_asm:addq(Offset, Reg))/binary,
                    (jit_x86_64_asm:movl({0, Reg}, Reg))/binary
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

and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    {free, Reg},
    SrcReg
) when
    ?IS_GPR(Reg), is_atom(SrcReg)
->
    I1 = jit_x86_64_asm:andq(SrcReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} =
        State,
    {free, Reg},
    Val
) when
    ?IS_GPR(Reg), is_integer(Val), Val < -16#80 orelse Val > 16#FFFFFFFF
->
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Val, TempReg),
    I2 = jit_x86_64_asm:andq(TempReg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, {free, Reg}, Val
) when
    ?IS_GPR(Reg)
->
    % 32 bits instructions on x86-64 zero the high 32 bits
    I1 =
        if
            Val >= 0, Val =< 16#FFFFFFFF -> jit_x86_64_asm:andl(Val, Reg);
            true -> jit_x86_64_asm:andq(Val, Reg)
        end,
    Stream1 = StreamModule:append(Stream0, I1),
    %% AND modifies the register, invalidate its contents tracking
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = Regs1}, Reg};
and_(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Val
) when
    ?IS_GPR(Reg), is_integer(Val), Val < -16#80 orelse Val > 16#FFFFFFFF
->
    Avail = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Avail),
    Bit = reg_bit(ResultReg),
    I1 = jit_x86_64_asm:movabsq(Val, ResultReg),
    I2 = jit_x86_64_asm:andq(Reg, ResultReg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    Regs2 = jit_regs:alloc_reg(Regs1, Bit),
    {State#state{stream = Stream1, regs = Regs2}, ResultReg};
and_(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Val
) when
    ?IS_GPR(Reg)
->
    Avail = jit_regs:available_regs(Regs0),
    ResultReg = first_avail(Avail),
    Bit = reg_bit(ResultReg),
    I1 = jit_x86_64_asm:movq(Reg, ResultReg),
    I2 =
        if
            Val >= 0, Val =< 16#FFFFFFFF -> jit_x86_64_asm:andl(Val, ResultReg);
            true -> jit_x86_64_asm:andq(Val, ResultReg)
        end,
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, ResultReg),
    Regs2 = jit_regs:alloc_reg(Regs1, Bit),
    {State#state{stream = Stream1, regs = Regs2}, ResultReg}.

or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I1 = jit_x86_64_asm:orq(SrcReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
or_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Reg,
    Val
) when is_integer(Val), Val < -16#80000000 orelse Val > 16#7FFFFFFF ->
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Val, TempReg),
    I2 = jit_x86_64_asm:orq(TempReg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), Reg),
    State#state{stream = Stream1, regs = Regs1};
or_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:orq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

xor_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, SrcReg) when
    is_atom(SrcReg)
->
    I1 = jit_x86_64_asm:xorq(SrcReg, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
xor_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Reg,
    Val
) when is_integer(Val), Val < -16#80000000 orelse Val > 16#7FFFFFFF ->
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Val, TempReg),
    I2 = jit_x86_64_asm:xorq(TempReg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), Reg),
    State#state{stream = Stream1, regs = Regs1};
xor_(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:xorq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

add(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Val
) when is_integer(Val), Val < -16#80000000 orelse Val > 16#7FFFFFFF ->
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Val, TempReg),
    I2 = jit_x86_64_asm:addq(TempReg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), Reg),
    State#state{stream = Stream1, regs = Regs1};
add(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:addq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

sub(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    Reg,
    Val
) when is_integer(Val), Val < -16#80000000 orelse Val > 16#7FFFFFFF ->
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Val, TempReg),
    I2 = jit_x86_64_asm:subq(TempReg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), Reg),
    State#state{stream = Stream1, regs = Regs1};
sub(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) ->
    I1 = jit_x86_64_asm:subq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%% Add register or immediate Val to Reg in place, setting flags (OF on signed
%% overflow); testable with the `overflow_set' if-condition.
-spec add_overflow(state(), x86_64_register(), x86_64_register() | integer()) -> state().
add_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_atom(Val); ?IS_SINT32_T(Val) ->
    I1 = jit_x86_64_asm:addq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%% Subtract register or immediate Val from Reg in place, setting flags. See
%% add_overflow/3.
-spec sub_overflow(state(), x86_64_register(), x86_64_register() | integer()) -> state().
sub_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val
) when is_atom(Val); ?IS_SINT32_T(Val) ->
    I1 = jit_x86_64_asm:subq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1}.

%% Multiply two tagged small integers Reg and Val, leaving the product shifted
%% into the value field of Reg but WITHOUT the small-integer tag (low bits
%% zero); the caller re-tags on the no-overflow path. Flags are set so the
%% `mul_overflow_set' if-condition is true iff the result does NOT fit in a
%% small integer.
%%
%% Both operands are (v << 4) | TERM_INTEGER_TAG. Strip Reg's tag so it holds
%% a << 4, untag Val to b, then imul: Reg = (a << 4) * b = (a * b) << 4. Because
%% the kept factor is pre-shifted by the tag size, the 64-bit signed overflow
%% flag (OF) is set exactly when (a * b) leaves the small-integer value range.
%% Val (a scratch copy of the second operand) is clobbered; on overflow the
%% caller re-reads the original operands for the bignum BIF.
-spec mul_overflow(state(), x86_64_register(), x86_64_register()) -> state().
mul_overflow(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    Reg,
    Val
) when is_atom(Val) ->
    Avail = jit_regs:available_regs(Regs0),
    %% The small-integer tag occupies the low 4 bits (mask ?TERM_INTEGER_TAG);
    %% clear it to get the untagged value, multiply, and the tag is re-applied
    %% by the caller.
    {Code, Regs1} =
        case Reg =:= Val of
            false ->
                {
                    <<
                        (jit_x86_64_asm:andq(bnot ?TERM_INTEGER_TAG, Reg))/binary,
                        (jit_x86_64_asm:sarq(4, Val))/binary,
                        (jit_x86_64_asm:imulq(Val, Reg))/binary
                    >>,
                    jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Val)
                };
            true ->
                %% Squaring (Reg * Reg): the in-place sequence would `sarq' the
                %% very register it then multiplies, dropping the kept factor's
                %% << 4 and yielding (a*a) instead of (a*a) << 4. Copy the
                %% untagged factor into a scratch register first.
                Tmp = first_avail(Avail),
                {
                    <<
                        (jit_x86_64_asm:movq(Reg, Tmp))/binary,
                        (jit_x86_64_asm:andq(bnot ?TERM_INTEGER_TAG, Reg))/binary,
                        (jit_x86_64_asm:sarq(4, Tmp))/binary,
                        (jit_x86_64_asm:imulq(Tmp, Reg))/binary
                    >>,
                    jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, Reg), Tmp)
                }
        end,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1, regs = Regs1}.

-spec mul(state(), x86_64_register(), integer() | x86_64_register()) -> state().
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
mul(
    #state{
        stream_module = StreamModule, stream = Stream0, regs = Regs0
    } = State,
    Reg,
    Val
) when is_integer(Val), (Val < -16#80000000 orelse Val > 16#7FFFFFFF) ->
    Avail = jit_regs:available_regs(Regs0),
    TempReg = first_avail(Avail),
    I1 = jit_x86_64_asm:movabsq(Val, TempReg),
    I2 = jit_x86_64_asm:imulq(TempReg, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, TempReg), Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, Reg, Val) when
    is_integer(Val)
->
    I1 = jit_x86_64_asm:imulq(Val, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    State#state{stream = Stream1, regs = Regs1};
mul(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State, DestReg, SrcReg
) when is_atom(SrcReg) ->
    I1 = jit_x86_64_asm:imulq(SrcReg, DestReg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, DestReg),
    State#state{stream = Stream1, regs = Regs1}.

%% Signed integer division: quotient = DividendReg / DivisorReg
%% Uses idivq which divides rdx:rax by operand, quotient in rax.
%% rdx is the native interface pointer and must be saved/restored.
-spec div_(state(), x86_64_register(), x86_64_register()) -> {state(), rax}.
div_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    Avail = jit_regs:available_regs(Regs0),
    %% DivisorReg must not be rax (clobbered by dividend move) or rdx (clobbered by cqo).
    %% If DivisorReg is rax, move it to a temp register first.
    {I0, ActualDivisor, Regs1} =
        case DivisorReg of
            rax ->
                Temp = first_avail(Avail band (bnot reg_bit(DividendReg))),
                {jit_x86_64_asm:movq(rax, Temp), Temp, jit_regs:invalidate_reg(Regs0, Temp)};
            rdx ->
                Temp = first_avail(Avail band (bnot reg_bit(DividendReg))),
                {jit_x86_64_asm:movq(rdx, Temp), Temp, jit_regs:invalidate_reg(Regs0, Temp)};
            _ ->
                {<<>>, DivisorReg, Regs0}
        end,
    I1 =
        case DividendReg of
            rax -> <<>>;
            _ -> jit_x86_64_asm:movq(DividendReg, rax)
        end,
    I2 = jit_x86_64_asm:pushq(rdx),
    I3 = jit_x86_64_asm:cqo(),
    I4 = jit_x86_64_asm:idivq(ActualDivisor),
    I5 = jit_x86_64_asm:popq(rdx),
    Code = <<I0/binary, I1/binary, I2/binary, I3/binary, I4/binary, I5/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    %% The quotient register must be accounted as allocated (like rem_'s
    %% RemTemp): returning it merely invalidated let a later temp
    %% allocation hand out rax while it still held the live result.
    Regs2 = jit_regs:alloc_reg(jit_regs:invalidate_reg(Regs1, rax), reg_bit(rax)),
    {State#state{stream = Stream1, regs = Regs2}, rax}.

%% Signed integer remainder: remainder = DividendReg rem DivisorReg
%% Uses idivq which divides rdx:rax by operand, remainder in rdx.
%% rdx is the native interface pointer and must be saved/restored.
-spec rem_(state(), x86_64_register(), x86_64_register()) -> {state(), x86_64_register()}.
rem_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    Avail = jit_regs:available_regs(Regs0),
    %% We need a temp register to save the remainder (rdx) before restoring rdx.
    %% This temp must not be rax (quotient) or the DivisorReg.
    RemTemp = first_avail(
        Avail band (bnot reg_bit(rax)) band (bnot reg_bit(DivisorReg)) band
            (bnot reg_bit(DividendReg))
    ),
    {I0, ActualDivisor, Regs1} =
        case DivisorReg of
            rax ->
                Temp = first_avail(
                    Avail band (bnot reg_bit(DividendReg)) band (bnot reg_bit(RemTemp))
                ),
                {jit_x86_64_asm:movq(rax, Temp), Temp, jit_regs:invalidate_reg(Regs0, Temp)};
            rdx ->
                Temp = first_avail(
                    Avail band (bnot reg_bit(DividendReg)) band (bnot reg_bit(RemTemp))
                ),
                {jit_x86_64_asm:movq(rdx, Temp), Temp, jit_regs:invalidate_reg(Regs0, Temp)};
            _ ->
                {<<>>, DivisorReg, Regs0}
        end,
    I1 =
        case DividendReg of
            rax -> <<>>;
            _ -> jit_x86_64_asm:movq(DividendReg, rax)
        end,
    I2 = jit_x86_64_asm:pushq(rdx),
    I3 = jit_x86_64_asm:cqo(),
    I4 = jit_x86_64_asm:idivq(ActualDivisor),
    I5 = jit_x86_64_asm:movq(rdx, RemTemp),
    I6 = jit_x86_64_asm:popq(rdx),
    Code = <<I0/binary, I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    RemBit = reg_bit(RemTemp),
    Regs2 = jit_regs:invalidate_reg(Regs1, rax),
    Regs3 = jit_regs:invalidate_reg(Regs2, RemTemp),
    Regs4 = jit_regs:alloc_reg(Regs3, RemBit),
    {State#state{stream = Stream1, regs = Regs4}, RemTemp}.

%% x86_64 always supports native idivq.
-spec supports_div(state()) -> boolean().
supports_div(_State) -> true.

%% x86_64 has SSE2, so it can inline double-precision fadd/fsub/fmul/fdiv. The
%% single-precision (FLOAT32) variant stores 4-byte floats in the fp register
%% array and is not handled inline here, so it falls back to the C primitive.
-spec supports_fp(state()) -> boolean().
supports_fp(#state{variant = Variant}) ->
    Variant band ?JIT_VARIANT_FLOAT32 =:= 0.

%% Inline a double-precision binary float op fr[F3] = fr[F1] <op> fr[F2], and
%% return a register that is 0 iff the result is non-finite (so the caller can
%% raise badarith with the same test used for the C primitive's boolean result).
-spec float_op(state(), non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {state(), x86_64_register()}.
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
            ?PRIM_FADD -> fun jit_x86_64_asm:addsd/2;
            ?PRIM_FSUB -> fun jit_x86_64_asm:subsd/2;
            ?PRIM_FMUL -> fun jit_x86_64_asm:mulsd/2;
            ?PRIM_FDIV -> fun jit_x86_64_asm:divsd/2
        end,
    CheckReg = first_avail(Avail0),
    BaseReg = first_avail(Avail0 band (bnot reg_bit(CheckReg))),
    %% Load the fp register array pointer (jit_state->fr), compute the operation in
    %% xmm0, store it back to fr[F3], then test the result's exponent bits: a
    %% value is non-finite (inf/nan) iff all exponent bits are set. The caller's
    %% badarith test reads CheckReg as a one-byte boolean (testb), so collapse
    %% the result to a clean 0/1 with setne (1 = finite, 0 = non-finite).
    I1 = jit_x86_64_asm:movq(?FP_REGS, BaseReg),
    I2 = jit_x86_64_asm:movsd(xmm0, {?FP_REG_OFFSET(State0, F1), BaseReg}),
    I3 = jit_x86_64_asm:movsd(xmm1, {?FP_REG_OFFSET(State0, F2), BaseReg}),
    I4 = Op(xmm0, xmm1),
    I5 = jit_x86_64_asm:movsd({?FP_REG_OFFSET(State0, F3), BaseReg}, xmm0),
    I6 = jit_x86_64_asm:movsd_to_gpr(CheckReg, xmm0),
    I7 = jit_x86_64_asm:movabsq(16#7FF0000000000000, BaseReg),
    I8 = jit_x86_64_asm:andq(BaseReg, CheckReg),
    I9 = jit_x86_64_asm:xorq(BaseReg, CheckReg),
    I10 = jit_x86_64_asm:setne(CheckReg),
    Code =
        <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary, I7/binary, I8/binary,
            I9/binary, I10/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    CheckBit = reg_bit(CheckReg),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, BaseReg), CheckReg),
    {
        State0#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, CheckBit)},
        CheckReg
    }.

%% Convert an untagged signed integer (already in IntReg, i.e. the small-int
%% term shifted right past its tag) to a double and store it in fr[FPRegIndex].
%% Used by the inline fconv fast path; integer-to-double can never be
%% non-finite, so there is nothing to check and no register is returned.
-spec float_conv_int(state(), x86_64_register(), non_neg_integer()) -> state().
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
    BaseReg = first_avail(Avail0),
    I1 = jit_x86_64_asm:movq(?FP_REGS, BaseReg),
    I2 = jit_x86_64_asm:cvtsi2sd(xmm0, IntReg),
    I3 = jit_x86_64_asm:movsd({?FP_REG_OFFSET(State0, FPRegIndex), BaseReg}, xmm0),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, BaseReg),
    State0#state{stream = Stream1, regs = Regs1}.

%% Unbox a boxed float term (in BoxedReg) and store its double value into
%% fr[FPRegIndex]. The double lives just past the boxed header word, i.e. at
%% offset one word from the untagged boxed pointer. BoxedReg is clobbered by
%% the in-place untag, so it must be passed as {free, Reg}: it is invalidated
%% (any cached vm-register association would be stale) and returned to the
%% pool.
-spec float_conv_float(state(), {free, x86_64_register()}, non_neg_integer()) -> state().
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
    BaseReg = first_avail(Avail0 band (bnot reg_bit(BoxedReg))),
    %% Clear the 2 primary (boxed) tag bits (mask 0x3) to get the boxed pointer
    %% (term.hrl not included here, so use the literal), load the double from
    %% boxed_ptr[1], load fr base, and store to fr[FPRegIndex].
    I1 = jit_x86_64_asm:andq(bnot 16#3, BoxedReg),
    I2 = jit_x86_64_asm:movsd(xmm0, {8, BoxedReg}),
    I3 = jit_x86_64_asm:movq(?FP_REGS, BaseReg),
    I4 = jit_x86_64_asm:movsd({?FP_REG_OFFSET(State0, FPRegIndex), BaseReg}, xmm0),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(Regs0, BaseReg),
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
    I1 = jit_x86_64_asm:movabsq(Bits, BitsReg),
    I2 = jit_x86_64_asm:movq(?FP_REGS, BaseReg),
    I3 = jit_x86_64_asm:movq(BitsReg, {?FP_REG_OFFSET(State0, FPRegIndex), BaseReg}),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, BitsReg), BaseReg),
    State0#state{stream = Stream1, regs = Regs1}.

%% Load the free space between heap and stack (ctx->e - ctx->heap.heap_ptr,
%% in bytes) into a freshly allocated register, for the inline test_heap fast
%% path.
%% Box the double in fr[FPRegIndex] as a float term, inline: the BEAM
%% compiler emits fmove-to-register only after a test_heap that reserved the
%% float's words, so the bump allocation cannot overflow. Replaces the
%% PRIM_TERM_FROM_FLOAT call (one C call per iteration in float loops).
%% Only used when supports_fp/1 holds (double-precision variant).
-spec term_from_float_inline(state(), non_neg_integer()) -> {state(), x86_64_register()}.
term_from_float_inline(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State0,
    FPRegIndex
) ->
    Avail0 = jit_regs:available_regs(Regs0),
    HpReg = first_avail(Avail0),
    Tmp = first_avail(Avail0 band (bnot reg_bit(HpReg))),
    %% float boxed term: header ((FLOAT_SIZE - 1) << 6) | TERM_BOXED_FLOAT
    %% = (1 << 6) | 16#18 on 64-bit, then the raw double (term.hrl is not
    %% included here, so use the literals directly).
    FloatHeader = (1 bsl 6) bor 16#18,
    I1 = jit_x86_64_asm:movq(?HP_REG, HpReg),
    I2 = jit_x86_64_asm:movq(FloatHeader, {0, HpReg}),
    I3 = jit_x86_64_asm:movq(?FP_REGS, Tmp),
    I4 = jit_x86_64_asm:movq({?FP_REG_OFFSET(State0, FPRegIndex), Tmp}, Tmp),
    I5 = jit_x86_64_asm:movq(Tmp, {8, HpReg}),
    I6 = jit_x86_64_asm:addq(16, ?HP_REG),
    I7 = <<>>,
    %% TERM_PRIMARY_BOXED = 2
    I8 = jit_x86_64_asm:orq(2, HpReg),
    Code =
        <<I1/binary, I2/binary, I3/binary, I4/binary, I5/binary, I6/binary, I7/binary, I8/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs1 = jit_regs:invalidate_reg(jit_regs:invalidate_reg(Regs0, HpReg), Tmp),
    {
        State0#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(HpReg))},
        HpReg
    }.

%% This backend accepts {{x_reg, X}, '!=' | '==', Imm} if_block conditions
%% (fused memory-operand compare).
-spec supports_vm_reg_cond() -> true.
supports_vm_reg_cond() ->
    true.

-spec read_avail_heap_memory(state()) -> {state(), x86_64_register()}.
read_avail_heap_memory(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    I1 = jit_x86_64_asm:movq(?E_REG, Reg),
    I3 = jit_x86_64_asm:subq(?HP_REG, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I3/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

%% Bump-allocate NWords terms from the context heap, returning a freshly
%% allocated register holding the pointer to the first allocated word. The
%% space is already reserved by the preceding test_heap/allocate (BEAM
%% bytecode guarantees it), so this is memory_heap_alloc inlined: no bounds
%% check, just a heap_ptr load/add/store.
-spec heap_bump_alloc(state(), pos_integer()) -> {state(), x86_64_register()}.
heap_bump_alloc(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State,
    NWords
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    I1 = jit_x86_64_asm:movq(?HP_REG, Reg),
    I2 = jit_x86_64_asm:addq(NWords * ?WORD_SIZE, ?HP_REG),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))},
        Reg
    }.

%% Load ctx->heap.root->next into a freshly allocated register, so deallocate
%% can test for pending heap fragments inline and only call the primitive
%% (which compacts them) when there are any.
-spec read_heap_fragments(state()) -> {state(), x86_64_register()}.
read_heap_fragments(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    I1 = jit_x86_64_asm:movq({16#8, ?CTX_REG}, Reg),
    I2 = jit_x86_64_asm:movq({0, Reg}, Reg),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
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
    Avail = jit_regs:available_regs(Regs0),
    Tmp = first_avail(Avail),
    I2 = jit_x86_64_asm:subq((StackNeed + 1) * ?WORD_SIZE, ?E_REG),
    I4 = jit_x86_64_asm:movq(?CP, Tmp),
    I5 = jit_x86_64_asm:movq(Tmp, {StackNeed * ?WORD_SIZE, ?E_REG}),
    Stream1 = StreamModule:append(
        Stream0, <<I2/binary, I4/binary, I5/binary>>
    ),
    Regs1 = jit_regs:invalidate_reg(Regs0, Tmp),
    State#state{stream = Stream1, regs = Regs1}.

%% Load the fp register array pointer (jit_state->fr) into a freshly allocated
%% register and return it, so the caller can test it for NULL and only call
%% the ensure_fpregs primitive (the malloc) when it has not been allocated yet.
-spec read_fp_regs_ptr(state()) -> {state(), x86_64_register()}.
read_fp_regs_ptr(
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        regs = Regs0
    } = State
) ->
    Avail = jit_regs:available_regs(Regs0),
    Reg = first_avail(Avail),
    Bit = reg_bit(Reg),
    I1 = jit_x86_64_asm:movq(?FP_REGS, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {
        State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, Bit)},
        Reg
    }.

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State0
) ->
    Avail = jit_regs:available_regs(Regs0),
    Temp = first_avail(Avail),
    Regs1 = jit_regs:invalidate_reg(Regs0, Temp),
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:decl(?JITSTATE_REMAINING_REDUCTIONS),
    {RewriteJNZOffset, I2} = jit_x86_64_asm:jnz_rel8(0),
    {RewriteLEAOffset, I3} = jit_x86_64_asm:leaq_rel32({0, rip}, Temp),
    I4 = jit_x86_64_asm:movq(Temp, ?JITSTATE_CONTINUATION),
    Code = <<I1/binary, I2/binary, I3/binary, I4/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State1 = State0#state{stream = Stream1, regs = Regs1},
    State2 = call_primitive_last(State1, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]),
    % Rewrite jumps
    #state{stream = Stream2} = State2,
    NewOffset = StreamModule:offset(Stream2),
    Stream3 = StreamModule:replace(Stream2, Offset + byte_size(I1) + RewriteJNZOffset, <<
        (NewOffset - Offset - byte_size(I1) - byte_size(I2))
    >>),
    Stream4 = StreamModule:replace(
        Stream3, Offset + byte_size(I1) + byte_size(I2) + RewriteLEAOffset, <<
            (NewOffset - Offset - byte_size(I1) - byte_size(I2) - byte_size(I3)):32/little
        >>
    ),
    %% schedule_next clobbers caller-saved regs; restore the pre-call masks
    %% (preserved by invalidate_all) and drop any cached contents.
    State2#state{stream = Stream4, regs = jit_regs:invalidate_all(State1#state.regs)}.

-spec call_or_schedule_next(state(), non_neg_integer()) -> state().
call_or_schedule_next(State0, Label) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_only_or_schedule_next(State1, Label),
    rewrite_cp_offset(State2, RewriteOffset).

call_only_or_schedule_next(
    StateP,
    Label
) ->
    %% Both the hot jump and the reschedule resume by reading ctx->x at
    %% Label: pendings the target reads keep their store.
    #state{
        stream_module = StreamModule,
        stream = Stream0,
        branches = Branches,
        labels = Labels
    } = State0 = pending_filter_label(StateP, Label),
    Offset = StreamModule:offset(Stream0),
    I1 = jit_x86_64_asm:decl(?JITSTATE_REMAINING_REDUCTIONS),
    I1Size = byte_size(I1),

    case Labels of
        #{Label := LabelOffset} ->
            % Label is already known, emit direct jmp with calculated offset
            % jz is 2 bytes, jmp_rel32 is 5 bytes
            JmpSize = 5,
            I2 = jit_x86_64_asm:jz(JmpSize + 2),
            I2Size = byte_size(I2),
            % Calculate relative offset: target - current
            RelOffset = LabelOffset - (Offset + I1Size + I2Size),
            {_RewriteJMPOffset, I3} = jit_x86_64_asm:jmp_rel32(RelOffset),
            Code = <<I1/binary, I2/binary, I3/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            State1 = State0#state{stream = Stream1};
        _ ->
            % Label not yet known, emit placeholder and add relocation
            {RewriteJMPOffset, I3} = jit_x86_64_asm:jmp_rel32(1),
            I2 = jit_x86_64_asm:jz(byte_size(I3) + 2),
            Sz = I1Size + byte_size(I2),
            BrEntry = {Offset + Sz + RewriteJMPOffset, 32},
            Code = <<I1/binary, I2/binary, I3/binary>>,
            Stream1 = StreamModule:append(Stream0, Code),
            ExistingBrs = maps:get(Label, Branches, []),
            State1 = State0#state{
                stream = Stream1,
                branches = Branches#{Label => [BrEntry | ExistingBrs]}
            }
    end,
    State2 = set_continuation_to_label(State1, Label),
    call_primitive_last(State2, ?PRIM_SCHEDULE_NEXT_CP, [ctx, jit_state]).

call_primitive_with_cp(State0, Primitive, Args) ->
    {State1, RewriteOffset} = set_cp(State0),
    State2 = call_primitive_last(State1, Primitive, Args),
    rewrite_cp_offset(State2, RewriteOffset).

%%-----------------------------------------------------------------------------
%% @doc OP_CALL_FUN/OP_CALL_FUN2 with an inline fast path for a local fun:
%% resolve the fun's module and label from its boxed representation and the
%% callee module's fun table, copy the frozen variables into the x registers
%% and branch to the callee's jump-table entry -- what PRIM_CALL_FUN_DIRECT
%% does in C. Anything the fast path cannot prove (external fun, arity
%% mismatch, too many frozen vars, emulated callee) falls through to the
%% primitive.
%% @end
%%-----------------------------------------------------------------------------
-spec call_fun_with_cp_direct(state(), non_neg_integer(), [arg()]) -> state().
call_fun_with_cp_direct(
    State0, Primitive, [ctx, jit_state, offset, FunRegArg, ArgsCount] = Args
) when
    is_integer(ArgsCount)
->
    {State1, RewriteOffset} = set_cp(State0),
    %% The fast path branches away without a C call, so deferred vm-register
    %% stores must be committed first (the callee reads ctx->x[]).
    #state{regs = Regs1} = State2 = pending_clear_all(State1),
    FunReg =
        case FunRegArg of
            {free, FR} -> FR;
            FR when is_atom(FR) -> FR
        end,
    Avail = [R || R <- mask_to_list(jit_regs:available_regs(Regs1)), R =/= FunReg],
    State3 =
        case length(Avail) >= 7 of
            true ->
                [T0, T1, T2, T3, T4, T5, T6 | _] = Avail,
                emit_call_fun_fast_path(State2, FunReg, ArgsCount, T0, T1, T2, T3, T4, T5, T6);
            false ->
                State2
        end,
    {State4, ResultReg} = call_primitive(State3, Primitive, Args),
    State5 = direct_dispatch(State4, ResultReg, true),
    rewrite_cp_offset(State5, RewriteOffset).

%% @private
%% The inline local-fun resolve. Every check that fails branches to the first
%% instruction after this block (the primitive call the caller emits), so the
%% code is built back to front: each guard's displacement is the size of
%% everything that follows it.
emit_call_fun_fast_path(State0, FunReg, ArgsCount, T0, T1, T2, T3, T4, T5, T6) ->
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    %% Copy the frozen variables from boxed[3..] to x[ArgsCount..]. T6 holds
    %% n_freeze, already known to be small enough that the range fits.
    CopyBody = <<
        (jit_x86_64_asm:movq({0, T1}, T2))/binary,
        (jit_x86_64_asm:movq(T2, {0, T0}))/binary,
        (jit_x86_64_asm:addq(8, T1))/binary,
        (jit_x86_64_asm:addq(8, T0))/binary,
        (jit_x86_64_asm:subq(1, T6))/binary
    >>,
    LoopBack = jit_x86_64_asm:jnz(-(byte_size(CopyBody) + 2)),
    Copy = <<
        (jit_x86_64_asm:addq(24, T1))/binary,
        (jit_x86_64_asm:leaq({16#58 + 8 * ArgsCount, ?CTX_REG}, T0))/binary,
        (jit_x86_64_asm:testq(T6, T6))/binary,
        (jit_x86_64_asm:jz(2 + byte_size(CopyBody) + byte_size(LoopBack)))/binary,
        CopyBody/binary,
        LoopBack/binary
    >>,
    %% Frozen vars in place: switch jit_state to the callee's module and jump
    %% to its jump-table entry (native_code + label * JUMP_TABLE_ENTRY_SIZE).
    SegF = <<
        Copy/binary,
        (jit_x86_64_asm:movq(T3, ?JITSTATE_MODULE))/binary,
        (jit_x86_64_asm:movl({0, T3}, T2))/binary,
        (jit_x86_64_asm:shlq(24, T2))/binary,
        (jit_x86_64_asm:movq(T2, ?JITSTATE_CPBASE))/binary,
        (jit_x86_64_asm:movq(T5, T0))/binary,
        (jit_x86_64_asm:shlq(2, T0))/binary,
        (jit_x86_64_asm:addq(T5, T0))/binary,
        (jit_x86_64_asm:addq(T0, T4))/binary,
        (jit_x86_64_asm:jmpq({T4}))/binary
    >>,
    %% Callee label, and its module's native code -- absent means emulated.
    SegE = <<
        (jit_x86_64_asm:movl({?FUN_TABLE_LABEL, T4}, T5))/binary,
        (jit_x86_64_asm:bswapl(T5))/binary,
        (jit_x86_64_asm:movq({?MODULE_NATIVE_CODE, T3}, T4))/binary,
        (jit_x86_64_asm:testq(T4, T4))/binary
    >>,
    %% arity + frozen vars must fit in the x registers.
    SegD = jit_x86_64_asm:cmpq(?MAX_REG, T5),
    %% fun_index -> fun table entry (24 bytes each), then arity and n_freeze
    %% (both big-endian); the fun's own arity is arity_and_freeze - n_freeze
    %% and must match the call site.
    SegC = <<
        (jit_x86_64_asm:shrq(4, T2))/binary,
        (jit_x86_64_asm:movq({?MODULE_FUN_TABLE, T3}, T4))/binary,
        (jit_x86_64_asm:movq(T2, T0))/binary,
        (jit_x86_64_asm:shlq(3, T0))/binary,
        (jit_x86_64_asm:addq(T0, T4))/binary,
        (jit_x86_64_asm:shlq(1, T0))/binary,
        (jit_x86_64_asm:addq(T0, T4))/binary,
        (jit_x86_64_asm:movl({?FUN_TABLE_ARITY, T4}, T5))/binary,
        (jit_x86_64_asm:bswapl(T5))/binary,
        (jit_x86_64_asm:movl({?FUN_TABLE_N_FREEZE, T4}, T6))/binary,
        (jit_x86_64_asm:bswapl(T6))/binary,
        (jit_x86_64_asm:movq(T5, T0))/binary,
        (jit_x86_64_asm:subq(T6, T0))/binary,
        (jit_x86_64_asm:cmpq(ArgsCount, T0))/binary
    >>,
    %% boxed[1] is a Module* (pointer-aligned) for a local fun, or a tagged
    %% atom for an external one.
    SegB = <<
        (jit_x86_64_asm:movq({8, T1}, T3))/binary,
        (jit_x86_64_asm:testq(3, T3))/binary
    >>,
    %% Unbox the fun (verify_is_function already checked the boxed header);
    %% boxed[2] is the fun index (a small int) for a local fun, or the
    %% function name (an atom) for an external one.
    SegA = <<
        (jit_x86_64_asm:movq(FunReg, T1))/binary,
        (jit_x86_64_asm:andq(-4, T1))/binary,
        (jit_x86_64_asm:movq({16, T1}, T2))/binary,
        (jit_x86_64_asm:movq(T2, T6))/binary,
        (jit_x86_64_asm:andq(16#F, T6))/binary,
        (jit_x86_64_asm:cmpq(16#F, T6))/binary
    >>,
    Slow = fun(JccFun, Rest) ->
        {_RelocOffset, Jcc} = JccFun(6 + byte_size(Rest)),
        <<Jcc/binary, Rest/binary>>
    end,
    Tail5 = Slow(fun jit_x86_64_asm:jz_rel32/1, SegF),
    Tail4 = Slow(fun jit_x86_64_asm:ja_rel32/1, <<SegE/binary, Tail5/binary>>),
    Tail3 = Slow(fun jit_x86_64_asm:jnz_rel32/1, <<SegD/binary, Tail4/binary>>),
    Tail2 = Slow(fun jit_x86_64_asm:jnz_rel32/1, <<SegC/binary, Tail3/binary>>),
    Tail1 = Slow(fun jit_x86_64_asm:jnz_rel32/1, <<SegB/binary, Tail2/binary>>),
    Code = <<SegA/binary, Tail1/binary>>,
    State0#state{stream = StreamModule:append(Stream0, Code)}.

%%-----------------------------------------------------------------------------
%% @doc OP_CALL_EXT/OP_CALL_EXT_LAST with an inline fast path for a target
%% that is already resolved to native code: read the imported function, check
%% it has been upgraded to a ModuleNativeFunction, switch jit_state to the
%% callee's module and branch straight to its native entry -- what
%% PRIM_CALL_EXT_DIRECT does in C, without the call round trip. Anything else
%% (unresolved import, BIF, NIF, emulated target) falls through to the
%% primitive.
%%
%% Unlike aarch64 no acquire load is needed for func->type: on x86-64 every
%% ordinary load is an acquire, so a plain `movl' already orders against the
%% release store of the in-place ModuleFunction -> ModuleNativeFunction
%% upgrade in jit.c.
%% @end
%%-----------------------------------------------------------------------------
-spec call_ext_with_cp_direct(state(), non_neg_integer(), non_neg_integer(), [arg()]) -> state().
call_ext_with_cp_direct(State0, Primitive, Index, Args) ->
    {State1, RewriteOffset} = set_cp(State0),
    %% The fast path branches away without a C call: deferred vm-register
    %% stores must be committed first (the callee reads ctx->x[]).
    #state{regs = Regs1} = State2 = pending_clear_all(State1),
    Avail = mask_to_list(jit_regs:available_regs(Regs1)),
    State3 =
        case length(Avail) >= 4 andalso ?IS_SINT32_T(Index * 8) of
            true ->
                [T0, T1, T2, T3 | _] = Avail,
                emit_call_ext_fast_path(State2, Index, T0, T1, T2, T3);
            false ->
                State2
        end,
    {State4, ResultReg} = call_primitive(State3, Primitive, Args),
    State5 = direct_dispatch(State4, ResultReg, true),
    rewrite_cp_offset(State5, RewriteOffset).

%% @private
%% The inline resolved call_ext; the type-check branch targets the first
%% instruction after this block, i.e. the primitive call the caller emits.
emit_call_ext_fast_path(State0, Index, T0, T1, T2, T3) ->
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    Code = call_ext_fast_path_code(Index, T0, T1, T2, T3, <<>>, true),
    State0#state{stream = StreamModule:append(Stream0, Code)}.

%% @private
%% Shared body of the two inline resolved call_ext forms. `Extra' is emitted
%% between the type check and the module switch (the frame pop of
%% CALL_EXT_LAST); `ClearCont' clears any stale continuation, as the *_direct
%% C wrappers do, which only the non-tail form needs.
call_ext_fast_path_code(Index, T0, T1, T2, T3, Extra, ClearCont) ->
    %% func = jit_state->module->imported_funcs[Index]
    Head = <<
        (jit_x86_64_asm:movq(?JITSTATE_MODULE, T0))/binary,
        (jit_x86_64_asm:movq({?MODULE_IMPORTED_FUNCS, T0}, T0))/binary,
        (jit_x86_64_asm:movq({Index * 8, T0}, T1))/binary,
        %% func->type must be ModuleNativeFunction (7).
        (jit_x86_64_asm:movl({0, T1}, T2))/binary,
        (jit_x86_64_asm:cmpl(7, T2))/binary
    >>,
    ContClear =
        case ClearCont of
            true -> jit_x86_64_asm:movq(0, ?JITSTATE_CONTINUATION);
            false -> <<>>
        end,
    Tail = <<
        ContClear/binary,
        Extra/binary,
        %% target Module* and native entry point
        (jit_x86_64_asm:movq({8, T1}, T3))/binary,
        (jit_x86_64_asm:movq({16, T1}, T1))/binary,
        %% jit_state_set_module: module + cp_base (module_index << 24)
        (jit_x86_64_asm:movq(T3, ?JITSTATE_MODULE))/binary,
        (jit_x86_64_asm:movl({0, T3}, T2))/binary,
        (jit_x86_64_asm:shlq(24, T2))/binary,
        (jit_x86_64_asm:movq(T2, ?JITSTATE_CPBASE))/binary,
        %% branch to the callee's native entry
        (jit_x86_64_asm:jmpq({T1}))/binary
    >>,
    {_RelocOffset, Jne} = jit_x86_64_asm:jnz_rel32(6 + byte_size(Tail)),
    <<Head/binary, Jne/binary, Tail/binary>>.

%%-----------------------------------------------------------------------------
%% @doc OP_CALL_EXT_LAST/OP_CALL_EXT_ONLY with the same inline resolved fast
%% path as call_ext_with_cp_direct/4. Tail position: no cp is set here; for
%% CALL_EXT_LAST (NWords >= 0) the fast path also pops the frame
%% (cp = e[NWords], e += NWords + 1) exactly like the primitive would.
%% @end
%%-----------------------------------------------------------------------------
-spec call_ext_last_direct(state(), non_neg_integer(), non_neg_integer(), integer(), [arg()]) ->
    state().
call_ext_last_direct(State0, Primitive, Index, NWords, Args) ->
    #state{regs = Regs1} = State1 = pending_clear_all(State0),
    Avail = mask_to_list(jit_regs:available_regs(Regs1)),
    State2 =
        case
            length(Avail) >= 4 andalso ?IS_SINT32_T(Index * 8) andalso
                ?IS_SINT32_T(NWords * 8)
        of
            true ->
                [T0, T1, T2, T3 | _] = Avail,
                emit_call_ext_last_fast_path(State1, Index, NWords, T0, T1, T2, T3);
            false ->
                State1
        end,
    call_primitive_direct(State2, Primitive, Args).

%% @private
emit_call_ext_last_fast_path(State0, Index, NWords, T0, T1, T2, T3) ->
    #state{stream_module = StreamModule, stream = Stream0} = State0,
    FramePop =
        case NWords >= 0 of
            true ->
                %% cp = e[NWords]; e += NWords + CP_SIZE_IN_TERMS
                <<
                    (jit_x86_64_asm:movq({NWords * 8, ?E_REG}, T2))/binary,
                    (jit_x86_64_asm:movq(T2, ?CP))/binary,
                    (jit_x86_64_asm:addq((NWords + 1) * 8, ?E_REG))/binary
                >>;
            false ->
                <<>>
        end,
    Code = call_ext_fast_path_code(Index, T0, T1, T2, T3, FramePop, false),
    State0#state{stream = StreamModule:append(Stream0, Code)}.

%% Tail-position variant: no cp is set (the callee returns to the caller's
%% caller) and STAY cannot occur, so only bit 0 is dispatched on. Code after
%% this is unreachable from this site.
-spec call_primitive_direct(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_direct(State0, Primitive, Args) ->
    {State1, ResultReg} = call_primitive(State0, Primitive, Args),
    State2 = direct_dispatch(State1, ResultReg, false),
    State2#state{regs = jit_regs:invalidate_all(State2#state.regs)}.

%% @private
%% Emit the tagged-result dispatch shared by the *_direct primitives (see
%% call_ext_with_cp_direct for the contract).
-spec direct_dispatch(state(), x86_64_register(), boolean()) -> state().
direct_dispatch(
    #state{stream_module = StreamModule, stream = Stream0} = State0, ResultReg, TestStay
) ->
    MovRet =
        case ResultReg of
            rax ->
                jit_x86_64_asm:retq();
            _ ->
                Mov = jit_x86_64_asm:movq(ResultReg, rax),
                Ret = jit_x86_64_asm:retq(),
                <<Mov/binary, Ret/binary>>
        end,
    IJmpCont = jit_x86_64_asm:jmpq(?JITSTATE_CONTINUATION),
    ITest0 = jit_x86_64_asm:testb(1, ResultReg),
    %% The primitive may have terminated this process, in which case it returns
    %% a plain (untagged) pointer to another Context and ctx is already freed:
    %% the hp/e reload is deferred to here, past the untagged early-out, so it
    %% only ever reads a live context.
    RL = reload_hp_e_code(),
    Code =
        case TestStay of
            true ->
                ITest1 = jit_x86_64_asm:testb(2, ResultReg),
                IJnz1 = jit_x86_64_asm:jnz(2 + byte_size(IJmpCont) + byte_size(MovRet)),
                IJz0 = jit_x86_64_asm:jz(
                    2 + byte_size(RL) + byte_size(ITest1) + byte_size(IJnz1) +
                        byte_size(IJmpCont)
                ),
                <<ITest0/binary, IJz0/binary, RL/binary, ITest1/binary, IJnz1/binary,
                    IJmpCont/binary, MovRet/binary>>;
            false ->
                IJz0 = jit_x86_64_asm:jz(2 + byte_size(RL) + byte_size(IJmpCont)),
                <<ITest0/binary, IJz0/binary, RL/binary, IJmpCont/binary, MovRet/binary>>
        end,
    Stream1 = StreamModule:append(Stream0, Code),
    free_native_register(State0#state{stream = Stream1}, ResultReg).

-spec set_cp(state()) -> {state(), non_neg_integer()}.
set_cp(State0) ->
    % get module index (dynamically)
    {#state{stream_module = StreamModule, stream = Stream0} = State1, Reg} = get_module_index(
        State0
    ),
    Offset = StreamModule:offset(Stream0),
    % build cp with module_index << 24
    I1 = jit_x86_64_asm:shlq(24, Reg),
    % next part of cp is instruction offset, after the call.
    {RewriteOffset, I2} = jit_x86_64_asm:orq_rel32(0, Reg),
    AddrOffset = Offset + byte_size(I1) + RewriteOffset,
    I3 = jit_x86_64_asm:movq(Reg, ?CP),
    Code = <<I1/binary, I2/binary, I3/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State2 = State1#state{stream = Stream1},
    State3 = free_native_register(State2, Reg),
    {State3, AddrOffset}.

-spec rewrite_cp_offset(state(), non_neg_integer()) -> state().
rewrite_cp_offset(
    #state{stream_module = StreamModule, stream = Stream0, offset = CodeOffset} = State0,
    RewriteOffset
) ->
    NewOffset = StreamModule:offset(Stream0) - CodeOffset,
    % Encode ReturnAddrOffset << 2
    Stream1 = StreamModule:replace(Stream0, RewriteOffset, <<(NewOffset bsl 2):32/little>>),
    %% Execution resumes here when the callee returns: registers are
    %% clobbered and, crucially, code is reachable again.
    State0#state{stream = Stream1, regs = jit_regs:invalidate_all(State0#state.regs)}.

set_bs(#state{stream_module = StreamModule, stream = Stream0} = State0, TermReg) ->
    I1 = jit_x86_64_asm:movq(TermReg, ?BS),
    I2 = jit_x86_64_asm:movq(0, ?BS_OFFSET),
    Stream1 = StreamModule:append(Stream0, <<I1/binary, I2/binary>>),
    State0#state{stream = Stream1}.

%% @doc Load ctx->bs, the binary the legacy bs_put_* opcodes fill in place.
-spec get_bs(state()) -> {state(), x86_64_register()}.
get_bs(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State) ->
    Reg = first_avail(jit_regs:available_regs(Regs0)),
    I1 = jit_x86_64_asm:movq(?BS, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))}, Reg}.

%% @doc Load ctx->bs_offset, the bit offset the next segment is written at.
-spec get_bs_offset(state()) -> {state(), x86_64_register()}.
get_bs_offset(#state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State) ->
    Reg = first_avail(jit_regs:available_regs(Regs0)),
    I1 = jit_x86_64_asm:movq(?BS_OFFSET, Reg),
    Stream1 = StreamModule:append(Stream0, I1),
    Regs1 = jit_regs:invalidate_reg(Regs0, Reg),
    {State#state{stream = Stream1, regs = jit_regs:alloc_reg(Regs1, reg_bit(Reg))}, Reg}.

%% @doc Store ctx->bs_offset after a segment has been written.
-spec set_bs_offset(state(), x86_64_register()) -> state().
set_bs_offset(#state{stream_module = StreamModule, stream = Stream0} = State0, OffsetReg) ->
    I1 = jit_x86_64_asm:movq(OffsetReg, ?BS_OFFSET),
    State0#state{stream = StreamModule:append(Stream0, I1)}.

%%-----------------------------------------------------------------------------
%% @param State current state
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
        stream = Stream0,
        labels = Labels
    } = State,
    SortedLines
) ->
    SortedLabels = lists:keysort(2, [
        {Label, LabelOffset}
     || {Label, LabelOffset} <- maps:to_list(Labels), is_integer(Label), Label /= 0
    ]),

    I2 = jit_x86_64_asm:retq(),
    {_RewriteLEAOffset, I1} = jit_x86_64_asm:leaq_rel32({byte_size(I2), rip}, rax),
    LabelsTable = <<<<Label:16, Offset:32>> || {Label, Offset} <- SortedLabels>>,
    LinesTable = <<<<Line:16, Offset:32>> || {Line, Offset} <- SortedLines>>,
    Stream1 = StreamModule:append(
        Stream0,
        <<I1/binary, I2/binary, (length(SortedLabels)):16, LabelsTable/binary,
            (length(SortedLines)):16, LinesTable/binary>>
    ),
    State#state{stream = Stream1}.

reg_bit(rax) -> ?REG_BIT_RAX;
reg_bit(rcx) -> ?REG_BIT_RCX;
reg_bit(rdx) -> ?REG_BIT_RDX;
reg_bit(rsi) -> ?REG_BIT_RSI;
reg_bit(rdi) -> ?REG_BIT_RDI;
reg_bit(r8) -> ?REG_BIT_R8;
reg_bit(r9) -> ?REG_BIT_R9;
reg_bit(r10) -> ?REG_BIT_R10;
reg_bit(r11) -> ?REG_BIT_R11;
reg_bit(rbx) -> ?REG_BIT_RBX;
reg_bit(r12) -> ?REG_BIT_R12;
reg_bit(r13) -> ?REG_BIT_R13;
reg_bit(r14) -> ?REG_BIT_R14;
reg_bit(r15) -> ?REG_BIT_R15.

%%-----------------------------------------------------------------------------
%% @doc Add a label at the current offset
%% @end
%% @param State current backend state
%% @param Label the label number or reference
%% @return Updated backend state
%%-----------------------------------------------------------------------------
-spec add_label(state(), integer() | reference()) -> state().
add_label(StateP, Label) ->
    %% Unknown predecessors may join here: pending stores not in the label's
    %% live-in mask are dead and get nop'd; those in the mask keep their
    %% store. The pending window ends either way.
    #state{stream_module = StreamModule, stream = Stream, regs = Regs0} =
        State = pending_flush_label(StateP, Label),
    Offset = StreamModule:offset(Stream),
    Regs1 = jit_regs:invalidate_all(Regs0),
    add_label(State#state{regs = Regs1}, Label, Offset).

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
    % Each jmp_rel32 instruction is 5 bytes
    JumpTableEntryOffset = JumpTableStart + Label * 5,
    RelativeOffset = LabelOffset - JumpTableEntryOffset,
    {_RelocOffset, JmpInstruction} = jit_x86_64_asm:jmp_rel32(RelativeOffset),
    Stream1 = StreamModule:replace(Stream0, JumpTableEntryOffset, JmpInstruction),

    % Eagerly patch any branches targeting this label
    {Stream2, RemainingBranches} = patch_branches_for_label(
        StreamModule,
        Stream1,
        Label,
        LabelOffset,
        Branches
    ),

    State#state{
        stream = Stream2,
        branches = RemainingBranches,
        labels = Labels#{Label => LabelOffset},
        regs = jit_regs:invalidate_all(State#state.regs)
    };
add_label(#state{labels = Labels, regs = Regs0} = State, Label, Offset) ->
    State#state{labels = Labels#{Label => Offset}, regs = jit_regs:invalidate_all(Regs0)}.

%% @doc Byte offset of the `x' register array within the Context struct.
%% Derived from ?X_REG so it tracks the codegen offset.
-spec dwarf_x_reg_offset() -> non_neg_integer().
dwarf_x_reg_offset() ->
    element(1, ?X_REG(0)).

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
%% @returns The DWARF register number of the pinned ctx register
%% @end
%%-----------------------------------------------------------------------------
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    dwarf_register_number(?CTX_REG).

-spec dwarf_register_number(atom()) -> non_neg_integer().
dwarf_register_number(rax) -> 0;
dwarf_register_number(rdx) -> 1;
dwarf_register_number(rcx) -> 2;
dwarf_register_number(rsi) -> 4;
dwarf_register_number(rdi) -> 5;
dwarf_register_number(r8) -> 8;
dwarf_register_number(r9) -> 9;
dwarf_register_number(r10) -> 10;
dwarf_register_number(r11) -> 11;
dwarf_register_number(rbx) -> 3;
dwarf_register_number(r12) -> 12;
dwarf_register_number(r13) -> 13;
dwarf_register_number(r14) -> 14;
dwarf_register_number(r15) -> 15.
-endif.
