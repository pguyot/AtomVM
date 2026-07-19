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

-module(jit_riscv64).

-export([
    word_size/0,
    new/3,
    set_live_masks/2,
    supports_loop_residency/0,
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
    jump_to_label_cond/3,
    set_branch_hints/2,
    take_overflows/1,
    rewind_stream/2,
    enable_eager_flush/1,
    labels/1,
    set_preset_labels/2,
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
    get_module_atom_index/2,
    and_/3,
    or_/3,
    add/3,
    add_overflow_check/3,
    sub/3,
    sub_overflow_check/3,
    mul/3,
    mul_overflow_check/3,
    div_/3,
    rem_/3,
    supports_div/1,
    supports_fp/1,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3,
    xor_/3,
    shift_right_arith/3,
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

-compile([warnings_as_errors, nowarn_unused_type]).

-include_lib("jit.hrl").

-include("primitives.hrl").
-include("term.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

-define(ASSERT(Expr), true = Expr).

%% RISC-V64 LP64 ABI: a0-a7 are used for argument passing (8 registers).
%% a0-a1 are used for return values.
%% s0-s11 are callee-saved registers (must be preserved across calls).
%% t0-t6 are caller-saved temporary registers.
%% sp is the stack pointer.
%% ra is the return address register.
%% zero (x0) is hardwired to constant 0.
%% This implementation uses RV64IMAC (base + multiply/compressed extensions).
%%
%% See: RISC-V Calling Convention
%% https://riscv.org/wp-content/uploads/2024/12/riscv-calling.pdf
%%
%% Registers used by the JIT backend (RISC-V64):
%%   - Argument/return: a0-a7 (up to 8 args in registers)
%%   - Callee-saved: s0-s11 (must preserve)
%%   - Temporaries: t0-t6 (caller-saved)
%%   - Stack pointer: sp
%%   - Return address: ra
%%   - Zero register: zero (always 0)
%%   - Available for JIT scratch: t0-t6 (7 temp registers)
%%
%% Note: Base RISC-V64 instructions are 32-bit with uniform encoding,
%% allowing access to all 32 registers. With the C extension, many common
%% instructions have 16-bit compressed forms. All registers are 64 bits wide.
%%
%% For more details, refer to the RISC-V LP64 Procedure Call Standard.

-type riscv64_register() ::
    a0
    | a1
    | a2
    | a3
    | a4
    | a5
    | a6
    | a7
    | t0
    | t1
    | t2
    | t3
    | t4
    | t5
    | t6
    | s0
    | s1
    | s2
    | s3
    | s4
    | s5
    | s6
    | s7
    | s8
    | s9
    | s10
    | s11
    | sp
    | ra.

-define(IS_GPR(Reg),
    (Reg =:= a0 orelse Reg =:= a1 orelse Reg =:= a2 orelse Reg =:= a3 orelse Reg =:= a4 orelse
        Reg =:= a5 orelse Reg =:= a6 orelse Reg =:= a7 orelse Reg =:= t0 orelse Reg =:= t1 orelse
        Reg =:= t2 orelse Reg =:= t3 orelse Reg =:= t4 orelse Reg =:= t5 orelse Reg =:= t6 orelse
        Reg =:= s0 orelse Reg =:= s1 orelse Reg =:= s2 orelse Reg =:= s3 orelse Reg =:= s4 orelse
        Reg =:= s5 orelse Reg =:= s6 orelse Reg =:= s7 orelse Reg =:= s8 orelse Reg =:= s9 orelse
        Reg =:= s10 orelse Reg =:= s11 orelse Reg =:= sp orelse Reg =:= ra)
).

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: #{integer() | reference() => [{non_neg_integer(), tuple()}]},
    jump_table_start :: non_neg_integer(),
    labels :: #{integer() | reference() => integer()},
    variant :: non_neg_integer(),
    %% Register value tracking and native-register mask bookkeeping
    regs :: jit_regs:regs(),
    %% Write-through x-store elision (jit_backend_pending_impl.hrl): per-label
    %% live-in masks (jit_liveness pass A), pending stores
    %% (x index -> {stream offset, store width, cond depth}) and the current
    %% conditional-emission depth.
    live_masks = undefined :: undefined | #{non_neg_integer() => non_neg_integer()},
    pending_x = #{} ::
        #{non_neg_integer() => {non_neg_integer(), non_neg_integer(), non_neg_integer()}},
    cond_depth = 0 :: non_neg_integer(),
    %% Buffered-stream backtrack (see jump_to_label_cond forward path): forward
    %% fused guard branches are emitted optimistically at a size taken from
    %% branch_hints (default 4), resolved at finalize, and any that overflow
    %% their reservation are reported in overflows so jit:compile can re-emit
    %% them pinned larger. branch_counter gives each a stable id across re-emits.
    branch_hints = #{} :: #{non_neg_integer() => pos_integer()},
    branch_counter = 0 :: non_neg_integer(),
    fused_branches = [] :: [tuple()],
    overflows = #{} :: #{non_neg_integer() => pos_integer()},
    eager_flush = false :: boolean(),
    preset_labels = undefined :: undefined | #{integer() => non_neg_integer()}
}).

-type state() :: #state{}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer() | extra}
    | {y_reg, non_neg_integer()}
    | {fp_reg, non_neg_integer()}
    | {ptr, riscv64_register()}.
-type native_register() :: riscv64_register().
-type value() :: immediate() | vm_register() | riscv64_register() | {ptr, riscv64_register()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_riscv64_register() ::
    {free, riscv64_register()} | riscv64_register().

-type condition() ::
    {riscv64_register(), '<', integer()}
    | {maybe_free_riscv64_register(), '<', riscv64_register()}
    | {maybe_free_riscv64_register(), '<u', riscv64_register()}
    | {integer(), '<', maybe_free_riscv64_register()}
    | {maybe_free_riscv64_register(), '==', riscv64_register() | integer()}
    | {maybe_free_riscv64_register(), '!=', riscv64_register() | integer()}
    | {'(int)', maybe_free_riscv64_register(), '==', integer()}
    | {'(int)', maybe_free_riscv64_register(), '!=', riscv64_register() | integer()}
    | {'(bool)', maybe_free_riscv64_register(), '==', false}
    | {'(bool)', maybe_free_riscv64_register(), '!=', false}
    | {maybe_free_riscv64_register(), '&', non_neg_integer(), '!=', integer()}
    | {{free, riscv64_register()}, '==', {free, riscv64_register()}}.

% Context offsets (64-bit architecture)
% ctx->e is 0x50
% ctx->x is 0x58
%% Pinned-register convention: ctx, jit_state, the primitives table and
%% ctx->e live in callee-saved registers, seeded once per C->native crossing
%% by the dispatch loop (opcodesswitch.h). C primitives preserve them per
%% the RISC-V ABI, so generated code never saves, restores or reloads them
%% around calls; a0/a1/a2 become scratch. ctx takes s1 (x9), the only
%% RVC-addressable callee-saved base besides the frame pointer, so argument
%% loads from x registers into a0-a5 keep their compressed encodings. There
%% are no inline heap operations on RISC-V, so hp is NOT pinned.
-define(CTX_REG, s1).
-define(NATIVE_INTERFACE_REG, s3).
%% ctx->e mutates (allocate/deallocate, GC): written back to ctx before
%% every C call and reloaded after calls that return, except around
%% primitives listed in jit_prim_pure.hrl.
-define(E_REG, s4).
-define(Y_REGS, {?CTX_REG, 16#50}).
-define(X_REG(N), {?CTX_REG, 16#58 + (N * 8)}).
-define(CP, {?CTX_REG, 16#E0}).
-define(FP_REGS, {?JITSTATE_REG, 16#18}).
-define(BS, {?CTX_REG, 16#E8}).
-define(BS_OFFSET, {?CTX_REG, 16#F0}).
% JITSTATE is in a1 register (no prolog needed)
-define(JITSTATE_REG, s2).
% Return address register
-define(RA_REG, ra).
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#8).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#10).
-define(PRIMITIVE(N), {?NATIVE_INTERFACE_REG, N * 8}).
-define(MODULE_INDEX(ModuleReg), {ModuleReg, 0}).
-define(MODULE_LOCAL_ATOMS_TABLE_OFFSET, 16#D8).

-define(JUMP_TABLE_ENTRY_SIZE, 8).

%% RISC-V64 register mappings

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).
-define(IS_SINT64_T(X),
    is_integer(X) andalso X >= -16#8000000000000000 andalso X < 16#8000000000000000
).
-define(IS_UINT64_T(X), is_integer(X) andalso X >= 0 andalso X < 16#10000000000000000).
-define(IS_SIGNED_OR_UNSIGNED_INT64_T(X),
    is_integer(X) andalso X >= -16#8000000000000000 andalso X < 16#10000000000000000
).

%% RISC-V64 LP64 ABI register allocation:
%% - a0: context pointer (reserved)
%% - a1: jit_state pointer (reserved)
%% - a2: native interface pointer (reserved)
%% - a3-a7: available for parameters to native functions
%% - t0-t6: temporaries, caller-saved, available for JIT scratch
%% - s0-s11: callee-saved (would need to be saved/restored)
%% PARAMETER_REGS includes a0-a7 because the calling convention places
%% ctx, jit_state, and native_interface in a0-a2 respectively.
-define(PARAMETER_REGS, [a0, a1, a2, a3, a4, a5, a6, a7]).

-define(REG_BIT_A0, (1 bsl 0)).
-define(REG_BIT_A1, (1 bsl 1)).
-define(REG_BIT_A2, (1 bsl 2)).
-define(REG_BIT_A3, (1 bsl 3)).
-define(REG_BIT_A4, (1 bsl 4)).
-define(REG_BIT_A5, (1 bsl 5)).
-define(REG_BIT_A6, (1 bsl 6)).
-define(REG_BIT_A7, (1 bsl 7)).
-define(REG_BIT_T0, (1 bsl 8)).
-define(REG_BIT_T1, (1 bsl 9)).
-define(REG_BIT_T2, (1 bsl 10)).
-define(REG_BIT_T3, (1 bsl 11)).
-define(REG_BIT_T4, (1 bsl 12)).
-define(REG_BIT_T5, (1 bsl 13)).
-define(REG_BIT_T6, (1 bsl 14)).
%% Callee-saved pinned registers: never in the available/used masks; the bits
%% exist so args_regs/regs_to_mask can pass over them.
-define(REG_BIT_S1, (1 bsl 15)).
-define(REG_BIT_S2, (1 bsl 16)).
-define(REG_BIT_S3, (1 bsl 17)).
-define(REG_BIT_S4, (1 bsl 18)).

%% AVAILABLE_REGS = [t6, t5, t4, t3, t2, t1, t0]
-define(AVAILABLE_REGS_MASK,
    (?REG_BIT_T6 bor ?REG_BIT_T5 bor ?REG_BIT_T4 bor ?REG_BIT_T3 bor
        ?REG_BIT_T2 bor ?REG_BIT_T1 bor ?REG_BIT_T0 bor
        ?REG_BIT_A2 bor ?REG_BIT_A1 bor ?REG_BIT_A0)
).
-include("jit_backend_dwarf_impl.hrl").
-define(ASM, jit_riscv64_asm).
-define(WORD_SIZE_BYTES, 8).
-define(BOOL_SHIFT, 63).
-define(Y_OFFSET_LIMIT, 248).
-define(PRIMITIVE_DIRECT_LOAD_LIMIT, -1).
-define(FLOAT_DATA_OFFSET, 8).
-define(IS_SIGNED_OR_UNSIGNED_WORD(X),
    is_integer(X) andalso X >= -16#8000000000000000 andalso X < 16#10000000000000000
).
-define(LOAD_WORD(Dst, Base, Off), jit_riscv64_asm:ld(Dst, Base, Off)).
-define(STORE_WORD(Base, Src, Off), jit_riscv64_asm:sd(Base, Src, Off)).
-define(DWARF_CTX_REG, ?DWARF_A0_REG_RISCV).
-define(ARRAY_OFFSET_FOLD_GUARD(Offset), is_integer(Offset)).
-define(ARRAY_OFFSET_FOLD(IndexReg, Offset), IndexReg + Offset).

-include("jit_riscv_impl.hrl").
-include("jit_prim_pure.hrl").

%% Native-register allocation bookkeeping (used_regs/1, available_regs/1,
%% free_native_registers/2, free_native_register/2, assert_all_native_free/1)
%% is shared across the register-based backends and flows through jit_regs.
%% Must be included after jit_riscv_impl.hrl, on whose register primitives it
%% depends.
-include("jit_backend_regs_impl.hrl").

-spec word_size() -> 4 | 8.
word_size() -> 8.

div_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I = jit_riscv64_asm:div_(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

rem_(
    #state{stream_module = StreamModule, stream = Stream0, regs = Regs0} = State,
    DividendReg,
    DivisorReg
) ->
    I = jit_riscv64_asm:rem_(DividendReg, DividendReg, DivisorReg),
    Stream1 = StreamModule:append(Stream0, I),
    Regs1 = jit_regs:invalidate_reg(Regs0, DividendReg),
    {State#state{stream = Stream1, regs = Regs1}, DividendReg}.

%% riscv64 (with the M extension assumed) always supports native div.
-spec supports_div(state()) -> boolean().
supports_div(_State) -> true.

%% Whether this backend can emit inline floating-point arithmetic instead of
%% calling the float primitives. False until the inline fp ops are implemented.
-spec supports_fp(state()) -> boolean().
supports_fp(_State) -> false.

%% @doc Load the 32-bit global atom index for a module-local atom id, i.e.
%% jit_state->module->local_atoms_to_global_table[AtomIndex], into a fresh
%% register. The shared jit caller applies the term tag; this only loads the raw
%% 32-bit index. Inlining these loads avoids the primitive-call overhead per
%% access (this is hot: every non-default atom literal access).
-spec get_module_atom_index(state(), non_neg_integer()) -> {state(), riscv64_register()}.
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
    RegBit = reg_bit(Reg),
    %% Reg = jit_state->module (64-bit load, jit_state is in a1)
    I1 = ?LOAD_WORD(Reg, ?JITSTATE_REG, ?JITSTATE_MODULE_OFFSET),
    %% Reg = module->local_atoms_to_global_table (64-bit load)
    I2 = jit_riscv64_asm:ld(Reg, Reg, ?MODULE_LOCAL_ATOMS_TABLE_OFFSET),
    %% Reg = local_atoms_to_global_table[AtomIndex] (uint32_t[], 4 bytes wide).
    %% Use lwu to zero-extend the 32-bit index into the 64-bit register (lw would
    %% sign-extend). RISC-V load offsets are 12-bit signed (-2048..2047), so for
    %% AtomIndex * 4 > 2047 the byte offset is added to the base first (the offset
    %% can exceed the 12-bit addi range, so it is materialized in a scratch
    %% register via li, then added).
    Offset = AtomIndex * 4,
    {LoadGid, Regs1} =
        case Offset =< 2047 of
            true ->
                {jit_riscv64_asm:lwu(Reg, Reg, Offset), Regs0};
            false ->
                %% Reserve a scratch register to hold the byte offset.
                AvailScratch = jit_regs:available_regs(jit_regs:alloc_reg(Regs0, RegBit)),
                Temp = first_avail(AvailScratch),
                LoadCode =
                    <<
                        (jit_riscv64_asm:li(Temp, Offset))/binary,
                        (jit_riscv64_asm:add(Reg, Reg, Temp))/binary,
                        (jit_riscv64_asm:lwu(Reg, Reg, 0))/binary
                    >>,
                %% Temp is consumed within this sequence; keep Regs unchanged so
                %% it stays available for later use.
                {LoadCode, Regs0}
        end,
    Code = <<I1/binary, I2/binary, LoadGid/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    Regs2 = jit_regs:set_contents(Regs1, Reg, {atom_index, AtomIndex}),
    {
        State#state{
            stream = Stream1,
            regs = jit_regs:alloc_reg(Regs2, RegBit)
        },
        Reg
    }.

% LP64: all arguments (including 64-bit) fit in a single register
parameter_regs0_avm_int64_t(T, [Reg | Rest], Acc) ->
    parameter_regs0(T, Rest, [Reg | Acc]).

% LP64: 64-bit values fit in a single register
handle_avm_int64_t(State, Value, ArgsT, ArgsRegs, ParamRegs, AvailGP, StackOffset) ->
    set_registers_args0(
        State, [Value | ArgsT], ArgsRegs, ParamRegs, AvailGP, StackOffset
    ).

%% @doc Byte offset of the `x' register array within the Context struct.
%% Derived from ?X_REG so it tracks the codegen offset.
-spec dwarf_x_reg_offset() -> non_neg_integer().
dwarf_x_reg_offset() ->
    element(2, ?X_REG(0)).

-ifdef(JIT_DWARF).
-spec dwarf_ctx_register() -> non_neg_integer().
dwarf_ctx_register() ->
    ?DWARF_A0_REG_RISCV.

-spec dwarf_register_number(atom()) -> non_neg_integer().
dwarf_register_number(Reg) -> jit_riscv32:dwarf_register_number(Reg).
-endif.
