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

-module(jit_x86_64_asm).

-export([
    movq/2,
    movabsq/2,
    movl/2,
    movzbq/2,
    movzwq/2,
    bswapl/1,
    movb_store/2,
    movw_store/2,
    movl_store/2,
    rolw/2,
    shlq/2,
    shrq/2,
    testb/2,
    testl/2,
    testq/2,
    jz/1,
    jz_rel8/1,
    jz_rel32/1,
    jnz/1,
    jnz_rel8/1,
    jnz_rel32/1,
    jno/1,
    jno_rel8/1,
    jno_rel32/1,
    jge/1,
    jge_rel8/1,
    jb/1,
    jb_rel8/1,
    jae/1,
    jae_rel8/1,
    jle/1,
    jle_rel8/1,
    jbe/1,
    jbe_rel8/1,
    jbe_rel32/1,
    jmp/1,
    jmp_rel8/1,
    jmp_rel32/1,
    andq/2,
    andl/2,
    andb/2,
    cmpl/2,
    cmpq/2,
    addq/2,
    subq/2,
    imulq/2,
    decl/1,
    orq/2,
    orq_rel32/2,
    leaq/2,
    leaq_rel32/2,
    callq/1,
    pushq/1,
    popq/1,
    jmpq/1,
    retq/0,
    nop/1,
    cmpb/2,
    xchgq/2,
    xorl/2,
    xorq/2,
    cqo/0,
    idivq/1,
    sarq/2,
    movsd/2,
    addsd/2,
    subsd/2,
    mulsd/2,
    divsd/2,
    movsd_to_gpr/2,
    cvtsi2sd/2,
    setne/1
]).

-define(IS_SINT8_T(X), is_integer(X) andalso X >= -128 andalso X =< 127).
-define(IS_SINT32_T(X), is_integer(X) andalso X >= -16#80000000 andalso X < 16#80000000).
-define(IS_UINT8_T(X), is_integer(X) andalso X >= 0 andalso X =< 255).
-define(IS_UINT32_T(X), is_integer(X) andalso X >= 0 andalso X < 16#100000000).

-type x86_64_register() :: rax | rcx | rdx | rsi | rdi | r8 | r9 | r10 | r11.

% Encode a register on 4 bits
% https://wiki.osdev.org/X86-64_Instruction_Encoding#Registers
-spec x86_64_x_reg(x86_64_register()) -> {0..1, 0..7}.
x86_64_x_reg(rax) -> {0, 0};
x86_64_x_reg(rcx) -> {0, 1};
x86_64_x_reg(rdx) -> {0, 2};
x86_64_x_reg(rsi) -> {0, 6};
x86_64_x_reg(rdi) -> {0, 7};
x86_64_x_reg(r8) -> {1, 0};
x86_64_x_reg(r9) -> {1, 1};
x86_64_x_reg(r10) -> {1, 2};
x86_64_x_reg(r11) -> {1, 3}.

-define(X86_64_REX(W, R, X, B), <<4:4, W:1, R:1, X:1, B:1>> / binary).

% Encode an SSE register on 4 bits (high bit goes to REX.R/REX.B)
-spec x86_64_xmm_reg(atom()) -> {0..1, 0..7}.
x86_64_xmm_reg(xmm0) -> {0, 0};
x86_64_xmm_reg(xmm1) -> {0, 1};
x86_64_xmm_reg(xmm2) -> {0, 2};
x86_64_xmm_reg(xmm3) -> {0, 3};
x86_64_xmm_reg(xmm4) -> {0, 4};
x86_64_xmm_reg(xmm5) -> {0, 5};
x86_64_xmm_reg(xmm6) -> {0, 6};
x86_64_xmm_reg(xmm7) -> {0, 7};
x86_64_xmm_reg(xmm8) -> {1, 0};
x86_64_xmm_reg(xmm9) -> {1, 1};
x86_64_xmm_reg(xmm10) -> {1, 2};
x86_64_xmm_reg(xmm11) -> {1, 3};
x86_64_xmm_reg(xmm12) -> {1, 4};
x86_64_xmm_reg(xmm13) -> {1, 5};
x86_64_xmm_reg(xmm14) -> {1, 6};
x86_64_xmm_reg(xmm15) -> {1, 7}.

% REX prefix, emitted only when one of its bits is set (an all-zero REX would be
% a harmless but non-canonical 0x40 byte).
rex_opt(0, 0, 0, 0) -> <<>>;
rex_opt(W, R, X, B) -> <<4:4, W:1, R:1, X:1, B:1>>.

% ModRM (+ SIB) + displacement for the memory operand [Base + Disp], valid for
% every base register: rm=100 (rsp/r12) needs a SIB byte (index=none), and
% rm=101 (rbp/r13) at mod=00 would mean RIP-relative, so those bases always
% carry at least a disp8.
modrm_mem(RegField, BaseRm, Disp) ->
    SIB =
        case BaseRm of
            4 -> <<16#24>>;
            _ -> <<>>
        end,
    if
        Disp =:= 0 andalso BaseRm =/= 5 ->
            <<0:2, RegField:3, BaseRm:3, SIB/binary>>;
        ?IS_SINT8_T(Disp) ->
            <<1:2, RegField:3, BaseRm:3, SIB/binary, Disp>>;
        true ->
            <<2:2, RegField:3, BaseRm:3, SIB/binary, Disp:32/little>>
    end.

% ModRM + displacement for the memory operand [Base + Disp]. The x86_64 backend
% only ever uses rax/rcx/rdx/rsi/rdi/r8..r11 as a base, none of which is rsp/r12
% (SIB) or rbp/r13 (rm 5 / RIP-relative), so the simple form is always valid.
sse_modrm_mem(RegField, BaseRm, 0) ->
    <<(modrm_mem(RegField, BaseRm, 0))/binary>>;
sse_modrm_mem(RegField, BaseRm, Disp) when ?IS_SINT8_T(Disp) ->
    <<(modrm_mem(RegField, BaseRm, Disp))/binary>>;
sse_modrm_mem(RegField, BaseRm, Disp) when ?IS_SINT32_T(Disp) ->
    <<(modrm_mem(RegField, BaseRm, Disp))/binary>>.

% movsd xmm, [Base+Disp]  (F2 0F 10 /r): load a double into an xmm register.
movsd(XmmDst, {Disp, Base}) when is_atom(XmmDst), is_atom(Base) ->
    {REX_R, MODRM_REG} = x86_64_xmm_reg(XmmDst),
    {REX_B, MODRM_RM} = x86_64_x_reg(Base),
    <<16#F2, (rex_opt(0, REX_R, 0, REX_B))/binary, 16#0F, 16#10,
        (sse_modrm_mem(MODRM_REG, MODRM_RM, Disp))/binary>>;
% movsd [Base+Disp], xmm  (F2 0F 11 /r): store an xmm register to a double.
movsd({Disp, Base}, XmmSrc) when is_atom(Base), is_atom(XmmSrc) ->
    {REX_R, MODRM_REG} = x86_64_xmm_reg(XmmSrc),
    {REX_B, MODRM_RM} = x86_64_x_reg(Base),
    <<16#F2, (rex_opt(0, REX_R, 0, REX_B))/binary, 16#0F, 16#11,
        (sse_modrm_mem(MODRM_REG, MODRM_RM, Disp))/binary>>.

% Scalar double arithmetic, xmm-to-xmm: XmmDst = XmmDst <op> XmmSrc.
addsd(XmmDst, XmmSrc) -> sse_arith(16#58, XmmDst, XmmSrc).
mulsd(XmmDst, XmmSrc) -> sse_arith(16#59, XmmDst, XmmSrc).
subsd(XmmDst, XmmSrc) -> sse_arith(16#5C, XmmDst, XmmSrc).
divsd(XmmDst, XmmSrc) -> sse_arith(16#5E, XmmDst, XmmSrc).

sse_arith(Opcode, XmmDst, XmmSrc) when is_atom(XmmDst), is_atom(XmmSrc) ->
    {REX_R, MODRM_REG} = x86_64_xmm_reg(XmmDst),
    {REX_B, MODRM_RM} = x86_64_xmm_reg(XmmSrc),
    <<16#F2, (rex_opt(0, REX_R, 0, REX_B))/binary, 16#0F, Opcode, 3:2, MODRM_REG:3, MODRM_RM:3>>.

% movq GprDst, XmmSrc  (66 REX.W 0F 7E /r): move the raw bits of a double into a
% general-purpose register, e.g. to inspect the result for finiteness.
movsd_to_gpr(GprDst, XmmSrc) when is_atom(GprDst), is_atom(XmmSrc) ->
    {REX_R, MODRM_REG} = x86_64_xmm_reg(XmmSrc),
    {REX_B, MODRM_RM} = x86_64_x_reg(GprDst),
    <<16#66, (rex_opt(1, REX_R, 0, REX_B))/binary, 16#0F, 16#7E, 3:2, MODRM_REG:3, MODRM_RM:3>>.

% cvtsi2sd xmm, r64  (F2 REX.W 0F 2A /r): convert a signed 64-bit integer in a
% general-purpose register to a double in an xmm register.
cvtsi2sd(XmmDst, GprSrc) when is_atom(XmmDst), is_atom(GprSrc) ->
    {REX_R, MODRM_REG} = x86_64_xmm_reg(XmmDst),
    {REX_B, MODRM_RM} = x86_64_x_reg(GprSrc),
    <<16#F2, (rex_opt(1, REX_R, 0, REX_B))/binary, 16#0F, 16#2A, 3:2, MODRM_REG:3, MODRM_RM:3>>.

% setne r/m8  (0F 95 /0): set the low byte of Reg to 1 if ZF is clear (last
% result was non-zero), else 0.
setne(Reg) when is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    %% rsi/rdi (low3 >= 4) and r8..r11 (REX.B) need a REX prefix to address the
    %% low byte (sil/dil/r8b...) rather than the legacy ah/ch/dh/bh.
    RexPrefix =
        case REX_B =:= 1 orelse MODRM_RM >= 4 of
            true -> <<4:4, 0:1, 0:1, 0:1, REX_B:1>>;
            false -> <<>>
        end,
    <<RexPrefix/binary, 16#0F, 16#95, 3:2, 0:3, MODRM_RM:3>>.

movq({0, SrcReg}, DestReg) when is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8B, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>;
movq({Offset, SrcReg}, DestReg) when is_atom(DestReg) andalso ?IS_SINT8_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp8
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8B, (modrm_mem(MODRM_REG, MODRM_RM, Offset))/binary>>;
movq({Offset, SrcReg}, DestReg) when is_atom(DestReg) andalso ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp32
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8B, (modrm_mem(MODRM_REG, MODRM_RM, Offset))/binary>>;
movq(DestReg, {0, SrcReg}) when is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>;
movq(DestReg, {Offset, SrcReg}) when is_atom(DestReg) andalso ?IS_SINT8_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp8
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, (modrm_mem(MODRM_REG, MODRM_RM, Offset))/binary>>;
movq(DestReg, {Offset, SrcReg}) when is_atom(DestReg) andalso ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    % disp32
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, (modrm_mem(MODRM_REG, MODRM_RM, Offset))/binary>>;
movq(SrcReg, DestReg) when is_atom(SrcReg) andalso is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#89, 3:2, MODRM_REG:3, MODRM_RM:3>>;
movq(Imm, DestReg) when is_integer(Imm) andalso is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#c7, 3:2, 0:3, MODRM_RM:3, Imm:32/little>>;
movq(Imm, {Offset, DestReg}) when is_integer(Imm) andalso ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#c7, (modrm_mem(0, MODRM_RM, Offset))/binary, Imm:32/little>>;
movq(Imm, {Offset, DestReg}) when is_integer(Imm) andalso ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#c7, (modrm_mem(0, MODRM_RM, Offset))/binary, Imm:32/little>>;
% movq reg, {0, base, index, scale} - SIB with no displacement
movq(RegA, {0, RegB, RegC, Scale}) when
    is_atom(RegA),
    is_atom(RegB),
    is_atom(RegC),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8)
->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_BASE} = x86_64_x_reg(RegB),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(RegC),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB, mod=00 for no displacement
    <<
        ?X86_64_REX(1, REX_R, REX_X, REX_B),
        16#89,
        0:2,
        MODRM_REG:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3
    >>;
movq(RegA, {Offset, RegB, RegC, Scale}) when
    is_atom(RegA),
    is_atom(RegB),
    is_atom(RegC),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT8_T(Offset),
    Offset =/= 0
->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_BASE} = x86_64_x_reg(RegB),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(RegC),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB
    <<
        ?X86_64_REX(1, REX_R, REX_X, REX_B),
        16#89,
        1:2,
        MODRM_REG:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset
    >>;
movq(RegA, {Offset, RegB, RegC, Scale}) when
    is_atom(RegA),
    is_atom(RegB),
    is_atom(RegC),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT32_T(Offset)
->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_BASE} = x86_64_x_reg(RegB),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(RegC),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB
    <<
        ?X86_64_REX(1, REX_R, REX_X, REX_B),
        16#89,
        2:2,
        MODRM_REG:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset:32/little
    >>;
movq(Imm, {Offset, Base, Index, Scale}) when
    is_integer(Imm),
    is_atom(Base),
    is_atom(Index),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT8_T(Offset),
    Offset =/= 0
->
    {REX_B, MODRM_BASE} = x86_64_x_reg(Base),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(Index),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB, mod=01 for disp8
    <<
        ?X86_64_REX(1, 0, REX_X, REX_B),
        16#c7,
        1:2,
        0:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset,
        Imm:32/little
    >>;
movq(Imm, {Offset, Base, Index, Scale}) when
    is_integer(Imm),
    is_atom(Base),
    is_atom(Index),
    (Scale == 1 orelse Scale == 2 orelse Scale == 4 orelse Scale == 8),
    ?IS_SINT32_T(Offset)
->
    {REX_B, MODRM_BASE} = x86_64_x_reg(Base),
    {REX_X, MODRM_INDEX} = x86_64_x_reg(Index),
    ScaleBits =
        case Scale of
            1 -> 0;
            2 -> 1;
            4 -> 2;
            8 -> 3
        end,
    % rm=100 for SIB
    <<
        ?X86_64_REX(1, 0, REX_X, REX_B),
        16#c7,
        2:2,
        0:3,
        4:3,
        ScaleBits:2,
        MODRM_INDEX:3,
        MODRM_BASE:3,
        Offset:32/little,
        Imm:32/little
    >>.

movabsq(Imm, Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, (16#B8 + Index), Imm:64/little>>;
        {1, Index} -> <<16#49, (16#B8 + Index), Imm:64/little>>
    end.

movl(Imm, DestReg) when is_integer(Imm), is_atom(DestReg) ->
    case x86_64_x_reg(DestReg) of
        {0, Index} -> <<(16#B8 + Index), Imm:32/little>>;
        {1, Index} -> <<16#41, (16#B8 + Index), Imm:32/little>>
    end;
movl({0, SrcReg}, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    (case {REX_R, REX_B} of
        {0, 0} -> <<16#8B, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>;
        _ -> <<?X86_64_REX(0, REX_R, 0, REX_B), 16#8B, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>
    end).

% movzx byte ptr [SrcReg], DestReg (zero-extended to 64 bits)
movzbq({0, SrcReg}, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#0F, 16#B6, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>.

% movzx word ptr [SrcReg], DestReg (zero-extended to 64 bits)
movzwq({0, SrcReg}, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#0F, 16#B7, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>.

% bswap on the 32-bit register (upper 32 bits are zeroed)
bswapl(Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#0F, (16#C8 + Index)>>;
        {1, Index} -> <<16#41, 16#0F, (16#C8 + Index)>>
    end.

% movb SrcReg(low byte), [AddrReg]: store the low byte of SrcReg to memory.
movb_store(SrcReg, {0, AddrReg}) when is_atom(SrcReg), is_atom(AddrReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(AddrReg),
    case {REX_R, REX_B} of
        {0, 0} -> <<16#88, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>;
        _ -> <<?X86_64_REX(0, REX_R, 0, REX_B), 16#88, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>
    end.

% movw SrcReg(low word), [AddrReg]: store the low 16 bits of SrcReg to memory.
movw_store(SrcReg, {0, AddrReg}) when is_atom(SrcReg), is_atom(AddrReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(AddrReg),
    case {REX_R, REX_B} of
        {0, 0} ->
            <<16#66, 16#89, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>;
        _ ->
            <<16#66, ?X86_64_REX(0, REX_R, 0, REX_B), 16#89,
                (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>
    end.

% movl SrcReg(low dword), [AddrReg]: store the low 32 bits of SrcReg to memory.
movl_store(SrcReg, {0, AddrReg}) when is_atom(SrcReg), is_atom(AddrReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(AddrReg),
    case {REX_R, REX_B} of
        {0, 0} -> <<16#89, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>;
        _ -> <<?X86_64_REX(0, REX_R, 0, REX_B), 16#89, (modrm_mem(MODRM_REG, MODRM_RM, 0))/binary>>
    end.

% rolw $Imm, Reg: rotate the low 16 bits of Reg left by Imm (used to byte-swap
% a 16-bit value before a movw store).
rolw(Imm, Reg) when ?IS_UINT8_T(Imm), is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#66, 16#C1, (16#C0 + Index), Imm>>;
        {1, Index} -> <<16#66, 16#41, 16#C1, (16#C0 + Index), Imm>>
    end.

shlq(Imm, Reg) when ?IS_UINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#C1, (16#E0 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#C1, (16#E0 + Index), Imm>>
    end.

shrq(Imm, Reg) when ?IS_UINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#C1, (16#E8 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#C1, (16#E8 + Index), Imm>>
    end.

testb(Reg, Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#84, (16#C0 bor (Index bsl 3) bor Index)>>;
        {1, Index} -> <<16#45, 16#84, (16#C0 bor (Index bsl 3) bor Index)>>
    end;
testb(Imm, rax) when ?IS_UINT8_T(Imm); ?IS_SINT8_T(Imm) ->
    <<16#A8, Imm>>;
testb(Imm, Reg) when ?IS_UINT8_T(Imm), is_atom(Reg); ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % TEST r/m8, imm8: 0xF6 /0 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#F6, 3:2, 0:3, MODRM_RM:3, Imm>>.

testq(Reg, Reg) when is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#85, (16#C0 bor (Index bsl 3) bor Index)>>;
        {1, Index} -> <<16#4D, 16#85, (16#C0 bor (Index bsl 3) bor Index)>>
    end;
testq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % TEST rax, imm32: REX.W 0xA9 imm32
    <<16#48, 16#A9, Imm:32/little>>;
testq(Imm, Reg) when is_atom(Reg), Reg =/= rax, ?IS_SINT32_T(Imm) ->
    % TEST r/m64, imm32: REX.W 0xF7 /0 ModRM imm32
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#F7, 3:2, 0:3, MODRM_RM:3, Imm:32/little>>.

testl(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    Prefix =
        case {REX_R, REX_B} of
            {0, 0} -> <<>>;
            _ -> <<?X86_64_REX(0, REX_R, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#85, (16#C0 bor (MODRM_REG bsl 3) bor MODRM_RM)>>.

jz(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#74, AdjustedOffset>>.

jz_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jz(Offset)}.

% Jump if zero/equal (ZF=1) with a 32-bit displacement (0F 84, 6-byte
% instruction). Used to skip a block that may exceed the rel8 +127 range.
jz_rel32(Offset) when ?IS_SINT32_T(Offset) ->
    AdjustedOffset = Offset - 6,
    {2, <<16#0F, 16#84, AdjustedOffset:32/little>>}.

jnz(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#75, AdjustedOffset>>.

jnz_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jnz(Offset)}.

% Jump if not zero/not equal (ZF=0) with a 32-bit displacement (0F 85, 6-byte
% instruction). Used for conditional jumps to labels beyond the rel8 range.
jnz_rel32(Offset) when ?IS_SINT32_T(Offset) ->
    AdjustedOffset = Offset - 6,
    {2, <<16#0F, 16#85, AdjustedOffset:32/little>>}.

jno(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Jump if no overflow (OF=0); short jump
    AdjustedOffset = Offset - 2,
    <<16#71, AdjustedOffset>>.

jno_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jno(Offset)}.

% Jump if no overflow (OF=0) with a 32-bit displacement (0F 81, 6-byte
% instruction). Used to skip a block that may exceed the rel8 +127 range.
jno_rel32(Offset) when ?IS_SINT32_T(Offset) ->
    AdjustedOffset = Offset - 6,
    {2, <<16#0F, 16#81, AdjustedOffset:32/little>>}.

jge(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#7D, AdjustedOffset>>.

jge_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jge(Offset)}.

jb(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#72, AdjustedOffset>>.

jb_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jb(Offset)}.

jae(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#73, AdjustedOffset>>.

jae_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jae(Offset)}.

jle(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#7E, AdjustedOffset>>.

jle_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jle(Offset)}.

jbe(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Jump if below or equal (unsigned, CF=1 or ZF=1); short jump
    AdjustedOffset = Offset - 2,
    <<16#76, AdjustedOffset>>.

jbe_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    {1, jbe(Offset)}.

% Jump if below or equal (unsigned) with a 32-bit displacement (0F 86, 6-byte
% instruction). Used to skip a block that may exceed the rel8 +127 range.
jbe_rel32(Offset) when ?IS_SINT32_T(Offset) ->
    AdjustedOffset = Offset - 6,
    {2, <<16#0F, 16#86, AdjustedOffset:32/little>>}.

jmp(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    <<16#EB, AdjustedOffset>>;
jmp(Offset) when ?IS_SINT32_T(Offset) ->
    % Adjust for 5-byte near jump instruction size
    AdjustedOffset = Offset - 5,
    <<16#E9, AdjustedOffset:32/little>>.

jmp_rel8(Offset) when Offset >= -126 andalso Offset =< 129 ->
    % Use short jump (matches assembler behavior)
    AdjustedOffset = Offset - 2,
    {1, <<16#EB, AdjustedOffset>>}.

jmp_rel32(Offset) when ?IS_SINT32_T(Offset) ->
    % Adjust for 5-byte near jump instruction size
    AdjustedOffset = Offset - 5,
    {1, <<16#E9, AdjustedOffset:32/little>>}.

andq(Imm, DestReg) when ?IS_SINT8_T(Imm) andalso is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 3:2, 4:3, MODRM_RM:3, Imm>>;
andq(Imm, {Offset, DestReg}) when ?IS_SINT8_T(Imm) andalso ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, (modrm_mem(4, MODRM_RM, Offset))/binary, Imm>>;
andq(Imm, {Offset, DestReg}) when ?IS_SINT8_T(Imm) andalso ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, (modrm_mem(4, MODRM_RM, Offset))/binary, Imm>>;
andq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#21, 3:2, MODRM_REG:3, MODRM_RM:3>>.

andl(Imm, Reg) when is_integer(Imm), Imm >= 0, Imm =< 127, is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % AND r/m32, imm8: 0x83 /4 ModRM imm8 (REX prefix for r8-r15)
    % imm8 is sign-extended to 32 bits, so only values 0-127 are safe
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#83, 3:2, 4:3, MODRM_RM:3, Imm>>;
andl(Imm, rax) when ?IS_UINT32_T(Imm) ->
    % Special short encoding for AND EAX, imm32: 0x25 imm32
    <<16#25, Imm:32/little>>;
andl(Imm, Reg) when ?IS_UINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % AND r/m32, imm32: 0x81 /4 ModRM imm32 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#81, 3:2, 4:3, MODRM_RM:3, Imm:32/little>>.

andb(Imm, rax) when ?IS_UINT8_T(Imm) orelse ?IS_SINT8_T(Imm) ->
    <<16#24, Imm>>;
andb(Imm, Reg) when ?IS_UINT8_T(Imm) orelse ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % AND r/m8, imm8: 0x80 /4 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#80, 3:2, 4:3, MODRM_RM:3, Imm>>.

cmpb(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    Prefix =
        case {REX_R, REX_B} of
            {0, 0} -> <<>>;
            _ -> <<?X86_64_REX(0, REX_R, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#38, (16#C0 bor (MODRM_REG bsl 3) bor MODRM_RM)>>;
cmpb(Imm, Reg) when ?IS_UINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    % CMP r/m8, imm8: 0x80 /7 ModRM imm8 (REX prefix for r8-r15)
    Prefix =
        case REX_B of
            0 -> <<>>;
            1 -> <<?X86_64_REX(0, 0, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#80, 3:2, 7:3, MODRM_RM:3, Imm>>.

cmpl(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    case REX_B of
        % No REX needed for rax..rdi
        0 -> <<16#83, 3:2, 7:3, MODRM_RM:3, Imm>>;
        % REX.B needed for r8..r11
        1 -> <<16#41, 16#83, 3:2, 7:3, MODRM_RM:3, Imm>>
    end;
cmpl(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    % CMP r/m32, r32: 0x39 /r (32-bit form of cmpq/2: a REX prefix without
    % REX.W, emitted only when r8..r15 are involved)
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    Prefix =
        case {REX_R, REX_B} of
            {0, 0} -> <<>>;
            _ -> <<?X86_64_REX(0, REX_R, 0, REX_B)>>
        end,
    <<Prefix/binary, 16#39, 3:2, MODRM_REG:3, MODRM_RM:3>>.

cmpq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#39, 3:2, MODRM_REG:3, MODRM_RM:3>>;
cmpq(Imm, {Offset, Reg}) when ?IS_SINT8_T(Imm), is_atom(Reg), ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, (modrm_mem(7, MODRM_RM, Offset))/binary, Imm>>;
cmpq(Imm, {Offset, Reg}) when ?IS_SINT32_T(Imm), is_atom(Reg), ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, (modrm_mem(7, MODRM_RM, Offset))/binary, Imm:32/little>>;
cmpq(Imm, {Offset, Reg}) when ?IS_SINT8_T(Imm), is_atom(Reg), ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, (modrm_mem(7, MODRM_RM, Offset))/binary, Imm>>;
cmpq(Imm, {Offset, Reg}) when ?IS_SINT32_T(Imm), is_atom(Reg), ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, (modrm_mem(7, MODRM_RM, Offset))/binary, Imm:32/little>>;
cmpq(Imm, Reg) when ?IS_SINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#83, (16#F8 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#83, (16#F8 + Index), Imm>>
    end;
cmpq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % Special short encoding for cmp imm32, %rax
    <<16#48, 16#3D, Imm:32/little>>;
cmpq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 7:3, MODRM_RM:3, Imm:32/little>>.

addq(Imm, {Offset, Reg}) when ?IS_SINT8_T(Imm), ?IS_SINT8_T(Offset), is_atom(Reg) ->
    % Memory-destination read-modify-write: addq $imm8, disp8(reg)
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, (modrm_mem(0, MODRM_RM, Offset))/binary, Imm>>;
addq(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#83, (16#C0 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#83, (16#C0 + Index), Imm>>
    end;
addq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % Special short encoding for add imm32, %rax
    <<16#48, 16#05, Imm:32/little>>;
addq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 0:3, MODRM_RM:3, Imm:32/little>>;
addq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#01, 3:2, MODRM_REG:3, MODRM_RM:3>>.

subq(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#83, (16#E8 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#83, (16#E8 + Index), Imm>>
    end;
subq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    % Special short encoding for sub imm32, %rax
    <<16#48, 16#2D, Imm:32/little>>;
subq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 5:3, MODRM_RM:3, Imm:32/little>>;
subq(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#29, 3:2, MODRM_REG:3, MODRM_RM:3>>.

imulq(Imm, Reg) when ?IS_SINT8_T(Imm), is_atom(Reg) ->
    {REX_H, MODRM} = x86_64_x_reg(Reg),
    REX = 16#48 bor (REX_H bsl 2) bor REX_H,
    <<REX, 16#6B, (16#C0 bor (MODRM bsl 3) bor MODRM), Imm>>;
imulq(Imm, Reg) when ?IS_SINT32_T(Imm), is_atom(Reg) ->
    {REX_H, MODRM} = x86_64_x_reg(Reg),
    REX = 16#48 bor (REX_H bsl 2) bor REX_H,
    <<REX, 16#69, (16#C0 bor (MODRM bsl 3) bor MODRM), Imm:32/little>>;
imulq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    % DestReg for REX.R and ModRM.reg
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    % SrcReg for REX.B and ModRM.rm
    {REX_B, MODRM_RM} = x86_64_x_reg(SrcReg),
    REX = 16#48 bor (REX_R bsl 2) bor REX_B,
    <<REX, 16#0F, 16#AF, (16#C0 bor (MODRM_REG bsl 3) bor MODRM_RM)>>.

decl({Offset, rsi}) when ?IS_SINT8_T(Offset) ->
    <<16#FF, 16#4E, Offset>>.

orq_rel32(Imm, rax) when ?IS_UINT32_T(Imm) ->
    {2, <<?X86_64_REX(1, 0, 0, 0), 16#0D, Imm:32/little>>};
orq_rel32(Imm, Reg) when ?IS_UINT32_T(Imm) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    {3, <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 1:3, MODRM_RM:3, Imm:32/little>>}.

orq(Imm, Reg) when ?IS_SINT8_T(Imm) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 3:2, 1:3, MODRM_RM:3, Imm>>;
orq(Imm, rax) when ?IS_UINT32_T(Imm) ->
    <<?X86_64_REX(1, 0, 0, 0), 16#0D, Imm:32/little>>;
orq(Imm, Reg) when ?IS_UINT32_T(Imm) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 1:3, MODRM_RM:3, Imm:32/little>>;
orq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#09, 3:2, MODRM_REG:3, MODRM_RM:3>>.

xorl(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    (case {REX_R, REX_B} of
        {0, 0} -> <<16#31, 3:2, MODRM_REG:3, MODRM_RM:3>>;
        _ -> <<(16#40 bor (REX_R bsl 2) bor REX_B), 16#31, 3:2, MODRM_REG:3, MODRM_RM:3>>
    end).

xorq(Imm, DestReg) when is_integer(Imm), Imm >= 16#80000000, Imm < 16#100000000, is_atom(DestReg) ->
    xorq(Imm - 16#100000000, DestReg);
xorq(Imm, DestReg) when ?IS_SINT8_T(Imm) andalso is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#83, 3:2, 6:3, MODRM_RM:3, Imm>>;
xorq(Imm, rax) when ?IS_SINT32_T(Imm) ->
    <<?X86_64_REX(1, 0, 0, 0), 16#35, Imm:32/little>>;
xorq(Imm, DestReg) when ?IS_SINT32_T(Imm) andalso is_atom(DestReg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#81, 3:2, 6:3, MODRM_RM:3, Imm:32/little>>;
xorq(SrcReg, DestReg) when is_atom(SrcReg), is_atom(DestReg) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(SrcReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(DestReg),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#31, 3:2, MODRM_REG:3, MODRM_RM:3>>.

leaq_rel32({Offset, rip}, Reg) when is_atom(Reg), ?IS_SINT32_T(Offset) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> {3, <<16#48, 16#8D, (16#05 + (Index bsl 3)), Offset:32/little>>};
        {1, Index} -> {3, <<16#4C, 16#8D, (16#05 + (Index bsl 3)), Offset:32/little>>}
    end.

leaq({rip, Offset}, DestReg) when is_atom(DestReg), ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    % RIP-relative addressing: ModRM: mod=00, reg=DestReg, rm=101 (RIP-relative)
    % (literal bytes, NOT modrm_mem: rm=101 at mod=00 IS the RIP encoding)
    <<?X86_64_REX(1, REX_R, 0, 0), 16#8D, 0:2, MODRM_REG:3, 5:3, Offset:32/little>>;
leaq({Offset, BaseReg}, DestReg) when is_atom(BaseReg), is_atom(DestReg), ?IS_SINT8_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(BaseReg),
    % ModRM: mod=01 (disp8), reg=DestReg, rm=BaseReg
    % SIB: scale=0, index=100 (none), base=BaseReg
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8D, (modrm_mem(MODRM_REG, MODRM_RM, Offset))/binary>>;
leaq({Offset, BaseReg}, DestReg) when is_atom(BaseReg), is_atom(DestReg), ?IS_SINT32_T(Offset) ->
    {REX_R, MODRM_REG} = x86_64_x_reg(DestReg),
    {REX_B, MODRM_RM} = x86_64_x_reg(BaseReg),
    % ModRM: mod=10 (disp32), reg=DestReg, rm=BaseReg
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#8D, (modrm_mem(MODRM_REG, MODRM_RM, Offset))/binary>>.

callq({Reg}) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#FF, (16#D0 + Index)>>;
        {1, Index} -> <<16#41, 16#FF, (16#D0 + Index)>>
    end.

pushq(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<(16#50 + Index)>>;
        {1, Index} -> <<16#41, (16#50 + Index)>>
    end.

popq(Reg) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<(16#58 + Index)>>;
        {1, Index} -> <<16#41, (16#58 + Index)>>
    end.

jmpq({0, Reg}) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    (case {REX_B, MODRM_RM} of
        {0, RM} -> <<16#FF, (modrm_mem(4, RM, 0))/binary>>;
        {1, RM} -> <<16#41, 16#FF, (modrm_mem(4, RM, 0))/binary>>
    end);
jmpq({Offset, Reg}) when ?IS_SINT8_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    (case REX_B of
        0 -> <<16#FF, (modrm_mem(4, MODRM_RM, Offset))/binary>>;
        1 -> <<16#41, 16#FF, (modrm_mem(4, MODRM_RM, Offset))/binary>>
    end);
jmpq({Offset, Reg}) when ?IS_SINT32_T(Offset) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    (case REX_B of
        0 -> <<16#FF, (modrm_mem(4, MODRM_RM, Offset))/binary>>;
        1 -> <<16#41, 16#FF, (modrm_mem(4, MODRM_RM, Offset))/binary>>
    end);
jmpq({Reg}) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#FF, (16#E0 + Index)>>;
        {1, Index} -> <<16#41, 16#FF, (16#E0 + Index)>>
    end.

retq() ->
    <<16#C3>>.

%% Canonical multi-byte NOP of exactly N bytes (Intel SDM recommended forms,
%% N =< 9). Widths larger than 9 are built from a 9-byte nop plus a shorter
%% one. Used to neutralize an elided store in place (jit_backend_pending).
nop(1) -> <<16#90>>;
nop(2) -> <<16#66, 16#90>>;
nop(3) -> <<16#0F, 16#1F, 16#00>>;
nop(4) -> <<16#0F, 16#1F, 16#40, 16#00>>;
nop(5) -> <<16#0F, 16#1F, 16#44, 16#00, 16#00>>;
nop(6) -> <<16#66, 16#0F, 16#1F, 16#44, 16#00, 16#00>>;
nop(7) -> <<16#0F, 16#1F, 16#80, 16#00, 16#00, 16#00, 16#00>>;
nop(8) -> <<16#0F, 16#1F, 16#84, 16#00, 16#00, 16#00, 16#00, 16#00>>;
nop(9) -> <<16#66, 16#0F, 16#1F, 16#84, 16#00, 16#00, 16#00, 16#00, 16#00>>;
nop(N) when N > 9 -> <<(nop(9))/binary, (nop(N - 9))/binary>>.

%% XCHG r64, r64: Exchange two 64-bit registers
%% Encoding: REX.W + 87 /r
xchgq(rax, rax) ->
    % NOP
    <<16#90>>;
xchgq(rax, Reg) when is_atom(Reg) ->
    % Special short encoding for XCHG rax, r64
    % For low registers: REX.W + 0x90 + reg
    % For high registers: REX.W + REX.B + 0x90 + reg (need REX.B to access r8-r11)
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, (16#90 + Index)>>;
        {1, Index} -> <<16#49, (16#90 + Index)>>
    end;
xchgq(Reg, rax) when is_atom(Reg) ->
    % XCHG is commutative
    xchgq(rax, Reg);
xchgq(RegA, RegB) when is_atom(RegA), is_atom(RegB) ->
    % General form: REX.W + 87 /r
    {REX_R, MODRM_REG} = x86_64_x_reg(RegA),
    {REX_B, MODRM_RM} = x86_64_x_reg(RegB),
    <<?X86_64_REX(1, REX_R, 0, REX_B), 16#87, 3:2, MODRM_REG:3, MODRM_RM:3>>.

cqo() ->
    <<16#48, 16#99>>.

idivq(Reg) when is_atom(Reg) ->
    {REX_B, MODRM_RM} = x86_64_x_reg(Reg),
    <<?X86_64_REX(1, 0, 0, REX_B), 16#F7, 3:2, 7:3, MODRM_RM:3>>.

sarq(Imm, Reg) when ?IS_UINT8_T(Imm) ->
    case x86_64_x_reg(Reg) of
        {0, Index} -> <<16#48, 16#C1, (16#F8 + Index), Imm>>;
        {1, Index} -> <<16#49, 16#C1, (16#F8 + Index), Imm>>
    end.
