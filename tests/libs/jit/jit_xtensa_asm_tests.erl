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

-module(jit_xtensa_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(xtensa, Bin, Str), Value)
).

%%=============================================================================
%% RRR-type Arithmetic Instruction Tests
%%=============================================================================

add_test_() ->
    [
        ?_assertAsmEqual(<<16#803450:24/little>>, "add a3, a4, a5", jit_xtensa_asm:add(a3, a4, a5)),
        ?_assertAsmEqual(<<16#800000:24/little>>, "add a0, a0, a0", jit_xtensa_asm:add(a0, a0, a0)),
        ?_assertAsmEqual(
            <<16#80FFE0:24/little>>, "add a15, a15, a14", jit_xtensa_asm:add(a15, a15, a14)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(<<16#C03450:24/little>>, "sub a3, a4, a5", jit_xtensa_asm:sub(a3, a4, a5)),
        ?_assertAsmEqual(
            <<16#C0BAC0:24/little>>, "sub a11, a10, a12", jit_xtensa_asm:sub(a11, a10, a12)
        )
    ].

and_test_() ->
    [
        ?_assertAsmEqual(<<16#103450:24/little>>, "and a3, a4, a5", jit_xtensa_asm:and_(a3, a4, a5))
    ].

or_test_() ->
    [
        ?_assertAsmEqual(<<16#203450:24/little>>, "or a3, a4, a5", jit_xtensa_asm:or_(a3, a4, a5))
    ].

xor_test_() ->
    [
        ?_assertAsmEqual(<<16#303450:24/little>>, "xor a3, a4, a5", jit_xtensa_asm:xor_(a3, a4, a5))
    ].

addx2_test_() ->
    [
        ?_assertAsmEqual(
            <<16#903450:24/little>>, "addx2 a3, a4, a5", jit_xtensa_asm:addx2(a3, a4, a5)
        )
    ].

addx4_test_() ->
    [
        ?_assertAsmEqual(
            <<16#A03450:24/little>>, "addx4 a3, a4, a5", jit_xtensa_asm:addx4(a3, a4, a5)
        )
    ].

addx8_test_() ->
    [
        ?_assertAsmEqual(
            <<16#B03450:24/little>>, "addx8 a3, a4, a5", jit_xtensa_asm:addx8(a3, a4, a5)
        )
    ].

%%=============================================================================
%% Shift Instruction Tests
%%=============================================================================

slli_test_() ->
    [
        ?_assertAsmEqual(<<16#113480:24/little>>, "slli a3, a4, 8", jit_xtensa_asm:slli(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#113400:24/little>>, "slli a3, a4, 16", jit_xtensa_asm:slli(a3, a4, 16)
        ),
        ?_assertAsmEqual(
            <<16#013110:24/little>>, "slli a3, a1, 31", jit_xtensa_asm:slli(a3, a1, 31)
        )
    ].

srli_test_() ->
    [
        ?_assertAsmEqual(<<16#413840:24/little>>, "srli a3, a4, 8", jit_xtensa_asm:srli(a3, a4, 8)),
        ?_assertAsmEqual(<<16#410040:24/little>>, "srli a0, a4, 0", jit_xtensa_asm:srli(a0, a4, 0))
    ].

srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16#313440:24/little>>, "srai a3, a4, 20", jit_xtensa_asm:srai(a3, a4, 20)
        ),
        ?_assertAsmEqual(<<16#213840:24/little>>, "srai a3, a4, 8", jit_xtensa_asm:srai(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#313F40:24/little>>, "srai a3, a4, 31", jit_xtensa_asm:srai(a3, a4, 31)
        )
    ].

ssl_test_() ->
    [
        ?_assertAsmEqual(<<16#401300:24/little>>, "ssl a3", jit_xtensa_asm:ssl(a3))
    ].

ssr_test_() ->
    [
        ?_assertAsmEqual(<<16#400300:24/little>>, "ssr a3", jit_xtensa_asm:ssr(a3))
    ].

ssa8l_test_() ->
    [
        ?_assertAsmEqual(<<16#402300:24/little>>, "ssa8l a3", jit_xtensa_asm:ssa8l(a3))
    ].

%%=============================================================================
%% Multiply/Divide Instruction Tests
%%=============================================================================

mull_test_() ->
    [
        ?_assertAsmEqual(
            <<16#823450:24/little>>, "mull a3, a4, a5", jit_xtensa_asm:mull(a3, a4, a5)
        )
    ].

quos_test_() ->
    [
        ?_assertAsmEqual(
            <<16#D23450:24/little>>, "quos a3, a4, a5", jit_xtensa_asm:quos(a3, a4, a5)
        )
    ].

rems_test_() ->
    [
        ?_assertAsmEqual(
            <<16#F23450:24/little>>, "rems a3, a4, a5", jit_xtensa_asm:rems(a3, a4, a5)
        )
    ].

quou_test_() ->
    [
        ?_assertAsmEqual(
            <<16#C23450:24/little>>, "quou a3, a4, a5", jit_xtensa_asm:quou(a3, a4, a5)
        )
    ].

remu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#E23450:24/little>>, "remu a3, a4, a5", jit_xtensa_asm:remu(a3, a4, a5)
        )
    ].

%%=============================================================================
%% Immediate Arithmetic Instruction Tests
%%=============================================================================

addi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0AC432:24/little>>, "addi a3, a4, 10", jit_xtensa_asm:addi(a3, a4, 10)
        ),
        ?_assertAsmEqual(
            <<16#FFC432:24/little>>, "addi a3, a4, -1", jit_xtensa_asm:addi(a3, a4, -1)
        ),
        ?_assertAsmEqual(
            <<16#80C432:24/little>>, "addi a3, a4, -128", jit_xtensa_asm:addi(a3, a4, -128)
        ),
        ?_assertAsmEqual(
            <<16#7FC432:24/little>>, "addi a3, a4, 127", jit_xtensa_asm:addi(a3, a4, 127)
        ),
        ?_assertAsmEqual(<<16#00C002:24/little>>, "addi a0, a0, 0", jit_xtensa_asm:addi(a0, a0, 0))
    ].

addmi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#01D432:24/little>>, "addmi a3, a4, 256", jit_xtensa_asm:addmi(a3, a4, 256)
        ),
        ?_assertAsmEqual(
            <<16#FFD432:24/little>>, "addmi a3, a4, -256", jit_xtensa_asm:addmi(a3, a4, -256)
        )
    ].

movi_test_() ->
    [
        ?_assertAsmEqual(<<16#00A032:24/little>>, "movi a3, 0", jit_xtensa_asm:movi(a3, 0)),
        ?_assertAsmEqual(<<16#64A032:24/little>>, "movi a3, 100", jit_xtensa_asm:movi(a3, 100)),
        ?_assertAsmEqual(<<16#FFAF32:24/little>>, "movi a3, -1", jit_xtensa_asm:movi(a3, -1)),
        ?_assertAsmEqual(<<16#FFA732:24/little>>, "movi a3, 2047", jit_xtensa_asm:movi(a3, 2047)),
        ?_assertAsmEqual(<<16#00A832:24/little>>, "movi a3, -2048", jit_xtensa_asm:movi(a3, -2048))
    ].

%%=============================================================================
%% Load Instruction Tests
%%=============================================================================

l32i_test_() ->
    [
        ?_assertAsmEqual(<<16#002432:24/little>>, "l32i a3, a4, 0", jit_xtensa_asm:l32i(a3, a4, 0)),
        ?_assertAsmEqual(<<16#022432:24/little>>, "l32i a3, a4, 8", jit_xtensa_asm:l32i(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#FF2432:24/little>>, "l32i a3, a4, 1020", jit_xtensa_asm:l32i(a3, a4, 1020)
        )
    ].

s32i_test_() ->
    [
        ?_assertAsmEqual(<<16#006432:24/little>>, "s32i a3, a4, 0", jit_xtensa_asm:s32i(a3, a4, 0)),
        ?_assertAsmEqual(<<16#026432:24/little>>, "s32i a3, a4, 8", jit_xtensa_asm:s32i(a3, a4, 8)),
        ?_assertAsmEqual(
            <<16#FF6432:24/little>>, "s32i a3, a4, 1020", jit_xtensa_asm:s32i(a3, a4, 1020)
        )
    ].

l8ui_test_() ->
    [
        ?_assertAsmEqual(<<16#000432:24/little>>, "l8ui a3, a4, 0", jit_xtensa_asm:l8ui(a3, a4, 0)),
        ?_assertAsmEqual(
            <<16#0A0432:24/little>>, "l8ui a3, a4, 10", jit_xtensa_asm:l8ui(a3, a4, 10)
        ),
        ?_assertAsmEqual(
            <<16#FF0432:24/little>>, "l8ui a3, a4, 255", jit_xtensa_asm:l8ui(a3, a4, 255)
        )
    ].

l16ui_test_() ->
    [
        ?_assertAsmEqual(
            <<16#001432:24/little>>, "l16ui a3, a4, 0", jit_xtensa_asm:l16ui(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#021432:24/little>>, "l16ui a3, a4, 4", jit_xtensa_asm:l16ui(a3, a4, 4)
        ),
        ?_assertAsmEqual(
            <<16#FF1432:24/little>>, "l16ui a3, a4, 510", jit_xtensa_asm:l16ui(a3, a4, 510)
        )
    ].

l16si_test_() ->
    [
        ?_assertAsmEqual(
            <<16#009432:24/little>>, "l16si a3, a4, 0", jit_xtensa_asm:l16si(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#029432:24/little>>, "l16si a3, a4, 4", jit_xtensa_asm:l16si(a3, a4, 4)
        )
    ].

s8i_test_() ->
    [
        ?_assertAsmEqual(<<16#004432:24/little>>, "s8i a3, a4, 0", jit_xtensa_asm:s8i(a3, a4, 0)),
        ?_assertAsmEqual(<<16#0A4432:24/little>>, "s8i a3, a4, 10", jit_xtensa_asm:s8i(a3, a4, 10))
    ].

s16i_test_() ->
    [
        ?_assertAsmEqual(<<16#005432:24/little>>, "s16i a3, a4, 0", jit_xtensa_asm:s16i(a3, a4, 0)),
        ?_assertAsmEqual(<<16#025432:24/little>>, "s16i a3, a4, 4", jit_xtensa_asm:s16i(a3, a4, 4))
    ].

%%=============================================================================
%% Load/Store Tuple Form Tests
%%=============================================================================

l32i_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:l32i(a3, a4, 8), jit_xtensa_asm:l32i(a3, {a4, 8})),
        ?_assertEqual(jit_xtensa_asm:l32i(a3, a4, 0), jit_xtensa_asm:l32i(a3, {a4, 0}))
    ].

s32i_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:s32i(a3, a4, 8), jit_xtensa_asm:s32i(a3, {a4, 8})),
        ?_assertEqual(jit_xtensa_asm:s32i(a3, a4, 0), jit_xtensa_asm:s32i(a3, {a4, 0}))
    ].

l16ui_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:l16ui(a3, a4, 4), jit_xtensa_asm:l16ui(a3, {a4, 4}))
    ].

l16si_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:l16si(a3, a4, 4), jit_xtensa_asm:l16si(a3, {a4, 4}))
    ].

l8ui_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:l8ui(a3, a4, 10), jit_xtensa_asm:l8ui(a3, {a4, 10}))
    ].

s16i_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:s16i(a3, a4, 4), jit_xtensa_asm:s16i(a3, {a4, 4}))
    ].

s8i_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:s8i(a3, a4, 10), jit_xtensa_asm:s8i(a3, {a4, 10}))
    ].

%%=============================================================================
%% Branch Instruction Tests (BRI8 format)
%% Note: Erlang offset value maps to asm ". + (offset + 4)" since
%% target = PC + 4 + sign_extend(offset)
%%=============================================================================

beq_test_() ->
    [
        ?_assertAsmEqual(
            <<16#081347:24/little>>, "beq a3, a4, . + 12", jit_xtensa_asm:beq(a3, a4, 8)
        ),
        ?_assertAsmEqual(
            <<16#001347:24/little>>, "beq a3, a4, . + 4", jit_xtensa_asm:beq(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#FC1347:24/little>>, "beq a3, a4, . + 0", jit_xtensa_asm:beq(a3, a4, -4)
        )
    ].

bne_test_() ->
    [
        ?_assertAsmEqual(
            <<16#009347:24/little>>, "bne a3, a4, . + 4", jit_xtensa_asm:bne(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#089347:24/little>>, "bne a3, a4, . + 12", jit_xtensa_asm:bne(a3, a4, 8)
        )
    ].

blt_test_() ->
    [
        ?_assertAsmEqual(
            <<16#002347:24/little>>, "blt a3, a4, . + 4", jit_xtensa_asm:blt(a3, a4, 0)
        )
    ].

bge_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00A347:24/little>>, "bge a3, a4, . + 4", jit_xtensa_asm:bge(a3, a4, 0)
        )
    ].

bltu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#003347:24/little>>, "bltu a3, a4, . + 4", jit_xtensa_asm:bltu(a3, a4, 0)
        )
    ].

bgeu_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00B347:24/little>>, "bgeu a3, a4, . + 4", jit_xtensa_asm:bgeu(a3, a4, 0)
        )
    ].

%%=============================================================================
%% Branch-vs-Immediate Instruction Tests
%% Same offset convention as BRI8
%%=============================================================================

beqi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#001326:24/little>>, "beqi a3, 1, . + 4", jit_xtensa_asm:beqi(a3, 1, 0)
        ),
        ?_assertAsmEqual(
            <<16#000326:24/little>>, "beqi a3, -1, . + 4", jit_xtensa_asm:beqi(a3, -1, 0)
        )
    ].

bnei_test_() ->
    [
        ?_assertAsmEqual(
            <<16#001366:24/little>>, "bnei a3, 1, . + 4", jit_xtensa_asm:bnei(a3, 1, 0)
        )
    ].

blti_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0013A6:24/little>>, "blti a3, 1, . + 4", jit_xtensa_asm:blti(a3, 1, 0)
        )
    ].

bgei_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0013E6:24/little>>, "bgei a3, 1, . + 4", jit_xtensa_asm:bgei(a3, 1, 0)
        )
    ].

bltui_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0043B6:24/little>>, "bltui a3, 4, . + 4", jit_xtensa_asm:bltui(a3, 4, 0)
        )
    ].

bgeui_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0043F6:24/little>>, "bgeui a3, 4, . + 4", jit_xtensa_asm:bgeui(a3, 4, 0)
        )
    ].

%%=============================================================================
%% Branch-Zero Instruction Tests (BRI12 format)
%% Offset convention: target = PC + 4 + offset, so asm ". + (offset + 4)"
%%=============================================================================

beqz_test_() ->
    [
        ?_assertAsmEqual(<<16#000316:24/little>>, "beqz a3, . + 4", jit_xtensa_asm:beqz(a3, 0)),
        ?_assertAsmEqual(<<16#07F316:24/little>>, "beqz a3, . + 131", jit_xtensa_asm:beqz(a3, 127))
    ].

bnez_test_() ->
    [
        ?_assertAsmEqual(<<16#000356:24/little>>, "bnez a3, . + 4", jit_xtensa_asm:bnez(a3, 0)),
        ?_assertAsmEqual(<<16#001356:24/little>>, "bnez a3, . + 5", jit_xtensa_asm:bnez(a3, 1))
    ].

bgez_test_() ->
    [
        ?_assertAsmEqual(<<16#0003D6:24/little>>, "bgez a3, . + 4", jit_xtensa_asm:bgez(a3, 0))
    ].

bltz_test_() ->
    [
        ?_assertAsmEqual(<<16#000396:24/little>>, "bltz a3, . + 4", jit_xtensa_asm:bltz(a3, 0))
    ].

%%=============================================================================
%% Bit Test Branch Instruction Tests
%% Same offset convention as BRI8
%%=============================================================================

bbci_test_() ->
    [
        ?_assertAsmEqual(
            <<16#006307:24/little>>, "bbci a3, 0, . + 4", jit_xtensa_asm:bbci(a3, 0, 0)
        ),
        ?_assertAsmEqual(
            <<16#0063F7:24/little>>, "bbci a3, 15, . + 4", jit_xtensa_asm:bbci(a3, 15, 0)
        ),
        ?_assertAsmEqual(
            <<16#007307:24/little>>, "bbci a3, 16, . + 4", jit_xtensa_asm:bbci(a3, 16, 0)
        ),
        ?_assertAsmEqual(
            <<16#0073F7:24/little>>, "bbci a3, 31, . + 4", jit_xtensa_asm:bbci(a3, 31, 0)
        )
    ].

bbsi_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00E307:24/little>>, "bbsi a3, 0, . + 4", jit_xtensa_asm:bbsi(a3, 0, 0)
        ),
        ?_assertAsmEqual(
            <<16#00F307:24/little>>, "bbsi a3, 16, . + 4", jit_xtensa_asm:bbsi(a3, 16, 0)
        ),
        ?_assertAsmEqual(
            <<16#00F3F7:24/little>>, "bbsi a3, 31, . + 4", jit_xtensa_asm:bbsi(a3, 31, 0)
        )
    ].

%%=============================================================================
%% Jump Instruction Tests
%%=============================================================================

j_test_() ->
    [
        ?_assertAsmEqual(<<16#000006:24/little>>, "j . + 4", jit_xtensa_asm:j(0)),
        ?_assertAsmEqual(<<16#001906:24/little>>, "j . + 104", jit_xtensa_asm:j(100))
    ].

jx_test_() ->
    [
        ?_assertAsmEqual(<<16#0003A0:24/little>>, "jx a3", jit_xtensa_asm:jx(a3)),
        ?_assertAsmEqual(<<16#0000A0:24/little>>, "jx a0", jit_xtensa_asm:jx(a0))
    ].

callx0_test_() ->
    [
        ?_assertAsmEqual(<<16#0003C0:24/little>>, "callx0 a3", jit_xtensa_asm:callx0(a3)),
        ?_assertAsmEqual(<<16#0000C0:24/little>>, "callx0 a0", jit_xtensa_asm:callx0(a0))
    ].

ret_test_() ->
    [
        ?_assertAsmEqual(<<16#000080:24/little>>, "ret", jit_xtensa_asm:ret())
    ].

ret_n_test_() ->
    [
        ?_assertAsmEqual(<<16#0D, 16#F0>>, "ret.n", jit_xtensa_asm:ret_n())
    ].

%%=============================================================================
%% Windowed ABI Instruction Tests
%%=============================================================================

retw_test_() ->
    [
        ?_assertAsmEqual(<<16#000090:24/little>>, "retw", jit_xtensa_asm:retw())
    ].

retw_n_test_() ->
    [
        ?_assertAsmEqual(<<16#1D, 16#F0>>, "retw.n", jit_xtensa_asm:retw_n())
    ].

callx8_test_() ->
    [
        ?_assertAsmEqual(<<16#0008E0:24/little>>, "callx8 a8", jit_xtensa_asm:callx8(a8)),
        ?_assertAsmEqual(<<16#0003E0:24/little>>, "callx8 a3", jit_xtensa_asm:callx8(a3))
    ].

entry_test_() ->
    [
        ?_assertAsmEqual(<<16#004136:24/little>>, "entry a1, 32", jit_xtensa_asm:entry(a1, 32)),
        ?_assertAsmEqual(<<16#006136:24/little>>, "entry a1, 48", jit_xtensa_asm:entry(a1, 48))
    ].

%%=============================================================================
%% Narrow (Density) Instruction Tests - 16-bit
%%=============================================================================

add_n_test_() ->
    [
        ?_assertAsmEqual(
            <<16#345A:16/little>>, "add.n a3, a4, a5", jit_xtensa_asm:add_n(a3, a4, a5)
        ),
        ?_assertAsmEqual(
            <<16#000A:16/little>>, "add.n a0, a0, a0", jit_xtensa_asm:add_n(a0, a0, a0)
        )
    ].

addi_n_test_() ->
    [
        ?_assertAsmEqual(
            <<16#345B:16/little>>, "addi.n a3, a4, 5", jit_xtensa_asm:addi_n(a3, a4, 5)
        ),
        ?_assertAsmEqual(
            <<16#340B:16/little>>, "addi.n a3, a4, -1", jit_xtensa_asm:addi_n(a3, a4, -1)
        ),
        ?_assertAsmEqual(
            <<16#341B:16/little>>, "addi.n a3, a4, 1", jit_xtensa_asm:addi_n(a3, a4, 1)
        ),
        ?_assertAsmEqual(
            <<16#34FB:16/little>>, "addi.n a3, a4, 15", jit_xtensa_asm:addi_n(a3, a4, 15)
        )
    ].

mov_n_test_() ->
    [
        ?_assertAsmEqual(<<16#043D:16/little>>, "mov.n a3, a4", jit_xtensa_asm:mov_n(a3, a4)),
        ?_assertAsmEqual(<<16#000D:16/little>>, "mov.n a0, a0", jit_xtensa_asm:mov_n(a0, a0))
    ].

l32i_n_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0438:16/little>>, "l32i.n a3, a4, 0", jit_xtensa_asm:l32i_n(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#2438:16/little>>, "l32i.n a3, a4, 8", jit_xtensa_asm:l32i_n(a3, a4, 8)
        ),
        ?_assertAsmEqual(
            <<16#F438:16/little>>, "l32i.n a3, a4, 60", jit_xtensa_asm:l32i_n(a3, a4, 60)
        )
    ].

s32i_n_test_() ->
    [
        ?_assertAsmEqual(
            <<16#0439:16/little>>, "s32i.n a3, a4, 0", jit_xtensa_asm:s32i_n(a3, a4, 0)
        ),
        ?_assertAsmEqual(
            <<16#2439:16/little>>, "s32i.n a3, a4, 8", jit_xtensa_asm:s32i_n(a3, a4, 8)
        ),
        ?_assertAsmEqual(
            <<16#F439:16/little>>, "s32i.n a3, a4, 60", jit_xtensa_asm:s32i_n(a3, a4, 60)
        )
    ].

l32i_n_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:l32i_n(a3, a4, 8), jit_xtensa_asm:l32i_n(a3, {a4, 8}))
    ].

s32i_n_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:s32i_n(a3, a4, 8), jit_xtensa_asm:s32i_n(a3, {a4, 8}))
    ].

movi_n_test_() ->
    [
        ?_assertAsmEqual(<<16#030C:16/little>>, "movi.n a3, 0", jit_xtensa_asm:movi_n(a3, 0)),
        ?_assertAsmEqual(<<16#F35C:16/little>>, "movi.n a3, 95", jit_xtensa_asm:movi_n(a3, 95)),
        ?_assertAsmEqual(<<16#036C:16/little>>, "movi.n a3, -32", jit_xtensa_asm:movi_n(a3, -32)),
        ?_assertAsmEqual(<<16#130C:16/little>>, "movi.n a3, 1", jit_xtensa_asm:movi_n(a3, 1))
    ].

%% beqz.n/bnez.n: target = PC + 4 + imm6, so asm ". + (offset + 4)"
beqz_n_test_() ->
    [
        ?_assertAsmEqual(<<16#038C:16/little>>, "beqz.n a3, . + 4", jit_xtensa_asm:beqz_n(a3, 0)),
        ?_assertAsmEqual(<<16#F38C:16/little>>, "beqz.n a3, . + 19", jit_xtensa_asm:beqz_n(a3, 15)),
        ?_assertAsmEqual(<<16#039C:16/little>>, "beqz.n a3, . + 20", jit_xtensa_asm:beqz_n(a3, 16)),
        ?_assertAsmEqual(<<16#F3BC:16/little>>, "beqz.n a3, . + 67", jit_xtensa_asm:beqz_n(a3, 63))
    ].

bnez_n_test_() ->
    [
        ?_assertAsmEqual(<<16#03CC:16/little>>, "bnez.n a3, . + 4", jit_xtensa_asm:bnez_n(a3, 0)),
        ?_assertAsmEqual(<<16#F3CC:16/little>>, "bnez.n a3, . + 19", jit_xtensa_asm:bnez_n(a3, 15)),
        ?_assertAsmEqual(<<16#03DC:16/little>>, "bnez.n a3, . + 20", jit_xtensa_asm:bnez_n(a3, 16)),
        ?_assertAsmEqual(<<16#F3FC:16/little>>, "bnez.n a3, . + 67", jit_xtensa_asm:bnez_n(a3, 63))
    ].

nop_n_test_() ->
    [
        ?_assertAsmEqual(<<16#3D, 16#F0>>, "nop.n", jit_xtensa_asm:nop_n())
    ].

%%=============================================================================
%% Misc Instruction Tests
%%=============================================================================

nop_test_() ->
    [
        %% nop is or a1, a1, a1
        ?_assertAsmEqual(<<16#201110:24/little>>, "or a1, a1, a1", jit_xtensa_asm:nop()),
        ?_assertEqual(jit_xtensa_asm:or_(a1, a1, a1), jit_xtensa_asm:nop())
    ].

break_test_() ->
    [
        ?_assertAsmEqual(<<16#004000:24/little>>, "break 0, 0", jit_xtensa_asm:break(0, 0)),
        ?_assertAsmEqual(<<16#004120:24/little>>, "break 1, 2", jit_xtensa_asm:break(1, 2))
    ].

memw_test_() ->
    [
        ?_assertAsmEqual(<<16#0020C0:24/little>>, "memw", jit_xtensa_asm:memw())
    ].

isync_test_() ->
    [
        ?_assertAsmEqual(<<16#002000:24/little>>, "isync", jit_xtensa_asm:isync())
    ].

dsync_test_() ->
    [
        ?_assertAsmEqual(<<16#002030:24/little>>, "dsync", jit_xtensa_asm:dsync())
    ].

neg_test_() ->
    [
        ?_assertAsmEqual(<<16#603050:24/little>>, "neg a3, a5", jit_xtensa_asm:neg(a3, a5)),
        ?_assertAsmEqual(<<16#600000:24/little>>, "neg a0, a0", jit_xtensa_asm:neg(a0, a0))
    ].

%%=============================================================================
%% Pseudo-instruction Tests
%%=============================================================================

mv_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:mov_n(a3, a4), jit_xtensa_asm:mv(a3, a4)),
        ?_assertEqual(2, byte_size(jit_xtensa_asm:mv(a3, a4)))
    ].

not_test_() ->
    [
        ?_assertEqual(
            <<(jit_xtensa_asm:movi(a3, -1))/binary, (jit_xtensa_asm:xor_(a3, a3, a4))/binary>>,
            jit_xtensa_asm:not_(a3, a4)
        ),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:not_(a3, a4))),
        ?_assertEqual(
            <<(jit_xtensa_asm:neg(a3, a3))/binary, (jit_xtensa_asm:addi(a3, a3, -1))/binary>>,
            jit_xtensa_asm:not_(a3, a3)
        ),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:not_(a3, a3)))
    ].

li_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:movi(a3, 0), jit_xtensa_asm:li(a3, 0)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, 100), jit_xtensa_asm:li(a3, 100)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, -1), jit_xtensa_asm:li(a3, -1)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, 2047), jit_xtensa_asm:li(a3, 2047)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, -2048), jit_xtensa_asm:li(a3, -2048)),
        ?_assertEqual(3, byte_size(jit_xtensa_asm:li(a3, 0))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, 4096))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, -4096))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, 32767))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, -32768))),
        ?_assertEqual(21, byte_size(jit_xtensa_asm:li(a3, 16#12345678)))
    ].
