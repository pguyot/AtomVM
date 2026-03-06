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

%%=============================================================================
%% RRR-type Arithmetic Instruction Tests
%%=============================================================================

%% add(Ar, As, At): encode_rrr(0, At, As, Ar, 8, 0)
%% add(a3, a4, a5): (0<<20)|(8<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x083450
add_test_() ->
    [
        ?_assertEqual(<<16#083450:24/little>>, jit_xtensa_asm:add(a3, a4, a5)),
        ?_assertEqual(<<16#080000:24/little>>, jit_xtensa_asm:add(a0, a0, a0)),
        ?_assertEqual(<<16#08FFE0:24/little>>, jit_xtensa_asm:add(a15, a15, a14))
    ].

%% sub(Ar, As, At): encode_rrr(0, At, As, Ar, 0xC, 0)
%% sub(a3, a4, a5): (0<<20)|(0xC<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x0C3450
sub_test_() ->
    [
        ?_assertEqual(<<16#0C3450:24/little>>, jit_xtensa_asm:sub(a3, a4, a5)),
        ?_assertEqual(<<16#0CBAC0:24/little>>, jit_xtensa_asm:sub(a11, a10, a12))
    ].

%% and_(Ar, As, At): encode_rrr(0, At, As, Ar, 1, 0)
and_test_() ->
    [
        ?_assertEqual(<<16#013450:24/little>>, jit_xtensa_asm:and_(a3, a4, a5))
    ].

%% or_(Ar, As, At): encode_rrr(0, At, As, Ar, 2, 0)
or_test_() ->
    [
        ?_assertEqual(<<16#023450:24/little>>, jit_xtensa_asm:or_(a3, a4, a5))
    ].

%% xor_(Ar, As, At): encode_rrr(0, At, As, Ar, 3, 0)
xor_test_() ->
    [
        ?_assertEqual(<<16#033450:24/little>>, jit_xtensa_asm:xor_(a3, a4, a5))
    ].

%% addx2(Ar, As, At): encode_rrr(0, At, As, Ar, 9, 0)
addx2_test_() ->
    [
        ?_assertEqual(<<16#093450:24/little>>, jit_xtensa_asm:addx2(a3, a4, a5))
    ].

%% addx4(Ar, As, At): encode_rrr(0, At, As, Ar, 0xA, 0)
addx4_test_() ->
    [
        ?_assertEqual(<<16#0A3450:24/little>>, jit_xtensa_asm:addx4(a3, a4, a5))
    ].

%% addx8(Ar, As, At): encode_rrr(0, At, As, Ar, 0xB, 0)
addx8_test_() ->
    [
        ?_assertEqual(<<16#0B3450:24/little>>, jit_xtensa_asm:addx8(a3, a4, a5))
    ].

%%=============================================================================
%% Shift Instruction Tests
%%=============================================================================

%% slli(Ar, As, Sa): Sa4=(Sa>>4)&1, Sa30=Sa&0xF
%% Instr = (Sa4<<20)|(1<<16)|(Ar<<12)|(As<<8)|(Sa30<<4)|0
%% slli(a3, a4, 8): Sa4=0, Sa30=8 -> (0<<20)|(1<<16)|(3<<12)|(4<<8)|(8<<4)|0 = 0x013480
%% slli(a3, a4, 16): Sa4=1, Sa30=0 -> (1<<20)|(1<<16)|(3<<12)|(4<<8)|(0<<4)|0 = 0x113400
slli_test_() ->
    [
        ?_assertEqual(<<16#013480:24/little>>, jit_xtensa_asm:slli(a3, a4, 8)),
        ?_assertEqual(<<16#113400:24/little>>, jit_xtensa_asm:slli(a3, a4, 16)),
        ?_assertEqual(<<16#1131F0:24/little>>, jit_xtensa_asm:slli(a3, a1, 31))
    ].

%% srli(Ar, At, Sa): encode_rrr(0, At, Sa&0xF, Ar, 4, 0)
%% srli(a3, a4, 8): (0<<20)|(4<<16)|(3<<12)|(8<<8)|(4<<4)|0 = 0x043840
srli_test_() ->
    [
        ?_assertEqual(<<16#043840:24/little>>, jit_xtensa_asm:srli(a3, a4, 8)),
        ?_assertEqual(<<16#040040:24/little>>, jit_xtensa_asm:srli(a0, a4, 0))
    ].

%% srai(Ar, At, Sa): Sa4=(Sa>>4)&1, Sa30=Sa&0xF
%% Instr = (Sa4<<20)|(4<<16)|(Ar<<12)|(Sa30<<8)|(At<<4)|0
%% srai(a3, a4, 20): Sa4=1, Sa30=4 -> (1<<20)|(4<<16)|(3<<12)|(4<<8)|(4<<4)|0 = 0x143440
srai_test_() ->
    [
        ?_assertEqual(<<16#143440:24/little>>, jit_xtensa_asm:srai(a3, a4, 20)),
        ?_assertEqual(<<16#043840:24/little>>, jit_xtensa_asm:srai(a3, a4, 8)),
        ?_assertEqual(<<16#143F40:24/little>>, jit_xtensa_asm:srai(a3, a4, 31))
    ].

%% ssl(As): encode_rrr(0, 0, As, 1, 4, 0)
%% ssl(a3): (0<<20)|(4<<16)|(1<<12)|(3<<8)|(0<<4)|0 = 0x041300
ssl_test_() ->
    [
        ?_assertEqual(<<16#041300:24/little>>, jit_xtensa_asm:ssl(a3))
    ].

%% ssr(As): encode_rrr(0, 0, As, 0, 4, 0)
%% ssr(a3): (0<<20)|(4<<16)|(0<<12)|(3<<8)|(0<<4)|0 = 0x040300
ssr_test_() ->
    [
        ?_assertEqual(<<16#040300:24/little>>, jit_xtensa_asm:ssr(a3))
    ].

%% ssa8l(As): encode_rrr(0, 0, As, 2, 4, 0)
%% ssa8l(a3): (0<<20)|(4<<16)|(2<<12)|(3<<8)|(0<<4)|0 = 0x042300
ssa8l_test_() ->
    [
        ?_assertEqual(<<16#042300:24/little>>, jit_xtensa_asm:ssa8l(a3))
    ].

%%=============================================================================
%% Multiply/Divide Instruction Tests
%%=============================================================================

%% mull(Ar, As, At): encode_rrr(0, At, As, Ar, 8, 2)
%% mull(a3, a4, a5): (2<<20)|(8<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x283450
mull_test_() ->
    [
        ?_assertEqual(<<16#283450:24/little>>, jit_xtensa_asm:mull(a3, a4, a5))
    ].

%% quos(Ar, As, At): encode_rrr(0, At, As, Ar, 0xD, 2)
%% quos(a3, a4, a5): (2<<20)|(0xD<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x2D3450
quos_test_() ->
    [
        ?_assertEqual(<<16#2D3450:24/little>>, jit_xtensa_asm:quos(a3, a4, a5))
    ].

%% rems(Ar, As, At): encode_rrr(0, At, As, Ar, 0xF, 2)
%% rems(a3, a4, a5): (2<<20)|(0xF<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x2F3450
rems_test_() ->
    [
        ?_assertEqual(<<16#2F3450:24/little>>, jit_xtensa_asm:rems(a3, a4, a5))
    ].

%% quou(Ar, As, At): encode_rrr(0, At, As, Ar, 0xC, 2)
%% quou(a3, a4, a5): (2<<20)|(0xC<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x2C3450
quou_test_() ->
    [
        ?_assertEqual(<<16#2C3450:24/little>>, jit_xtensa_asm:quou(a3, a4, a5))
    ].

%% remu(Ar, As, At): encode_rrr(0, At, As, Ar, 0xE, 2)
%% remu(a3, a4, a5): (2<<20)|(0xE<<16)|(3<<12)|(4<<8)|(5<<4)|0 = 0x2E3450
remu_test_() ->
    [
        ?_assertEqual(<<16#2E3450:24/little>>, jit_xtensa_asm:remu(a3, a4, a5))
    ].

%%=============================================================================
%% Immediate Arithmetic Instruction Tests
%%=============================================================================

%% addi(At, As, Imm8): encode_rri8(2, At, As, 0xC, Imm8)
%% = ((Imm8&0xFF)<<16)|(0xC<<12)|(As<<8)|(At<<4)|2
%% addi(a3, a4, 10): (10<<16)|(0xC<<12)|(4<<8)|(3<<4)|2 = 0x0AC432
addi_test_() ->
    [
        ?_assertEqual(<<16#0AC432:24/little>>, jit_xtensa_asm:addi(a3, a4, 10)),
        ?_assertEqual(<<16#FFC432:24/little>>, jit_xtensa_asm:addi(a3, a4, -1)),
        ?_assertEqual(<<16#80C432:24/little>>, jit_xtensa_asm:addi(a3, a4, -128)),
        ?_assertEqual(<<16#7FC432:24/little>>, jit_xtensa_asm:addi(a3, a4, 127)),
        ?_assertEqual(<<16#00C002:24/little>>, jit_xtensa_asm:addi(a0, a0, 0))
    ].

%% addmi(At, As, Imm): Imm8 = (Imm >> 8) & 0xFF
%% addmi(a3, a4, 256): Imm8 = 1. encode_rri8(2, 3, 4, 0xD, 1)
%% = (1<<16)|(0xD<<12)|(4<<8)|(3<<4)|2 = 0x01D432
addmi_test_() ->
    [
        ?_assertEqual(<<16#01D432:24/little>>, jit_xtensa_asm:addmi(a3, a4, 256)),
        ?_assertEqual(<<16#FFD432:24/little>>, jit_xtensa_asm:addmi(a3, a4, -256))
    ].

%% movi(At, Imm): Imm12 = Imm & 0xFFF, Low8 = Imm12 & 0xFF, Hi4 = (Imm12>>8) & 0xF
%% encode_rri8(2, At, Hi4, 0xA, Low8)
%% movi(a3, 0): Imm12=0, Low8=0, Hi4=0 -> (0<<16)|(0xA<<12)|(0<<8)|(3<<4)|2 = 0x00A032
%% movi(a3, 100): Imm12=0x64, Low8=0x64, Hi4=0 -> (0x64<<16)|(0xA<<12)|(0<<8)|(3<<4)|2 = 0x64A032
%% movi(a3, -1): Imm12=0xFFF, Low8=0xFF, Hi4=0xF -> (0xFF<<16)|(0xA<<12)|(0xF<<8)|(3<<4)|2 = 0xFFAF32
%% movi(a3, 2047): Imm12=0x7FF, Low8=0xFF, Hi4=7 -> (0xFF<<16)|(0xA<<12)|(7<<8)|(3<<4)|2 = 0xFFA732
%% movi(a3, -2048): Imm12=(-2048)&0xFFF=0x800, Low8=0, Hi4=8 -> (0<<16)|(0xA<<12)|(8<<8)|(3<<4)|2 = 0x00A832
movi_test_() ->
    [
        ?_assertEqual(<<16#00A032:24/little>>, jit_xtensa_asm:movi(a3, 0)),
        ?_assertEqual(<<16#64A032:24/little>>, jit_xtensa_asm:movi(a3, 100)),
        ?_assertEqual(<<16#FFAF32:24/little>>, jit_xtensa_asm:movi(a3, -1)),
        ?_assertEqual(<<16#FFA732:24/little>>, jit_xtensa_asm:movi(a3, 2047)),
        ?_assertEqual(<<16#00A832:24/little>>, jit_xtensa_asm:movi(a3, -2048))
    ].

%%=============================================================================
%% Load Instruction Tests
%%=============================================================================

%% l32i(At, As, Offset): encode_rri8(2, At, As, 2, Offset/4)
%% l32i(a3, a4, 0): (0<<16)|(2<<12)|(4<<8)|(3<<4)|2 = 0x002432
%% l32i(a3, a4, 8): Offset/4=2 -> (2<<16)|(2<<12)|(4<<8)|(3<<4)|2 = 0x022432
%% l32i(a3, a4, 1020): Offset/4=255 -> (0xFF<<16)|(2<<12)|(4<<8)|(3<<4)|2 = 0xFF2432
l32i_test_() ->
    [
        ?_assertEqual(<<16#002432:24/little>>, jit_xtensa_asm:l32i(a3, a4, 0)),
        ?_assertEqual(<<16#022432:24/little>>, jit_xtensa_asm:l32i(a3, a4, 8)),
        ?_assertEqual(<<16#FF2432:24/little>>, jit_xtensa_asm:l32i(a3, a4, 1020))
    ].

%% s32i(At, As, Offset): encode_rri8(2, At, As, 6, Offset/4)
%% s32i(a3, a4, 8): Offset/4=2 -> (2<<16)|(6<<12)|(4<<8)|(3<<4)|2 = 0x026432
s32i_test_() ->
    [
        ?_assertEqual(<<16#006432:24/little>>, jit_xtensa_asm:s32i(a3, a4, 0)),
        ?_assertEqual(<<16#026432:24/little>>, jit_xtensa_asm:s32i(a3, a4, 8)),
        ?_assertEqual(<<16#FF6432:24/little>>, jit_xtensa_asm:s32i(a3, a4, 1020))
    ].

%% l8ui(At, As, Offset): encode_rri8(2, At, As, 0, Offset)
%% l8ui(a3, a4, 10): (10<<16)|(0<<12)|(4<<8)|(3<<4)|2 = 0x0A0432
l8ui_test_() ->
    [
        ?_assertEqual(<<16#000432:24/little>>, jit_xtensa_asm:l8ui(a3, a4, 0)),
        ?_assertEqual(<<16#0A0432:24/little>>, jit_xtensa_asm:l8ui(a3, a4, 10)),
        ?_assertEqual(<<16#FF0432:24/little>>, jit_xtensa_asm:l8ui(a3, a4, 255))
    ].

%% l16ui(At, As, Offset): encode_rri8(2, At, As, 1, Offset/2)
%% l16ui(a3, a4, 4): Offset/2=2 -> (2<<16)|(1<<12)|(4<<8)|(3<<4)|2 = 0x021432
l16ui_test_() ->
    [
        ?_assertEqual(<<16#001432:24/little>>, jit_xtensa_asm:l16ui(a3, a4, 0)),
        ?_assertEqual(<<16#021432:24/little>>, jit_xtensa_asm:l16ui(a3, a4, 4)),
        ?_assertEqual(<<16#FF1432:24/little>>, jit_xtensa_asm:l16ui(a3, a4, 510))
    ].

%% l16si(At, As, Offset): encode_rri8(2, At, As, 9, Offset/2)
%% l16si(a3, a4, 4): Offset/2=2 -> (2<<16)|(9<<12)|(4<<8)|(3<<4)|2 = 0x029432
l16si_test_() ->
    [
        ?_assertEqual(<<16#009432:24/little>>, jit_xtensa_asm:l16si(a3, a4, 0)),
        ?_assertEqual(<<16#029432:24/little>>, jit_xtensa_asm:l16si(a3, a4, 4))
    ].

%% s8i(At, As, Offset): encode_rri8(2, At, As, 4, Offset)
%% s8i(a3, a4, 10): (10<<16)|(4<<12)|(4<<8)|(3<<4)|2 = 0x0A4432
s8i_test_() ->
    [
        ?_assertEqual(<<16#004432:24/little>>, jit_xtensa_asm:s8i(a3, a4, 0)),
        ?_assertEqual(<<16#0A4432:24/little>>, jit_xtensa_asm:s8i(a3, a4, 10))
    ].

%% s16i(At, As, Offset): encode_rri8(2, At, As, 5, Offset/2)
%% s16i(a3, a4, 4): Offset/2=2 -> (2<<16)|(5<<12)|(4<<8)|(3<<4)|2 = 0x025432
s16i_test_() ->
    [
        ?_assertEqual(<<16#005432:24/little>>, jit_xtensa_asm:s16i(a3, a4, 0)),
        ?_assertEqual(<<16#025432:24/little>>, jit_xtensa_asm:s16i(a3, a4, 4))
    ].

%%=============================================================================
%% Load/Store Tuple Form Tests
%%=============================================================================

%% Verify that the 2-arg tuple form produces the same result as the 3-arg form.
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
%%=============================================================================

%% beq(As, At, Offset): encode_bri8(7, At, As, 1, Offset)
%% beq(a3, a4, 8): (8<<16)|(1<<12)|(3<<8)|(4<<4)|7 = 0x081347
%% beq(a3, a4, 0): (0<<16)|(1<<12)|(3<<8)|(4<<4)|7 = 0x001347
beq_test_() ->
    [
        ?_assertEqual(<<16#081347:24/little>>, jit_xtensa_asm:beq(a3, a4, 8)),
        ?_assertEqual(<<16#001347:24/little>>, jit_xtensa_asm:beq(a3, a4, 0)),
        ?_assertEqual(<<16#FC1347:24/little>>, jit_xtensa_asm:beq(a3, a4, -4))
    ].

%% bne(As, At, Offset): encode_bri8(7, At, As, 9, Offset)
%% bne(a3, a4, 0): (0<<16)|(9<<12)|(3<<8)|(4<<4)|7 = 0x009347
bne_test_() ->
    [
        ?_assertEqual(<<16#009347:24/little>>, jit_xtensa_asm:bne(a3, a4, 0)),
        ?_assertEqual(<<16#089347:24/little>>, jit_xtensa_asm:bne(a3, a4, 8))
    ].

%% blt(As, At, Offset): encode_bri8(7, At, As, 2, Offset)
%% blt(a3, a4, 0): (0<<16)|(2<<12)|(3<<8)|(4<<4)|7 = 0x002347
blt_test_() ->
    [
        ?_assertEqual(<<16#002347:24/little>>, jit_xtensa_asm:blt(a3, a4, 0))
    ].

%% bge(As, At, Offset): encode_bri8(7, At, As, 0xA, Offset)
%% bge(a3, a4, 0): (0<<16)|(0xA<<12)|(3<<8)|(4<<4)|7 = 0x00A347
bge_test_() ->
    [
        ?_assertEqual(<<16#00A347:24/little>>, jit_xtensa_asm:bge(a3, a4, 0))
    ].

%% bltu(As, At, Offset): encode_bri8(7, At, As, 3, Offset)
%% bltu(a3, a4, 0): (0<<16)|(3<<12)|(3<<8)|(4<<4)|7 = 0x003347
bltu_test_() ->
    [
        ?_assertEqual(<<16#003347:24/little>>, jit_xtensa_asm:bltu(a3, a4, 0))
    ].

%% bgeu(As, At, Offset): encode_bri8(7, At, As, 0xB, Offset)
%% bgeu(a3, a4, 0): (0<<16)|(0xB<<12)|(3<<8)|(4<<4)|7 = 0x00B347
bgeu_test_() ->
    [
        ?_assertEqual(<<16#00B347:24/little>>, jit_xtensa_asm:bgeu(a3, a4, 0))
    ].

%%=============================================================================
%% Branch-vs-Immediate Instruction Tests
%%=============================================================================

%% beqi(As, Imm, Offset): R = b4const_encode(Imm), encode_bri8(7, 2, As, R, Offset)
%% beqi(a3, 1, 0): R=1 -> (0<<16)|(1<<12)|(3<<8)|(2<<4)|7 = 0x001327
beqi_test_() ->
    [
        ?_assertEqual(<<16#001327:24/little>>, jit_xtensa_asm:beqi(a3, 1, 0)),
        ?_assertEqual(<<16#000327:24/little>>, jit_xtensa_asm:beqi(a3, -1, 0))
    ].

%% bnei(As, Imm, Offset): R = b4const_encode(Imm), encode_bri8(7, 6, As, R, Offset)
%% bnei(a3, 1, 0): R=1 -> (0<<16)|(1<<12)|(3<<8)|(6<<4)|7 = 0x001367
bnei_test_() ->
    [
        ?_assertEqual(<<16#001367:24/little>>, jit_xtensa_asm:bnei(a3, 1, 0))
    ].

%% blti(As, Imm, Offset): R = b4const_encode(Imm), encode_bri8(7, 3, As, R, Offset)
%% blti(a3, 1, 0): R=1 -> (0<<16)|(1<<12)|(3<<8)|(3<<4)|7 = 0x001337
blti_test_() ->
    [
        ?_assertEqual(<<16#001337:24/little>>, jit_xtensa_asm:blti(a3, 1, 0))
    ].

%% bgei(As, Imm, Offset): R = b4const_encode(Imm), encode_bri8(7, 0xB, As, R, Offset)
%% bgei(a3, 1, 0): R=1 -> (0<<16)|(1<<12)|(3<<8)|(0xB<<4)|7 = 0x0013B7
bgei_test_() ->
    [
        ?_assertEqual(<<16#0013B7:24/little>>, jit_xtensa_asm:bgei(a3, 1, 0))
    ].

%% bltui(As, Imm, Offset): R = b4constu_encode(Imm), encode_bri8(7, 7, As, R, Offset)
%% bltui(a3, 4, 0): R=4 -> (0<<16)|(4<<12)|(3<<8)|(7<<4)|7 = 0x004377
bltui_test_() ->
    [
        ?_assertEqual(<<16#004377:24/little>>, jit_xtensa_asm:bltui(a3, 4, 0))
    ].

%% bgeui(As, Imm, Offset): R = b4constu_encode(Imm), encode_bri8(7, 0xF, As, R, Offset)
%% bgeui(a3, 4, 0): R=4 -> (0<<16)|(4<<12)|(3<<8)|(0xF<<4)|7 = 0x0043F7
bgeui_test_() ->
    [
        ?_assertEqual(<<16#0043F7:24/little>>, jit_xtensa_asm:bgeui(a3, 4, 0))
    ].

%%=============================================================================
%% Branch-Zero Instruction Tests (BRI12 format)
%%=============================================================================

%% beqz(As, Offset): encode_bri12(6, 1, As, Offset & 0xFFF)
%% = ((Offset&0xFFF)<<12)|(As<<8)|(1<<4)|6
%% beqz(a3, 0): (0<<12)|(3<<8)|(1<<4)|6 = 0x000316
beqz_test_() ->
    [
        ?_assertEqual(<<16#000316:24/little>>, jit_xtensa_asm:beqz(a3, 0)),
        ?_assertEqual(<<16#07F316:24/little>>, jit_xtensa_asm:beqz(a3, 127))
    ].

%% bnez(As, Offset): encode_bri12(6, 5, As, Offset & 0xFFF)
%% bnez(a3, 0): (0<<12)|(3<<8)|(5<<4)|6 = 0x000356
bnez_test_() ->
    [
        ?_assertEqual(<<16#000356:24/little>>, jit_xtensa_asm:bnez(a3, 0)),
        ?_assertEqual(<<16#001356:24/little>>, jit_xtensa_asm:bnez(a3, 1))
    ].

%% bgez(As, Offset): encode_bri12(6, 0xD, As, Offset & 0xFFF)
%% bgez(a3, 0): (0<<12)|(3<<8)|(0xD<<4)|6 = 0x0003D6
bgez_test_() ->
    [
        ?_assertEqual(<<16#0003D6:24/little>>, jit_xtensa_asm:bgez(a3, 0))
    ].

%% bltz(As, Offset): encode_bri12(6, 9, As, Offset & 0xFFF)
%% bltz(a3, 0): (0<<12)|(3<<8)|(9<<4)|6 = 0x000396
bltz_test_() ->
    [
        ?_assertEqual(<<16#000396:24/little>>, jit_xtensa_asm:bltz(a3, 0))
    ].

%%=============================================================================
%% Bit Test Branch Instruction Tests
%%=============================================================================

%% bbci(As, Bit, Offset): T = if Bit>=16 -> 8; true -> 0 end
%% encode_bri8(7, T, As, Bit&0xF, Offset)
%% bbci(a3, 0, 0): T=0 -> (0<<16)|(0<<12)|(3<<8)|(0<<4)|7 = 0x000307
%% bbci(a3, 16, 0): T=8 -> (0<<16)|(0<<12)|(3<<8)|(8<<4)|7 = 0x000387
bbci_test_() ->
    [
        ?_assertEqual(<<16#000307:24/little>>, jit_xtensa_asm:bbci(a3, 0, 0)),
        ?_assertEqual(<<16#00F307:24/little>>, jit_xtensa_asm:bbci(a3, 15, 0)),
        ?_assertEqual(<<16#000387:24/little>>, jit_xtensa_asm:bbci(a3, 16, 0)),
        ?_assertEqual(<<16#00F387:24/little>>, jit_xtensa_asm:bbci(a3, 31, 0))
    ].

%% bbsi(As, Bit, Offset): T = if Bit>=16 -> 0xC; true -> 4 end
%% encode_bri8(7, T, As, Bit&0xF, Offset)
%% bbsi(a3, 0, 0): T=4 -> (0<<16)|(0<<12)|(3<<8)|(4<<4)|7 = 0x000347
%% bbsi(a3, 16, 0): T=0xC -> (0<<16)|(0<<12)|(3<<8)|(0xC<<4)|7 = 0x0003C7
bbsi_test_() ->
    [
        ?_assertEqual(<<16#000347:24/little>>, jit_xtensa_asm:bbsi(a3, 0, 0)),
        ?_assertEqual(<<16#0003C7:24/little>>, jit_xtensa_asm:bbsi(a3, 16, 0)),
        ?_assertEqual(<<16#00F3C7:24/little>>, jit_xtensa_asm:bbsi(a3, 31, 0))
    ].

%%=============================================================================
%% Jump Instruction Tests
%%=============================================================================

%% j(Offset): encode_call(6, 0, Offset & 0x3FFFF)
%% = ((Offset&0x3FFFF)<<6)|(0<<4)|6
%% j(0): 6 = 0x000006
%% j(100): (100<<6)|6 = (0x64<<6)|6 = 0x1900|6 = 0x001906
j_test_() ->
    [
        ?_assertEqual(<<16#000006:24/little>>, jit_xtensa_asm:j(0)),
        ?_assertEqual(<<16#001906:24/little>>, jit_xtensa_asm:j(100))
    ].

%% jx(As): encode_rrr(0, 0xA, 0, As, 0, 0)
%% jx(a3): (0<<20)|(0<<16)|(3<<12)|(0<<8)|(0xA<<4)|0 = 0x0030A0
jx_test_() ->
    [
        ?_assertEqual(<<16#0030A0:24/little>>, jit_xtensa_asm:jx(a3)),
        ?_assertEqual(<<16#0000A0:24/little>>, jit_xtensa_asm:jx(a0))
    ].

%% call0(Offset): Offset18 = (Offset >> 2) & 0x3FFFF, encode_call(5, 0, Offset18)
%% call0(0): encode_call(5, 0, 0) = 5 = 0x000005
%% call0(4): Offset18 = 1, encode_call(5, 0, 1) = (1<<6)|5 = 0x45 = 0x000045
call0_test_() ->
    [
        ?_assertEqual(<<16#000005:24/little>>, jit_xtensa_asm:call0(0)),
        ?_assertEqual(<<16#000045:24/little>>, jit_xtensa_asm:call0(4))
    ].

%% callx0(As): encode_rrr(0, 0xC, 0, As, 0, 0)
%% callx0(a3): (0<<20)|(0<<16)|(3<<12)|(0<<8)|(0xC<<4)|0 = 0x0030C0
callx0_test_() ->
    [
        ?_assertEqual(<<16#0030C0:24/little>>, jit_xtensa_asm:callx0(a3)),
        ?_assertEqual(<<16#0000C0:24/little>>, jit_xtensa_asm:callx0(a0))
    ].

%% ret(): encode_rrr(0, 8, 0, 0, 0, 0)
%% (0<<20)|(0<<16)|(0<<12)|(0<<8)|(8<<4)|0 = 0x000080
ret_test_() ->
    [
        ?_assertEqual(<<16#000080:24/little>>, jit_xtensa_asm:ret())
    ].

%% ret_n(): hardcoded <<0x0D, 0xF0>>
ret_n_test_() ->
    [
        ?_assertEqual(<<16#0D, 16#F0>>, jit_xtensa_asm:ret_n())
    ].

%%=============================================================================
%% Narrow (Density) Instruction Tests - 16-bit
%%=============================================================================

%% add_n(Ar, As, At): encode_rrrn(0xC, At, As, Ar)
%% = (Ar<<12)|(As<<8)|(At<<4)|0xC
%% add_n(a3, a4, a5): (3<<12)|(4<<8)|(5<<4)|0xC = 0x345C
add_n_test_() ->
    [
        ?_assertEqual(<<16#345C:16/little>>, jit_xtensa_asm:add_n(a3, a4, a5)),
        ?_assertEqual(<<16#000C:16/little>>, jit_xtensa_asm:add_n(a0, a0, a0))
    ].

%% addi_n(Ar, As, Imm): for Imm=-1: encode_rrrn(0xB, 0, As, Ar)
%%                       for Imm>=1: encode_rrrn(0xB, Imm, As, Ar)
%% addi_n(a3, a4, 5): (3<<12)|(4<<8)|(5<<4)|0xB = 0x345B
%% addi_n(a3, a4, -1): (3<<12)|(4<<8)|(0<<4)|0xB = 0x340B
addi_n_test_() ->
    [
        ?_assertEqual(<<16#345B:16/little>>, jit_xtensa_asm:addi_n(a3, a4, 5)),
        ?_assertEqual(<<16#340B:16/little>>, jit_xtensa_asm:addi_n(a3, a4, -1)),
        ?_assertEqual(<<16#341B:16/little>>, jit_xtensa_asm:addi_n(a3, a4, 1)),
        ?_assertEqual(<<16#34FB:16/little>>, jit_xtensa_asm:addi_n(a3, a4, 15))
    ].

%% mov_n(At, As): encode_rrrn(0xD, At, As, 0)
%% mov_n(a3, a4): (0<<12)|(4<<8)|(3<<4)|0xD = 0x043D
mov_n_test_() ->
    [
        ?_assertEqual(<<16#043D:16/little>>, jit_xtensa_asm:mov_n(a3, a4)),
        ?_assertEqual(<<16#000D:16/little>>, jit_xtensa_asm:mov_n(a0, a0))
    ].

%% l32i_n(At, As, Offset): encode_ri7n(8, At, As, Offset/4)
%% = ((Offset/4)<<12)|(As<<8)|(At<<4)|8
%% l32i_n(a3, a4, 8): Offset/4=2 -> (2<<12)|(4<<8)|(3<<4)|8 = 0x2438
l32i_n_test_() ->
    [
        ?_assertEqual(<<16#0438:16/little>>, jit_xtensa_asm:l32i_n(a3, a4, 0)),
        ?_assertEqual(<<16#2438:16/little>>, jit_xtensa_asm:l32i_n(a3, a4, 8)),
        ?_assertEqual(<<16#F438:16/little>>, jit_xtensa_asm:l32i_n(a3, a4, 60))
    ].

%% s32i_n(At, As, Offset): encode_ri7n(9, At, As, Offset/4)
%% s32i_n(a3, a4, 8): Offset/4=2 -> (2<<12)|(4<<8)|(3<<4)|9 = 0x2439
s32i_n_test_() ->
    [
        ?_assertEqual(<<16#0439:16/little>>, jit_xtensa_asm:s32i_n(a3, a4, 0)),
        ?_assertEqual(<<16#2439:16/little>>, jit_xtensa_asm:s32i_n(a3, a4, 8)),
        ?_assertEqual(<<16#F439:16/little>>, jit_xtensa_asm:s32i_n(a3, a4, 60))
    ].

%% l32i_n/s32i_n tuple form
l32i_n_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:l32i_n(a3, a4, 8), jit_xtensa_asm:l32i_n(a3, {a4, 8}))
    ].

s32i_n_tuple_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:s32i_n(a3, a4, 8), jit_xtensa_asm:s32i_n(a3, {a4, 8}))
    ].

%% movi_n(As, Imm): Imm7 = Imm & 0x7F, Low4 = Imm7 & 0xF, Hi3 = (Imm7>>4) & 7
%% Instr = (Hi3<<12)|(Low4<<8)|(As<<4)|0xC
%% movi_n(a3, 0): Imm7=0, Low4=0, Hi3=0 -> (0<<12)|(0<<8)|(3<<4)|0xC = 0x003C
%% movi_n(a3, 95): Imm7=95=0x5F, Low4=0xF, Hi3=5 -> (5<<12)|(0xF<<8)|(3<<4)|0xC = 0x5F3C
%% movi_n(a3, -32): Imm7=(-32)&0x7F=0x60, Low4=0, Hi3=6 -> (6<<12)|(0<<8)|(3<<4)|0xC = 0x603C
movi_n_test_() ->
    [
        ?_assertEqual(<<16#003C:16/little>>, jit_xtensa_asm:movi_n(a3, 0)),
        ?_assertEqual(<<16#5F3C:16/little>>, jit_xtensa_asm:movi_n(a3, 95)),
        ?_assertEqual(<<16#603C:16/little>>, jit_xtensa_asm:movi_n(a3, -32)),
        ?_assertEqual(<<16#013C:16/little>>, jit_xtensa_asm:movi_n(a3, 1))
    ].

%% beqz_n(As, Offset): Imm30=Offset&0xF, Imm54=(Offset>>4)&3
%% Instr = ((8|Imm54)<<12)|(Imm30<<8)|(As<<4)|0xC
%% beqz_n(a3, 0): ((8|0)<<12)|(0<<8)|(3<<4)|0xC = (8<<12)|0x3C = 0x803C
beqz_n_test_() ->
    [
        ?_assertEqual(<<16#803C:16/little>>, jit_xtensa_asm:beqz_n(a3, 0)),
        ?_assertEqual(<<16#8F3C:16/little>>, jit_xtensa_asm:beqz_n(a3, 15)),
        ?_assertEqual(<<16#903C:16/little>>, jit_xtensa_asm:beqz_n(a3, 16)),
        ?_assertEqual(<<16#BF3C:16/little>>, jit_xtensa_asm:beqz_n(a3, 63))
    ].

%% bnez_n(As, Offset): Imm30=Offset&0xF, Imm54=(Offset>>4)&3
%% Instr = ((0xC|Imm54)<<12)|(Imm30<<8)|(As<<4)|0xC
%% bnez_n(a3, 0): ((0xC|0)<<12)|(0<<8)|(3<<4)|0xC = (0xC<<12)|0x3C = 0xC03C
bnez_n_test_() ->
    [
        ?_assertEqual(<<16#C03C:16/little>>, jit_xtensa_asm:bnez_n(a3, 0)),
        ?_assertEqual(<<16#CF3C:16/little>>, jit_xtensa_asm:bnez_n(a3, 15)),
        ?_assertEqual(<<16#D03C:16/little>>, jit_xtensa_asm:bnez_n(a3, 16)),
        ?_assertEqual(<<16#FF3C:16/little>>, jit_xtensa_asm:bnez_n(a3, 63))
    ].

%% nop_n(): hardcoded <<0x3D, 0xF0>>
nop_n_test_() ->
    [
        ?_assertEqual(<<16#3D, 16#F0>>, jit_xtensa_asm:nop_n())
    ].

%% break_n(S, T): (0xF<<12)|((S&0xF)<<8)|((T&0xF)<<4)|0xD
%% break_n(0, 0): (0xF<<12)|(0<<8)|(0<<4)|0xD = 0xF00D
break_n_test_() ->
    [
        ?_assertEqual(<<16#F00D:16/little>>, jit_xtensa_asm:break_n(0, 0)),
        ?_assertEqual(<<16#F12D:16/little>>, jit_xtensa_asm:break_n(1, 2))
    ].

%%=============================================================================
%% L32R and Misc Instruction Tests
%%=============================================================================

%% l32r(At, Imm16): encode_ri16(1, At, Imm16)
%% = ((Imm16&0xFFFF)<<8)|(At<<4)|1
%% l32r(a3, 0): (0<<8)|(3<<4)|1 = 0x000031
l32r_test_() ->
    [
        ?_assertEqual(<<16#000031:24/little>>, jit_xtensa_asm:l32r(a3, 0)),
        ?_assertEqual(<<16#FFFF31:24/little>>, jit_xtensa_asm:l32r(a3, 16#FFFF))
    ].

%% nop(): or_(a1, a1, a1)
%% encode_rrr(0, 1, 1, 1, 2, 0) = (0<<20)|(2<<16)|(1<<12)|(1<<8)|(1<<4)|0 = 0x021110
nop_test_() ->
    [
        ?_assertEqual(<<16#021110:24/little>>, jit_xtensa_asm:nop()),
        %% Verify nop is the same as or_(a1, a1, a1)
        ?_assertEqual(jit_xtensa_asm:or_(a1, a1, a1), jit_xtensa_asm:nop())
    ].

%% break(S, T): encode_rrr(0, T&0xF, S&0xF, 4, 0, 0)
%% break(1, 2): (0<<20)|(0<<16)|(4<<12)|(1<<8)|(2<<4)|0 = 0x004120
break_test_() ->
    [
        ?_assertEqual(<<16#004120:24/little>>, jit_xtensa_asm:break(1, 2)),
        ?_assertEqual(<<16#004000:24/little>>, jit_xtensa_asm:break(0, 0))
    ].

%% memw(): encode_rrr(0, 0xC, 0, 0, 0, 2)
%% (2<<20)|(0<<16)|(0<<12)|(0<<8)|(0xC<<4)|0 = 0x2000C0
memw_test_() ->
    [
        ?_assertEqual(<<16#2000C0:24/little>>, jit_xtensa_asm:memw())
    ].

%% isync(): encode_rrr(0, 0, 0, 0, 0, 2)
%% (2<<20)|(0<<16)|(0<<12)|(0<<8)|(0<<4)|0 = 0x200000
isync_test_() ->
    [
        ?_assertEqual(<<16#200000:24/little>>, jit_xtensa_asm:isync())
    ].

%% dsync(): encode_rrr(0, 3, 0, 0, 0, 2)
%% (2<<20)|(0<<16)|(0<<12)|(0<<8)|(3<<4)|0 = 0x200030
dsync_test_() ->
    [
        ?_assertEqual(<<16#200030:24/little>>, jit_xtensa_asm:dsync())
    ].

%%=============================================================================
%% Pseudo-instruction Tests
%%=============================================================================

%% mv(Dst, Src): mov_n(Dst, Src) (narrow 2-byte move)
mv_test_() ->
    [
        ?_assertEqual(jit_xtensa_asm:mov_n(a3, a4), jit_xtensa_asm:mv(a3, a4)),
        ?_assertEqual(2, byte_size(jit_xtensa_asm:mv(a3, a4)))
    ].

%% neg(Ar, At): encode_rrr(0, At, 0, Ar, 0, 6)
%% neg(a3, a5): (6<<20)|(0<<16)|(3<<12)|(0<<8)|(5<<4)|0 = 0x603050
neg_test_() ->
    [
        ?_assertEqual(<<16#603050:24/little>>, jit_xtensa_asm:neg(a3, a5)),
        ?_assertEqual(<<16#600000:24/little>>, jit_xtensa_asm:neg(a0, a0))
    ].

%% not_(Ar, As) when Ar =/= As: movi(Ar, -1) ++ xor_(Ar, Ar, As)
%% not_(Ar, As) when Ar =:= As: neg(Ar, As) ++ addi(Ar, Ar, -1)
not_test_() ->
    [
        %% not_(a3, a4): movi(a3, -1) ++ xor_(a3, a3, a4) = 6 bytes
        ?_assertEqual(
            <<(jit_xtensa_asm:movi(a3, -1))/binary, (jit_xtensa_asm:xor_(a3, a3, a4))/binary>>,
            jit_xtensa_asm:not_(a3, a4)
        ),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:not_(a3, a4))),
        %% not_(a3, a3): neg(a3, a3) ++ addi(a3, a3, -1) = 6 bytes
        ?_assertEqual(
            <<(jit_xtensa_asm:neg(a3, a3))/binary, (jit_xtensa_asm:addi(a3, a3, -1))/binary>>,
            jit_xtensa_asm:not_(a3, a3)
        ),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:not_(a3, a3)))
    ].

%% li(At, Imm): uses movi for -2048..2047, movi+addmi for -32768..32767,
%%              and li_large for arbitrary 32-bit values.
li_test_() ->
    [
        %% Small values: single movi instruction (3 bytes)
        ?_assertEqual(jit_xtensa_asm:movi(a3, 0), jit_xtensa_asm:li(a3, 0)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, 100), jit_xtensa_asm:li(a3, 100)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, -1), jit_xtensa_asm:li(a3, -1)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, 2047), jit_xtensa_asm:li(a3, 2047)),
        ?_assertEqual(jit_xtensa_asm:movi(a3, -2048), jit_xtensa_asm:li(a3, -2048)),
        ?_assertEqual(3, byte_size(jit_xtensa_asm:li(a3, 0))),
        %% Medium values: movi + addmi (6 bytes)
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, 4096))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, -4096))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, 32767))),
        ?_assertEqual(6, byte_size(jit_xtensa_asm:li(a3, -32768))),
        %% Large values: multi-instruction sequence (movi + 3*(slli+addi) = 21 bytes)
        ?_assertEqual(21, byte_size(jit_xtensa_asm:li(a3, 16#12345678)))
    ].
