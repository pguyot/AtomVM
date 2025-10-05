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

add_test_() ->
    [
        ?_assertAsmEqual(
            <<42, 1>>, "add.n a0, a1, a2", jit_xtensa_asm:add(a0, a1, a2)
        ),
        ?_assertAsmEqual(
            <<90, 52>>, "add.n a3, a4, a5", jit_xtensa_asm:add(a3, a4, a5)
        ),
        ?_assertAsmEqual(
            <<10, 0>>, "add.n a0, a0, a0", jit_xtensa_asm:add(a0, a0, a0)
        )
    ].

addi_test_() ->
    [
        ?_assertAsmEqual(
            <<27, 33>>,
            "addi a2, a1, 1",
            jit_xtensa_asm:addi(a2, a1, 1)
        ),
        ?_assertAsmEqual(
            <<171, 50>>,
            "addi a3, a2, 10",
            jit_xtensa_asm:addi(a3, a2, 10)
        ),
        ?_assertAsmEqual(
            <<11, 67>>,
            "addi a4, a3, -1",
            jit_xtensa_asm:addi(a4, a3, -1)
        ),
        ?_assertAsmEqual(
            <<13, 0>>,
            "addi a0, a0, 0",
            jit_xtensa_asm:addi(a0, a0, 0)
        )
    ].

sub_test_() ->
    [
        ?_assertAsmEqual(
            <<32, 1, 192>>,
            "sub a0, a1, a2",
            jit_xtensa_asm:sub(a0, a1, a2)
        ),
        ?_assertAsmEqual(
            <<80, 52, 192>>,
            "sub a3, a4, a5",
            jit_xtensa_asm:sub(a3, a4, a5)
        )
    ].

mov_test_() ->
    [
        ?_assertAsmEqual(
            <<45, 1>>, "mov.n a2, a1", jit_xtensa_asm:mov(a2, a1)
        ),
        ?_assertAsmEqual(
            <<13, 0>>, "mov.n a0, a0", jit_xtensa_asm:mov(a0, a0)
        ),
        ?_assertAsmEqual(
            <<93, 3>>, "mov.n a5, a3", jit_xtensa_asm:mov(a5, a3)
        )
    ].

movi_test_() ->
    [
        ?_assertAsmEqual(
            <<12, 18>>,
            "movi a2, 1",
            jit_xtensa_asm:movi(a2, 1)
        ),
        ?_assertAsmEqual(
            <<12, 163>>,
            "movi a3, 10",
            jit_xtensa_asm:movi(a3, 10)
        ),
        ?_assertAsmEqual(
            <<12, 4>>,
            "movi a4, 0",
            jit_xtensa_asm:movi(a4, 0)
        ),
        ?_assertAsmEqual(
            <<124, 245>>,
            "movi a5, -1",
            jit_xtensa_asm:movi(a5, -1)
        )
    ].

l32i_test_() ->
    [
        ?_assertAsmEqual(
            <<40, 1>>,
            "l32i a2, a1, 0",
            jit_xtensa_asm:l32i(a2, a1, 0)
        ),
        ?_assertAsmEqual(
            <<56, 18>>,
            "l32i a3, a2, 4",
            jit_xtensa_asm:l32i(a3, a2, 4)
        ),
        ?_assertAsmEqual(
            <<72, 163>>,
            "l32i a4, a3, 40",
            jit_xtensa_asm:l32i(a4, a3, 40)
        )
    ].

s32i_test_() ->
    [
        ?_assertAsmEqual(
            <<41, 1>>,
            "s32i a2, a1, 0",
            jit_xtensa_asm:s32i(a2, a1, 0)
        ),
        ?_assertAsmEqual(
            <<57, 18>>,
            "s32i a3, a2, 4",
            jit_xtensa_asm:s32i(a3, a2, 4)
        ),
        ?_assertAsmEqual(
            <<73, 163>>,
            "s32i a4, a3, 40",
            jit_xtensa_asm:s32i(a4, a3, 40)
        )
    ].

ret_test_() ->
    [
        ?_assertAsmEqual(<<13, 240>>, "ret.n", jit_xtensa_asm:ret())
    ].

retw_test_() ->
    [
        ?_assertAsmEqual(<<29, 240>>, "retw.n", jit_xtensa_asm:retw())
    ].

callx0_test_() ->
    [
        ?_assertAsmEqual(
            <<192, 0, 0>>,
            "callx0 a0",
            jit_xtensa_asm:callx0(a0)
        ),
        ?_assertAsmEqual(
            <<192, 2, 0>>,
            "callx0 a2",
            jit_xtensa_asm:callx0(a2)
        )
    ].

jx_test_() ->
    [
        ?_assertAsmEqual(
            <<160, 0, 0>>,
            "jx a0",
            jit_xtensa_asm:jx(a0)
        ),
        ?_assertAsmEqual(
            <<160, 2, 0>>,
            "jx a2",
            jit_xtensa_asm:jx(a2)
        )
    ].

and_test_() ->
    [
        ?_assertAsmEqual(
            <<32, 1, 16>>,
            "and a0, a1, a2",
            jit_xtensa_asm:and_(a0, a1, a2)
        ),
        ?_assertAsmEqual(
            <<80, 52, 16>>,
            "and a3, a4, a5",
            jit_xtensa_asm:and_(a3, a4, a5)
        )
    ].

or_test_() ->
    [
        ?_assertAsmEqual(
            <<32, 1, 32>>,
            "or a0, a1, a2",
            jit_xtensa_asm:or_(a0, a1, a2)
        ),
        ?_assertAsmEqual(
            <<80, 52, 32>>,
            "or a3, a4, a5",
            jit_xtensa_asm:or_(a3, a4, a5)
        )
    ].

xor_test_() ->
    [
        ?_assertAsmEqual(
            <<32, 1, 48>>,
            "xor a0, a1, a2",
            jit_xtensa_asm:xor_(a0, a1, a2)
        ),
        ?_assertAsmEqual(
            <<80, 52, 48>>,
            "xor a3, a4, a5",
            jit_xtensa_asm:xor_(a3, a4, a5)
        )
    ].

slli_test_() ->
    [
        ?_assertAsmEqual(
            <<240, 1, 17>>,
            "slli a0, a1, 1",
            jit_xtensa_asm:slli(a0, a1, 1)
        ),
        ?_assertAsmEqual(
            <<128, 50, 17>>,
            "slli a3, a2, 8",
            jit_xtensa_asm:slli(a3, a2, 8)
        ),
        ?_assertAsmEqual(
            <<0, 67, 17>>,
            "slli a4, a3, 16",
            jit_xtensa_asm:slli(a4, a3, 16)
        )
    ].

srli_test_() ->
    [
        ?_assertAsmEqual(
            <<16, 1, 65>>,
            "srli a0, a1, 1",
            jit_xtensa_asm:srli(a0, a1, 1)
        ),
        ?_assertAsmEqual(
            <<32, 56, 65>>,
            "srli a3, a2, 8",
            jit_xtensa_asm:srli(a3, a2, 8)
        )
    ].

srai_test_() ->
    [
        ?_assertAsmEqual(
            <<16, 1, 33>>,
            "srai a0, a1, 1",
            jit_xtensa_asm:srai(a0, a1, 1)
        ),
        ?_assertAsmEqual(
            <<32, 56, 33>>,
            "srai a3, a2, 8",
            jit_xtensa_asm:srai(a3, a2, 8)
        ),
        ?_assertAsmEqual(
            <<48, 64, 49>>,
            "srai a4, a3, 16",
            jit_xtensa_asm:srai(a4, a3, 16)
        )
    ].

mull_test_() ->
    [
        ?_assertAsmEqual(
            <<32, 1, 130>>,
            "mull a0, a1, a2",
            jit_xtensa_asm:mull(a0, a1, a2)
        ),
        ?_assertAsmEqual(
            <<80, 52, 130>>,
            "mull a3, a4, a5",
            jit_xtensa_asm:mull(a3, a4, a5)
        )
    ].

beqz_test_() ->
    [
        ?_assertAsmEqual(
            <<22, 194, 255>>,
            "beqz a2, .+0",
            jit_xtensa_asm:beqz(a2, 0)
        ),
        ?_assertAsmEqual(
            <<140, 3>>,
            "beqz a3, .+4",
            jit_xtensa_asm:beqz(a3, 4)
        ),
        ?_assertAsmEqual(
            <<22, 132, 255>>,
            "beqz a4, .-4",
            jit_xtensa_asm:beqz(a4, -4)
        )
    ].

bnez_test_() ->
    [
        ?_assertAsmEqual(
            <<86, 194, 255>>,
            "bnez a2, .+0",
            jit_xtensa_asm:bnez(a2, 0)
        ),
        ?_assertAsmEqual(
            <<204, 3>>,
            "bnez a3, .+4",
            jit_xtensa_asm:bnez(a3, 4)
        ),
        ?_assertAsmEqual(
            <<86, 132, 255>>,
            "bnez a4, .-4",
            jit_xtensa_asm:bnez(a4, -4)
        )
    ].

beq_test_() ->
    [
        ?_assertAsmEqual(
            <<55, 18, 252>>,
            "beq a2, a3, .+0",
            jit_xtensa_asm:beq(a2, a3, 0)
        ),
        ?_assertAsmEqual(
            <<71, 19, 0>>,
            "beq a3, a4, .+4",
            jit_xtensa_asm:beq(a3, a4, 4)
        )
    ].

bne_test_() ->
    [
        ?_assertAsmEqual(
            <<55, 146, 252>>,
            "bne a2, a3, .+0",
            jit_xtensa_asm:bne(a2, a3, 0)
        ),
        ?_assertAsmEqual(
            <<71, 147, 0>>,
            "bne a3, a4, .+4",
            jit_xtensa_asm:bne(a3, a4, 4)
        )
    ].

blt_test_() ->
    [
        ?_assertAsmEqual(
            <<55, 34, 252>>,
            "blt a2, a3, .+0",
            jit_xtensa_asm:blt(a2, a3, 0)
        ),
        ?_assertAsmEqual(
            <<71, 35, 0>>,
            "blt a3, a4, .+4",
            jit_xtensa_asm:blt(a3, a4, 4)
        )
    ].

bge_test_() ->
    [
        ?_assertAsmEqual(
            <<55, 162, 252>>,
            "bge a2, a3, .+0",
            jit_xtensa_asm:bge(a2, a3, 0)
        ),
        ?_assertAsmEqual(
            <<71, 163, 0>>,
            "bge a3, a4, .+4",
            jit_xtensa_asm:bge(a3, a4, 4)
        )
    ].

nop_test_() ->
    [
        ?_assertAsmEqual(<<61, 240>>, "nop.n", jit_xtensa_asm:nop())
    ].
