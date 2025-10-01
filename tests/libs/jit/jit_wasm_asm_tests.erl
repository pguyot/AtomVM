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

-module(jit_wasm_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(wasm, Bin, Str), Value)
).

%% Test LEB128 encoding
encode_uleb128_test_() ->
    [
        ?_assertEqual(<<0>>, jit_wasm_asm:encode_uleb128(0)),
        ?_assertEqual(<<1>>, jit_wasm_asm:encode_uleb128(1)),
        ?_assertEqual(<<42>>, jit_wasm_asm:encode_uleb128(42)),
        ?_assertEqual(<<127>>, jit_wasm_asm:encode_uleb128(127)),
        ?_assertEqual(<<16#80, 16#01>>, jit_wasm_asm:encode_uleb128(128)),
        ?_assertEqual(<<16#FF, 16#01>>, jit_wasm_asm:encode_uleb128(255)),
        ?_assertEqual(<<16#80, 16#02>>, jit_wasm_asm:encode_uleb128(256)),
        ?_assertEqual(<<16#E5, 16#8E, 16#26>>, jit_wasm_asm:encode_uleb128(624485))
    ].

encode_sleb128_test_() ->
    [
        ?_assertEqual(<<0>>, jit_wasm_asm:encode_sleb128(0)),
        ?_assertEqual(<<1>>, jit_wasm_asm:encode_sleb128(1)),
        ?_assertEqual(<<42>>, jit_wasm_asm:encode_sleb128(42)),
        ?_assertEqual(<<16#7F>>, jit_wasm_asm:encode_sleb128(-1)),
        ?_assertEqual(<<16#7E>>, jit_wasm_asm:encode_sleb128(-2)),
        ?_assertEqual(<<16#40>>, jit_wasm_asm:encode_sleb128(-64)),
        ?_assertEqual(<<16#BF, 16#7F>>, jit_wasm_asm:encode_sleb128(-65)),
        ?_assertEqual(<<16#80, 16#01>>, jit_wasm_asm:encode_sleb128(128)),
        ?_assertEqual(<<16#FF, 16#00>>, jit_wasm_asm:encode_sleb128(127))
    ].

%% Control flow instructions
block_test_() ->
    [
        ?_assertAsmEqual(<<16#02, 16#40>>, "(block)", jit_wasm_asm:block(empty)),
        ?_assertAsmEqual(<<16#02, 16#7F>>, "(block (result i32))", jit_wasm_asm:block(i32)),
        ?_assertAsmEqual(<<16#02, 16#7E>>, "(block (result i64))", jit_wasm_asm:block(i64))
    ].

loop_test_() ->
    [
        ?_assertAsmEqual(<<16#03, 16#40>>, "(loop)", jit_wasm_asm:loop(empty)),
        ?_assertAsmEqual(<<16#03, 16#7F>>, "(loop (result i32))", jit_wasm_asm:loop(i32))
    ].

if_test_() ->
    [
        ?_assertAsmEqual(<<16#04, 16#40>>, "(if)", jit_wasm_asm:if_(empty)),
        ?_assertAsmEqual(<<16#04, 16#7F>>, "(if (result i32))", jit_wasm_asm:if_(i32))
    ].

else_test_() ->
    [
        ?_assertAsmEqual(<<16#05>>, "(else)", jit_wasm_asm:else_())
    ].

end_test_() ->
    [
        ?_assertAsmEqual(<<16#0B>>, "(end)", jit_wasm_asm:end_())
    ].

br_test_() ->
    [
        ?_assertAsmEqual(<<16#0C, 16#00>>, "(br 0)", jit_wasm_asm:br(0)),
        ?_assertAsmEqual(<<16#0C, 16#01>>, "(br 1)", jit_wasm_asm:br(1)),
        ?_assertAsmEqual(<<16#0C, 16#0A>>, "(br 10)", jit_wasm_asm:br(10))
    ].

br_if_test_() ->
    [
        ?_assertAsmEqual(<<16#0D, 16#00>>, "(br_if 0)", jit_wasm_asm:br_if(0)),
        ?_assertAsmEqual(<<16#0D, 16#01>>, "(br_if 1)", jit_wasm_asm:br_if(1))
    ].

return_test_() ->
    [
        ?_assertAsmEqual(<<16#0F>>, "(return)", jit_wasm_asm:return_())
    ].

%% Variable access
local_get_test_() ->
    [
        ?_assertAsmEqual(<<16#20, 16#00>>, "(local.get 0)", jit_wasm_asm:local_get(0)),
        ?_assertAsmEqual(<<16#20, 16#01>>, "(local.get 1)", jit_wasm_asm:local_get(1)),
        ?_assertAsmEqual(<<16#20, 16#0A>>, "(local.get 10)", jit_wasm_asm:local_get(10))
    ].

local_set_test_() ->
    [
        ?_assertAsmEqual(<<16#21, 16#00>>, "(local.set 0)", jit_wasm_asm:local_set(0)),
        ?_assertAsmEqual(<<16#21, 16#01>>, "(local.set 1)", jit_wasm_asm:local_set(1)),
        ?_assertAsmEqual(<<16#21, 16#02>>, "(local.set 2)", jit_wasm_asm:local_set(2))
    ].

local_tee_test_() ->
    [
        ?_assertAsmEqual(<<16#22, 16#00>>, "(local.tee 0)", jit_wasm_asm:local_tee(0)),
        ?_assertAsmEqual(<<16#22, 16#01>>, "(local.tee 1)", jit_wasm_asm:local_tee(1))
    ].

%% Constants
i32_const_test_() ->
    [
        ?_assertAsmEqual(<<16#41, 16#00>>, "(i32.const 0)", jit_wasm_asm:i32_const(0)),
        ?_assertAsmEqual(<<16#41, 16#01>>, "(i32.const 1)", jit_wasm_asm:i32_const(1)),
        ?_assertAsmEqual(<<16#41, 16#2A>>, "(i32.const 42)", jit_wasm_asm:i32_const(42)),
        ?_assertAsmEqual(<<16#41, 16#7F>>, "(i32.const -1)", jit_wasm_asm:i32_const(-1)),
        ?_assertAsmEqual(<<16#41, 16#7E>>, "(i32.const -2)", jit_wasm_asm:i32_const(-2))
    ].

%% Arithmetic
i32_add_test_() ->
    [
        ?_assertAsmEqual(<<16#6A>>, "(i32.add)", jit_wasm_asm:i32_add())
    ].

i32_sub_test_() ->
    [
        ?_assertAsmEqual(<<16#6B>>, "(i32.sub)", jit_wasm_asm:i32_sub())
    ].

i32_mul_test_() ->
    [
        ?_assertAsmEqual(<<16#6C>>, "(i32.mul)", jit_wasm_asm:i32_mul())
    ].

i32_and_test_() ->
    [
        ?_assertAsmEqual(<<16#71>>, "(i32.and)", jit_wasm_asm:i32_and())
    ].

i32_or_test_() ->
    [
        ?_assertAsmEqual(<<16#72>>, "(i32.or)", jit_wasm_asm:i32_or())
    ].

i32_xor_test_() ->
    [
        ?_assertAsmEqual(<<16#73>>, "(i32.xor)", jit_wasm_asm:i32_xor())
    ].

i32_shl_test_() ->
    [
        ?_assertAsmEqual(<<16#74>>, "(i32.shl)", jit_wasm_asm:i32_shl())
    ].

i32_shr_s_test_() ->
    [
        ?_assertAsmEqual(<<16#75>>, "(i32.shr_s)", jit_wasm_asm:i32_shr_s())
    ].

i32_shr_u_test_() ->
    [
        ?_assertAsmEqual(<<16#76>>, "(i32.shr_u)", jit_wasm_asm:i32_shr_u())
    ].

%% Comparison
i32_eqz_test_() ->
    [
        ?_assertAsmEqual(<<16#45>>, "(i32.eqz)", jit_wasm_asm:i32_eqz())
    ].

i32_eq_test_() ->
    [
        ?_assertAsmEqual(<<16#46>>, "(i32.eq)", jit_wasm_asm:i32_eq())
    ].

i32_ne_test_() ->
    [
        ?_assertAsmEqual(<<16#47>>, "(i32.ne)", jit_wasm_asm:i32_ne())
    ].

i32_lt_s_test_() ->
    [
        ?_assertAsmEqual(<<16#48>>, "(i32.lt_s)", jit_wasm_asm:i32_lt_s())
    ].

i32_lt_u_test_() ->
    [
        ?_assertAsmEqual(<<16#49>>, "(i32.lt_u)", jit_wasm_asm:i32_lt_u())
    ].

i32_gt_s_test_() ->
    [
        ?_assertAsmEqual(<<16#4A>>, "(i32.gt_s)", jit_wasm_asm:i32_gt_s())
    ].

i32_gt_u_test_() ->
    [
        ?_assertAsmEqual(<<16#4B>>, "(i32.gt_u)", jit_wasm_asm:i32_gt_u())
    ].

i32_le_s_test_() ->
    [
        ?_assertAsmEqual(<<16#4C>>, "(i32.le_s)", jit_wasm_asm:i32_le_s())
    ].

i32_le_u_test_() ->
    [
        ?_assertAsmEqual(<<16#4D>>, "(i32.le_u)", jit_wasm_asm:i32_le_u())
    ].

i32_ge_s_test_() ->
    [
        ?_assertAsmEqual(<<16#4E>>, "(i32.ge_s)", jit_wasm_asm:i32_ge_s())
    ].

i32_ge_u_test_() ->
    [
        ?_assertAsmEqual(<<16#4F>>, "(i32.ge_u)", jit_wasm_asm:i32_ge_u())
    ].

%% Memory access
i32_load_test_() ->
    [
        ?_assertAsmEqual(
            <<16#28, 16#00, 16#00>>, "(i32.load align=0 offset=0)", jit_wasm_asm:i32_load(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#28, 16#02, 16#00>>, "(i32.load align=2 offset=0)", jit_wasm_asm:i32_load(2, 0)
        ),
        ?_assertAsmEqual(
            <<16#28, 16#02, 16#04>>, "(i32.load align=2 offset=4)", jit_wasm_asm:i32_load(2, 4)
        ),
        ?_assertAsmEqual(
            <<16#28, 16#02, 16#18>>, "(i32.load align=2 offset=24)", jit_wasm_asm:i32_load(2, 24)
        )
    ].

i32_store_test_() ->
    [
        ?_assertAsmEqual(
            <<16#36, 16#00, 16#00>>, "(i32.store align=0 offset=0)", jit_wasm_asm:i32_store(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#36, 16#02, 16#00>>, "(i32.store align=2 offset=0)", jit_wasm_asm:i32_store(2, 0)
        ),
        ?_assertAsmEqual(
            <<16#36, 16#02, 16#04>>, "(i32.store align=2 offset=4)", jit_wasm_asm:i32_store(2, 4)
        )
    ].

i32_load8_s_test_() ->
    [
        ?_assertAsmEqual(
            <<16#2C, 16#00, 16#00>>,
            "(i32.load8_s align=0 offset=0)",
            jit_wasm_asm:i32_load8_s(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#2C, 16#00, 16#01>>,
            "(i32.load8_s align=0 offset=1)",
            jit_wasm_asm:i32_load8_s(0, 1)
        )
    ].

i32_load8_u_test_() ->
    [
        ?_assertAsmEqual(
            <<16#2D, 16#00, 16#00>>,
            "(i32.load8_u align=0 offset=0)",
            jit_wasm_asm:i32_load8_u(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#2D, 16#00, 16#01>>,
            "(i32.load8_u align=0 offset=1)",
            jit_wasm_asm:i32_load8_u(0, 1)
        )
    ].

i32_load16_s_test_() ->
    [
        ?_assertAsmEqual(
            <<16#2E, 16#00, 16#00>>,
            "(i32.load16_s align=0 offset=0)",
            jit_wasm_asm:i32_load16_s(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#2E, 16#01, 16#00>>,
            "(i32.load16_s align=1 offset=0)",
            jit_wasm_asm:i32_load16_s(1, 0)
        )
    ].

i32_load16_u_test_() ->
    [
        ?_assertAsmEqual(
            <<16#2F, 16#00, 16#00>>,
            "(i32.load16_u align=0 offset=0)",
            jit_wasm_asm:i32_load16_u(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#2F, 16#01, 16#00>>,
            "(i32.load16_u align=1 offset=0)",
            jit_wasm_asm:i32_load16_u(1, 0)
        )
    ].

i32_store8_test_() ->
    [
        ?_assertAsmEqual(
            <<16#3A, 16#00, 16#00>>, "(i32.store8 align=0 offset=0)", jit_wasm_asm:i32_store8(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#3A, 16#00, 16#01>>, "(i32.store8 align=0 offset=1)", jit_wasm_asm:i32_store8(0, 1)
        )
    ].

i32_store16_test_() ->
    [
        ?_assertAsmEqual(
            <<16#3B, 16#00, 16#00>>,
            "(i32.store16 align=0 offset=0)",
            jit_wasm_asm:i32_store16(0, 0)
        ),
        ?_assertAsmEqual(
            <<16#3B, 16#01, 16#00>>,
            "(i32.store16 align=1 offset=0)",
            jit_wasm_asm:i32_store16(1, 0)
        )
    ].
