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

-module(test_inline_arith).

-export([
    start/0,
    add_small/1,
    sub_small/1,
    add_large/1,
    sub_large/1,
    bsl_zero/1,
    bsl_small/1,
    bsr_zero/1,
    bsr_small/1,
    mul_const/1,
    mul_regs/2,
    mul_regs_neg/2,
    square/1,
    div_by_literal/1,
    div_regs/2,
    rem_by_literal/1,
    rem_regs/2,
    div_negative/1,
    rem_negative/1,
    mul_neg_regs/2,
    div_by_pow2/1,
    rem_by_pow2/1,
    div_by_pow2_large/1,
    rem_by_pow2_large/1,
    untyped_add/2,
    untyped_sub/2,
    untyped_mul/2,
    untyped_div_lit/1,
    untyped_rem_lit/1,
    untyped_divrem_dense/1
]).

% Test inline addition with safe ranges - SHOULD BE INLINED
add_small(X) when is_integer(X), X >= 0, X < 100 ->
    X + 20.

% Test inline subtraction with safe ranges - SHOULD BE INLINED
sub_small(X) when is_integer(X), X >= 50, X < 150 ->
    X - 30.

% Test addition with large range - SHOULD NOT BE INLINED (uses BIF)
add_large(X) when is_integer(X), X >= 0, X < (1 bsl 60) ->
    X + 1.

% Test subtraction with large range - SHOULD NOT BE INLINED (uses BIF)
sub_large(X) when is_integer(X), X >= -(1 bsl 60), X < 100 ->
    X - 1.

% Test bsl by 0 - compiler does NOT fold this away, so the JIT inline path handles it
bsl_zero(X) when is_integer(X), X >= -100, X < 100 ->
    X bsl 0.

% Test bsl with non-zero shift amount - exercises the actual shift path
bsl_small(X) when is_integer(X), X >= 0, X < 100 ->
    X bsl 3.

% Test bsr by 0 - compiler does NOT fold this away, so the JIT inline path handles it
% Range must be non-negative for can_inline_bsr to succeed
bsr_zero(X) when is_integer(X), X >= 0, X < 100 ->
    X bsr 0.

% Test bsr with non-zero shift amount - exercises the actual shift path
bsr_small(X) when is_integer(X), X >= 0, X < 1000 ->
    X bsr 2.

% Test multiplication by a literal constant - exercises the typed*literal path
mul_const(X) when is_integer(X), X >= 0, X < 100 ->
    X * 5.

% Test register * register multiplication - both args are typed registers
mul_regs(X, Y) when is_integer(X), X >= 0, X < 100, is_integer(Y), Y >= 0, Y < 100 ->
    X * Y.

% Test squaring (X * X) - both operands are the SAME typed register, which
% exercises the register-aliasing case of the inlined reg*reg multiply.
square(X) when is_integer(X), X >= -1000, X =< 1000 ->
    X * X.

% Test register * register multiplication with negative range
% Exercises the reg*reg path where Arg2 can be negative
mul_regs_neg(X, Y) when is_integer(X), X >= 0, X < 10, is_integer(Y), Y >= -10, Y < 10 ->
    X * Y.

% Test inline div with literal divisor
div_by_literal(X) when is_integer(X), X >= 0, X < 100 ->
    X div 3.

% Test inline div with register operands
div_regs(X, Y) when is_integer(X), X >= 0, X < 100, is_integer(Y), Y >= 1, Y < 50 ->
    X div Y.

% Test inline rem with literal divisor
rem_by_literal(X) when is_integer(X), X >= 0, X < 100 ->
    X rem 3.

% Test inline rem with register operands
rem_regs(X, Y) when is_integer(X), X >= 0, X < 100, is_integer(Y), Y >= 1, Y < 50 ->
    X rem Y.

% Test inline div with negative values
div_negative(X) when is_integer(X), X >= -100, X < 100 ->
    X div 3.

% Test inline rem with negative values
rem_negative(X) when is_integer(X), X >= -100, X < 100 ->
    X rem 3.

% Test multiplication with negative register operands
% This tests correctness of the shift_right used in mul register-register path
mul_neg_regs(X, Y) when is_integer(X), X >= -10, X =< 10, is_integer(Y), Y >= -10, Y =< 10 ->
    X * Y.

% Test inline div by power of 2 - should use shift optimization
div_by_pow2(X) when is_integer(X), X >= 0, X < 100 ->
    X div 4.

% Test inline rem by power of 2 - should use AND optimization
rem_by_pow2(X) when is_integer(X), X >= 0, X < 100 ->
    X rem 8.

% Test div by larger power of 2
div_by_pow2_large(X) when is_integer(X), X >= 0, X < 10000 ->
    X div 256.

% Test rem by larger power of 2
rem_by_pow2_large(X) when is_integer(X), X >= 0, X < 10000 ->
    X rem 256.

% Hide the type of X so the dividend is NOT statically known to be a small
% integer, exercising the runtime small-integer div/rem fast path (the typed
% clauses do not apply).
id(X) -> X.

% Untyped operands (type hidden by id/1), exercising the runtime small-integer
% +/-/* fast paths (the typed clauses do not apply). Two untyped operands force
% the both-small runtime guard; a bignum operand or overflow takes the BIF.
untyped_add(X, Y) -> id(X) + id(Y).
untyped_sub(X, Y) -> id(X) - id(Y).
untyped_mul(X, Y) -> id(X) * id(Y).

untyped_div_lit(X) -> id(X) div 3.
untyped_rem_lit(X) -> id(X) rem 3.

% Dense sequence: a bignum div/rem (taking the runtime fallback) immediately
% followed by a small div whose result is matched without an intervening call.
% This is the shape that previously miscompiled due to the result landing in
% the wrong register; it must produce a correct, matchable result.
untyped_divrem_dense(X) ->
    Big = 999999999999999999999,
    333333333333333333333 = id(Big) div 3,
    0 = id(Big) rem 3,
    VD = id(X) div 3,
    VD = X div 3,
    true = is_integer(VD),
    VD.

start() ->
    % Test safe addition - should be inlined
    20 = ?MODULE:add_small(0),
    50 = ?MODULE:add_small(30),
    119 = ?MODULE:add_small(99),

    % Test safe subtraction - should be inlined
    20 = ?MODULE:sub_small(50),
    70 = ?MODULE:sub_small(100),
    119 = ?MODULE:sub_small(149),

    % Test large addition - not inlined, uses BIF (but should still work correctly)
    % Using values near the upper bound but within the guard range
    1 = ?MODULE:add_large(0),
    100000001 = ?MODULE:add_large(100000000),
    (1 bsl 59) = ?MODULE:add_large((1 bsl 59) - 1),

    % Test large subtraction - not inlined, uses BIF (but should still work correctly)
    % Using values near the lower bound but within the guard range
    -1 = ?MODULE:sub_large(0),
    -100000001 = ?MODULE:sub_large(-100000000),
    -(1 bsl 59) - 1 = ?MODULE:sub_large(-(1 bsl 59)),

    % Test bsl by 0 - exercises JIT inline bsl path with shift amount 0
    0 = ?MODULE:bsl_zero(0),
    42 = ?MODULE:bsl_zero(42),
    -99 = ?MODULE:bsl_zero(-99),

    % Test bsl with non-zero shift amount
    0 = ?MODULE:bsl_small(0),
    8 = ?MODULE:bsl_small(1),
    336 = ?MODULE:bsl_small(42),
    792 = ?MODULE:bsl_small(99),

    % Test bsr by 0 - exercises JIT inline bsr path with shift amount 0
    0 = ?MODULE:bsr_zero(0),
    42 = ?MODULE:bsr_zero(42),
    99 = ?MODULE:bsr_zero(99),

    % Test bsr with non-zero shift amount
    0 = ?MODULE:bsr_small(0),
    0 = ?MODULE:bsr_small(3),
    10 = ?MODULE:bsr_small(42),
    249 = ?MODULE:bsr_small(999),

    % Test multiplication by literal constant
    0 = ?MODULE:mul_const(0),
    5 = ?MODULE:mul_const(1),
    210 = ?MODULE:mul_const(42),
    495 = ?MODULE:mul_const(99),

    % Test register * register multiplication
    0 = ?MODULE:mul_regs(0, 5),
    0 = ?MODULE:mul_regs(5, 0),
    1 = ?MODULE:mul_regs(1, 1),
    42 = ?MODULE:mul_regs(6, 7),
    99 = ?MODULE:mul_regs(99, 1),
    9801 = ?MODULE:mul_regs(99, 99),

    % Test squaring (X * X, same register on both sides of the multiply)
    0 = ?MODULE:square(0),
    1 = ?MODULE:square(1),
    176400 = ?MODULE:square(420),
    900 = ?MODULE:square(-30),
    176400 = ?MODULE:square(-420),
    1000000 = ?MODULE:square(1000),

    % Test register * register multiplication with negative values
    0 = ?MODULE:mul_regs_neg(0, -5),
    -30 = ?MODULE:mul_regs_neg(6, -5),
    -9 = ?MODULE:mul_regs_neg(9, -1),
    45 = ?MODULE:mul_regs_neg(9, 5),

    % Test inline div by literal
    0 = ?MODULE:div_by_literal(0),
    0 = ?MODULE:div_by_literal(2),
    1 = ?MODULE:div_by_literal(3),
    3 = ?MODULE:div_by_literal(10),
    33 = ?MODULE:div_by_literal(99),

    % Test inline div with register operands
    0 = ?MODULE:div_regs(0, 1),
    5 = ?MODULE:div_regs(10, 2),
    3 = ?MODULE:div_regs(10, 3),
    2 = ?MODULE:div_regs(10, 4),
    2 = ?MODULE:div_regs(99, 49),

    % Test inline rem by literal
    0 = ?MODULE:rem_by_literal(0),
    2 = ?MODULE:rem_by_literal(2),
    0 = ?MODULE:rem_by_literal(3),
    1 = ?MODULE:rem_by_literal(10),
    0 = ?MODULE:rem_by_literal(99),

    % Test inline rem with register operands
    0 = ?MODULE:rem_regs(0, 1),
    0 = ?MODULE:rem_regs(10, 2),
    1 = ?MODULE:rem_regs(10, 3),
    2 = ?MODULE:rem_regs(10, 4),
    1 = ?MODULE:rem_regs(99, 49),

    % Test inline div with negative values
    0 = ?MODULE:div_negative(0),
    -1 = ?MODULE:div_negative(-3),
    0 = ?MODULE:div_negative(-2),
    -33 = ?MODULE:div_negative(-99),
    3 = ?MODULE:div_negative(10),
    -3 = ?MODULE:div_negative(-10),

    % Test inline rem with negative values
    0 = ?MODULE:rem_negative(0),
    0 = ?MODULE:rem_negative(-3),
    -2 = ?MODULE:rem_negative(-2),
    0 = ?MODULE:rem_negative(-99),
    1 = ?MODULE:rem_negative(10),
    -1 = ?MODULE:rem_negative(-10),

    % Test multiplication with negative register operands
    0 = ?MODULE:mul_neg_regs(0, 5),
    -50 = ?MODULE:mul_neg_regs(-5, 10),
    50 = ?MODULE:mul_neg_regs(-5, -10),
    100 = ?MODULE:mul_neg_regs(10, 10),
    -100 = ?MODULE:mul_neg_regs(10, -10),
    100 = ?MODULE:mul_neg_regs(-10, -10),

    % Test div by power of 2 (shift optimization)
    0 = ?MODULE:div_by_pow2(0),
    0 = ?MODULE:div_by_pow2(3),
    1 = ?MODULE:div_by_pow2(4),
    1 = ?MODULE:div_by_pow2(7),
    2 = ?MODULE:div_by_pow2(8),
    24 = ?MODULE:div_by_pow2(99),

    % Test rem by power of 2 (AND optimization)
    0 = ?MODULE:rem_by_pow2(0),
    3 = ?MODULE:rem_by_pow2(3),
    0 = ?MODULE:rem_by_pow2(8),
    1 = ?MODULE:rem_by_pow2(9),
    7 = ?MODULE:rem_by_pow2(15),
    3 = ?MODULE:rem_by_pow2(99),

    % Test div by larger power of 2
    0 = ?MODULE:div_by_pow2_large(0),
    0 = ?MODULE:div_by_pow2_large(255),
    1 = ?MODULE:div_by_pow2_large(256),
    3 = ?MODULE:div_by_pow2_large(1000),
    39 = ?MODULE:div_by_pow2_large(9999),

    % Test rem by larger power of 2
    0 = ?MODULE:rem_by_pow2_large(0),
    255 = ?MODULE:rem_by_pow2_large(255),
    0 = ?MODULE:rem_by_pow2_large(256),
    232 = ?MODULE:rem_by_pow2_large(1000),
    15 = ?MODULE:rem_by_pow2_large(9999),

    % Runtime small-integer div/rem fast path with an untyped dividend.
    0 = ?MODULE:untyped_div_lit(2),
    1 = ?MODULE:untyped_div_lit(3),
    33 = ?MODULE:untyped_div_lit(99),
    -1 = ?MODULE:untyped_div_lit(-3),
    -33 = ?MODULE:untyped_div_lit(-100),
    % Bignum dividend takes the runtime fallback.
    333333333333333333333 = ?MODULE:untyped_div_lit(999999999999999999999),
    0 = ?MODULE:untyped_rem_lit(99),
    1 = ?MODULE:untyped_rem_lit(10),
    -1 = ?MODULE:untyped_rem_lit(-10),
    0 = ?MODULE:untyped_rem_lit(999999999999999999999),

    % Regression: bignum div/rem fallback immediately followed by a small div
    % whose result is matched without an intervening call.
    -33 = ?MODULE:untyped_divrem_dense(-100),
    33 = ?MODULE:untyped_divrem_dense(100),
    -192153584101141162 = ?MODULE:untyped_divrem_dense(-(1 bsl 59)),

    % Runtime small-integer +/-/* fast paths with untyped operands. The small
    % cases take the inline path; a bignum operand takes the BIF fallback.
    % (1 bsl 60) is a bignum on both 32-bit and 64-bit builds.
    0 = ?MODULE:untyped_add(0, 0),
    42 = ?MODULE:untyped_add(20, 22),
    -5 = ?MODULE:untyped_add(-2, -3),
    -7 = ?MODULE:untyped_add(-10, 3),
    7 = ?MODULE:untyped_sub(10, 3),
    0 = ?MODULE:untyped_sub(5, 5),
    1 = ?MODULE:untyped_sub(-2, -3),
    12 = ?MODULE:untyped_mul(3, 4),
    -12 = ?MODULE:untyped_mul(3, -4),
    12 = ?MODULE:untyped_mul(-3, -4),
    0 = ?MODULE:untyped_mul(0, 7),
    81 = ?MODULE:untyped_mul(9, 9),
    % Bignum operands take the BIF fallback (correct large result).
    % (1 bsl 60) is a bignum on both 32-bit and 64-bit builds.
    1152921504606846977 = ?MODULE:untyped_add(1152921504606846976, 1),
    1152921504606846975 = ?MODULE:untyped_sub(1152921504606846976, 1),
    2305843009213693952 = ?MODULE:untyped_mul(1152921504606846976, 2),

    0.
