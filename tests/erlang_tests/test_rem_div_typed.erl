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

%% Exercises rem/div on operands the BEAM compiler proves are integers but
%% whose range is not bounded to small integers ({t_integer, {N, '+inf'}}).
%% This drives the JIT's typed-integer (non-small) rem/div path, covering the
%% inline small-int fast path, the bignum fallback, and the divide-by-zero
%% error path through the same code.
-module(test_rem_div_typed).

-export([start/0, do_rem/2, do_div/2]).

start() ->
    % do_rem/do_div get {t_integer, {2,'+inf'}} typing from the guard below.
    % Small-int operands -> inline fast path.
    2 = do_rem(17, 5),
    3 = do_div(17, 5),
    0 = do_rem(20, 4),
    5 = do_div(20, 4),
    % Bignum operand(s) -> must take the fallback and still be correct.
    Big = bignum(),
    BigRemSmall = do_rem(Big, 7),
    SmallRemMatchesErlang = (Big rem 7),
    true = (BigRemSmall =:= SmallRemMatchesErlang),
    BigDivSmall = do_div(Big, 7),
    true = (BigDivSmall =:= (Big div 7)),
    % rem/div by zero must raise badarith on the typed path too.
    ok = expect_badarith(fun() -> do_rem(10, zero()) end),
    ok = expect_badarith(fun() -> do_div(10, zero()) end),
    % Negative operands (rem follows dividend's sign).
    -2 = do_rem(neg(17), 5),
    0.

do_rem(A, B) when A > 1, B > 1 ->
    A rem B;
do_rem(A, B) ->
    A rem B.

do_div(A, B) when A > 1, B > 1 ->
    A div B;
do_div(A, B) ->
    A div B.

% Opaque-ish helpers so the compiler cannot constant-fold these away.
bignum() ->
    1 bsl 80 + 12345.

zero() ->
    erlang:system_time() * 0.

neg(X) ->
    -X.

expect_badarith(F) ->
    try F() of
        _ -> not_raised
    catch
        error:badarith -> ok
    end.
