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

%% Regression test: bsr/bsl by the most-negative small integer
%% (-2^59 on 64-bit, -2^27 on 32-bit). A negative shift amount is turned
%% into the opposite shift by its magnitude; negating the most-negative
%% small int overflowed and produced a malformed term that read back as
%% negative, causing unbounded bsl<->bsr recursion (stack overflow,
%% SIGSEGV/SIGBUS). It must instead raise a normal Erlang exception.
-module(test_shift_int_min_amount).

-export([start/0]).

start() ->
    Min = int_min(),
    %% Non-integer LHS must raise badarith, not crash the VM.
    badarith = catch_class(fun() -> shr(id(ok), Min) end),
    badarith = catch_class(fun() -> shl(id(ok), Min) end),
    %% Integer LHS: a huge shift either overflows or collapses to a sign.
    %% ok bsr Min == ok bsl |Min| -> huge left shift -> overflow.
    error = catch_class(fun() -> shr(id(1024), Min) end),
    %% 1024 bsl Min == 1024 bsr |Min| -> 0; -8 -> -1; 0 -> 0.
    0 = shl(id(1024), Min),
    -1 = shl(id(-8), Min),
    0 = shl(id(0), Min),
    0 = shr(id(0), Min),
    ok = ok,
    0.

%% Most-negative small integer for the current word size.
int_min() ->
    case erlang:system_info(wordsize) of
        8 -> -576460752303423488;
        4 -> -134217728
    end.

shr(A, B) -> A bsr B.
shl(A, B) -> A bsl B.

%% Return the error class atom (badarith/error/...) or the value on success.
catch_class(Fun) ->
    try Fun() of
        V -> V
    catch
        error:badarith -> badarith;
        error:_ -> error
    end.

id(X) -> X.
