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
-module(hb).
-export([start/0]).
%% Cost of structurally hashing the key shapes the compiler actually uses,
%% against the cost of a large-map lookup on the same keys.
start() ->
    Keys2 = [{b, I} || I <- lists:seq(1, 256)],
    Keys3 = [{b, I, x} || I <- lists:seq(1, 256)],
    KeysC = [{b, {v, I, y}, x} || I <- lists:seq(1, 256)],
    bench("hash {b,I}      ", fun() -> hashall(Keys2, 0) end, 256),
    bench("hash {b,I,x}    ", fun() -> hashall(Keys3, 0) end, 256),
    bench("hash {b,{v,I,y},x}", fun() -> hashall(KeysC, 0) end, 256),
    Big = maps:from_list([{K, 1} || K <- Keys3]),
    Big4 = maps:from_list([{{b, I, x}, 1} || I <- lists:seq(1, 4096)]),
    bench("lookup n=256    ", fun() -> lookall(Keys3, Big, 0) end, 256),
    bench("lookup n=4096   ", fun() -> lookall(Keys3, Big4, 0) end, 256),
    ok.
hashall([], A) -> A;
hashall([K | Ks], A) -> hashall(Ks, A + erlang:phash2(K)).
lookall([], _M, A) -> A;
lookall([K | Ks], M, A) -> lookall(Ks, M, A + maps:get(K, M)).
bench(Label, F, PerRep) ->
    Reps = 4000,
    _ = F(),
    T0 = erlang:monotonic_time(microsecond),
    loop(F, Reps),
    T = erlang:monotonic_time(microsecond) - T0,
    io:format("~s ~7.1f ns/op~n", [Label, T * 1000 / (Reps * PerRep)]).
loop(_F, 0) ->
    ok;
loop(F, N) ->
    _ = F(),
    loop(F, N - 1).
