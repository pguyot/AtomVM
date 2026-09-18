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
-module(db).
-export([start/0]).
%% Bulk delete from a large map: O(n) per delete makes this quadratic.
start() ->
    [run(N) || N <- [512, 1024, 2048, 4096, 8192]],
    ok.
run(N) ->
    Keys = [{b, I, x} || I <- lists:seq(1, N)],
    M = maps:from_list([{K, 1} || K <- Keys]),
    Del = lists:sublist(Keys, N div 2),
    Reps = max(1, 400000 div N),
    T0 = erlang:monotonic_time(microsecond),
    loop(M, Del, Reps),
    T = erlang:monotonic_time(microsecond) - T0,
    io:format("n=~-6b ~8.2f us per (delete ~p keys)~n", [N, T / Reps, length(Del)]).
loop(_M, _D, 0) ->
    ok;
loop(M, D, R) ->
    _ = lists:foldl(fun(K, Acc) -> maps:remove(K, Acc) end, M, D),
    loop(M, D, R - 1).
