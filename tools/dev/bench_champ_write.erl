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
-module(cw).
-export([start/0]).
start() ->
    io:format("-- immediate keys {b, I, x} --~n"),
    [run(N, fun(I) -> {b, I, x} end) || N <- [1024, 4096]],
    io:format("-- compound keys {b, {v, I, y}, x} --~n"),
    [run(N, fun(I) -> {b, {v, I, y}, x} end) || N <- [1024, 4096]],
    ok.
run(N, F) ->
    Keys = [F(I) || I <- lists:seq(1, N)],
    M = maps:from_list([{K, 1} || K <- Keys]),
    Del = lists:sublist(Keys, N div 2),
    Reps = 3,
    {BB, BD} = atomvm:champ_bench(M, Del, Reps, btree),
    {CB, CD} = atomvm:champ_bench(M, Del, Reps, champ),
    io:format(
        "  n=~-6b build: btree ~6.1f ns/op  champ ~6.1f ns/op (~4.2fx)   "
        "remove: btree ~6.1f  champ ~6.1f (~4.2fx)~n",
        [
            N,
            BB * 1000 / (Reps * N),
            CB * 1000 / (Reps * N),
            CB / max(BB, 1),
            BD * 1000 / (Reps * length(Del)),
            CD * 1000 / (Reps * length(Del)),
            CD / max(BD, 1)
        ]
    ).
