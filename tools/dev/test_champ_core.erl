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
-module(ct).
-export([start/0]).
start() ->
    Shapes = [
        {imm, fun(I) -> {b, I, x} end},
        {cmp, fun(I) -> {b, {v, I, y}, x} end},
        {atomish, fun(I) -> list_to_atom("k" ++ integer_to_list(I)) end},
        {int, fun(I) -> I end},
        {mixed, fun
            (I) when I rem 3 =:= 0 -> I;
            (I) when I rem 3 =:= 1 -> {b, I};
            (I) -> [I, I + 1]
        end}
    ],
    Sizes = [1, 2, 3, 8, 33, 129, 500, 2000, 9000],
    [check(Name, F, N) || {Name, F} <- Shapes, N <- Sizes],
    io:format("champ core: all shapes and sizes ok~n").
check(Name, F, N) ->
    Keys = [F(I) || I <- lists:seq(1, N)],
    U = lists:usort(Keys),
    case length(U) of
        N ->
            M = maps:from_list([{K, {val, K}} || K <- Keys]),
            Del = [K || {Idx, K} <- lists:zip(lists:seq(1, N), Keys), Idx rem 3 =:= 0],
            case atomvm:champ_check(M, Del) of
                0 -> ok;
                Err -> erlang:error({champ_check_failed, Name, N, Err})
            end;
        _ ->
            ok
    end.
