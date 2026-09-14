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
%% Split the ESTONE `ets' micro by operation, so a change to one of them can be
%% attributed instead of guessed at. Every variant populates the table the same
%% way and differs only in the operation under test, so subtracting the
%% `insert' baseline gives that operation's own cost. `all' repeats the exact
%% sequence the micro runs, as a cross-check.
-module(ets_split).
-export([start/0]).
-define(REPS, 342).

start() ->
    _ = bench(all, 1),
    [
        io:format("etsop ~p ~p~n", [N, bench(N, ?REPS)])
     || N <- [
            insert,
            delete2,
            match_delete,
            match_boundkey,
            match_freekey,
            first_next,
            newdelete,
            all
        ]
    ],
    ok.

bench(Name, Reps) ->
    T0 = erlang:monotonic_time(),
    ok = repeat(Name, Reps),
    erlang:convert_time_unit(erlang:monotonic_time() - T0, native, microsecond).

repeat(_N, 0) ->
    ok;
repeat(N, I) ->
    _ = one(N),
    repeat(N, I - 1).

one(newdelete) ->
    T1 = ets:new(a, [set]),
    T2 = ets:new(c, [bag, private]),
    ets:delete(T1),
    ets:delete(T2);
one(Name) ->
    T1 = ets:new(a, [set]),
    T2 = ets:new(c, [bag, private]),
    L = [T1, T2],
    run_tabs(Name, L, L, 1),
    ets:delete(T1),
    ets:delete(T2).

run_tabs(_N, _, _, 0) ->
    ok;
run_tabs(N, [], L, I) ->
    run_tabs(N, L, L, I - 1);
run_tabs(N, [Tab | Tail], L, I) ->
    run_tab(N, Tab, I * 20, (I + 1) * 20, I),
    run_tabs(N, Tail, L, I).

run_tab(_N, _Tab, X, X, _) ->
    ok;
run_tab(N, Tab, Beg, End, J) ->
    %% always populate, so every variant sees the same table
    ets:insert(Tab, {Beg, J}),
    ets:insert(Tab, {J, Beg}),
    ets:insert(Tab, {{foo, Beg}, J}),
    ets:insert(Tab, {{foo, J}, Beg}),
    op(N, Tab, Beg, J),
    run_tab(N, Tab, Beg + 1, End, J).

op(insert, _T, _B, _J) ->
    ok;
op(delete2, T, B, _J) ->
    ets:delete(T, haha),
    ets:delete(T, B);
op(match_delete, T, _B, _J) ->
    ets:match_delete(T, {k, j});
op(match_boundkey, T, B, _J) ->
    ets:match(T, {B, '$1'});
op(match_freekey, T, _B, J) ->
    ets:match(T, {'$1', J});
op(first_next, T, _B, _J) ->
    K = ets:first(T),
    _ = ets:next(T, K),
    ok;
op(all, T, B, J) ->
    ets:delete(T, haha),
    ets:match_delete(T, {k, j}),
    ets:match(T, {B, '$1'}),
    ets:match(T, {'$1', J}),
    ets:delete(T, B),
    K = ets:first(T),
    _ = ets:next(T, K),
    ok.
