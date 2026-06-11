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

%% Exercises update_record with the inplace hint under the generational
%% collector: the record is threaded uniquely through a loop with enough
%% allocation churn that it gets promoted to the old generation, after which
%% each destructive update stores a freshly allocated (young) tuple into it.
%% Without the write barrier the young value dangles after the next minor
%% collection.
-module(test_update_record_inplace).

-export([start/0, id/1]).

-record(state, {a = 0, box = {0}, count = 0}).

start() ->
    {Pid, Ref} = spawn_opt(
        fun() -> ok = run(id(5)) end, [monitor, {fullsweep_after, 65535}]
    ),
    receive
        {'DOWN', Ref, process, Pid, normal} -> 0
    after 30000 -> 1
    end.

run(0) ->
    ok;
run(K) ->
    % The first half of the loop only updates count: at promotion the box
    % cell still holds the literal default, so the promotion scan has no
    % young pointer to remember there. The second half stores freshly
    % allocated tuples into the (long promoted) box cell: exactly the
    % destructive writes the generational write barrier must record. Both
    % halves live in one function so the alias analysis keeps S unique.
    S = loop(id(3000), #state{a = id(K)}, []),
    % churn so any dangling young pointer in S is clobbered before reading;
    % the rotating Keep list keeps garbage live across an iteration so minor
    % collections must run
    Tail = churn(id(200), []),
    30 = count(hd(Tail), 0),
    3000 = S#state.count,
    {1} = S#state.box,
    K = S#state.a,
    run(K - 1).

loop(0, S, _Keep) ->
    S;
loop(N, S, Keep) when N > 1500 ->
    % allocation that stays live across the tail call forces minor
    % collections to fire mid-loop, so S gets promoted
    G = seq(id(30)),
    30 = count(G, 0),
    loop(N - 1, S#state{count = S#state.count + 1}, [G | prune(Keep, id(20))]);
loop(N, S, Keep) ->
    G = seq(id(30)),
    30 = count(G, 0),
    loop(N - 1, S#state{count = S#state.count + 1, box = {N}}, [G | prune(Keep, id(20))]).

churn(0, Keep) ->
    Keep;
churn(N, Keep) ->
    G = seq(id(30)),
    30 = count(G, 0),
    churn(N - 1, [G | prune(Keep, id(20))]).

prune(L, Limit) ->
    prune(L, Limit, 0).

prune([], _Limit, _N) -> [];
prune(_, Limit, Limit) -> [];
prune([H | T], Limit, N) -> [H | prune(T, Limit, N + 1)].

seq(0) -> [];
seq(N) -> [{x, N} | seq(N - 1)].

count([], Acc) -> Acc;
count([_ | T], Acc) -> count(T, Acc + 1).

id(X) ->
    X.
