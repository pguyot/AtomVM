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

%% A receive clause that sends the message straight on can hand over the
%% incoming message block instead of copying the term. Exercises the shapes
%% that qualify, and the ones that must not.
-module(test_forward_message).

-export([start/0, fwd/1, keeps/2, rebuild/1, echo/0]).

start() ->
    chain_ok() + keeps_ok() + rebuild_ok() + self_ok() + named_ok().

%% The forwarding shape: nothing derived from the message survives the tail
%% call. Runs a payload with a reference, a bignum, a float and a binary
%% through three hops, collecting garbage at each end.
fwd(To) ->
    receive
        {_From, {message, X}} ->
            To ! {self(), {message, X}},
            fwd(To);
        stop ->
            To ! stop,
            ok
    end.

payload() ->
    {make_ref(), 1 bsl 90, 2.5, <<1, 2, 3, 4, 5, 6, 7, 8, 9>>, [a, {b, c}]}.

chain_ok() ->
    Me = self(),
    P1 = spawn_opt(fun() -> fwd(Me) end, []),
    P2 = spawn_opt(fun() -> fwd(P1) end, []),
    P3 = spawn_opt(fun() -> fwd(P2) end, []),
    Msg = payload(),
    N = chain_loop(20, P3, Msg, 0),
    P3 ! stop,
    receive
        stop -> ok
    end,
    case N of
        20 -> 1;
        _ -> 0
    end.

chain_loop(0, _P, _Msg, Acc) ->
    Acc;
chain_loop(I, P, Msg, Acc) ->
    P ! {self(), {message, Msg}},
    erlang:garbage_collect(),
    receive
        {_From, {message, Back}} when Back =:= Msg ->
            chain_loop(I - 1, P, Msg, Acc + 1);
        {_From, {message, _Other}} ->
            Acc
    end.

%% Must NOT forward: the tail call keeps a message-derived value live (in x1),
%% so a handed-over block would be freed underneath it. forward_analysis/2
%% rejects this shape on the liveness check; the assertion here is that it
%% still behaves, whichever way that decision goes.
keeps(To, Acc) ->
    receive
        {_From, X} ->
            To ! {self(), X},
            keeps(To, X);
        stop ->
            To ! {kept, Acc}
    end.

keeps_ok() ->
    Me = self(),
    P = spawn_opt(fun() -> keeps(Me, undefined) end, []),
    Msg = payload(),
    P ! {self(), Msg},
    R1 =
        receive
            {_, Back} when Back =:= Msg -> 1;
            _ -> 0
        end,
    %% Drop our copy and collect, so a block handed over here would be freed,
    %% then allocate hard so that memory is handed out again.
    erlang:garbage_collect(),
    _ = churn(2000),
    erlang:garbage_collect(),
    _ = churn(2000),
    erlang:garbage_collect(),
    P ! stop,
    R2 =
        receive
            {kept, Kept} when Kept =:= Msg -> 1;
            _ -> 0
        end,
    case R1 + R2 of
        2 -> 1;
        _ -> 0
    end.

churn(0) -> [];
churn(N) -> [{N, N} | churn(N - 1)].

%% Must NOT forward: an element of the outgoing tuple is a fresh boxed term,
%% not an immediate, so the block cannot simply be patched.
rebuild(To) ->
    receive
        {_From, {message, X}} ->
            To ! {{tagged, self()}, {message, X}},
            rebuild(To);
        _ ->
            ok
    end.

rebuild_ok() ->
    Me = self(),
    P = spawn_opt(fun() -> rebuild(Me) end, []),
    Msg = payload(),
    P ! {self(), {message, Msg}},
    erlang:garbage_collect(),
    receive
        {{tagged, _}, {message, Back}} when Back =:= Msg -> 1;
        _ -> 0
    end.

%% Falls back: a process forwarding to itself keeps the block on its own heap.
echo() ->
    receive
        {From, {message, X}} ->
            self() ! {From, {message, X}},
            echo2();
        _ ->
            ok
    end.

echo2() ->
    receive
        {From, {message, X}} -> From ! {done, X}
    end.

self_ok() ->
    Me = self(),
    P = spawn_opt(fun() -> echo() end, []),
    Msg = payload(),
    P ! {Me, {message, Msg}},
    receive
        {done, Back} when Back =:= Msg -> 1;
        _ -> 0
    end.

%% Falls back: the recipient is a registered name, not a local pid.
named_ok() ->
    Me = self(),
    register(test_forward_target, Me),
    P = spawn_opt(fun() -> to_named() end, []),
    Msg = payload(),
    P ! {Me, {message, Msg}},
    R =
        receive
            {_, {message, Back}} when Back =:= Msg -> 1;
            _ -> 0
        end,
    unregister(test_forward_target),
    R.

to_named() ->
    receive
        {_From, {message, X}} ->
            test_forward_target ! {self(), {message, X}},
            ok
    end.
