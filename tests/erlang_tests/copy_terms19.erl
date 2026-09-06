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

%% Round trip every boxed leaf that a message can carry. Both processes
%% garbage collect while holding their copy, so a copy that kept a pointer
%% into the sender's heap is caught rather than tolerated.
-module(copy_terms19).

-export([start/0, echo/0]).

start() ->
    Pid = spawn_opt(fun echo/0, []),
    N = check(terms(), Pid, 0),
    Pid ! terminate,
    N.

check([], _Pid, Acc) ->
    Acc;
check([T | Rest], Pid, Acc) ->
    check(Rest, Pid, Acc + roundtrip(Pid, T)).

terms() ->
    Big = 1 bsl 100,
    <<_:32, Sub:100/binary, _/binary>> = <<0:2048>>,
    [
        %% newly eligible for the single-pass copy
        make_ref(),
        1.5,
        Big,
        -Big,
        <<1, 2, 3, 4, 5, 6, 7, 8>>,
        <<"a literal binary that is long enough to be reference counted">>,
        %% still on the general path, must keep working
        Sub,
        #{a => 1, b => [2, 3]},
        fun erlang:'+'/2,
        %% and the same leaves nested inside tuples and lists
        {make_ref(), [1.5, Big], {<<1, 2, 3>>, -Big}},
        [make_ref(), {1.5}, [Big | -Big]]
    ].

roundtrip(Pid, Term) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Term},
    %% churn this heap so a stale pointer into it would not survive
    _ = mklist(200, []),
    erlang:garbage_collect(),
    receive
        {Ref, Back} when Back =:= Term -> 1;
        {Ref, _Other} -> 0
    end.

mklist(0, Acc) -> Acc;
mklist(N, Acc) -> mklist(N - 1, [{N, N} | Acc]).

echo() ->
    receive
        {From, Ref, Term} ->
            erlang:garbage_collect(),
            From ! {Ref, Term},
            echo();
        terminate ->
            ok
    end.
