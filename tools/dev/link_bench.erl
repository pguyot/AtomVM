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

-module(link_bench).

-export([start/0, idle/0]).

-define(NOOP_ITERATIONS, 100000).
-define(UNIQUE_LINKS, 256).
-define(SUPERVISOR_CHILDREN, 500).

start() ->
    process_flag(trap_exit, true),
    run(false),
    run(true),
    0.

run(Print) ->
    Target = spawn(?MODULE, idle, []),
    true = link(Target),
    Duplicate = timed(fun() -> duplicate_link(Target, ?NOOP_ITERATIONS) end),
    true = unlink(Target),
    wait_until_unlinked(Target),
    Absent = timed(fun() -> absent_unlink(Target, ?NOOP_ITERATIONS) end),
    stop_one(Target),

    Pids = spawn_idle(?UNIQUE_LINKS, []),
    UniqueAdd = timed(fun() -> link_all(Pids) end),
    UniqueRemove = timed(fun() -> unlink_all(Pids) end),
    wait_until_no_links(Pids),
    stop_all(Pids),

    Supervisor = timed(fun() -> supervisor_churn(?SUPERVISOR_CHILDREN) end),
    case Print of
        true ->
            report(duplicate_link, ?NOOP_ITERATIONS, Duplicate),
            report(absent_unlink, ?NOOP_ITERATIONS, Absent),
            report(unique_link_256, ?UNIQUE_LINKS, UniqueAdd),
            report(unique_unlink_256, ?UNIQUE_LINKS, UniqueRemove),
            report(supervisor_churn_500, ?SUPERVISOR_CHILDREN, Supervisor);
        false ->
            ok
    end.

timed(Fun) ->
    Start = erlang:monotonic_time(microsecond),
    ok = Fun(),
    erlang:monotonic_time(microsecond) - Start.

report(Name, Iterations, Usec) ->
    io:format("BENCH ~s ~B ~B~n", [atom_to_list(Name), Iterations, Usec]).

duplicate_link(_Pid, 0) ->
    ok;
duplicate_link(Pid, N) ->
    true = link(Pid),
    duplicate_link(Pid, N - 1).

absent_unlink(_Pid, 0) ->
    ok;
absent_unlink(Pid, N) ->
    true = unlink(Pid),
    absent_unlink(Pid, N - 1).

spawn_idle(0, Acc) ->
    Acc;
spawn_idle(N, Acc) ->
    spawn_idle(N - 1, [spawn(?MODULE, idle, []) | Acc]).

link_all([]) ->
    ok;
link_all([Pid | Tail]) ->
    true = link(Pid),
    link_all(Tail).

unlink_all([]) ->
    ok;
unlink_all([Pid | Tail]) ->
    true = unlink(Pid),
    unlink_all(Tail).

wait_until_unlinked(Pid) ->
    case process_info(self(), links) of
        {links, Links} ->
            case lists:member(Pid, Links) of
                true ->
                    receive
                    after 0 -> ok
                    end,
                    wait_until_unlinked(Pid);
                false ->
                    ok
            end
    end.

wait_until_no_links(Pids) ->
    case process_info(self(), links) of
        {links, Links} ->
            case has_any(Pids, Links) of
                true ->
                    receive
                    after 0 -> ok
                    end,
                    wait_until_no_links(Pids);
                false ->
                    ok
            end
    end.

has_any([], _Links) ->
    false;
has_any([Pid | Tail], Links) ->
    lists:member(Pid, Links) orelse has_any(Tail, Links).

stop_one(Pid) ->
    Pid ! {stop, self()},
    receive
        {stopped, Pid} -> ok
    end.

stop_all([]) ->
    ok;
stop_all([Pid | Tail]) ->
    stop_one(Pid),
    stop_all(Tail).

supervisor_churn(Count) ->
    Pids = spawn_link_idle(Count, []),
    stop_all(Pids).

spawn_link_idle(0, Acc) ->
    Acc;
spawn_link_idle(N, Acc) ->
    spawn_link_idle(N - 1, [spawn_link(?MODULE, idle, []) | Acc]).

idle() ->
    receive
        {stop, From} ->
            From ! {stopped, self()},
            ok
    end.
