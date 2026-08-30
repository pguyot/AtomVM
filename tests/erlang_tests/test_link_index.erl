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

%% Exercises local link bookkeeping across the two representations a process
%% uses: the tagged filter of a process with few links, and the hash index a
%% process with many links is promoted to (and demoted from).
-module(test_link_index).

-export([start/0, idle/0]).

%% Must stay above the promotion threshold used by context.c, and high enough
%% that the index has to grow at least once.
-define(MANY, 20).

start() ->
    ok = test_idempotent_link(),
    ok = test_absent_unlink(),
    ok = test_index_promotion(),
    ok = test_index_demotion(),
    ok = test_exit_removes_link(),
    0.

test_idempotent_link() ->
    [Pid] = spawn_idle(1),
    true = link(Pid),
    true = link(Pid),
    1 = count_link(Pid),
    true = unlink(Pid),
    ok = wait_unlinked([Pid]),
    0 = count_link(Pid),
    true = unlink(Pid),
    stop_all([Pid]).

test_absent_unlink() ->
    [Pid] = spawn_idle(1),
    true = unlink(Pid),
    0 = count_link(Pid),
    %% Linking after an unlink of an absent relation must still work.
    true = link(Pid),
    1 = count_link(Pid),
    true = unlink(Pid),
    ok = wait_unlinked([Pid]),
    stop_all([Pid]).

test_index_promotion() ->
    Pids = spawn_idle(?MANY),
    ok = link_all(Pids),
    ok = expect_linked(Pids),
    %% Duplicates must still be detected once lookup goes through the index.
    ok = link_all(Pids),
    ok = expect_linked(Pids),
    ok = unlink_all(Pids),
    ok = wait_unlinked(Pids),
    stop_all(Pids).

test_index_demotion() ->
    Pids = spawn_idle(?MANY),
    ok = link_all(Pids),
    {Removed, Kept} = split(?MANY - 3, Pids, []),
    ok = unlink_all(Removed),
    ok = wait_unlinked(Removed),
    %% The filter rebuilt while demoting must still describe the kept links.
    ok = expect_linked(Kept),
    ok = link_all(Kept),
    ok = expect_linked(Kept),
    ok = unlink_all(Kept),
    ok = wait_unlinked(Kept),
    stop_all(Pids).

test_exit_removes_link() ->
    false = process_flag(trap_exit, true),
    Pids = spawn_idle(?MANY),
    ok = link_all(Pids),
    ok = stop_all(Pids),
    ok = collect_exits(Pids),
    ok = wait_unlinked(Pids),
    true = process_flag(trap_exit, false),
    ok.

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

expect_linked([]) ->
    ok;
expect_linked([Pid | Tail]) ->
    1 = count_link(Pid),
    expect_linked(Tail).

collect_exits([]) ->
    ok;
collect_exits(Pids) ->
    receive
        {'EXIT', Pid, normal} ->
            collect_exits(delete(Pid, Pids, []))
    after 30000 ->
        timeout
    end.

spawn_idle(Count) ->
    spawn_idle(Count, []).

spawn_idle(0, Acc) ->
    Acc;
spawn_idle(Count, Acc) ->
    spawn_idle(Count - 1, [spawn_opt(?MODULE, idle, [], []) | Acc]).

idle() ->
    receive
        stop -> ok
    end.

links() ->
    {links, Links} = process_info(self(), links),
    Links.

count_link(Pid) ->
    count(Pid, links(), 0).

count(_Pid, [], Count) ->
    Count;
count(Pid, [Pid | Tail], Count) ->
    count(Pid, Tail, Count + 1);
count(Pid, [_Other | Tail], Count) ->
    count(Pid, Tail, Count).

%% unlink/1 only marks the relation: the local half goes away when the target
%% acknowledges, so a test that observes the list has to wait for it.
wait_unlinked(Pids) ->
    case any_linked(Pids, links()) of
        false ->
            ok;
        true ->
            receive
            after 1 -> ok
            end,
            wait_unlinked(Pids)
    end.

any_linked([], _Links) ->
    false;
any_linked([Pid | Tail], Links) ->
    case count(Pid, Links, 0) of
        0 -> any_linked(Tail, Links);
        _ -> true
    end.

stop_all([]) ->
    ok;
stop_all([Pid | Tail]) ->
    Pid ! stop,
    stop_all(Tail).

split(0, Rest, Acc) ->
    {Acc, Rest};
split(Count, [Head | Tail], Acc) ->
    split(Count - 1, Tail, [Head | Acc]).

delete(_Item, [], Acc) ->
    Acc;
delete(Item, [Item | Tail], Acc) ->
    append(Acc, Tail);
delete(Item, [Head | Tail], Acc) ->
    delete(Item, Tail, [Head | Acc]).

append([], List) ->
    List;
append([Head | Tail], List) ->
    append(Tail, [Head | List]).
