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

%% A process killed while waiting in a receive terminates as it handles the
%% kill signal. A process spawned right after must run from its own entry
%% point, even if it happens to be given the address of the terminated one.
-module(test_spawn_after_kill).

-export([start/0, wait_quit/1, reply/1]).

-spec start() -> 0.
start() ->
    ok = loop(20000),
    0.

loop(0) ->
    ok;
loop(N) ->
    {Victim, Ref} = spawn_opt(?MODULE, wait_quit, [self()], [monitor]),
    receive
        {waiting, Victim} -> ok
    end,
    exit(Victim, kill),
    %% the victim is gone, and its memory may be handed to the next process
    receive
        {'DOWN', Ref, process, Victim, killed} -> ok
    end,
    spawn_opt(?MODULE, reply, [self()], []),
    receive
        replied -> loop(N - 1)
    after 5000 -> {lost, N}
    end.

-spec wait_quit(pid()) -> ok.
wait_quit(Parent) ->
    Parent ! {waiting, self()},
    receive
        quit -> ok
    end.

-spec reply(pid()) -> replied.
reply(Parent) ->
    Parent ! replied.
