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

-module(link_noproc).

-export([start/0]).

% link/1 to a process that does not exist: when the caller traps exits, the
% call succeeds and an exit signal with reason noproc is delivered as an
% {'EXIT', Pid, noproc} message; otherwise it fails with noproc.

start() ->
    DeadPid = make_dead_process(),
    test_trap(DeadPid) + test_no_trap(DeadPid).

make_dead_process() ->
    {Pid, Ref} = spawn_opt(fun() -> ok end, [monitor]),
    receive
        {'DOWN', Ref, process, Pid, normal} -> Pid
    end.

test_trap(DeadPid) ->
    process_flag(trap_exit, true),
    true = link(DeadPid),
    R =
        receive
            {'EXIT', DeadPid, noproc} -> 1
        after 500 -> 0
        end,
    process_flag(trap_exit, false),
    R.

test_no_trap(DeadPid) ->
    try link(DeadPid) of
        _ -> 0
    catch
        error:noproc -> 2;
        _:_ -> 0
    end.
