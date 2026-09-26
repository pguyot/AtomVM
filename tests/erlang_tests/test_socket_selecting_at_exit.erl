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

-module(test_socket_selecting_at_exit).

-export([start/0]).

% Leave a process selecting on a socket when the VM is destroyed.
start() ->
    Parent = self(),
    Pid = spawn_opt(fun() -> selecting(Parent) end, []),
    receive
        {Pid, selecting} -> 0
    after 5000 -> 1
    end.

selecting(Parent) ->
    {ok, Socket} = socket:open(inet, dgram, udp),
    ok = socket:bind(Socket, #{family => inet, addr => loopback, port => 0}),
    ok = select_read(Socket),
    Parent ! {self(), selecting},
    receive
        stop -> ok
    end.

select_read(Socket) ->
    case erlang:system_info(machine) of
        "ATOM" ->
            socket:nif_select_read(Socket, make_ref());
        "BEAM" ->
            {select, _} = socket:recv(Socket, 0, nowait),
            ok
    end.
