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

%% @doc Peer node for serial distribution tests.
%% Reads PTY_PATH and TEST_NAME from environment variables.
%% Starts serial distribution on the given pty and connects to the
%% orchestrator node via autoconnect (sending a message to a named
%% process on the remote node).

-module(test_serial_dist_socat_peer).

-export([start/0]).

start() ->
    PtyPath = os:getenv("PTY_PATH"),
    TestName = os:getenv("TEST_NAME"),
    {ok, _} = net_kernel:start('atomvm_b@serial.local', #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => #{
            uart_opts => [{peripheral, PtyPath}, {speed, 115200}],
            uart_module => uart
        }
    }),
    erlang:set_cookie('SerialTest'),
    PeerNode = 'a@serial.local',
    case TestName of
        "ping" ->
            %% Trigger autoconnect by sending to remote named process
            {test_serial, PeerNode} ! {self(), ping},
            receive
                {_Pid, pong} ->
                    io:format("pong~n")
            after 30000 ->
                io:format("timeout~n")
            end;
        "rpc" ->
            {test_serial, PeerNode} ! {self(), {apply, erlang, system_info, [machine]}},
            receive
                {_Pid, Result} ->
                    io:format("~s~n", [Result])
            after 30000 ->
                io:format("timeout~n")
            end
    end.
