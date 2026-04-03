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

%% @doc Peer node for three-node serial distribution relay tests.
%%
%% Reads PTY_PATH and TEST_NAME from environment variables.
%%
%% TEST_NAME determines the node's role:
%% <ul>
%%   <li>`relay_c' - Node C: connects to B, registers `test_relay',
%%       waits for a message from A (routed through B), replies.</li>
%%   <li>`relay_ping_a' - Node A (ping test): connects to B, then
%%       sends a ping to C via relay, expects pong back.</li>
%%   <li>`relay_rpc_a' - Node A (rpc test): connects to B, then
%%       sends an RPC request to C via relay.</li>
%% </ul>

-module(test_serial_dist_relay_socat_peer).

-export([start/0]).

start() ->
    PtyPath = os:getenv("PTY_PATH"),
    TestName = os:getenv("TEST_NAME"),
    %% Use fixed node names so peers can address each other.
    NodeName =
        case TestName of
            "relay_c" -> 'c@serial.local';
            _ -> 'a@serial.local'
        end,
    %% Pre-warm JIT compilation of modules used during handshake
    _ = crypto:module_info(),
    _ = dist_util:module_info(),
    _ = uart:module_info(),
    _ = timer_manager:module_info(),
    _ = dist_relay:module_info(),
    _ = dist_relay_controller:module_info(),
    {ok, _} = net_kernel:start(NodeName, #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => #{
            uart_opts => [{peripheral, PtyPath}, {speed, 115200}],
            uart_module => uart
        }
    }),
    erlang:set_cookie('SerialTest'),
    run_test(TestName).

%%--------------------------------------------------------------------
%% Node C: wait for relayed message from A
%%--------------------------------------------------------------------

run_test("relay_c") ->
    %% First connect to B (the relay node in the middle)
    wait_for_connection('b@serial.local'),
    register(test_relay, self()),
    io:format("relay_c_ready~n"),
    relay_c_loop();

%%--------------------------------------------------------------------
%% Node A (ping test): send ping to C through B
%%--------------------------------------------------------------------

run_test("relay_ping_a") ->
    wait_for_connection('b@serial.local'),
    relay_send_with_retry('c@serial.local', ping, 30);

%%--------------------------------------------------------------------
%% Node A (rpc test): send RPC to C through B
%%--------------------------------------------------------------------

run_test("relay_rpc_a") ->
    wait_for_connection('b@serial.local'),
    relay_send_with_retry('c@serial.local', {apply, erlang, system_info, [machine]}, 30).

%%--------------------------------------------------------------------
%% C's message loop: handle ping and rpc requests from A
%%--------------------------------------------------------------------

relay_c_loop() ->
    receive
        {From, ping} ->
            From ! {self(), pong},
            relay_c_loop();
        {From, {apply, M, F, A}} ->
            Result = apply(M, F, A),
            From ! {self(), Result},
            relay_c_loop()
    after 120000 ->
        io:format("relay_c_timeout~n")
    end.

%%--------------------------------------------------------------------
%% A's relay logic: try to reach C through B with retries
%%--------------------------------------------------------------------

relay_send_with_retry(_TargetNode, _Request, 0) ->
    io:format("relay_failed~n");
relay_send_with_retry(TargetNode, Request, Retries) ->
    {test_relay, TargetNode} ! {self(), Request},
    receive
        {_Pid, pong} ->
            io:format("relay_pong~n");
        {_Pid, _Result} ->
            io:format("relay_rpc_ok~n")
    after 3000 ->
        relay_send_with_retry(TargetNode, Request, Retries - 1)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

wait_for_connection(Node) ->
    case net_kernel:connect_node(Node) of
        true ->
            ok;
        false ->
            receive after 1000 -> ok end,
            wait_for_connection(Node)
    end.
