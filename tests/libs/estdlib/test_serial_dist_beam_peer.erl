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

%% @doc AtomVM peer node for BEAM<->AtomVM serial distribution tests.
%%
%% This module runs on AtomVM. It starts serial distribution, registers
%% as `test_serial', and waits for a ping from the BEAM peer. When it
%% receives `{Pid, ping}', it replies with `{self(), pong}' and prints
%% "pong" to stdout for the test harness to verify.

-module(test_serial_dist_beam_peer).

-export([start/0]).

start() ->
    PtyPath = os:getenv("PTY_PATH"),
    %% Force JIT compilation of modules used during handshake
    _ = crypto:module_info(),
    _ = dist_util:module_info(),
    _ = uart:module_info(),
    _ = timer_manager:module_info(),
    {ok, _} = net_kernel:start('atomvm_peer@serial.local', #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => #{
            uart_opts => [{peripheral, PtyPath}, {speed, 115200}],
            uart_module => uart
        }
    }),
    erlang:set_cookie('SerialTest'),
    register(test_serial, self()),
    receive
        {Pid, ping} ->
            Pid ! {self(), pong},
            io:format("pong~n")
    after 30000 ->
        io:format("timeout~n"),
        exit(timeout)
    end.
