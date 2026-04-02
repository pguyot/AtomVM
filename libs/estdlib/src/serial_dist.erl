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

%%-----------------------------------------------------------------------------
%% @doc Distribution over serial (UART) connections.
%%
%% This module implements the Erlang distribution protocol over UART,
%% enabling distributed Erlang between devices connected via serial lines.
%% This is particularly useful for microcontrollers that lack WiFi/TCP
%% (e.g. STM32) but have UART available.
%%
%% <h3>Multi-port support</h3>
%%
%% Many MCUs have multiple UARTs (e.g. UART0 for console, UART1 and UART2
%% for peer connections). This module supports opening several serial ports,
%% each connecting to a different peer node. Pass a list of UART
%% configurations via the `uart_ports' option:
%%
%% ```
%% {ok, _} = net_kernel:start('mynode@serial.local', #{
%%     name_domain => longnames,
%%     proto_dist => serial_dist,
%%     avm_dist_opts => #{
%%         uart_ports => [
%%             [{peripheral, "UART1"}, {speed, 115200}, {tx, 17}, {rx, 16}],
%%             [{peripheral, "UART2"}, {speed, 115200}, {tx, 4}, {rx, 5}]
%%         ],
%%         uart_module => uart
%%     }
%% }).
%% '''
%%
%% For a single port, the legacy `uart_opts' key is still supported:
%%
%% ```
%% avm_dist_opts => #{
%%     uart_opts => [{peripheral, "UART1"}, {speed, 115200}]
%% }
%% '''
%%
%% <h3>Peer-to-peer model</h3>
%%
%% Each UART handles exactly one connection. A coordinator process
%% manages one link manager per UART port. Each link manager owns all
%% reads from its UART and arbitrates between incoming connections
%% (accept) and outgoing connections (setup).
%%
%% The link managers periodically send sync markers (`<<16#AA, 16#55>>')
%% on their UART so the peer can detect we are alive. When a peer wants
%% to connect, it sends a preamble of repeated sync markers followed by a
%% framed handshake packet. Each frame on the wire uses the format:
%%
%% ```
%% <<16#AA, 16#55, Length, Payload, CRC32:32>>
%% '''
%%
%% The CRC32 covers the Length and Payload bytes. False sync matches
%% (where `<<16#AA, 16#55>>' appears in payload data) are rejected by
%% maximum frame size checks and CRC verification. On CRC failure the
%% connection is torn down.
%%
%% <h3>BEAM compatibility</h3>
%%
%% This module also works on BEAM (standard Erlang/OTP). On BEAM, start
%% the VM with `-proto_dist serial' and configure UART options via
%% application environment before calling `net_kernel:start/1':
%%
%% ```
%% application:set_env(serial_dist, dist_opts, #{
%%     uart_opts => [{peripheral, "/dev/ttyUSB0"}, {speed, 115200}],
%%     uart_module => my_uart_module
%% }).
%% net_kernel:start(['mynode@serial.local', longnames]).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(serial_dist).

% dist interface
-export([
    listen/1,
    listen/2,
    accept/1,
    accept_connection/5,
    setup/5,
    close/1,
    select/1,
    address/0
]).

-include_lib("kernel/include/net_address.hrl").
-include_lib("kernel/include/dist.hrl").
-include_lib("kernel/include/dist_util.hrl").

-define(SYNC_MAGIC, <<16#AA, 16#55>>).

-spec listen(string()) -> {ok, {any(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name) ->
    listen(Name, #{}).

-spec listen(string(), map() | string() | atom()) ->
    {ok, {any(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name, Opts) when is_map(Opts) ->
    %% AtomVM path: Opts comes from avm_dist_opts.
    %% BEAM OTP 25+ may also pass an empty map.
    %% Merge with application env to pick up BEAM-side config.
    EnvOpts = beam_env_opts(),
    EffectiveOpts = maps:merge(EnvOpts, Opts),
    listen_impl(Name, EffectiveOpts);
listen(Name, _Host) ->
    %% BEAM path (OTP < 25): Host is a hostname string.
    listen_impl(Name, beam_env_opts()).

beam_env_opts() ->
    case application:get_env(serial_dist, dist_opts) of
        {ok, O} when is_map(O) -> O;
        _ -> #{}
    end.

listen_impl(_Name, Opts) ->
    UartMod = maps:get(uart_module, Opts, uart),
    UartConfigs = case maps:find(uart_ports, Opts) of
        {ok, Ports} ->
            Ports;
        error ->
            case maps:find(uart_opts, Opts) of
                {ok, UartOpts} ->
                    [UartOpts];
                error ->
                    %% Fallback: try SERIAL_DEVICE env var
                    case os:getenv("SERIAL_DEVICE") of
                        false ->
                            [[{peripheral, "UART1"}, {speed, 115200}]];
                        Device ->
                            [[{peripheral, Device}, {speed, 115200}]]
                    end
            end
    end,
    OpenPorts = lists:filtermap(fun(UartOpts) ->
        case UartMod:open(UartOpts) of
            {error, _} -> false;
            UartPort -> {true, {UartPort, UartMod}}
        end
    end, UartConfigs),
    case OpenPorts of
        [] ->
            {error, no_uart_ports};
        _ ->
            Address = #net_address{
                host = serial,
                protocol = serial,
                family = serial
            },
            {ok, {OpenPorts, Address, 1}}
    end.

-spec address() -> #net_address{}.
address() ->
    #net_address{
        host = serial,
        protocol = serial,
        family = serial
    }.

%% @doc Start the coordinator and link manager processes.
%% One link manager is spawned per UART port.
-spec accept([{any(), module()}]) -> pid().
accept(Ports) when is_list(Ports) ->
    Kernel = self(),
    spawn_link(fun() ->
        register(serial_dist_link_manager, self()),
        Coordinator = self(),
        Managers = [spawn_link(fun() ->
            link_manager(Coordinator, UartPort, UartMod, <<>>)
        end) || {UartPort, UartMod} <- Ports],
        coordinator_loop(Kernel, Managers, [])
    end);
accept({UartPort, UartMod}) ->
    accept([{UartPort, UartMod}]).

%%--------------------------------------------------------------------
%% Coordinator
%%
%% The coordinator routes messages between link managers and
%% the net_kernel. It:
%%   - Forwards accept notifications from link managers to kernel
%%   - Forwards controller assignments from kernel back to managers
%%   - Dispatches setup (outgoing) requests to an available manager
%%--------------------------------------------------------------------

coordinator_loop(Kernel, Managers, PendingAccepts) ->
    receive
        %% A link manager detected an incoming connection
        {link_accept, ManagerPid, Ctrl} ->
            Kernel ! {accept, self(), Ctrl, serial, serial},
            coordinator_loop(Kernel, Managers, PendingAccepts ++ [ManagerPid]);
        %% Kernel accepts the connection
        {Kernel, controller, SupervisorPid} ->
            [ManagerPid | Rest] = PendingAccepts,
            ManagerPid ! {coordinator_accept, SupervisorPid},
            coordinator_loop(Kernel, Managers, Rest);
        %% Kernel rejects the connection
        {Kernel, unsupported_protocol} ->
            [ManagerPid | Rest] = PendingAccepts,
            ManagerPid ! {coordinator_reject},
            coordinator_loop(Kernel, Managers, Rest);
        %% Outgoing connection request from setup
        {setup, SetupPid} ->
            [First | _] = Managers,
            First ! {setup, SetupPid},
            coordinator_loop(Kernel, Managers, PendingAccepts)
    end.

%%--------------------------------------------------------------------
%% Link manager
%%
%% One link manager per UART port. It:
%%   1. Checks mailbox for {setup, Pid} (non-blocking)
%%   2. Sends a sync marker so the peer knows we are alive
%%   3. Reads from UART with a short timeout
%%   4. Tries to parse a framed handshake packet (scan_frame)
%%   5. If a complete or partial frame is detected -> accept path
%%   6. If only sync bytes -> loop
%%
%% On recovery after a failed handshake, stale setup messages are
%% flushed and the loop restarts cleanly.
%%--------------------------------------------------------------------

link_manager(Coordinator, UartPort, UartMod, Buffer) ->
    %% Check for setup request from coordinator
    receive
        {setup, SetupPid} ->
            do_setup_handshake(Coordinator, UartPort, UartMod, SetupPid)
    after 0 ->
        ok
    end,
    %% Send sync so peer knows we are alive
    UartMod:write(UartPort, ?SYNC_MAGIC),
    %% Read from UART
    NewBuffer =
        case UartMod:read(UartPort, 500) of
            {ok, Data} -> <<Buffer/binary, Data/binary>>;
            {error, timeout} -> Buffer;
            {error, Reason} -> exit({uart_read_error, Reason})
        end,
    %% Try to detect a framed handshake packet
    case serial_dist_controller:scan_frame(NewBuffer, 16) of
        {ok, _Payload, _Rest} ->
            %% Complete frame found; pass raw buffer to accept
            do_accept_handshake(Coordinator, UartPort, UartMod, NewBuffer);
        {crc_error, _} ->
            %% Bad data; discard and loop
            link_manager(Coordinator, UartPort, UartMod, <<>>);
        {need_more, Trimmed} when byte_size(Trimmed) > 4 ->
            %% Partial frame in flight; enter accept, controller will read more
            do_accept_handshake(Coordinator, UartPort, UartMod, Trimmed);
        {need_more, Trimmed} ->
            %% Only sync bytes or too little data; keep buffered
            link_manager(Coordinator, UartPort, UartMod, Trimmed)
    end.

%% Responder: handshake data arrived from peer.
do_accept_handshake(Coordinator, UartPort, UartMod, Data) ->
    {ok, Ctrl} = serial_dist_controller:start(UartPort, UartMod, Data),
    Coordinator ! {link_accept, self(), Ctrl},
    receive
        {coordinator_accept, SupervisorPid} ->
            true = serial_dist_controller:supervisor(Ctrl, SupervisorPid);
        {coordinator_reject} ->
            exit(unsupported_protocol)
    end,
    %% Monitor controller -- if handshake fails, recover.
    Ref = monitor(process, Ctrl),
    receive
        {'DOWN', Ref, process, Ctrl, _Reason} ->
            flush_setup_messages(),
            link_manager(Coordinator, UartPort, UartMod, <<>>)
    end.

%% Initiator: send sync preamble, create controller and hand to setup process.
do_setup_handshake(Coordinator, UartPort, UartMod, SetupPid) ->
    serial_dist_controller:send_preamble(UartMod, UartPort),
    {ok, Ctrl} = serial_dist_controller:start(UartPort, UartMod),
    SetupPid ! {link_manager, Ctrl},
    Ref = monitor(process, Ctrl),
    receive
        {'DOWN', Ref, process, Ctrl, _Reason} ->
            flush_setup_messages(),
            link_manager(Coordinator, UartPort, UartMod, <<>>)
    end.

flush_setup_messages() ->
    receive
        {setup, _} -> flush_setup_messages()
    after 0 ->
        ok
    end.

accept_connection(_AcceptPid, DistCtrl, MyNode, Allowed, SetupTime) ->
    Kernel = self(),
    spawn_opt(
        fun() -> do_accept_connection(Kernel, DistCtrl, MyNode, Allowed, SetupTime) end,
        dist_util:net_ticker_spawn_options()
    ).

do_accept_connection(Kernel, DistCtrl, MyNode, Allowed, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    HSData = hs_data(Kernel, MyNode, DistCtrl, Allowed, undefined, undefined, undefined, Timer),
    dist_util:handshake_other_started(HSData).

hs_data(Kernel, MyNode, DistCtrl, Allowed, OtherNode, OtherVersion, Type, Timer) ->
    #hs_data{
        kernel_pid = Kernel,
        this_node = MyNode,
        socket = DistCtrl,
        timer = Timer,
        this_flags = 0,
        allowed = Allowed,
        other_node = OtherNode,
        other_version = OtherVersion,
        f_send = fun serial_dist_controller:send/2,
        f_recv = fun serial_dist_controller:recv/3,
        f_setopts_pre_nodeup = fun serial_dist_controller:setopts_pre_nodeup/1,
        f_setopts_post_nodeup = fun serial_dist_controller:setopts_post_nodeup/1,
        f_getll = fun serial_dist_controller:getll/1,
        f_address = fun serial_dist_controller:address/2,
        f_handshake_complete = fun serial_dist_controller:handshake_complete/3,
        mf_tick = fun serial_dist_controller:tick/1,
        mf_getstat = fun serial_dist_controller:getstat/1,
        request_type = Type
    }.

%% @doc Initiate an outgoing connection via the coordinator.
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    spawn_opt(
        fun() -> do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) end,
        dist_util:net_ticker_spawn_options()
    ).

do_setup(Kernel, Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    case whereis(serial_dist_link_manager) of
        Mgr when is_pid(Mgr) ->
            Mgr ! {setup, self()},
            receive
                {link_manager, DistController} ->
                    dist_util:reset_timer(Timer),
                    true = serial_dist_controller:supervisor(DistController, self()),
                    HSData = hs_data(
                        Kernel, MyNode, DistController, [], Node, 6, Type, Timer
                    ),
                    dist_util:handshake_we_started(HSData)
            after 5000 ->
                ?shutdown2(Node, no_link_manager_response)
            end;
        _ ->
            ?shutdown2(Node, no_link_manager)
    end.

close(Ports) when is_list(Ports) ->
    lists:foreach(fun({UartPort, UartMod}) -> UartMod:close(UartPort) end, Ports);
close({UartPort, UartMod}) ->
    UartMod:close(UartPort);
close(_) ->
    ok.

select(_Node) ->
    true.
