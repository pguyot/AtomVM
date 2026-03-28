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
%% <h3>Peer-to-peer model</h3>
%%
%% Unlike TCP distribution, serial is point-to-point: one UART handles
%% exactly one connection. A single link manager process owns all reads
%% from the UART and arbitrates between incoming connections (accept)
%% and outgoing connections (setup).
%%
%% The link manager periodically sends sync markers (`<<16#AA, 16#55>>')
%% on the UART so the peer can detect we are alive. When a peer wants to
%% connect, it sends a preamble of repeated sync markers followed by a
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
%% <h3>Configuration</h3>
%%
%% Pass options via `avm_dist_opts' when starting `net_kernel':
%%
%% ```
%% {ok, _} = net_kernel:start('mynode@serial.local', #{
%%     name_domain => longnames,
%%     proto_dist => serial_dist,
%%     avm_dist_opts => #{
%%         uart_opts => [{peripheral, "UART1"}, {speed, 115200},
%%                       {tx, 17}, {rx, 16}],
%%         uart_module => uart
%%     }
%% }).
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

-spec listen(string(), map()) ->
    {ok, {any(), #net_address{}, pos_integer()}} | {error, any()}.
listen(_Name, Opts) ->
    UartOpts = maps:get(uart_opts, Opts, [{peripheral, "UART1"}, {speed, 115200}]),
    UartMod = maps:get(uart_module, Opts, uart),
    case UartMod:open(UartOpts) of
        {error, _} = Error ->
            Error;
        UartPort ->
            Address = #net_address{
                host = serial,
                protocol = serial,
                family = serial
            },
            {ok, {{UartPort, UartMod}, Address, 1}}
    end.

-spec address() -> #net_address{}.
address() ->
    #net_address{
        host = serial,
        protocol = serial,
        family = serial
    }.

%% @doc Start the link manager process.
-spec accept({any(), module()}) -> pid().
accept({UartPort, UartMod}) ->
    Kernel = self(),
    spawn_link(fun() ->
        register(serial_dist_link_manager, self()),
        link_manager(Kernel, UartPort, UartMod, <<>>)
    end).

%%--------------------------------------------------------------------
%% Link manager
%%
%% The link manager owns all UART reads. On each iteration it:
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

link_manager(Kernel, UartPort, UartMod, Buffer) ->
    %% Check for setup request from net_kernel
    receive
        {setup, SetupPid} ->
            do_setup_handshake(Kernel, UartPort, UartMod, SetupPid)
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
            do_accept_handshake(Kernel, UartPort, UartMod, NewBuffer);
        {crc_error, _} ->
            %% Bad data; discard and loop
            link_manager(Kernel, UartPort, UartMod, <<>>);
        {need_more, Trimmed} when byte_size(Trimmed) > 4 ->
            %% Partial frame in flight; enter accept, controller will read more
            do_accept_handshake(Kernel, UartPort, UartMod, Trimmed);
        {need_more, Trimmed} ->
            %% Only sync bytes or too little data; keep buffered
            link_manager(Kernel, UartPort, UartMod, Trimmed)
    end.

%% Responder: handshake data arrived from peer.
do_accept_handshake(Kernel, UartPort, UartMod, Data) ->
    {ok, Ctrl} = serial_dist_controller:start(UartPort, UartMod, Data),
    Kernel ! {accept, self(), Ctrl, serial, serial},
    receive
        {Kernel, controller, SupervisorPid} ->
            true = serial_dist_controller:supervisor(Ctrl, SupervisorPid);
        {Kernel, unsupported_protocol} ->
            exit(unsupported_protocol)
    end,
    %% Monitor controller -- if handshake fails, recover.
    Ref = monitor(process, Ctrl),
    receive
        {'DOWN', Ref, process, Ctrl, _Reason} ->
            flush_setup_messages(),
            link_manager(Kernel, UartPort, UartMod, <<>>)
    end.

%% Initiator: send sync preamble, create controller and hand to setup process.
do_setup_handshake(Kernel, UartPort, UartMod, SetupPid) ->
    serial_dist_controller:send_preamble(UartMod, UartPort),
    {ok, Ctrl} = serial_dist_controller:start(UartPort, UartMod),
    SetupPid ! {link_manager, Ctrl},
    Ref = monitor(process, Ctrl),
    receive
        {'DOWN', Ref, process, Ctrl, _Reason} ->
            flush_setup_messages(),
            link_manager(Kernel, UartPort, UartMod, <<>>)
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

%% @doc Initiate an outgoing connection via the link manager.
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

close({UartPort, UartMod}) ->
    UartMod:close(UartPort);
close(_) ->
    ok.

select(_Node) ->
    true.
