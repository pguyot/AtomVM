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
%% @doc Distribution controller for serial (UART) connections.
%%
%% This module manages the serial link for the Erlang distribution protocol.
%% It is used by {@link serial_dist} and follows the same pattern as
%% {@link socket_dist_controller}.
%%
%% During the handshake phase, `send/2' and `recv/3' are called
%% synchronously. Packets are framed with a 2-byte big-endian length
%% prefix, matching the TCP distribution handshake format.
%%
%% After `handshake_complete/3', the controller switches to asynchronous
%% mode: a dedicated reader process continuously reads from the UART and
%% forwards data to the controller, which reassembles 4-byte
%% length-prefixed distribution packets and feeds them to
%% `erlang:dist_ctrl_put_data/2'.
%% @end
%%-----------------------------------------------------------------------------
-module(serial_dist_controller).

-include_lib("kernel/include/net_address.hrl").

% BEAM's dist_util expects packets to be list of integers.
-ifdef(BEAM_INTERFACE).
-define(POST_PROCESS(Packet), binary_to_list(Packet)).
-define(PRE_PROCESS(Packet), iolist_to_binary(Packet)).
-else.
-define(POST_PROCESS(Packet), Packet).
-define(PRE_PROCESS(Packet), Packet).
-endif.

% interface with serial_dist
-export([
    start/2,
    start/3,
    supervisor/2,
    recv/3,
    send/2,
    setopts_pre_nodeup/1,
    setopts_post_nodeup/1,
    getll/1,
    address/2,
    tick/1,
    getstat/1,
    handshake_complete/3
]).

% gen_server API
-behaviour(gen_server).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    uart :: port(),
    uart_module :: module(),
    dhandle :: reference() | undefined,
    buffer :: binary(),
    received :: non_neg_integer(),
    sent :: non_neg_integer(),
    reader :: pid() | undefined
}).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

%% @doc Start a controller for an outgoing (setup) connection.
start(Uart, UartMod) ->
    gen_server:start(?MODULE, {Uart, UartMod, <<>>}, []).

%% @doc Start a controller for an incoming (accept) connection with
%% initial data already read from the UART by the accept loop.
start(Uart, UartMod, InitialData) when is_binary(InitialData) ->
    gen_server:start(?MODULE, {Uart, UartMod, InitialData}, []).

supervisor(Controller, Pid) ->
    gen_server:call(Controller, {supervisor, Pid}).

%% @doc Synchronous receive during handshake. Reads one complete
%% 2-byte length-prefixed packet from UART.
recv(Controller, Length, Timeout) ->
    gen_server:call(Controller, {recv, Length, Timeout}, infinity).

%% @doc Synchronous send during handshake. Writes data with a 2-byte
%% length prefix.
send(Controller, Data) ->
    gen_server:call(Controller, {send, Data}).

setopts_pre_nodeup(_Controller) ->
    ok.

setopts_post_nodeup(_Controller) ->
    ok.

getll(Controller) ->
    {ok, Controller}.

address(_Controller, Node) ->
    case string:split(atom_to_list(Node), "@") of
        [_Name, Host] ->
            #net_address{address = serial, host = Host, protocol = serial, family = serial};
        _ ->
            {error, no_node}
    end.

tick(Controller) ->
    gen_server:cast(Controller, tick).

getstat(Controller) ->
    gen_server:call(Controller, getstat).

handshake_complete(Controller, _Node, DHandle) ->
    gen_server:cast(Controller, {handshake_complete, DHandle}),
    ok.

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init({Uart, UartMod, InitialBuffer}) ->
    {ok, #state{
        uart = Uart,
        uart_module = UartMod,
        buffer = InitialBuffer,
        received = 0,
        sent = 0
    }}.

handle_call({supervisor, Pid}, _From, State) ->
    Result = link(Pid),
    {reply, Result, State};
handle_call(
    {recv, _Length, Timeout},
    _From,
    #state{uart = Uart, uart_module = UartMod, buffer = Buffer} = State
) ->
    case recv_handshake_packet(Uart, UartMod, Buffer, Timeout) of
        {ok, Packet, NewBuffer} ->
            {reply, {ok, ?POST_PROCESS(Packet)}, State#state{buffer = NewBuffer}};
        {error, _} = Error ->
            {reply, Error, State}
    end;
handle_call(
    {send, Data},
    _From,
    #state{uart = Uart, uart_module = UartMod, sent = Sent} = State
) ->
    DataBin = iolist_to_binary(Data),
    DataSize = byte_size(DataBin),
    Result = UartMod:write(Uart, <<DataSize:16, DataBin/binary>>),
    {reply, Result, State#state{sent = Sent + 1}};
handle_call(getstat, _From, #state{received = Received, sent = Sent} = State) ->
    {reply, {ok, Received, Sent, 0}, State}.

handle_cast(tick, #state{uart = Uart, uart_module = UartMod, sent = Sent} = State) ->
    UartMod:write(Uart, <<0:32>>),
    {noreply, State#state{sent = Sent + 1}};
handle_cast(
    {handshake_complete, DHandle},
    #state{uart = Uart, uart_module = UartMod, buffer = Buffer, received = Received} = State0
) ->
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    % Process any data left over in the buffer from the handshake phase.
    % After the handshake, packets use a 4-byte length prefix.
    {NewBuffer, NewReceived} = process_recv_buffer(DHandle, Buffer, Received),
    % Spawn a dedicated reader that blocks on uart:read/1 and
    % forwards chunks to this gen_server.
    Self = self(),
    Reader = spawn_link(fun() -> reader_loop(Self, Uart, UartMod) end),
    {noreply, State0#state{
        dhandle = DHandle,
        buffer = NewBuffer,
        received = NewReceived,
        reader = Reader
    }}.

handle_info(dist_data, State0) ->
    State1 = send_data_loop(State0),
    {noreply, State1};
handle_info(
    {serial_data, Data},
    #state{dhandle = DHandle, buffer = Buffer, received = Received} = State
) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    {NewBuffer2, NewReceived} = process_recv_buffer(DHandle, NewBuffer, Received),
    {noreply, State#state{buffer = NewBuffer2, received = NewReceived}};
handle_info({serial_error, _Reason}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Handshake-phase packet I/O (2-byte length prefix)
%%--------------------------------------------------------------------

recv_handshake_packet(Uart, UartMod, Buffer, Timeout) ->
    Cleaned = strip_sync(Buffer),
    case parse_handshake_packet(Cleaned) of
        {ok, Packet, Rest} ->
            {ok, Packet, Rest};
        need_more ->
            case UartMod:read(Uart, Timeout) of
                {ok, Data} ->
                    recv_handshake_packet(
                        Uart, UartMod, <<Cleaned/binary, Data/binary>>, Timeout
                    );
                {error, _} = Error ->
                    Error
            end
    end.

%% Remove sync magic bytes (<<16#AA, 16#55>>) from buffer.
strip_sync(Bin) ->
    strip_sync(Bin, <<>>).
strip_sync(<<16#AA, 16#55, Rest/binary>>, Acc) ->
    strip_sync(Rest, Acc);
strip_sync(<<B, Rest/binary>>, Acc) ->
    strip_sync(Rest, <<Acc/binary, B>>);
strip_sync(<<>>, Acc) ->
    Acc.

parse_handshake_packet(<<Len:16, Rest/binary>>) when byte_size(Rest) >= Len ->
    <<Packet:Len/binary, Tail/binary>> = Rest,
    {ok, Packet, Tail};
parse_handshake_packet(_) ->
    need_more.

%%--------------------------------------------------------------------
%% Async reader process
%%--------------------------------------------------------------------

reader_loop(Controller, Uart, UartMod) ->
    case UartMod:read(Uart) of
        {ok, Data} ->
            Controller ! {serial_data, Data},
            reader_loop(Controller, Uart, UartMod);
        {error, _Reason} ->
            Controller ! {serial_error, _Reason}
    end.

%%--------------------------------------------------------------------
%% Data-phase packet I/O (4-byte length prefix)
%%--------------------------------------------------------------------

process_recv_buffer(DHandle, <<Size:32, Rest/binary>>, Received) when byte_size(Rest) >= Size ->
    <<Packet:Size/binary, Tail/binary>> = Rest,
    case Packet of
        <<>> -> ok;
        _ -> ok = erlang:dist_ctrl_put_data(DHandle, Packet)
    end,
    process_recv_buffer(DHandle, Tail, Received + 1);
process_recv_buffer(_DHandle, Other, Received) ->
    {Other, Received}.

send_data_loop(#state{dhandle = DHandle, uart = Uart, uart_module = UartMod, sent = Sent} = State) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            ok = erlang:dist_ctrl_get_data_notification(DHandle),
            State;
        Data ->
            DataBin = ?PRE_PROCESS(Data),
            DataSize = byte_size(DataBin),
            UartMod:write(Uart, <<DataSize:32, DataBin/binary>>),
            send_data_loop(State#state{sent = Sent + 1})
    end.
