%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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
-module(websocket_dist_controller).

% Required include headers
-include_lib("kernel/include/net_address.hrl").

% interface with websocket_dist
-export([
    start/1,
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

start(Socket) ->
    Address = #net_address{
        address = {{0, 0, 0, 0}, 9000}, host = <<"localhost">>, protocol = tcp, family = inet
    },
    gen_server:start(?MODULE, {Address, Socket}, []).

supervisor(Controller, Pid) ->
    gen_server:call(Controller, {supervisor, Pid}).

recv(Controller, _Length, _Timeout) ->
    {ok, Packet} = gen_server:call(Controller, recv),
    case erlang:system_info(machine) of
        "BEAM" -> {ok, binary_to_list(Packet)};
        "ATOM" -> {ok, Packet}
    end.

send(Controller, Data) ->
    gen_server:cast(Controller, {send, Data}).

setopts_pre_nodeup(_Controller) ->
    ok.

setopts_post_nodeup(_Controller) ->
    ok.

getll(Controller) ->
    {ok, Controller}.

address(Controller, _Node) ->
    gen_server:call(Controller, address).

tick(Controller) ->
    gen_server:cast(Controller, tick).

getstat(Controller) ->
    gen_server:call(Controller, getstat).

handshake_complete(Controller, _Node, DHandle) ->
    gen_server:cast(Controller, {handshake_complete, DHandle}),
    ok.

-record(state, {
    websocket,
    address,
    dhandle :: reference() | undefined,
    buffer :: [binary()],
    pending :: [any()],
    received :: non_neg_integer(),
    sent :: non_neg_integer()
}).

init({Address, WebSocket}) ->
    {ok, #state{
        address = Address, websocket = WebSocket, buffer = [], pending = [], received = 0, sent = 0
    }}.

handle_call({supervisor, Pid}, _From, #state{} = State0) ->
    Result = link(Pid),
    {reply, Result, State0};
handle_call(recv, _From, #state{buffer = [Packet | Tail], pending = []} = State) ->
    {reply, {ok, Packet}, State#state{buffer = Tail}};
handle_call(recv, From, #state{pending = Pending, buffer = []} = State) ->
    {noreply, State#state{pending = Pending ++ [From]}};
handle_call(getstat, _From, #state{received = Received, sent = Sent} = State) ->
    {reply, {ok, Received, Sent, 0}, State};
handle_call(address, _From, #state{address = Address} = State) ->
    {reply, Address, State}.

handle_cast(tick, #state{websocket = WebSocket, sent = Sent} = State) ->
    websocket:send_binary(WebSocket, <<>>),
    {noreply, State#state{sent = Sent + 1}};
handle_cast({send, Data}, #state{websocket = WebSocket} = State0) ->
    websocket:send_binary(WebSocket, iolist_to_binary(Data)),
    {noreply, State0};
handle_cast({handshake_complete, DHandle}, #state{buffer = Buffer} = State0) ->
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    % We now need to get messages when data is coming.
    lists:foreach(
        fun(Packet) ->
            ok = erlang:dist_ctrl_put_data(DHandle, Packet)
        end,
        Buffer
    ),
    State1 = State0#state{pending = [], buffer = [], dhandle = DHandle},
    {noreply, State1}.

handle_info(dist_data, State0) ->
    State1 = send_data_loop(State0),
    {noreply, State1};
handle_info(
    {websocket, WebSocket, Data},
    #state{websocket = WebSocket, buffer = Buffer, pending = [], dhandle = undefined} = State0
) ->
    State1 = State0#state{buffer = Buffer ++ [Data]},
    {noreply, State1};
handle_info(
    {websocket, WebSocket, Data},
    #state{websocket = WebSocket, buffer = [], pending = [From | PendingT], dhandle = undefined} =
        State0
) ->
    gen_server:reply(From, {ok, Data}),
    State1 = State0#state{pending = PendingT},
    {noreply, State1};
handle_info(
    {websocket, WebSocket, Data},
    #state{websocket = WebSocket, buffer = [], pending = [], dhandle = DHandle} = State0
) ->
    case Data of
        <<>> ->
            % Ignore tick
            ok;
        _ ->
            erlang:dist_ctrl_put_data(DHandle, Data)
    end,
    {noreply, State0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send_data_loop(#state{dhandle = DHandle, websocket = WebSocket, sent = Sent} = State) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            ok = erlang:dist_ctrl_get_data_notification(DHandle),
            State;
        Data ->
            DataBin = iolist_to_binary(Data),
            websocket:send_binary(WebSocket, DataBin),
            send_data_loop(State#state{sent = Sent + 1})
    end.
