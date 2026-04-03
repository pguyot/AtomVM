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
%% @doc Virtual distribution controller for routed (relayed) connections.
%%
%% This module implements the same gen_server call/cast protocol as
%% {@link serial_dist_controller} (and {@link socket_dist_controller}),
%% allowing it to be used transparently with `dist_util' handshake
%% functions. Instead of reading/writing a UART or socket, data is
%% tunnelled through an intermediary relay node's {@link dist_relay}
%% process using normal Erlang distribution messages.
%%
%% During the handshake phase, `send/recv' calls are synchronous:
%% outgoing data is sent as `{relay_data, Ref, Self, Binary}' to the
%% relay process, and incoming data arrives as the same tuple from the
%% relay and is buffered until the next `recv' call.
%%
%% After `handshake_complete/3', the controller switches to async mode
%% using `erlang:dist_ctrl_get_data/1' and `erlang:dist_ctrl_put_data/2'
%% with the relay as transport.
%% @end
%%-----------------------------------------------------------------------------
-module(dist_relay_controller).

-behaviour(gen_server).

-include_lib("kernel/include/net_address.hrl").

-ifdef(BEAM_INTERFACE).
-define(POST_PROCESS(Packet), binary_to_list(Packet)).
-else.
-define(POST_PROCESS(Packet), Packet).
-endif.

-export([start/2]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    relay_pid :: pid(),
    relay_ref :: reference(),
    dhandle :: reference() | undefined,
    recv_buffer :: [binary()],
    recv_from :: {pid(), term()} | undefined,
    received :: non_neg_integer(),
    sent :: non_neg_integer()
}).

%% @doc Start a relay controller.
%% `RelayPid' is the `dist_relay' process on the intermediary node.
%% `RelayRef' identifies the relay session.
-spec start(pid(), reference()) -> {ok, pid()}.
start(RelayPid, RelayRef) ->
    gen_server:start(?MODULE, {RelayPid, RelayRef}, []).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init({RelayPid, RelayRef}) ->
    monitor(process, RelayPid),
    {ok, #state{
        relay_pid = RelayPid,
        relay_ref = RelayRef,
        recv_buffer = [],
        received = 0,
        sent = 0
    }}.

handle_call({supervisor, Pid}, _From, State) ->
    {reply, link(Pid), State};
handle_call(
    {send, Data},
    _From,
    #state{relay_pid = Pid, relay_ref = Ref, sent = Sent} = State
) ->
    Pid ! {relay_data, Ref, self(), iolist_to_binary(Data)},
    {reply, ok, State#state{sent = Sent + 1}};
handle_call(
    {recv, _Length, _Timeout},
    _From,
    #state{recv_buffer = [Data | Rest], received = Recv} = State
) ->
    {reply, {ok, ?POST_PROCESS(Data)}, State#state{recv_buffer = Rest, received = Recv + 1}};
handle_call(
    {recv, _Length, _Timeout},
    From,
    #state{recv_buffer = []} = State
) ->
    {noreply, State#state{recv_from = From}};
handle_call(getstat, _From, #state{received = Recv, sent = Sent} = State) ->
    {reply, {ok, Recv, Sent, 0}, State}.

handle_cast(tick, #state{sent = Sent} = State) ->
    %% Ticks keep the local dist_util connection ticker alive.
    %% The underlying A-B and B-C serial links have their own ticks.
    {noreply, State#state{sent = Sent + 1}};
handle_cast({handshake_complete, DHandle}, State) ->
    ok = erlang:dist_ctrl_get_data_notification(DHandle),
    {noreply, State#state{dhandle = DHandle}}.

%% Handshake-phase: buffer incoming data for the next recv call.
handle_info(
    {relay_data, Ref, _From, Data},
    #state{relay_ref = Ref, dhandle = undefined} = State
) ->
    case State#state.recv_from of
        undefined ->
            {noreply, State#state{recv_buffer = State#state.recv_buffer ++ [Data]}};
        From ->
            gen_server:reply(From, {ok, ?POST_PROCESS(Data)}),
            {noreply, State#state{
                recv_from = undefined,
                received = State#state.received + 1
            }}
    end;
%% Data-phase: feed directly to the VM's distribution input.
handle_info(
    {relay_data, Ref, _From, Data},
    #state{relay_ref = Ref, dhandle = DHandle, received = Recv} = State
) ->
    ok = erlang:dist_ctrl_put_data(DHandle, Data),
    {noreply, State#state{received = Recv + 1}};
%% VM has outgoing distribution data ready.
handle_info(dist_data, State) ->
    {noreply, send_data_loop(State)};
%% Relay process died -- tear down.
handle_info({'DOWN', _MRef, process, _Pid, Reason}, State) ->
    {stop, {relay_down, Reason}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

send_data_loop(
    #state{dhandle = DHandle, relay_pid = Pid, relay_ref = Ref, sent = Sent} = State
) ->
    case erlang:dist_ctrl_get_data(DHandle) of
        none ->
            ok = erlang:dist_ctrl_get_data_notification(DHandle),
            State;
        Data ->
            Pid ! {relay_data, Ref, self(), iolist_to_binary(Data)},
            send_data_loop(State#state{sent = Sent + 1})
    end.
