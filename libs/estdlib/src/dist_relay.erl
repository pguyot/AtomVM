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
%% @doc Relay process for transparent distribution routing.
%%
%% When three Erlang nodes A, B, and C are arranged such that A is
%% connected to B and B is connected to C (but A has no direct link
%% to C), B can act as a transparent relay so that A and C establish
%% a full distribution connection through B.
%%
%% One `dist_relay' process runs on every node that participates in
%% serial (or bridge) distribution.  It is registered locally as
%% `dist_relay'.
%%
%% <h3>Setup flow</h3>
%%
%% When A wants to connect to C:
%%
%% <ol>
%%   <li>A's `serial_dist:setup' discovers that no direct UART is
%%       available and queries B's relay:
%%       `gen_server:call({dist_relay, B}, {can_route, C})' returns
%%       `true'.</li>
%%   <li>A requests relay establishment:
%%       `gen_server:call({dist_relay, B}, {setup_relay, C})' returns
%%       `{ok, Ref, BRelayPid}'.</li>
%%   <li>B's relay asks C's relay to accept the relayed connection:
%%       `gen_server:call({dist_relay, C}, {accept_relay, Ref, BRelayPid})'.
%%       C creates a {@link dist_relay_controller} and triggers the
%%       accept flow on C's `net_kernel'.</li>
%%   <li>A creates its own `dist_relay_controller' and begins the
%%       distribution handshake. Handshake packets flow:
%%       A&nbsp;&rarr; B&nbsp;relay &rarr; C, and vice versa.</li>
%%   <li>After the handshake completes, ongoing distribution data
%%       continues to flow through B's relay using the same path.</li>
%% </ol>
%%
%% <h3>Data forwarding</h3>
%%
%% Each relay session is identified by a unique reference.  The relay
%% process on B receives `{relay_data, Ref, SenderPid, Data}' messages
%% and forwards them to the other controller in the session.  The
%% sender identity is learned from the first message (for A's side)
%% or from the accept setup (for C's side).
%% @end
%%-----------------------------------------------------------------------------
-module(dist_relay).

-behaviour(gen_server).

-export([start_link/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(session, {
    ctrl_a :: pid() | undefined,
    ctrl_c :: pid() | undefined,
    %% Messages from A buffered before C's ctrl is known (shouldn't
    %% normally happen, but guards against races).
    buffer_for_c :: [{pid(), binary()}]
}).

-record(state, {
    sessions :: #{reference() => #session{}}
}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, dist_relay}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([]) ->
    {ok, #state{sessions = #{}}}.

%% @doc Check whether `Node' is directly connected (and thus routable).
handle_call({can_route, Node}, _From, State) ->
    {reply, lists:member(Node, nodes()), State};

%% @doc Called on the intermediary (B) by the initiating node (A).
%% Contacts C's relay, registers the session, and returns
%% `{ok, Ref, BRelayPid}' so A can create its controller.
handle_call({setup_relay, TargetNode}, From, State) ->
    Ref = make_ref(),
    Self = self(),
    spawn_link(fun() ->
        try
            {ok, CtrlC} = gen_server:call(
                {dist_relay, TargetNode}, {accept_relay, Ref, Self}, 15000
            ),
            ok = gen_server:call(Self, {register_session, Ref, undefined, CtrlC}),
            gen_server:reply(From, {ok, Ref, Self})
        catch
            Class:Reason ->
                gen_server:reply(From, {error, {Class, Reason}})
        end
    end),
    {noreply, State};

%% @doc Called on the target node (C) by the intermediary (B).
%% Creates a virtual controller and triggers the distribution accept
%% flow through the local coordinator (serial_dist_link_manager) or
%% directly through net_kernel for bridge_dist.
handle_call({accept_relay, Ref, BRelayPid}, From, State) ->
    {ok, VCtrl} = dist_relay_controller:start(BRelayPid, Ref),
    spawn_link(fun() ->
        case whereis(serial_dist_link_manager) of
            Coordinator when is_pid(Coordinator) ->
                Coordinator ! {relay_accept, VCtrl, self()},
                receive
                    {coordinator_accept, SupervisorPid} ->
                        true = gen_server:call(VCtrl, {supervisor, SupervisorPid}),
                        gen_server:reply(From, {ok, VCtrl});
                    {coordinator_reject} ->
                        gen_server:reply(From, {error, rejected})
                after 15000 ->
                    gen_server:reply(From, {error, accept_timeout})
                end;
            _ ->
                %% No serial_dist coordinator -- try sending accept
                %% directly to net_kernel (for bridge_dist or other
                %% dist modules).
                case whereis(net_kernel) of
                    Kernel when is_pid(Kernel) ->
                        Kernel ! {accept, self(), VCtrl, serial, serial},
                        receive
                            {Kernel, controller, SupervisorPid} ->
                                true = gen_server:call(
                                    VCtrl, {supervisor, SupervisorPid}
                                ),
                                gen_server:reply(From, {ok, VCtrl});
                            {Kernel, unsupported_protocol} ->
                                gen_server:reply(From, {error, rejected})
                        after 15000 ->
                            gen_server:reply(From, {error, accept_timeout})
                        end;
                    _ ->
                        gen_server:reply(From, {error, no_dist})
                end
        end
    end),
    {noreply, State};

%% @doc Internal: register a new relay session.
handle_call({register_session, Ref, CtrlA, CtrlC}, _From, #state{sessions = S} = State) ->
    Session = #session{ctrl_a = CtrlA, ctrl_c = CtrlC, buffer_for_c = []},
    {reply, ok, State#state{sessions = S#{Ref => Session}}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Forward relay data between the two endpoints of a session.
handle_info(
    {relay_data, Ref, SenderPid, Data},
    #state{sessions = Sessions} = State
) ->
    case maps:find(Ref, Sessions) of
        {ok, #session{ctrl_a = CtrlA, ctrl_c = CtrlC} = Session} ->
            if
                SenderPid =:= CtrlC ->
                    %% From C -> forward to A
                    case CtrlA of
                        undefined ->
                            %% Should not happen: A always sends first
                            {noreply, State};
                        _ ->
                            CtrlA ! {relay_data, Ref, SenderPid, Data},
                            {noreply, State}
                    end;
                SenderPid =:= CtrlA ->
                    %% From A -> forward to C
                    CtrlC ! {relay_data, Ref, SenderPid, Data},
                    {noreply, State};
                CtrlA =:= undefined ->
                    %% First message from A's side -- learn the pid
                    NewSession = Session#session{ctrl_a = SenderPid},
                    %% Forward to C
                    CtrlC ! {relay_data, Ref, SenderPid, Data},
                    %% Flush any buffered messages for C
                    lists:foreach(
                        fun({Sender, Buf}) ->
                            CtrlC ! {relay_data, Ref, Sender, Buf}
                        end,
                        Session#session.buffer_for_c
                    ),
                    NewSession2 = NewSession#session{buffer_for_c = []},
                    {noreply, State#state{
                        sessions = Sessions#{Ref => NewSession2}
                    }};
                true ->
                    %% Unknown sender
                    {noreply, State}
            end;
        error ->
            %% Unknown session
            {noreply, State}
    end;
handle_info({relay_handshake_complete, _Ref, _Pid}, State) ->
    {noreply, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
