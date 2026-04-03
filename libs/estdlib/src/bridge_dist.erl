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
%% @doc Multi-transport bridge distribution module.
%%
%% This module implements the Erlang distribution protocol interface
%% by combining several sub-distribution modules (e.g. `serial_dist'
%% and `socket_dist').  A bridge node can simultaneously accept
%% connections over different transports and route messages between
%% nodes that are reachable only through different communication
%% layers.
%%
%% <h3>Configuration</h3>
%%
%% Pass the list of sub-distribution modules and their options via
%% `avm_dist_opts':
%%
%% ```
%% {ok, _} = net_kernel:start('bridge@node.local', #{
%%     name_domain => longnames,
%%     proto_dist => bridge_dist,
%%     avm_dist_opts => #{
%%         sub_dists => [
%%             {serial_dist, #{
%%                 uart_opts => [{peripheral, "UART1"}, {speed, 115200}],
%%                 uart_module => uart
%%             }},
%%             {socket_dist, #{
%%                 listen_port_min => 9100,
%%                 listen_port_max => 9110
%%             }}
%%         ]
%%     }
%% }).
%% '''
%%
%% <h3>Accept flow</h3>
%%
%% The bridge acceptor spawns each sub-distribution module's accept
%% process.  These sub-acceptors believe the bridge acceptor is
%% `net_kernel'; the bridge acceptor proxies `{accept, ...}' and
%% `{controller, ...}' messages between sub-acceptors and the real
%% `net_kernel'.
%%
%% <h3>Setup flow</h3>
%%
%% When setting up an outgoing connection, each sub-distribution
%% module is tried in order.  If one succeeds the handshake, the
%% connection is established.  If all fail, the relay mechanism from
%% {@link dist_relay} is attempted: the bridge queries every connected
%% node's relay process to find one that can route to the target.
%%
%% <h3>BEAM compatibility</h3>
%%
%% This module works on both AtomVM and BEAM (Erlang/OTP).  On BEAM,
%% configure via application environment before calling
%% `net_kernel:start/1':
%%
%% ```
%% application:set_env(bridge_dist, dist_opts, #{
%%     sub_dists => [
%%         {serial_dist, #{...}},
%%         {socket_dist, #{...}}
%%     ]
%% }).
%% net_kernel:start(['bridge@node.local', longnames]).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(bridge_dist).

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

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type sub_dist_handle() :: {module(), any()}.
-type listen_handle() :: [sub_dist_handle()].

%%--------------------------------------------------------------------
%% dist interface
%%--------------------------------------------------------------------

-spec listen(string()) -> {ok, {listen_handle(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name) ->
    listen(Name, #{}).

-spec listen(string(), map() | string()) ->
    {ok, {listen_handle(), #net_address{}, pos_integer()}} | {error, any()}.
listen(Name, Opts) when is_map(Opts) ->
    EnvOpts = beam_env_opts(),
    EffectiveOpts = maps:merge(EnvOpts, Opts),
    listen_impl(Name, EffectiveOpts);
listen(Name, _Host) ->
    listen_impl(Name, beam_env_opts()).

beam_env_opts() ->
    case application:get_env(bridge_dist, dist_opts) of
        {ok, O} when is_map(O) -> O;
        _ -> #{}
    end.

listen_impl(Name, Opts) ->
    SubDists = maps:get(sub_dists, Opts, []),
    Results = lists:filtermap(
        fun({Mod, SubOpts}) ->
            case Mod:listen(Name, SubOpts) of
                {ok, {Handle, _Address, _Creation}} ->
                    {true, {Mod, Handle}};
                {error, Reason} ->
                    io:format("bridge_dist: ~p:listen failed: ~p~n", [Mod, Reason]),
                    false
            end
        end,
        SubDists
    ),
    case Results of
        [] ->
            {error, no_sub_dists};
        _ ->
            Address = #net_address{
                address = bridge,
                host = bridge,
                protocol = bridge,
                family = bridge
            },
            {ok, {Results, Address, 1}}
    end.

-spec address() -> #net_address{}.
address() ->
    #net_address{
        address = bridge,
        host = bridge,
        protocol = bridge,
        family = bridge
    }.

%% @doc Spawn a bridge acceptor that proxies between net_kernel and
%% each sub-distribution module's accept process.
-spec accept(listen_handle()) -> pid().
accept(SubHandles) ->
    Kernel = self(),
    spawn_link(fun() ->
        process_flag(trap_exit, true),
        %% Start the relay process for transparent routing
        case whereis(dist_relay) of
            undefined -> {ok, _} = dist_relay:start_link();
            _ -> ok
        end,
        %% Start each sub-dist's accept.  Each sub-dist's accept
        %% function captures self() as "Kernel".  Since we are
        %% calling from the bridge acceptor, self() is us -- not
        %% the real net_kernel.  Sub-acceptors will send {accept,...}
        %% to us, and we proxy to the real Kernel.
        _SubAcceptPids = [Mod:accept(Handle) || {Mod, Handle} <- SubHandles],
        bridge_accept_loop(Kernel, [])
    end).

bridge_accept_loop(Kernel, PendingAccepts) ->
    receive
        %% A sub-acceptor detected an incoming connection
        {accept, SubPid, Controller, Family, Protocol} ->
            Kernel ! {accept, self(), Controller, Family, Protocol},
            bridge_accept_loop(Kernel, PendingAccepts ++ [{SubPid}]);
        %% Kernel accepted the connection -- forward to the right sub-acceptor
        {Kernel, controller, SupervisorPid} ->
            case PendingAccepts of
                [{relay, ReplyPid} | Rest] ->
                    ReplyPid ! {coordinator_accept, SupervisorPid},
                    bridge_accept_loop(Kernel, Rest);
                [{SubPid} | Rest] ->
                    SubPid ! {self(), controller, SupervisorPid},
                    bridge_accept_loop(Kernel, Rest);
                [] ->
                    io:format("bridge_dist: controller assigned with no pending accepts~n"),
                    bridge_accept_loop(Kernel, PendingAccepts)
            end;
        %% Kernel rejected the connection
        {Kernel, unsupported_protocol} ->
            case PendingAccepts of
                [{relay, ReplyPid} | Rest] ->
                    ReplyPid ! {coordinator_reject},
                    bridge_accept_loop(Kernel, Rest);
                [{SubPid} | Rest] ->
                    SubPid ! {self(), unsupported_protocol},
                    bridge_accept_loop(Kernel, Rest);
                [] ->
                    bridge_accept_loop(Kernel, PendingAccepts)
            end;
        %% Relay accept -- virtual controller from dist_relay
        {relay_accept, VCtrl, ReplyPid} ->
            Kernel ! {accept, self(), VCtrl, serial, serial},
            bridge_accept_loop(Kernel, PendingAccepts ++ [{relay, ReplyPid}]);
        %% Sub-acceptor or relay process died -- remove from pending
        {'EXIT', Kernel, Reason} ->
            exit(Reason);
        {'EXIT', _Pid, _Reason} ->
            bridge_accept_loop(Kernel, PendingAccepts)
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

%% @doc Determine the appropriate functions for the controller.
%% Sniff the controller's module by sending a getstat call -- both
%% serial_dist_controller and socket_dist_controller respond.
%% For relay controllers (dist_relay_controller), the same gen_server
%% protocol is used, so serial_dist_controller functions work.
hs_data(Kernel, MyNode, DistCtrl, Allowed, OtherNode, OtherVersion, Type, Timer) ->
    %% All controllers (serial, socket, relay) implement the same
    %% gen_server call/cast protocol, so we can use serial_dist_controller
    %% functions as the common interface.  The only difference is
    %% address/2, which only inspects the node name and is the same
    %% across modules.
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
        f_address = fun bridge_dist_address/2,
        f_handshake_complete = fun serial_dist_controller:handshake_complete/3,
        mf_tick = fun serial_dist_controller:tick/1,
        mf_getstat = fun serial_dist_controller:getstat/1,
        request_type = Type
    }.

%% @private Address function that works across transports.
bridge_dist_address(_Controller, Node) ->
    case string:split(atom_to_list(Node), "@") of
        [_Name, Host] ->
            #net_address{address = bridge, host = Host, protocol = bridge, family = bridge};
        _ ->
            {error, no_node}
    end.

%% @doc Try each sub-distribution module for outgoing connections.
%% If all direct connections fail, fall back to relay routing.
setup(Node, Type, MyNode, LongOrShortNames, SetupTime) ->
    Kernel = self(),
    spawn_opt(
        fun() -> do_setup(Kernel, Node, Type, MyNode, LongOrShortNames, SetupTime) end,
        dist_util:net_ticker_spawn_options()
    ).

do_setup(Kernel, Node, Type, MyNode, _LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    SubDists = get_sub_dist_modules(),
    case try_sub_dists(SubDists, Kernel, Node, Type, MyNode, Timer) of
        ok ->
            ok;
        {error, _} ->
            %% All sub-dists failed; try relay through connected nodes.
            case find_relay(Node, nodes()) of
                {ok, RelayPid, Ref} ->
                    dist_util:reset_timer(Timer),
                    {ok, VCtrl} = dist_relay_controller:start(RelayPid, Ref),
                    true = serial_dist_controller:supervisor(VCtrl, self()),
                    HSData = hs_data(Kernel, MyNode, VCtrl, [], Node, 6, Type, Timer),
                    dist_util:handshake_we_started(HSData);
                {error, Reason} ->
                    ?shutdown2(Node, {no_route, Reason})
            end
    end.

%% @private Try each sub-dist module to set up a direct connection.
try_sub_dists([], _Kernel, _Node, _Type, _MyNode, _Timer) ->
    {error, no_sub_dist};
try_sub_dists([Mod | Rest], Kernel, Node, Type, MyNode, Timer) ->
    case catch try_sub_dist_setup(Mod, Kernel, Node, Type, MyNode, Timer) of
        ok ->
            ok;
        {'EXIT', _} ->
            try_sub_dists(Rest, Kernel, Node, Type, MyNode, Timer);
        _ ->
            try_sub_dists(Rest, Kernel, Node, Type, MyNode, Timer)
    end.

%% @private Attempt setup through a specific sub-dist module.
%% On success, enters the connection ticker loop and never returns.
%% On failure, exits (caught by try_sub_dists).
try_sub_dist_setup(serial_dist, Kernel, Node, Type, MyNode, Timer) ->
    case whereis(serial_dist_link_manager) of
        Mgr when is_pid(Mgr) ->
            Ref = monitor(process, Mgr),
            Mgr ! {setup, self()},
            receive
                {link_manager, DistController} ->
                    demonitor(Ref, [flush]),
                    dist_util:reset_timer(Timer),
                    true = serial_dist_controller:supervisor(DistController, self()),
                    HSData = hs_data(Kernel, MyNode, DistController, [], Node, 6, Type, Timer),
                    dist_util:handshake_we_started(HSData),
                    ok;
                {link_manager_unavailable} ->
                    demonitor(Ref, [flush]),
                    exit(no_link_managers);
                {'DOWN', Ref, process, Mgr, Reason} ->
                    exit({link_manager_died, Reason})
            after 5000 ->
                demonitor(Ref, [flush]),
                exit(link_manager_timeout)
            end;
        _ ->
            exit(no_link_manager)
    end;
try_sub_dist_setup(socket_dist, Kernel, Node, Type, MyNode, Timer) ->
    %% Reuse socket_dist's internal setup logic.
    case string:split(atom_to_list(Node), "@") of
        [Name, Host] ->
            case inet:getaddr(Host, inet) of
                {ok, Ip} ->
                    dist_util:reset_timer(Timer),
                    ErlEpmd = net_kernel:epmd_module(),
                    case ErlEpmd:port_please(Name, Ip) of
                        {port, TcpPort, Version} ->
                            dist_util:reset_timer(Timer),
                            {ok, Sock} = socket:open(inet, stream, tcp),
                            case socket:connect(Sock, #{
                                family => inet, addr => Ip, port => TcpPort
                            }) of
                                ok ->
                                    {ok, DistController} = socket_dist_controller:start(Sock),
                                    true = socket_dist_controller:supervisor(
                                        DistController, self()
                                    ),
                                    HSData = #hs_data{
                                        kernel_pid = Kernel,
                                        this_node = MyNode,
                                        socket = DistController,
                                        timer = Timer,
                                        this_flags = 0,
                                        allowed = [],
                                        other_node = Node,
                                        other_version = Version,
                                        f_send = fun socket_dist_controller:send/2,
                                        f_recv = fun socket_dist_controller:recv/3,
                                        f_setopts_pre_nodeup =
                                            fun socket_dist_controller:setopts_pre_nodeup/1,
                                        f_setopts_post_nodeup =
                                            fun socket_dist_controller:setopts_post_nodeup/1,
                                        f_getll = fun socket_dist_controller:getll/1,
                                        f_address = fun socket_dist_controller:address/2,
                                        f_handshake_complete =
                                            fun socket_dist_controller:handshake_complete/3,
                                        mf_tick = fun socket_dist_controller:tick/1,
                                        mf_getstat = fun socket_dist_controller:getstat/1,
                                        request_type = Type
                                    },
                                    dist_util:handshake_we_started(HSData),
                                    ok;
                                _ConnErr ->
                                    exit(connect_failed)
                            end;
                        _ ->
                            exit(epmd_lookup_failed)
                    end;
                _ ->
                    exit(getaddr_failed)
            end;
        _ ->
            exit(bad_node_name)
    end;
try_sub_dist_setup(Mod, _Kernel, _Node, _Type, _MyNode, _Timer) ->
    exit({unsupported_sub_dist, Mod}).

%% @private Retrieve the list of sub-dist modules from the dist_relay
%% or process dictionary.  During accept, the module list is stored
%% in the bridge acceptor's state; during setup, we read it from the
%% application environment.
get_sub_dist_modules() ->
    case application:get_env(bridge_dist, dist_opts) of
        {ok, #{sub_dists := SubDists}} ->
            [Mod || {Mod, _Opts} <- SubDists];
        _ ->
            %% Fallback: try both known sub-dists
            [serial_dist, socket_dist]
    end.

find_relay(_Node, []) ->
    {error, no_relay_available};
find_relay(Node, [RelayNode | Rest]) ->
    case catch gen_server:call({dist_relay, RelayNode}, {can_route, Node}, 5000) of
        true ->
            case catch gen_server:call({dist_relay, RelayNode}, {setup_relay, Node}, 15000) of
                {ok, Ref, RelayPid} ->
                    {ok, RelayPid, Ref};
                _ ->
                    find_relay(Node, Rest)
            end;
        _ ->
            find_relay(Node, Rest)
    end.

close(SubHandles) when is_list(SubHandles) ->
    lists:foreach(fun({Mod, Handle}) -> Mod:close(Handle) end, SubHandles);
close(_) ->
    ok.

select(_Node) ->
    true.
