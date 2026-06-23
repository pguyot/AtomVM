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
%% @doc A subset of the Erlang/OTP application master.
%%
%% One master process backs each running application. Following OTP's design it
%% is a `proc_lib' special process (not a `gen_server') that traps exits, stays
%% linked to the application's root supervisor, and on stop tears the tree down
%% and calls the callback's `stop/1'.
%%
%% Like OTP it acknowledges its controller immediately (`proc_lib:init_ack')
%% and reports the start result asynchronously with
%% `gen_server:cast(Controller, {application_started, Name, Res})', so the
%% controller is never blocked while an application's `start/2' runs (which
%% could otherwise deadlock if `start/2' calls back into the controller).
%%
%% Differences from OTP (documented subset): the master does not become the
%% I/O group leader, so there is no separate I/O-relaying "starter" process and
%% no group-leader-based killing of stray (non-supervised) processes; start
%% phases and distributed applications are not supported.
%% @end
%%-----------------------------------------------------------------------------
-module(application_master).

-export([start_link/3, stop/1]).
%% proc_lib entry point
-export([init/4]).

-define(SHUTDOWN_TIMEOUT, 5000).

-record(master, {
    app :: atom(),
    mod :: {module(), term()} | undefined,
    root :: pid() | undefined,
    app_state :: term()
}).

%%-----------------------------------------------------------------------------
%% @param Controller the application controller (receives the start result)
%% @param App application name
%% @param Keys application resource keys
%% @param Type start type passed to the callback's `start/2'
%% @doc Start a master for an application. Returns as soon as the master
%% process is created; the start result is delivered to the controller with a
%% `{application_started, App, Res}' cast.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link(App :: atom(), Keys :: [tuple()], Type :: atom()) ->
    {ok, pid()}.
start_link(App, Keys, Type) ->
    proc_lib:start_link(?MODULE, init, [self(), App, Keys, Type]).

%%-----------------------------------------------------------------------------
%% @param MasterPid master process
%% @doc Stop the application: shut down the supervision tree and call `stop/1'.
%% Synchronous; intended to be called from a helper process so the controller
%% is not blocked.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(MasterPid :: pid()) -> ok.
stop(MasterPid) ->
    Ref = erlang:monitor(process, MasterPid),
    MasterPid ! {stop, self(), Ref},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, MasterPid, _Reason} ->
            ok
    end.

%% @hidden
init(Controller, App, Keys, Type) ->
    process_flag(trap_exit, true),
    %% Unblock the controller immediately, like OTP.
    proc_lib:init_ack(Controller, {ok, self()}),
    ModSpec =
        case lists:keyfind(mod, 1, Keys) of
            {mod, {M, A}} -> {M, A};
            _ -> undefined
        end,
    case start_app(ModSpec, Type) of
        {ok, Root, AppState} ->
            gen_server:cast(Controller, {application_started, App, {ok, self()}}),
            main_loop(Controller, #master{
                app = App, mod = ModSpec, root = Root, app_state = AppState
            });
        {error, Reason} ->
            gen_server:cast(Controller, {application_started, App, {error, Reason}}),
            %% Exit normally so the (non-trapping) controller is not taken down;
            %% the error has already been reported via the cast above.
            exit(normal)
    end.

start_app(undefined, _Type) ->
    %% Library application: nothing to start.
    {ok, undefined, undefined};
start_app({Mod, Args}, Type) ->
    try Mod:start(Type, Args) of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid, undefined};
        {ok, Pid, AppState} when is_pid(Pid) -> {ok, Pid, AppState};
        {error, _Reason} = Error -> Error;
        Other -> {error, {bad_return, Other}}
    catch
        Class:CaughtReason -> {error, {Class, CaughtReason}}
    end.

main_loop(Controller, #master{root = Root} = State) ->
    receive
        {stop, From, Ref} ->
            ok = terminate(State),
            From ! {Ref, ok},
            exit(normal);
        {'EXIT', Controller, Reason} ->
            ok = terminate(State),
            exit(Reason);
        {'EXIT', Root, _Reason} when Root =/= undefined ->
            %% The application's root supervisor died: the application is gone.
            ok = call_stop(State),
            exit(normal);
        {'EXIT', _Other, _Reason} ->
            main_loop(Controller, State);
        _Other ->
            main_loop(Controller, State)
    end.

terminate(#master{root = Root} = State) ->
    ok = shutdown_root(Root),
    ok = call_stop(State),
    ok.

shutdown_root(undefined) ->
    ok;
shutdown_root(Root) ->
    exit(Root, shutdown),
    receive
        {'EXIT', Root, _Reason} -> ok
    after ?SHUTDOWN_TIMEOUT ->
        exit(Root, kill),
        receive
            {'EXIT', Root, _Reason2} -> ok
        end
    end.

call_stop(#master{mod = undefined}) ->
    ok;
call_stop(#master{mod = {Mod, _Args}, app_state = AppState}) ->
    try
        _ = Mod:stop(AppState),
        ok
    catch
        _Class:_Reason -> ok
    end.
