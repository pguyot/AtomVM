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
%% @doc A subset of the Erlang/OTP application controller.
%%
%% This registered server owns the set of loaded applications (their resource
%% keys and configuration environment) and the set of running applications
%% (each backed by an {@link application_master}). It is started lazily the
%% first time the {@link application} API is used, so it does not depend on a
%% boot script being run first.
%%
%% Following OTP, the controller traps exits, is linked to every application
%% master, and never blocks on application code: `start' and `stop' are
%% deferred (`{noreply, ...}'), and the caller is replied to once the master
%% reports completion. `start/2' callbacks may therefore safely call back into
%% the controller (e.g. `application:get_env/2', `application:set_env/3').
%%
%% Not implemented (documented subset): start phases, distributed applications,
%% the `ac_tab' ETS table and group-leader-based process tracking (so
%% `get_application/0' and killing stray non-supervised processes on stop are
%% out of scope for now).
%% @end
%%-----------------------------------------------------------------------------
-module(application_controller).

-behaviour(gen_server).

%% Internal API, used by the application module.
-export([
    ensure_started/0,
    start/1,
    load_application/1,
    unload_application/1,
    start_application/2,
    stop_application/1,
    which_applications/0,
    get_key/2,
    get_env/2,
    set_env/3,
    unset_env/2,
    get_all_env/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    %% App => Keys (the property list from the application resource)
    loaded = #{} :: #{atom() => [tuple()]},
    %% App => #{Par => Val}
    env = #{} :: #{atom() => #{atom() => term()}},
    %% App => {StartType, MasterPid}
    running = #{} :: #{atom() => {atom(), pid()}},
    %% [{App, Type, From, MasterPid}] awaiting an application_started report
    starting = [] :: [{atom(), atom(), term(), pid()}],
    %% [{App, From}] awaiting an application_stopped report
    stopping = [] :: [{atom(), term()}]
}).

%%-----------------------------------------------------------------------------
%% @doc Start the controller if it is not already running. Idempotent.
%% @end
%%-----------------------------------------------------------------------------
-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            ok;
        undefined ->
            case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
                {ok, _Pid} -> ok;
                {error, {already_started, _Pid}} -> ok
            end
    end.

%%-----------------------------------------------------------------------------
%% @doc Start the controller with an application's data already loaded. Called
%% from a boot script via
%% `{kernelProcess, application_controller, {application_controller, start, [ApplData]}}'.
%% Idempotent: if the controller is already running, the running pid is returned
%% and ApplData is not re-loaded.
%% @end
%%-----------------------------------------------------------------------------
-spec start(ApplData :: {application, atom(), [tuple()]}) -> {ok, pid()}.
start(ApplData) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            case gen_server:start({local, ?MODULE}, ?MODULE, [ApplData], []) of
                {ok, Pid} -> {ok, Pid};
                {error, {already_started, Pid}} -> {ok, Pid}
            end
    end.

-spec load_application(AppSpec :: {application, atom(), [tuple()]}) ->
    ok | {error, term()}.
load_application(AppSpec) ->
    call({load, AppSpec}).

-spec unload_application(App :: atom()) -> ok | {error, term()}.
unload_application(App) ->
    call({unload, App}).

-spec start_application(App :: atom(), Type :: atom()) -> ok | {error, term()}.
start_application(App, Type) ->
    call({start, App, Type}).

-spec stop_application(App :: atom()) -> ok | {error, term()}.
stop_application(App) ->
    call({stop, App}).

-spec which_applications() -> [{atom(), string(), string()}].
which_applications() ->
    call(which_applications).

-spec get_key(App :: atom(), Key :: atom()) -> {ok, term()} | undefined.
get_key(App, Key) ->
    call({get_key, App, Key}).

-spec get_env(App :: atom(), Par :: atom()) -> {ok, term()} | undefined.
get_env(App, Par) ->
    call({get_env, App, Par}).

-spec set_env(App :: atom(), Par :: atom(), Val :: term()) -> ok.
set_env(App, Par, Val) ->
    call({set_env, App, Par, Val}).

-spec unset_env(App :: atom(), Par :: atom()) -> ok.
unset_env(App, Par) ->
    call({unset_env, App, Par}).

-spec get_all_env(App :: atom()) -> [{atom(), term()}].
get_all_env(App) ->
    call({get_all_env, App}).

call(Request) ->
    ok = ensure_started(),
    gen_server:call(?MODULE, Request, infinity).

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}};
init([{application, _App, _Keys} = ApplData]) ->
    process_flag(trap_exit, true),
    {_Reply, State} = load_app(ApplData, #state{}),
    {ok, State}.

%% @hidden
handle_call({load, AppSpec}, _From, State) ->
    {Reply, NewState} = load_app(AppSpec, State),
    {reply, Reply, NewState};
handle_call({unload, App}, _From, State) ->
    case is_active(App, State) of
        true ->
            {reply, {error, {running, App}}, State};
        false ->
            NewState = State#state{
                loaded = maps:remove(App, State#state.loaded),
                env = maps:remove(App, State#state.env)
            },
            {reply, ok, NewState}
    end;
handle_call({start, App, Type}, From, State) ->
    case maps:find(App, State#state.loaded) of
        error ->
            {reply, {error, {not_loaded, App}}, State};
        {ok, Keys} ->
            case is_active(App, State) of
                true ->
                    {reply, {error, {already_started, App}}, State};
                false ->
                    do_start(App, Type, Keys, From, State)
            end
    end;
handle_call({stop, App}, From, State) ->
    case maps:find(App, State#state.running) of
        error ->
            {reply, {error, {not_started, App}}, State};
        {ok, {_Type, MasterPid}} ->
            spawn_stopper(App, MasterPid),
            {noreply, State#state{stopping = [{App, From} | State#state.stopping]}}
    end;
handle_call(which_applications, _From, State) ->
    Apps = [
        {App, description(State, App), vsn(State, App)}
     || App <- maps:keys(State#state.running)
    ],
    {reply, Apps, State};
handle_call({get_key, App, Key}, _From, State) ->
    Reply =
        case maps:find(App, State#state.loaded) of
            error -> undefined;
            {ok, Keys} -> get_key_from(Keys, Key)
        end,
    {reply, Reply, State};
handle_call({get_env, App, Par}, _From, State) ->
    Reply =
        case maps:find(App, State#state.env) of
            error ->
                undefined;
            {ok, EnvMap} ->
                case maps:find(Par, EnvMap) of
                    error -> undefined;
                    {ok, Val} -> {ok, Val}
                end
        end,
    {reply, Reply, State};
handle_call({set_env, App, Par, Val}, _From, State) ->
    EnvMap = maps:get(App, State#state.env, #{}),
    NewState = State#state{env = maps:put(App, maps:put(Par, Val, EnvMap), State#state.env)},
    {reply, ok, NewState};
handle_call({unset_env, App, Par}, _From, State) ->
    EnvMap = maps:get(App, State#state.env, #{}),
    NewState = State#state{env = maps:put(App, maps:remove(Par, EnvMap), State#state.env)},
    {reply, ok, NewState};
handle_call({get_all_env, App}, _From, State) ->
    Reply =
        case maps:find(App, State#state.env) of
            error -> [];
            {ok, EnvMap} -> maps:to_list(EnvMap)
        end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @hidden
handle_cast({application_started, App, Res}, State) ->
    case lists:keytake(App, 1, State#state.starting) of
        {value, {App, Type, From, MasterPid}, Starting} ->
            case Res of
                {ok, MasterPid} ->
                    gen_server:reply(From, ok),
                    {noreply, State#state{
                        starting = Starting,
                        running = maps:put(App, {Type, MasterPid}, State#state.running)
                    }};
                {error, Reason} ->
                    gen_server:reply(From, {error, {App, Reason}}),
                    {noreply, State#state{starting = Starting}}
            end;
        false ->
            {noreply, State}
    end;
handle_cast({application_stopped, App}, State) ->
    case lists:keytake(App, 1, State#state.stopping) of
        {value, {App, From}, Stopping} ->
            gen_server:reply(From, ok),
            {noreply, State#state{
                stopping = Stopping,
                running = maps:remove(App, State#state.running)
            }};
        false ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info({'EXIT', MasterPid, _Reason}, State) ->
    %% An application master exited. If it was running and not in the middle of
    %% a controlled stop, the application terminated on its own; drop it.
    NewRunning = maps:filter(
        fun(_App, {_Type, Pid}) -> Pid =/= MasterPid end,
        State#state.running
    ),
    {noreply, State#state{running = NewRunning}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------------------
%% internal
%%-----------------------------------------------------------------------------
load_app({application, App, Keys}, State) ->
    case maps:is_key(App, State#state.loaded) of
        true ->
            {{error, {already_loaded, App}}, State};
        false ->
            Env0 = proplists:get_value(env, Keys, []),
            EnvMap = maps:from_list(Env0),
            {ok, State#state{
                loaded = maps:put(App, Keys, State#state.loaded),
                env = maps:put(App, EnvMap, State#state.env)
            }}
    end.

do_start(App, Type, Keys, From, State) ->
    Deps = proplists:get_value(applications, Keys, []),
    case missing_dependency(Deps, State#state.running) of
        {missing, Dep} ->
            {reply, {error, {not_started, Dep}}, State};
        none ->
            {ok, MasterPid} = application_master:start_link(App, Keys, Type),
            {noreply, State#state{
                starting = [{App, Type, From, MasterPid} | State#state.starting]
            }}
    end.

%% Run the (synchronous) master stop in a helper process so the controller is
%% free to service other requests (including any made from the app's stop path).
spawn_stopper(App, MasterPid) ->
    Controller = self(),
    spawn(fun() ->
        ok = application_master:stop(MasterPid),
        gen_server:cast(Controller, {application_stopped, App})
    end).

is_active(App, State) ->
    maps:is_key(App, State#state.running) orelse
        lists:keymember(App, 1, State#state.starting).

missing_dependency([], _Running) ->
    none;
missing_dependency([Dep | Rest], Running) ->
    case maps:is_key(Dep, Running) of
        true -> missing_dependency(Rest, Running);
        false -> {missing, Dep}
    end.

get_key_from(Keys, Key) ->
    case lists:keyfind(Key, 1, Keys) of
        {Key, Val} -> {ok, Val};
        false -> undefined
    end.

description(State, App) ->
    key_or(State, App, description, "").

vsn(State, App) ->
    key_or(State, App, vsn, "").

key_or(State, App, Key, Default) ->
    case maps:find(App, State#state.loaded) of
        {ok, Keys} -> proplists:get_value(Key, Keys, Default);
        error -> Default
    end.
