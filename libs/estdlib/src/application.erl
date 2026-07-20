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

%%-----------------------------------------------------------------------------
%% @doc An implementation of a subset of the Erlang/OTP application interface.
%%
%% Applications are backed by the {@link application_controller}, which is
%% started lazily on first use. Loading currently expects an application
%% specification term (`{application, Name, Keys}'); loading by name from a
%% packbeam resource is not yet supported.
%% @end
%%-----------------------------------------------------------------------------
-module(application).

-export([
    load/1,
    unload/1,
    start/1,
    start/2,
    start_boot/2,
    ensure_all_started/1,
    ensure_all_started/2,
    stop/1,
    which_applications/0,
    get_key/2,
    get_env/2,
    get_env/3,
    set_env/3,
    unset_env/2,
    get_all_env/1,
    get_application/1
]).
-export_type([start_type/0]).

-type start_type() :: normal | {takeover, Node :: node()} | {failover, Node :: node()}.
-type restart_type() :: permanent | transient | temporary.
-type app_spec() :: {application, atom(), [tuple()]}.

%%-----------------------------------------------------------------------------
%% @param   Application an application name, or an application specification
%%          `{application, Name, Keys}'
%% @returns `ok' or `{error, Reason}' (e.g. `{already_loaded, Name}')
%% @doc     Load an application. When given a name, the application
%%          specification is read from an `<Application>.app.bin' resource (a
%%          `term_to_binary' encoded `{application, Name, Keys}') in the AVM
%%          pack; when given a specification, it is loaded directly.
%% @end
%%-----------------------------------------------------------------------------
-spec load(Application :: atom() | app_spec()) -> ok | {error, term()}.
load(Application) ->
    application_controller:load_application(Application).

%%-----------------------------------------------------------------------------
%% @param   Application application to unload
%% @returns `ok' or `{error, Reason}'
%% @doc     Unload an application. The application must not be running.
%% @end
%%-----------------------------------------------------------------------------
-spec unload(Application :: atom()) -> ok | {error, term()}.
unload(Application) ->
    application_controller:unload_application(Application).

%%-----------------------------------------------------------------------------
%% @equiv start(Application, temporary)
%% @end
%%-----------------------------------------------------------------------------
-spec start(Application :: atom()) -> ok | {error, term()}.
start(Application) ->
    start(Application, temporary).

%%-----------------------------------------------------------------------------
%% @param   Application application to start
%% @param   Type restart type
%% @returns `ok' or `{error, Reason}'
%% @doc     Start an application. Every application it depends on must already
%%          be started, otherwise `{error, {not_started, Dep}}' is returned.
%% @end
%%-----------------------------------------------------------------------------
-spec start(Application :: atom(), Type :: restart_type()) -> ok | {error, term()}.
start(Application, Type) ->
    application_controller:start_application(Application, Type).

%%-----------------------------------------------------------------------------
%% @param   Application application to start
%% @param   Type restart type
%% @returns `ok' or `{error, Reason}'
%% @doc     Start an application from a boot script. The application is expected
%%          to have been loaded already (the boot script loads it first). On
%%          AtomVM this is equivalent to {@link start/2}.
%% @end
%%-----------------------------------------------------------------------------
-spec start_boot(Application :: atom(), Type :: restart_type()) -> ok | {error, term()}.
start_boot(Application, Type) ->
    application_controller:start_application(Application, Type).

%%-----------------------------------------------------------------------------
%% @equiv ensure_all_started(Application, temporary)
%% @end
%%-----------------------------------------------------------------------------
-spec ensure_all_started(Application :: atom()) ->
    {ok, [atom()]} | {error, term()}.
ensure_all_started(Application) ->
    ensure_all_started(Application, temporary).

%%-----------------------------------------------------------------------------
%% @param   Application application to start
%% @param   Type restart type
%% @returns `{ok, Started}' where `Started' lists the applications that were
%%          started (dependencies first), or `{error, Reason}'
%% @doc     Start an application and all the applications it depends on, in
%%          dependency order.
%% @end
%%-----------------------------------------------------------------------------
-spec ensure_all_started(Application :: atom(), Type :: restart_type()) ->
    {ok, [atom()]} | {error, term()}.
ensure_all_started(Application, Type) ->
    case do_ensure_started(Application, Type, []) of
        {ok, Started} -> {ok, lists:reverse(Started)};
        {error, _Reason} = Error -> Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Application application to stop
%% @returns `ok' or `{error, {not_started, Application}}'
%% @doc     Stop a running application, tearing down its supervision tree.
%% @end
%%-----------------------------------------------------------------------------
-spec stop(Application :: atom()) -> ok | {error, term()}.
stop(Application) ->
    application_controller:stop_application(Application).

%%-----------------------------------------------------------------------------
%% @returns A list of `{Application, Description, Vsn}' for running applications.
%% @doc     Return the list of currently running applications.
%% @end
%%-----------------------------------------------------------------------------
-spec which_applications() -> [{atom(), string(), string()}].
which_applications() ->
    application_controller:which_applications().

%%-----------------------------------------------------------------------------
%% @param   Application application to read the key of
%% @param   Key resource key
%% @returns `{ok, Value}' or `undefined'
%% @doc     Return the value of a resource key (e.g. `vsn', `applications').
%% @end
%%-----------------------------------------------------------------------------
-spec get_key(Application :: atom(), Key :: atom()) -> {ok, term()} | undefined.
get_key(Application, Key) ->
    application_controller:get_key(Application, Key).

%%-----------------------------------------------------------------------------
%% @param   Application application to get the parameter value of
%% @param   Parameter parameter to get the value of
%% @returns `{ok, Value}' or `undefined' if not found.
%% @end
%%-----------------------------------------------------------------------------
%%-----------------------------------------------------------------------------
%% @param   ModuleOrPid a module or a pid
%% @returns `undefined'
%% @doc     Compatibility stub. AtomVM does not track which application a
%% module or process belongs to, so this always returns `undefined'.
%% @end
%%-----------------------------------------------------------------------------
-spec get_application(ModuleOrPid :: module() | pid()) -> {ok, atom()} | undefined.
get_application(_ModuleOrPid) ->
    undefined.

-spec get_env(Application :: atom(), Parameter :: atom()) -> {ok, term()} | undefined.
get_env(Application, Parameter) ->
    application_controller:get_env(Application, Parameter).

%%-----------------------------------------------------------------------------
%% @param   Application application to get the parameter value of
%% @param   Parameter parameter to get the value of
%% @param   Default default value if parameter is not found
%% @returns the parameter value or `Default' if not found.
%% @end
%%-----------------------------------------------------------------------------
-spec get_env(Application :: atom(), Parameter :: atom(), Default :: term()) -> term().
get_env(Application, Parameter, Default) ->
    case application_controller:get_env(Application, Parameter) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

%%-----------------------------------------------------------------------------
%% @doc Set the value of a configuration parameter for an application.
%% @end
%%-----------------------------------------------------------------------------
-spec set_env(Application :: atom(), Parameter :: atom(), Value :: term()) -> ok.
set_env(Application, Parameter, Value) ->
    application_controller:set_env(Application, Parameter, Value).

%%-----------------------------------------------------------------------------
%% @doc Remove a configuration parameter for an application.
%% @end
%%-----------------------------------------------------------------------------
-spec unset_env(Application :: atom(), Parameter :: atom()) -> ok.
unset_env(Application, Parameter) ->
    application_controller:unset_env(Application, Parameter).

%%-----------------------------------------------------------------------------
%% @returns A list of `{Parameter, Value}' for the application's environment.
%% @doc     Return all configuration parameters of an application.
%% @end
%%-----------------------------------------------------------------------------
-spec get_all_env(Application :: atom()) -> [{atom(), term()}].
get_all_env(Application) ->
    application_controller:get_all_env(Application).

%%-----------------------------------------------------------------------------
%% internal
%%-----------------------------------------------------------------------------
do_ensure_started(Application, Type, Started) ->
    case is_running(Application) of
        true ->
            {ok, Started};
        false ->
            %% Load the application (from its <App>.app.bin resource) if it is
            %% not already loaded, so its dependencies can be resolved.
            _ = load(Application),
            Deps = dependencies(Application),
            case do_ensure_started_list(Deps, Type, Started) of
                {ok, Started1} ->
                    case start(Application, Type) of
                        ok ->
                            {ok, [Application | Started1]};
                        {error, {already_started, Application}} ->
                            {ok, Started1};
                        {error, _Reason} = Error ->
                            Error
                    end;
                {error, _Reason} = Error ->
                    Error
            end
    end.

do_ensure_started_list([], _Type, Started) ->
    {ok, Started};
do_ensure_started_list([Dep | Rest], Type, Started) ->
    case do_ensure_started(Dep, Type, Started) of
        {ok, Started1} -> do_ensure_started_list(Rest, Type, Started1);
        {error, _Reason} = Error -> Error
    end.

dependencies(Application) ->
    case get_key(Application, applications) of
        {ok, Deps} when is_list(Deps) -> Deps;
        _ -> []
    end.

is_running(Application) ->
    lists:keymember(Application, 1, which_applications()).
