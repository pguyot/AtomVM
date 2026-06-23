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

-module(test_application).

% test entry
-export([start/0, test/0]).
% application behaviour (test_app is its own callback module)
-export([start/2, stop/1]).
% supervisor behaviour
-export([init/1]).
% worker MFA
-export([start_reg_worker/1]).

start() ->
    ok = test().

test() ->
    ok = test_load_and_env(),
    ok = test_set_env(),
    ok = test_start_stop(),
    ok = test_ensure_all_started(),
    ok.

%%-----------------------------------------------------------------------------
%% Application specs used by the tests
%%-----------------------------------------------------------------------------
myapp_spec() ->
    {application, myapp, [
        {description, "my test app"},
        {vsn, "1.2.3"},
        {applications, [depapp]},
        {mod, {?MODULE, [test_app_worker]}},
        {env, [{key1, val1}, {key2, val2}]}
    ]}.

depapp_spec() ->
    %% Library application (no mod): "starts" trivially.
    {application, depapp, [
        {description, "dependency"},
        {vsn, "0.1"},
        {applications, []},
        {env, []}
    ]}.

%%-----------------------------------------------------------------------------
test_load_and_env() ->
    cleanup(),
    ok = application:load(myapp_spec()),
    {error, {already_loaded, myapp}} = application:load(myapp_spec()),
    {ok, val1} = application:get_env(myapp, key1),
    undefined = application:get_env(myapp, nope),
    val1 = application:get_env(myapp, key1, default),
    default = application:get_env(myapp, nope, default),
    {ok, [depapp]} = application:get_key(myapp, applications),
    ok = application:unload(myapp),
    undefined = application:get_env(myapp, key1),
    ok.

test_set_env() ->
    cleanup(),
    ok = application:load(myapp_spec()),
    ok = application:set_env(myapp, key3, val3),
    {ok, val3} = application:get_env(myapp, key3),
    ok = application:set_env(myapp, key1, changed),
    {ok, changed} = application:get_env(myapp, key1),
    ok = application:unset_env(myapp, key1),
    undefined = application:get_env(myapp, key1),
    ok = application:unload(myapp),
    ok.

test_start_stop() ->
    cleanup(),
    ok = application:load(depapp_spec()),
    ok = application:load(myapp_spec()),
    %% myapp depends on depapp, which is not started yet
    {error, {not_started, depapp}} = application:start(myapp),
    ok = application:start(depapp),
    ok = application:start(myapp),
    {error, {already_started, myapp}} = application:start(myapp),
    %% app supervision tree is up: the worker registered its name
    true = is_pid(whereis(test_app_sup)),
    true = is_pid(whereis(test_app_worker)),
    %% both apps reported as running
    Running = application:which_applications(),
    {myapp, "my test app", "1.2.3"} = lists:keyfind(myapp, 1, Running),
    {depapp, _, _} = lists:keyfind(depapp, 1, Running),
    %% stop tears the tree down
    ok = application:stop(myapp),
    ok = wait_unregistered(test_app_worker, 50),
    undefined = whereis(test_app_sup),
    false = lists:keyfind(myapp, 1, application:which_applications()),
    {error, {not_started, myapp}} = application:stop(myapp),
    ok = application:stop(depapp),
    cleanup(),
    ok.

test_ensure_all_started() ->
    cleanup(),
    ok = application:load(depapp_spec()),
    ok = application:load(myapp_spec()),
    {ok, Started} = application:ensure_all_started(myapp),
    %% dependency must be started before the app that needs it
    true = lists:member(depapp, Started),
    true = lists:member(myapp, Started),
    true =
        index_of(depapp, Started) < index_of(myapp, Started),
    true = is_pid(whereis(test_app_worker)),
    %% idempotent
    {ok, []} = application:ensure_all_started(myapp),
    cleanup(),
    ok.

%%-----------------------------------------------------------------------------
%% application + supervisor callbacks
%%-----------------------------------------------------------------------------
start(_Type, [WorkerName]) ->
    supervisor:start_link({local, test_app_sup}, ?MODULE, {app_sup, WorkerName}).

stop(_State) ->
    ok.

init({app_sup, WorkerName}) ->
    ChildSpecs = [
        #{
            id => worker,
            start => {?MODULE, start_reg_worker, [WorkerName]},
            restart => transient,
            shutdown => brutal_kill,
            type => worker
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 10, period => 60}, ChildSpecs}}.

start_reg_worker(WorkerName) ->
    Pid = spawn_link(fun() ->
        true = register(WorkerName, self()),
        receive
            stop -> ok
        end
    end),
    {ok, Pid}.

%%-----------------------------------------------------------------------------
%% helpers
%%-----------------------------------------------------------------------------
cleanup() ->
    _ = application:stop(myapp),
    _ = application:stop(depapp),
    _ = application:unload(myapp),
    _ = application:unload(depapp),
    ok.

wait_unregistered(_Name, 0) ->
    {error, still_registered};
wait_unregistered(Name, N) ->
    case whereis(Name) of
        undefined ->
            ok;
        _ ->
            timer:sleep(10),
            wait_unregistered(Name, N - 1)
    end.

index_of(X, L) ->
    index_of(X, L, 1).
index_of(X, [X | _], I) -> I;
index_of(X, [_ | T], I) -> index_of(X, T, I + 1);
index_of(_, [], _) -> -1.
