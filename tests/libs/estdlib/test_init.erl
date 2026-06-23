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

-module(test_init).
-export([start/0, test/0]).
%% application + supervisor callbacks for the boot-script test
-export([start/2, stop/1, init/1, start_boot_worker/1]).

start() ->
    test().

test() ->
    %% On AtomVM no emulator flags exist; the test harness may pass its own
    %% plain arguments on BEAM, so only the shape of the results is checked.
    Arguments = init:get_arguments(),
    true = is_list(Arguments),
    ok = check_flags(Arguments),
    PlainArguments = init:get_plain_arguments(),
    true = lists:all(fun(A) -> is_list(A) end, PlainArguments),
    error = init:get_argument(no_such_flag_atomvm_test),
    %% init:boot_script/1 is AtomVM-specific (BEAM's init has no such function).
    case erlang:system_info(machine) of
        "BEAM" -> ok;
        _ -> ok = test_boot_script()
    end,
    ok.

check_flags([]) ->
    ok;
check_flags([{Flag, Values} | T]) when is_atom(Flag) ->
    true = lists:all(fun(V) -> is_list(V) end, Values),
    check_flags(T).

%%-----------------------------------------------------------------------------
%% Evaluate a systools-shaped boot script: it must load and start applications
%% (in dependency order), no-op the load instructions, and tolerate a
%% kernelProcess referencing a module AtomVM does not provide.
%%-----------------------------------------------------------------------------
test_boot_script() ->
    cleanup_boot(),
    DepSpec =
        {application, boot_dep, [
            {description, "boot dep"}, {vsn, "1"}, {applications, []}, {env, []}
        ]},
    AppSpec =
        {application, boot_app, [
            {description, "boot app"},
            {vsn, "1"},
            {applications, [boot_dep]},
            {mod, {?MODULE, [boot_test_worker]}},
            {env, []}
        ]},
    Script =
        {script, {"boot_test", "1"}, [
            {progress, preloaded},
            {preLoaded, [?MODULE]},
            {path, ["ignored"]},
            {primLoad, [?MODULE]},
            {kernel_load_completed},
            {progress, kernel_load_completed},
            %% module not provided by AtomVM: must be skipped, not abort the boot
            {kernelProcess, missing, {nonexistent_boot_module_atomvm_test, start, []}},
            {apply, {application, load, [DepSpec]}},
            {apply, {application, load, [AppSpec]}},
            {progress, applications_loaded},
            {apply, {application, start_boot, [boot_dep, permanent]}},
            {apply, {application, start_boot, [boot_app, permanent]}},
            {progress, started}
        ]},
    ok = init:boot_script(erlang:term_to_binary(Script)),
    Running = application:which_applications(),
    true = lists:keymember(boot_dep, 1, Running),
    true = lists:keymember(boot_app, 1, Running),
    true = is_pid(whereis(boot_test_worker)),
    cleanup_boot(),
    ok.

cleanup_boot() ->
    _ = application:stop(boot_app),
    _ = application:stop(boot_dep),
    _ = application:unload(boot_app),
    _ = application:unload(boot_dep),
    ok.

%% application + supervisor callbacks
start(_Type, [WorkerName]) ->
    supervisor:start_link({local, boot_test_sup}, ?MODULE, {sup, WorkerName}).

stop(_State) ->
    ok.

init({sup, WorkerName}) ->
    ChildSpecs = [
        #{
            id => worker,
            start => {?MODULE, start_boot_worker, [WorkerName]},
            restart => transient,
            shutdown => brutal_kill,
            type => worker
        }
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 60}, ChildSpecs}}.

start_boot_worker(WorkerName) ->
    Pid = spawn_link(fun() ->
        true = register(WorkerName, self()),
        receive
            stop -> ok
        end
    end),
    {ok, Pid}.
