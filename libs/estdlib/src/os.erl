%
% This file is part of AtomVM.
%
% Copyright 2025 Jakub Gonet <jakub.gonet@swmansion.com>
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
-module(os).

-export([getenv/1, cmd/1, system_time/0, system_time/1, type/0]).
-export([find_executable/1, unsetenv/1, set_signal/2, putenv/2, getpid/0, env/0]).

%%-----------------------------------------------------------------------------
%% @param   Command command to execute in a shell
%% @returns the standard output of the command as a string
%% @doc     Execute a command in the default shell and capture its output.
%% Only available on platforms with popen (generic_unix).
%% @end
%%-----------------------------------------------------------------------------
-spec cmd(Command :: iodata() | atom()) -> string().
cmd(_Command) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Name name of the environment variable
%% @returns the value of environment variable or false if unset
%% @doc     Get an environment variable value if defined
%% @end
%%-----------------------------------------------------------------------------
-spec getenv(Name :: nonempty_string()) -> nonempty_string() | false.
getenv(_VarName) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns An integer representing system time.
%% @doc     Returns the current OS system time in native time unit.
%% @end
%%-----------------------------------------------------------------------------
-spec system_time() -> integer().
system_time() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns An integer representing system time.
%% @doc     Returns the current OS system time in the time unit.
%% @end
%%-----------------------------------------------------------------------------
-spec system_time(TimeUnit :: erlang:time_unit()) -> integer().
system_time(_TimeUnit) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns `{Family, Name}' describing the operating system
%% @doc     Return the OS family and name. On AtomVM the family is `unix'
%%          and the name is the AtomVM platform name.
%% @end
%%-----------------------------------------------------------------------------
-spec type() -> {unix, atom()}.
type() ->
    {unix, atomvm:platform()}.

%%-----------------------------------------------------------------------------
%% @param   Name name of the program to find
%% @returns the full path of the program, or `false' if it is not found
%% @doc     Look up a program in the directories of the `PATH' environment
%% variable. A name containing a `/' is only probed as given.
%% @end
%%-----------------------------------------------------------------------------
-spec find_executable(Name :: string()) -> string() | false.
find_executable(Name) ->
    case lists:member($/, Name) of
        true ->
            case is_regular_file(Name) of
                true -> Name;
                false -> false
            end;
        false ->
            % fully qualified so the call resolves to the NIF, not this stub module
            case ?MODULE:getenv("PATH") of
                false -> false;
                Path -> find_executable0(string:split(Path, ":", all), Name)
            end
    end.

%% @private
find_executable0([], _Name) ->
    false;
find_executable0([Dir | Rest], Name) ->
    Candidate =
        case Dir of
            "" -> Name;
            _ -> filename:join(Dir, Name)
        end,
    case is_regular_file(Candidate) of
        true -> Candidate;
        false -> find_executable0(Rest, Name)
    end.

%% @private
%% element 3 of a #file_info{} record is the type
is_regular_file(Filename) ->
    case file:read_file_info(Filename) of
        {ok, Info} when element(3, Info) =:= regular -> true;
        _ -> false
    end.

%%-----------------------------------------------------------------------------
%% @param   Name name of the environment variable to unset
%% @returns `true'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec unsetenv(Name :: nonempty_string()) -> true.
unsetenv(_Name) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Signal signal to configure
%% @param   Option `default', `handle' or `ignore'
%% @returns `ok'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec set_signal(Signal :: atom(), Option :: default | handle | ignore) -> ok.
set_signal(_Signal, _Option) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Name name of the environment variable
%% @param   Value value to set
%% @returns `true'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec putenv(Name :: nonempty_string(), Value :: string()) -> true.
putenv(_Name, _Value) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns the process identifier of the emulator process
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec getpid() -> string().
getpid() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @returns the list of all environment variables as `{Name, Value}' pairs
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec env() -> [{string(), string()}].
env() ->
    erlang:nif_error(undefined).
