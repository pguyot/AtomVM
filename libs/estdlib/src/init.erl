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
%% @doc An implementation of the Erlang/OTP init interface.
%%
%% This module implements a strict subset of the Erlang/OTP init
%% interface.
%% @end
%%-----------------------------------------------------------------------------
-module(init).

-export([
    boot/1,
    boot_script/1,
    get_argument/1,
    get_plain_arguments/0,
    get_arguments/0,
    notify_when_started/1
]).

%%-----------------------------------------------------------------------------
%% @param Args command line arguments
%% @doc Entry point.
%% @end
%%-----------------------------------------------------------------------------
-spec boot([binary() | atom() | string()]) -> any().
boot([<<"-s">>, escript, <<"--">>, _Filename | Args]) ->
    % Escript mode: check for start beam and main/1 function before starting kernel
    case atomvm:get_start_beam(escript) of
        {ok, ModuleNameBinary} ->
            % Remove .beam suffix if present (check last 5 bytes)
            Size = byte_size(ModuleNameBinary),
            ModuleName =
                if
                    Size > 5 ->
                        case binary:part(ModuleNameBinary, Size - 5, 5) of
                            <<".beam">> -> binary:part(ModuleNameBinary, 0, Size - 5);
                            _ -> ModuleNameBinary
                        end;
                    true ->
                        ModuleNameBinary
                end,
            Module = binary_to_atom(ModuleName, utf8),
            case erlang:function_exported(Module, main, 1) of
                true ->
                    {ok, _KernelPid} = kernel:start(boot, []),
                    Module:main(Args);
                false ->
                    io:format("Function ~s:main/1 is not exported~n", [Module]),
                    error
            end;
        _ ->
            io:format("start_beam not found~n"),
            error
    end;
boot([<<"-s">>, StartupModule]) when is_atom(StartupModule) ->
    % Until we have boot scripts, we just start kernel application.
    {ok, _KernelPid} = kernel:start(boot, []),
    StartupModule:start().

%%-----------------------------------------------------------------------------
%% @param BootData the contents of an OTP `.boot' file: a `term_to_binary'
%% encoded boot script, as produced by `systools:make_script/2' (e.g. with the
%% `no_dot_erlang' option).
%% @doc Evaluate an OTP boot script.
%%
%% The boot script is decoded with `binary_to_term/1' and its instructions are
%% evaluated in order. AtomVM loads modules on demand from its packbeam, so the
%% module-loading instructions (`preLoaded', `path', `primLoad' and
%% `kernel_load_completed') are no-ops, mirroring OTP's non-embedded behaviour
%% where the code server loads modules dynamically. `kernelProcess' and `apply'
%% instructions are applied; an instruction whose module is not available on
%% AtomVM (for example the `heart' or `logger_server' kernel processes
%% referenced by a stock OTP boot script) is skipped rather than aborting the
%% boot.
%% @end
%%-----------------------------------------------------------------------------
-spec boot_script(BootData :: binary()) -> ok.
boot_script(BootData) when is_binary(BootData) ->
    {script, {_Name, _Vsn}, Instructions} = binary_to_term(BootData),
    eval_script(Instructions).

%% @private
eval_script([]) ->
    ok;
eval_script([Instruction | Rest]) ->
    eval_instruction(Instruction),
    eval_script(Rest).

%% @private
eval_instruction({progress, _Info}) -> ok;
eval_instruction({preLoaded, _Modules}) -> ok;
eval_instruction({path, _Paths}) -> ok;
eval_instruction({primLoad, _Modules}) -> ok;
eval_instruction({kernel_load_completed}) -> ok;
eval_instruction({kernelProcess, _Name, MFA}) -> boot_apply(MFA);
eval_instruction({apply, MFA}) -> boot_apply(MFA);
eval_instruction(_Other) -> ok.

%% @private
%% Apply a boot instruction, tolerating modules AtomVM does not provide so a
%% stock OTP boot script remains usable.
boot_apply({Module, Function, Args}) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            _ = apply(Module, Function, Args),
            ok;
        _Error ->
            ok
    end.

%%-----------------------------------------------------------------------------
%% @param Flag flag to get values for
%% @return `error' if no value is associated with provided flag or values in
%% order of the command line
%% @doc Returns values associated with a given command-line user flag.
%% Currently always returns `error' on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec get_argument(Flag :: atom()) -> {ok, [string()]} | error.
get_argument(_Flag) ->
    error.

%%-----------------------------------------------------------------------------
%% @return plain command-line arguments as a list of strings.
%% @doc Gets plain command-line arguments.
%% Currently always returns `[]' on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec get_plain_arguments() -> [string()].
get_plain_arguments() ->
    [].

%%-----------------------------------------------------------------------------
%% @return all command-line user flags with their values.
%% @doc Returns all command-line flags, as `{Flag, Values}' tuples.
%% AtomVM has no emulator flags, so this always returns `[]'.
%% @end
%%-----------------------------------------------------------------------------
-spec get_arguments() -> [{atom(), [string()]}].
get_arguments() ->
    [].

%% @private
-spec notify_when_started(Pid :: pid()) -> ok | started.
notify_when_started(_Pid) ->
    started.
