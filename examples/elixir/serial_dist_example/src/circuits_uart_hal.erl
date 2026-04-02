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
%% @doc UART wrapper for BEAM using Circuits.UART.
%%
%% This module adapts the Circuits.UART Elixir library to the interface
%% expected by {@link serial_dist} and {@link serial_dist_controller}.
%%
%% Circuits.UART provides cross-platform serial port access on Linux,
%% macOS and Windows. It is available from hex.pm as `circuits_uart'.
%%
%% This module is intended for use on BEAM (standard Erlang/OTP).
%% On AtomVM, use the platform-specific `uart' module instead.
%% @end
%%-----------------------------------------------------------------------------
-module(circuits_uart_hal).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).

%% @doc Open a UART device.
%%
%% Options: `{peripheral, Path}', `{speed, BaudRate}'.
%% Returns: `pid()' (Circuits.UART GenServer pid) on success.
-spec open(list()) -> pid() | {error, term()}.
open(Opts) ->
    Peripheral = proplists:get_value(peripheral, Opts),
    Speed = proplists:get_value(speed, Opts, 115200),
    case 'Elixir.Circuits.UART':start_link() of
        {ok, Pid} ->
            UartOpts = [{speed, Speed}, {active, false}],
            case 'Elixir.Circuits.UART':open(Pid, to_binary(Peripheral), UartOpts) of
                ok ->
                    Pid;
                {error, _} = Err ->
                    'Elixir.Circuits.UART':stop(Pid),
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

%% @doc Open a UART device by name.
-spec open(string() | binary(), list()) -> pid() | {error, term()}.
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

%% @doc Close the UART device.
-spec close(pid()) -> ok | {error, term()}.
close(Pid) ->
    'Elixir.Circuits.UART':close(Pid).

%% @doc Blocking read — waits until data is available.
-spec read(pid()) -> {ok, binary()} | {error, term()}.
read(Pid) ->
    read_loop(Pid).

%% @doc Read with timeout in milliseconds.
-spec read(pid(), pos_integer() | infinity) -> {ok, binary()} | {error, term()}.
read(Pid, infinity) ->
    read(Pid);
read(Pid, Timeout) ->
    case 'Elixir.Circuits.UART':read(Pid, Timeout) of
        {ok, <<>>} -> {error, timeout};
        {ok, Data} -> {ok, Data};
        {error, _} = Err -> Err
    end.

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V).

%% Internal: keep reading until we get data.
read_loop(Pid) ->
    case 'Elixir.Circuits.UART':read(Pid, 5000) of
        {ok, <<>>} ->
            read_loop(Pid);
        {ok, Data} ->
            {ok, Data};
        {error, _} = Err ->
            Err
    end.
