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
%% @doc USB CDC implementation for STM32 using TinyUSB.
%%
%% This module implements the {@link uart_hal} behaviour over USB CDC ACM,
%% enabling serial distribution over USB connections.
%%
%% Requires TinyUSB to be integrated into the STM32 build and
%% `AVM_USB_CDC_PORT_DRIVER_ENABLED' to be defined.
%%
%% Example:
%% ```
%% USB = usb:open([]),
%% ok = usb:write(USB, <<"hello">>),
%% case usb:read(USB, 1000) of
%%     {ok, Data} -> io:format("Got: ~p~n", [Data]);
%%     {error, timeout} -> io:format("No data~n")
%% end,
%% ok = usb:close(USB).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(usb).

-behaviour(uart_hal).

-export([open/1, open/2, close/1, read/1, read/2, write/2]).

%%-----------------------------------------------------------------------------
%% @param Name peripheral name (ignored — single CDC interface)
%% @param Opts configuration options
%% @returns Port handle or error
%% @doc Open the USB CDC interface.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: string() | binary(), Opts :: list()) ->
    port() | {error, term()}.
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

%%-----------------------------------------------------------------------------
%% @param Opts configuration options
%% @returns Port handle or error
%% @doc Open the USB CDC interface.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Opts :: list()) -> port() | {error, term()}.
open(Opts) ->
    open_port({spawn, "usb_cdc"}, Opts).

%%-----------------------------------------------------------------------------
%% @param Port handle returned by open
%% @returns ok or error
%% @doc Close the USB CDC interface.
%% @end
%%-----------------------------------------------------------------------------
-spec close(Port :: port()) -> ok | {error, term()}.
close(Port) when is_port(Port) ->
    port:call(Port, close).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Read data from USB CDC. Blocks until data is available.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port()) -> {ok, binary()} | {error, term()}.
read(Port) when is_port(Port) ->
    port:call(Port, read).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @param Timeout in milliseconds
%% @returns `{ok, Data}' or `{error, timeout}' or `{error, Reason}'
%% @doc Read data from USB CDC with timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port(), Timeout :: pos_integer()) ->
    {ok, binary()} | {error, term()}.
read(Port, Timeout) when is_port(Port) ->
    case port:call(Port, read, Timeout) of
        {error, timeout} ->
            port:call(Port, cancel_read),
            {error, timeout};
        Result ->
            Result
    end.

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @param Data to write
%% @returns ok or error
%% @doc Write data to USB CDC.
%% @end
%%-----------------------------------------------------------------------------
-spec write(Port :: port(), Data :: iodata()) -> ok | {error, term()}.
write(Port, Data) when is_port(Port) ->
    port:call(Port, {write, Data}).
