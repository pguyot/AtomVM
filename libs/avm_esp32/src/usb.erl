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
%% @doc USB CDC implementation for ESP32 using TinyUSB.
%%
%% This module implements the {@link uart_hal} behaviour over USB CDC ACM,
%% enabling serial distribution over USB connections.
%%
%% The peripheral name is the CDC interface identifier, e.g. `"CDC0"' or
%% `"USB0"'. If omitted, CDC interface 0 is used.
%%
%% This driver requires `CONFIG_AVM_ENABLE_USB_CDC_PORT_DRIVER' to be
%% enabled in the ESP-IDF Kconfig and `CONFIG_USE_USB_SERIAL' for TinyUSB.
%%
%% Example:
%% ```
%% USB = usb:open([{peripheral, "CDC0"}]),
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
%% @param Name CDC interface name, e.g. `"CDC0"' or `"USB0"'
%% @param Opts configuration options (currently unused for USB CDC)
%% @returns Port handle or error
%% @doc Open a USB CDC interface with the given name and options.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Name :: string() | binary(), Opts :: list()) ->
    port() | {error, term()}.
open(Name, Opts) ->
    open([{peripheral, Name} | Opts]).

%%-----------------------------------------------------------------------------
%% @param Opts configuration options including `{peripheral, Name}'
%% @returns Port handle or error
%% @doc Open a USB CDC interface.
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
