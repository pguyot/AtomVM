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
%% @doc USB MIDI device for RP2040/RP2350 using TinyUSB.
%%
%% The device is a class-compliant USB MIDI 1.0 device with one virtual
%% cable, so it is recognized by macOS, Linux and Windows without drivers.
%% It requires AtomVM to be built with `-DAVM_USB_MIDI_PORT_DRIVER_ENABLED=ON'
%% (optionally together with `-DAVM_USB_CDC_PORT_DRIVER_ENABLED=ON' for a
%% composite CDC + MIDI device). In this configuration, stdio is on UART0.
%%
%% Data is exchanged as MIDI 1.0 byte streams. Bytes passed to {@link write/2}
%% must be complete MIDI messages without running status. Bytes returned by
%% {@link read/1} and {@link read/2} are complete MIDI messages received from
%% the host (SysEx may be split across reads).
%%
%% The USB device is connected to the host at boot. Writes are silently
%% dropped while the host has not configured the device.
%%
%% Example:
%% ```
%% MIDI = usb_midi:open([]),
%% ok = usb_midi:write(MIDI, <<16#90, 60, 100>>),
%% case usb_midi:read(MIDI, 1000) of
%%     {ok, Data} -> io:format("Got: ~p~n", [Data]);
%%     {error, timeout} -> io:format("No data~n")
%% end,
%% ok = usb_midi:close(MIDI).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(usb_midi).

-export([open/1, close/1, read/1, read/2, write/2, mounted/1]).

%%-----------------------------------------------------------------------------
%% @param Opts configuration options (none currently)
%% @returns Port handle or error
%% @doc Open the USB MIDI interface. The USB device is connected to the host
%% at boot.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Opts :: list()) -> port() | {error, term()}.
open(Opts) ->
    open_port({spawn, "usb_midi"}, Opts).

%%-----------------------------------------------------------------------------
%% @param Port handle returned by open
%% @returns ok or error
%% @doc Close the USB MIDI interface.
%% @end
%%-----------------------------------------------------------------------------
-spec close(Port :: port()) -> ok | {error, term()}.
close(Port) when is_port(Port) ->
    port:call(Port, close).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @returns `{ok, Data}' or `{error, Reason}'
%% @doc Read MIDI bytes sent by the host. Blocks until data is available.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port()) -> {ok, binary()} | {error, term()}.
read(Port) when is_port(Port) ->
    port:call(Port, read).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @param Timeout in milliseconds
%% @returns `{ok, Data}' or `{error, timeout}' or `{error, Reason}'
%% @doc Read MIDI bytes sent by the host, with timeout.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Port :: port(), Timeout :: pos_integer()) ->
    {ok, binary()} | {error, term()}.
read(Port, Timeout) when is_port(Port) ->
    Self = self(),
    MonitorRef = monitor(port, Port),
    Port ! {'$call', {Self, MonitorRef}, read},
    Result =
        receive
            {MonitorRef, Reply} ->
                Reply;
            {'DOWN', MonitorRef, port, Port, normal} ->
                {error, noproc};
            {'DOWN', MonitorRef, port, Port, Reason} ->
                {error, Reason}
        after Timeout ->
            % Pass MonitorRef so the driver only clears its slot if it
            % still belongs to us.
            port:call(Port, {cancel_read, MonitorRef}),
            % Drop any reply that landed between the timeout and the cancel.
            receive
                {MonitorRef, _LateReply} -> ok
            after 0 -> ok
            end,
            {error, timeout}
        end,
    demonitor(MonitorRef, [flush]),
    Result.

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @param Data complete MIDI messages to send to the host
%% @returns ok or error
%% @doc Send MIDI bytes to the host.
%% @end
%%-----------------------------------------------------------------------------
-spec write(Port :: port(), Data :: iodata()) -> ok | {error, term()}.
write(Port, Data) when is_port(Port) ->
    port:call(Port, {write, Data}).

%%-----------------------------------------------------------------------------
%% @param Port handle
%% @returns `true' if the host has configured the MIDI interface
%% @doc Determine if the USB MIDI interface is configured by the host.
%% @end
%%-----------------------------------------------------------------------------
-spec mounted(Port :: port()) -> boolean() | {error, term()}.
mounted(Port) when is_port(Port) ->
    port:call(Port, mounted).
