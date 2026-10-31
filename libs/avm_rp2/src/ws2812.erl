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
%% @doc AtomVM WS2812 (NeoPixel) LED interface for RP2 (Pico)
%%
%% This module drives a chain of WS2812 RGB LEDs with a PIO state machine,
%% such as the on-board LED of Waveshare RP2040-Zero (GP16).
%%
%% ```
%% {ok, Leds} = ws2812:init(16),
%% ok = ws2812:write(Leds, [{0, 32, 0}]).
%% '''
%% @end
%%-----------------------------------------------------------------------------
-module(ws2812).

-export([
    init/1,
    deinit/1,
    write/2
]).

-type pin() :: non_neg_integer().
-type ws2812_resource() :: reference().
-type color() :: {Red :: 0..255, Green :: 0..255, Blue :: 0..255}.

-export_type([ws2812_resource/0, color/0]).

%%-----------------------------------------------------------------------------
%% @param   Pin GPIO connected to the data input of the first LED
%% @returns `{ok, Resource}' or `{error, no_pio}' if no PIO state machine
%%          or program space is available
%% @doc     Claim a PIO state machine to drive WS2812 LEDs on a pin.
%%
%%          The state machine is released by {@link deinit/1} or when the
%%          resource is garbage collected.
%% @end
%%-----------------------------------------------------------------------------
-spec init(Pin :: pin()) -> {ok, ws2812_resource()} | {error, no_pio}.
init(_Pin) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource resource returned by `init/1'
%% @returns `ok'
%% @doc     Release the PIO state machine and the pin.
%% @end
%%-----------------------------------------------------------------------------
-spec deinit(Resource :: ws2812_resource()) -> ok.
deinit(_Resource) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Resource resource returned by `init/1'
%% @param   Colors colors of the LEDs, starting with the first of the chain
%% @returns `ok'
%% @doc     Set the colors of the LEDs.
%%
%%          This function blocks while the colors are sent (30 µs per LED),
%%          and, if called again within 330 µs, until the previous colors
%%          are latched.
%% @end
%%-----------------------------------------------------------------------------
-spec write(Resource :: ws2812_resource(), Colors :: [color()]) -> ok.
write(_Resource, _Colors) ->
    erlang:nif_error(undefined).
