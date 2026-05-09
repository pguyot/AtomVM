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

%% Bidirectional rot13 bridge between UART0 and the integrated USB-Serial-JTAG
%% controller. Bytes typed on one endpoint are rot13'd and written to the other,
%% which exercises both read and write paths on the USB_SERIAL_JTAG peripheral.
%%
%% Requires a target SoC with SOC_USB_SERIAL_JTAG_SUPPORTED
%% (C3/C5/C6/C61/H2/H21/H4/P4/S3). Connect the board over both the UART USB
%% bridge and the native USB port to interact with each side.
-module(usb_serial_jtag_rot13).
-export([start/0]).

start() ->
    UART0 = uart:open("UART0", []),
    JTAG = uart:open("USB_SERIAL_JTAG", []),
    spawn_link(fun() -> bridge_loop(UART0, JTAG) end),
    bridge_loop(JTAG, UART0).

bridge_loop(In, Out) ->
    {ok, Bin} = uart:read(In),
    ok = uart:write(Out, rot13(Bin)),
    bridge_loop(In, Out).

rot13(Bin) when is_binary(Bin) ->
    <<<<(rot13_char(C))>> || <<C>> <= Bin>>.

rot13_char(C) when C >= $a, C =< $z -> $a + (C - $a + 13) rem 26;
rot13_char(C) when C >= $A, C =< $Z -> $A + (C - $A + 13) rem 26;
rot13_char(C) -> C.
