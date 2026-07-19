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

-module(jit_stream_flash_mock).

% Host-side model of jit_stream_flash for the compiler test suite: a binary
% with a flush horizon. It enforces the flash discipline strictly (even a bit
% stricter than the real per-page stream: the horizon is byte-exact):
% - flush_upto/2 advances the horizon;
% - replace/3 below the horizon must only clear bits (0xFF-placeholder
%   patches), like NOR flash programming without erase;
% - reset/2 (backtrack re-emit) cannot rewind below the horizon.

-export([
    new/1,
    offset/1,
    committed_offset/1,
    append/2,
    replace/3,
    reset/2,
    flush_upto/2,
    flush/1
]).

new(_MaxSize) ->
    {<<>>, 0}.

offset({Bin, _Horizon}) ->
    byte_size(Bin).

committed_offset({_Bin, Horizon}) ->
    Horizon.

append({Bin, Horizon}, Binary) ->
    {<<Bin/binary, Binary/binary>>, Horizon}.

replace({Bin, Horizon}, ReplaceOffset, Binary) ->
    Size = byte_size(Binary),
    <<Pre:ReplaceOffset/binary, Current:Size/binary, Post/binary>> = Bin,
    case ReplaceOffset < Horizon of
        true ->
            %% Below the flush horizon: flashed content, bit-clear only.
            ok = assert_bit_clear(Current, Binary, ReplaceOffset);
        false ->
            ok
    end,
    {<<Pre/binary, Binary/binary, Post/binary>>, Horizon}.

reset({Bin, Horizon}, NewOffset) when NewOffset >= Horizon ->
    <<Keep:NewOffset/binary, _/binary>> = Bin,
    {Keep, Horizon}.

flush_upto({Bin, Horizon}, Offset) ->
    {Bin, max(Horizon, min(Offset, byte_size(Bin)))}.

flush({Bin, _Horizon}) ->
    {Bin, byte_size(Bin)}.

assert_bit_clear(<<>>, <<>>, _Offset) ->
    ok;
assert_bit_clear(<<C, CurRest/binary>>, <<N, NewRest/binary>>, Offset) ->
    case (bnot C) band N band 16#FF of
        0 -> assert_bit_clear(CurRest, NewRest, Offset + 1);
        Bits -> error({bits_set_below_flush_horizon, Offset, C, N, Bits})
    end.
