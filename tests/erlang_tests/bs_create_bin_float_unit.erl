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

-module(bs_create_bin_float_unit).

-export([start/0, id/1]).

% Regression test for a float bit-syntax bug (JIT + emulator): a segment
% <<F:Size/float-unit:Unit>> has a field width of Size * Unit bits, which must
% be 16, 32 or 64. AtomVM validated and dispatched the float encoder on Size
% alone, ignoring Unit: it accepted invalid widths (writing a wrong-width
% float instead of badarg) and, for valid unit /= 1 combinations, wrote the
% Size-width float instead of the (Size * Unit)-width one. id/1 defeats
% constant-folding so the segment reaches the runtime handler.

start() ->
    ok = test_unit_makes_valid_width(),
    ok = test_unit_invalid_width_badarg(),
    ok = test_get_with_unit(),
    0.

% Valid total widths via unit: 8*2=16, 16*2=32, 32*2=64.
test_unit_makes_valid_width() ->
    F = id(1.0),
    B16 = <<F:8/float-unit:2>>,
    16 = bit_size(B16),
    <<F:16/float>> = B16,
    B32 = <<F:16/float-unit:2>>,
    32 = bit_size(B32),
    <<F:32/float>> = B32,
    B64 = <<F:32/float-unit:2>>,
    64 = bit_size(B64),
    <<F:64/float>> = B64,
    ok.

% Total widths that are not 16/32/64 must raise badarg even though Size alone
% is one of {16,32,64}.
test_unit_invalid_width_badarg() ->
    F = id(1.0),
    ok = assert_badarg(fun() -> <<F:16/float-unit:242>> end),
    ok = assert_badarg(fun() -> <<F:64/float-unit:2>> end),
    ok = assert_badarg(fun() -> <<F:16/float-unit:3>> end),
    ok.

% The match side must also honour unit: <<X:16/float-unit:2>> reads 32 bits.
test_get_with_unit() ->
    F = id(3.5),
    Bin = id(<<F:32/float>>),
    <<X:16/float-unit:2>> = Bin,
    F = X,
    32 = bit_size(Bin),
    ok.

id(X) -> X.

assert_badarg(Fun) ->
    try
        R = Fun(),
        {fail_no_ex, R}
    catch
        error:badarg -> ok
    end.
