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

-module(bs_create_bin_integer_unit).

-export([start/0]).

% Regression test for a JIT bs_create_bin codegen bug: building
% <<Var:Size/integer-unit:U>> with a literal Size and unit >= 2 crashed the
% aarch64 JIT with function_clause in shift_left/3, because the literal size
% was passed into mul/3's register slot. A literal value (<<42:5/...>>) takes a
% different, working path, so the value must be a runtime variable here; id/1
% defeats constant-folding.

start() ->
    ok = test_unit8_size5(),
    ok = test_unit8_size2(),
    ok = test_unit16_size1(),
    ok = test_variable_size(),
    0.

% <<V:5/integer-unit:8>> == <<V:40>>: the original minimal repro.
test_unit8_size5() ->
    V = id(258),
    Bin = <<V:5/integer-unit:8>>,
    40 = bit_size(Bin),
    <<0, 0, 0, 1, 2>> = Bin,
    ok.

% <<V:2/integer-unit:8>> == <<V:16>>.
test_unit8_size2() ->
    V = id(258),
    Bin = <<V:2/integer-unit:8>>,
    16 = bit_size(Bin),
    <<1, 2>> = Bin,
    ok.

% unit:16 with literal size 1 -> 16 bits.
test_unit16_size1() ->
    V = id(258),
    Bin = <<V:1/integer-unit:16>>,
    16 = bit_size(Bin),
    <<1, 2>> = Bin,
    ok.

% Regression guard: variable size must still work (it always did).
test_variable_size() ->
    V = id(258),
    Size = id(2),
    Bin = <<V:Size/integer-unit:8>>,
    16 = bit_size(Bin),
    <<1, 2>> = Bin,
    ok.

id(X) -> X.
