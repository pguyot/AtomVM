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

%% Regression test: a binary segment in bs_create_bin with a *literal* size
%% larger than the source binary must raise badarg, not read past the end of
%% the source (which crashed the JIT with SIGSEGV/SIGBUS).
-module(test_bs_create_bin_oversize).

-export([start/0]).

start() ->
    ok = test_oversize_empty(),
    ok = test_oversize_partial(),
    ok = test_oversize_unit(),
    ok = test_exact_size(),
    0.

%% Source is empty, ask for 4 bytes -> badarg.
test_oversize_empty() ->
    badarg = build4(id(<<>>)),
    ok.

%% Source has 2 bytes, ask for 4 bytes -> badarg.
test_oversize_partial() ->
    badarg = build4(id(<<1, 2>>)),
    ok.

%% Source has 1 byte, ask for 2 units of 16 bits (4 bytes) -> badarg.
test_oversize_unit() ->
    badarg = build_unit(id(<<1>>)),
    ok.

%% Source has exactly 4 bytes -> succeeds.
test_exact_size() ->
    <<1, 2, 3, 4>> = build4(id(<<1, 2, 3, 4>>)),
    ok.

build4(Bin) ->
    try
        <<Bin:4/binary>>
    catch
        error:badarg -> badarg
    end.

build_unit(Bin) ->
    try
        <<Bin:2/binary-unit:16>>
    catch
        error:badarg -> badarg
    end.

id(X) -> X.
