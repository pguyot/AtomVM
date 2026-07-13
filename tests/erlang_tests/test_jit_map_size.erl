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

-module(test_jit_map_size).

-export([start/0, check/1]).

%% Exercises the JIT inline for erlang:map_size/1 on a value the Type chunk
%% proves is a map (the is_map/1 guard gives the operand a plain t_map type).
%% Covers both the flat (small) and tree (large) map representations, including
%% the empty map, so a wrong header/size read is caught behaviourally.

start() ->
    0 = check(mk(0)),
    1 = check(mk(1)),
    2 = check(mk(2)),
    7 = check(mk(7)),
    32 = check(mk(32)),
    100 = check(mk(100)),
    3 = check(id(#{x => 1, y => 2, z => 3})),
    0.

check(M) when is_map(M) -> map_size(M).

mk(N) -> maps:from_list([{K, K} || K <- lists:seq(1, N)]).

id(X) -> X.
