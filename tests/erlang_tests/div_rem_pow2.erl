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

%% `div'/`rem' by a power-of-two literal with a dividend of unknown sign: the
%% JIT strength-reduces these to shifts, and an arithmetic shift floors while
%% Erlang truncates toward zero, so negative dividends are the interesting
%% cases (-7 div 2 = -3, not -4; -7 rem 2 = -1, not 1).
-module(div_rem_pow2).

-export([start/0, id/1]).

start() ->
    sum(vals(), 0).

vals() ->
    [
        0,
        1,
        2,
        3,
        7,
        8,
        9,
        15,
        16,
        -1,
        -2,
        -3,
        -7,
        -8,
        -9,
        -15,
        -16,
        1023,
        1024,
        1025,
        -1023,
        -1024,
        -1025,
        134217727,
        -134217728
    ].

sum([], Acc) ->
    Acc;
sum([V0 | Tail], Acc) ->
    V = id(V0),
    sum(Tail, Acc + weigh(V)).

weigh(V) ->
    (V div 2) + (V rem 2) * 3 +
        (V div 4) * 5 + (V rem 4) * 7 +
        (V div 8) * 11 + (V rem 8) * 13 +
        (V div 16) * 17 + (V rem 16) * 19 +
        (V div 1024) * 23 + (V rem 1024) * 29 +
        (V div 1048576) * 31 + (V rem 1048576).

id(X) ->
    X.
