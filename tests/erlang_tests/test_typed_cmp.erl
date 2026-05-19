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

-module(test_typed_cmp).

-export([
    start/0,
    ne_exact_lengths/2,
    eq_exact_lengths/2,
    lt_lengths/2
]).

% =/= between two typed t_integer registers whose ranges fit small_integer_bounds.
% length/1 returns a typed integer in {0, 2^58 - 1} which is within bounds on 64-bit,
% so the JIT's op_is_not_eq_exact takes the inlined tagged-cmp path.
ne_exact_lengths(A, B) when is_list(A), is_list(B), length(A) =/= length(B) ->
    not_equal;
ne_exact_lengths(_, _) ->
    equal.

% Mirror for is_eq_exact (already covered by other tests but kept here for symmetry).
eq_exact_lengths(A, B) when is_list(A), is_list(B), length(A) =:= length(B) ->
    equal;
eq_exact_lengths(_, _) ->
    not_equal.

% Mirror for is_lt typed/typed.
lt_lengths(A, B) when is_list(A), is_list(B), length(A) < length(B) ->
    less;
lt_lengths(_, _) ->
    not_less.

start() ->
    not_equal = ?MODULE:ne_exact_lengths([1, 2], [1, 2, 3]),
    equal = ?MODULE:ne_exact_lengths([1, 2], [a, b]),
    equal = ?MODULE:ne_exact_lengths([], []),
    equal = ?MODULE:ne_exact_lengths(not_a_list, [1]),

    equal = ?MODULE:eq_exact_lengths([], []),
    equal = ?MODULE:eq_exact_lengths([1, 2], [a, b]),
    not_equal = ?MODULE:eq_exact_lengths([1, 2], [1, 2, 3]),

    less = ?MODULE:lt_lengths([1], [1, 2]),
    not_less = ?MODULE:lt_lengths([1, 2], [1]),
    not_less = ?MODULE:lt_lengths([], []),

    0.
