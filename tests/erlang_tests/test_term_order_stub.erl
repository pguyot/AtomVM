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

%% @doc Term ordering across every shape the factorized compare stub decides
%% inline (see compare_stub_call in the backends) and the ones it must leave to
%% the C comparator: identical words, small integers (signed), a small integer
%% against another immediate, immediates against lists, list heads and tails,
%% tuples by arity then by leftmost unequal element, atoms by their cached
%% 8-byte name sort_key -- including pairs whose sort_key ties and so must fall
%% back -- and boxed operands (floats, bignums, binaries, maps), which rank by
%% rules the stub deliberately does not know.
%%
%% The expected order is BEAM's, so this is a real oracle rather than a
%% restatement of the implementation.
-module(test_term_order_stub).

-export([start/0, terms/0, id/1]).

id(X) -> X.

terms() ->
    [
        -5,
        0,
        7,
        12345678901234567890,
        -12345678901234567890,
        1.5,
        -2.5,
        a,
        b,
        zz,
        '',
        abcdefgh,
        abcdefghi,
        abcdefghij,
        abcdefgh_x,
        abcdefgh_y,
        'aaaaaaaaaaaaaaaaaaaa1',
        'aaaaaaaaaaaaaaaaaaaa2',
        [],
        [1],
        [1, 2],
        [2],
        [a],
        [1 | 2],
        [[1]],
        [1, 2, 3],
        {},
        {1},
        {1, 2},
        {2},
        {a, b},
        {1, 2, 3},
        {x, 1},
        {y, 2},
        <<>>,
        <<1>>,
        <<"bin">>,
        #{a => 1},
        #{b => 2}
    ].

expected() ->
    [
        -12345678901234567890,
        -5,
        -2.5,
        0,
        1.5,
        7,
        12345678901234567890,
        '',
        a,
        'aaaaaaaaaaaaaaaaaaaa1',
        'aaaaaaaaaaaaaaaaaaaa2',
        abcdefgh,
        abcdefgh_x,
        abcdefgh_y,
        abcdefghi,
        abcdefghij,
        b,
        zz,
        {},
        {1},
        {2},
        {1, 2},
        {a, b},
        {x, 1},
        {y, 2},
        {1, 2, 3},
        #{a => 1},
        #{b => 2},
        [],
        [1 | 2],
        [1],
        [1, 2],
        [1, 2, 3],
        [2],
        [a],
        [[1]],
        <<>>,
        <<1>>,
        <<"bin">>
    ].

start() ->
    Sorted = lists:sort(terms()),
    Sorted = expected(),

    %% Pairwise consistency over the whole matrix: for every ordered pair in
    %% the sorted list the comparison operators must agree with the position.
    ok = check_pairs(Sorted),

    %% Identical operands compare equal under every operator, whatever shape.
    ok = check_identity(terms()),

    %% Exact vs arithmetic equality on shapes the stub sees: it must never
    %% decide 1 == 1.0 (a boxed float against an immediate small integer).
    true = id(1) == id(1.0),
    false = id(1) =:= id(1.0),
    true = id(1) < id(1.5),
    true = id(-2.5) < id(0),

    %% Atoms whose sort_key ties (a shared 8-byte name prefix) still order by
    %% full name through the C fallback.
    true = id('aaaaaaaaaaaaaaaaaaaa1') < id('aaaaaaaaaaaaaaaaaaaa2'),
    true = id(abcdefgh) < id(abcdefgh_x),
    true = id(abcdefgh_x) < id(abcdefgh_y),
    true = id(abcdefghi) < id(abcdefghij),

    %% Tuples: arity first, then the leftmost unequal element.
    true = id({2}) < id({1, 2}),
    true = id({1, 2}) < id({a, b}),
    true = id({x, 1}) < id({y, 2}),

    %% Lists: leftmost unequal head, else the tails; an improper tail falls
    %% out of the loop into the scalar rules.
    true = id([1 | 2]) < id([1]),
    true = id([1, 2]) < id([2]),
    true = id([1, 2]) < id([1, 2, 3]),

    0.

check_pairs([]) ->
    ok;
check_pairs([H | T]) ->
    ok = check_against(H, T),
    check_pairs(T).

check_against(_A, []) ->
    ok;
check_against(A, [B | T]) ->
    %% A precedes B in the sorted order, so A =< B and not B < A.
    true = id(A) =< id(B),
    false = id(B) < id(A),
    check_against(A, T).

check_identity([]) ->
    ok;
check_identity([H | T]) ->
    true = id(H) =:= id(H),
    true = id(H) == id(H),
    false = id(H) < id(H),
    true = id(H) =< id(H),
    check_identity(T).
