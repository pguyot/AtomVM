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

%% Tests for the parts of `ets' that AtomVM implements in Erlang on top of the
%% ETS NIFs, and so cannot be exercised by `tests/erlang_tests/test_ets.erl'.
-module(test_ets).

-export([test/0]).

test() ->
    ok = test_select(),
    ok = test_match(),
    ok = test_info(),
    ok = test_keypos(),
    ok = test_bag(),
    ok.

test_select() ->
    T = new_table([{a, 1}, {b, 2}, {c, 3}]),
    [a, b] = lists:sort(ets:select(T, [{{'$1', '$2'}, [{'<', '$2', 3}], ['$1']}])),
    [{c, 3}] = ets:select(T, [{{c, '_'}, [], ['$_']}]),
    2 = ets:select_count(T, [{{'_', '$1'}, [{'>', '$1', 1}], [true]}]),

    % '$$' lists the bindings in numeric order: '$10' after '$2', not before it.
    Wide = new_table([{k, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}]),
    WidePattern = {'$1', '$2', '$3', '$4', '$5', '$6', '$7', '$8', '$9', '$10', '$11'},
    [[k, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]] = ets:select(Wide, [{WidePattern, [], ['$$']}]),
    ok.

test_match() ->
    T = new_table([{a, 1}, {b, 2}, {c, 1}]),
    [[a], [c]] = lists:sort(ets:match(T, {'$1', 1})),
    [[2]] = ets:match(T, {b, '$1'}),
    [[], []] = ets:match(T, {'_', 1}),
    [] = ets:match(T, {missing, '$1'}),

    % A variable repeated in the pattern must bind to the same value everywhere.
    Pairs = new_table([{1, 1}, {2, 3}]),
    [[1]] = ets:match(Pairs, {'$1', '$1'}),
    ok.

%% select/2 narrows to a lookup when the key element of the pattern is ground,
%% so it has to find the key at the table's own keypos, not at element 1.
test_keypos() ->
    T = ets:new(test, [{keypos, 2}]),
    true = ets:insert(T, [{a, 1}, {b, 2}, {c, 3}]),
    [[a]] = ets:match(T, {'$1', 1}),
    [[c]] = ets:match(T, {'$1', 3}),
    [] = ets:match(T, {'$1', 9}),
    [[a, 1], [b, 2], [c, 3]] = lists:sort(ets:match(T, {'$1', '$2'})),
    [{b, 2}] = ets:select(T, [{{'_', 2}, [], ['$_']}]),
    % Ground at element 1 but not at the keypos: still a full traversal.
    [[1]] = ets:match(T, {a, '$1'}),
    true = ets:match_delete(T, {'_', 2}),
    [] = ets:match(T, {'$1', 2}),
    [[a, 1], [c, 3]] = lists:sort(ets:match(T, {'$1', '$2'})),
    ok.

%% A bag holds several objects under one key: narrowing must return them all.
test_bag() ->
    T = ets:new(test, [bag]),
    true = ets:insert(T, [{k, 1}, {k, 2}, {j, 3}]),
    [[1], [2]] = lists:sort(ets:match(T, {k, '$1'})),
    [[j], [k], [k]] = lists:sort(ets:match(T, {'$1', '_'})),
    true = ets:match_delete(T, {k, 1}),
    [[2]] = ets:match(T, {k, '$1'}),
    [[j], [k]] = lists:sort(ets:match(T, {'$1', '_'})),
    ok.

test_info() ->
    T = new_table([{a, 1}, {b, 2}]),
    1 = ets:info(T, keypos),
    2 = ets:info(T, size),
    set = ets:info(T, type),
    test = ets:info(T, name),
    false = ets:info(T, named_table),
    protected = ets:info(T, protection),
    undefined = ets:info(T, no_such_item),
    Bag = ets:new(other, [bag, private, {keypos, 3}]),
    3 = ets:info(Bag, keypos),
    0 = ets:info(Bag, size),
    bag = ets:info(Bag, type),
    private = ets:info(Bag, protection),
    ok.

new_table(Tuples) ->
    T = ets:new(test, []),
    true = ets:insert(T, Tuples),
    T.
