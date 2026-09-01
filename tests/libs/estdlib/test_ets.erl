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

new_table(Tuples) ->
    T = ets:new(test, []),
    true = ets:insert(T, Tuples),
    T.
