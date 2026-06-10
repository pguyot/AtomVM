%
% This file is part of AtomVM.
%
% Copyright 2019-2020 Fred Dushin <fred@dushin.net>
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

-module(test_proplists).

-export([test/0]).

test() ->
    ok = test_get_value(),
    ok = test_substitute_aliases(),
    ok = test_substitute_negations(),
    ok = test_expand(),
    ok = test_normalize(),
    ok.

test_substitute_aliases() ->
    ok = etest:assert_match(
        proplists:substitute_aliases([{color, colour}], [{color, red}, color, size]),
        [{colour, red}, colour, size]
    ),
    ok = etest:assert_match(proplists:substitute_aliases([], [a, {b, 1}]), [a, {b, 1}]),
    ok.

test_substitute_negations() ->
    ok = etest:assert_match(
        proplists:substitute_negations(
            [{no_foo, foo}], [no_foo, {no_foo, true}, {no_foo, false}, bar]
        ),
        [{foo, false}, {foo, false}, foo, bar]
    ),
    ok.

test_expand() ->
    ok = etest:assert_match(
        proplists:expand([{fast, [{speed, 9}, low_mem]}], [slim, fast, low]),
        [slim, {speed, 9}, low_mem, low]
    ),
    % a non-matching minimal representation is kept as is
    ok = etest:assert_match(
        proplists:expand([{{foo, true}, [bar]}], [fie, {foo, false}, fum]),
        [fie, {foo, false}, fum]
    ),
    % later entries with the same key are deleted on expansion
    ok = etest:assert_match(
        proplists:expand([{foo, [bar]}], [fie, foo, fum, foo]),
        [fie, bar, fum]
    ),
    ok.

test_normalize() ->
    % the form beam_ssa_opt uses
    ok = etest:assert_match(
        proplists:normalize(
            [no_copt, {inline, true}, verbose], [{negations, [{no_copt, copt}]}]
        ),
        [{copt, false}, inline, verbose]
    ),
    ok = etest:assert_match(
        proplists:normalize(
            [{color, red}, no_foo],
            [
                {aliases, [{color, colour}]},
                {negations, [{no_foo, foo}]},
                {expand, [{colour, [paint]}]}
            ]
        ),
        [{colour, red}, {foo, false}]
    ),
    ok.

test_get_value() ->
    ok = etest:assert_match(proplists:get_value(a, []), undefined),
    ok = etest:assert_match(proplists:get_value(a, [a]), true),
    ok = etest:assert_match(proplists:get_value(a, [{a, foo}]), foo),

    ok = etest:assert_match(proplists:get_value(a, [], gnu), gnu),
    ok = etest:assert_match(proplists:get_value(a, [a], gnu), true),
    ok = etest:assert_match(proplists:get_value(a, [{a, foo}], gnu), foo),
    ok = etest:assert_match(proplists:get_value(b, [{a, foo}], gnu), gnu),
    ok.
