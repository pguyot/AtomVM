%
% This file is part of AtomVM.
%
% Copyright 2020 Davide Bettio <davide@uninstall.it>
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

-module(test_dict).

-export([start/0, put_int/1, stringize/1, factorial/1, id/1, the_get/1, the_erase/1]).

start() ->
    ok = test_get_0_erase_0(),
    put_int(0),
    X = put_int(1),
    put_int(2),
    put(6, X),
    put_int(3),
    put_int(4),
    put_int(5),
    Y = stringize(factorial(8)),
    Z = stringize(factorial(9)),
    [H | _T] = stringize(erlang:list_to_integer(Z) - erlang:list_to_integer(Y)),
    _ = stringize(factorial(6)),
    _ = stringize(factorial(7)),
    _ = stringize(123456),
    _ = stringize(876543),
    W = erlang:list_to_integer(the_get(stringize((H - $0) - 2)) ++ the_erase(id(6))),
    _ = stringize(111222),
    _ = stringize(333222),
    "undefined" = erlang:atom_to_list(the_erase(6)),
    ok = test_put_get_erase(),
    ok = test_immediate_key_identity(),
    W.

%% dictionary_find scans immediate keys by word equality, which is only correct
%% because a value fitting the small-int range is never boxed. Look the key up
%% again through terms built by producers other than the literal: arithmetic,
%% binary_to_term, and a wide bit-syntax segment, which goes through the bigint
%% builder and so must be normalized back to a small int.
test_immediate_key_identity() ->
    undefined = put(id(7), seven),
    seven = get(id(3 + 4)),
    seven = get(binary_to_term(term_to_binary(id(7)))),
    seven = get(u72(<<7:72>>)),
    undefined = put(id(-7), minus_seven),
    minus_seven = get(id(0 - 7)),
    minus_seven = get(binary_to_term(term_to_binary(id(-7)))),
    minus_seven = get(s72(<<-7:72/signed>>)),
    seven = erase(id(7)),
    minus_seven = erase(id(-7)),
    undefined = get(id(7)),
    ok.

put_int(N) ->
    put(stringize(id(factorial(id(N)))), stringize(id(factorial(id(N))) * 2)).

stringize(I) when is_integer(I) ->
    erlang:integer_to_list(I) ++ "0";
stringize(_I) ->
    "".

factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N - 1).

id(X) when is_integer(X) ->
    X;
id(_X) ->
    0.

the_get(L) when is_list(L) ->
    get(L);
the_get(_X) ->
    "-1".

the_erase(N) when is_integer(N) ->
    erase(N);
the_erase(_X) ->
    "0".

test_put_get_erase() ->
    undefined = put({any_term}, 1),
    1 = get({any_term}),
    1 = apply(erlang, list_to_atom("put"), [{any_term}, 2]),
    2 = apply(erlang, list_to_atom("get"), [{any_term}]),
    2 = apply(erlang, list_to_atom("erase"), [{any_term}]),
    undefined = erase({any_term}),
    ok.

test_get_0_erase_0() ->
    [] = get(),
    undefined = put({any_term}, 1),
    [{{any_term}, 1}] = get(),
    [{{any_term}, 1}] = erase(),
    [] = get(),
    [] = erase(),
    ok.

u72(Bin) ->
    <<X:72>> = Bin,
    X.

s72(Bin) ->
    <<X:72/signed>> = Bin,
    X.
