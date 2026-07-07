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

-module(test_map_tree_order).

-export([start/0, id/1]).

%% Tree-backed (> 32 entries) maps with key families chosen to collide on any
%% truncated order-key prefix: atoms sharing long name prefixes, integers at
%% and beyond the small/boxed boundary, floats whose truncation ties, tuples
%% equal in arity and first element, deep lists, and binaries with long
%% common prefixes. Lookups, updates and deletions must stay exact, and the
%% incrementally-built map must be canonically equal (same key order, same
%% external encoding) to one bulk-built by maps:from_list. Uses only
%% BIF/NIF-backed functions so it runs without the standard library.
start() ->
    Keys = id(adversarial_keys()),
    N = length(Keys),
    true = N > 90,
    KVs = index_kvs(Keys, 1, []),
    M = build(KVs, #{}),
    N = map_size(M),
    ok = check_get(KVs, M),
    %% Canonical-order oracle: bulk construction (sorts once) and the
    %% incremental tree must agree structurally and byte-for-byte.
    MRef = maps:from_list(KVs),
    true = M =:= MRef,
    true = term_to_binary(M) =:= term_to_binary(MRef),
    %% Update every entry through the tree path and re-check.
    KV2 = update_values(KVs, []),
    M2 = build(KV2, M),
    N = map_size(M2),
    ok = check_get(KV2, M2),
    true = M2 =:= maps:from_list(KV2),
    %% Deletion down through the flat threshold keeps lookups exact.
    M3 = remove_alternate(KVs, M2, 0),
    ok = check_alternate(KV2, M3, 0),
    0.

adversarial_keys() ->
    LongPrefixAtoms = make_atoms("a_very_long_common_atom_prefix_", 20, []),
    BoundaryInts = [
        0,
        -1,
        1,
        16#7FFFFFF,
        -16#8000000,
        16#3FFFFFFFFFFFFF,
        -16#40000000000000,
        16#7FFFFFFFFFFFFFF,
        -16#800000000000000,
        1 bsl 62,
        -(1 bsl 62),
        1 bsl 80,
        -(1 bsl 80),
        (1 bsl 80) + 1
    ],
    TruncTieFloats = [-2.5, -2.25, -2.0, 2.0, 2.25, 2.5, 0.0, -0.75, 0.75],
    SameHeadTuples = make_tuples(20, []),
    SameHeadTuples2 = make_atom_tuples(10, []),
    DeepLists = make_lists(10, []),
    PrefixBinaries = make_binaries(10, []),
    ShortBinaries = [<<>>, <<0>>, <<0, 0>>, <<1>>, <<"a_long_common_binary_prefix_0123456789">>],
    Mixed = [nil, [], {}, #{}, {1}, {1, 2, 3}],
    LongPrefixAtoms ++ BoundaryInts ++ TruncTieFloats ++ SameHeadTuples ++
        SameHeadTuples2 ++ DeepLists ++ PrefixBinaries ++ ShortBinaries ++ Mixed.

make_atoms(_Prefix, 0, Acc) ->
    Acc;
make_atoms(Prefix, I, Acc) ->
    A = list_to_atom(Prefix ++ integer_to_list(I)),
    make_atoms(Prefix, I - 1, [A | Acc]).

make_tuples(0, Acc) ->
    Acc;
make_tuples(I, Acc) ->
    make_tuples(I - 1, [{b_var, I} | Acc]).

make_atom_tuples(0, Acc) ->
    Acc;
make_atom_tuples(I, Acc) ->
    make_atom_tuples(I - 1, [{b_var, list_to_atom("v" ++ integer_to_list(I))} | Acc]).

make_lists(0, Acc) ->
    Acc;
make_lists(I, Acc) ->
    make_lists(I - 1, [[common, prefix, I] | Acc]).

make_binaries(-1, Acc) ->
    Acc;
make_binaries(I, Acc) ->
    B = <<"a_long_common_binary_prefix_0123456789", I:8>>,
    make_binaries(I - 1, [B | Acc]).

index_kvs([], _I, Acc) ->
    lists:reverse(Acc);
index_kvs([K | T], I, Acc) ->
    index_kvs(T, I + 1, [{K, I} | Acc]).

update_values([], Acc) ->
    lists:reverse(Acc);
update_values([{K, I} | T], Acc) ->
    update_values(T, [{K, {v, I}} | Acc]).

build([], M) ->
    M;
build([{K, V} | T], M) ->
    %% Use the map-update syntax (put_map_assoc opcode) rather than maps:put/3:
    %% the latter is only inlined by the compiler on OTP 28+, and on earlier
    %% versions becomes an external call to the maps module, which is not on
    %% the load path for standalone erlang_tests (unlike from_list/remove,
    %% which are NIFs). This keeps the test stdlib-free on every OTP release.
    build(T, M#{K => V}).

check_get([], _M) ->
    ok;
check_get([{K, V} | T], M) ->
    V = maps:get(K, M),
    true = is_map_key(K, M),
    check_get(T, M).

remove_alternate([], M, _I) ->
    M;
remove_alternate([{K, _} | T], M, I) when I rem 2 =:= 0 ->
    remove_alternate(T, maps:remove(K, M), I + 1);
remove_alternate([_ | T], M, I) ->
    remove_alternate(T, M, I + 1).

check_alternate([], _M, _I) ->
    ok;
check_alternate([{K, _} | T], M, I) when I rem 2 =:= 0 ->
    false = is_map_key(K, M),
    check_alternate(T, M, I + 1);
check_alternate([{K, V} | T], M, I) ->
    V = maps:get(K, M),
    check_alternate(T, M, I + 1).

id(X) ->
    X.
