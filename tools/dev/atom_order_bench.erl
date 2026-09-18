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

%% Atom ordering, isolated: sorting a list of atoms is nothing but
%% atom_table_cmp_using_atom_index, which is what an order rank replaces.
%% Run as: AtomVM libs/atomvmlib-<target>.avm atom_order_bench.beam
-module(atom_order_bench).
-export([start/0]).

%% Two atom name shapes: one where the first eight bytes tie (the shape the
%% census found in erl_parse, where 63.7% of atom orderings share more than
%% eight characters), and one where they differ in byte one.
start() ->
    N = 4000,
    Common = [list_to_atom("bench_atom_" ++ integer_to_list(I)) || I <- lists:seq(1, N)],
    Distinct = [list_to_atom(integer_to_list(I) ++ "_bench_atom") || I <- lists:seq(1, N)],
    run("common_prefix", shuffle(Common)),
    run("distinct_prefix", shuffle(Distinct)),
    ok.

run(Label, L) ->
    warm(L, 2),
    T0 = erlang:monotonic_time(microsecond),
    warm(L, 20),
    T1 = erlang:monotonic_time(microsecond),
    io:format("~s sort_us ~p~n", [Label, T1 - T0]).

warm(_L, 0) ->
    ok;
warm(L, N) ->
    _ = lists:sort(L),
    warm(L, N - 1).

shuffle(L) ->
    [X || {_, X} <- lists:sort([{erlang:phash2({I, seed}), X} || {I, X} <- enum(L, 1)])].

enum([], _) -> [];
enum([H | T], I) -> [{I, H} | enum(T, I + 1)].
