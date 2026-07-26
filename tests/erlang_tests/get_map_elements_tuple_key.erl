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

%% @doc get_map_elements over the key shapes the factorized map-get stub
%% decides inline (see map_get_stub_call in the backends): immediate keys and
%% 2-tuples of immediates -- the {Name, Index} shape the compiler itself uses
%% -- both on a hit and on a definitive not-found, plus the shapes it must
%% hand to the C path (tree maps, boxed keys, non-map arguments).
-module(get_map_elements_tuple_key).

-export([start/0, id/1]).

id(X) -> X.

%% Matched out of a map with literal keys, so the compiler emits
%% get_map_elements with the corresponding key shapes.
imm_hit(M) ->
    #{a := A, 7 := B} = M,
    {A, B}.

imm_miss(M) ->
    case M of
        #{zzz := V} -> {found, V};
        _ -> not_found
    end.

tuple_hit(M) ->
    #{{x, 1} := A, {y, 2} := B} = M,
    {A, B}.

tuple_miss(M) ->
    case M of
        #{{nope, 9} := V} -> {found, V};
        _ -> not_found
    end.

boxed_key_hit(M) ->
    case M of
        #{<<"bin">> := V} -> {found, V};
        _ -> not_found
    end.

%% A map big enough to be stored as a tree rather than a flat map: the stub
%% must report unsupported and let the C path decide.
big_map() ->
    maps:from_list([{K, K * 2} || K <- lists:seq(1, 80)]).

start() ->
    Flat = id(#{a => 1, 7 => 2, {x, 1} => 3, {y, 2} => 4, <<"bin">> => 5}),

    {1, 2} = imm_hit(Flat),
    not_found = imm_miss(Flat),
    {3, 4} = tuple_hit(Flat),
    not_found = tuple_miss(Flat),
    {found, 5} = boxed_key_hit(Flat),

    %% a key that is a 2-tuple with a non-immediate element must not be
    %% decided inline, and must still find its entry
    Deep = id(#{{k, <<"v">>} => 6}),
    case Deep of
        #{{k, <<"v">>} := 6} -> ok
    end,

    %% tree map: every lookup goes through the C path
    Big = id(big_map()),
    case Big of
        #{40 := 80} -> ok
    end,
    case Big of
        #{999 := _} -> erlang:error(unexpected);
        _ -> ok
    end,

    %% a one-entry flat map, exercising the loop bound at both ends
    One = id(#{only => 11}),
    case One of
        #{only := 11} -> ok
    end,
    not_found = imm_miss(One),

    %% an empty map has no keys at all to scan
    Empty = id(#{}),
    not_found = imm_miss(Empty),

    %% non-map arguments must still raise, not be decided by the stub
    badmatch =
        try imm_hit(id(not_a_map)) of
            _ -> no_error
        catch
            error:{badmatch, _} -> badmatch
        end,

    0.
