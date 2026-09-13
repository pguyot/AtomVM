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

-module(test_select_val_ranges).
-export([start/0, dense/1, ranges/1, signed_ranges/1, bounded/1]).
start() ->
    Integers = seq(-40, 50),
    Others = [
        true,
        false,
        ok,
        undefined,
        [],
        {value},
        <<1>>,
        0.0,
        1.0,
        -1.0,
        1 bsl 80,
        -(1 bsl 80),
        self(),
        make_ref(),
        fun() -> ok end
    ],
    foreach(
        fun(X) ->
            DenseExpected =
                case member(X, seq(0, 15)) of
                    true -> element(X + 1, {a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p});
                    false -> default
                end,
            DenseExpected = dense(X),
            RangeExpected =
                case X of
                    0 ->
                        exceptional;
                    _ ->
                        case member(X, [1] ++ seq(3, 18) ++ seq(20, 27)) of
                            true -> common;
                            false -> default
                        end
                end,
            RangeExpected = ranges(X),
            BoundedExpected =
                case X of
                    0 -> common;
                    1 -> common;
                    2 -> common;
                    3 -> exceptional;
                    _ -> default
                end,
            BoundedExpected = bounded(X),
            SignedExpected =
                case member(X, seq(-15, -3)) of
                    true ->
                        negative;
                    false ->
                        case member(X, seq(2, 14)) of
                            true -> positive;
                            false -> default
                        end
                end,
            SignedExpected = signed_ranges(X)
        end,
        Integers ++ Others
    ),
    0.
dense(X) ->
    case X of
        0 -> a;
        1 -> b;
        2 -> c;
        3 -> d;
        4 -> e;
        5 -> f;
        6 -> g;
        7 -> h;
        8 -> i;
        9 -> j;
        10 -> k;
        11 -> l;
        12 -> m;
        13 -> n;
        14 -> o;
        15 -> p;
        _ -> default
    end.
ranges(X) ->
    case X of
        0 -> exceptional;
        1 -> common;
        3 -> common;
        4 -> common;
        5 -> common;
        6 -> common;
        7 -> common;
        8 -> common;
        9 -> common;
        10 -> common;
        11 -> common;
        12 -> common;
        13 -> common;
        14 -> common;
        15 -> common;
        16 -> common;
        17 -> common;
        18 -> common;
        20 -> common;
        21 -> common;
        22 -> common;
        23 -> common;
        24 -> common;
        25 -> common;
        26 -> common;
        27 -> common;
        _ -> default
    end.
signed_ranges(X) ->
    case X of
        -15 -> negative;
        -14 -> negative;
        -13 -> negative;
        -12 -> negative;
        -11 -> negative;
        -10 -> negative;
        -9 -> negative;
        -8 -> negative;
        -7 -> negative;
        -6 -> negative;
        -5 -> negative;
        -4 -> negative;
        -3 -> negative;
        2 -> positive;
        3 -> positive;
        4 -> positive;
        5 -> positive;
        6 -> positive;
        7 -> positive;
        8 -> positive;
        9 -> positive;
        10 -> positive;
        11 -> positive;
        12 -> positive;
        13 -> positive;
        14 -> positive;
        _ -> default
    end.

bounded(X) when is_integer(X), X >= 0, X =< 50 ->
    case X of
        0 -> common;
        1 -> common;
        2 -> common;
        3 -> exceptional;
        _ -> default
    end;
bounded(_) ->
    default.

%% The C test runner uses a minimal lists module, so keep the oracle local.
seq(First, Last) when First =< Last -> [First | seq(First + 1, Last)];
seq(_, _) -> [].
member(_, []) ->
    false;
member(Value, [Head | Tail]) ->
    case Value =:= Head of
        true -> true;
        false -> member(Value, Tail)
    end.
foreach(_, []) ->
    ok;
foreach(Fun, [Head | Tail]) ->
    Fun(Head),
    foreach(Fun, Tail).
