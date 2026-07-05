% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% Micro-benchmark isolating the JIT opcode fusions:
%%   list_match     - is_nonempty_list + get_list        (fusion D)
%%   record_access  - is_tagged_tuple + get_tuple_element (fusion E)
%%   bin_decode     - bs_match fixed-size integer reads   (fusion H)
%%
%% Each op is a tight loop where the fused sequence is a large fraction of the
%% per-iteration work, so a per-op A/B ratio reflects the fusion's effect much
%% more directly than the whole-app benchmark (where these ops are a small
%% fraction of runtime). Prints "<op>: <microseconds>" lines; the `checks` line
%% must be identical between baseline and fusion builds (correctness guard).
-module(fusion_microbench).
-export([start/0]).
-record(rec, {a, b, c, d}).

start() ->
    L = mklist(2000, []),
    T0 = erlang:monotonic_time(microsecond),
    R1 = loop_list(3000, L, 0),
    T1 = erlang:monotonic_time(microsecond),
    R2 = loop_rec(600000, #rec{a = 1, b = 2, c = 3, d = 4}, 0),
    T2 = erlang:monotonic_time(microsecond),
    Bin = <<<<X:8, (X * 7):16, (X * 13):32>> || X <- lists:seq(0, 199)>>,
    R3 = loop_bin(3000, Bin, 0),
    T3 = erlang:monotonic_time(microsecond),
    io:format("list_match: ~p~n", [T1 - T0]),
    io:format("record_access: ~p~n", [T2 - T1]),
    io:format("bin_decode: ~p~n", [T3 - T2]),
    io:format("checks: ~p ~p ~p~n", [R1, R2, R3]),
    ok.

mklist(0, Acc) -> Acc;
mklist(N, Acc) -> mklist(N - 1, [N | Acc]).

%% is_nonempty_list + get_list on each cons cell.
loop_list(0, _L, Acc) -> Acc;
loop_list(N, L, Acc) -> loop_list(N - 1, L, Acc + sum_list(L, 0)).
sum_list([H | T], Acc) -> sum_list(T, Acc + H);
sum_list([], Acc) -> Acc.

%% is_tagged_tuple + four get_tuple_element on each iteration.
loop_rec(0, _R, Acc) ->
    Acc;
loop_rec(N, R, Acc) ->
    #rec{a = A, b = B, c = C, d = D} = R,
    loop_rec(N - 1, R, Acc + A + B + C + D).

%% bs_match: ensure + fixed 8/16/32-bit integer reads on each record.
loop_bin(0, _B, Acc) -> Acc;
loop_bin(N, B, Acc) -> loop_bin(N - 1, B, Acc + sum_bin(B, 0)).
sum_bin(<<A:8, B:16, C:32, Rest/binary>>, Acc) -> sum_bin(Rest, Acc + A + B + C);
sum_bin(<<>>, Acc) -> Acc.
