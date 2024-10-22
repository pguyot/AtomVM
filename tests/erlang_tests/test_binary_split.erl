%
% This file is part of AtomVM.
%
% Copyright 2019 Davide Bettio <davide@uninstall.it>
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

-module(test_binary_split).

-export([start/0, split_compare/3, split_compare/2, compare_bin/2, fail_split/1]).

start() ->
    ok = test_alloc(),
    split_compare(<<"Hello:World">>, <<"Hello">>, <<"World">>) +
        split_compare(<<"Hello:::World:">>, <<"Hello">>, <<"::World:">>) +
        split_compare(<<"Test:">>, <<"Test">>, <<>>) +
        split_compare(<<":">>, <<>>, <<>>) +
        split_compare(<<>>, <<>>) +
        split_compare(<<"Test">>, <<>>) +
        split_compare2(<<"Test">>, <<>>) +
        split_compare2(<<"helloSEPARATORworld">>, <<"hello">>, <<"world">>) +
        fail_split(<<>>) +
        fail_split({1, 2}) +
        fail_split2({1, 2}).

split_compare(Bin, Part1) ->
    [A] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A).

split_compare(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<":">>),
    compare_bin(Part1, A) + compare_bin(B, Part2).

split_compare2(Bin, Part1) ->
    [A] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A).

split_compare2(Bin, Part1, Part2) ->
    [A, B] = binary:split(Bin, <<"SEPARATOR">>),
    compare_bin(Part1, A) + compare_bin(B, Part2).

compare_bin(Bin1, Bin2) ->
    compare_bin(Bin1, Bin2, byte_size(Bin1) - 1).

compare_bin(_Bin1, _Bin2, -1) ->
    1;
compare_bin(Bin1, Bin2, Index) ->
    B1 = binary:at(Bin1, Index),
    case binary:at(Bin2, Index) of
        B1 ->
            compare_bin(Bin1, Bin2, Index - 1);
        _Any ->
            0
    end.

fail_split(Separator) ->
    try binary:split(<<"TESTBIN">>, Separator) of
        _Any -> 2000
    catch
        error:badarg -> 1;
        _:_ -> 4000
    end.

fail_split2(Bin) ->
    try binary:split(Bin, <<"TESTSEPARATOR">>) of
        _Any -> 2000
    catch
        error:badarg -> 1;
        _:_ -> 4000
    end.

test_alloc() ->
    [A, B] = binary:split(<<"Hello world\r\n">>, <<"\r\n">>),
    [A, C] = binary:split(<<"Hello world\r\n1">>, <<"\r\n">>),
    [A, D] = binary:split(<<"Hello world\r\n12">>, <<"\r\n">>),
    [A, E] = binary:split(<<"Hello world\r\n123">>, <<"\r\n">>),
    [A, F] = binary:split(<<"Hello world\r\n1234">>, <<"\r\n">>),
    [A, G] = binary:split(<<"Hello world\r\n12345">>, <<"\r\n">>),
    [A, H] = binary:split(<<"Hello world\r\n123456">>, <<"\r\n">>),
    [A, I] = binary:split(<<"Hello world\r\n1234567">>, <<"\r\n">>),
    [A, J] = binary:split(<<"Hello world\r\n12345678">>, <<"\r\n">>),
    [A, K] = binary:split(<<"Hello world\r\n123456789">>, <<"\r\n">>),
    [A, L] = binary:split(<<"Hello world\r\n1234567890">>, <<"\r\n">>),
    [A, M] = binary:split(<<"Hello world\r\n12345678901">>, <<"\r\n">>),
    [A, N] = binary:split(<<"Hello world\r\n123456789012">>, <<"\r\n">>),
    [A, O] = binary:split(<<"Hello world\r\n1234567890123">>, <<"\r\n">>),
    [A, P] = binary:split(<<"Hello world\r\n12345678901234">>, <<"\r\n">>),
    [A, Q] = binary:split(<<"Hello world\r\n123456789012345">>, <<"\r\n">>),
    [A, R] = binary:split(<<"Hello world\r\n1234567890123456">>, <<"\r\n">>),
    [A, S] = binary:split(<<"Hello world\r\n12345678901234567">>, <<"\r\n">>),
    [A, T] = binary:split(<<"Hello world\r\n123456789012345678">>, <<"\r\n">>),
    19 = length([byte_size(Bin) || Bin <- [B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]]),
    <<C:1/binary, $2>> = D,
    <<D:2/binary, $3>> = E,
    <<E:3/binary, $4>> = F,
    <<F:4/binary, $5>> = G,
    <<G:5/binary, $6>> = H,
    <<H:6/binary, $7>> = I,
    <<I:7/binary, $8>> = J,
    <<J:8/binary, $9>> = K,
    <<K:9/binary, $0>> = L,
    <<L:10/binary, $1>> = M,
    <<M:11/binary, $2>> = N,
    <<N:12/binary, $3>> = O,
    <<O:13/binary, $4>> = P,
    <<P:14/binary, $5>> = Q,
    <<Q:15/binary, $6>> = R,
    <<R:16/binary, $7>> = S,
    <<S:17/binary, $8>> = T,
    ok.
