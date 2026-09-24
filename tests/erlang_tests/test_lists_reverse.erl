%
% This file is part of AtomVM.
%
% Copyright 2023 Paul Guyot <pguyot@kallisys.net>
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

-module(test_lists_reverse).

-export([start/0]).

start() ->
    ok = test_lists_reverse_1(),
    ok = test_lists_reverse_2(),
    ok = test_lists_reverse_long(),
    0.

test_lists_reverse_1() ->
    [] = lists:reverse([]),
    [a, b, c] = lists:reverse([c, b, a]),
    ok =
        try
            lists:reverse([a, b | improper]),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            lists:reverse(not_a_list),
            fail
        catch
            % AtomVM
            error:badarg -> ok;
            % BEAM
            error:function_clause -> ok
        end,
    ok.

test_lists_reverse_2() ->
    [] = lists:reverse([], []),
    [a, b, c, d, e, f] = lists:reverse([c, b, a], [d, e, f]),
    ok =
        try
            lists:reverse([a, b | improper], [a]),
            fail
        catch
            % AtomVM
            error:badarg -> ok;
            % BEAM
            T:V -> {T, V}
        end,
    [b, a, c, d | improper] = lists:reverse([a, b], [c, d | improper]),
    ok =
        try
            lists:reverse(not_a_list, []),
            fail
        catch
            error:badarg -> ok
        end,
    not_a_list = lists:reverse([], not_a_list),
    [c, b, a | not_a_list] = lists:reverse([a, b, c], not_a_list),
    ok.

% Lists longer than the free space on the heap, so the reversal needs a
% collection, including improper ones whose tail is only reached then. The
% minimum heap growth strategy leaves no free space to start with.
test_lists_reverse_long() ->
    ok = reverse_long(),
    Opts =
        case erlang:system_info(machine) of
            "ATOM" -> [monitor, {atomvm_heap_growth, minimum}];
            _ -> [monitor]
        end,
    {Pid, Ref} = spawn_opt(fun() -> ok = reverse_long() end, Opts),
    normal =
        receive
            {'DOWN', Ref, process, Pid, Reason} -> Reason
        end,
    ok.

reverse_long() ->
    Long = seq(1, 50000),
    Reversed = seq_down(50000, 1),
    Reversed = lists:reverse(Long),
    [50000 | _] = lists:reverse(Long, [tail]),
    [tail] = drop(50000, lists:reverse(Long, [tail])),
    ImproperLong = seq(1, 50000, improper),
    ok =
        try
            lists:reverse(ImproperLong),
            fail
        catch
            error:badarg -> ok
        end,
    ok =
        try
            lists:reverse(ImproperLong, []),
            fail
        catch
            % AtomVM
            error:badarg -> ok;
            % BEAM
            _:_ -> ok
        end,
    % The failed reversals left the heap consistent.
    Reversed = lists:reverse(Long),
    ok.

seq(From, To) -> seq(From, To, []).

seq(From, To, Tail) when From > To -> Tail;
seq(From, To, Tail) -> [From | seq(From + 1, To, Tail)].

seq_down(From, To) when From < To -> [];
seq_down(From, To) -> [From | seq_down(From - 1, To)].

drop(0, List) -> List;
drop(N, [_ | Tail]) -> drop(N - 1, Tail).
