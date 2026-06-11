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

-module(test_heap_growth).

-export([start/0]).

start() ->
    ok = test_grow_beyond_min_heap_size(),
    ok = test_bounded_free_strategy(false),
    ok = test_bounded_free_strategy(true),
    ok = test_minimum_strategy(),
    ok = test_fibonacci_strategy(),
    ok = test_messages_get_gcd(),
    0.

test_grow_beyond_min_heap_size() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            % Heap size is set to minimum at first GC/Heap growth
            alloc_some_heap_words(10),
            {total_heap_size, 100} = process_info(self(), total_heap_size),
            Var = alloc_some_heap_words(200),
            {total_heap_size, X} = process_info(self(), total_heap_size),
            true = X > 100,
            % do something with Var to avoid compiler optimizations
            true = 200 =:= length(Var)
        end,
        [monitor, {min_heap_size, 100}, {fullsweep_after, 0}]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 1000 -> timeout
        end,
    ok.

test_bounded_free_strategy(UseDefault) ->
    Opt =
        if
            UseDefault -> [];
            true -> [{atomvm_heap_growth, bounded_free}]
        end,
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            {total_heap_size, X1} = process_info(self(), total_heap_size),
            ok = test_growth_bounded(32),
            {total_heap_size, X2} = process_info(self(), total_heap_size),
            % Allocate again, this is when heap will be shrunk
            Var1 = alloc_some_heap_words(10),
            {total_heap_size, X3} = process_info(self(), total_heap_size),
            20 = erts_debug:flat_size(Var1),
            true = X3 < X2,
            true = X3 - X1 - erts_debug:flat_size(Var1) < 32
        end,
        [monitor, {fullsweep_after, 0} | Opt]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 5000 -> timeout
        end,
    ok.

test_minimum_strategy() ->
    Parent = self(),
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            receive
                {step, 1} -> Parent ! ok
            end,
            receive
                {step, 2} -> Parent ! ok
            end,
            % Cannot really be 0 because we allocate more than just the list.
            ok = test_growth_bounded(10),
            receive
                {step, 3} -> Parent ! ok
            end,
            receive
                {step, 4} -> Parent ! ok
            end,
            % Allocate again, this is when heap will be shrunk
            Var1 = alloc_some_heap_words(10),
            receive
                {step, 5} -> Parent ! ok
            end,
            receive
                {step, 6} -> Parent ! ok
            end,
            20 = erts_debug:flat_size(Var1)
        end,
        [monitor, {atomvm_heap_growth, minimum}, {fullsweep_after, 0}]
    ),
    % Get heap size from the outside to have no influence on the heap
    Pid1 ! {step, 1},
    ok =
        receive
            ok -> ok
        after 1000 -> timeout
        end,
    {total_heap_size, X1} = process_info(Pid1, total_heap_size),
    Pid1 ! {step, 2},
    ok =
        receive
            ok -> ok
        after 1000 -> timeout
        end,
    Pid1 ! {step, 3},
    ok =
        receive
            ok -> ok
        after 1000 -> timeout
        end,
    {total_heap_size, X2} = process_info(Pid1, total_heap_size),
    Pid1 ! {step, 4},
    ok =
        receive
            ok -> ok
        after 1000 -> timeout
        end,
    Pid1 ! {step, 5},
    ok =
        receive
            ok -> ok
        after 1000 -> timeout
        end,
    {total_heap_size, X3} = process_info(Pid1, total_heap_size),
    Pid1 ! {step, 6},
    ok =
        receive
            ok -> ok
        after 1000 -> timeout
        end,
    true = X3 < X2,
    true = X3 - X1 - 20 =< 10,
    normal =
        receive
            {'DOWN', Ref1, process, Pid1, Reason} -> Reason
        after 5000 -> timeout
        end,
    ok.

% This test is a little bit long on the CI
% It aims to test:
% - that heap sizes progress following fibonacci suite like progression
% - until a given size where they progress slower
% - and in both cases, memory is eventually reclaimed
test_fibonacci_strategy() ->
    % Test small increments follow fibonacci
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            MaxHeap = test_growth_fibonacci(50, 20000),
            % Allocate again until heap is shrunk
            NewHeap = allocate_until_heap_size_changes(MaxHeap),
            true = NewHeap < MaxHeap
        end,
        [monitor, link, {atomvm_heap_growth, fibonacci}, {fullsweep_after, 0}]
    ),
    % Test large increments no longer follow fibonacci
    {Pid2, Ref2} = spawn_opt(
        fun() ->
            MaxHeap = test_growth_fibonacci(50000, 10000000),
            % Allocate again until heap is shrunk
            NewHeap = allocate_until_heap_size_changes(MaxHeap),
            true = NewHeap < MaxHeap
        end,
        [monitor, link, {atomvm_heap_growth, fibonacci}, {fullsweep_after, 0}]
    ),
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 300000 -> timeout
        end,
    ok =
        receive
            {'DOWN', Ref2, process, Pid2, normal} -> ok
        after 300000 -> timeout
        end,
    ok.

alloc_some_heap_words(N) ->
    alloc_some_heap_words(N, []).

alloc_some_heap_words(0, Acc) -> Acc;
alloc_some_heap_words(N, Acc) -> alloc_some_heap_words(N - 1, [N | Acc]).

test_growth_bounded(Boundary) ->
    % Test growth is bounded
    Var1 = alloc_some_heap_words(10),
    {total_heap_size, X1} = process_info(self(), total_heap_size),
    Var2 = alloc_some_heap_words(10),
    {total_heap_size, X2} = process_info(self(), total_heap_size),
    Var3 = alloc_some_heap_words(10),
    {total_heap_size, X3} = process_info(self(), total_heap_size),
    true = X2 - X1 - erts_debug:flat_size(Var2) =< Boundary,
    true = X3 - X2 - erts_debug:flat_size(Var3) =< Boundary,
    true = X3 - X1 - erts_debug:flat_size(Var2) - erts_debug:flat_size(Var3) =< Boundary,
    20 = erts_debug:flat_size(Var1),
    ok.

test_growth_fibonacci(Increment, TargetMax) ->
    HeapSizes = collect_heap_sizes([], [], Increment, TargetMax),
    [MaxHeap | _] = HeapSizes,
    ok = test_fibonacci_heap_sizes(Increment, HeapSizes),
    MaxHeap.

collect_heap_sizes(Acc, Terms, Increment, TargetMax) ->
    {total_heap_size, S1} = process_info(self(), total_heap_size),
    % Heap should not shrink
    NewAcc =
        case Acc of
            [] -> [S1 | Acc];
            [S1 | _Tail] -> Acc;
            [SmallerSize | _Tail] when SmallerSize < S1 -> [S1 | Acc]
        end,
    if
        S1 >= TargetMax ->
            NewAcc;
        true ->
            collect_heap_sizes(
                NewAcc, [alloc_some_heap_words(Increment) | Terms], Increment, TargetMax
            )
    end.

% The growth policy guarantees at least a quarter of the heap is free after
% a collection, so consecutive sizes may skip fibonacci steps; each observed
% size must still be a fibonacci series size (or follow the +20% rule past
% the end of the series).
test_fibonacci_heap_sizes(Increment, [HeapSize, Previous | Tail]) when HeapSize > 5709651 ->
    Diff = HeapSize - Previous,
    Delta = Diff - Previous div 5,
    case Delta >= -4 andalso Delta =< 4 of
        true -> ok;
        % a step can be skipped here as well
        false -> true = lists:member(Previous, fibonacci_sizes()) orelse Previous > 5709651
    end,
    test_fibonacci_heap_sizes(Increment, [Previous | Tail]);
test_fibonacci_heap_sizes(Increment, [HeapSize | Tail]) ->
    true = lists:member(HeapSize, fibonacci_sizes()),
    case Tail of
        [] -> ok;
        _ -> test_fibonacci_heap_sizes(Increment, Tail)
    end.

fibonacci_sizes() ->
    [
        12,
        38,
        51,
        90,
        142,
        233,
        376,
        610,
        987,
        1598,
        2586,
        4185,
        6772,
        10958,
        17731,
        28690,
        46422,
        75113,
        121536,
        196650,
        318187,
        514838,
        833026,
        1347865,
        2180892,
        3528758,
        5709651
    ].

allocate_until_heap_size_changes(Heap) ->
    {total_heap_size, S1} = process_info(self(), total_heap_size),
    if
        S1 =/= Heap ->
            S1;
        true ->
            alloc_some_heap_words(100),
            allocate_until_heap_size_changes(Heap)
    end.

% This test ensures that when messages are received, they eventually get gc'd
test_messages_get_gcd() ->
    {Pid1, Ref1} = spawn_opt(
        fun() ->
            loop([])
        end,
        [monitor, {atomvm_heap_growth, minimum}, {fullsweep_after, 0}]
    ),
    FinalHeapSize = loop_send(Pid1, 20),
    Pid1 ! quit,
    ok =
        receive
            {'DOWN', Ref1, process, Pid1, normal} -> ok
        after 5000 -> timeout
        end,
    ok =
        if
            FinalHeapSize < 200 -> ok;
            true -> {heap_size_too_large, FinalHeapSize}
        end,
    ok.

loop(State) when is_list(State) ->
    NewData =
        receive
            {get_data, Pid} ->
                Pid ! {data, State},
                State;
            {data, Data} ->
                Data;
            quit ->
                ok
        end,
    loop(NewData);
loop(_Other) ->
    ok.

loop_send(Pid, 0) ->
    {total_heap_size, THS} = process_info(Pid, total_heap_size),
    THS;
loop_send(Pid, N) ->
    Pid ! {data, alloc_some_heap_words(40)},
    loop_send(Pid, N - 1).
