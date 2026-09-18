% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% Mechanism probes, not a replacement for the publication benchmarks.
%% Run with one scheduler on both engines; timings exclude setup.
-module(new_ideas_probe).
-export([start/0]).

start() ->
    erlang:system_flag(schedulers_online, 1),
    [isolated(fun() -> receive_probe(N) end) || N <- [0, 100, 1000, 10000]],
    [
        isolated(fun() -> iterator_probe(N, Take) end)
     || N <- [100, 1000, 10000], Take <- [1, 8]
    ],
    [isolated(fun() -> iterator_probe(N, N) end) || N <- [100, 1000, 10000]],
    ok.

isolated(Fun) ->
    Parent = self(),
    Pid = spawn(fun() ->
        Fun(),
        Parent ! {done, self()}
    end),
    receive
        {done, Pid} -> ok
    after 60000 -> error(probe_timeout)
    end.

receive_probe(N) ->
    fill(N),
    erlang:garbage_collect(),
    T0 = erlang:monotonic_time(microsecond),
    3000 = request_loop(3000, 0),
    Time = erlang:monotonic_time(microsecond) - T0,
    N = drain(0),
    io:format("receive_~B: ~B~n", [N, Time]).

fill(0) ->
    ok;
fill(N) ->
    self() ! old_message,
    fill(N - 1).

request_loop(0, Sum) ->
    Sum;
request_loop(N, Sum) ->
    Ref = make_ref(),
    self() ! {Ref, 1},
    receive
        {Ref, Value} -> request_loop(N - 1, Sum + Value)
    after 5000 -> error(request_timeout)
    end.

drain(N) ->
    receive
        old_message -> drain(N + 1)
    after 0 -> N
    end.

iterator_probe(N, Take) ->
    Map = make_map(N, #{}),
    erlang:garbage_collect(),
    Repeats =
        case N =:= Take of
            true -> 10;
            false -> 1000
        end,
    T0 = erlang:monotonic_time(microsecond),
    Checksum = first_loop(Repeats, Map, Take, 0),
    Time = erlang:monotonic_time(microsecond) - T0,
    true = is_integer(Checksum),
    true = Checksum > 0,
    case N =:= Take of
        true -> Checksum = Repeats * N * (N + 1) div 2;
        false -> ok
    end,
    io:format("iterator_~B_take_~B: ~B~n", [N, Take, Time]).

make_map(0, Map) -> Map;
make_map(N, Map) -> make_map(N - 1, Map#{N => N}).

first_loop(0, _Map, _Take, Sum) ->
    Sum;
first_loop(N, Map, Take, Sum) ->
    First = take(Take, maps:iterator(Map), 0),
    first_loop(N - 1, Map, Take, Sum + First).

take(0, _Iterator, Sum) ->
    Sum;
take(N, Iterator, Sum) ->
    {Key, Value, Next} = maps:next(Iterator),
    Key = Value,
    take(N - 1, Next, Sum + Key).
