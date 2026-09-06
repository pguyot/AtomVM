-module(mb).
-export([start/0]).

%% The inner loop of beam_ssa_dead:maps_is_subset_kv/2, the hottest function
%% in BEAM's compile of unicode_util.erl, plus its two components measured
%% separately: maps:next/1 iteration and a single-key map lookup.

start() ->
    [run(N) || N <- [16, 64, 256, 1024, 4096]],
    ok.

run(N) ->
    Big = maps:from_list([{{b, I, x}, I} || I <- lists:seq(1, N)]),
    Small = maps:from_list([{{b, I, x}, I} || I <- lists:seq(1, N)]),
    Reps = max(1, 2000000 div N),
    T1 = bench(fun() -> subset_kv(maps:next(maps:iterator(Small)), Big) end, Reps),
    T2 = bench(fun() -> iterate(maps:next(maps:iterator(Small)), 0) end, Reps),
    Keys = [{b, I, x} || I <- lists:seq(1, N)],
    T3 = bench(fun() -> lookups(Keys, Big, 0) end, Reps),
    Ops = Reps * N,
    io:format("n=~-5b subset_kv ~7.1f ns/entry   next ~7.1f ns/entry   "
              "lookup ~7.1f ns/key~n",
              [N, T1 * 1000 / Ops, T2 * 1000 / Ops, T3 * 1000 / Ops]),
    ok.

bench(F, Reps) ->
    _ = F(),
    T0 = erlang:monotonic_time(microsecond),
    loop(F, Reps),
    erlang:monotonic_time(microsecond) - T0.

loop(_F, 0) -> ok;
loop(F, N) -> _ = F(), loop(F, N - 1).

subset_kv({K, V, Iterator}, BigMap) ->
    Next = maps:next(Iterator),
    case BigMap of
        #{K := V} -> subset_kv(Next, BigMap);
        #{} -> false
    end;
subset_kv(none, _BigMap) -> true.

iterate(none, Acc) -> Acc;
iterate({_K, _V, I}, Acc) -> iterate(maps:next(I), Acc + 1).

lookups([], _M, Acc) -> Acc;
lookups([K | Ks], M, Acc) -> lookups(Ks, M, Acc + maps:get(K, M)).
