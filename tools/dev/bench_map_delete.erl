-module(db).
-export([start/0]).
%% Bulk delete from a large map: O(n) per delete makes this quadratic.
start() ->
    [run(N) || N <- [512, 1024, 2048, 4096, 8192]],
    ok.
run(N) ->
    Keys = [{b, I, x} || I <- lists:seq(1, N)],
    M = maps:from_list([{K, 1} || K <- Keys]),
    Del = lists:sublist(Keys, N div 2),
    Reps = max(1, 400000 div N),
    T0 = erlang:monotonic_time(microsecond),
    loop(M, Del, Reps),
    T = erlang:monotonic_time(microsecond) - T0,
    io:format("n=~-6b ~8.2f us per (delete ~p keys)~n", [N, T / Reps, length(Del)]).
loop(_M, _D, 0) -> ok;
loop(M, D, R) ->
    _ = lists:foldl(fun(K, Acc) -> maps:remove(K, Acc) end, M, D),
    loop(M, D, R - 1).
