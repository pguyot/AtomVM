-module(cb2).
-export([start/0]).
%% Same head-to-head, but with the key shape unicode_util's compiler maps
%% actually use: {Atom, Tuple, X}, whose ordering comparison has to walk into
%% the middle element while a hash is paid once.
start() ->
    io:format("-- immediate keys {b, I, x} --~n"),
    [run(N, fun(I) -> {b, I, x} end) || N <- [1024, 4096, 8192]],
    io:format("-- compound keys {b, {v, I, y}, x} --~n"),
    [run(N, fun(I) -> {b, {v, I, y}, x} end) || N <- [1024, 4096, 8192]],
    io:format("-- deeper {b, {v, {w, I}, y}, x} --~n"),
    [run(N, fun(I) -> {b, {v, {w, I}, y}, x} end) || N <- [1024, 4096, 8192]],
    ok.
run(N, F) ->
    Keys = [F(I) || I <- lists:seq(1, N)],
    M = maps:from_list([{K, 1} || K <- Keys]),
    Step = max(1, N div 256),
    Probe = [F(I) || I <- lists:seq(1, N, Step)],
    Reps = 1500,
    B = atomvm:map_backend_bench(M, Probe, Reps, btree),
    C = atomvm:map_backend_bench(M, Probe, Reps, champ),
    Ops = Reps * length(Probe),
    io:format(
        "  n=~-6b btree ~6.1f ns   champ ~6.1f ns   champ/btree ~5.2fx~n",
        [N, B * 1000 / Ops, C * 1000 / Ops, C / max(B, 1)]
    ).
