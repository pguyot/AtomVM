-module(test_jit_map_size).

-export([start/0, check/1]).

%% Exercises the JIT inline for erlang:map_size/1 on a value the Type chunk
%% proves is a map (the is_map/1 guard gives the operand a plain t_map type).
%% Covers both the flat (small) and tree (large) map representations, including
%% the empty map, so a wrong header/size read is caught behaviourally.

start() ->
    0 = check(mk(0)),
    1 = check(mk(1)),
    2 = check(mk(2)),
    7 = check(mk(7)),
    32 = check(mk(32)),
    100 = check(mk(100)),
    3 = check(id(#{x => 1, y => 2, z => 3})),
    0.

check(M) when is_map(M) -> map_size(M).

mk(N) -> maps:from_list([{K, K} || K <- lists:seq(1, N)]).

id(X) -> X.
