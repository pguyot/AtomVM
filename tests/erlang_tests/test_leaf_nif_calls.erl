% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-module(test_leaf_nif_calls).
-export([start/0, id/1, tail_put/2, tail_erase/1, tail_demonitor/1, tail_size/1]).

start() ->
    loop(200),
    Parent = self(),
    spawn_opt(fun() -> Parent ! {payload, {{a, b}, [1, 2, 3], <<1, 2, 3>>}} end, []),
    receive
        {payload, Payload} ->
            undefined = put(received, Payload),
            erlang:garbage_collect(),
            Payload = erase(received)
    end,
    0.

loop(0) ->
    ok;
loop(I) ->
    K = {key, I},
    V = {value, [I, I + 1]},
    undefined = put(K, V),
    true = is_process_alive(self()),
    V = tail_put(K, I),
    I = tail_erase(K),
    undefined = erase(K),
    Put = id(put),
    undefined = apply(erlang, Put, [K, V]),
    Erase = id(fun erlang:erase/1),
    V = Erase(K),
    0 = erts_debug:flat_size(I),
    9 = tail_size({I, [1, 2, 3]}),
    -1 = erts_internal:cmp_term(I, I + 1),
    1 = erts_internal:cmp_term(1.0, 1),
    Ref = monitor(process, self()),
    true = demonitor(Ref),
    true = demonitor(Ref, [flush]),
    false = demonitor(make_ref(), [info]),
    true = tail_demonitor(Ref),
    ok = bad_reference(),
    loop(I - 1).

bad_reference() ->
    try erlang:demonitor(id(not_a_reference)) of
        _ -> unexpected_success
    catch
        error:badarg:Stack ->
            [{erlang, demonitor, _, _} | _] = Stack,
            ok
    end.

tail_put(K, V) -> put(K, V).
tail_erase(K) -> erase(K).
tail_demonitor(Ref) -> demonitor(Ref).
tail_size(T) -> erts_debug:flat_size(T).
id(T) -> T.
