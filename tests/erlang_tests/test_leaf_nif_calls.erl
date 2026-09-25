% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-module(test_leaf_nif_calls).
-export([
    start/0,
    id/1,
    tail_put/2,
    tail_erase/1,
    tail_demonitor/1,
    tail_size/1,
    tail_reverse/1,
    tail_reverse_framed/2,
    tail_demonitor_flush/1
]).

start() ->
    loop(200),
    ok = ordinary_nifs(),
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
            check_demonitor_stacktrace(Stack)
    end.

% AVM_CREATE_STACKTRACES=off answers `undefined' rather than a list.
check_demonitor_stacktrace(undefined) ->
    ok;
check_demonitor_stacktrace([{erlang, demonitor, _, _} | _]) ->
    ok.

% Ordinary NIFs take the same direct path at non-tail call sites: an error
% keeps its MFA stacktrace, and a collection inside the NIF (the minimum
% heap growth strategy leaves no free space) keeps the arguments and the
% caller's live values.
ordinary_nifs() ->
    ok = bad_list(),
    Opts =
        case erlang:system_info(machine) of
            "ATOM" -> [monitor, {atomvm_heap_growth, minimum}];
            _ -> [monitor]
        end,
    {Pid, Ref} = spawn_opt(
        fun() ->
            ok = collect_in_nif(50),
            ok = tail_nifs(50)
        end,
        Opts
    ),
    normal =
        receive
            {'DOWN', Ref, process, Pid, Reason} -> Reason
        end,
    ok.

% Tail calls to an ordinary NIF, without a frame (call_ext_only) and after
% popping one (call_ext_last), and to a NIF that traps.
tail_nifs(0) ->
    ok;
tail_nifs(N) ->
    List = [{N, I} || I <- lists:seq(1, 200)],
    [{N, 200} | _] = tail_reverse(List),
    [{N, 200} | _] = tail_reverse_framed(List, N),
    [] = tail_reverse([]),
    ok =
        try tail_reverse(id([a, b | c])) of
            _ -> unexpected_success
        catch
            error:badarg -> ok
        end,
    Ref = monitor(process, self()),
    true = tail_demonitor_flush(Ref),
    tail_nifs(N - 1).

bad_list() ->
    try lists:reverse(id([a, b | c])) of
        _ -> unexpected_success
    catch
        error:badarg:Stack ->
            check_reverse_stacktrace(Stack)
    end.

check_reverse_stacktrace(undefined) ->
    ok;
check_reverse_stacktrace([{lists, reverse, _, _} | _]) ->
    ok.

collect_in_nif(0) ->
    ok;
collect_in_nif(N) ->
    Live = {live, N, [N, N + 1]},
    List = [{N, I} || I <- lists:seq(1, 200)],
    Tail = [tail, N],
    Reversed = lists:reverse(List, Tail),
    [{N, 200} | _] = Reversed,
    202 = length(Reversed),
    {live, N, [N, M]} = Live,
    M = N + 1,
    [tail, N] = Tail,
    collect_in_nif(N - 1).

tail_put(K, V) -> put(K, V).
tail_erase(K) -> erase(K).
tail_demonitor(Ref) -> demonitor(Ref).
tail_size(T) -> erts_debug:flat_size(T).
tail_reverse(L) -> lists:reverse(L).
tail_reverse_framed(L, N) ->
    Tail = id([N]),
    lists:reverse(L, Tail).
tail_demonitor_flush(Ref) -> demonitor(Ref, [flush]).
id(T) -> T.
