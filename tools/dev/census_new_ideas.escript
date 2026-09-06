#!/usr/bin/env escript
%% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%! +S 1
main(_) ->
    Files = filelib:wildcard("/opt/local/lib/erlang/lib/*/ebin/*.beam"),
    Fs = lists:flatmap(fun(F) ->
        case beam_disasm:file(F) of
            {beam_file, _, _, _, _, Funcs} ->
                [{F, N, A, Is} || {function, N, A, _, Is} <- Funcs];
            Other -> error({disassembly_failed, F, Other})
        end
    end, Files),
    Ops = lists:append([Is || {_, _, _, Is} <- Fs]),
    Div = [{Op, D} || {gc_bif, Op, _, _, [_, {integer, D}], _} <- Ops,
        (Op =:= 'div' orelse Op =:= 'rem'), is_integer(D), D > 0,
        (D band (D - 1)) =:= 0],
    Histogram = lists:foldl(fun(K, M) ->
        maps:update_with(K, fun(V) -> V + 1 end, 1, M)
    end, #{}, Div),
    FloatCounts = [length([ok || {bif, Op, _, _, _} <- Is,
        lists:member(Op, [fadd, fsub, fmul, fdiv])]) || {_, _, _, Is} <- Fs],
    io:format("files=~p functions=~p markers=~p pow2_ops=~p~npow2=~p~n", [
        length(Files), length(Fs), length([ok || {recv_marker_use, _} <- Ops]),
        length(Div), Histogram]),
    io:format("float_ops=~p multi_float_functions=~p~n", [
        lists:sum(FloatCounts), length([ok || C <- FloatCounts, C > 1])]).
