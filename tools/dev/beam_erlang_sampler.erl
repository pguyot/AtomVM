-module(sampler_run).
-export([main/1]).

%% Statistical Erlang-level profiler: poll which process is RUNNING and record
%% its innermost frame. No per-call instrumentation, so the target runs at
%% full speed and the time attribution is trustworthy (unlike eprof/call_time,
%% whose per-call overhead swamps an 8.5 ns average function).
main([Src, Inc1, Inc2]) ->
    Opts = [{i, Inc1}, {i, Inc2}, {outdir, "/tmp"}, binary, return_errors],
    _ = compile:file(Src, Opts),                  % warm up: load + page in
    Me = self(),
    Tab = ets:new(samples, [public, set]),
    Sampler = spawn_opt(fun() -> loop(Me, Tab, 0) end, [{priority, high}]),
    T0 = erlang:monotonic_time(microsecond),
    _ = compile:file(Src, Opts),
    T1 = erlang:monotonic_time(microsecond),
    Sampler ! {stop, self()},
    N = receive {stopped, K} -> K after 5000 -> 0 end,
    report(Tab, T1 - T0, N),
    halt(0).

loop(Skip, Tab, N) ->
    receive
        {stop, From} -> From ! {stopped, N}
    after 0 ->
        Top = running_frame(Skip),
        case Top of
            none -> ok;
            MFA -> ets:update_counter(Tab, MFA, 1, {MFA, 0})
        end,
        erlang:yield(),
        loop(Skip, Tab, N + 1)
    end.

running_frame(Skip) ->
    running_frame(processes(), Skip).

running_frame([], _Skip) -> none;
running_frame([P | Ps], Skip) when P =:= Skip -> running_frame(Ps, Skip);
running_frame([P | Ps], Skip) ->
    case P =:= self() of
        true -> running_frame(Ps, Skip);
        false ->
            case erlang:process_info(P, [status, current_stacktrace]) of
                [{status, running}, {current_stacktrace, [{M, F, A, _} | _]}] ->
                    {M, F, A};
                _ -> running_frame(Ps, Skip)
            end
    end.

report(Tab, Wall, Polls) ->
    Rows = lists:reverse(lists:keysort(2, ets:tab2list(Tab))),
    Tot = lists:sum([C || {_, C} <- Rows]),
    io:format("WALL ~p us, ~p polls, ~p samples landed on a running process~n",
              [Wall, Polls, Tot]),
    io:format("~-52s ~9s ~8s~n", ["function", "samples", "%"]),
    [io:format("~-52s ~9b ~7.2f~n",
               [lists:flatten(io_lib:format("~p:~p/~p", [M, F, A])), C,
                100.0 * C / max(Tot, 1)])
     || {{M, F, A}, C} <- lists:sublist(Rows, 40)],
    ok.
