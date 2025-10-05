%
% This file is part of AtomVM.
%
% Copyright 2019-2022 Fred Dushin <fred@dushin.net>
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

-module(test_gen_udp).

-export([test/0, start_sender/3]).

-include("etest.hrl").

test() ->
    BackendOptions =
        case get_otp_version() of
            Version when Version =:= atomvm orelse (is_integer(Version) andalso Version >= 24) ->
                [[{inet_backend, inet}], [{inet_backend, socket}]];
            _ ->
                [[]]
        end,
    Results = [
        case catch run_with_timeout(
            fun() -> test_send_receive(SpawnControllingProcess, IsActive, Mode, BackendOption) end,
            {SpawnControllingProcess, IsActive, Mode, BackendOption},
            15000
        ) of
            ok -> ok;
            Error -> {error, {SpawnControllingProcess, IsActive, Mode, BackendOption}, Error}
        end
     || SpawnControllingProcess <- [false, true],
        IsActive <- [false, true],
        Mode <- [binary, list],
        BackendOption <- BackendOptions
    ],
    Failures = [R || R <- Results, element(1, R) =:= error],
    case Failures of
        [] ->
            ok;
        _ ->
            io:format("~n=== FAILED TESTS ===~n~p~n", [Failures]),
            error({tests_failed, length(Failures)})
    end.

test_send_receive(SpawnControllingProcess, IsActive, Mode, BackendOption) ->
    StartTime = erlang:monotonic_time(millisecond),
    io:format("GEN_UDP-TEST> START SpawnControllingProcess=~p IsActive=~p Mode=~p Backendoption=~p~n", [
        SpawnControllingProcess, IsActive, Mode, BackendOption
    ]),

    io:format("GEN_UDP-TEST> [~pms] Opening socket...~n", [elapsed(StartTime)]),
    {ok, Socket} = gen_udp:open(0, BackendOption ++ [{active, IsActive}, Mode]),
    {ok, Port} = inet:port(Socket),
    io:format("GEN_UDP-TEST> [~pms] Socket opened on port ~p~n", [elapsed(StartTime), Port]),

    Self = self(),
    F = fun() ->
        case SpawnControllingProcess of
            true -> Self ! ready;
            _ -> ok
        end,
        NumReceived = count_received(Socket, IsActive, Mode),
        case SpawnControllingProcess of
            true ->
                case SpawnControllingProcess of
                    true -> Self ! {done, NumReceived};
                    _ -> ok
                end;
            false ->
                ok
        end,
        NumReceived
    end,

    case SpawnControllingProcess of
        true ->
            io:format("GEN_UDP-TEST> [~pms] Spawning controlling process...~n", [elapsed(StartTime)]),
            Pid = spawn(F),
            gen_udp:controlling_process(Socket, Pid),
            receive
                ready ->
                    io:format("GEN_UDP-TEST> [~pms] Controlling process ready~n", [elapsed(StartTime)]),
                    ok
            after 5000 ->
                io:format("GEN_UDP-TEST> [~pms] TIMEOUT waiting for ready. Message queue length: ~p~n",
                    [elapsed(StartTime), get_message_queue_len()]),
                try_get_socket_info(Socket),
                error({timeout, ?MODULE, ?LINE, waiting_for_ready})
            end;
        false ->
            ok
    end,

    NumToSend = 10,
    io:format("GEN_UDP-TEST> [~pms] Spawning sender for ~p messages...~n", [elapsed(StartTime), NumToSend]),
    {Sender, SenderMonitor} = erlang:spawn_opt(
        ?MODULE, start_sender, [Socket, Port, make_messages(NumToSend)], [monitor]
    ),

    NumReceived =
        case SpawnControllingProcess of
            true ->
                io:format("GEN_UDP-TEST> [~pms] Waiting for {done, _} message...~n", [elapsed(StartTime)]),
                receive
                    {done, Received} ->
                        io:format("GEN_UDP-TEST> [~pms] Received ~p messages~n", [elapsed(StartTime), Received]),
                        Received
                after 5000 ->
                    io:format("GEN_UDP-TEST> [~pms] TIMEOUT waiting for {done, _}. Message queue length: ~p~n",
                        [elapsed(StartTime), get_message_queue_len()]),
                    try_get_socket_info(Socket),
                    error({timeout, ?MODULE, ?LINE, waiting_for_done})
                end;
            false ->
                io:format("GEN_UDP-TEST> [~pms] Counting received messages (inline)...~n", [elapsed(StartTime)]),
                Received = F(),
                io:format("GEN_UDP-TEST> [~pms] Received ~p messages~n", [elapsed(StartTime), Received]),
                Received
        end,
    io:format("GEN_UDP-TEST> [~pms] Stopping sender...~n", [elapsed(StartTime)]),
    Sender ! stop,
    receive
        {'DOWN', SenderMonitor, process, Sender, normal} ->
            io:format("GEN_UDP-TEST> [~pms] Sender stopped~n", [elapsed(StartTime)]),
            ok
    after 5000 ->
        io:format("GEN_UDP-TEST> [~pms] TIMEOUT waiting for sender to stop~n", [elapsed(StartTime)]),
        error({timeout, ?MODULE, ?LINE, waiting_for_sender_down})
    end,

    ?ASSERT_TRUE((0 < NumReceived) and (NumReceived =< NumToSend)),
    %% NB. Might be closed if controlling process terminates
    io:format("GEN_UDP-TEST> [~pms] Closing socket...~n", [elapsed(StartTime)]),
    case SpawnControllingProcess of
        true ->
            catch gen_udp:close(Socket);
        _ ->
            ok = gen_udp:close(Socket)
    end,
    io:format("GEN_UDP-TEST> [~pms] COMPLETED successfully~n", [elapsed(StartTime)]),
    ok.

make_messages(0) ->
    [];
make_messages(N) ->
    [<<"foo">> | make_messages(N - 1)].

start_sender(Socket, Port, Msgs) ->
    send(Socket, Port, Msgs),
    receive
        stop ->
            ok
    end.

send(_Socket, _Port, []) ->
    ok;
send(Socket, Port, [Msg | Rest]) ->
    gen_udp:send(Socket, {127, 0, 0, 1}, Port, Msg),
    send(Socket, Port, Rest).

count_received(_Socket, true = _IsActive, Mode) ->
    count_active_received(Mode, 0);
count_received(Socket, false = _IsActive, Mode) ->
    count_passive_received(Socket, Mode, 0).

count_active_received(Mode, I) ->
    receive
        {udp, _Pid, _Address, _Port, <<"foo">>} when Mode =:= binary ->
            count_active_received(Mode, I + 1);
        {udp, _Pid, _Address, _Port, "foo"} when Mode =:= list ->
            count_active_received(Mode, I + 1);
        Other ->
            erlang:display({count_active_received, unexpected, Other}),
            count_active_received(Mode, I)
    after 500 ->
        I
    end.

count_passive_received(Socket, Mode, I) ->
    case gen_udp:recv(Socket, 0, 500) of
        {ok, {_Address, _Port, <<"foo">>}} when Mode =:= binary ->
            count_passive_received(Socket, Mode, I + 1);
        {ok, {_Address, _Port, "foo"}} when Mode =:= list ->
            count_passive_received(Socket, Mode, I + 1);
        {error, timeout} ->
            I;
        Other ->
            erlang:display({count_passive_received, unexpected, Other}),
            count_passive_received(Socket, Mode, I)
    end.

get_otp_version() ->
    case erlang:system_info(machine) of
        "BEAM" ->
            list_to_integer(erlang:system_info(otp_release));
        _ ->
            atomvm
    end.

run_with_timeout(F, Params, Timeout) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Parent ! {self(), result, F()}
    end),
    receive
        {Pid, result, Result} ->
            erlang:demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            error({test_crashed, Params, Reason})
    after Timeout ->
        io:format("GEN_UDP-TEST> TIMEOUT after ~pms for params: ~p~n", [Timeout, Params]),
        case process_info(Pid) of
            undefined ->
                io:format("GEN_UDP-TEST> Test process already dead~n");
            Info ->
                io:format("GEN_UDP-TEST> Test process state: ~p~n", [Info])
        end,
        io:format("GEN_UDP-TEST> Total processes: ~p~n", [length(processes())]),
        exit(Pid, kill),
        erlang:demonitor(Ref, [flush]),
        error({timeout, Params, Timeout})
    end.

elapsed(StartTime) ->
    erlang:monotonic_time(millisecond) - StartTime.

get_message_queue_len() ->
    case process_info(self(), message_queue_len) of
        {message_queue_len, Len} -> Len;
        undefined -> unknown
    end.

try_get_socket_info(Socket) ->
    try
        case inet:info(Socket) of
            {ok, SockInfo} ->
                io:format("GEN_UDP-TEST> Socket info: ~p~n", [SockInfo]);
            Error ->
                io:format("GEN_UDP-TEST> Socket info error: ~p~n", [Error])
        end
    catch
        Class:Reason ->
            io:format("GEN_UDP-TEST> Could not get socket info: ~p:~p~n", [Class, Reason])
    end.
