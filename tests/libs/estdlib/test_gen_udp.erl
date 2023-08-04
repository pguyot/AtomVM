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
    ok = test_send_receive_active(false, binary),
    ok = test_send_receive_active(true, binary),
    ok = test_send_receive_active(false, list),
    ok = test_send_receive_active(true, list),
    ok.

test_send_receive_active(SpawnControllingProcess, Mode) ->
    {ok, Socket} = gen_udp:open(0, [{active, true}, Mode]),
    {ok, Port} = inet:port(Socket),
    NumToSend = 10,
    Self = self(),
    F = fun() ->
        NumReceived = count_received(Mode, NumToSend),
        case SpawnControllingProcess of
            true ->
                Self ! {done, NumReceived};
            _ ->
                NumReceived
        end
    end,

    case SpawnControllingProcess of
        true ->
            Pid = spawn(F),
            gen_udp:controlling_process(Socket, Pid);
        false ->
            ok
    end,

    Sender = erlang:spawn(?MODULE, start_sender, [Socket, Port, make_messages(NumToSend)]),

    NumReceived =
        case SpawnControllingProcess of
            true ->
                receive
                    {done, Received} ->
                        Received
                end;
            false ->
                F()
        end,
    ?ASSERT_EQUALS(NumToSend, NumReceived),
    ok = gen_udp:close(Socket),
    ok.

make_messages(0) ->
    [];
make_messages(N) ->
    [<<"foo">> | make_messages(N - 1)].

start_sender(Socket, Port, Msgs) ->
    send(Socket, Port, Msgs).

send(_Socket, _Port, []) ->
    ok;
send(Socket, Port, [Msg | Rest]) ->
    gen_udp:send(Socket, {127, 0, 0, 1}, Port, Msg),
    send(Socket, Port, Rest).

count_received(Mode, Max) ->
    count_received0(Mode, Max, 0).

count_received0(Mode, Max, I) ->
    Timeout =
        if
            Max > I -> 1500;
            true -> 200
        end,
    receive
        {udp, _Pid, _Address, _Port, <<"foo">>} when Mode =:= binary ->
            count_received0(Mode, Max, I + 1);
        {udp, _Pid, _Address, _Port, "foo"} when Mode =:= list ->
            count_received0(Mode, Max, I + 1);
        Other ->
            erlang:display({unexpected, Other}),
            count_received0(Mode, Max, I)
    after Timeout -> I
    end.
