%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Three-node relay test for serial distribution.
%%
%% This test verifies that two nodes (A and C) which are NOT directly
%% connected over serial can transparently communicate through a relay
%% node (B) using the dist_relay mechanism.
%%
%% Topology:
%%   A <-- UART1 --> B <-- UART2 --> C
%%
%% The main test process acts as node B (the relay/bridge node) with
%% two UARTs.  Peers A and C are separate AtomVM subprocesses, each
%% with a single UART.

-module(test_serial_dist_relay_socat).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    case erlang:system_info(machine) of
        "ATOM" ->
            case has_socat() andalso has_working_ptys() of
                true ->
                    %% Pre-warm JIT compilation of modules used during
                    %% the distribution handshake so their compilation
                    %% doesn't eat into the handshake timer.
                    _ = crypto:module_info(),
                    _ = dist_util:module_info(),
                    _ = uart:module_info(),
                    _ = timer_manager:module_info(),
                    _ = dist_relay:module_info(),
                    _ = dist_relay_controller:module_info(),
                    ok = test_relay_ping(),
                    ok = test_relay_rpc(),
                    ok;
                false ->
                    io:format("test_serial_dist_relay_socat: socat/ptys not available, skipping~n"),
                    ok
            end;
        _ ->
            io:format("test_serial_dist_relay_socat: skipping on BEAM~n"),
            ok
    end.

%%--------------------------------------------------------------------
%% Test: relay ping (A pings C through B)
%%--------------------------------------------------------------------

test_relay_ping() ->
    run_relay_test("relay_ping").

test_relay_rpc() ->
    run_relay_test("relay_rpc").

run_relay_test(TestName) ->
    %% Create two socat pairs: A<->B and B<->C
    {OsPid1, SocatFd1, PtyB1, PtyA} = start_socat(),
    case start_socat() of
        {OsPid2, SocatFd2, PtyB2, PtyC} ->
            try
                do_run_relay_test(TestName, PtyB1, PtyB2, PtyA, PtyC)
            after
                catch net_kernel:stop(),
                stop_socat(OsPid1, SocatFd1),
                stop_socat(OsPid2, SocatFd2)
            end;
        _ ->
            stop_socat(OsPid1, SocatFd1),
            io:format("test_serial_dist_relay_socat: not enough PTYs, skipping ~s~n", [TestName]),
            ok
    end.

do_run_relay_test(TestName, PtyB1, PtyB2, PtyA, PtyC) ->
    %% Start node B (this process) with two UARTs
    {ok, _} = net_kernel:start('b@serial.local', #{
        name_domain => longnames,
        proto_dist => serial_dist,
        avm_dist_opts => #{
            uart_ports => [
                [{peripheral, binary_to_list(PtyB1)}, {speed, 115200}],
                [{peripheral, binary_to_list(PtyB2)}, {speed, 115200}]
            ],
            uart_module => uart
        }
    }),
    erlang:set_cookie('SerialTest'),

    %% Start peer C first (it just registers and waits for messages)
    PeerFdC = start_relay_peer(PtyC, "relay_c"),

    %% Wait for C to connect to B
    ok = wait_for_node('c@serial.local', 30),
    io:format("test_serial_dist_relay_socat: B-C connected~n"),

    %% Start peer A (it will connect to B, then try to relay to C)
    PeerFdA = start_relay_peer(PtyA, TestName ++ "_a"),

    %% Wait for A to connect to B
    ok = wait_for_node('a@serial.local', 30),
    io:format("test_serial_dist_relay_socat: B-A connected~n"),

    %% Read results from peers
    ResultA = read_peer_line(PeerFdA),
    io:format("test_serial_dist_relay_socat: ~s peer A result: ~s~n", [TestName, ResultA]),

    Expected = expected_result(TestName),
    Expected = ResultA,

    catch unregister(test_serial),
    atomvm:posix_close(PeerFdA),
    atomvm:posix_close(PeerFdC),
    ok.

expected_result("relay_ping") -> <<"relay_pong">>;
expected_result("relay_rpc") -> <<"relay_rpc_ok">>.

wait_for_node(_Node, 0) ->
    error(node_connect_timeout);
wait_for_node(Node, Retries) ->
    case lists:member(Node, nodes()) of
        true ->
            ok;
        false ->
            receive after 1000 -> ok end,
            wait_for_node(Node, Retries - 1)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

start_socat() ->
    {ok, OsPid, Fd} = atomvm:subprocess(
        "/bin/sh",
        ["sh", "-c", "socat -d -d pty,raw,echo=0 pty,raw,echo=0 2>&1"],
        undefined,
        [stdout]
    ),
    PtyA = extract_pty(read_line(Fd)),
    PtyB = extract_pty(read_line(Fd)),
    receive
    after 200 -> ok
    end,
    {OsPid, Fd, PtyA, PtyB}.

stop_socat(OsPid, Fd) ->
    atomvm:posix_close(Fd),
    {ok, _, KillFd} = atomvm:subprocess(
        "/bin/kill", ["kill", integer_to_list(OsPid)], undefined, [stdout]
    ),
    atomvm:posix_close(KillFd).

start_relay_peer(PtyPath, TestName) ->
    AvmBin = find_atomvm_binary(),
    PeerAvm = find_peer_avm(),
    Cmd =
        "PTY_PATH=" ++ binary_to_list(PtyPath) ++
            " TEST_NAME=" ++ TestName ++
            " " ++ AvmBin ++ " " ++ PeerAvm ++ " 2>&1",
    io:format("test_serial_dist_relay_socat: peer cmd: ~s~n", [Cmd]),
    {ok, _PeerPid, PeerFd} = atomvm:subprocess(
        "/bin/sh", ["sh", "-c", Cmd], undefined, [stdout]
    ),
    PeerFd.

find_atomvm_binary() ->
    case atomvm:posix_stat("./src/AtomVM") of
        {ok, _} ->
            "./src/AtomVM";
        _ ->
            case atomvm:posix_stat("./AtomVM") of
                {ok, _} -> "./AtomVM";
                _ -> "AtomVM"
            end
    end.

find_peer_avm() ->
    case atomvm:posix_stat("./tests/libs/estdlib/test_serial_dist_relay_socat_peer.avm") of
        {ok, _} -> "./tests/libs/estdlib/test_serial_dist_relay_socat_peer.avm";
        _ -> "test_serial_dist_relay_socat_peer.avm"
    end.

read_line(Fd) ->
    read_line(Fd, <<>>).

read_line(Fd, Acc) ->
    case atomvm:posix_read(Fd, 1) of
        {ok, <<$\n>>} ->
            Acc;
        {ok, Byte} ->
            read_line(Fd, <<Acc/binary, Byte/binary>>);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} -> ok
            after 5000 ->
                exit(socat_read_timeout)
            end,
            read_line(Fd, Acc)
    end.

read_peer_line(Fd) ->
    Line = read_peer_line(Fd, <<>>),
    %% Skip JIT compilation output, OTP reports, and other diagnostic lines
    case Line of
        <<"Compilation of ", _/binary>> -> read_peer_line(Fd);
        <<"+Compilation of ", _/binary>> -> read_peer_line(Fd);
        <<"Unable to open ", _/binary>> -> read_peer_line(Fd);
        <<"Failed load module", _/binary>> -> read_peer_line(Fd);
        <<"Warning", _/binary>> -> read_peer_line(Fd);
        <<"=", _/binary>> -> read_peer_line(Fd);
        <<" ", _/binary>> -> read_peer_line(Fd);
        <<>> -> read_peer_line(Fd);
        _ -> Line
    end.

read_peer_line(Fd, Acc) ->
    case atomvm:posix_read(Fd, 1) of
        {ok, <<$\n>>} ->
            Acc;
        {ok, <<$\r>>} ->
            read_peer_line(Fd, Acc);
        {ok, Byte} ->
            read_peer_line(Fd, <<Acc/binary, Byte/binary>>);
        {error, eagain} ->
            ok = atomvm:posix_select_read(Fd, self(), undefined),
            receive
                {select, _FdRes, undefined, ready_input} -> ok
            after 60000 ->
                exit({peer_read_timeout, got_so_far, Acc})
            end,
            read_peer_line(Fd, Acc);
        eof ->
            Acc
    end.

extract_pty(Line) ->
    case binary:match(Line, <<"PTY is ">>) of
        {Pos, Len} ->
            binary:part(Line, Pos + Len, byte_size(Line) - Pos - Len);
        nomatch ->
            exit({unexpected_socat_output, Line})
    end.

has_socat() ->
    try
        {ok, _, Fd} = atomvm:subprocess(
            "/bin/sh", ["sh", "-c", "command -v socat"], undefined, [stdout]
        ),
        Result =
            case atomvm:posix_read(Fd, 200) of
                eof -> false;
                {ok, _} -> true
            end,
        ok = atomvm:posix_close(Fd),
        Result
    catch
        _:_ -> false
    end.

%% Verify that socat ptys support termios (fails under qemu-user).
has_working_ptys() ->
    try
        {OsPid, SocatFd, PtyA, _PtyB} = start_socat(),
        Result =
            case atomvm:posix_open(PtyA, [o_rdwr, o_noctty]) of
                {ok, Fd} ->
                    case atomvm:posix_tcgetattr(Fd) of
                        {ok, _} ->
                            atomvm:posix_close(Fd),
                            true;
                        {error, _} ->
                            atomvm:posix_close(Fd),
                            false
                    end;
                {error, _} ->
                    false
            end,
        stop_socat(OsPid, SocatFd),
        Result
    catch
        _:_ -> false
    end.
