%
% This file is part of AtomVM.
%
% Copyright 2024 Paul Guyot <pguyot@kallisys.net>
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

-module(test_net_kernel).

-export([test/0, start/0]).

start() ->
    test().

test() ->
    Platform = erlang:system_info(machine),
    case has_erl(Platform) andalso has_epmd(Platform) of
        true ->
            ok = ensure_epmd(Platform),
            ok = setup(Platform),
            ok = test_ping_from_beam(Platform),
            ok = test_fail_with_wrong_cookie(Platform),
            io:format("test passed\n"),
            ok;
        false ->
            io:format("~s: skipped\n", [?MODULE]),
            ok
    end.

has_erl(Platform) ->
    has_command(Platform, "erl").

has_epmd(Platform) ->
    has_command(Platform, "epmd").

has_command("BEAM", Command) ->
    R = os:cmd("command -v " ++ Command),
    R =/= [];
has_command("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "command -v " ++ Command], [], [stdout]),
    Result =
        case atomvm:posix_read(Fd, 200) of
            eof -> false;
            {ok, _Line} -> true
        end,
    ok = atomvm:posix_close(Fd),
    Result.

ensure_epmd("BEAM") ->
    _ = os:cmd("epmd -daemon"),
    ok;
ensure_epmd("ATOM") ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", "epmd -daemon"], [], [stdout]),
    ok = atomvm:posix_close(Fd),
    ok.

test_ping_from_beam(Platform) ->
    {ok, _NetKernelPid} = net_kernel:start(atomvm, #{name_domain => shortnames}),
    Node = node(),
    erlang:set_cookie('AtomVM'),
    Result = execute_command(
        Platform,
        "erl -sname otp -setcookie AtomVM -eval \"R = net_adm:ping('" ++ atom_to_list(Node) ++
            "'), erlang:display(R).\" -s init stop -noshell"
    ),
    "pong" ++ _ = Result,
    net_kernel:stop(),
    ok.

test_fail_with_wrong_cookie(_Platform) ->
    ok.

% On AtomVM, we need to start kernel.
setup("BEAM") ->
    ok;
setup("ATOM") ->
    {ok, _KernelPid} = kernel:start(normal, []),
    ok.

execute_command("BEAM", Command) ->
    os:cmd(Command);
execute_command("ATOM", Command) ->
    {ok, _, Fd} = atomvm:subprocess("/bin/sh", ["sh", "-c", Command], [], [stdout]),
    Result = loop_read(Fd, []),
    ok = atomvm:posix_close(Fd),
    Result.

loop_read(Fd, Acc) ->
    case atomvm:posix_read(Fd, 10) of
        eof ->
            lists:flatten(lists:reverse(Acc));
        {ok, Line} ->
            loop_read(Fd, [binary_to_list(Line) | Acc])
    end.
