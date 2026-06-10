%
% This file is part of AtomVM.
%
% Copyright 2025 Tomasz Sobkiewicz <tomasz.sobkiewicz@swmansion.com>
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

-module(test_file).
-export([start/0, test/0, until_dot/3]).

start() ->
    test().

test() ->
    Res = file:native_name_encoding(),
    ok = is_proper_encoding(Res),
    ok = test_get_cwd(),
    HasRealFs = erlang:system_info(machine) == "BEAM" orelse atomvm:platform() == generic_unix,
    if
        HasRealFs ->
            ok = test_write_read_delete(),
            ok = test_rename(),
            ok = test_open_read_close(),
            ok = test_position(),
            ok = test_get_until();
        true ->
            ok
    end,
    ok.

tmp_path(Name) ->
    {ok, Cwd} = file:get_cwd(),
    _ = Cwd,
    "/tmp/atomvm_test_file_" ++ Name ++ "_" ++ integer_to_list(erlang:system_time(millisecond)).

test_write_read_delete() ->
    Path = tmp_path("wrd"),
    ok = file:write_file(Path, <<"hello world">>),
    {ok, <<"hello world">>} = file:read_file(Path),
    ok = file:delete(Path),
    {error, enoent} = file:read_file(Path),
    {error, enoent} = file:delete(Path),
    ok.

test_rename() ->
    PathA = tmp_path("rena"),
    PathB = tmp_path("renb"),
    ok = file:write_file(PathA, <<"content">>),
    ok = file:rename(PathA, PathB),
    {error, enoent} = file:read_file(PathA),
    {ok, <<"content">>} = file:read_file(PathB),
    ok = file:delete(PathB),
    ok.

test_open_read_close() ->
    Path = tmp_path("orc"),
    ok = file:write_file(Path, <<"0123456789">>),
    {ok, Fd} = file:open(Path, [read]),
    true = is_pid(Fd),
    {ok, "0123"} = file:read(Fd, 4),
    {ok, "456789"} = file:read(Fd, 100),
    eof = file:read(Fd, 1),
    ok = file:close(Fd),
    {error, enoent} = file:open(tmp_path("missing"), [read]),
    ok = file:delete(Path),
    ok.

test_position() ->
    Path = tmp_path("pos"),
    ok = file:write_file(Path, <<"abcdef">>),
    {ok, Fd} = file:open(Path, [read]),
    {ok, "ab"} = file:read(Fd, 2),
    {ok, 2} = file:position(Fd, cur),
    {ok, 0} = file:position(Fd, 0),
    {ok, "abcd"} = file:read(Fd, 4),
    {ok, 1} = file:position(Fd, {bof, 1}),
    {ok, "bc"} = file:read(Fd, 2),
    ok = file:close(Fd),
    ok = file:delete(Path),
    ok.

%% A continuation collector in the style erl_scan:tokens/3 is used by the io
%% protocol get_until request: collect characters up to and including a dot,
%% across chunk boundaries.
until_dot(Cont, eof, _Loc) ->
    case Cont of
        [] -> {done, eof, []};
        _ -> {done, {ok, lists:reverse(Cont)}, []}
    end;
until_dot(Cont, Chars, Loc) ->
    until_dot0(Cont, Chars, Loc).

until_dot0(Cont, [], _Loc) ->
    {more, Cont};
until_dot0(Cont, [$. | Rest], _Loc) ->
    {done, {ok, lists:reverse([$. | Cont])}, Rest};
until_dot0(Cont, [C | Rest], Loc) ->
    until_dot0([C | Cont], Rest, Loc).

test_get_until() ->
    Path = tmp_path("until"),
    ok = file:write_file(Path, <<"abc.def.tail">>),
    {ok, Fd} = file:open(Path, [read]),
    {ok, "abc."} = io_request_get_until(Fd, {1, 1}),
    %% leftover characters after the dot must be buffered for the next request
    {ok, "def."} = io_request_get_until(Fd, {2, 1}),
    {ok, "tail"} = io_request_get_until(Fd, {3, 1}),
    eof = io_request_get_until(Fd, {4, 1}),
    ok = file:close(Fd),
    ok = file:delete(Path),
    ok.

io_request_get_until(Fd, Loc) ->
    Ref = make_ref(),
    Fd ! {io_request, self(), Ref, {get_until, unicode, '', ?MODULE, until_dot, [Loc]}},
    receive
        {io_reply, Ref, Result} -> Result
    after 5000 ->
        error(io_request_timeout)
    end.

is_proper_encoding(utf8) ->
    ok;
is_proper_encoding(latin1) ->
    ok;
is_proper_encoding(_) ->
    error.

test_get_cwd() ->
    {ok, Path} = file:get_cwd(),
    CanValidate = erlang:system_info(machine) == "BEAM" orelse atomvm:platform() == generic_unix,
    if
        CanValidate -> validate_path(Path);
        true -> ok
    end.

validate_path("/" ++ _Rest) -> ok.
