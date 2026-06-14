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
            ok = test_get_until(),
            ok = test_read_file_info(),
            ok = test_list_dir_make_dir(),
            ok = test_path_open(),
            ok = test_device_write(),
            ok = test_close_no_exit_to_trapping_owner();
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
    % write_file/3 with default modes, as used by the compiler's write_binary
    ok = file:write_file(Path, <<"opts">>, []),
    {ok, <<"opts">>} = file:read_file(Path),
    ok = file:write_file(Path, <<"raw">>, [raw, binary]),
    {ok, <<"raw">>} = file:read_file(Path),
    ok = file:delete(Path),
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
    Opts = io:getopts(Fd),
    {binary, false} = lists:keyfind(binary, 1, Opts),
    {encoding, _} = lists:keyfind(encoding, 1, Opts),
    %% temporarily switch to binary mode and back, as epp's encoding
    %% detection does
    ok = io:setopts(Fd, [binary, {encoding, latin1}]),
    {ok, <<"01">>} = file:read(Fd, 2),
    {ok, 0} = file:position(Fd, 0),
    ok = io:setopts(Fd, [{binary, false}, {encoding, unicode}]),
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

test_read_file_info() ->
    Path = tmp_path("info"),
    ok = file:write_file(Path, <<"12345">>),
    {ok, Info} = file:read_file_info(Path),
    %% #file_info{} as defined in kernel/include/file.hrl
    file_info = element(1, Info),
    5 = element(2, Info),
    regular = element(3, Info),
    {{_, _, _}, {_, _, _}} = element(6, Info),
    {error, enoent} = file:read_file_info(tmp_path("missing")),
    ok = file:delete(Path),
    ok.

test_list_dir_make_dir() ->
    Dir = tmp_path("dir"),
    ok = file:make_dir(Dir),
    {ok, []} = file:list_dir(Dir),
    ok = file:write_file(Dir ++ "/a.txt", <<"a">>),
    ok = file:write_file(Dir ++ "/b.txt", <<"b">>),
    {ok, Names} = file:list_dir(Dir),
    ["a.txt", "b.txt"] = lists:sort(Names),
    {error, enoent} = file:list_dir(tmp_path("missingdir")),
    ok = file:delete(Dir ++ "/a.txt"),
    ok = file:delete(Dir ++ "/b.txt"),
    ok.

test_path_open() ->
    Dir = tmp_path("po"),
    ok = file:make_dir(Dir),
    ok = file:write_file(Dir ++ "/inc.hrl", <<"x.">>),
    {ok, Fd, FullName} = file:path_open([Dir, "/nonexistent"], "inc.hrl", [read]),
    true = is_pid(Fd),
    true = lists:suffix("inc.hrl", FullName),
    {ok, "x."} = file:read(Fd, 10),
    ok = file:close(Fd),
    {error, enoent} = file:path_open([Dir], "missing.hrl", [read]),
    ok = file:delete(Dir ++ "/inc.hrl"),
    ok.

test_device_write() ->
    Path = tmp_path("devw"),
    {ok, Fd} = file:open(Path, [write]),
    ok = file:write(Fd, <<"hello ">>),
    ok = file:write(Fd, "world"),
    ok = file:close(Fd),
    {ok, <<"hello world">>} = file:read_file(Path),
    ok = file:delete(Path),
    ok.

%% Regression: closing a file must not deliver an {'EXIT', Device, _} signal to
%% an owner that traps exits. epp opens include files while trapping exits and
%% turns any {'EXIT',_,R} into exit(R), so a stray normal-exit from the file
%% device would abort every -include directive (whole-module compile failure).
test_close_no_exit_to_trapping_owner() ->
    Path = tmp_path("noexit"),
    ok = file:write_file(Path, <<"hello">>),
    Parent = self(),
    Child = spawn(fun() ->
        process_flag(trap_exit, true),
        {ok, Fd} = file:open(Path, [read]),
        {ok, "hello"} = file:read(Fd, 5),
        ok = file:close(Fd),
        %% Any stray EXIT from the device would arrive as a message here.
        Got =
            receive
                {'EXIT', _, _} = E -> E
            after 200 -> none
            end,
        Parent ! {self(), Got}
    end),
    Result =
        receive
            {Child, R} -> R
        after 5000 -> timeout
        end,
    none = Result,
    ok = file:delete(Path),
    ok.
