%
% This file is part of AtomVM.
%
% Copyright 2025 Software Mansion S.A.
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
%

%% @doc A subset of the Erlang/OTP file interface, implemented over the
%% `atomvm:posix_*' functions.
%%
%% `open/2' returns an io device: a process implementing enough of the
%% Erlang io protocol (`get_until', `get_chars') and file requests
%% (`position', `close') for readers such as `epp' and `io:scan_erl_form/4'.
-module(file).

-export([
    native_name_encoding/0,
    get_cwd/0,
    open/2,
    close/1,
    read/2,
    read_line/1,
    position/2,
    read_file/1,
    read_file/2,
    write_file/2,
    write_file/3,
    write/2,
    read_file_info/1,
    read_file_info/2,
    read_link_info/1,
    read_link_info/2,
    read_link/1,
    write_file_info/3,
    list_dir/1,
    list_dir_all/1,
    make_dir/1,
    del_dir/1,
    path_open/3,
    delete/1,
    rename/2,
    copy/2,
    copy/3,
    consult/1,
    change_mode/2,
    change_owner/2,
    change_group/2,
    make_symlink/2,
    make_link/2,
    set_cwd/1,
    get_cwd/1,
    format_error/1
]).

-define(READ_CHUNK, 1024).

-spec native_name_encoding() -> latin1 | utf8.
native_name_encoding() ->
    utf8.

get_cwd() ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Drive a drive letter string (Windows only on Erlang/OTP)
%% @returns `{error, enotsup}'
%% @doc     Return the working directory of a drive. Drives do not exist on
%%          AtomVM platforms, so this always returns `{error, enotsup}', like
%%          Erlang/OTP on non-Windows systems.
%% @end
%%-----------------------------------------------------------------------------
-spec get_cwd(Drive :: string()) -> {error, enotsup}.
get_cwd(_Drive) ->
    {error, enotsup}.

%%-----------------------------------------------------------------------------
%% @param   Dirname name of the directory to change to
%% @returns `ok' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec set_cwd(Dirname :: iodata()) -> ok | {error, any()}.
set_cwd(_Dirname) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to open
%% @param   Modes open modes; `read', `write' and `binary' are supported
%% @returns `{ok, IoDevice}' or `{error, Reason}'
%% @doc     Open a file and return an io device (a process) serving it.
%% @end
%%-----------------------------------------------------------------------------
-spec open(Filename :: iodata(), Modes :: [atom()]) -> {ok, pid()} | {error, any()}.
open(Filename, Modes) ->
    Write = lists:member(write, Modes),
    OpenResult =
        case Write of
            true -> atomvm:posix_open(Filename, [o_wronly, o_creat, o_trunc], 8#644);
            false -> atomvm:posix_open(Filename, [o_rdonly])
        end,
    case OpenResult of
        {ok, Fd} ->
            Binary = lists:member(binary, Modes),
            Owner = self(),
            Pid = spawn_link(fun() ->
                file_server(#{
                    fd => Fd,
                    owner => Owner,
                    buffer => [],
                    binary => Binary,
                    pending_bytes => <<>>,
                    encoding => unicode
                })
            end),
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   IoDevice device returned by `open/2'
%% @returns `ok'
%% @doc     Close an io device.
%% @end
%%-----------------------------------------------------------------------------
-spec close(IoDevice :: pid()) -> ok | {error, any()}.
close(IoDevice) when is_pid(IoDevice) ->
    file_request(IoDevice, close).

%%-----------------------------------------------------------------------------
%% @param   IoDevice device returned by `open/2'
%% @param   Count maximum number of characters to read
%% @returns `{ok, Data}', `eof' or `{error, Reason}'
%% @doc     Read up to Count characters. Data is a list unless the device was
%%          opened in `binary' mode.
%% @end
%%-----------------------------------------------------------------------------
-spec read(IoDevice :: pid(), Count :: non_neg_integer()) ->
    {ok, list() | binary()} | eof | {error, any()}.
read(IoDevice, Count) when is_pid(IoDevice) ->
    file_request(IoDevice, {read, Count}).

%%-----------------------------------------------------------------------------
%% @param   IoDevice device returned by `open/2'
%% @param   Location new position: an offset, `cur', `bof', `eof' or a
%%          `{bof | cur | eof, Offset}' tuple
%% @returns `{ok, AbsolutePosition}' or `{error, Reason}'
%% @doc     Set (or query, with `cur') the device position.
%% @end
%%-----------------------------------------------------------------------------
-spec position(IoDevice :: pid(), Location :: term()) ->
    {ok, non_neg_integer()} | {error, any()}.
position(IoDevice, Location) when is_pid(IoDevice) ->
    file_request(IoDevice, {position, Location}).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to read
%% @returns `{ok, Binary}' or `{error, Reason}'
%% @doc     Read a whole file as a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec read_file(Filename :: iodata()) -> {ok, binary()} | {error, any()}.
read_file(Filename) ->
    case atomvm:posix_open(Filename, [o_rdonly]) of
        {ok, Fd} ->
            Result = read_all(Fd, []),
            _ = atomvm:posix_close(Fd),
            Result;
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to read
%% @param   Opts options; accepted for Erlang/OTP compatibility (the file is
%%          always read as with an empty option list)
%% @returns `{ok, Binary}' or `{error, Reason}'
%% @doc     Read a whole file as a binary.
%% @end
%%-----------------------------------------------------------------------------
-spec read_file(Filename :: iodata(), Opts :: [raw]) -> {ok, binary()} | {error, any()}.
read_file(Filename, _Opts) ->
    read_file(Filename).

%%-----------------------------------------------------------------------------
%% @param   IoDevice device to read a line from
%% @returns `{ok, Line}', `eof' or `{error, Reason}'
%% @doc     Read a line from an io device opened with `open/2'. The
%% terminating newline, if any, is included in the result.
%% @end
%%-----------------------------------------------------------------------------
-spec read_line(IoDevice :: pid()) -> {ok, binary() | string()} | eof | {error, any()}.
read_line(IoDevice) when is_pid(IoDevice) ->
    file_request(IoDevice, read_line).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to write
%% @param   Data data to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write (create or truncate) a whole file.
%% @end
%%-----------------------------------------------------------------------------
-spec write_file(Filename :: iodata(), Data :: iodata()) -> ok | {error, any()}.
write_file(Filename, Data) ->
    write_file(Filename, Data, []).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to write
%% @param   Data data to write
%% @param   Modes open modes; `raw', `binary' and `write' are accepted (and
%%          implied), other modes are not supported
%% @returns `ok' or `{error, Reason}'
%% @doc     Write (create or truncate) a whole file.
%% @end
%%-----------------------------------------------------------------------------
-spec write_file(Filename :: iodata(), Data :: iodata(), Modes :: [atom()]) ->
    ok | {error, any()}.
write_file(Filename, Data, Modes) when is_list(Modes) ->
    case [M || M <- Modes, M =/= raw, M =/= binary, M =/= write] of
        [] ->
            do_write_file(Filename, Data);
        _Unsupported ->
            {error, badarg}
    end.

do_write_file(Filename, Data) ->
    case atomvm:posix_open(Filename, [o_wronly, o_creat, o_trunc], 8#644) of
        {ok, Fd} ->
            Result = write_all(Fd, erlang:iolist_to_binary(Data)),
            _ = atomvm:posix_close(Fd),
            Result;
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to delete
%% @returns `ok' or `{error, Reason}'
%% @doc     Delete a file.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Filename :: iodata()) -> ok | {error, any()}.
delete(Filename) ->
    atomvm:posix_unlink(Filename).

%%-----------------------------------------------------------------------------
%% @param   Source current file name
%% @param   Destination new file name
%% @returns `ok' or `{error, Reason}'
%% @doc     Rename a file.
%% @end
%%-----------------------------------------------------------------------------
-spec rename(Source :: iodata(), Destination :: iodata()) -> ok | {error, any()}.
rename(Source, Destination) ->
    atomvm:posix_rename(Source, Destination).

%%-----------------------------------------------------------------------------
%% @param   Reason an error reason returned by a file operation
%% @returns a descriptive string
%% @doc     Format a file error reason.
%% @end
%%-----------------------------------------------------------------------------
-spec format_error(Reason :: term()) -> string().
format_error(Reason) when is_atom(Reason) ->
    atom_to_list(Reason);
format_error(Reason) ->
    lists:flatten(io_lib:format("~p", [Reason])).

%%
%% internal: client side
%%

%% @private
file_request(IoDevice, Request) ->
    Ref = erlang:monitor(process, IoDevice),
    IoDevice ! {file_request, self(), Ref, Request},
    receive
        {file_reply, Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, IoDevice, _Reason} ->
            {error, terminated}
    end.

%% @private
read_all(Fd, Acc) ->
    case atomvm:posix_read(Fd, ?READ_CHUNK) of
        {ok, Bin} -> read_all(Fd, [Bin | Acc]);
        eof -> {ok, erlang:iolist_to_binary(lists:reverse(Acc))};
        {error, _} = Error -> Error
    end.

%% @private
write_all(_Fd, <<>>) ->
    ok;
write_all(Fd, Bin) ->
    case atomvm:posix_write(Fd, Bin) of
        {ok, Written} when Written =:= byte_size(Bin) ->
            ok;
        {ok, Written} ->
            <<_:Written/binary, Rest/binary>> = Bin,
            write_all(Fd, Rest);
        {error, _} = Error ->
            Error
    end.

%%
%% internal: server side
%%
%% The server owns the posix file descriptor and a readahead buffer (a list
%% of characters already read but not yet consumed, e.g. what a get_until
%% continuation function left over).
%%

%% @private
file_server(State) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            {reply, Reply, NewState} = io_request(Request, State),
            From ! {io_reply, ReplyAs, Reply},
            file_server(NewState);
        {file_request, From, ReplyAs, close} ->
            _ = atomvm:posix_close(maps:get(fd, State)),
            %% This process is about to exit normally. It was spawn_link'ed by
            %% the owner, so unlink first: an owner that traps exits (e.g. epp,
            %% which opens and closes include files) would otherwise receive
            %% {'EXIT', _, normal} and abort. OTP's file io server is likewise
            %% not linked to the user process.
            _ = unlink(maps:get(owner, State)),
            From ! {file_reply, ReplyAs, ok};
        {file_request, From, ReplyAs, Request} ->
            {reply, Reply, NewState} = file_request_impl(Request, State),
            From ! {file_reply, ReplyAs, Reply},
            file_server(NewState)
    end.

%% @private
io_request({get_until, _Encoding, _Prompt, M, F, As}, State) ->
    get_until(M, F, As, [], State);
io_request({get_chars, _Encoding, _Prompt, N}, State) ->
    {Data, NewState} = take_chars(N, State),
    {reply, Data, NewState};
io_request({put_chars, _Encoding, Chars}, State) ->
    {reply, device_write(Chars, State), State};
io_request({put_chars, _Encoding, M, F, A}, State) ->
    {reply, device_write(apply(M, F, A), State), State};
io_request({setopts, Opts}, State) ->
    {reply, ok, apply_opts(Opts, State)};
io_request(getopts, State) ->
    Encoding =
        case maps:get(encoding, State, unicode) of
            latin1 -> latin1;
            _ -> unicode
        end,
    {reply, [{binary, maps:get(binary, State)}, {encoding, Encoding}], State};
io_request(_Other, State) ->
    {reply, {error, request}, State}.

%% @private
%% Apply setopts options: binary mode and encoding selection.
apply_opts([], State) ->
    State;
apply_opts([binary | T], State) ->
    apply_opts(T, State#{binary := true});
apply_opts([{binary, Bool} | T], State) when is_boolean(Bool) ->
    apply_opts(T, State#{binary := Bool});
apply_opts([list | T], State) ->
    apply_opts(T, State#{binary := false});
apply_opts([{encoding, Enc} | T], State) ->
    apply_opts(T, State#{encoding => Enc});
apply_opts([_Other | T], State) ->
    apply_opts(T, State).

%% @private
%% Drive an io-protocol get_until continuation: feed buffered or freshly-read
%% characters to M:F(Continuation, Data, As...) until it returns done.
get_until(M, F, As, Cont, State) ->
    #{buffer := Buffer} = State,
    {Data, State1} =
        case Buffer of
            [] -> read_chars(State);
            _ -> {Buffer, State#{buffer := []}}
        end,
    case apply(M, F, [Cont, Data | As]) of
        {done, Result, RestChars} ->
            NewBuffer =
                case RestChars of
                    eof -> [];
                    _ -> RestChars
                end,
            {reply, Result, State1#{buffer := NewBuffer}};
        {more, Cont1} when Data =/= eof ->
            get_until(M, F, As, Cont1, State1);
        {more, _Cont1} ->
            %% the continuation could not complete on eof
            {reply, {error, collect_failed}, State1}
    end.

%% @private
%% Read the next chunk of the file as a list of characters, decoding utf8;
%% returns eof at end of file. Bytes of an incomplete trailing utf8 sequence
%% are carried over to the next read via pending_bytes.
read_chars(State) ->
    #{fd := Fd, pending_bytes := Pending} = State,
    case atomvm:posix_read(Fd, ?READ_CHUNK) of
        {ok, Bin0} when map_get(encoding, State) =:= latin1 ->
            %% latin1: characters are bytes, no decoding
            Bin = <<Pending/binary, Bin0/binary>>,
            {erlang:binary_to_list(Bin), State#{pending_bytes := <<>>}};
        {ok, Bin0} ->
            Bin = <<Pending/binary, Bin0/binary>>,
            case unicode:characters_to_list(Bin, utf8) of
                List when is_list(List) ->
                    {List, State#{pending_bytes := <<>>}};
                {incomplete, List, RestBytes} ->
                    {List, State#{pending_bytes := RestBytes}};
                {error, _, _} ->
                    %% not valid utf8: fall back to byte-wise (latin1) reading
                    {erlang:binary_to_list(Bin), State#{pending_bytes := <<>>}}
            end;
        eof when Pending =/= <<>> ->
            {erlang:binary_to_list(Pending), State#{pending_bytes := <<>>}};
        eof ->
            {eof, State};
        {error, _} = Error ->
            {Error, State}
    end.

%% @private
%% Take up to N characters for get_chars / read. Returns eof when the file is
%% exhausted and nothing could be read.
take_chars(N, State) ->
    case take_chars0(N, [], State) of
        {[], NewState} -> {eof, NewState};
        {Chars, NewState} -> {{ok, maybe_binary(Chars, NewState)}, NewState}
    end.

%% @private
take_chars0(0, Acc, State) ->
    {lists:reverse(Acc), State};
take_chars0(N, Acc, State) ->
    #{buffer := Buffer} = State,
    case Buffer of
        [C | Rest] ->
            take_chars0(N - 1, [C | Acc], State#{buffer := Rest});
        [] ->
            case read_chars(State) of
                {eof, State1} ->
                    {lists:reverse(Acc), State1};
                {{error, _}, State1} ->
                    {lists:reverse(Acc), State1};
                {Chars, State1} ->
                    take_chars0(N, Acc, State1#{buffer := Chars})
            end
    end.

%% @private
maybe_binary(Chars, State) ->
    case maps:get(binary, State) of
        true ->
            case maps:get(encoding, State, unicode) of
                latin1 -> erlang:list_to_binary(Chars);
                _ -> unicode:characters_to_binary(Chars, utf8)
            end;
        false ->
            Chars
    end.

%% @private
device_write(Data, State) ->
    write_all(maps:get(fd, State), erlang:iolist_to_binary(Data)).

%% @private
file_request_impl({write, Data}, State) ->
    {reply, device_write(Data, State), State};
file_request_impl(read_line, State) ->
    %% Buffered line reads. The line buffer holds raw look-ahead bytes; a
    %% device should not mix read_line with read/2 or the io protocol (the
    %% look-ahead is not shared), which matches how it is used (compilers
    %% read diagnostics snippets line by line and nothing else).
    read_line_buffered(State);
file_request_impl({read, Count}, State) ->
    {Data, NewState} = take_chars(Count, State),
    {reply, Data, NewState};
file_request_impl({position, Location}, State) ->
    #{fd := Fd, buffer := Buffer, pending_bytes := Pending} = State,
    %% The buffer holds characters already read from the fd: the logical
    %% position the client sees is the fd position minus what is buffered.
    %% Buffered character counts equal byte counts only for ASCII content;
    %% epp only repositions to offsets obtained right after open, before any
    %% multi-byte character has been buffered.
    BufferedBytes = length(Buffer) + byte_size(Pending),
    SeekResult =
        case Location of
            cur ->
                case atomvm:posix_seek(Fd, 0, seek_cur) of
                    {ok, FdPos} -> atomvm:posix_seek(Fd, FdPos - BufferedBytes, seek_set);
                    {error, _} = E0 -> E0
                end;
            bof ->
                atomvm:posix_seek(Fd, 0, seek_set);
            eof ->
                atomvm:posix_seek(Fd, 0, seek_end);
            {bof, Offset} ->
                atomvm:posix_seek(Fd, Offset, seek_set);
            {cur, Offset} ->
                case atomvm:posix_seek(Fd, 0, seek_cur) of
                    {ok, FdPos} ->
                        atomvm:posix_seek(Fd, FdPos - BufferedBytes + Offset, seek_set);
                    {error, _} = E1 ->
                        E1
                end;
            {eof, Offset} ->
                atomvm:posix_seek(Fd, Offset, seek_end);
            Offset when is_integer(Offset) ->
                atomvm:posix_seek(Fd, Offset, seek_set)
        end,
    case SeekResult of
        {ok, NewPos} ->
            {reply, {ok, NewPos}, State#{buffer := [], pending_bytes := <<>>}};
        {error, _} = Error ->
            {reply, Error, State}
    end;
file_request_impl(_Other, State) ->
    {reply, {error, request}, State}.

%% @private
read_line_buffered(State) ->
    Buf = maps:get(line_buffer, State, <<>>),
    case binary:split(Buf, <<"\n">>) of
        [Line, Rest] ->
            Data = <<Line/binary, "\n">>,
            {reply, {ok, line_data(Data, State)}, State#{line_buffer => Rest}};
        _ ->
            case atomvm:posix_read(maps:get(fd, State), ?READ_CHUNK) of
                {ok, Bin} ->
                    read_line_buffered(State#{line_buffer => <<Buf/binary, Bin/binary>>});
                eof when Buf =:= <<>> ->
                    {reply, eof, State};
                eof ->
                    {reply, {ok, line_data(Buf, State)}, State#{line_buffer => <<>>}};
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end.

%% @private
line_data(Bin, State) ->
    case maps:get(binary, State) of
        true -> Bin;
        false -> binary_to_list(Bin)
    end.

%%-----------------------------------------------------------------------------
%% @param   IoDevice device returned by `open/2' with the `write' mode
%% @param   Data data to write
%% @returns `ok' or `{error, Reason}'
%% @doc     Write data to an io device.
%% @end
%%-----------------------------------------------------------------------------
-spec write(IoDevice :: pid(), Data :: iodata()) -> ok | {error, any()}.
write(IoDevice, Data) when is_pid(IoDevice) ->
    file_request(IoDevice, {write, Data}).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to stat
%% @returns `{ok, FileInfo}' or `{error, Reason}', where FileInfo is a
%%          `#file_info{}' record as defined in kernel's `file.hrl'
%% @doc     Get information about a file. Unlike Erlang/OTP, times are
%%          universal rather than local.
%% @end
%%-----------------------------------------------------------------------------
-spec read_file_info(File :: iodata() | pid()) -> {ok, tuple()} | {error, any()}.
read_file_info(IoDevice) when is_pid(IoDevice) ->
    %% Stat of an already-open file. AtomVM has no fstat, so the size is found
    %% by seeking to the end and restoring the position; the other #file_info
    %% fields use neutral defaults (callers such as epp's -doc/-moduledoc
    %% handling only need the size).
    case position(IoDevice, cur) of
        {ok, Cur} ->
            case position(IoDevice, eof) of
                {ok, Size} ->
                    _ = position(IoDevice, {bof, Cur}),
                    Info =
                        {file_info, Size, regular, read_write, undefined, undefined, undefined, 0,
                            1, 0, 0, 0, 0, 0},
                    {ok, Info};
                {error, _} = SeekErr ->
                    SeekErr
            end;
        {error, _} = CurErr ->
            CurErr
    end;
read_file_info(Filename) ->
    read_file_info(Filename, []).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to stat
%% @param   Opts options; only `{time, posix | universal | local}' is honored
%%          (`local' is treated as `universal' since AtomVM has no timezone db)
%% @returns `{ok, FileInfo}' or `{error, Reason}'
%% @doc     Return information about a file as a `#file_info{}' record, with the
%% time fields in the requested format. Elixir's `File.dir?'/`File.mkdir_p' call
%% this with `[{time, posix}]'.
%% @end
%%-----------------------------------------------------------------------------
-spec read_file_info(File :: iodata(), Opts :: list()) -> {ok, tuple()} | {error, any()}.
read_file_info(Filename, Opts) ->
    TimeFmt = proplists:get_value(time, Opts, universal),
    case atomvm:posix_stat(Filename) of
        {ok, Stat} ->
            {ok, file_info_from_stat(Stat, TimeFmt)};
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Filename name of the link (or file) to stat
%% @returns `{ok, FileInfo}' or `{error, Reason}'
%% @doc     Get information about a file or link. Unlike Erlang/OTP, symbolic
%%          links are followed (AtomVM has no `lstat' wrapper), so this is
%%          equivalent to {@link read_file_info/1}.
%% @end
%%-----------------------------------------------------------------------------
-spec read_link_info(Filename :: iodata()) -> {ok, tuple()} | {error, any()}.
read_link_info(Filename) ->
    read_file_info(Filename).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the link (or file) to stat
%% @param   Opts options, see {@link read_file_info/2}
%% @returns `{ok, FileInfo}' or `{error, Reason}'
%% @doc     Get information about a file or link, see {@link read_link_info/1}.
%% @end
%%-----------------------------------------------------------------------------
-spec read_link_info(Filename :: iodata(), Opts :: list()) -> {ok, tuple()} | {error, any()}.
read_link_info(Filename, Opts) ->
    read_file_info(Filename, Opts).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the symbolic link to read
%% @returns `{ok, Target}' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec read_link(Filename :: iodata()) -> {ok, string()} | {error, any()}.
read_link(_Filename) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to modify
%% @param   FileInfo a `#file_info{}' record with the fields to set
%% @param   Opts options
%% @returns `ok' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec write_file_info(Filename :: iodata(), FileInfo :: tuple(), Opts :: list()) ->
    ok | {error, any()}.
write_file_info(_Filename, _FileInfo, _Opts) ->
    erlang:nif_error(undefined).

%% @private
file_info_from_stat(Stat, TimeFmt) ->
    #{
        st_dev := Dev,
        st_ino := Ino,
        st_mode := Mode,
        st_nlink := NLink,
        st_uid := Uid,
        st_gid := Gid,
        st_size := Size,
        st_atime_s := ATime,
        st_mtime_s := MTime,
        st_ctime_s := CTime
    } = Stat,
    Type =
        case Mode band 16#F000 of
            16#4000 -> directory;
            16#8000 -> regular;
            16#A000 -> symlink;
            _ -> other
        end,
    %% #file_info{size, type, access, atime, mtime, ctime, mode,
    %%            links, major_device, minor_device, inode, uid, gid}
    {file_info, Size, Type, read_write, format_time(ATime, TimeFmt), format_time(MTime, TimeFmt),
        format_time(CTime, TimeFmt), Mode, NLink, Dev, 0, Ino, Uid, Gid}.

%% @private
format_time(PosixSeconds, posix) ->
    PosixSeconds;
format_time(PosixSeconds, _Universal) ->
    calendar:system_time_to_universal_time(PosixSeconds, second).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file
%% @param   Mode the desired permission bits (ignored)
%% @returns `ok'
%% @doc     Change file permissions. AtomVM has no `chmod' syscall wrapper, so
%% this is a best-effort no-op that reports success — provided for compatibility
%% with code (e.g. Elixir's `File.chmod') that expects the function to exist.
%% @end
%%-----------------------------------------------------------------------------
-spec change_mode(Filename :: iodata(), Mode :: non_neg_integer()) -> ok | {error, any()}.
change_mode(_Filename, _Mode) ->
    ok.

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file
%% @param   Uid the new owner
%% @returns `ok' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec change_owner(Filename :: iodata(), Uid :: integer()) -> ok | {error, any()}.
change_owner(_Filename, _Uid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file
%% @param   Gid the new group
%% @returns `ok' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec change_group(Filename :: iodata(), Gid :: integer()) -> ok | {error, any()}.
change_group(_Filename, _Gid) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Existing target of the symbolic link
%% @param   New name of the symbolic link to create
%% @returns `ok' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec make_symlink(Existing :: iodata(), New :: iodata()) -> ok | {error, any()}.
make_symlink(_Existing, _New) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Existing target of the hard link
%% @param   New name of the hard link to create
%% @returns `ok' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM.
%% @end
%%-----------------------------------------------------------------------------
-spec make_link(Existing :: iodata(), New :: iodata()) -> ok | {error, any()}.
make_link(_Existing, _New) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Source name of the file to copy
%% @param   Destination name of the file to copy to
%% @returns `{ok, BytesCopied}' or `{error, Reason}'
%% @doc     Copy a file. Unlike Erlang/OTP, only filenames are supported
%%          (not io devices).
%% @end
%%-----------------------------------------------------------------------------
-spec copy(Source :: iodata(), Destination :: iodata()) ->
    {ok, non_neg_integer()} | {error, any()}.
copy(Source, Destination) ->
    copy(Source, Destination, infinity).

%%-----------------------------------------------------------------------------
%% @param   Source name of the file to copy
%% @param   Destination name of the file to copy to
%% @param   ByteCount maximum number of bytes to copy, or `infinity'
%% @returns `{ok, BytesCopied}' or `{error, Reason}'
%% @doc     Copy at most `ByteCount' bytes of a file. Unlike Erlang/OTP, only
%%          filenames are supported (not io devices).
%% @end
%%-----------------------------------------------------------------------------
-spec copy(Source :: iodata(), Destination :: iodata(), ByteCount :: non_neg_integer() | infinity) ->
    {ok, non_neg_integer()} | {error, any()}.
copy(Source, Destination, ByteCount) ->
    case read_file(Source) of
        {ok, Bin} ->
            Data =
                case ByteCount of
                    infinity ->
                        Bin;
                    N when is_integer(N), N >= 0, N < byte_size(Bin) ->
                        <<Part:N/binary, _/binary>> = Bin,
                        Part;
                    N when is_integer(N), N >= 0 ->
                        Bin
                end,
            case write_file(Destination, Data) of
                ok -> {ok, byte_size(Data)};
                {error, _} = WriteError -> WriteError
            end;
        {error, _} = ReadError ->
            ReadError
    end.

%%-----------------------------------------------------------------------------
%% @param   Filename name of the file to read Erlang terms from
%% @returns `{ok, Terms}' or `{error, Reason}'
%% @doc Compatibility stub; not supported on AtomVM (no `erl_scan' /
%% `erl_parse' in estdlib).
%% @end
%%-----------------------------------------------------------------------------
-spec consult(Filename :: iodata()) -> {ok, [term()]} | {error, any()}.
consult(_Filename) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param   Dirname name of the directory to list
%% @returns `{ok, Filenames}' or `{error, Reason}'
%% @doc     List the files of a directory ("." and ".." excluded).
%% @end
%%-----------------------------------------------------------------------------
-spec list_dir(Dirname :: iodata()) -> {ok, [string()]} | {error, any()}.
list_dir(Dirname) ->
    case atomvm:posix_opendir(Dirname) of
        {ok, Dir} ->
            Result = list_dir0(Dir, []),
            _ = atomvm:posix_closedir(Dir),
            Result;
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Dirname name of the directory to list
%% @returns `{ok, Filenames}' or `{error, Reason}'
%% @doc     List the files of a directory, like {@link list_dir/1}. On AtomVM
%%          all names are returned as strings, so this is equivalent to
%%          {@link list_dir/1}.
%% @end
%%-----------------------------------------------------------------------------
-spec list_dir_all(Dirname :: iodata()) -> {ok, [string()]} | {error, any()}.
list_dir_all(Dirname) ->
    list_dir(Dirname).

%% @private
list_dir0(Dir, Acc) ->
    case atomvm:posix_readdir(Dir) of
        {ok, {dirent, _Ino, NameBin}} ->
            case NameBin of
                <<".">> -> list_dir0(Dir, Acc);
                <<"..">> -> list_dir0(Dir, Acc);
                _ -> list_dir0(Dir, [erlang:binary_to_list(NameBin) | Acc])
            end;
        eof ->
            {ok, lists:reverse(Acc)};
        {error, _} = Error ->
            Error
    end.

%%-----------------------------------------------------------------------------
%% @param   Dirname name of the directory to create
%% @returns `ok' or `{error, Reason}'
%% @doc     Create a directory.
%% @end
%%-----------------------------------------------------------------------------
-spec make_dir(Dirname :: iodata()) -> ok | {error, any()}.
make_dir(Dirname) ->
    atomvm:posix_mkdir(Dirname, 8#755).

%%-----------------------------------------------------------------------------
%% @param   Dirname name of the directory to delete
%% @returns `ok' or `{error, Reason}'
%% @doc     Delete a directory. The directory must be empty.
%% @end
%%-----------------------------------------------------------------------------
-spec del_dir(Dirname :: iodata()) -> ok | {error, any()}.
del_dir(Dirname) ->
    atomvm:posix_rmdir(Dirname).

%%-----------------------------------------------------------------------------
%% @param   Path list of directories to search
%% @param   Filename name of the file to open
%% @param   Modes open modes, see `open/2'
%% @returns `{ok, IoDevice, FullName}' or `{error, Reason}'
%% @doc     Search a list of directories for a file and open it. The name is
%%          also tried as-is (equivalent to a "." path entry coming first
%%          when the name is relative, per Erlang/OTP semantics for absolute
%%          names).
%% @end
%%-----------------------------------------------------------------------------
-spec path_open(Path :: [iodata()], Filename :: iodata(), Modes :: [atom()]) ->
    {ok, pid(), string()} | {error, any()}.
path_open(Path, Filename, Modes) ->
    case filename:pathtype(Filename) of
        absolute ->
            case open(Filename, Modes) of
                {ok, Fd} -> {ok, Fd, Filename};
                {error, _} = Error -> Error
            end;
        _ ->
            path_open0(Path, Filename, Modes)
    end.

%% @private
path_open0([], _Filename, _Modes) ->
    {error, enoent};
path_open0([Dir | Rest], Filename, Modes) ->
    FullName = filename:join(Dir, Filename),
    case open(FullName, Modes) of
        {ok, Fd} -> {ok, Fd, FullName};
        {error, _} -> path_open0(Rest, Filename, Modes)
    end.
