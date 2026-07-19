%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_stream_flash).

% Stream implementation using flash, suitable for MCUs with programmable flash

-export([
    new/1,
    offset/1,
    committed_offset/1,
    append/2,
    replace/3,
    reset/2,
    flush_upto/2,
    map/4,
    flush/1
]).

%% Additional nif
-export([
    read/3
]).

-export_type([stream/0]).

-type stream() :: binary().

%%-----------------------------------------------------------------------------
%% @returns An empty binary
%% @param   MaxSize maximum size of the stream
%% @doc     Create a new stream, i.e. return an empty binary
%% @end
%%-----------------------------------------------------------------------------
-spec new(MaxSize :: pos_integer()) -> stream().
new(_MaxSize) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to get the offset from
%% @returns The current offset
%% @doc     Get the current offset in the stream
%% @end
%%-----------------------------------------------------------------------------
-spec offset(stream()) -> non_neg_integer().
offset(_Stream) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to get the backtrack horizon from
%% @returns The flush horizon
%% @doc     Backtrack horizon of the stream (see jit:compile's backtrack loop):
%%          the offset below which content has been written to flash and can
%%          only receive bit-clear (0xFF-placeholder) patches. Content at or
%%          above the horizon is held in RAM and freely rewritable, so the
%%          compiler can emit optimistic fused forward branches and resolve
%%          or re-emit them in-buffer. The horizon advances only through
%%          flush_upto/2.
%% @end
%%-----------------------------------------------------------------------------
-spec committed_offset(stream()) -> non_neg_integer().
committed_offset(_Stream) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to rewind
%% @param Offset    offset to rewind the write cursor to
%% @returns The updated stream
%% @doc     Rewind the write cursor for a backtrack re-emit, restoring the
%%          erased state of the rewound region. Only possible at or above the
%%          flush horizon (flushed flash content is immutable). Used by the
%%          backend's rewind_stream/2.
%% @end
%%-----------------------------------------------------------------------------
-spec reset(stream(), non_neg_integer()) -> stream().
reset(_Stream, _Offset) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to flush
%% @param Offset    offset below which content is final
%% @returns The updated stream
%% @doc     Declare every byte below Offset final: fully-appended pages
%%          entirely below it are written to flash and the flush horizon
%%          advances, bounding the RAM held by the stream.
%% @end
%%-----------------------------------------------------------------------------
-spec flush_upto(stream(), non_neg_integer()) -> stream().
flush_upto(_Stream, _Offset) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream    stream to append to
%% @param Binary    binary to append to the stream
%% @returns The updated stream
%% @doc     Append a binary to the stream
%% @end
%%-----------------------------------------------------------------------------
-spec append(stream(), binary()) -> stream().
append(_Stream, _Binary) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Replacement   binary to write at offset
%% @returns The updated stream
%% @doc     Replace bytes at a given offset
%% @end
%%-----------------------------------------------------------------------------
-spec replace(stream(), non_neg_integer(), binary()) -> stream().
replace(_Stream, _Offset, _Replacement) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream        stream to update
%% @param Offset        offset to update from
%% @param Length        length of the section to update
%% @param MapFunction   function that updates the binary
%% @returns The updated stream
%% @doc     Replace bytes at a given offset calling a map function
%% @end
%%-----------------------------------------------------------------------------
-spec map(stream(), non_neg_integer(), pos_integer(), fun((binary()) -> binary())) -> stream().
map(Stream, Offset, Length, MapFunction) ->
    Binary = ?MODULE:read(Stream, Offset, Length),
    Mapped = MapFunction(Binary),
    ?MODULE:replace(Stream, Offset, Mapped).

%%-----------------------------------------------------------------------------
%% @param Stream        stream to read from
%% @param Offset        offset to read from
%% @param Length        number of bytes to read
%% @returns The binary data read from the stream
%% @doc     Read bytes at a given offset
%%
%% @end
%%-----------------------------------------------------------------------------
-spec read(stream(), non_neg_integer(), pos_integer()) -> binary().
read(_Stream, _Offset, _Length) ->
    erlang:nif_error(undefined).

%%-----------------------------------------------------------------------------
%% @param Stream        stream to flush
%% @returns The stream flushed
%% @doc     Flush the stream. Typically invalidates instruction cache.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec flush(stream()) -> stream().
flush(_Stream) ->
    erlang:nif_error(undefined).
