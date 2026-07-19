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
%

-module(jit_stream_size).

% Counting stream: tracks only the write offset, discarding all content. Used
% for the sizing pass of the two-pass flash compile (jit:compile_sizing): the
% backtrack loop converges the forward fused-branch size hints here, without
% holding any output, so the subsequent single emission pass to flash is
% guaranteed overflow-free and can flush eagerly. Compilation is a pure
% function of its input and the hints, so offsets computed on this stream are
% exact.

-export([
    new/1,
    offset/1,
    committed_offset/1,
    append/2,
    replace/3,
    reset/2,
    flush/1
]).

-export_type([stream/0]).

-type stream() :: non_neg_integer().

-spec new(MaxSize :: non_neg_integer()) -> stream().
new(_MaxSize) ->
    0.

-spec offset(stream()) -> non_neg_integer().
offset(Offset) ->
    Offset.

%% Nothing is ever committed: the whole-module backtrack is always possible.
-spec committed_offset(stream()) -> 0.
committed_offset(_Offset) ->
    0.

-spec append(stream(), binary()) -> stream().
append(Offset, Binary) ->
    Offset + byte_size(Binary).

-spec replace(stream(), non_neg_integer(), binary()) -> stream().
replace(Offset, _ReplaceOffset, _Binary) ->
    Offset.

-spec reset(stream(), non_neg_integer()) -> stream().
reset(_Offset, NewOffset) ->
    NewOffset.

-spec flush(stream()) -> stream().
flush(Offset) ->
    Offset.
