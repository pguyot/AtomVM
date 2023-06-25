%
% This file is part of AtomVM.
%
% Copyright 2020 Fred Dushin <fred@dushin.net>
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

-module(test_gc).

-export([start/0]).

start() ->
    {HeapSize, _} = make_a_big_heap(),
    MemorySize = erlang:process_info(self(), memory),
    true = erlang:garbage_collect(),
    NewHeapSize = erlang:process_info(self(), heap_size),
    erlang:display({new_heap_size, NewHeapSize, heap_size, HeapSize}),
    ok =
        case NewHeapSize < HeapSize of
            true -> ok;
            _ -> {fail, new_heap_size, NewHeapSize, heap_size, HeapSize}
        end,
    NewMemorySize = erlang:process_info(self(), memory),
    erlang:display({new_memory_size, NewMemorySize, memory_size, MemorySize}),
    ok =
        case NewMemorySize < MemorySize of
            true -> ok;
            _ -> {fail, new_memory_size, NewMemorySize, memory_size, MemorySize}
        end,
    0.

make_a_big_heap() ->
    LargeBlob = create_blob(512, []),
    HeapSize = erlang:process_info(self(), heap_size),
    {HeapSize, length(LargeBlob)}.

create_blob(0, Accum) ->
    Accum;
create_blob(Len, Accum) ->
    create_blob(Len - 1, [{Len rem 256} | Accum]).
