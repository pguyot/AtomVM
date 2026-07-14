%
% This file is part of AtomVM.
%
% Copyright 2023 Davide Bettio <davide@uninstall.it>
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

-module(test_esp_partition).
-export([start/0]).

start() ->
    ok = assert_partition_layout(esp:partition_list()),
    ok = esp:partition_erase_range(<<"data">>, 0),
    ok = esp:partition_write(<<"data">>, 0, <<"hello">>),
    {ok, <<"hello">>} = esp:partition_read(<<"data">>, 0, 5),
    ok = mmap_expect(<<"hello">>),
    ok = esp:partition_erase_range(<<"data">>, 0, 4096),
    ok = esp:partition_write(<<"data">>, 0, <<"world">>),
    {ok, <<"world">>} = esp:partition_read(<<"data">>, 0, 5),
    ok = mmap_expect(<<"world">>),
    0.

%% Map the partition in a short-lived process. A mapping lives until its
%% resource-backed binary is collected (the resource destructor calls
%% esp_partition_munmap), and MMU-mapped chips (esp32c3/s3) refuse to map the
%% same paddr block twice; whether the caller's dead mapping has been
%% collected by the time of a second mmap depends on execution mode and GC
%% timing (JIT-compiled code allocates less, and a dead tuple parked in a
%% stale x register survives even an explicit garbage_collect). Scoping the
%% mapping to a terminated process guarantees release; the bounded retry
%% absorbs the window where 'DOWN' was delivered but the mapping resource is
%% still being torn down.
mmap_expect(Expected) ->
    mmap_expect(Expected, 100).

mmap_expect(_Expected, 0) ->
    error;
mmap_expect(Expected, Retries) ->
    {Pid, Ref} = spawn_opt(
        fun() ->
            {ok, Expected} = esp:partition_mmap(<<"data">>, 0, 5)
        end,
        [monitor]
    ),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        {'DOWN', Ref, process, Pid, _Crash} ->
            receive
            after 10 -> ok
            end,
            mmap_expect(Expected, Retries - 1)
    end.

%% Wokwi uses the 4MB test/partitions.csv layout; QEMU/JIT tests use
%% the larger 8MB partitions-test.csv layout.
assert_partition_layout([
    {<<"nvs">>, 1, 2, 16#9000, 16#6000, []},
    {<<"phy_init">>, 1, 1, 16#f000, 16#1000, []},
    {<<"factory">>, 0, 0, 16#10000, 16#2C0000, []},
    {<<"lib.avm">>, 1, 1, 16#2D0000, 16#40000, []},
    {<<"main.avm">>, 1, 1, 16#310000, 16#40000, []},
    {<<"data">>, 1, 1, 16#350000, 16#10000, []}
]) ->
    ok;
assert_partition_layout([
    {<<"nvs">>, 1, 2, 16#9000, 16#6000, []},
    {<<"phy_init">>, 1, 1, 16#f000, 16#1000, []},
    {<<"factory">>, 0, 0, 16#10000, 16#500000, []},
    {<<"lib.avm">>, 1, 1, 16#510000, 16#80000, []},
    {<<"main.avm">>, 1, 1, 16#590000, 16#40000, []},
    {<<"data">>, 1, 1, 16#5D0000, 16#10000, []}
]) ->
    ok.
