%
% This file is part of AtomVM.
%
% Copyright 2022 Davide Bettio <davide@uninstall.it>
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

-module(test_time_and_processes).
-export([start/0, wait/2]).

-define(INTERVAL_1, 50).
-define(INTERVAL_2, 10).

start() ->
    T1 = erlang:system_time(millisecond),
    Pid1 = spawn(?MODULE, wait, [self(), ?INTERVAL_1]),
    Pid2 = spawn(?MODULE, wait, [self(), ?INTERVAL_2]),
    receive
        {done, Pid2} -> ok
    end,
    T2 = erlang:system_time(millisecond),
    receive
        {done, Pid1} -> ok
    end,
    T3 = erlang:system_time(millisecond),
    true = T3 - T1 >= ?INTERVAL_1,
    true = T2 - T1 >= ?INTERVAL_2,
    ok.

wait(Pid, Interval) ->
    receive
    after Interval ->
        Pid ! {done, self()}
    end.
