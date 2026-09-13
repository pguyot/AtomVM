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
%% Break the ESTONE `fcalls' micro down into its six call kinds, so the arm32
%% JIT's per-call cost can be attributed instead of guessed at.
%%
%% The bodies are copied verbatim from estone_test.erl; `all' repeats the exact
%% sequence the micro runs, as a cross-check that the six parts add up.
%%
-module(fcalls_bench).

-export([
    start/0,
    local0/1,
    local1/1,
    remote0/1,
    remote1/1,
    app0/1,
    app1/1
]).

-define(REPS, 882).
-define(INNER, 400).

start() ->
    % Same warm-up shape as the suite: run everything once before timing.
    _ = bench(each, 1),
    lists:foreach(
        fun(Name) ->
            Micros = bench(Name, ?REPS),
            io:format("fcall ~p ~p~n", [Name, Micros])
        end,
        [local0, remote0, app0, local1, remote1, app1, all]
    ),
    ok.

bench(Name, Reps) ->
    Before = erlang:monotonic_time(),
    ok = repeat(Name, Reps),
    After = erlang:monotonic_time(),
    erlang:convert_time_unit(After - Before, native, microsecond).

repeat(_Name, 0) ->
    ok;
repeat(Name, N) ->
    _ = one(Name),
    repeat(Name, N - 1).

one(local0) ->
    local0(?INNER);
one(remote0) ->
    remote0(?INNER);
one(app0) ->
    app0(?INNER);
one(local1) ->
    local1(?INNER);
one(remote1) ->
    remote1(?INNER);
one(app1) ->
    app1(?INNER);
one(all) ->
    local0(?INNER),
    remote0(?INNER),
    app0(?INNER),
    local1(?INNER),
    remote1(?INNER),
    app1(?INNER);
one(each) ->
    one(all).

local0(0) -> 0;
local0(N) -> local0(N - 1).

local1(0) -> 0;
local1(N) -> 1 + local1(N - 1).

remote0(0) -> 0;
remote0(N) -> ?MODULE:remote0(N - 1).

remote1(0) -> 0;
remote1(N) -> 1 + ?MODULE:remote1(N - 1).

app0(0) -> 0;
app0(N) -> apply(?MODULE, app0, [N - 1]).

app1(0) -> 0;
app1(N) -> 1 + apply(?MODULE, app1, [N - 1]).
