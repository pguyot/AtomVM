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

-module(test_improper_cmp).

-export([start/0, id/1]).

%% Ordering of improper lists: a scalar tail compares against nil by term
%% order (number < atom < ... < nil), so an improper list sorts BELOW its
%% proper prefix. A regression test for the invalid-term-as-bottom scheme
%% that inverted this.
start() ->
    true = ?MODULE:id([1, 2 | 3]) < ?MODULE:id([1, 2]),
    true = ?MODULE:id([1, 2]) > ?MODULE:id([1, 2 | 3]),
    true = ?MODULE:id([1 | 2]) < ?MODULE:id([1, 2]),
    true = ?MODULE:id([1, 2 | a]) < ?MODULE:id([1, 2]),
    true = ?MODULE:id([1, 2 | 3]) < ?MODULE:id([1, 2 | 4]),
    true = ?MODULE:id([1, 2 | 3]) < ?MODULE:id([1, 2 | a]),
    true = ?MODULE:id([1, 2 | {3}]) > ?MODULE:id([1, 2 | a]),
    %% proper-list prefix ordering still holds
    true = ?MODULE:id("a") < ?MODULE:id("ab"),
    true = ?MODULE:id("ab") > ?MODULE:id("a"),
    true = ?MODULE:id([]) < ?MODULE:id([1]),
    %% nested improper lists
    true = ?MODULE:id([[1 | 2]]) < ?MODULE:id([[1, 2]]),
    %% heads decide before tails
    true = ?MODULE:id([0 | improper]) < ?MODULE:id([1]),
    %% equality is unaffected
    true = ?MODULE:id([1, 2 | 3]) =:= ?MODULE:id([1, 2 | 3]),
    false = ?MODULE:id([1, 2 | 3]) =:= ?MODULE:id([1, 2]),
    0.

id(X) ->
    X.
