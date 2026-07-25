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

-module(test_atom_sort_key_tie).

-export([start/0, id/1]).

%% Regression test for the JIT aarch64 compare-stub atom-vs-atom fast path,
%% which decides ordering from the atom table's cached 8-byte sort_key
%% without leaving generated code. test_atom_ordering.erl already covers
%% ordinary short-atom sorting; this file targets the cases that specifically
%% exercise the sort_key tie-break, which the pre-existing test's atoms
%% (all distinct well within the first 8 bytes) never reach.
start() ->
    %% One atom is a proper prefix of the other: the shorter one's
    %% zero-padded sort_key is numerically smaller, matching memcmp
    true = ?MODULE:id(ab) < ?MODULE:id(abc),
    true = ?MODULE:id(abc) > ?MODULE:id(ab),
    %% Exactly 8 bytes, tying the whole cached sort_key window
    true = ?MODULE:id(abcdefgh) < ?MODULE:id(abcdefgi),
    true = ?MODULE:id(abcdefgh) =:= ?MODULE:id(abcdefgh),
    %% More than 8 bytes shared: sort_key ties, forcing the fallback to the
    %% C comparator's full-name memcmp
    true = ?MODULE:id(abcdefghij) < ?MODULE:id(abcdefghkl),
    true = ?MODULE:id(abcdefghkl) > ?MODULE:id(abcdefghij),
    true = ?MODULE:id(abcdefghij) =:= ?MODULE:id(abcdefghij),
    false = ?MODULE:id(abcdefghij) =:= ?MODULE:id(abcdefghkl),
    %% Shared prefix longer than 8 bytes, one a proper prefix of the other
    true = ?MODULE:id(abcdefghij) < ?MODULE:id(abcdefghijk),
    %% Long shared prefix (16+ bytes), differing only near the very end
    true = ?MODULE:id(abcdefghijklmnop) < ?MODULE:id(abcdefghijklmnoq),
    %% Same tie cases nested in a tuple (tuple fast path's atom leaf) and a
    %% list (list-scan fast path's atom leaf), not just the bare scalar path
    true = ?MODULE:id({abcdefghij, 1}) < ?MODULE:id({abcdefghkl, 1}),
    true = ?MODULE:id([abcdefghij]) < ?MODULE:id([abcdefghkl]),
    0.

id(X) ->
    X.
