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

%% Test byte_size/1 on values that may be typed as t_bs_matchable by
%% the compiler (i.e. values that could be either binaries or match
%% states at runtime). This exercises the JIT inline byte_size
%% optimization to ensure it correctly handles match states.

-module(test_byte_size_match_state).

-export([start/0, byte_size_during_match/1, byte_size_of_alias/1, byte_size_after_submatch/1]).

start() ->
    10 = byte_size_during_match(<<"hello world">>),
    11 = byte_size_of_alias(<<"hello world">>),
    3 = byte_size_after_submatch(<<1, 2, 3, 4, 5>>),
    % Return expected value: 10 + 11 + 3 = 24
    byte_size_during_match(<<"hello world">>) +
        byte_size_of_alias(<<"hello world">>) +
        byte_size_after_submatch(<<1, 2, 3, 4, 5>>).

%% Call byte_size on the matched rest binary.
%% The compiler may keep Rest as a match state internally.
byte_size_during_match(<<_:8, Rest/binary>>) ->
    byte_size(Rest).

%% Call byte_size on the original binary that is aliased
%% with a pattern match. The compiler may internally replace
%% Bin with the match state from the binary match.
byte_size_of_alias(<<_H:8, _/binary>> = Bin) ->
    byte_size(Bin).

%% Call byte_size on a sub-binary from a nested match context.
byte_size_after_submatch(<<_:8, _:8, Rest/binary>>) ->
    byte_size(Rest).
