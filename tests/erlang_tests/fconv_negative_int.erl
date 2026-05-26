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

%% Regression test for the inline fconv (float conversion) fast path: a
%% negative small integer must be untagged with an arithmetic (sign-preserving)
%% shift before being converted to a double. A logical shift would turn a
%% negative integer into a large positive one.
-module(fconv_negative_int).

-export([start/0, conv/1]).

start() ->
    %% conv/1 forces an integer -> float conversion (fconv) of its argument.
    -7 = trunc(conv(-7)),
    -1 = trunc(conv(-1)),
    7 = trunc(conv(7)),
    %% Sum a few signed conversions; result must be 0 if signs are preserved.
    trunc(conv(-7) + conv(-1) + conv(8)).

%% The "/ 1.0" forces N onto a float register via fconv; id/1 hides the value
%% from the compiler so it is not folded at compile time.
conv(N) ->
    id(N) / 1.0.

id(X) -> X.
