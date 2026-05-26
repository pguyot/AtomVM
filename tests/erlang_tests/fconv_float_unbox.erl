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

%% Regression test for the inline fconv float fast path: a float-typed source
%% (a boxed float) must be unboxed correctly when converted to a float
%% register. Exercises positive, negative and fractional boxed floats.
-module(fconv_float_unbox).

-export([start/0, fadd/2]).

start() ->
    %% Each fadd/2 forces both arguments (provably floats) onto float registers
    %% via fconv, then a float add.
    4 = trunc(fadd(1.5, 2.5)),
    -1 = trunc(fadd(-3.5, 2.5)),
    0 = trunc(fadd(2.5, -2.5)),
    7 = trunc(fadd(fadd(1.0, 2.0), 4.0)),
    %% Result is 0 when sign and magnitude survive unboxing.
    trunc(fadd(fadd(-3.5, 2.5), 1.0)).

fadd(X, Y) when is_float(X), is_float(Y) ->
    X + Y.
