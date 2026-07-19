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

% Packaged as PLAIN BEAM (compile_erlang_no_jit): JIT-compiled at runtime, on
% the device, by code_server via jit_stream_flash. Deliberately guard-heavy:
% every clause of classify/1 is a chain of forward guard fail-jumps, and
% triangle/2 is a backward loop with guards, so the module returns ok only if
% the runtime-compiled branches (forward and backward) are all correct.
% Expected constants are pre-computed on the host:
%   lists:sum([classify(N) || N <- lists:seq(1, 500)]) =:= 1389
%   triangle(1000, 0) =:= 500500
-module(test_jit_runtime_guarded).

-export([run/0]).

run() ->
    1389 = lists:foldl(fun(N, A) -> A + classify(N) end, 0, lists:seq(1, 500)),
    500500 = triangle(1000, 0),
    ok.

classify(N) when N rem 15 =:= 0 -> 15;
classify(N) when N rem 5 =:= 0 -> 5;
classify(N) when N rem 3 =:= 0 -> 3;
classify(N) when N < 100 -> 2;
classify(N) when N < 200 -> 1;
classify(_) -> 0.

triangle(0, Acc) -> Acc;
triangle(N, Acc) when is_integer(N), N > 0 -> triangle(N - 1, Acc + N).
