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

%% Value-context '=:=' / '=/=' where the left operand is typed t_atom by the
%% is_atom/1 guard: the JIT compiles these bif2 calls to an inline word
%% compare. The right operands cover an equal atom, a different atom, and
%% boxed/immediate non-atoms, so the inline compare is checked against every
%% word-inequality class (same immediate, other immediate, tagged pointer).
-module(test_jit_typed_eq).

-export([start/0, eq/2, ne/2]).

start() ->
    true = eq(foo, foo),
    false = eq(foo, bar),
    false = eq(foo, id([1, 2])),
    false = eq(foo, id(<<"foo">>)),
    false = eq(foo, id(42)),
    false = eq(foo, id(3.14)),
    false = eq(foo, id({foo})),
    true = eq(id_atom(quux), quux),
    false = ne(ok, ok),
    true = ne(ok, error),
    true = ne(ok, id([])),
    true = ne(ok, id(self())),
    0.

eq(X, Y) when is_atom(X) -> X =:= Y.

ne(X, Y) when is_atom(X) -> X =/= Y.

id_atom(X) when is_atom(X) -> X.

id(X) -> X.
