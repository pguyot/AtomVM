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
%% Probe functions for the codegen audit: each exercises one pattern whose
%% emitted assembly is worth comparing against BeamAsm's. Compile with erlc,
%% then dump both sides (tools/dev/jit_dwdump.sh and `erl +JDdump true`).
-module(codegen_probe).
-export([tup4/1, rec/1, movey/2, lst/2, arith/1, cmp/2]).

%% four consecutive tuple elements -- BEAM: load_tuple_ptr + get_two_tuple_elements x2
tup4({A, B, C, D}) -> {D, C, B, A}.

%% tagged tuple (record) then several fields
rec(R) when element(1, R) =:= rec, tuple_size(R) =:= 5 ->
    element(2, R) + element(3, R) + element(4, R) + element(5, R).

%% consecutive y-register traffic across a call
movey(A, B) ->
    X = ext(A),
    Y = ext(B),
    Z = ext(A),
    {X, Y, Z, A, B}.

lst([H | T], Acc) -> lst(T, [H | Acc]);
lst([], Acc) -> Acc.

arith(N) -> N * 3 + 7 - 1.

cmp(A, B) when A < B -> lt;
cmp(A, B) when A > B -> gt;
cmp(_, _) -> eq.

ext(X) -> X.
