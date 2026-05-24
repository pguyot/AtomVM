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

-module(get_map_elements_bignum_key).

-export([start/0, fw/1, fv/1]).

% Regression test for a JIT codegen bug: get_map_elements codegen calls
% skip_compact_term/1 to step over operands. Its COMPACT_LARGE_INTEGER_NBITS
% clause (a bignum operand >= 9 bytes, i.e. >= 2^64) reused the head's bound
% `Rest` variable as the tail of an inner binary match, raising {badmatch, ...}
% and failing JIT compilation of the entire module.
%
% The trigger is a map pattern whose key is itself a map-update expression
% (#{...} #{ K := V }) carrying an inline bignum -- that places the bignum as an
% inline NBITS operand that skip_compact_term must walk past. (A plain bignum
% map key goes through the literal table and does not exercise the path.) Both
% the wildcard (fw) and value-binding (fv) clause shapes are kept: the bug is
% sensitive to the emitted operand layout and both forms reproduced it.

fw(#{(#{}#{ok := 18446744073709551616}) := _}) -> wild.

fv(#{(#{ok => 0}#{ok := 18446744073709551616}) := V}) -> {bound, V};
fv(_) -> nomatch.

start() ->
    % Exercising fv/fw at runtime confirms the get_map_elements codegen is not
    % merely skipped. Inputs intentionally do not share literal structure with
    % the clause heads, so the crashing head bytecode is preserved.
    nomatch = fv(id(#{a => 1})),
    nomatch = fv(id(#{})),
    _ = (catch fw(id(#{b => 2}))),
    0.

id(X) -> X.
