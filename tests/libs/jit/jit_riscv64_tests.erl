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

-module(jit_riscv64_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_riscv64).

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	00063f83          	ld	t6,0(a2)\n"
            "   4:	1101                	addi	sp,sp,-32\n"
            "   6:	e006                	sd	ra,0(sp)\n"
            "   8:	e42a                	sd	a0,8(sp)\n"
            "   a:	e82e                	sd	a1,16(sp)\n"
            "   c:	ec32                	sd	a2,24(sp)\n"
            "   e:	9f82                	jalr	t6\n"
            "  10:	8faa                	mv	t6,a0\n"
            "  12:	6082                	ld	ra,0(sp)\n"
            "  14:	6522                	ld	a0,8(sp)\n"
            "  16:	65c2                	ld	a1,16(sp)\n"
            "  18:	6662                	ld	a2,24(sp)\n"
            "  1a:	02010113          	addi	sp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	4fa1                	li	t6,8\n"
            "   2:	9fb2                	add	t6,t6,a2\n"
            "   4:	000fbf83          	ld	t6,0(t6)\n"
            "   8:	1101                	addi	sp,sp,-32\n"
            "   a:	e006                	sd	ra,0(sp)\n"
            "   c:	e42a                	sd	a0,8(sp)\n"
            "   e:	e82e                	sd	a1,16(sp)\n"
            "  10:	ec32                	sd	a2,24(sp)\n"
            "  12:	9f82                	jalr	t6\n"
            "  14:	8faa                	mv	t6,a0\n"
            "  16:	6082                	ld	ra,0(sp)\n"
            "  18:	6522                	ld	a0,8(sp)\n"
            "  1a:	65c2                	ld	a1,16(sp)\n"
            "  1c:	6662                	ld	a2,24(sp)\n"
            "  1e:	02010113          	addi	sp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

word_size_test() ->
    ?assertEqual(8, ?BACKEND:word_size()).
