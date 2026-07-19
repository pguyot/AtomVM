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
-include("jit_tests_common.hrl").

-define(BACKEND, jit_riscv64).

% disassembly obtained with:
% riscv64-linux-gnu-objdump -b binary -m riscv:rv64 -D dump.bin

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	0009bf83          	ld	t6,0(s3)\n"
            "   4:	1141                	addi	sp,sp,-16\n"
            "   6:	e006                	sd	ra,0(sp)\n"
            "   8:	0544b823          	sd	s4,80(s1)\n"
            "   c:	9f82                	jalr	t6\n"
            "   e:	8faa                	mv	t6,a0\n"
            "  10:	6082                	ld	ra,0(sp)\n"
            "  12:	0141                	addi	sp,sp,16\n"
            "  14:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	4fa1                	li	t6,8\n"
            "   2:	9fce                	add	t6,t6,s3\n"
            "   4:	000fbf83          	ld	t6,0(t6)\n"
            "   8:	1141                	addi	sp,sp,-16\n"
            "   a:	e006                	sd	ra,0(sp)\n"
            "   c:	0544b823          	sd	s4,80(s1)\n"
            "  10:	9f82                	jalr	t6\n"
            "  12:	8faa                	mv	t6,a0\n"
            "  14:	6082                	ld	ra,0(sp)\n"
            "  16:	0141                	addi	sp,sp,16\n"
            "  18:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	4fc1                	li	t6,16\n"
            "   2:	9fce                	add	t6,t6,s3\n"
            "   4:	000fbf83          	ld	t6,0(t6)\n"
            "   8:	1141                	addi	sp,sp,-16\n"
            "   a:	e006                	sd	ra,0(sp)\n"
            "   c:	02a00513          	li	a0,42\n"
            "  10:	02b00593          	li	a1,43\n"
            "  14:	02c00613          	li	a2,44\n"
            "  18:	0544b823          	sd	s4,80(s1)\n"
            "  1c:	9f82                	jalr	t6\n"
            "  1e:	8faa                	mv	t6,a0\n"
            "  20:	6082                	ld	ra,0(sp)\n"
            "  22:	0141                	addi	sp,sp,16\n"
            "  24:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	02800f93          	li	t6,40\n"
            "   4:	9fce                	add	t6,t6,s3\n"
            "   6:	000fbf83          	ld	t6,0(t6)\n"
            "   a:	4541                	li	a0,16\n"
            "   c:	02000593          	li	a1,32\n"
            "  10:	4609                	li	a2,2\n"
            "  12:	0544b823          	sd	s4,80(s1)\n"
            "  16:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_6_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get bin_ptr from x_reg 0 (similar to get_list_test pattern)
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:and_(State1, {free, RegA}, ?TERM_PRIMARY_CLEAR_MASK),
    % Get another register for the last parameter to test {free, Reg} handling
    {State3, OtherReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 1}),
    % Call PRIM_BITSTRING_EXTRACT_INTEGER with 6 arguments
    {State4, _ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, RegA}, 64, 8, {free, OtherReg}
    ]),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	ffcfff93          	andi	t6,t6,-4\n"
            "   8:	0604bf03          	ld	t5,96(s1)\n"
            "   c:	02e00e93          	li	t4,46\n"
            "  10:	0e8e                	slli	t4,t4,0x3\n"
            "  12:	9ece                	add	t4,t4,s3\n"
            "  14:	000ebe83          	ld	t4,0(t4)\n"
            "  18:	1141                	addi	sp,sp,-16\n"
            "  1a:	e006                	sd	ra,0(sp)\n"
            "  1c:	857e                	mv	a0,t6\n"
            "  1e:	04000593          	li	a1,64\n"
            "  22:	4621                	li	a2,8\n"
            "  24:	86fa                	mv	a3,t5\n"
            "  26:	0544b823          	sd	s4,80(s1)\n"
            "  2a:	9e82                	jalr	t4\n"
            "  2c:	8eaa                	mv	t4,a0\n"
            "  2e:	6082                	ld	ra,0(sp)\n"
            "  30:	0141                	addi	sp,sp,16\n"
            "  32:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_extended_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:call_primitive(State0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]),
    {State2, RegB} = ?BACKEND:call_primitive(State1, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 20]),
    {State3, RegC} = ?BACKEND:call_primitive(State2, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]),
    {State4, ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_PUT_LIST, [
        ctx, {free, {ptr, RegA}}, {free, {ptr, RegB}}
    ]),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {ptr, RegC}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg, {ptr, RegC}]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	09000f93          	li	t6,144\n"
        "   4:	9fce                	add	t6,t6,s3\n"
        "   6:	000fbf83          	ld	t6,0(t6)\n"
        "   a:	1141                	addi	sp,sp,-16\n"
        "   c:	e006                	sd	ra,0(sp)\n"
        "   e:	454d                	li	a0,19\n"
        "  10:	9f82                	jalr	t6\n"
        "  12:	8faa                	mv	t6,a0\n"
        "  14:	6082                	ld	ra,0(sp)\n"
        "  16:	0141                	addi	sp,sp,16\n"
        "  18:	09000f13          	li	t5,144\n"
        "  1c:	9f4e                	add	t5,t5,s3\n"
        "  1e:	000f3f03          	ld	t5,0(t5)\n"
        "  22:	1141                	addi	sp,sp,-16\n"
        "  24:	e006                	sd	ra,0(sp)\n"
        "  26:	e47e                	sd	t6,8(sp)\n"
        "  28:	4551                	li	a0,20\n"
        "  2a:	9f02                	jalr	t5\n"
        "  2c:	8f2a                	mv	t5,a0\n"
        "  2e:	6082                	ld	ra,0(sp)\n"
        "  30:	6fa2                	ld	t6,8(sp)\n"
        "  32:	0141                	addi	sp,sp,16\n"
        "  34:	09000e93          	li	t4,144\n"
        "  38:	9ece                	add	t4,t4,s3\n"
        "  3a:	000ebe83          	ld	t4,0(t4)\n"
        "  3e:	1101                	addi	sp,sp,-32\n"
        "  40:	e006                	sd	ra,0(sp)\n"
        "  42:	e47e                	sd	t6,8(sp)\n"
        "  44:	e87a                	sd	t5,16(sp)\n"
        "  46:	454d                	li	a0,19\n"
        "  48:	9e82                	jalr	t4\n"
        "  4a:	8eaa                	mv	t4,a0\n"
        "  4c:	6082                	ld	ra,0(sp)\n"
        "  4e:	6fa2                	ld	t6,8(sp)\n"
        "  50:	6f42                	ld	t5,16(sp)\n"
        "  52:	02010113          	addi	sp,sp,32\n"
        "  56:	06800e13          	li	t3,104\n"
        "  5a:	9e4e                	add	t3,t3,s3\n"
        "  5c:	000e3e03          	ld	t3,0(t3)\n"
        "  60:	1141                	addi	sp,sp,-16\n"
        "  62:	e006                	sd	ra,0(sp)\n"
        "  64:	e476                	sd	t4,8(sp)\n"
        "  66:	000fb503          	ld	a0,0(t6)\n"
        "  6a:	000f3583          	ld	a1,0(t5)\n"
        "  6e:	0544b823          	sd	s4,80(s1)\n"
        "  72:	9e02                	jalr	t3\n"
        "  74:	8e2a                	mv	t3,a0\n"
        "  76:	6082                	ld	ra,0(sp)\n"
        "  78:	6ea2                	ld	t4,8(sp)\n"
        "  7a:	0141                	addi	sp,sp,16\n"
        "  7c:	0504ba03          	ld	s4,80(s1)\n"
        "  80:	01ceb023          	sd	t3,0(t4)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_few_free_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, 2),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, 3),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, 4),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, 5),
    {State6, ResultReg} = ?BACKEND:call_primitive(State5, ?PRIM_BITSTRING_INSERT_INTEGER, [
        t5, t6, {free, t3}, t4, {free, t2}
    ]),
    State7 = ?BACKEND:free_native_registers(State6, [ResultReg, t5, t6, t4]),
    ?BACKEND:assert_all_native_free(State7),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        "   0:	4f85                	li	t6,1\n"
        "   2:	4f09                	li	t5,2\n"
        "   4:	4e8d                	li	t4,3\n"
        "   6:	4e11                	li	t3,4\n"
        "   8:	4395                	li	t2,5\n"
        "   a:	03900313          	li	t1,57\n"
        "   e:	030e                	slli	t1,t1,0x3\n"
        "  10:	934e                	add	t1,t1,s3\n"
        "  12:	00033303          	ld	t1,0(t1)\n"
        "  16:	1101                	addi	sp,sp,-32\n"
        "  18:	e006                	sd	ra,0(sp)\n"
        "  1a:	e47e                	sd	t6,8(sp)\n"
        "  1c:	e87a                	sd	t5,16(sp)\n"
        "  1e:	ec76                	sd	t4,24(sp)\n"
        "  20:	857a                	mv	a0,t5\n"
        "  22:	85fe                	mv	a1,t6\n"
        "  24:	8672                	mv	a2,t3\n"
        "  26:	86f6                	mv	a3,t4\n"
        "  28:	871e                	mv	a4,t2\n"
        "  2a:	0544b823          	sd	s4,80(s1)\n"
        "  2e:	9302                	jalr	t1\n"
        "  30:	832a                	mv	t1,a0\n"
        "  32:	6082                	ld	ra,0(sp)\n"
        "  34:	6fa2                	ld	t6,8(sp)\n"
        "  36:	6f42                	ld	t5,16(sp)\n"
        "  38:	6ee2                	ld	t4,24(sp)\n"
        "  3a:	02010113          	addi	sp,sp,32\n"
        "  3e:	0504ba03          	ld	s4,80(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	01092f83          	lw	t6,16(s2)\n"
        "   4:	1ffd                	addi	t6,t6,-1\n"
        "   6:	01f92823          	sw	t6,16(s2)\n"
        "   a:	000f9f63          	bnez	t6,0x28\n"
        "   e:	00000f97          	auipc	t6,0x0\n"
        "  12:	0fe9                	addi	t6,t6,26 # 0x28\n"
        "  14:	0001                	nop\n"
        "  16:	01f93423          	sd	t6,8(s2)\n"
        "  1a:	4fc1                	li	t6,16\n"
        "  1c:	9fce                	add	t6,t6,s3\n"
        "  1e:	000fbf83          	ld	t6,0(t6)\n"
        "  22:	0544b823          	sd	s4,80(s1)\n"
        "  26:	8f82                	jr	t6\n"
        "  28:	02000f93          	li	t6,32\n"
        "  2c:	9fce                	add	t6,t6,s3\n"
        "  2e:	000fbf83          	ld	t6,0(t6)\n"
        "  32:	03200513          	li	a0,50\n"
        "  36:	4589                	li	a1,2\n"
        "  38:	4609                	li	a2,2\n"
        "  3a:	56fd                	li	a3,-1\n"
        "  3c:	0544b823          	sd	s4,80(s1)\n"
        "  40:	8f82                	jr	t6"
    >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_last_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?CASE_CLAUSE_ATOM, {free, RegA}
    ]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0584bf83          	ld	t6,88(s1)\n"
        "   4:	09800f13          	li	t5,152\n"
        "   8:	9f4e                	add	t5,t5,s3\n"
        "   a:	000f3f03          	ld	t5,0(t5)\n"
        "   e:	4539                	li	a0,14\n"
        "  10:	2cb00593          	li	a1,715\n"
        "  14:	867e                	mv	a2,t6\n"
        "  16:	0544b823          	sd	s4,80(s1)\n"
        "  1a:	8f02                	jr	t5"
    >>,
    ?assertStream(riscv64, Dump, Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	01092f83          	lw	t6,16(s2)\n"
        "   4:	1ffd                	addi	t6,t6,-1\n"
        "   6:	01f92823          	sw	t6,16(s2)\n"
        "   a:	000f9f63          	bnez	t6,0x28\n"
        "   e:	00000f97          	auipc	t6,0x0\n"
        "  12:	0fe9                	addi	t6,t6,26 # 0x28\n"
        "  14:	0001                	nop\n"
        "  16:	01f93423          	sd	t6,8(s2)\n"
        "  1a:	4fc1                	li	t6,16\n"
        "  1c:	9fce                	add	t6,t6,s3\n"
        "  1e:	000fbf83          	ld	t6,0(t6)\n"
        "  22:	0544b823          	sd	s4,80(s1)\n"
        "  26:	8f82                	jr	t6\n"
        "  28:	02000f93          	li	t6,32\n"
        "  2c:	9fce                	add	t6,t6,s3\n"
        "  2e:	000fbf83          	ld	t6,0(t6)\n"
        "  32:	03200513          	li	a0,50\n"
        "  36:	4589                	li	a1,2\n"
        "  38:	4609                	li	a2,2\n"
        "  3a:	46a9                	li	a3,10\n"
        "  3c:	0544b823          	sd	s4,80(s1)\n"
        "  40:	8f82                	jr	t6"
    >>,
    ?assertStream(riscv64, Dump, Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	0009bf83          	ld	t6,0(s3)\n"
            "   4:	02a00513          	li	a0,42\n"
            "   8:	0544b823          	sd	s4,80(s1)\n"
            "   c:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

return_if_not_equal_to_ctx_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(t6, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	0a800f93          	li	t6,168\n"
                            "   4:	9fce                	add	t6,t6,s3\n"
                            "   6:	000fbf83          	ld	t6,0(t6)\n"
                            "   a:	1141                	addi	sp,sp,-16\n"
                            "   c:	e006                	sd	ra,0(sp)\n"
                            "   e:	0544b823          	sd	s4,80(s1)\n"
                            "  12:	9f82                	jalr	t6\n"
                            "  14:	8faa                	mv	t6,a0\n"
                            "  16:	6082                	ld	ra,0(sp)\n"
                            "  18:	0141                	addi	sp,sp,16\n"
                            "  1a:	0504ba03          	ld	s4,80(s1)\n"
                            "  1e:	009f8463          	beq	t6,s1,0x26\n"
                            "  22:	857e                	mv	a0,t6\n"
                            "  24:	8082                	ret"
                        >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(t6, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(t5, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	0a800f93          	li	t6,168\n"
                            "   4:	9fce                	add	t6,t6,s3\n"
                            "   6:	000fbf83          	ld	t6,0(t6)\n"
                            "   a:	1141                	addi	sp,sp,-16\n"
                            "   c:	e006                	sd	ra,0(sp)\n"
                            "   e:	0544b823          	sd	s4,80(s1)\n"
                            "  12:	9f82                	jalr	t6\n"
                            "  14:	8faa                	mv	t6,a0\n"
                            "  16:	6082                	ld	ra,0(sp)\n"
                            "  18:	0141                	addi	sp,sp,16\n"
                            "  1a:	0504ba03          	ld	s4,80(s1)\n"
                            "  1e:	8f7e                	mv	t5,t6\n"
                            "  20:	009f0463          	beq	t5,s1,0x28\n"
                            "  24:	857a                	mv	a0,t5\n"
                            "  26:	8082                	ret"
                        >>,
                    ?assertStream(riscv64, Dump, Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	000a3f83          	ld	t6,0(s4)\n"
            "   4:	0ff4b023          	sd	t6,224(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	038a0a13          	addi	s4,s4,56"
        >>,
    ?assertStream(riscv64, Dump, Stream).

if_block_test_() ->
    {setup,
        fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State2, RegA, RegB}
        end,
        fun({State0, RegA, RegB}) ->
            [
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000fd363          	bgez	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	01efd363          	bge	t6,t5,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01dfd363          	bge	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 1024},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	40000e93          	li	t4,1024\n"
                        "   c:	01dfd363          	bge	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2\n"
                        "  12:	a0fd                	j	0x100"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000f9363          	bnez	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000f9363          	bnez	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', -1},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	5efd                	li	t4,-1\n"
                        "   a:	01df9363          	bne	t6,t4,0x10\n"
                        "   e:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000f9363          	bnez	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000f9363          	bnez	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000f8363          	beqz	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	000f8363          	beqz	t6,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03b00e93          	li	t4,59\n"
                        "   c:	01df8363          	beq	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '!=', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03b00e93          	li	t4,59\n"
                        "   c:	01df8363          	beq	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '!=', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df8363          	beq	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    % Test large immediate (1995) that requires temporary register
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', 1995},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 1)
                        end
                    ),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	7cb00e93          	li	t4,1995\n"
                        "   c:	01df8363          	beq	t6,t4,0x12\n"
                        "  10:	0f05                	addi	t5,t5,1\n"
                        "  12:	a0fd                	j	0x100"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '!=', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df8363          	beq	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03b00e93          	li	t4,59\n"
                        "   c:	01df9363          	bne	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03b00e93          	li	t4,59\n"
                        "   c:	01df9363          	bne	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df9363          	bne	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '==', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	02a00e93          	li	t4,42\n"
                        "   c:	01df9363          	bne	t6,t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '==', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03ff9e93          	slli	t4,t6,0x3f\n"
                        "   c:	000ec363          	bltz	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '==', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03ff9e93          	slli	t4,t6,0x3f\n"
                        "   c:	000ec363          	bltz	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '!=', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03ff9e93          	slli	t4,t6,0x3f\n"
                        "   c:	000ed363          	bgez	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '!=', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03ff9e93          	slli	t4,t6,0x3f\n"
                        "   c:	000ed363          	bgez	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#7, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	007ffe93          	andi	t4,t6,7\n"
                        "   c:	000e8363          	beqz	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#5, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	005ffe93          	andi	t4,t6,5\n"
                        "   c:	000e8363          	beqz	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '&', 16#7, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	007ffe93          	andi	t4,t6,7\n"
                        "   c:	000e8363          	beqz	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	ffffce93          	not	t4,t6\n"
                        "   c:	1ef2                	slli	t4,t4,0x3c\n"
                        "   e:	000e8363          	beqz	t4,0x14\n"
                        "  12:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	ffffcf93          	not	t6,t6\n"
                        "   c:	1ff2                	slli	t6,t6,0x3c\n"
                        "   e:	000f8363          	beqz	t6,0x14\n"
                        "  12:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	8efe                	mv	t4,t6\n"
                        "   a:	03fefe93          	andi	t4,t4,63\n"
                        "   e:	4e21                	li	t3,8\n"
                        "  10:	01ce8363          	beq	t4,t3,0x16\n"
                        "  14:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '<', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	01efd363          	bge	t6,t5,0xe\n"
                        "   c:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {
                            {free, RegA},
                            '&',
                            ?TERM_BOXED_TAG_MASK,
                            '!=',
                            ?TERM_BOXED_POSITIVE_INTEGER
                        },
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	03ffff93          	andi	t6,t6,63\n"
                        "   c:	4ea1                	li	t4,8\n"
                        "   e:	01df8363          	beq	t6,t4,0x14\n"
                        "  12:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                %% Test {RegA, '&', 16#3, '!=', 0} using ANDI instruction
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#3, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	003ffe93          	andi	t4,t6,3\n"
                        "   c:	000e8363          	beqz	t4,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {100, '<', RegA},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	06400e93          	li	t4,100\n"
                        "   c:	01fed363          	bge	t4,t6,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {100, '<', {free, RegA}},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	06400e93          	li	t4,100\n"
                        "   c:	01fed363          	bge	t4,t6,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {1024, '<', RegA},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	40000e93          	li	t4,1024\n"
                        "   c:	01fed363          	bge	t4,t6,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {1024, '<', {free, RegA}},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	0604bf03          	ld	t5,96(s1)\n"
                        "   8:	40000e93          	li	t4,1024\n"
                        "   c:	01fed363          	bge	t4,t6,0x12\n"
                        "  10:	0f09                	addi	t5,t5,2"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end)
            ]
        end}.

if_else_block_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_else_block(
        State2,
        {Reg1, '==', ?TERM_NIL},
        fun(BSt0) ->
            ?BACKEND:add(BSt0, Reg2, 2)
        end,
        fun(BSt0) ->
            ?BACKEND:add(BSt0, Reg2, 4)
        end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0604bf03          	ld	t5,96(s1)\n"
            "   8:	03b00e93          	li	t4,59\n"
            "   c:	01df9463          	bne	t6,t4,0x14\n"
            "  10:	0f09                	addi	t5,t5,2\n"
            "  12:	a011                	j	0x16\n"
            "  14:	0f11                	addi	t5,t5,4"
        >>,
    ?assertStream(riscv64, Dump, Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 3),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	0584bf83          	ld	t6,88(s1)\n"
                    "   4:	003fdf93          	srli	t6,t6,0x3"
                >>,
            ?assertStream(riscv64, Dump, Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 3),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	0584bf83          	ld	t6,88(s1)\n"
                    "   4:	003fdf13          	srli	t5,t6,0x3"
                >>,
            ?assertStream(riscv64, Dump, Stream)
        end)
    ].

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0f8e                	slli	t6,t6,0x3"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	00000697          	auipc	a3,0x0\n"
            "   4:	05268067          	jr	82(a3) # 0x52\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	01068067          	jr	16(a3) # 0x18\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	03868067          	jr	56(a3) # 0x48\n"
            "  18:	01092f83          	lw	t6,16(s2)\n"
            "  1c:	1ffd                	addi	t6,t6,-1\n"
            "  1e:	01f92823          	sw	t6,16(s2)\n"
            "  22:	000f8663          	beqz	t6,0x2e\n"
            "  26:	a00d                	j	0x48\n"
            "  28:	0001                	nop\n"
            "  2a:	00000013          	nop\n"
            "  2e:	00000f97          	auipc	t6,0x0\n"
            "  32:	0fe9                	addi	t6,t6,26 # 0x48\n"
            "  34:	0001                	nop\n"
            "  36:	01f93423          	sd	t6,8(s2)\n"
            "  3a:	4fc1                	li	t6,16\n"
            "  3c:	9fce                	add	t6,t6,s3\n"
            "  3e:	000fbf83          	ld	t6,0(t6)\n"
            "  42:	0544b823          	sd	s4,80(s1)\n"
            "  46:	8f82                	jr	t6\n"
            "  48:	0009bf83          	ld	t6,0(s3)\n"
            "  4c:	0544b823          	sd	s4,80(s1)\n"
            "  50:	8f82                	jr	t6\n"
            "  52:	4fa1                	li	t6,8\n"
            "  54:	9fce                	add	t6,t6,s3\n"
            "  56:	000fbf83          	ld	t6,0(t6)\n"
            "  5a:	0544b823          	sd	s4,80(s1)\n"
            "  5e:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_only_or_schedule_next_known_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#36),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	00000697          	auipc	a3,0x0\n"
            "   4:	04868067          	jr	72(a3) # 0x48\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	01068067          	jr	16(a3) # 0x18\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	02668067          	jr	38(a3) # 0x36\n"
            "  18:	01092f83          	lw	t6,16(s2)\n"
            "  1c:	1ffd                	addi	t6,t6,-1\n"
            "  1e:	01f92823          	sw	t6,16(s2)\n"
            "  22:	000f9a63          	bnez	t6,0x36\n"
            "  26:	00000f97          	auipc	t6,0x0\n"
            "  2a:	0fc1                	addi	t6,t6,16 # 0x36\n"
            "  2c:	01f93423          	sd	t6,8(s2)\n"
            "  30:	4fc1                	li	t6,16\n"
            "  32:	9fce                	add	t6,t6,s3\n"
            "  34:	000fbf83          	ld	t6,0(t6)\n"
            "  38:	0544b823          	sd	s4,80(s1)\n"
            "  3c:	8f82                	jr	t6\n"
            "  3e:	0009bf83          	ld	t6,0(s3)\n"
            "  42:	0544b823          	sd	s4,80(s1)\n"
            "  46:	8f82                	jr	t6\n"
            "  48:	4fa1                	li	t6,8\n"
            "  4a:	9fce                	add	t6,t6,s3\n"
            "  4c:	000fbf83          	ld	t6,0(t6)\n"
            "  50:	0544b823          	sd	s4,80(s1)\n"
            "  54:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test with large gap (256+ bytes) to force mov_immediate path
call_only_or_schedule_next_and_label_relocation_large_gap_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    % Add large padding by emitting many move_to_native_register operations
    % This creates a large gap between the jump table and the actual function bodies
    % Each operation emits ~2 bytes, so 128 operations = ~256 bytes
    StatePadded = lists:foldl(
        fun(_, S) ->
            ?BACKEND:move_to_native_register(S, {x_reg, 2}, a3)
        end,
        State1,
        lists:seq(1, 128)
    ),
    State2 = ?BACKEND:add_label(StatePadded, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    % Extract the final section starting at 0x118 (after jump table 24 bytes + 128 loads 256 bytes)
    % RISC-V: Jump table is 3×8=24 bytes, loads are 2 bytes each (compressed)
    Dump = <<
        "   0:	01092f83          	lw	t6,16(s2)\n"
        "   4:	1ffd                	addi	t6,t6,-1\n"
        "   6:	01f92823          	sw	t6,16(s2)\n"
        "   a:	000f8663          	beqz	t6,0x16\n"
        "   e:	a00d                	j	0x30\n"
        "  10:	0001                	nop\n"
        "  12:	00000013          	nop\n"
        "  16:	00000f97          	auipc	t6,0x0\n"
        "  1a:	0fe9                	addi	t6,t6,26 # 0x30\n"
        "  1c:	0001                	nop\n"
        "  1e:	01f93423          	sd	t6,8(s2)\n"
        "  22:	4fc1                	li	t6,16\n"
        "  24:	9fce                	add	t6,t6,s3\n"
        "  26:	000fbf83          	ld	t6,0(t6)\n"
        "  2a:	0544b823          	sd	s4,80(s1)\n"
        "  2e:	8f82                	jr	t6\n"
        "  30:	0009bf83          	ld	t6,0(s3)\n"
        "  34:	0544b823          	sd	s4,80(s1)\n"
        "  38:	8f82                	jr	t6\n"
        "  3a:	4fa1                	li	t6,8\n"
        "  3c:	9fce                	add	t6,t6,s3\n"
        "  3e:	000fbf83          	ld	t6,0(t6)\n"
        "  42:	0544b823          	sd	s4,80(s1)\n"
        "  46:	8f82                	jr	t6"
    >>,
    {_, RelevantBinary} = split_binary(Stream, 16#118),
    ?assertStream(riscv64, Dump, RelevantBinary).

call_bif_with_large_literal_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = ?BACKEND:call_primitive(State1, 15, [ctx, 998238357]),
    {State3, ResultReg} = ?BACKEND:call_func_ptr(State2, {free, FuncPtr}, [
        ctx, 0, 1, {free, {x_reg, 0}}, {free, ArgReg}
    ]),
    State4 = ?BACKEND:if_block(State3, {ResultReg, '==', 0}, fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {x_reg, 0}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	04000f93          	li	t6,64\n"
            "   4:	9fce                	add	t6,t6,s3\n"
            "   6:	000fbf83          	ld	t6,0(t6)\n"
            "   a:	1141                	addi	sp,sp,-16\n"
            "   c:	e006                	sd	ra,0(sp)\n"
            "   e:	4509                	li	a0,2\n"
            "  10:	9f82                	jalr	t6\n"
            "  12:	8faa                	mv	t6,a0\n"
            "  14:	6082                	ld	ra,0(sp)\n"
            "  16:	0141                	addi	sp,sp,16\n"
            "  18:	07800f13          	li	t5,120\n"
            "  1c:	9f4e                	add	t5,t5,s3\n"
            "  1e:	000f3f03          	ld	t5,0(t5)\n"
            "  22:	1141                	addi	sp,sp,-16\n"
            "  24:	e006                	sd	ra,0(sp)\n"
            "  26:	e47e                	sd	t6,8(sp)\n"
            "  28:	3b7ff537          	lui	a0,0x3b7ff\n"
            "  2c:	8955051b          	addiw	a0,a0,-1899 # 0x3b7fe895\n"
            "  30:	0544b823          	sd	s4,80(s1)\n"
            "  34:	9f02                	jalr	t5\n"
            "  36:	8f2a                	mv	t5,a0\n"
            "  38:	6082                	ld	ra,0(sp)\n"
            "  3a:	6fa2                	ld	t6,8(sp)\n"
            "  3c:	0141                	addi	sp,sp,16\n"
            "  3e:	0504ba03          	ld	s4,80(s1)\n"
            "  42:	1141                	addi	sp,sp,-16\n"
            "  44:	e006                	sd	ra,0(sp)\n"
            "  46:	8526                	mv	a0,s1\n"
            "  48:	4581                	li	a1,0\n"
            "  4a:	4605                	li	a2,1\n"
            "  4c:	6cb4                	ld	a3,88(s1)\n"
            "  4e:	877a                	mv	a4,t5\n"
            "  50:	0544b823          	sd	s4,80(s1)\n"
            "  54:	9f82                	jalr	t6\n"
            "  56:	8faa                	mv	t6,a0\n"
            "  58:	6082                	ld	ra,0(sp)\n"
            "  5a:	0141                	addi	sp,sp,16\n"
            "  5c:	0504ba03          	ld	s4,80(s1)\n"
            "  60:	000f9c63          	bnez	t6,0x78\n"
            "  64:	03000f93          	li	t6,48\n"
            "  68:	9fce                	add	t6,t6,s3\n"
            "  6a:	000fbf83          	ld	t6,0(t6)\n"
            "  6e:	06e00513          	li	a0,110\n"
            "  72:	0544b823          	sd	s4,80(s1)\n"
            "  76:	8f82                	jr	t6\n"
            "  78:	05f4bc23          	sd	t6,88(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

get_list_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg} = ?BACKEND:and_(State1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    State3 = ?BACKEND:move_array_element(State2, Reg, 1, {y_reg, 1}),
    State4 = ?BACKEND:move_array_element(State3, Reg, 0, {y_reg, 0}),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	ffcfff93          	andi	t6,t6,-4\n"
            "   8:	008fbe83          	ld	t4,8(t6)\n"
            "   c:	01da3423          	sd	t4,8(s4)\n"
            "  10:	000fbe83          	ld	t4,0(t6)\n"
            "  14:	01da3023          	sd	t4,0(s4)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

is_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    Arg1 = {x_reg, 0},
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, Arg1),
    State3 = ?BACKEND:if_block(
        State2, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(MSt0) ->
            MSt1 = ?BACKEND:if_block(
                MSt0, {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            ),
            {MSt2, Reg} = ?BACKEND:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
            MSt3 = ?BACKEND:move_array_element(MSt2, Reg, 0, Reg),
            ?BACKEND:if_block(
                MSt3,
                {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            )
        end
    ),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	ffff                	.insn	2, 0xffff\n"
            "   2:	ffff                	.insn	2, 0xffff\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	0f868067          	jr	248(a3) # 0x100\n"
            "  10:	0584bf83          	ld	t6,88(s1)\n"
            "  14:	ffffcf13          	not	t5,t6\n"
            "  18:	1f72                	slli	t5,t5,0x3c\n"
            "  1a:	020f0963          	beqz	t5,0x4c\n"
            "  1e:	8f7e                	mv	t5,t6\n"
            "  20:	003f7f13          	andi	t5,t5,3\n"
            "  24:	4e89                	li	t4,2\n"
            "  26:	01df0663          	beq	t5,t4,0x32\n"
            "  2a:	a8d9                	j	0x100\n"
            "  2c:	0001                	nop\n"
            "  2e:	00000013          	nop\n"
            "  32:	ffcfff93          	andi	t6,t6,-4\n"
            "  36:	000fbf83          	ld	t6,0(t6)\n"
            "  3a:	03ffff93          	andi	t6,t6,63\n"
            "  3e:	4f21                	li	t5,8\n"
            "  40:	01ef8663          	beq	t6,t5,0x4c\n"
            "  44:	a875                	j	0x100\n"
            "  46:	0001                	nop\n"
            "  48:	00000013          	nop"
        >>,
    ?assertStream(riscv64, Dump, Stream).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

%% Keep the unoptimized version to test the and case.
is_number_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    Arg1 = {x_reg, 0},
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, Arg1),
    State3 = ?BACKEND:if_block(
        State2, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(BSt0) ->
            BSt1 = cond_jump_to_label(
                {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, ?BACKEND, BSt0
            ),
            {BSt2, Reg} = ?BACKEND:and_(BSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
            BSt3 = ?BACKEND:move_array_element(BSt2, Reg, 0, Reg),
            cond_jump_to_label(
                {'and', [
                    {Reg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                    {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FLOAT}
                ]},
                Label,
                ?BACKEND,
                BSt3
            )
        end
    ),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	ffff                	.insn	2, 0xffff\n"
            "   2:	ffff                	.insn	2, 0xffff\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	0f868067          	jr	248(a3) # 0x100\n"
            "  10:	0584bf83          	ld	t6,88(s1)\n"
            "  14:	ffffcf13          	not	t5,t6\n"
            "  18:	1f72                	slli	t5,t5,0x3c\n"
            "  1a:	020f0f63          	beqz	t5,0x58\n"
            "  1e:	8f7e                	mv	t5,t6\n"
            "  20:	003f7f13          	andi	t5,t5,3\n"
            "  24:	4e89                	li	t4,2\n"
            "  26:	01df0663          	beq	t5,t4,0x32\n"
            "  2a:	a8d9                	j	0x100\n"
            "  2c:	0001                	nop\n"
            "  2e:	00000013          	nop\n"
            "  32:	ffcfff93          	andi	t6,t6,-4\n"
            "  36:	000fbf83          	ld	t6,0(t6)\n"
            "  3a:	8f7e                	mv	t5,t6\n"
            "  3c:	03ff7f13          	andi	t5,t5,63\n"
            "  40:	4ea1                	li	t4,8\n"
            "  42:	01df0b63          	beq	t5,t4,0x58\n"
            "  46:	03ffff93          	andi	t6,t6,63\n"
            "  4a:	4f61                	li	t5,24\n"
            "  4c:	01ef8663          	beq	t6,t5,0x58\n"
            "  50:	a845                	j	0x100\n"
            "  52:	0001                	nop\n"
            "  54:	00000013          	nop"
        >>,
    ?assertStream(riscv64, Dump, Stream).

is_boolean_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(State2, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	ffff                	.insn	2, 0xffff\n"
        "   2:	ffff                	.insn	2, 0xffff\n"
        "   4:	ffff                	.insn	2, 0xffff\n"
        "   6:	ffff                	.insn	2, 0xffff\n"
        "   8:	00000697          	auipc	a3,0x0\n"
        "   c:	0f868067          	jr	248(a3) # 0x100\n"
        "  10:	0584bf83          	ld	t6,88(s1)\n"
        "  14:	04b00f13          	li	t5,75\n"
        "  18:	01ef8963          	beq	t6,t5,0x2a\n"
        "  1c:	4f2d                	li	t5,11\n"
        "  1e:	01ef8663          	beq	t6,t5,0x2a\n"
        "  22:	a8f9                	j	0x100\n"
        "  24:	0001                	nop\n"
        "  26:	00000013          	nop"
    >>,
    ?assertStream(riscv64, Dump, Stream).

is_boolean_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    State1 = ?BACKEND:jump_table(State0, 1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(State2, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#1000),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	ffff                	.insn	2, 0xffff\n"
            "   2:	ffff                	.insn	2, 0xffff\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	00001697          	auipc	a3,0x1\n"
            "   c:	ff868067          	jr	-8(a3) # 0x1000\n"
            "  10:	0584bf83          	ld	t6,88(s1)\n"
            "  14:	04b00f13          	li	t5,75\n"
            "  18:	01ef8963          	beq	t6,t5,0x2a\n"
            "  1c:	4f2d                	li	t5,11\n"
            "  1e:	01ef8663          	beq	t6,t5,0x2a\n"
            "  22:	7df0006f          	j	0x1000\n"
            "  26:	00000013          	nop"
        >>,
    ?assertStream(riscv64, Dump, Stream).

is_boolean_far_known_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    State2 = ?BACKEND:add_label(State1, Label, 16#1000),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	ffff                	.insn	2, 0xffff\n"
            "   2:	ffff                	.insn	2, 0xffff\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	00001697          	auipc	a3,0x1\n"
            "   c:	ff868067          	jr	-8(a3) # 0x1000\n"
            "  10:	0584bf83          	ld	t6,88(s1)\n"
            "  14:	04b00f13          	li	t5,75\n"
            "  18:	01ef8963          	beq	t6,t5,0x2a\n"
            "  1c:	4f2d                	li	t5,11\n"
            "  1e:	01ef8663          	beq	t6,t5,0x2a\n"
            "  22:	00001f17          	auipc	t5,0x1\n"
            "  26:	fdef0067          	jr	-34(t5) # 0x1000"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test OP_WAIT_TIMEOUT pattern that uses set_continuation_to_offset and continuation_entry_point
wait_timeout_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    Label = 42,
    {State1, OffsetRef0} = ?BACKEND:set_continuation_to_offset(State0),
    {State2, TimeoutReg} = ?BACKEND:move_to_native_register(State1, 5000),
    State3 = ?BACKEND:call_primitive_last(State2, ?PRIM_WAIT_TIMEOUT, [
        ctx, jit_state, {free, TimeoutReg}, Label
    ]),
    State4 = ?BACKEND:add_label(State3, OffsetRef0),
    State5 = ?BACKEND:continuation_entry_point(State4),
    {State6, ResultReg0} = ?BACKEND:call_primitive(State5, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    State7 = ?BACKEND:return_if_not_equal_to_ctx(State6, {free, ResultReg0}),
    % ?WAITING_TIMEOUT_EXPIRED
    {State8, ResultReg1} = ?BACKEND:call_primitive(State7, ?PRIM_CONTEXT_GET_FLAGS, [ctx, 2]),
    State9 = ?BACKEND:if_block(State8, {{free, ResultReg1}, '==', 0}, fun(BlockSt) ->
        ?BACKEND:call_primitive_last(BlockSt, ?PRIM_WAIT_TIMEOUT_TRAP_HANDLER, [
            ctx, jit_state, Label
        ])
    end),
    State10 = ?BACKEND:update_branches(State9),

    Stream = ?BACKEND:stream(State10),
    Dump =
        <<
            "   0:	00000f97          	auipc	t6,0x0\n"
            "   4:	028f8f93          	addi	t6,t6,40 # 0x28\n"
            "   8:	01f93423          	sd	t6,8(s2)\n"
            "   c:	6f85                	lui	t6,0x1\n"
            "   e:	388f8f9b          	addiw	t6,t6,904 # 0x1388\n"
            "  12:	0f000f13          	li	t5,240\n"
            "  16:	9f4e                	add	t5,t5,s3\n"
            "  18:	000f3f03          	ld	t5,0(t5)\n"
            "  1c:	857e                	mv	a0,t6\n"
            "  1e:	02a00593          	li	a1,42\n"
            "  22:	0544b823          	sd	s4,80(s1)\n"
            "  26:	8f02                	jr	t5\n"
            "  28:	0a800f93          	li	t6,168\n"
            "  2c:	9fce                	add	t6,t6,s3\n"
            "  2e:	000fbf83          	ld	t6,0(t6)\n"
            "  32:	1141                	addi	sp,sp,-16\n"
            "  34:	e006                	sd	ra,0(sp)\n"
            "  36:	0544b823          	sd	s4,80(s1)\n"
            "  3a:	9f82                	jalr	t6\n"
            "  3c:	8faa                	mv	t6,a0\n"
            "  3e:	6082                	ld	ra,0(sp)\n"
            "  40:	0141                	addi	sp,sp,16\n"
            "  42:	0504ba03          	ld	s4,80(s1)\n"
            "  46:	009f8463          	beq	t6,s1,0x4e\n"
            "  4a:	857e                	mv	a0,t6\n"
            "  4c:	8082                	ret\n"
            "  4e:	02100f93          	li	t6,33\n"
            "  52:	0f8e                	slli	t6,t6,0x3\n"
            "  54:	9fce                	add	t6,t6,s3\n"
            "  56:	000fbf83          	ld	t6,0(t6)\n"
            "  5a:	1141                	addi	sp,sp,-16\n"
            "  5c:	e006                	sd	ra,0(sp)\n"
            "  5e:	4509                	li	a0,2\n"
            "  60:	9f82                	jalr	t6\n"
            "  62:	8faa                	mv	t6,a0\n"
            "  64:	6082                	ld	ra,0(sp)\n"
            "  66:	0141                	addi	sp,sp,16\n"
            "  68:	000f9c63          	bnez	t6,0x80\n"
            "  6c:	0f800f93          	li	t6,248\n"
            "  70:	9fce                	add	t6,t6,s3\n"
            "  72:	000fbf83          	ld	t6,0(t6)\n"
            "  76:	02a00513          	li	a0,42\n"
            "  7a:	0544b823          	sd	s4,80(s1)\n"
            "  7e:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test OP_WAIT pattern that uses set_continuation_to_label
wait_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:set_continuation_to_label(State2, Label),
    State4 = ?BACKEND:call_primitive_last(State3, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),

    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	ffff                	.insn	2, 0xffff\n"
            "   2:	ffff                	.insn	2, 0xffff\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	02868067          	jr	40(a3) # 0x30\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	0f068067          	jr	240(a3) # 0x100\n"
            "  18:	ffff                	.insn	2, 0xffff\n"
            "  1a:	ffff                	.insn	2, 0xffff\n"
            "  1c:	ffff                	.insn	2, 0xffff\n"
            "  1e:	ffff                	.insn	2, 0xffff\n"
            "  20:	ffff                	.insn	2, 0xffff\n"
            "  22:	ffff                	.insn	2, 0xffff\n"
            "  24:	ffff                	.insn	2, 0xffff\n"
            "  26:	ffff                	.insn	2, 0xffff\n"
            "  28:	ffff                	.insn	2, 0xffff\n"
            "  2a:	ffff                	.insn	2, 0xffff\n"
            "  2c:	ffff                	.insn	2, 0xffff\n"
            "  2e:	ffff                	.insn	2, 0xffff\n"
            "  30:	00000f97          	auipc	t6,0x0\n"
            "  34:	0d0f8f93          	addi	t6,t6,208 # 0x100\n"
            "  38:	01f93423          	sd	t6,8(s2)\n"
            "  3c:	0e800f93          	li	t6,232\n"
            "  40:	9fce                	add	t6,t6,s3\n"
            "  42:	000fbf83          	ld	t6,0(t6)\n"
            "  46:	0544b823          	sd	s4,80(s1)\n"
            "  4a:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test set_continuation_to_label with known label
wait_known_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:add_label(State2, Label, 16#100),
    State4 = ?BACKEND:set_continuation_to_label(State3, Label),
    State5 = ?BACKEND:call_primitive_last(State4, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    State6 = ?BACKEND:update_branches(State5),

    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	ffff                	.insn	2, 0xffff\n"
            "   2:	ffff                	.insn	2, 0xffff\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	02868067          	jr	40(a3) # 0x30\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	0f068067          	jr	240(a3) # 0x100\n"
            "  18:	ffff                	.insn	2, 0xffff\n"
            "  1a:	ffff                	.insn	2, 0xffff\n"
            "  1c:	ffff                	.insn	2, 0xffff\n"
            "  1e:	ffff                	.insn	2, 0xffff\n"
            "  20:	ffff                	.insn	2, 0xffff\n"
            "  22:	ffff                	.insn	2, 0xffff\n"
            "  24:	ffff                	.insn	2, 0xffff\n"
            "  26:	ffff                	.insn	2, 0xffff\n"
            "  28:	ffff                	.insn	2, 0xffff\n"
            "  2a:	ffff                	.insn	2, 0xffff\n"
            "  2c:	ffff                	.insn	2, 0xffff\n"
            "  2e:	ffff                	.insn	2, 0xffff\n"
            "  30:	00000f97          	auipc	t6,0x0\n"
            "  34:	0d0f8f93          	addi	t6,t6,208 # 0x100\n"
            "  38:	01f93423          	sd	t6,8(s2)\n"
            "  3c:	0e800f93          	li	t6,232\n"
            "  40:	9fce                	add	t6,t6,s3\n"
            "  42:	000fbf83          	ld	t6,0(t6)\n"
            "  46:	0544b823          	sd	s4,80(s1)\n"
            "  4a:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test return_labels_and_lines/2 function
return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),

    % Test return_labels_and_lines with some sample labels and lines
    State2 = ?BACKEND:add_label(State1, 2, 32),
    State3 = ?BACKEND:add_label(State2, 1, 16),

    % {Line, Offset} pairs
    SortedLines = [{10, 16}, {20, 32}],

    State4 = ?BACKEND:return_labels_and_lines(State3, SortedLines),
    Stream = ?BACKEND:stream(State4),

    % Should have jump table + generated code with label/line tables
    ?assert(byte_size(Stream) >= 32),

    % Expected: jump table (3 entries, 24 bytes) + auipc + addi + ret + padding + labels table + lines table
    Dump =
        <<
            "   0:  ffff                .insn   2, 0xffff\n"
            "   2:  ffff                .insn   2, 0xffff\n"
            "   4:  ffff                .insn   2, 0xffff\n"
            "   6:  ffff                .insn   2, 0xffff\n"
            "   8:  00000697            auipc   a3,0x0\n"
            "   c:  00868067            jr  8(a3) # 0x10\n"
            "  10:  00000697            auipc   a3,0x0\n"
            "  14:  01068067            jr  16(a3) # 0x20\n"
            "  18:  00000517            auipc   a0,0x0\n"
            "  1c:  0529                addi    a0,a0,10 # 0x22\n"
            "  1e:  8082                ret\n"
            "  20:  ffff                .insn   2, 0xffff\n"
            "  22:  0200                addi    s0,sp,256\n"
            "  24:  0100                addi    s0,sp,128\n"
            "  26:  0000                unimp\n"
            "  28:  1000                addi    s0,sp,32\n"
            "  2a:  0200                addi    s0,sp,256\n"
            "  2c:  0000                unimp\n"
            "  2e:  2000                fld fs0,0(s0)\n"
            "  30:  0200                addi    s0,sp,256\n"
            "  32:  0a00                addi    s0,sp,272\n"
            "  34:  0000                unimp\n"
            "  36:  1000                addi    s0,sp,32\n"
            "  38:  1400                addi    s0,sp,544\n"
            "  3a:  0000                unimp\n"
            "  3c:  2000                fld fs0,0(s0)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test call_primitive with {free, {x_reg, X}}
gc_bif2_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_GET_IMPORTED_BIF, [jit_state, 42]),
    {State2, _ResultReg} = ?BACKEND:call_func_ptr(State1, {free, FuncPtr}, [
        ctx, 0, 3, {y_reg, 0}, {free, {x_reg, 0}}
    ]),

    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	04000f93          	li	t6,64\n"
            "   4:	9fce                	add	t6,t6,s3\n"
            "   6:	000fbf83          	ld	t6,0(t6)\n"
            "   a:	1141                	addi	sp,sp,-16\n"
            "   c:	e006                	sd	ra,0(sp)\n"
            "   e:	02a00513          	li	a0,42\n"
            "  12:	9f82                	jalr	t6\n"
            "  14:	8faa                	mv	t6,a0\n"
            "  16:	6082                	ld	ra,0(sp)\n"
            "  18:	0141                	addi	sp,sp,16\n"
            "  1a:	1141                	addi	sp,sp,-16\n"
            "  1c:	e006                	sd	ra,0(sp)\n"
            "  1e:	8526                	mv	a0,s1\n"
            "  20:	4581                	li	a1,0\n"
            "  22:	460d                	li	a2,3\n"
            "  24:	000a3683          	ld	a3,0(s4)\n"
            "  28:	6cb8                	ld	a4,88(s1)\n"
            "  2a:	0544b823          	sd	s4,80(s1)\n"
            "  2e:	9f82                	jalr	t6\n"
            "  30:	8faa                	mv	t6,a0\n"
            "  32:	6082                	ld	ra,0(sp)\n"
            "  34:	0141                	addi	sp,sp,16\n"
            "  36:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test case where parameter value is in a1
memory_ensure_free_with_roots_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, {free, a1}, 4, 1
    ]),

    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	02c00f93          	li	t6,44\n"
            "   4:	0f8e                	slli	t6,t6,0x3\n"
            "   6:	9fce                	add	t6,t6,s3\n"
            "   8:	000fbf83          	ld	t6,0(t6)\n"
            "   c:	1141                	addi	sp,sp,-16\n"
            "   e:	e006                	sd	ra,0(sp)\n"
            "  10:	852e                	mv	a0,a1\n"
            "  12:	4591                	li	a1,4\n"
            "  14:	4605                	li	a2,1\n"
            "  16:	0544b823          	sd	s4,80(s1)\n"
            "  1a:	9f82                	jalr	t6\n"
            "  1c:	8faa                	mv	t6,a0\n"
            "  1e:	6082                	ld	ra,0(sp)\n"
            "  20:	0141                	addi	sp,sp,16\n"
            "  22:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_ext_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	01092f83          	lw	t6,16(s2)\n"
            "   4:	1ffd                	addi	t6,t6,-1\n"
            "   6:	01f92823          	sw	t6,16(s2)\n"
            "   a:	000f9f63          	bnez	t6,0x28\n"
            "   e:	00000f97          	auipc	t6,0x0\n"
            "  12:	0fe9                	addi	t6,t6,26 # 0x28\n"
            "  14:	0001                	nop\n"
            "  16:	01f93423          	sd	t6,8(s2)\n"
            "  1a:	4fc1                	li	t6,16\n"
            "  1c:	9fce                	add	t6,t6,s3\n"
            "  1e:	000fbf83          	ld	t6,0(t6)\n"
            "  22:	0544b823          	sd	s4,80(s1)\n"
            "  26:	8f82                	jr	t6\n"
            "  28:	00093f03          	ld	t5,0(s2)\n"
            "  2c:	000f2f03          	lw	t5,0(t5)\n"
            "  30:	0f62                	slli	t5,t5,0x18\n"
            "  32:	16000f93          	li	t6,352\n"
            "  36:	00000013          	nop\n"
            "  3a:	01ff6f33          	or	t5,t5,t6\n"
            "  3e:	0fe4b023          	sd	t5,224(s1)\n"
            "  42:	02000f93          	li	t6,32\n"
            "  46:	9fce                	add	t6,t6,s3\n"
            "  48:	000fbf83          	ld	t6,0(t6)\n"
            "  4c:	4509                	li	a0,2\n"
            "  4e:	4595                	li	a1,5\n"
            "  50:	567d                	li	a2,-1\n"
            "  52:	0544b823          	sd	s4,80(s1)\n"
            "  56:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

call_fun_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    FuncReg = {x_reg, 0},
    ArgsCount = 0,
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, FuncReg),
    {State3, RegCopy} = ?BACKEND:copy_to_native_register(State2, Reg),
    State4 = ?BACKEND:if_block(
        State3, {RegCopy, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
            ?BACKEND:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, RegCopy
            ])
        end
    ),
    {State5, RegCopy} = ?BACKEND:and_(State4, {free, RegCopy}, ?TERM_PRIMARY_CLEAR_MASK),
    State6 = ?BACKEND:move_array_element(State5, RegCopy, 0, RegCopy),
    State7 = ?BACKEND:if_block(
        State6, {RegCopy, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FUN}, fun(BSt0) ->
            ?BACKEND:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, RegCopy
            ])
        end
    ),
    State8 = ?BACKEND:free_native_registers(State7, [RegCopy]),
    State9 = ?BACKEND:call_primitive_with_cp(State8, ?PRIM_CALL_FUN, [
        ctx, jit_state, Reg, ArgsCount
    ]),
    ?BACKEND:assert_all_native_free(State9),
    Stream = ?BACKEND:stream(State9),
    Dump =
        <<
            "   0:	01092f83          	lw	t6,16(s2)\n"
            "   4:	1ffd                	addi	t6,t6,-1\n"
            "   6:	01f92823          	sw	t6,16(s2)\n"
            "   a:	000f9f63          	bnez	t6,0x28\n"
            "   e:	00000f97          	auipc	t6,0x0\n"
            "  12:	0fe9                	addi	t6,t6,26 # 0x28\n"
            "  14:	0001                	nop\n"
            "  16:	01f93423          	sd	t6,8(s2)\n"
            "  1a:	4fc1                	li	t6,16\n"
            "  1c:	9fce                	add	t6,t6,s3\n"
            "  1e:	000fbf83          	ld	t6,0(t6)\n"
            "  22:	0544b823          	sd	s4,80(s1)\n"
            "  26:	8f82                	jr	t6\n"
            "  28:	0584bf83          	ld	t6,88(s1)\n"
            "  2c:	8f7e                	mv	t5,t6\n"
            "  2e:	8efa                	mv	t4,t5\n"
            "  30:	003efe93          	andi	t4,t4,3\n"
            "  34:	4e09                	li	t3,2\n"
            "  36:	01ce8f63          	beq	t4,t3,0x54\n"
            "  3a:	09800f93          	li	t6,152\n"
            "  3e:	9fce                	add	t6,t6,s3\n"
            "  40:	000fbf83          	ld	t6,0(t6)\n"
            "  44:	04400513          	li	a0,68\n"
            "  48:	18b00593          	li	a1,395\n"
            "  4c:	867a                	mv	a2,t5\n"
            "  4e:	0544b823          	sd	s4,80(s1)\n"
            "  52:	8f82                	jr	t6\n"
            "  54:	ffcf7f13          	andi	t5,t5,-4\n"
            "  58:	000f3f03          	ld	t5,0(t5)\n"
            "  5c:	8efa                	mv	t4,t5\n"
            "  5e:	03fefe93          	andi	t4,t4,63\n"
            "  62:	4e51                	li	t3,20\n"
            "  64:	01ce8f63          	beq	t4,t3,0x82\n"
            "  68:	09800f93          	li	t6,152\n"
            "  6c:	9fce                	add	t6,t6,s3\n"
            "  6e:	000fbf83          	ld	t6,0(t6)\n"
            "  72:	07200513          	li	a0,114\n"
            "  76:	18b00593          	li	a1,395\n"
            "  7a:	867a                	mv	a2,t5\n"
            "  7c:	0544b823          	sd	s4,80(s1)\n"
            "  80:	8f82                	jr	t6\n"
            "  82:	00093e83          	ld	t4,0(s2)\n"
            "  86:	000eae83          	lw	t4,0(t4)\n"
            "  8a:	0ee2                	slli	t4,t4,0x18\n"
            "  8c:	2c800f13          	li	t5,712\n"
            "  90:	00000013          	nop\n"
            "  94:	01eeeeb3          	or	t4,t4,t5\n"
            "  98:	0fd4b023          	sd	t4,224(s1)\n"
            "  9c:	02000f13          	li	t5,32\n"
            "  a0:	0f0e                	slli	t5,t5,0x3\n"
            "  a2:	9f4e                	add	t5,t5,s3\n"
            "  a4:	000f3f03          	ld	t5,0(t5)\n"
            "  a8:	857e                	mv	a0,t6\n"
            "  aa:	4581                	li	a1,0\n"
            "  ac:	0544b823          	sd	s4,80(s1)\n"
            "  b0:	8f02                	jr	t5"
        >>,
    ?assertStream(riscv64, Dump, Stream).

decrement_reductions_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [Reg]),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    {State4, Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	0584bf83          	ld	t6,88(s1)\n"
        "   4:	01092f83          	lw	t6,16(s2)\n"
        "   8:	1ffd                	addi	t6,t6,-1\n"
        "   a:	01f92823          	sw	t6,16(s2)\n"
        "   e:	000f9f63          	bnez	t6,0x2c\n"
        "  12:	00000f97          	auipc	t6,0x0\n"
        "  16:	0fe9                	addi	t6,t6,26 # 0x2c\n"
        "  18:	0001                	nop\n"
        "  1a:	01f93423          	sd	t6,8(s2)\n"
        "  1e:	4fc1                	li	t6,16\n"
        "  20:	9fce                	add	t6,t6,s3\n"
        "  22:	000fbf83          	ld	t6,0(t6)\n"
        "  26:	0544b823          	sd	s4,80(s1)\n"
        "  2a:	8f82                	jr	t6\n"
        "  2c:	0584bf83          	ld	t6,88(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

move_to_vm_register_test0(State, Source, Dest, Dump) ->
    State1 = ?BACKEND:move_to_vm_register(State, Source, Dest),
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    ?assertStream(riscv64, Dump, Stream).

move_to_vm_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            {timeout, 60, [
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, 0}, <<
                        "   0:	4f81                	li	t6,0\n"
                        "   2:	05f4bc23          	sd	t6,88(s1)\n"
                        "   6:	a8ed                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	4f81                	li	t6,0\n"
                        "   2:	0df4bc23          	sd	t6,216(s1)\n"
                        "   6:	a8ed                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, t5}, <<
                        "   0:\t4f81                \tli\tt6,0\n"
                        "   2:\t01ff3023          \tsd\tt6,0(t5)\n"
                        "   6:\ta8ed                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	4f01                	li	t5,0\n"
                        "   2:	01ea3823          	sd	t5,16(s4)\n"
                        "   6:	a8ed                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	4f01                	li	t5,0\n"
                        "   2:	0bea3023          	sd	t5,160(s4)\n"
                        "   6:	a8ed                	j	0x100"
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	02a00f93          	li	t6,42\n"
                        "   4:	05f4bc23          	sd	t6,88(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	02a00f93          	li	t6,42\n"
                        "   4:	0df4bc23          	sd	t6,216(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	02a00f13          	li	t5,42\n"
                        "   4:	01ea3823          	sd	t5,16(s4)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	02a00f13          	li	t5,42\n"
                        "   4:	0bea3023          	sd	t5,160(s4)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, a3}, <<
                        "   0:\t06300f93          \tli\tt6,99\n"
                        "   4:\t01f6b023          \tsd\tt6,0(a3)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	0604bf83          	ld	t6,96(s1)\n"
                        "   4:	07f4b423          	sd	t6,104(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, a1}, <<
                        "   0:	0604bf83          	ld	t6,96(s1)\n"
                        "   4:	01f5b023          	sd	t6,0(a1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, t3}, {x_reg, 3}, <<
                        "   0:	000e3f83          	ld	t6,0(t3)\n"
                        "   4:	07f4b823          	sd	t6,112(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	01fa3423          	sd	t6,8(s4)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	000a3f83          	ld	t6,0(s4)\n"
                        "   4:	07f4b823          	sd	t6,112(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	008a3f83          	ld	t6,8(s4)\n"
                        "   4:	07f4b823          	sd	t6,112(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, t4, {x_reg, 0}, <<
                        "   0:	05d4bc23          	sd	t4,88(s1)\n"
                        "   4:	a8f5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, t5, {x_reg, extra}, <<
                        "   0:	0de4bc23          	sd	t5,216(s1)\n"
                        "   4:	a8f5                	j	0x100"
                    >>)
                end),
                %% Test: Native register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, t3, {ptr, a3}, <<
                        "   0:\t01c6b023          \tsd\tt3,0(a3)\n"
                        "   4:\ta8f5                \tj\t0x100"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, a1, {y_reg, 0}, <<
                        "   0:	00ba3023          	sd	a1,0(s4)\n"
                        "   4:	a8f5                	j	0x100"
                    >>)
                end),
                %% Test: Large immediate to x_reg (uses lui + addi in RISC-V)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, 0}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f9b          	addiw	t6,t6,1656 # 0x12345678\n"
                        "   8:	05f4bc23          	sd	t6,88(s1)\n"
                        "   c:	a8d5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, extra}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f9b          	addiw	t6,t6,1656 # 0x12345678\n"
                        "   8:	0df4bc23          	sd	t6,216(s1)\n"
                        "   c:	a8d5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 2}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f9b          	addiw	t6,t6,1656 # 0x12345678\n"
                        "   8:	01fa3823          	sd	t6,16(s4)\n"
                        "   c:	a8d5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 20}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f9b          	addiw	t6,t6,1656 # 0x12345678\n"
                        "   8:	0bfa3023          	sd	t6,160(s4)\n"
                        "   c:	a8d5                	j	0x100"
                    >>)
                end),
                %% Test: Large immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {ptr, a3}, <<
                        "   0:	12345fb7          	lui	t6,0x12345\n"
                        "   4:	678f8f9b          	addiw	t6,t6,1656 # 0x12345678\n"
                        "   8:	01f6b023          	sd	t6,0(a3)\n"
                        "   c:	a8d5                	j	0x100"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	0d04bf83          	ld	t6,208(s1)\n"
                        "   4:	0ffa3c23          	sd	t6,248(s4)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	0f8a3f83          	ld	t6,248(s4)\n"
                        "   4:	0df4b823          	sd	t6,208(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                %% Test: Large y_reg index (32) that exceeds str immediate offset limit
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 32}, <<
                        "   0:	02a00f13          	li	t5,42\n"
                        "   4:	10000f93          	li	t6,256\n"
                        "   8:	9fd2                	add	t6,t6,s4\n"
                        "   a:	01efb023          	sd	t5,0(t6)\n"
                        "   e:	a8cd                	j	0x100"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	5ffd                	li	t6,-1\n"
                        "   2:	05f4bc23          	sd	t6,88(s1)\n"
                        "   6:	a8ed                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, -100, {x_reg, 0}, <<
                        "   0:	f9c00f93          	li	t6,-100\n"
                        "   4:	05f4bc23          	sd	t6,88(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, -1000, {x_reg, 0}, <<
                        "   0:	c1800f93          	li	t6,-1000\n"
                        "   4:	05f4bc23          	sd	t6,88(s1)\n"
                        "   8:	a8e5                	j	0x100"
                    >>)
                end)
            ]}
        end}.

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(riscv64, Dump, Stream).

move_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 2, {x_reg, 0}, <<
                        "   0:	0106bf83          	ld	t6,16(a3)\n"
                        "   4:	05f4bc23          	sd	t6,88(s1)"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, a3, 3, {ptr, t4}, <<
                        "   0:\t0186bf83          \tld\tt6,24(a3)\n"
                        "   4:\t01feb023          \tsd\tt6,0(t4)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 1, {y_reg, 2}, <<
                        "   0:	0086bf03          	ld	t5,8(a3)\n"
                        "   4:	01ea3823          	sd	t5,16(s4)"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (t4)
                ?_test(begin
                    move_array_element_test0(State0, a3, 1, t4, <<
                        "   0:\t0086be83          \tld\tt4,8(a3)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 7, {y_reg, 31}, <<
                        "   0:	0386bf03          	ld	t5,56(a3)\n"
                        "   4:	0fea3c23          	sd	t5,248(s4)"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 7, {x_reg, 15}, <<
                        "   0:	0386bf83          	ld	t6,56(a3)\n"
                        "   4:	0df4b823          	sd	t6,208(s1)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {x_reg, 2}, <<
                        "   0:	0206bf83          	ld	t6,32(a3)\n"
                        "   4:	0f8e                	slli	t6,t6,0x3\n"
                        "   6:	01f68fb3          	add	t6,a3,t6\n"
                        "   a:	000fbf83          	ld	t6,0(t6)\n"
                        "   e:	07f4b423          	sd	t6,104(s1)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {ptr, t4}, <<
                        "   0:\t0206bf83          \tld\tt6,32(a3)\n"
                        "   4:\t0f8e                \tslli\tt6,t6,0x3\n"
                        "   6:\t01f68fb3          \tadd\tt6,a3,t6\n"
                        "   a:\t000fbf83          \tld\tt6,0(t6)\n"
                        "   e:\t01feb023          \tsd\tt6,0(t4)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {y_reg, 31}, <<
                        "   0:	0206bf83          	ld	t6,32(a3)\n"
                        "   4:	0f8e                	slli	t6,t6,0x3\n"
                        "   6:	01f68fb3          	add	t6,a3,t6\n"
                        "   a:	000fbf83          	ld	t6,0(t6)\n"
                        "   e:	0ffa3c23          	sd	t6,248(s4)"
                    >>)
                end),
                %% move_array_element with integer index and x_reg destination
                ?_test(begin
                    {State1, BaseReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    move_array_element_test0(State1, BaseReg, 2, {x_reg, 5}, <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	010fbf03          	ld	t5,16(t6)\n"
                        "   8:	09e4b023          	sd	t5,128(s1)"
                    >>)
                end)
            ]
        end}.

get_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% get_array_element: reg[x] to new native reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, t3, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t020e3f83          \tld\tt6,32(t3)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(t6, Reg)
                end)
            ]
        end}.

move_to_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_to_array_element/4: x_reg to reg[x]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	01f6b823          	sd	t6,16(a3)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	8f72                	mv	t5,t3\n"
                        "   6:	0f0e                	slli	t5,t5,0x3\n"
                        "   8:	01e68f33          	add	t5,a3,t5\n"
                        "   c:	01ff3023          	sd	t6,0(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/4: ptr to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {ptr, t6}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t000fbf83          \tld\tt6,0(t6)\n"
                        "   4:\t8f72                \tmv\tt5,t3\n"
                        "   6:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   8:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "   c:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/4: y_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 2}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	010a3f83          	ld	t6,16(s4)\n"
                        "   4:	8f72                	mv	t5,t3\n"
                        "   6:	0f0e                	slli	t5,t5,0x3\n"
                        "   8:	01e68f33          	add	t5,a3,t5\n"
                        "   c:	01ff3023          	sd	t6,0(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	01f6bc23          	sd	t6,24(a3)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    A3Bit = 1 bsl 3,
                    T3Bit = 1 bsl 11,
                    UsedMask = A3Bit bor T3Bit,
                    Regs0 = element(9, State0),
                    AvailMask = jit_regs:available_regs(Regs0) band (bnot UsedMask),
                    State2 = setelement(9, State0, jit_regs:set_masks(Regs0, AvailMask, UsedMask)),
                    ?assertEqual(lists:sort([a3, t3]), lists:sort(?BACKEND:used_regs(State2))),
                    State3 = ?BACKEND:move_to_array_element(State2, {x_reg, 0}, a3, t3, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	001e0f13          	addi	t5,t3,1\n"
                        "   8:	0f0e                	slli	t5,t5,0x3\n"
                        "   a:	01e68f33          	add	t5,a3,t5\n"
                        "   e:	01ff3023          	sd	t6,0(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/5: imm to reg[x+offset]
                ?_test(begin
                    A3Bit = 1 bsl 3,
                    T3Bit = 1 bsl 11,
                    UsedMask = A3Bit bor T3Bit,
                    Regs0 = element(9, State0),
                    AvailMask = jit_regs:available_regs(Regs0) band (bnot UsedMask),
                    State2 = setelement(9, State0, jit_regs:set_masks(Regs0, AvailMask, UsedMask)),
                    ?assertEqual(lists:sort([a3, t3]), lists:sort(?BACKEND:used_regs(State2))),
                    State3 = ?BACKEND:move_to_array_element(State2, 42, a3, t3, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:\t02a00f93          \tli\tt6,42\n"
                        "   4:\t001e0f13          \taddi\tt5,t3,1\n"
                        "   8:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   a:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "   e:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end)
            ]
        end}.

move_to_native_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_to_native_register/2: imm
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, 42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  02a00f93            li  t6,42"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: negative value
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  fd600f93            li  t6,-42"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: -255 (boundary case)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -255),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  f0100f93            li  t6,-255"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: -256 (boundary case, fits in immediate for RISC-V)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -256),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  f0000f93            li  t6,-256\n"
                        "   4:  a8f5                    j   0x100"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, t5}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t5, Reg),
                    Dump = <<
                        "   0:\t000f3f03          \tld\tt5,0(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: {x_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	0804bf83          	ld	t6,128(s1)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: {y_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:	018a3f83          	ld	t6,24(s4)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, t5),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:  02a00f13            li  t5,42"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, t6, t4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:  8efe                    mv  t4,t6"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, t6}, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t000fbe03          \tld\tt3,0(t6)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: {x_reg, x} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, a3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	74b4                	ld	a3,104(s1)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: {y_reg, y} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, a1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	010a3583          	ld	a1,16(s4)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                %% Test: ptr with offset to fp_reg (term_to_float)
                ?_test(begin
                    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    State2 = ?BACKEND:move_to_vm_register(
                        State1, {free, {ptr, RegA, 1}}, {fp_reg, 3}
                    ),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	0584bf83          	ld	t6,88(s1)\n"
                        "   4:	01893f03          	ld	t5,24(s2)\n"
                        "   8:	008fae83          	lw	t4,8(t6)\n"
                        "   c:	01df2c23          	sw	t4,24(t5)\n"
                        "  10:	00cfae83          	lw	t4,12(t6)\n"
                        "  14:	01df2e23          	sw	t4,28(t5)"
                    >>,
                    ?assertStream(riscv64, Dump, Stream)
                end)
            ]
        end}.

add_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:add(State0, Reg, Imm),
    % Force emission of literal pool
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    ?assertStream(riscv64, Dump, Stream).

add_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    add_test0(State0, a2, 2, <<
                        "   0:  0609                    addi    a2,a2,2\n"
                        "   2:  a8fd                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, a2, 256, <<
                        "   0:  10000f93            li  t6,256\n"
                        "   4:  967e                    add a2,a2,t6\n"
                        "   6:  a8ed                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, a2, a3, <<
                        "   0:  9636                    add a2,a2,a3\n"
                        "   2:  a8fd                    j   0x100"
                    >>)
                end)
            ]
        end}.

sub_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:sub(State0, Reg, Imm),
    % Force emission of literal pool
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    ?assertStream(riscv64, Dump, Stream).

sub_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    sub_test0(State0, a2, 2, <<
                        "   0:  1679                    addi    a2,a2,-2\n"
                        "   2:  a8fd                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, a2, 256, <<
                        "      0:   10000f93            li  t6,256\n"
                        "      4:   41f60633            sub a2,a2,t6\n"
                        "      8:   a8e5                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, a2, a3, <<
                        "      0:   8e15                    sub a2,a2,a3\n"
                        "      2:   a8fd                    j   0x100"
                    >>)
                end)
            ]
        end}.

mul_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:mul(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(riscv64, Dump, Stream).

mul_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    mul_test0(State0, a2, 2, <<
                        "      0:   0606                    slli    a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 3, <<
                        "      0:   00161f93            slli    t6,a2,0x1\n"
                        "      4:   00cf8633            add a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 4, <<
                        "      0:   060a                    slli    a2,a2,0x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 5, <<
                        "      0:   00261f93            slli    t6,a2,0x2\n"
                        "      4:   00cf8633            add a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 6, <<
                        "      0:   00161f93            slli    t6,a2,0x1\n"
                        "      4:   00cf8633            add a2,t6,a2\n"
                        "      8:   0606                    slli    a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 7, <<
                        "      0:   00361f93            slli    t6,a2,0x3\n"
                        "      4:   40cf8633            sub a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 8, <<
                        "      0:   060e                    slli    a2,a2,0x3"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 9, <<
                        "      0:   00361f93            slli    t6,a2,0x3\n"
                        "      4:   00cf8633            add a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 10, <<
                        "      0:   00261f93            slli    t6,a2,0x2\n"
                        "      4:   00cf8633            add a2,t6,a2\n"
                        "      8:   0606                    slli    a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 11, <<
                        "      0:   4fad                    li  t6,11\n"
                        "      2:   03f60633            mul a2,a2,t6"
                    >>)
                end)
            ]
        end}.

%% Test set_args1 with y_reg pattern
set_args1_y_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    % Call primitive with y_reg argument to trigger {y_reg, X} pattern in set_args1
    % This mirrors: {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}])
    % but with {y_reg, 5} instead of {free, Src}
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_BITSTRING_GET_UTF8, [
        {y_reg, 5}
    ]),

    Stream = ?BACKEND:stream(State1),
    % Expected disassembly for loading from y_reg and calling primitive
    Dump = <<
        "   0:	04300f93          	li	t6,67\n"
        "   4:	0f8e                	slli	t6,t6,0x3\n"
        "   6:	9fce                	add	t6,t6,s3\n"
        "   8:	000fbf83          	ld	t6,0(t6)\n"
        "   c:	1141                	addi	sp,sp,-16\n"
        "   e:	e006                	sd	ra,0(sp)\n"
        "  10:	028a3503          	ld	a0,40(s4)\n"
        "  14:	9f82                	jalr	t6\n"
        "  16:	8faa                	mv	t6,a0\n"
        "  18:	6082                	ld	ra,0(sp)\n"
        "  1a:	0141                	addi	sp,sp,16"
    >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test large Y register read (Y=123, offset=492, exceeds immediate limit)
large_y_reg_read_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move from a large Y register (123 * 4 = 492 bytes, exceeds immediate limit)
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 123}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp register for large offset
    Dump = <<
        "   0:	3d800f93          	li	t6,984\n"
        "   4:	9fd2                	add	t6,t6,s4\n"
        "   6:	000fbf83          	ld	t6,0(t6)"
    >>,
    ?assertStream(riscv64, Dump, Stream),
    ?assertEqual(t6, Reg).

%% Test large Y register write with immediate value
large_y_reg_write_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move immediate to a large Y register (123 * 4 = 492 bytes)
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {y_reg, 123}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp registers for large offset
    Dump = <<
        "   0:	02a00f13          	li	t5,42\n"
        "   4:	3d800f93          	li	t6,984\n"
        "   8:	9fd2                	add	t6,t6,s4\n"
        "   a:	01efb023          	sd	t5,0(t6)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test large Y register read with limited registers (uses IP_REG fallback)
large_y_reg_read_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate most available registers to simulate near-exhaustion (leave 1 for the y_reg helper)
    {State1, _} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, _} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Leave one register available so the y_reg helper can work, but it will need IP_REG fallback
    {StateFinal, ResultReg} = ?BACKEND:move_to_native_register(State5, {y_reg, 35}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses t0+t1 fallback sequence when temps are exhausted
    Dump = <<
        "   0:	0584bf83          	ld	t6,88(s1)\n"
        "   4:	0604bf03          	ld	t5,96(s1)\n"
        "   8:	0684be83          	ld	t4,104(s1)\n"
        "   c:	0704be03          	ld	t3,112(s1)\n"
        "  10:	0784b383          	ld	t2,120(s1)\n"
        "  14:	11800313          	li	t1,280\n"
        "  18:	9352                	add	t1,t1,s4\n"
        "  1a:	00033303          	ld	t1,0(t1)"
    >>,
    ?assertStream(riscv64, Dump, Stream),
    ?assertEqual(t1, ResultReg).

%% Test large Y register write with register exhaustion (uses t1/t0 fallback)
large_y_reg_write_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get a source register first
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    % Allocate most remaining registers to simulate exhaustion
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Try to write to large Y register when only one temp register is available
    StateFinal = ?BACKEND:move_to_vm_register(State5, SrcReg, {y_reg, 50}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses t1/t0 fallback sequence
    Dump = <<
        "   0:	0584bf83          	ld	t6,88(s1)\n"
        "   4:	0604bf03          	ld	t5,96(s1)\n"
        "   8:	0684be83          	ld	t4,104(s1)\n"
        "   c:	0704be03          	ld	t3,112(s1)\n"
        "  10:	0784b383          	ld	t2,120(s1)\n"
        "  14:	19000313          	li	t1,400\n"
        "  18:	9352                	add	t1,t1,s4\n"
        "  1a:	01f33023          	sd	t6,0(t1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test boundary case: Y=31 (124 bytes, exactly at limit, should use direct addressing)
y_reg_boundary_direct_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 31}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses direct addressing since 31 * 4 = 124 < 2048
    Dump = <<
        "   0:	0f8a3f83          	ld	t6,248(s4)"
    >>,
    ?assertStream(riscv64, Dump, Stream),
    ?assertEqual(t6, Reg).

%% Test debugger function
debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:\t9002                \tebreak"
    >>,
    ?assertStream(riscv64, Dump, Stream).

and_register_exhaustion_negative_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test negative immediate (-4) which should use NOT+AND with t0 as temp
    {StateResult, t6} = ?BACKEND:and_(StateNoRegs, {free, t6}, -4),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:	0584bf83          	ld	t6,88(s1)\n"
        "   4:	0604bf03          	ld	t5,96(s1)\n"
        "   8:	0684be83          	ld	t4,104(s1)\n"
        "   c:	0704be03          	ld	t3,112(s1)\n"
        "  10:	0784b383          	ld	t2,120(s1)\n"
        "  14:	0804b303          	ld	t1,128(s1)\n"
        "  18:	ffcfff93          	andi	t6,t6,-4"
    >>,
    ?assertStream(riscv64, ExpectedDump, Stream).

and_register_exhaustion_positive_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test positive immediate (0x3F) which should use AND with t0 as temp
    {StateResult, t6} = ?BACKEND:and_(StateNoRegs, {free, t6}, 16#3F),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:	0584bf83          	ld	t6,88(s1)\n"
        "   4:	0604bf03          	ld	t5,96(s1)\n"
        "   8:	0684be83          	ld	t4,104(s1)\n"
        "   c:	0704be03          	ld	t3,112(s1)\n"
        "  10:	0784b383          	ld	t2,120(s1)\n"
        "  14:	0804b303          	ld	t1,128(s1)\n"
        "  18:	03ffff93          	andi	t6,t6,63"
    >>,
    ?assertStream(riscv64, ExpectedDump, Stream).

jump_table_large_labels_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    % RISC-V: Each jump table entry is 8 bytes (AUIPC + JALR)
    ?assertEqual((512 + 1) * 8, byte_size(Stream)).

alloc_boxed_integer_fragment_small_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 42}
    ]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	07800f93          	li	t6,120\n"
            "   4:	9fce                	add	t6,t6,s3\n"
            "   6:	000fbf83          	ld	t6,0(t6)\n"
            "   a:	1141                	addi	sp,sp,-16\n"
            "   c:	e006                	sd	ra,0(sp)\n"
            "   e:	02a00513          	li	a0,42\n"
            "  12:	0544b823          	sd	s4,80(s1)\n"
            "  16:	9f82                	jalr	t6\n"
            "  18:	8faa                	mv	t6,a0\n"
            "  1a:	6082                	ld	ra,0(sp)\n"
            "  1c:	0141                	addi	sp,sp,16\n"
            "  1e:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

alloc_boxed_integer_fragment_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    % Add a call primitive last to emit literal pool
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADMATCH_ATOM, {free, ResultReg}
    ]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	07800f93          	li	t6,120\n"
            "   4:	9fce                	add	t6,t6,s3\n"
            "   6:	000fbf83          	ld	t6,0(t6)\n"
            "   a:	1141                	addi	sp,sp,-16\n"
            "   c:	e006                	sd	ra,0(sp)\n"
            "   e:	12345537          	lui	a0,0x12345\n"
            "  12:	6795051b          	addiw	a0,a0,1657 # 0x12345679\n"
            "  16:	0532                	slli	a0,a0,0xc\n"
            "  18:	9ac50513          	addi	a0,a0,-1620\n"
            "  1c:	0532                	slli	a0,a0,0xc\n"
            "  1e:	cde50513          	addi	a0,a0,-802\n"
            "  22:	0522                	slli	a0,a0,0x8\n"
            "  24:	0f050513          	addi	a0,a0,240\n"
            "  28:	0544b823          	sd	s4,80(s1)\n"
            "  2c:	9f82                	jalr	t6\n"
            "  2e:	8faa                	mv	t6,a0\n"
            "  30:	6082                	ld	ra,0(sp)\n"
            "  32:	0141                	addi	sp,sp,16\n"
            "  34:	0504ba03          	ld	s4,80(s1)\n"
            "  38:	09800f13          	li	t5,152\n"
            "  3c:	9f4e                	add	t5,t5,s3\n"
            "  3e:	000f3f03          	ld	t5,0(t5)\n"
            "  42:	04200513          	li	a0,66\n"
            "  46:	28b00593          	li	a1,651\n"
            "  4a:	867e                	mv	a2,t6\n"
            "  4c:	0544b823          	sd	s4,80(s1)\n"
            "  50:	8f02                	jr	t5"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test for stack alignment issue in call_func_ptr
%% RISC-V maintains 16-byte stack alignment (RISC-V calling convention)
call_func_ptr_stack_alignment_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _ResultReg} = ?BACKEND:call_func_ptr(State4, {free, t3}, [42]),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0604bf03          	ld	t5,96(s1)\n"
            "   8:	0684be83          	ld	t4,104(s1)\n"
            "   c:	0704be03          	ld	t3,112(s1)\n"
            "  10:	1101                	addi	sp,sp,-32\n"
            "  12:	e006                	sd	ra,0(sp)\n"
            "  14:	e47e                	sd	t6,8(sp)\n"
            "  16:	e87a                	sd	t5,16(sp)\n"
            "  18:	ec76                	sd	t4,24(sp)\n"
            "  1a:	02a00513          	li	a0,42\n"
            "  1e:	0544b823          	sd	s4,80(s1)\n"
            "  22:	9e02                	jalr	t3\n"
            "  24:	8e2a                	mv	t3,a0\n"
            "  26:	6082                	ld	ra,0(sp)\n"
            "  28:	6fa2                	ld	t6,8(sp)\n"
            "  2a:	6f42                	ld	t5,16(sp)\n"
            "  2c:	6ee2                	ld	t4,24(sp)\n"
            "  2e:	02010113          	addi	sp,sp,32\n"
            "  32:	0504ba03          	ld	s4,80(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% Test for register exhaustion issue in call_func_ptr with 5+ arguments
%% When all registers are used and we call a function with 5+ args,
%% set_args needs temporary registers but none are available
call_func_ptr_register_exhaustion_test_() ->
    {setup,
        fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

            % Allocate all available registers to simulate register pressure
            {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
            {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
            {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
            {State6, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
            State6
        end,
        fun(State6) ->
            [
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, 3, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	0584bf83          	ld	t6,88(s1)\n"
                            "   4:	0604bf03          	ld	t5,96(s1)\n"
                            "   8:	0684be83          	ld	t4,104(s1)\n"
                            "   c:	0704be03          	ld	t3,112(s1)\n"
                            "  10:	0784b383          	ld	t2,120(s1)\n"
                            "  14:	0804b303          	ld	t1,128(s1)\n"
                            "  18:	fd010113          	addi	sp,sp,-48\n"
                            "  1c:	e006                	sd	ra,0(sp)\n"
                            "  1e:	e47e                	sd	t6,8(sp)\n"
                            "  20:	e876                	sd	t4,16(sp)\n"
                            "  22:	ec72                	sd	t3,24(sp)\n"
                            "  24:	f01a                	sd	t1,32(sp)\n"
                            "  26:	8526                	mv	a0,s1\n"
                            "  28:	85ca                	mv	a1,s2\n"
                            "  2a:	861e                	mv	a2,t2\n"
                            "  2c:	468d                	li	a3,3\n"
                            "  2e:	4705                	li	a4,1\n"
                            "  30:	0544b823          	sd	s4,80(s1)\n"
                            "  34:	9f02                	jalr	t5\n"
                            "  36:	8f2a                	mv	t5,a0\n"
                            "  38:	6082                	ld	ra,0(sp)\n"
                            "  3a:	6fa2                	ld	t6,8(sp)\n"
                            "  3c:	6ec2                	ld	t4,16(sp)\n"
                            "  3e:	6e62                	ld	t3,24(sp)\n"
                            "  40:	7302                	ld	t1,32(sp)\n"
                            "  42:	03010113          	addi	sp,sp,48\n"
                            "  46:	0504ba03          	ld	s4,80(s1)"
                        >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, 1, t1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	0584bf83          	ld	t6,88(s1)\n"
                            "   4:	0604bf03          	ld	t5,96(s1)\n"
                            "   8:	0684be83          	ld	t4,104(s1)\n"
                            "   c:	0704be03          	ld	t3,112(s1)\n"
                            "  10:	0784b383          	ld	t2,120(s1)\n"
                            "  14:	0804b303          	ld	t1,128(s1)\n"
                            "  18:	fd010113          	addi	sp,sp,-48\n"
                            "  1c:	e006                	sd	ra,0(sp)\n"
                            "  1e:	e47e                	sd	t6,8(sp)\n"
                            "  20:	e876                	sd	t4,16(sp)\n"
                            "  22:	ec72                	sd	t3,24(sp)\n"
                            "  24:	f01a                	sd	t1,32(sp)\n"
                            "  26:	8526                	mv	a0,s1\n"
                            "  28:	85ca                	mv	a1,s2\n"
                            "  2a:	861e                	mv	a2,t2\n"
                            "  2c:	4685                	li	a3,1\n"
                            "  2e:	871a                	mv	a4,t1\n"
                            "  30:	0544b823          	sd	s4,80(s1)\n"
                            "  34:	9f02                	jalr	t5\n"
                            "  36:	8f2a                	mv	t5,a0\n"
                            "  38:	6082                	ld	ra,0(sp)\n"
                            "  3a:	6fa2                	ld	t6,8(sp)\n"
                            "  3c:	6ec2                	ld	t4,16(sp)\n"
                            "  3e:	6e62                	ld	t3,24(sp)\n"
                            "  40:	7302                	ld	t1,32(sp)\n"
                            "  42:	03010113          	addi	sp,sp,48\n"
                            "  46:	0504ba03          	ld	s4,80(s1)"
                        >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, t1, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	0584bf83          	ld	t6,88(s1)\n"
                            "   4:	0604bf03          	ld	t5,96(s1)\n"
                            "   8:	0684be83          	ld	t4,104(s1)\n"
                            "   c:	0704be03          	ld	t3,112(s1)\n"
                            "  10:	0784b383          	ld	t2,120(s1)\n"
                            "  14:	0804b303          	ld	t1,128(s1)\n"
                            "  18:	fd010113          	addi	sp,sp,-48\n"
                            "  1c:	e006                	sd	ra,0(sp)\n"
                            "  1e:	e47e                	sd	t6,8(sp)\n"
                            "  20:	e876                	sd	t4,16(sp)\n"
                            "  22:	ec72                	sd	t3,24(sp)\n"
                            "  24:	f01a                	sd	t1,32(sp)\n"
                            "  26:	8526                	mv	a0,s1\n"
                            "  28:	85ca                	mv	a1,s2\n"
                            "  2a:	861e                	mv	a2,t2\n"
                            "  2c:	869a                	mv	a3,t1\n"
                            "  2e:	4705                	li	a4,1\n"
                            "  30:	0544b823          	sd	s4,80(s1)\n"
                            "  34:	9f02                	jalr	t5\n"
                            "  36:	8f2a                	mv	t5,a0\n"
                            "  38:	6082                	ld	ra,0(sp)\n"
                            "  3a:	6fa2                	ld	t6,8(sp)\n"
                            "  3c:	6ec2                	ld	t4,16(sp)\n"
                            "  3e:	6e62                	ld	t3,24(sp)\n"
                            "  40:	7302                	ld	t1,32(sp)\n"
                            "  42:	03010113          	addi	sp,sp,48\n"
                            "  46:	0504ba03          	ld	s4,80(s1)"
                        >>,
                    ?assertStream(riscv64, Dump, Stream),
                    ?assertEqual(t5, ResultReg)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, a1},
                        [t5, a3]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	0584bf83          	ld	t6,88(s1)\n"
                            "   4:	0604bf03          	ld	t5,96(s1)\n"
                            "   8:	0684be83          	ld	t4,104(s1)\n"
                            "   c:	0704be03          	ld	t3,112(s1)\n"
                            "  10:	0784b383          	ld	t2,120(s1)\n"
                            "  14:	0804b303          	ld	t1,128(s1)\n"
                            "  18:	fc010113          	addi	sp,sp,-64\n"
                            "  1c:	e006                	sd	ra,0(sp)\n"
                            "  1e:	e47e                	sd	t6,8(sp)\n"
                            "  20:	e87a                	sd	t5,16(sp)\n"
                            "  22:	ec76                	sd	t4,24(sp)\n"
                            "  24:	f072                	sd	t3,32(sp)\n"
                            "  26:	f41e                	sd	t2,40(sp)\n"
                            "  28:	f81a                	sd	t1,48(sp)\n"
                            "  2a:	8fae                	mv	t6,a1\n"
                            "  2c:	857a                	mv	a0,t5\n"
                            "  2e:	85b6                	mv	a1,a3\n"
                            "  30:	0544b823          	sd	s4,80(s1)\n"
                            "  34:	9f82                	jalr	t6\n"
                            "  36:	85aa                	mv	a1,a0\n"
                            "  38:	6082                	ld	ra,0(sp)\n"
                            "  3a:	6fa2                	ld	t6,8(sp)\n"
                            "  3c:	6f42                	ld	t5,16(sp)\n"
                            "  3e:	6ee2                	ld	t4,24(sp)\n"
                            "  40:	7e02                	ld	t3,32(sp)\n"
                            "  42:	73a2                	ld	t2,40(sp)\n"
                            "  44:	7342                	ld	t1,48(sp)\n"
                            "  46:	04010113          	addi	sp,sp,64\n"
                            "  4a:	0504ba03          	ld	s4,80(s1)"
                        >>,
                    ?assertStream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {primitive, 2},
                        [{free, t5}, a3]
                    ),
                    ?assertEqual(ResultReg, t5),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:	0584bf83          	ld	t6,88(s1)\n"
                            "   4:	0604bf03          	ld	t5,96(s1)\n"
                            "   8:	0684be83          	ld	t4,104(s1)\n"
                            "   c:	0704be03          	ld	t3,112(s1)\n"
                            "  10:	0784b383          	ld	t2,120(s1)\n"
                            "  14:	0804b303          	ld	t1,128(s1)\n"
                            "  18:	fd010113          	addi	sp,sp,-48\n"
                            "  1c:	e006                	sd	ra,0(sp)\n"
                            "  1e:	e47e                	sd	t6,8(sp)\n"
                            "  20:	e876                	sd	t4,16(sp)\n"
                            "  22:	ec72                	sd	t3,24(sp)\n"
                            "  24:	f01e                	sd	t2,32(sp)\n"
                            "  26:	f41a                	sd	t1,40(sp)\n"
                            "  28:	4fc1                	li	t6,16\n"
                            "  2a:	9fce                	add	t6,t6,s3\n"
                            "  2c:	000fbf83          	ld	t6,0(t6)\n"
                            "  30:	857a                	mv	a0,t5\n"
                            "  32:	85b6                	mv	a1,a3\n"
                            "  34:	0544b823          	sd	s4,80(s1)\n"
                            "  38:	9f82                	jalr	t6\n"
                            "  3a:	8f2a                	mv	t5,a0\n"
                            "  3c:	6082                	ld	ra,0(sp)\n"
                            "  3e:	6fa2                	ld	t6,8(sp)\n"
                            "  40:	6ec2                	ld	t4,16(sp)\n"
                            "  42:	6e62                	ld	t3,24(sp)\n"
                            "  44:	7382                	ld	t2,32(sp)\n"
                            "  46:	7322                	ld	t1,40(sp)\n"
                            "  48:	03010113          	addi	sp,sp,48\n"
                            "  4c:	0504ba03          	ld	s4,80(s1)"
                        >>,
                    ?assertStream(riscv64, Dump, Stream)
                end)
            ]
        end}.

%% Test jump_to_continuation optimization for intra-module returns
jump_to_continuation_test_() ->
    [
        ?_test(begin
            % Test 1: jump_to_continuation at offset 0
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            State1 = ?BACKEND:jump_to_continuation(State0, {free, a0}),
            Stream = ?BACKEND:stream(State1),
            % Expected: riscv64 PIC sequence
            Dump =
                <<
                    "   0:  00000f97            auipc   t6,0x0\n"
                    "   4:  9faa                add t6,t6,a0\n"
                    "   6:  8f82                jr  t6"
                >>,
            ?assertStream(riscv64, Dump, Stream)
        end),
        ?_test(begin
            % Test 2: jump_to_continuation after jump table (non-zero relative address)
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            % Generate a jump table for 3 labels (4 entries * 8 bytes = 32 bytes)
            State1 = ?BACKEND:jump_table(State0, 3),
            State2 = ?BACKEND:jump_to_continuation(State1, {free, a0}),
            Stream = ?BACKEND:stream(State2),
            % Expected: jump table (32 bytes) + jump_to_continuation
            % NetOffset = 0 - 32 = -32 (0xFFFFFFE0)
            Dump =
                <<
                    "   0:  ffffffff            .insn   4, 0xffffffff\n"
                    "   4:  ffffffff            .insn   4, 0xffffffff\n"
                    "   8:  ffffffff            .insn   4, 0xffffffff\n"
                    "   c:  ffffffff            .insn   4, 0xffffffff\n"
                    "  10:  ffffffff            .insn   4, 0xffffffff\n"
                    "  14:  ffffffff            .insn   4, 0xffffffff\n"
                    "  18:  ffffffff            .insn   4, 0xffffffff\n"
                    "  1c:  ffffffff            .insn   4, 0xffffffff\n"
                    "  20:  00000f97            auipc   t6,0x0\n"
                    "  24:  1f81                addi    t6,t6,-32 # 0x0\n"
                    "  26:  9faa                add t6,t6,a0\n"
                    "  28:  8f82                jr  t6"
                >>,
            ?assertStream(riscv64, Dump, Stream)
        end)
    ].

%% Mimic part of add.beam
add_beam_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:move_to_vm_register(State2, 16#9f, {x_reg, 1}),
    State4 = ?BACKEND:move_to_vm_register(State3, 16#8f, {x_reg, 0}),
    State5 = ?BACKEND:call_only_or_schedule_next(State4, 2),
    State6 = ?BACKEND:add_label(State5, 2),
    {State7, ResultReg} = ?BACKEND:call_primitive(State6, ?PRIM_ALLOCATE, [
        ctx, jit_state, 1, 0, 1
    ]),
    State8 = ?BACKEND:if_block(State7, {'(bool)', {free, ResultReg}, '==', false}, fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    State9 = ?BACKEND:move_to_vm_register(State8, ?TERM_NIL, {y_reg, 0}),
    State10 = ?BACKEND:call_or_schedule_next(State9, 3),
    State11 = ?BACKEND:add_label(State10, 3),
    State12 = ?BACKEND:call_primitive_last(State11, ?PRIM_RETURN, [
        ctx, jit_state
    ]),
    % OP_INT_CALL_END
    State13 = ?BACKEND:add_label(State12, 0),
    State14 = ?BACKEND:call_primitive_last(State13, 1, [ctx, jit_state]),
    State15 = ?BACKEND:update_branches(State14),
    Stream = ?BACKEND:stream(State15),
    Dump =
        <<
            "   0:	00000697          	auipc	a3,0x0\n"
            "   4:	10068067          	jr	256(a3) # 0x100\n"
            "   8:	00000697          	auipc	a3,0x0\n"
            "   c:	01868067          	jr	24(a3) # 0x20\n"
            "  10:	00000697          	auipc	a3,0x0\n"
            "  14:	05068067          	jr	80(a3) # 0x60\n"
            "  18:	00000697          	auipc	a3,0x0\n"
            "  1c:	0da68067          	jr	218(a3) # 0xf2\n"
            "  20:	09f00f93          	li	t6,159\n"
            "  24:	07f4b023          	sd	t6,96(s1)\n"
            "  28:	08f00f93          	li	t6,143\n"
            "  2c:	05f4bc23          	sd	t6,88(s1)\n"
            "  30:	01092f83          	lw	t6,16(s2)\n"
            "  34:	1ffd                	addi	t6,t6,-1\n"
            "  36:	01f92823          	sw	t6,16(s2)\n"
            "  3a:	000f8663          	beqz	t6,0x46\n"
            "  3e:	a00d                	j	0x60\n"
            "  40:	0001                	nop\n"
            "  42:	00000013          	nop\n"
            "  46:	00000f97          	auipc	t6,0x0\n"
            "  4a:	0fe9                	addi	t6,t6,26 # 0x60\n"
            "  4c:	0001                	nop\n"
            "  4e:	01f93423          	sd	t6,8(s2)\n"
            "  52:	4fc1                	li	t6,16\n"
            "  54:	9fce                	add	t6,t6,s3\n"
            "  56:	000fbf83          	ld	t6,0(t6)\n"
            "  5a:	0544b823          	sd	s4,80(s1)\n"
            "  5e:	8f82                	jr	t6\n"
            "  60:	02800f93          	li	t6,40\n"
            "  64:	9fce                	add	t6,t6,s3\n"
            "  66:	000fbf83          	ld	t6,0(t6)\n"
            "  6a:	1141                	addi	sp,sp,-16\n"
            "  6c:	e006                	sd	ra,0(sp)\n"
            "  6e:	4505                	li	a0,1\n"
            "  70:	4581                	li	a1,0\n"
            "  72:	4605                	li	a2,1\n"
            "  74:	0544b823          	sd	s4,80(s1)\n"
            "  78:	9f82                	jalr	t6\n"
            "  7a:	8faa                	mv	t6,a0\n"
            "  7c:	6082                	ld	ra,0(sp)\n"
            "  7e:	0141                	addi	sp,sp,16\n"
            "  80:	0504ba03          	ld	s4,80(s1)\n"
            "  84:	03ff9f13          	slli	t5,t6,0x3f\n"
            "  88:	000f4c63          	bltz	t5,0xa0\n"
            "  8c:	03000f93          	li	t6,48\n"
            "  90:	9fce                	add	t6,t6,s3\n"
            "  92:	000fbf83          	ld	t6,0(t6)\n"
            "  96:	09600513          	li	a0,150\n"
            "  9a:	0544b823          	sd	s4,80(s1)\n"
            "  9e:	8f82                	jr	t6\n"
            "  a0:	03b00f13          	li	t5,59\n"
            "  a4:	01ea3023          	sd	t5,0(s4)\n"
            "  a8:	00093f03          	ld	t5,0(s2)\n"
            "  ac:	000f2f03          	lw	t5,0(t5)\n"
            "  b0:	0f62                	slli	t5,t5,0x18\n"
            "  b2:	3c800f93          	li	t6,968\n"
            "  b6:	00000013          	nop\n"
            "  ba:	01ff6f33          	or	t5,t5,t6\n"
            "  be:	0fe4b023          	sd	t5,224(s1)\n"
            "  c2:	01092f83          	lw	t6,16(s2)\n"
            "  c6:	1ffd                	addi	t6,t6,-1\n"
            "  c8:	01f92823          	sw	t6,16(s2)\n"
            "  cc:	000f8663          	beqz	t6,0xd8\n"
            "  d0:	a00d                	j	0xf2\n"
            "  d2:	0001                	nop\n"
            "  d4:	00000013          	nop\n"
            "  d8:	00000f97          	auipc	t6,0x0\n"
            "  dc:	0fe9                	addi	t6,t6,26 # 0xf2\n"
            "  de:	0001                	nop\n"
            "  e0:	01f93423          	sd	t6,8(s2)\n"
            "  e4:	4fc1                	li	t6,16\n"
            "  e6:	9fce                	add	t6,t6,s3\n"
            "  e8:	000fbf83          	ld	t6,0(t6)\n"
            "  ec:	0544b823          	sd	s4,80(s1)\n"
            "  f0:	8f82                	jr	t6\n"
            "  f2:	4fa1                	li	t6,8\n"
            "  f4:	9fce                	add	t6,t6,s3\n"
            "  f6:	000fbf83          	ld	t6,0(t6)\n"
            "  fa:	0544b823          	sd	s4,80(s1)\n"
            "  fe:	8f82                	jr	t6\n"
            " 100:	4fa1                	li	t6,8\n"
            " 102:	9fce                	add	t6,t6,s3\n"
            " 104:	000fbf83          	ld	t6,0(t6)\n"
            " 108:	0544b823          	sd	s4,80(s1)\n"
            " 10c:	8f82                	jr	t6"
        >>,
    ?assertStream(riscv64, Dump, Stream).

and_positive_imm_invalidates_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [t5]),
    % and_ with 0x3F00 (> 2047) picks t5 as Temp, loads mask into it
    {State4, t6} = ?BACKEND:and_(State3, {free, t6}, 16#3F00),
    % This must emit a ld to reload x_reg 1, not use stale cache
    {State5, t5} = ?BACKEND:move_to_native_register(State4, {x_reg, 1}),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0604bf03          	ld	t5,96(s1)\n"
            "   8:	6f11                	lui	t5,0x4\n"
            "   a:	f00f0f1b          	addiw	t5,t5,-256 # 0x3f00\n"
            "   e:	01efffb3          	and	t6,t6,t5\n"
            "  12:	0604bf03          	ld	t5,96(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

fixed_dst_x_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, t6),
    Offset1 = ?BACKEND:offset(State1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 2}),
    ?assertEqual(t6, Reg),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0684bf83          	ld	t6,104(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

fixed_dst_y_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, t5),
    Offset1 = ?BACKEND:offset(State1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {y_reg, 2}),
    ?assertEqual(t5, Reg),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	010a3f03          	ld	t5,16(s4)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

if_block_cond_free_reg_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(
        State2,
        {{free, t6}, '&', 16#F, '!=', 16#F},
        fun(BSt0) -> ?BACKEND:add(BSt0, t5, 2) end
    ),
    {State4, t6} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0604bf03          	ld	t5,96(s1)\n"
            "   8:	ffffcf93          	not	t6,t6\n"
            "   c:	1ff2                	slli	t6,t6,0x3c\n"
            "   e:	000f8363          	beqz	t6,0x14\n"
            "  12:	0f09                	addi	t5,t5,2\n"
            "  14:	0584bf83          	ld	t6,88(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

jump_to_label_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [t6]),
    State3 = ?BACKEND:jump_to_label(State2, 42),
    {State4, t6} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	ffff                	.insn	2, 0xffff\n"
            "   6:	ffff                	.insn	2, 0xffff\n"
            "   8:	ffff                	.insn	2, 0xffff\n"
            "   a:	ffff                	.insn	2, 0xffff\n"
            "   c:	0584bf83          	ld	t6,88(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

unreachable_test_state() ->
    ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)).

setup_cached_x_reg0(State0) ->
    {State1, CondReg} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, CachedReg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    {?BACKEND:free_native_registers(State2, [CachedReg]), CondReg}.

setup_cached_x_reg0_with_offset(State0) ->
    {State1, OffsetReg} = ?BACKEND:move_to_native_register(State0, 16#100),
    {State2, CondReg} = ?BACKEND:move_to_native_register(State1, 1),
    {State3, CachedReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {?BACKEND:free_native_registers(State3, [CachedReg]), CondReg, OffsetReg, CachedReg}.

terminal_if_preserves_cached_x_reg0(State0, TerminalFun) ->
    {State1, CondReg} = setup_cached_x_reg0(State0),
    State2 = ?BACKEND:if_block(State1, {{free, CondReg}, '==', 0}, TerminalFun),
    {State3, _} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State3.

call_primitive_last_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, 0, [ctx, jit_state])
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	4f85                	li	t6,1\n"
        "   2:	0584bf03          	ld	t5,88(s1)\n"
        "   6:	000f9763          	bnez	t6,0x14\n"
        "   a:	0009bf83          	ld	t6,0(s3)\n"
        "   e:	0544b823          	sd	s4,80(s1)\n"
        "  12:	8f82                	jr	t6"
    >>,
    ?assertStream(riscv64, Dump, Stream).

jump_to_label_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_label(BSt0, 42)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	4f85                	li	t6,1\n"
        "   2:	0584bf03          	ld	t5,88(s1)\n"
        "   6:	000f9663          	bnez	t6,0x12\n"
        "   a:	ffff                	.insn	2, 0xffff\n"
        "   c:	ffff                	.insn	2, 0xffff\n"
        "   e:	ffff                	.insn	2, 0xffff\n"
        "  10:	ffff                	.insn	2, 0xffff"
    >>,
    ?assertStream(riscv64, Dump, Stream).

jump_to_offset_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_offset(BSt0, 16#100)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	4f85                	li	t6,1\n"
        "   2:	0584bf03          	ld	t5,88(s1)\n"
        "   6:	000f9363          	bnez	t6,0xc\n"
        "   a:	a8dd                	j	0x100"
    >>,
    ?assertStream(riscv64, Dump, Stream).

jump_to_continuation_if_block_preserves_cache_test() ->
    State0 = unreachable_test_state(),
    {State1, CondReg, OffsetReg, CachedReg} = setup_cached_x_reg0_with_offset(State0),
    State2 = ?BACKEND:if_block(State1, {{free, CondReg}, '==', 0}, fun(BSt0) ->
        ?BACKEND:jump_to_continuation(BSt0, {free, OffsetReg})
    end),
    Offset2 = ?BACKEND:offset(State2),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    ?assertEqual(CachedReg, Reg),
    Offset3 = ?BACKEND:offset(State3),
    ?assertEqual(Offset2, Offset3),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	10000f93          	li	t6,256\n"
        "   4:	4f05                	li	t5,1\n"
        "   6:	0584be83          	ld	t4,88(s1)\n"
        "   a:	000f1763          	bnez	t5,0x18\n"
        "   e:	00000f17          	auipc	t5,0x0\n"
        "  12:	1f49                	addi	t5,t5,-14 # 0x0\n"
        "  14:	9f7e                	add	t5,t5,t6\n"
        "  16:	8f02                	jr	t5"
    >>,
    ?assertStream(riscv64, Dump, Stream).

move_array_element_x_reg_invalidates_vm_loc_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:move_array_element(State2, t5, 0, {x_reg, 5}),
    {State4, _Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 5}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	0804bf83          	ld	t6,128(s1)\n"
        "   4:	0584bf03          	ld	t5,88(s1)\n"
        "   8:	000f3e83          	ld	t4,0(t5)\n"
        "   c:	09d4b023          	sd	t4,128(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

ldr_y_reg_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [t5, t4]),
    {State5, t5} = ?BACKEND:move_to_native_register(State4, {y_reg, 0}),
    {State6, t4} = ?BACKEND:move_to_native_register(State5, {x_reg, 2}),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0604bf03          	ld	t5,96(s1)\n"
            "   8:	0684be83          	ld	t4,104(s1)\n"
            "   c:	000a3f03          	ld	t5,0(s4)\n"
            "  10:	0684be83          	ld	t4,104(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

y_reg_load_last_available_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    {State7, t0} = ?BACKEND:move_to_native_register(State6, {y_reg, 0}),
    Stream = ?BACKEND:stream(State7),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)\n"
            "   4:	0604bf03          	ld	t5,96(s1)\n"
            "   8:	0684be83          	ld	t4,104(s1)\n"
            "   c:	0704be03          	ld	t3,112(s1)\n"
            "  10:	0784b383          	ld	t2,120(s1)\n"
            "  14:	0804b303          	ld	t1,128(s1)\n"
            "  18:	000a3283          	ld	t0,0(s4)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

%% After freeing a register, cache is preserved so reload is elided
cached_load_after_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [t6]),
    {State3, t6} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	0584bf83          	ld	t6,88(s1)"
        >>,
    ?assertStream(riscv64, Dump, Stream).

cached_move_to_vm_x_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {x_reg, 1}, {x_reg, 0}),
    Offset1 = ?BACKEND:offset(State1),
    {State2, t6} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	0604bf83          	ld	t6,96(s1)\n"
        "   4:	05f4bc23          	sd	t6,88(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

cached_move_to_vm_y_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {y_reg, 0}, {x_reg, 0}),
    Offset1 = ?BACKEND:offset(State1),
    {State2, t6} = ?BACKEND:move_to_native_register(State1, {y_reg, 0}),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	000a3f83          	ld	t6,0(s4)\n"
        "   4:	05f4bc23          	sd	t6,88(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).

cached_move_to_vm_imm_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {x_reg, 0}),
    Offset1 = ?BACKEND:offset(State1),
    {State2, t6} = ?BACKEND:move_to_native_register(State1, 42),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	02a00f93          	li	t6,42\n"
        "   4:	05f4bc23          	sd	t6,88(s1)"
    >>,
    ?assertStream(riscv64, Dump, Stream).
