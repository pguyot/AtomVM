%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_arm32_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_arm32).

% disassembly obtained with:
%  arm-elf-objdump -D -b binary -marm -z

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_test() ->
    State = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual([], ?BACKEND:used_regs(State)),
    Available = ?BACKEND:available_regs(State),
    ?assertEqual(7, length(Available)),
    ?assert(lists:member(r0, Available)),
    ?assert(lists:member(r1, Available)),
    ?assert(lists:member(r2, Available)),
    ?assert(lists:member(r3, Available)),
    ?assert(lists:member(r4, Available)),
    ?assert(lists:member(r5, Available)),
    ?assert(lists:member(r6, Available)).

add_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:add_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
            "   8:	e0966005 	adds	r6, r6, r5"
        >>,
    ?assertStream(arm32, Dump, Stream).

sub_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:sub_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
            "   8:	e0566005 	subs	r6, r6, r5"
        >>,
    ?assertStream(arm32, Dump, Stream).

mul_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:mul_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
            "   8:	e3c6600f 	bic	r6, r6, #15\n"
            "   c:	e1a05245 	asr	r5, r5, #4\n"
            "  10:	e0c43596 	smull	r3, r4, r6, r5\n"
            "  14:	e1a02fc3 	asr	r2, r3, #31\n"
            "  18:	e1540002 	cmp	r4, r2\n"
            "  1c:	e1a06003 	mov	r6, r3"
        >>,
    ?assertStream(arm32, Dump, Stream).

if_block_overflow_set_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:add_overflow(State2, RegA, RegB),
    State4 = ?BACKEND:if_block(State3, overflow_set, fun(BSt0) ->
        ?BACKEND:move_to_vm_register(BSt0, RegA, {x_reg, 2})
    end),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
            "   8:	e0966005 	adds	r6, r6, r5\n"
            "   c:	7a000000 	bvc	0x14\n"
            "  10:	e5876034 	str	r6, [r7, #52]	@ 0x34"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assert(is_atom(ResultReg)),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e5996000 	ldr	r6, [r9]\n"
            "   4:	e92d4040 	push	{r6, lr}\n"
            "   8:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "   c:	e12fff36 	blx	r6\n"
            "  10:	e1a05000 	mov	r5, r0\n"
            "  14:	e8bd4040 	pop	{r6, lr}\n"
            "  18:	e5978028 	ldr	r8, [r7, #40]	@ 0x28"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assert(is_atom(ResultReg)),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e5996004 	ldr	r6, [r9, #4]\n"
            "   4:	e92d4040 	push	{r6, lr}\n"
            "   8:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "   c:	e12fff36 	blx	r6\n"
            "  10:	e1a05000 	mov	r5, r0\n"
            "  14:	e8bd4040 	pop	{r6, lr}\n"
            "  18:	e5978028 	ldr	r8, [r7, #40]	@ 0x28"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e5996014 	ldr	r6, [r9, #20]\n"
            "   4:	e3a00010 	mov	r0, #16\n"
            "   8:	e3a01020 	mov	r1, #32\n"
            "   c:	e3a02002 	mov	r2, #2\n"
            "  10:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  14:	e12fff16 	bx	r6"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_primitive_6_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:and_(State1, {free, RegA}, ?TERM_PRIMARY_CLEAR_MASK),
    {State3, OtherReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 1}),
    {State4, _ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, RegA}, 64, 8, {free, OtherReg}
    ]),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e3a05003 	mov	r5, #3\n"
            "   8:	e1c66005 	bic	r6, r6, r5\n"
            "   c:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
            "  10:	e59940b8 	ldr	r4, [r9, #184]	@ 0xb8\n"
            "  14:	e92d4010 	push	{r4, lr}\n"
            "  18:	e1a00006 	mov	r0, r6\n"
            "  1c:	e3a01040 	mov	r1, #64	@ 0x40\n"
            "  20:	e3a02008 	mov	r2, #8\n"
            "  24:	e1a03005 	mov	r3, r5\n"
            "  28:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  2c:	e12fff34 	blx	r4\n"
            "  30:	e1a06000 	mov	r6, r0\n"
            "  34:	e8bd4010 	pop	{r4, lr}\n"
            "  38:	e5978028 	ldr	r8, [r7, #40]	@ 0x28"
        >>,
    ?assertStream(arm32, Dump, Stream).

move_to_vm_register_x_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5876030 	str	r6, [r7, #48]	@ 0x30"
        >>,
    ?assertStream(arm32, Dump, Stream).

move_to_vm_register_y_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {y_reg, 0}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5886000 	str	r6, [r8]"
        >>,
    ?assertStream(arm32, Dump, Stream).

jump_table_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    Stream = ?BACKEND:stream(State1),
    % 4 entries (0..3) * 8 bytes each = 32 bytes
    ?assertEqual(32, byte_size(Stream)).

add_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2),
    ?assertEqual(32, ?BACKEND:offset(State3)).

and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:and_(State1, {free, Reg}, 16#FC),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e3a050fc 	mov	r5, #252	@ 0xfc\n"
            "   8:	e0066005 	and	r6, r6, r5"
        >>,
    ?assertStream(arm32, Dump, Stream).

or_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:or_(State1, Reg, 16#0F),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e3a0500f 	mov	r5, #15\n"
            "   8:	e1866005 	orr	r6, r6, r5"
        >>,
    ?assertStream(arm32, Dump, Stream).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 2),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e1a06106 	lsl	r6, r6, #2"
        >>,
    ?assertStream(arm32, Dump, Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 4),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a06226 	lsr	r6, r6, #4"
                >>,
            ?assertStream(arm32, Dump, Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 4),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05226 	lsr	r5, r6, #4"
                >>,
            ?assertStream(arm32, Dump, Stream)
        end)
    ].

add_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:add(State1, Reg, 42),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e286602a 	add	r6, r6, #42	@ 0x2a"
        >>,
    ?assertStream(arm32, Dump, Stream).

sub_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:sub(State1, Reg, 42),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e246602a 	sub	r6, r6, #42	@ 0x2a"
        >>,
    ?assertStream(arm32, Dump, Stream).

decrement_reductions_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e320f000 	nop	{0}\n"
            "   4:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
            "   8:	e320f000 	nop	{0}\n"
            "   c:	ea000001 	b	0x18\n"
            "  10:	e320f000 	nop	{0}\n"
            "  14:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
            "  18:	e1a0500a 	mov	r5, sl\n"
            "  1c:	e5956008 	ldr	r6, [r5, #8]\n"
            "  20:	e2566001 	subs	r6, r6, #1\n"
            "  24:	e5856008 	str	r6, [r5, #8]\n"
            "  28:	1a000004 	bne	0x40\n"
            "  2c:	e28f600c 	add	r6, pc, #12\n"
            "  30:	e5856004 	str	r6, [r5, #4]\n"
            "  34:	e5996008 	ldr	r6, [r9, #8]\n"
            "  38:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  3c:	e12fff16 	bx	r6"
        >>,
    ?assertStream(arm32, Dump, Stream).

debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    % BKPT is a single 4-byte ARM instruction
    ?assertEqual(4, byte_size(Stream)).

flush_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:flush(State0),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(0, byte_size(Stream)).

xor_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:xor_(State1, Reg, 16#FF),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e3a050ff 	mov	r5, #255	@ 0xff\n"
            "   8:	e0266005 	eor	r6, r6, r5"
        >>,
    ?assertStream(arm32, Dump, Stream).

mul_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:mul(State1, Reg, 4),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e1a06106 	lsl	r6, r6, #2"
        >>,
    ?assertStream(arm32, Dump, Stream).

return_if_not_equal_to_ctx_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [ctx, jit_state]
                    ),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	e5996054 	ldr	r6, [r9, #84]	@ 0x54\n"
                            "   4:	e92d4040 	push	{r6, lr}\n"
                            "   8:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
                            "   c:	e12fff36 	blx	r6\n"
                            "  10:	e1a05000 	mov	r5, r0\n"
                            "  14:	e8bd4040 	pop	{r6, lr}\n"
                            "  18:	e5978028 	ldr	r8, [r7, #40]	@ 0x28\n"
                            "  1c:	e1550007 	cmp	r5, r7\n"
                            "  20:	0a000001 	beq	0x2c\n"
                            "  24:	e1a00005 	mov	r0, r5\n"
                            "  28:	e12fff1e 	bx	lr"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [ctx, jit_state]
                    ),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	e5996054 	ldr	r6, [r9, #84]	@ 0x54\n"
                            "   4:	e92d4040 	push	{r6, lr}\n"
                            "   8:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
                            "   c:	e12fff36 	blx	r6\n"
                            "  10:	e1a05000 	mov	r5, r0\n"
                            "  14:	e8bd4040 	pop	{r6, lr}\n"
                            "  18:	e5978028 	ldr	r8, [r7, #40]	@ 0x28\n"
                            "  1c:	e1a06005 	mov	r6, r5\n"
                            "  20:	e1560007 	cmp	r6, r7\n"
                            "  24:	0a000001 	beq	0x30\n"
                            "  28:	e1a00006 	mov	r0, r6\n"
                            "  2c:	e12fff1e 	bx	lr"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e5986000 	ldr	r6, [r8]\n"
            "   4:	e5876070 	str	r6, [r7, #112]	@ 0x70\n"
            "   8:	e5986004 	ldr	r6, [r8, #4]\n"
            "   c:	e5876074 	str	r6, [r7, #116]	@ 0x74"
        >>,
    ?assertStream(arm32, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e288801c 	add	r8, r8, #28"
        >>,
    ?assertStream(arm32, Dump, Stream).

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
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3560000 	cmp	r6, #0\n"
                            "   c:	5a000000 	bpl	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', RegB},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e1560005 	cmp	r6, r5\n"
                            "   c:	aa000000 	bge	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 42},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e356002a 	cmp	r6, #42	@ 0x2a\n"
                            "   c:	aa000000 	bge	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 1024},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3a04b01 	mov	r4, #1024	@ 0x400\n"
                            "   c:	e1560004 	cmp	r6, r4\n"
                            "  10:	aa000000 	bge	0x18\n"
                            "  14:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3560000 	cmp	r6, #0\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3560000 	cmp	r6, #0\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3560000 	cmp	r6, #0\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', ?TERM_NIL},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e356003b 	cmp	r6, #59	@ 0x3b\n"
                            "   c:	0a000000 	beq	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '==', false},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3160001 	tst	r6, #1\n"
                            "   c:	1a000000 	bne	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '!=', false},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3160001 	tst	r6, #1\n"
                            "   c:	0a000000 	beq	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#F, '!=', 0},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e316000f 	tst	r6, #15\n"
                            "   c:	0a000000 	beq	0x14\n"
                            "  10:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'and', [{RegA, '<', 0}, {RegB, '==', 0}]},
                        fun(BSt0) -> ?BACKEND:add(BSt0, RegB, 2) end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
                            "   8:	e3560000 	cmp	r6, #0\n"
                            "   c:	5a000002 	bpl	0x1c\n"
                            "  10:	e3550000 	cmp	r5, #0\n"
                            "  14:	1a000000 	bne	0x1c\n"
                            "  18:	e2855002 	add	r5, r5, #2"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
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
        fun(BSt0) -> ?BACKEND:add(BSt0, Reg2, 2) end,
        fun(BSt0) -> ?BACKEND:add(BSt0, Reg2, 4) end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
            "   8:	e356003b 	cmp	r6, #59	@ 0x3b\n"
            "   c:	1a000001 	bne	0x18\n"
            "  10:	e2855002 	add	r5, r5, #2\n"
            "  14:	ea000000 	b	0x1c\n"
            "  18:	e2855004 	add	r5, r5, #4"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	e1a0500a 	mov	r5, sl\n"
            "   4:	e5956008 	ldr	r6, [r5, #8]\n"
            "   8:	e2566001 	subs	r6, r6, #1\n"
            "   c:	e5856008 	str	r6, [r5, #8]\n"
            "  10:	1a000004 	bne	0x28\n"
            "  14:	e28f600c 	add	r6, pc, #12\n"
            "  18:	e5856004 	str	r6, [r5, #4]\n"
            "  1c:	e5996008 	ldr	r6, [r9, #8]\n"
            "  20:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  24:	e12fff16 	bx	r6\n"
            "  28:	e5996010 	ldr	r6, [r9, #16]\n"
            "  2c:	e3a0002c 	mov	r0, #44	@ 0x2c\n"
            "  30:	e3a01002 	mov	r1, #2\n"
            "  34:	e3a02002 	mov	r2, #2\n"
            "  38:	e3e03000 	mvn	r3, #0\n"
            "  3c:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  40:	e12fff16 	bx	r6"
        >>,
    ?assertStream(arm32, Dump, Stream).

decrement_reductions_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [Reg]),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    {State4, Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e1a0500a 	mov	r5, sl\n"
        "   8:	e5956008 	ldr	r6, [r5, #8]\n"
        "   c:	e2566001 	subs	r6, r6, #1\n"
        "  10:	e5856008 	str	r6, [r5, #8]\n"
        "  14:	1a000004 	bne	0x2c\n"
        "  18:	e28f600c 	add	r6, pc, #12\n"
        "  1c:	e5856004 	str	r6, [r5, #4]\n"
        "  20:	e5996008 	ldr	r6, [r9, #8]\n"
        "  24:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
        "  28:	e12fff16 	bx	r6\n"
        "  2c:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c"
    >>,
    ?assertStream(arm32, Dump, Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	e320f000 	nop	{0}\n"
            "   4:	ea000013 	b	0x58\n"
            "   8:	e320f000 	nop	{0}\n"
            "   c:	ea000001 	b	0x18\n"
            "  10:	e320f000 	nop	{0}\n"
            "  14:	ea00000c 	b	0x4c\n"
            "  18:	e1a0500a 	mov	r5, sl\n"
            "  1c:	e5956008 	ldr	r6, [r5, #8]\n"
            "  20:	e2566001 	subs	r6, r6, #1\n"
            "  24:	e5856008 	str	r6, [r5, #8]\n"
            "  28:	1a000007 	bne	0x4c\n"
            "  2c:	e1a0600f 	mov	r6, pc\n"
            "  30:	e3e05023 	mvn	r5, #35	@ 0x23\n"
            "  34:	e0855006 	add	r5, r5, r6\n"
            "  38:	e1a0600a 	mov	r6, sl\n"
            "  3c:	e5865004 	str	r5, [r6, #4]\n"
            "  40:	e5996008 	ldr	r6, [r9, #8]\n"
            "  44:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  48:	e12fff16 	bx	r6\n"
            "  4c:	e5996000 	ldr	r6, [r9]\n"
            "  50:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  54:	e12fff16 	bx	r6\n"
            "  58:	e5996004 	ldr	r6, [r9, #4]\n"
            "  5c:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  60:	e12fff16 	bx	r6"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_only_or_schedule_next_known_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:add_label(State4, 0),
    State6 = ?BACKEND:call_primitive_last(State5, 1, [ctx, jit_state]),
    State7 = ?BACKEND:update_branches(State6),
    Stream = ?BACKEND:stream(State7),
    Dump =
        <<
            "   0:	e320f000 	nop	{0}\n"
            "   4:	ea000010 	b	0x4c\n"
            "   8:	e320f000 	nop	{0}\n"
            "   c:	ea000001 	b	0x18\n"
            "  10:	e320f000 	nop	{0}\n"
            "  14:	eaffffff 	b	0x18\n"
            "  18:	e1a0500a 	mov	r5, sl\n"
            "  1c:	e5956008 	ldr	r6, [r5, #8]\n"
            "  20:	e2566001 	subs	r6, r6, #1\n"
            "  24:	e5856008 	str	r6, [r5, #8]\n"
            "  28:	1afffffa 	bne	0x18\n"
            "  2c:	e1a0600f 	mov	r6, pc\n"
            "  30:	e3e05023 	mvn	r5, #35	@ 0x23\n"
            "  34:	e0855006 	add	r5, r5, r6\n"
            "  38:	e1a0600a 	mov	r6, sl\n"
            "  3c:	e5865004 	str	r5, [r6, #4]\n"
            "  40:	e5996008 	ldr	r6, [r9, #8]\n"
            "  44:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  48:	e12fff16 	bx	r6\n"
            "  4c:	e5996004 	ldr	r6, [r9, #4]\n"
            "  50:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  54:	e12fff16 	bx	r6"
        >>,
    ?assertStream(arm32, Dump, Stream).

jump_to_continuation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_to_continuation(State0, {free, r1}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	e1a0600f 	mov	r6, pc\n"
            "   4:	e0811006 	add	r1, r1, r6\n"
            "   8:	e3e06007 	mvn	r6, #7\n"
            "   c:	e0811006 	add	r1, r1, r6\n"
            "  10:	e12fff11 	bx	r1"
        >>,
    ?assertStream(arm32, Dump, Stream).

return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 2, 32),
    State3 = ?BACKEND:add_label(State2, 1, 16),
    SortedLines = [{10, 16}, {20, 32}],
    State4 = ?BACKEND:return_labels_and_lines(State3, SortedLines),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:	e320f000 	nop	{0}\n"
            "   4:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
            "   8:	e320f000 	nop	{0}\n"
            "   c:	eaffffff 	b	0x10\n"
            "  10:	e320f000 	nop	{0}\n"
            "  14:	ea000001 	b	0x20\n"
            "  18:	e28f0000 	add	r0, pc, #0\n"
            "  1c:	e12fff1e 	bx	lr\n"
            "  20:	01000200 	mrseq	r0, R8_usr\n"
            "  24:	10000000 	andne	r0, r0, r0\n"
            "  28:	00000200 	andeq	r0, r0, r0, lsl #4\n"
            "  2c:	02002000 	andeq	r2, r0, #0\n"
            "  30:	00000a00 	andeq	r0, r0, r0, lsl #20\n"
            "  34:	14001000 	strne	r1, [r0], #-0\n"
            "  38:	20000000 	andcs	r0, r0, r0"
        >>,
    ?assertStream(arm32, Dump, Stream).

set_bs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:set_bs(State1, Reg),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
            "   4:	e5876078 	str	r6, [r7, #120]	@ 0x78\n"
            "   8:	e3a05000 	mov	r5, #0\n"
            "   c:	e587507c 	str	r5, [r7, #124]	@ 0x7c"
        >>,
    ?assertStream(arm32, Dump, Stream).

call_or_schedule_next_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    State6 = ?BACKEND:add_label(State5, 3),
    State7 = ?BACKEND:call_primitive_last(State6, ?PRIM_RETURN, [ctx, jit_state]),
    State8 = ?BACKEND:add_label(State7, 0),
    State9 = ?BACKEND:call_primitive_last(State8, 1, [ctx, jit_state]),
    State10 = ?BACKEND:update_branches(State9),
    Stream = ?BACKEND:stream(State10),
    Dump =
        <<
            "   0:	e320f000 	nop	{0}\n"
            "   4:	ea00001d 	b	0x80\n"
            "   8:	e320f000 	nop	{0}\n"
            "   c:	ea000003 	b	0x20\n"
            "  10:	e320f000 	nop	{0}\n"
            "  14:	ea000013 	b	0x68\n"
            "  18:	e320f000 	nop	{0}\n"
            "  1c:	ea000014 	b	0x74\n"
            "  20:	e1a0500a 	mov	r5, sl\n"
            "  24:	e5956000 	ldr	r6, [r5]\n"
            "  28:	e5876074 	str	r6, [r7, #116]	@ 0x74\n"
            "  2c:	e3a06e1a 	mov	r6, #416	@ 0x1a0\n"
            "  30:	e5876070 	str	r6, [r7, #112]	@ 0x70\n"
            "  34:	e1a0500a 	mov	r5, sl\n"
            "  38:	e5956008 	ldr	r6, [r5, #8]\n"
            "  3c:	e2566001 	subs	r6, r6, #1\n"
            "  40:	e5856008 	str	r6, [r5, #8]\n"
            "  44:	1a000007 	bne	0x68\n"
            "  48:	e1a0600f 	mov	r6, pc\n"
            "  4c:	e3e0503f 	mvn	r5, #63	@ 0x3f\n"
            "  50:	e0855006 	add	r5, r5, r6\n"
            "  54:	e1a0600a 	mov	r6, sl\n"
            "  58:	e5865004 	str	r5, [r6, #4]\n"
            "  5c:	e5996008 	ldr	r6, [r9, #8]\n"
            "  60:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  64:	e12fff16 	bx	r6\n"
            "  68:	e5996000 	ldr	r6, [r9]\n"
            "  6c:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  70:	e12fff16 	bx	r6\n"
            "  74:	e5996004 	ldr	r6, [r9, #4]\n"
            "  78:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  7c:	e12fff16 	bx	r6\n"
            "  80:	e5996004 	ldr	r6, [r9, #4]\n"
            "  84:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
            "  88:	e12fff16 	bx	r6"
        >>,
    ?assertStream(arm32, Dump, Stream).

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(arm32, Dump, Stream).

move_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_array_element: reg[2] to x_reg 0
                ?_test(begin
                    move_array_element_test0(State0, r3, 2, {x_reg, 0}, <<
                        "   0:	e5936008 	ldr	r6, [r3, #8]\n"
                        "   4:	e587602c 	str	r6, [r7, #44]	@ 0x2c"
                    >>)
                end),
                %% move_array_element: reg[3] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r3, 3, {ptr, r5}, <<
                        "   0:	e593600c 	ldr	r6, [r3, #12]\n"
                        "   4:	e5856000 	str	r6, [r5]"
                    >>)
                end),
                %% move_array_element: reg[1] to y_reg 2
                ?_test(begin
                    move_array_element_test0(State0, r3, 1, {y_reg, 2}, <<
                        "   0:	e5935004 	ldr	r5, [r3, #4]\n"
                        "   4:	e5885008 	str	r5, [r8, #8]"
                    >>)
                end),
                %% move_array_element: reg[1] to native reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 1, r5, <<
                        "   0:	e5935004 	ldr	r5, [r3, #4]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {x_reg, 2}, <<
                        "   0:	e5936010 	ldr	r6, [r3, #16]\n"
                        "   4:	e1a06106 	lsl	r6, r6, #2\n"
                        "   8:	e7936006 	ldr	r6, [r3, r6]\n"
                        "   c:	e5876034 	str	r6, [r7, #52]	@ 0x34"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to ptr
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {ptr, r5}, <<
                        "   0:	e5936010 	ldr	r6, [r3, #16]\n"
                        "   4:	e1a06106 	lsl	r6, r6, #2\n"
                        "   8:	e7936006 	ldr	r6, [r3, r6]\n"
                        "   c:	e5856000 	str	r6, [r5]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r3, 4),
                    move_array_element_test0(State1, r3, {free, Reg}, {y_reg, 2}, <<
                        "   0:	e5936010 	ldr	r6, [r3, #16]\n"
                        "   4:	e1a06106 	lsl	r6, r6, #2\n"
                        "   8:	e7936006 	ldr	r6, [r3, r6]\n"
                        "   c:	e5886008 	str	r6, [r8, #8]"
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
                %% get_array_element: reg[4] to new native reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r4, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5946010 	ldr	r6, [r4, #16]"
                    >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual(r6, Reg)
                end),
                %% get_array_element: {free, reg}[4]
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, {free, r3}, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5933010 	ldr	r3, [r3, #16]"
                    >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual(r3, Reg)
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
                %% move_to_array_element/4: x_reg to reg[2]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                        "   4:	e5836008 	str	r6, [r3, #8]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                        "   4:	e1a05004 	mov	r5, r4\n"
                        "   8:	e1a05105 	lsl	r5, r5, #2\n"
                        "   c:	e7836005 	str	r6, [r3, r5]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset] (2+1=3, 3*4=12)
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                        "   4:	e583600c 	str	r6, [r3, #12]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[1024], past the 4095
                %% byte str immediate: the offset is built in a register
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r3, 1024),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                            "   4:	e3a05004 	mov	r5, #4\n"
                            "   8:	e0855003 	add	r5, r5, r3\n"
                            "   c:	e5856ffc 	str	r6, [r5, #4092]	@ 0xffc"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_array_element/5: y_reg to reg[1020+4]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 1}, r3, 1020, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e5986004 	ldr	r6, [r8, #4]\n"
                            "   4:	e3a05004 	mov	r5, #4\n"
                            "   8:	e0855003 	add	r5, r5, r3\n"
                            "   c:	e5856ffc 	str	r6, [r5, #4092]	@ 0xffc"
                        >>,
                    ?assertStream(arm32, Dump, Stream)
                end)
            ]
        end}.

%% Array accesses past the 4095-byte ldr/str immediate range: every one of
%% these has a separate "large offset" clause that the test-suite corpus never
%% reaches (no module has a 1024-element tuple), so they are only exercised
%% here.
large_array_offset_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% get_array_element: reg[1024] to a new native register
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r4, 1024),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e3a05004 	mov	r5, #4\n"
                            "   4:	e0855004 	add	r5, r5, r4\n"
                            "   8:	e5956ffc 	ldr	r6, [r5, #4092]	@ 0xffc"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual(r6, Reg)
                end),
                %% get_array_element: {free, reg}[1024]
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, {free, r3}, 1024),
                    Stream = ?BACKEND:stream(State1),
                    Dump =
                        <<
                            "   0:	e3a06004 	mov	r6, #4\n"
                            "   4:	e0833006 	add	r3, r3, r6\n"
                            "   8:	e5933ffc 	ldr	r3, [r3, #4092]	@ 0xffc"
                        >>,
                    ?assertStream(arm32, Dump, Stream),
                    ?assertEqual(r3, Reg)
                end),
                %% move_array_element: reg[1024] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 1024, {x_reg, 0}, <<
                        "   0:	e3a06004 	mov	r6, #4\n"
                        "   4:	e0866003 	add	r6, r6, r3\n"
                        "   8:	e5965ffc 	ldr	r5, [r6, #4092]	@ 0xffc\n"
                        "   c:	e587502c 	str	r5, [r7, #44]	@ 0x2c"
                    >>)
                end),
                %% move_array_element: reg[1024] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r3, 1024, {ptr, r5}, <<
                        "   0:	e3a06004 	mov	r6, #4\n"
                        "   4:	e0866003 	add	r6, r6, r3\n"
                        "   8:	e5966ffc 	ldr	r6, [r6, #4092]	@ 0xffc\n"
                        "   c:	e5856000 	str	r6, [r5]"
                    >>)
                end),
                %% move_array_element: reg[1024] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r3, 1024, {y_reg, 2}, <<
                        "   0:	e3a05004 	mov	r5, #4\n"
                        "   4:	e0855003 	add	r5, r5, r3\n"
                        "   8:	e5955ffc 	ldr	r5, [r5, #4092]	@ 0xffc\n"
                        "   c:	e5885008 	str	r5, [r8, #8]"
                    >>)
                end),
                %% move_array_element: reg[1024] to a native register
                ?_test(begin
                    move_array_element_test0(State0, r3, 1024, r5, <<
                        "   0:	e3a06004 	mov	r6, #4\n"
                        "   4:	e0866003 	add	r6, r6, r3\n"
                        "   8:	e5965ffc 	ldr	r5, [r6, #4092]	@ 0xffc"
                    >>)
                end),
                %% move_array_element: {free, reg}[1024] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, {free, r3}, 1024, {y_reg, 2}, <<
                        "   0:	e3a05004 	mov	r5, #4\n"
                        "   4:	e0855003 	add	r5, r5, r3\n"
                        "   8:	e5953ffc 	ldr	r3, [r5, #4092]	@ 0xffc\n"
                        "   c:	e5883008 	str	r3, [r8, #8]"
                    >>)
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
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	e3a0602a 	mov	r6, #42	@ 0x2a"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: negative value via MVN
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -1),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	e3e06000 	mvn	r6, #0"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, r6}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	e5966000 	ldr	r6, [r6]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: {x_reg, 3}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	e5976038 	ldr	r6, [r7, #56]	@ 0x38"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/2: {y_reg, 3}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	e598600c 	ldr	r6, [r8, #12]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, r6),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e3a0602a 	mov	r6, #42	@ 0x2a"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, r7, r5),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e1a05007 	mov	r5, r7"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, r7}, r4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5974000 	ldr	r4, [r7]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: {x_reg, 2} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5973034 	ldr	r3, [r7, #52]	@ 0x34"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end),
                %% move_to_native_register/3: {y_reg, 2} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	e5981008 	ldr	r1, [r8, #8]"
                    >>,
                    ?assertStream(arm32, Dump, Stream)
                end)
            ]
        end}.

%% Test large Y register read (Y=1024, offset=4096, exceeds 4095-byte limit)
%% This tests the fix that changed BaseOffset from 4092 to 4080
large_y_reg_read_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 1024}),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(r6, Reg),
    Dump = <<
        "   0:	e2886eff 	add	r6, r8, #4080	@ 0xff0\n"
        "   4:	e5966010 	ldr	r6, [r6, #16]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test large Y register write
large_y_reg_write_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, SrcReg, {y_reg, 1024}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e2885eff 	add	r5, r8, #4080	@ 0xff0\n"
        "   8:	e5856010 	str	r6, [r5, #16]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test boundary case: Y=1023 (4092 bytes, within 4095 limit, should use direct addressing)
y_reg_boundary_direct_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 1023}),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual(r6, Reg),
    Dump = <<
        "   0:	e5986ffc 	ldr	r6, [r8, #4092]	@ 0xffc"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test y_reg load when only one register is available (last register, AvailT=0)
y_reg_load_last_available_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6, r1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    %% r0 is the last available register
    {State10, r0} = ?BACKEND:move_to_native_register(State6, {y_reg, 0}),
    Stream = ?BACKEND:stream(State10),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
        "   8:	e5974034 	ldr	r4, [r7, #52]	@ 0x34\n"
        "   c:	e5973038 	ldr	r3, [r7, #56]	@ 0x38\n"
        "  10:	e597203c 	ldr	r2, [r7, #60]	@ 0x3c\n"
        "  14:	e5971040 	ldr	r1, [r7, #64]	@ 0x40\n"
        "  18:	e5980000 	ldr	r0, [r8]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Cache invalidation: after free, a reload of the same register should be elided
cached_load_after_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [r11]),
    {State3, r6} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c"
    >>,
    ?assertStream(arm32, Dump, Stream).

fixed_dst_x_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r3),
    Offset1 = ?BACKEND:offset(State1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 2}),
    ?assertEqual(r3, Reg),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	e5973034 	ldr	r3, [r7, #52]	@ 0x34"
    >>,
    ?assertStream(arm32, Dump, Stream).

fixed_dst_y_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r1),
    Offset1 = ?BACKEND:offset(State1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {y_reg, 2}),
    ?assertEqual(r1, Reg),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	e5981008 	ldr	r1, [r8, #8]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% and_ with negative immediate should invalidate temp register cache
and_negative_imm_invalidates_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [r10]),
    {State4, r6} = ?BACKEND:and_(State3, {free, r6}, -4),
    {State5, r5} = ?BACKEND:move_to_native_register(State4, {x_reg, 1}),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
        "   8:	e3a04003 	mov	r4, #3\n"
        "   c:	e1c66004 	bic	r6, r6, r4"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% and_ with positive immediate should invalidate temp register cache
and_positive_imm_invalidates_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [r10]),
    {State4, r6} = ?BACKEND:and_(State3, {free, r6}, 16#3F),
    {State5, r5} = ?BACKEND:move_to_native_register(State4, {x_reg, 1}),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
        "   8:	e3a0403f 	mov	r4, #63	@ 0x3f\n"
        "   c:	e0066004 	and	r6, r6, r4"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% jump_to_label should invalidate all register caching
jump_to_label_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [r6]),
    State3 = ?BACKEND:jump_to_label(State2, 42),
    {State4, r6} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff\n"
        "   8:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c"
    >>,
    ?assertStream(arm32, Dump, Stream).

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
        "   0:	e3a06001 	mov	r6, #1\n"
        "   4:	e597502c 	ldr	r5, [r7, #44]	@ 0x2c\n"
        "   8:	e3560000 	cmp	r6, #0\n"
        "   c:	1a000002 	bne	0x1c\n"
        "  10:	e5996000 	ldr	r6, [r9]\n"
        "  14:	e5878028 	str	r8, [r7, #40]	@ 0x28\n"
        "  18:	e12fff16 	bx	r6"
    >>,
    ?assertStream(arm32, Dump, Stream).

jump_to_label_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_label(BSt0, 42)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	e3a06001 	mov	r6, #1\n"
        "   4:	e597502c 	ldr	r5, [r7, #44]	@ 0x2c\n"
        "   8:	e3560000 	cmp	r6, #0\n"
        "   c:	1a000000 	bne	0x14\n"
        "  10:	ffffffff 			@ <UNDEFINED> instruction: 0xffffffff"
    >>,
    ?assertStream(arm32, Dump, Stream).

jump_to_offset_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_offset(BSt0, 16#100)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	e3a06001 	mov	r6, #1\n"
        "   4:	e597502c 	ldr	r5, [r7, #44]	@ 0x2c\n"
        "   8:	e3560000 	cmp	r6, #0\n"
        "   c:	1a000000 	bne	0x14\n"
        "  10:	ea00003a 	b	0x100"
    >>,
    ?assertStream(arm32, Dump, Stream).

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
        "   0:	e3a06c01 	mov	r6, #256	@ 0x100\n"
        "   4:	e3a05001 	mov	r5, #1\n"
        "   8:	e597402c 	ldr	r4, [r7, #44]	@ 0x2c\n"
        "   c:	e3550000 	cmp	r5, #0\n"
        "  10:	1a000004 	bne	0x28\n"
        "  14:	e1a0500f 	mov	r5, pc\n"
        "  18:	e0866005 	add	r6, r6, r5\n"
        "  1c:	e3e0501b 	mvn	r5, #27\n"
        "  20:	e0866005 	add	r6, r6, r5\n"
        "  24:	e12fff16 	bx	r6"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% move_array_element to x_reg should invalidate vm_loc cache
move_array_element_x_reg_invalidates_vm_loc_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
    {State2, r5} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    S3 = ?BACKEND:move_array_element(State2, r10, 0, {x_reg, 5}),
    {S4, _Reg} = ?BACKEND:move_to_native_register(S3, {x_reg, 5}),
    Stream = ?BACKEND:stream(S4),
    Dump = <<
        "   0:	e5976040 	ldr	r6, [r7, #64]	@ 0x40\n"
        "   4:	e597502c 	ldr	r5, [r7, #44]	@ 0x2c\n"
        "   8:	e59a4000 	ldr	r4, [sl]\n"
        "   c:	e5874040 	str	r4, [r7, #64]	@ 0x40\n"
        "  10:	e5974040 	ldr	r4, [r7, #64]	@ 0x40"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% ldr_y_reg should invalidate its hidden temp register's cache
ldr_y_reg_invalidates_hidden_temp_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    State4 = ?BACKEND:free_native_registers(State3, [r6, r5]),
    {State5, r6} = ?BACKEND:move_to_native_register(State4, {y_reg, 0}),
    %% e is pinned: no hidden temp is used, so the x[2] cache in r4 stays
    %% valid and no reload is emitted.
    {State6, r4} = ?BACKEND:move_to_native_register(State5, {x_reg, 2}),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e5975030 	ldr	r5, [r7, #48]	@ 0x30\n"
        "   8:	e5974034 	ldr	r4, [r7, #52]	@ 0x34\n"
        "   c:	e5986000 	ldr	r6, [r8]"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test shift_right_arith
shift_right_arith_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg} = ?BACKEND:shift_right_arith(State1, {free, Reg}, 4),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
        "   4:	e1a06246 	asr	r6, r6, #4"
    >>,
    ?assertStream(arm32, Dump, Stream).

%% Test large jump table
jump_table_large_labels_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual((512 + 1) * 8, byte_size(Stream)).

%% mul/3 has a shift-and-add clause per constant the compiler emits (tuple and
%% record index scaling), plus a generic fallback for everything else. Only the
%% powers of two show up in the test corpus, so the rest are pinned here.
mul_constants_test_() ->
    [
        {"mul by 1", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 1),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<"   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c">>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 2", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 2),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<
                "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                "   4:	e1a06086 	lsl	r6, r6, #1"
            >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 3", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 3),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05086 	lsl	r5, r6, #1\n"
                    "   8:	e0856006 	add	r6, r5, r6"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 4", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 4),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<
                "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                "   4:	e1a06106 	lsl	r6, r6, #2"
            >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 5", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 5),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05106 	lsl	r5, r6, #2\n"
                    "   8:	e0856006 	add	r6, r5, r6"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 6", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 6),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05086 	lsl	r5, r6, #1\n"
                    "   8:	e0856006 	add	r6, r5, r6\n"
                    "   c:	e1a06086 	lsl	r6, r6, #1"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 7", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 7),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05186 	lsl	r5, r6, #3\n"
                    "   8:	e0456006 	sub	r6, r5, r6"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 8", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 8),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<
                "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                "   4:	e1a06186 	lsl	r6, r6, #3"
            >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 9", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 9),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05186 	lsl	r5, r6, #3\n"
                    "   8:	e0856006 	add	r6, r5, r6"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 10", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 10),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05106 	lsl	r5, r6, #2\n"
                    "   8:	e0856006 	add	r6, r5, r6\n"
                    "   c:	e1a06086 	lsl	r6, r6, #1"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 15", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 15),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a05206 	lsl	r5, r6, #4\n"
                    "   8:	e0456006 	sub	r6, r5, r6"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 16", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 16),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<
                "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                "   4:	e1a06206 	lsl	r6, r6, #4"
            >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 32", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 32),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<
                "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                "   4:	e1a06286 	lsl	r6, r6, #5"
            >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 64", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 64),
            State2 = ?BACKEND:flush(State2x),
            Dump = <<
                "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                "   4:	e1a06306 	lsl	r6, r6, #6"
            >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 100", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 100),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e3a05064 	mov	r5, #100	@ 0x64\n"
                    "   8:	e0060596 	mul	r6, r6, r5"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"mul by 12345", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2x = ?BACKEND:mul(State1, Reg, 12345),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e59f5000 	ldr	r5, [pc]	@ 0xc\n"
                    "   8:	e0060596 	mul	r6, r6, r5\n"
                    "   c:	00003039 	andeq	r3, r0, r9, lsr r0"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end}
    ].

%% and_/3 with immediates outside the ARM rotated-imm12 encoding: the mask goes
%% through mov_immediate (16#FFFFFF and the small negatives have their own
%% encodings, via BIC).
and_large_immediate_test_() ->
    [
        {"and_ with 16#FFFFFF", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2x, _} = ?BACKEND:and_(State1, {free, Reg}, 16#FFFFFF),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e1a06406 	lsl	r6, r6, #8\n"
                    "   8:	e1a06426 	lsr	r6, r6, #8"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"and_ with -16", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2x, _} = ?BACKEND:and_(State1, {free, Reg}, -16),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e3a0500f 	mov	r5, #15\n"
                    "   8:	e1c66005 	bic	r6, r6, r5"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"and_ with -256", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2x, _} = ?BACKEND:and_(State1, {free, Reg}, -256),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e3a050ff 	mov	r5, #255	@ 0xff\n"
                    "   8:	e1c66005 	bic	r6, r6, r5"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"and_ with 16#12345", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2x, _} = ?BACKEND:and_(State1, {free, Reg}, 16#12345),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e59f5000 	ldr	r5, [pc]	@ 0xc\n"
                    "   8:	e0066005 	and	r6, r6, r5\n"
                    "   c:	00012345 	andeq	r2, r1, r5, asr #6"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end},
        {"and_ with 16#7FFFFFFF", fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2x, _} = ?BACKEND:and_(State1, {free, Reg}, 16#7FFFFFFF),
            State2 = ?BACKEND:flush(State2x),
            Dump =
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e3e05102 	mvn	r5, #-2147483648	@ 0x80000000\n"
                    "   8:	e0066005 	and	r6, r6, r5"
                >>,
            ?assertStream(arm32, Dump, ?BACKEND:stream(State2))
        end}
    ].

%

%% More large-operand shapes, both emitted by jit.erl: a freed base register
%% (bs_match/bs_get_integer) and an immediate value (put_map key/value).
large_operand_extra_test_() ->
    [
        {"get_array_element at index 1024 with a freed base", fun() ->
            State0 = large_operand_state(),
            {State1, Base} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, _Reg} = ?BACKEND:get_array_element(State1, {free, Base}, 1024),
            large_operand_dump(
                State2,
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e3a05004 	mov	r5, #4\n"
                    "   8:	e0866005 	add	r6, r6, r5\n"
                    "   c:	e5966ffc 	ldr	r6, [r6, #4092]	@ 0xffc"
                >>
            )
        end},
        {"move_to_array_element of an immediate at index 1024", fun() ->
            State0 = large_operand_state(),
            {State1, Base} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:move_to_array_element(State1, 42, Base, 1024),
            large_operand_dump(
                State2,
                <<
                    "   0:	e597602c 	ldr	r6, [r7, #44]	@ 0x2c\n"
                    "   4:	e3a0502a 	mov	r5, #42	@ 0x2a\n"
                    "   8:	e3a04004 	mov	r4, #4\n"
                    "   c:	e0844006 	add	r4, r4, r6\n"
                    "  10:	e5845ffc 	str	r5, [r4, #4092]	@ 0xffc"
                >>
            )
        end}
    ].

large_operand_state() ->
    ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)).

large_operand_dump(State, Dump) ->
    ?assertStream(arm32, Dump, ?BACKEND:stream(?BACKEND:flush(State))).
