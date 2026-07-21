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

-module(jit_aarch64_tests).

-include_lib("eunit/include/eunit.hrl").

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_aarch64).

% disassembly obtained with:
% aarch64-elf-objdump -b binary -D dump.bin -M aarch64

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(r0, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400290 	ldr	x16, [x20]\n"
            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "   8:	f9000eb6 	str	x22, [x21, #24]\n"
            "   c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  10:	d63f0200 	blr	x16\n"
            "  14:	f84107fe 	ldr	x30, [sp], #16\n"
            "  18:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  1c:	f9402ab7 	ldr	x23, [x21, #80]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(r0, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400690 	ldr	x16, [x20, #8]\n"
            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "   8:	f9000eb6 	str	x22, [x21, #24]\n"
            "   c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  10:	d63f0200 	blr	x16\n"
            "  14:	f84107fe 	ldr	x30, [sp], #16\n"
            "  18:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  1c:	f9402ab7 	ldr	x23, [x21, #80]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    ?assertEqual(r0, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400a90 	ldr	x16, [x20, #16]\n"
            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "   8:	d2800540 	mov	x0, #0x2a\n"
            "   c:	d2800561 	mov	x1, #0x2b\n"
            "  10:	d2800582 	mov	x2, #0x2c\n"
            "  14:	f9000eb6 	str	x22, [x21, #24]\n"
            "  18:	f9002ab7 	str	x23, [x21, #80]\n"
            "  1c:	d63f0200 	blr	x16\n"
            "  20:	f84107fe 	ldr	x30, [sp], #16\n"
            "  24:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  28:	f9402ab7 	ldr	x23, [x21, #80]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9401687 	ldr	x7, [x20, #40]\n"
            "   4:	d2800200 	mov	x0, #0x10\n"
            "   8:	d2800401 	mov	x1, #0x20\n"
            "   c:	d2800042 	mov	x2, #0x2\n"
            "  10:	f9000eb6 	str	x22, [x21, #24]\n"
            "  14:	f9002ab7 	str	x23, [x21, #80]\n"
            "  18:	d61f00e0 	br	x7"
        >>,
    ?assertStream(aarch64, Dump, Stream).

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
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
            "   8:	f94032a8 	ldr	x8, [x21, #96]\n"
            "   c:	f940ba90 	ldr	x16, [x20, #368]\n"
            "  10:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "  14:	aa0703e0 	mov	x0, x7\n"
            "  18:	d2800801 	mov	x1, #0x40\n"
            "  1c:	d2800102 	mov	x2, #0x8\n"
            "  20:	aa0803e3 	mov	x3, x8\n"
            "  24:	f9000eb6 	str	x22, [x21, #24]\n"
            "  28:	f9002ab7 	str	x23, [x21, #80]\n"
            "  2c:	d63f0200 	blr	x16\n"
            "  30:	f84107fe 	ldr	x30, [sp], #16\n"
            "  34:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  38:	f9402ab7 	ldr	x23, [x21, #80]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

add_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:add_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
            "   8:	ab0800e7 	adds	x7, x7, x8"
        >>,
    ?assertStream(aarch64, Dump, Stream).

sub_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:sub_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
            "   8:	eb0800e7 	subs	x7, x7, x8"
        >>,
    ?assertStream(aarch64, Dump, Stream).

mul_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:mul_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
            "   8:	9344fce9 	asr	x9, x7, #4\n"
            "   c:	9344fd0a 	asr	x10, x8, #4\n"
            "  10:	9b4a7d2b 	smulh	x11, x9, x10\n"
            "  14:	9b0a7d2a 	mul	x10, x9, x10\n"
            "  18:	d37ced47 	lsl	x7, x10, #4\n"
            "  1c:	937bfd4c 	asr	x12, x10, #59\n"
            "  20:	eb0c017f 	cmp	x11, x12"
        >>,
    ?assertStream(aarch64, Dump, Stream).

if_block_overflow_set_test() ->
    %% overflow_set executes the block when V is set; the patched branch skips
    %% it (b.vc) when overflow is clear.
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
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
            "   8:	ab0800e7 	adds	x7, x7, x8\n"
            "   c:	54000047 	b.vc	0x14\n"
            "  10:	f90036a7 	str	x7, [x21, #104]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

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
    Dump =
        <<
            "   0:	f9404a90 	ldr	x16, [x20, #144]\n"
            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "   8:	d2800260 	mov	x0, #0x13\n"
            "   c:	d63f0200 	blr	x16\n"
            "  10:	f84107fe 	ldr	x30, [sp], #16\n"
            "  14:	f9404a90 	ldr	x16, [x20, #144]\n"
            "  18:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  1c:	d2800280 	mov	x0, #0x14\n"
            "  20:	d63f0200 	blr	x16\n"
            "  24:	aa0003e7 	mov	x7, x0\n"
            "  28:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  2c:	f9404a90 	ldr	x16, [x20, #144]\n"
            "  30:	a9bf1ffe 	stp	x30, x7, [sp, #-16]!\n"
            "  34:	f81f0fe0 	str	x0, [sp, #-16]!\n"
            "  38:	d2800260 	mov	x0, #0x13\n"
            "  3c:	d63f0200 	blr	x16\n"
            "  40:	aa0003e8 	mov	x8, x0\n"
            "  44:	f84107e0 	ldr	x0, [sp], #16\n"
            "  48:	a8c11ffe 	ldp	x30, x7, [sp], #16\n"
            "  4c:	f9403690 	ldr	x16, [x20, #104]\n"
            "  50:	a9bf23fe 	stp	x30, x8, [sp, #-16]!\n"
            "  54:	f9400000 	ldr	x0, [x0]\n"
            "  58:	f94000e1 	ldr	x1, [x7]\n"
            "  5c:	f9000eb6 	str	x22, [x21, #24]\n"
            "  60:	f9002ab7 	str	x23, [x21, #80]\n"
            "  64:	d63f0200 	blr	x16\n"
            "  68:	a8c123fe 	ldp	x30, x8, [sp], #16\n"
            "  6c:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  70:	f9402ab7 	ldr	x23, [x21, #80]\n"
            "  74:	f9000100 	str	x0, [x8]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

call_primitive_few_free_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, 2),
    {State3, Reg3} = ?BACKEND:move_to_native_register(State2, 3),
    {State4, Reg4} = ?BACKEND:move_to_native_register(State3, 4),
    {State5, Reg5} = ?BACKEND:move_to_native_register(State4, 5),
    {State6, ResultReg} = ?BACKEND:call_primitive(State5, ?PRIM_BITSTRING_INSERT_INTEGER, [
        Reg2, Reg1, {free, Reg4}, Reg3, {free, Reg5}
    ]),
    State7 = ?BACKEND:free_native_registers(State6, [ResultReg, Reg2, Reg1, Reg3]),
    ?BACKEND:assert_all_native_free(State7),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        "   0:	d2800027 	mov	x7, #0x1\n"
        "   4:	d2800048 	mov	x8, #0x2\n"
        "   8:	d2800069 	mov	x9, #0x3\n"
        "   c:	d280008a 	mov	x10, #0x4\n"
        "  10:	d28000ab 	mov	x11, #0x5\n"
        "  14:	f940e690 	ldr	x16, [x20, #456]\n"
        "  18:	a9bf1ffe 	stp	x30, x7, [sp, #-16]!\n"
        "  1c:	a9bf27e8 	stp	x8, x9, [sp, #-16]!\n"
        "  20:	aa0803e0 	mov	x0, x8\n"
        "  24:	aa0703e1 	mov	x1, x7\n"
        "  28:	aa0a03e2 	mov	x2, x10\n"
        "  2c:	aa0903e3 	mov	x3, x9\n"
        "  30:	aa0b03e4 	mov	x4, x11\n"
        "  34:	f9000eb6 	str	x22, [x21, #24]\n"
        "  38:	f9002ab7 	str	x23, [x21, #80]\n"
        "  3c:	d63f0200 	blr	x16\n"
        "  40:	a8c127e8 	ldp	x8, x9, [sp], #16\n"
        "  44:	a8c11ffe 	ldp	x30, x7, [sp], #16\n"
        "  48:	f9400eb6 	ldr	x22, [x21, #24]\n"
        "  4c:	f9402ab7 	ldr	x23, [x21, #80]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	b9401267 	ldr	w7, [x19, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001267 	str	w7, [x19, #16]\n"
        "   c:	540000e1 	b.ne	0x28\n"
        "  10:	100000c7 	adr	x7, 0x28\n"
        "  14:	f9000667 	str	x7, [x19, #8]\n"
        "  18:	f9400a87 	ldr	x7, [x20, #16]\n"
        "  1c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  20:	f9002ab7 	str	x23, [x21, #80]\n"
        "  24:	d61f00e0 	br	x7\n"
        "  28:	f9401287 	ldr	x7, [x20, #32]\n"
        "  2c:	d2800040 	mov	x0, #0x2\n"
        "  30:	aa0003e1 	mov	x1, x0\n"
        "  34:	92800002 	mov	x2, #0xffffffffffffffff\n"
        "  38:	f9000eb6 	str	x22, [x21, #24]\n"
        "  3c:	f9002ab7 	str	x23, [x21, #80]\n"
        "  40:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

call_primitive_last_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?CASE_CLAUSE_ATOM, {free, RegA}
    ]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   4:	f9404e88 	ldr	x8, [x20, #152]\n"
        "   8:	d2800100 	mov	x0, #0x8\n"
        "   c:	d2805961 	mov	x1, #0x2cb\n"
        "  10:	aa0703e2 	mov	x2, x7\n"
        "  14:	f9000eb6 	str	x22, [x21, #24]\n"
        "  18:	f9002ab7 	str	x23, [x21, #80]\n"
        "  1c:	d61f0100 	br	x8"
    >>,
    ?assertStream(aarch64, Dump, Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	b9401267 	ldr	w7, [x19, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001267 	str	w7, [x19, #16]\n"
        "   c:	540000e1 	b.ne	0x28\n"
        "  10:	100000c7 	adr	x7, 0x28\n"
        "  14:	f9000667 	str	x7, [x19, #8]\n"
        "  18:	f9400a87 	ldr	x7, [x20, #16]\n"
        "  1c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  20:	f9002ab7 	str	x23, [x21, #80]\n"
        "  24:	d61f00e0 	br	x7\n"
        "  28:	f9401287 	ldr	x7, [x20, #32]\n"
        "  2c:	d2800040 	mov	x0, #0x2\n"
        "  30:	aa0003e1 	mov	x1, x0\n"
        "  34:	d2800142 	mov	x2, #0xa\n"
        "  38:	f9000eb6 	str	x22, [x21, #24]\n"
        "  3c:	f9002ab7 	str	x23, [x21, #80]\n"
        "  40:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	f9400287 	ldr	x7, [x20]\n"
            "   4:	d2800540 	mov	x0, #0x2a\n"
            "   8:	f9000eb6 	str	x22, [x21, #24]\n"
            "   c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  10:	d61f00e0 	br	x7"
        >>,
    ?assertStream(aarch64, Dump, Stream).

%% The gc_bif func pointer is resolved inline rather than via the
%% PRIM_GET_IMPORTED_GCBIF primitive call: an inline extended-register
%% emptiness check (calling PRIM_TRIM_LIVE_REGS only when non-empty) followed
%% by module->imported_funcs[Bif]->bif0_ptr loads.
move_imported_gcbif_to_native_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r7} = ?BACKEND:move_imported_gcbif_to_native_register(State0, 2, 5),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	9103e2a7 	add	x7, x21, #0xf8\n"
            "   4:	f94000e8 	ldr	x8, [x7]\n"
            "   8:	eb07011f 	cmp	x8, x7\n"
            "   c:	540000c0 	b.eq	0x24\n"
            "  10:	f9401e90 	ldr	x16, [x20, #56]\n"
            "  14:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "  18:	d2800040 	mov	x0, #0x2\n"
            "  1c:	d63f0200 	blr	x16\n"
            "  20:	f84107fe 	ldr	x30, [sp], #16\n"
            "  24:	f9400267 	ldr	x7, [x19]\n"
            "  28:	f94048e7 	ldr	x7, [x7, #144]\n"
            "  2c:	f94014e7 	ldr	x7, [x7, #40]\n"
            "  30:	f94004e7 	ldr	x7, [x7, #8]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

%% A compile-time float constant is stored into fr[1] as its raw IEEE-754
%% bits: load the bits, load the fr array base (ctx->fr), store.
move_float_to_fp_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_float_to_fp_reg(State0, 4.0, 1),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	d2e80207 	mov	x7, #0x4010000000000000\n"
        "   4:	f9400e68 	ldr	x8, [x19, #24]\n"
        "   8:	f9000507 	str	x7, [x8, #8]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
        "   0:	d2800027 	mov	x7, #0x1\n"
        "   4:	f9402ea8 	ldr	x8, [x21, #88]\n"
        "   8:	b50000a7 	cbnz	x7, 0x1c\n"
        "   c:	f9400287 	ldr	x7, [x20]\n"
        "  10:	f9000eb6 	str	x22, [x21, #24]\n"
        "  14:	f9002ab7 	str	x23, [x21, #80]\n"
        "  18:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

jump_to_label_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_label(BSt0, 42)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	d2800027 	mov	x7, #0x1\n"
        "   4:	f9402ea8 	ldr	x8, [x21, #88]\n"
        "   8:	b5000047 	cbnz	x7, 0x10\n"
        "   c:	14000000 	b	0xc"
    >>,
    ?assertStream(aarch64, Dump, Stream).

jump_to_offset_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_offset(BSt0, 16#100)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	d2800027 	mov	x7, #0x1\n"
        "   4:	f9402ea8 	ldr	x8, [x21, #88]\n"
        "   8:	b5000047 	cbnz	x7, 0x10\n"
        "   c:	1400003d 	b	0x100"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
        "   0:	d2802007 	mov	x7, #0x100\n"
        "   4:	d2800028 	mov	x8, #0x1\n"
        "   8:	f9402ea9 	ldr	x9, [x21, #88]\n"
        "   c:	b5000088 	cbnz	x8, 0x1c\n"
        "  10:	10ffff88 	adr	x8, 0x0\n"
        "  14:	8b070108 	add	x8, x8, x7\n"
        "  18:	d61f0100 	br	x8"
    >>,
    ?assertStream(aarch64, Dump, Stream).

move_array_element_x_reg_invalidates_vm_loc_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r7} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
    {State2, r8} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:move_array_element(State2, r8, 0, {x_reg, 5}),
    {State4, _Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 5}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	f94042a7 	ldr	x7, [x21, #128]\n"
        "   4:	f9402ea8 	ldr	x8, [x21, #88]\n"
        "   8:	f9400109 	ldr	x9, [x8]\n"
        "   c:	f90042a9 	str	x9, [x21, #128]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
                    ?assertEqual(r0, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	f9405690 	ldr	x16, [x20, #168]\n"
                            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
                            "   8:	f9000eb6 	str	x22, [x21, #24]\n"
                            "   c:	f9002ab7 	str	x23, [x21, #80]\n"
                            "  10:	d63f0200 	blr	x16\n"
                            "  14:	f84107fe 	ldr	x30, [sp], #16\n"
                            "  18:	f9400eb6 	ldr	x22, [x21, #24]\n"
                            "  1c:	f9402ab7 	ldr	x23, [x21, #80]\n"
                            "  20:	eb15001f 	cmp	x0, x21\n"
                            "  24:	54000040 	b.eq	0x2c\n"
                            "  28:	d65f03c0 	ret"
                        >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(r0, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(r7, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	f9405690 	ldr	x16, [x20, #168]\n"
                            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
                            "   8:	f9000eb6 	str	x22, [x21, #24]\n"
                            "   c:	f9002ab7 	str	x23, [x21, #80]\n"
                            "  10:	d63f0200 	blr	x16\n"
                            "  14:	f84107fe 	ldr	x30, [sp], #16\n"
                            "  18:	f9400eb6 	ldr	x22, [x21, #24]\n"
                            "  1c:	f9402ab7 	ldr	x23, [x21, #80]\n"
                            "  20:	aa0003e7 	mov	x7, x0\n"
                            "  24:	eb1500ff 	cmp	x7, x21\n"
                            "  28:	54000060 	b.eq	0x34\n"
                            "  2c:	aa0703e0 	mov	x0, x7\n"
                            "  30:	d65f03c0 	ret"
                        >>,
                    ?assertStream(aarch64, Dump, Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    %% BaseReg (x7) keeps y_regs_base reserved for a follow-up increment_sp.
    %% ValReg (x8) is the temp used to load y[0] and store it to CP.
    Dump =
        <<
            "   0:	f94002e7 	ldr	x7, [x23]\n"
            "   4:	f90072a7 	str	x7, [x21, #224]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	9100e2f7 	add	x23, x23, #0x38"
        >>,
    ?assertStream(aarch64, Dump, Stream).

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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	b6f80047 	tbz	x7, #63, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	eb0800ff 	cmp	x7, x8\n"
                        "   c:	5400004a 	b.ge	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	b5000047 	cbnz	x7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	b5000047 	cbnz	x7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	35000047 	cbnz	w7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	35000047 	cbnz	w7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	b4000047 	cbz	x7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	b4000047 	cbz	x7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	34000047 	cbz	w7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	34000047 	cbz	w7, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                %% register right-hand side: emitted by OP_IS_FUNCTION2 when the
                %% arity is a typed integer register (e.g. is_function(F, N))
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '!=', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	6b0800ff 	cmp	w7, w8\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '!=', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	6b0800ff 	cmp	w7, w8\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000041 	b.ne	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f100ecff 	cmp	x7, #0x3b\n"
                        "   c:	54000041 	b.ne	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000041 	b.ne	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	7100a8ff 	cmp	w7, #0x2a\n"
                        "   c:	54000041 	b.ne	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	37000047 	tbnz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	37000047 	tbnz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	36000047 	tbz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	36000047 	tbz	w7, #0, 0x10\n"
                        "   c:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f24008ff 	tst	x7, #0x7\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	d28000a9 	mov	x9, #0x5\n"
                        "   c:	ea0900ff 	tst	x7, x9\n"
                        "  10:	54000040 	b.eq	0x18\n"
                        "  14:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f24008ff 	tst	x7, #0x7\n"
                        "   c:	54000040 	b.eq	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	92400ce9 	and	x9, x7, #0xf\n"
                        "   c:	f1003d3f 	cmp	x9, #0xf\n"
                        "  10:	54000040 	b.eq	0x18\n"
                        "  14:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	92400cf0 	and	x16, x7, #0xf\n"
                        "   c:	f1003e1f 	cmp	x16, #0xf\n"
                        "  10:	54000040 	b.eq	0x18\n"
                        "  14:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f10190ff 	cmp	x7, #0x64\n"
                        "   c:	5400004d 	b.le	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
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
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f10190ff 	cmp	x7, #0x64\n"
                        "   c:	5400004d 	b.le	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 100},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f10190ff 	cmp	x7, #0x64\n"
                        "   c:	5400004a 	b.ge	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '<', 100},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
                        "   8:	f10190ff 	cmp	x7, #0x64\n"
                        "   c:	5400004a 	b.ge	0x14\n"
                        "  10:	91000908 	add	x8, x8, #0x2"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
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
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	f94032a8 	ldr	x8, [x21, #96]\n"
            "   8:	f100ecff 	cmp	x7, #0x3b\n"
            "   c:	54000061 	b.ne	0x18\n"
            "  10:	91000908 	add	x8, x8, #0x2\n"
            "  14:	14000002 	b	0x1c\n"
            "  18:	91001108 	add	x8, x8, #0x4"
        >>,
    ?assertStream(aarch64, Dump, Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 3),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                    "   4:	d343fce7 	lsr	x7, x7, #3"
                >>,
            ?assertStream(aarch64, Dump, Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 3),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                    "   4:	d343fce8 	lsr	x8, x7, #3"
                >>,
            ?assertStream(aarch64, Dump, Stream)
        end)
    ].

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
            "   4:	d37df0e7 	lsl	x7, x7, #3"
        >>,
    ?assertStream(aarch64, Dump, Stream).

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
            "   0:	14000013 	b	0x4c\n"
            "   4:	14000002 	b	0xc\n"
            "   8:	1400000d 	b	0x3c\n"
            "   c:	b9401267 	ldr	w7, [x19, #16]\n"
            "  10:	f10004e7 	subs	x7, x7, #0x1\n"
            "  14:	b9001267 	str	w7, [x19, #16]\n"
            "  18:	54000040 	b.eq	0x20\n"
            "  1c:	14000008 	b	0x3c\n"
            "  20:	100000e7 	adr	x7, 0x3c\n"
            "  24:	914000e7 	add	x7, x7, #0x0, lsl #12\n"
            "  28:	f9000667 	str	x7, [x19, #8]\n"
            "  2c:	f9400a87 	ldr	x7, [x20, #16]\n"
            "  30:	f9000eb6 	str	x22, [x21, #24]\n"
            "  34:	f9002ab7 	str	x23, [x21, #80]\n"
            "  38:	d61f00e0 	br	x7\n"
            "  3c:	f9400287 	ldr	x7, [x20]\n"
            "  40:	f9000eb6 	str	x22, [x21, #24]\n"
            "  44:	f9002ab7 	str	x23, [x21, #80]\n"
            "  48:	d61f00e0 	br	x7\n"
            "  4c:	f9400687 	ldr	x7, [x20, #8]\n"
            "  50:	f9000eb6 	str	x22, [x21, #24]\n"
            "  54:	f9002ab7 	str	x23, [x21, #80]\n"
            "  58:	d61f00e0 	br	x7"
        >>,
    ?assertStream(aarch64, Dump, Stream).

call_only_or_schedule_next_known_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#2c),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	14000011 	b	0x44\n"
            "   4:	14000002 	b	0xc\n"
            "   8:	14000009 	b	0x2c\n"
            "   c:	b9401267 	ldr	w7, [x19, #16]\n"
            "  10:	f10004e7 	subs	x7, x7, #0x1\n"
            "  14:	b9001267 	str	w7, [x19, #16]\n"
            "  18:	540000a1 	b.ne	0x2c\n"
            "  1c:	10000087 	adr	x7, 0x2c\n"
            "  20:	f9000667 	str	x7, [x19, #8]\n"
            "  24:	f9400a87 	ldr	x7, [x20, #16]\n"
            "  28:	f9000eb6 	str	x22, [x21, #24]\n"
            "  2c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  30:	d61f00e0 	br	x7\n"
            "  34:	f9400287 	ldr	x7, [x20]\n"
            "  38:	f9000eb6 	str	x22, [x21, #24]\n"
            "  3c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  40:	d61f00e0 	br	x7\n"
            "  44:	f9400687 	ldr	x7, [x20, #8]\n"
            "  48:	f9000eb6 	str	x22, [x21, #24]\n"
            "  4c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  50:	d61f00e0 	br	x7"
        >>,
    ?assertStream(aarch64, Dump, Stream).

call_bif_with_large_literal_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = ?BACKEND:call_primitive(State1, 15, [ctx, {avm_int64_t, 9208452466117618637}]),
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
            "   0:	f9402290 	ldr	x16, [x20, #64]\n"
            "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "   8:	d2800040 	mov	x0, #0x2\n"
            "   c:	d63f0200 	blr	x16\n"
            "  10:	f84107fe 	ldr	x30, [sp], #16\n"
            "  14:	f9403e90 	ldr	x16, [x20, #120]\n"
            "  18:	a9bf03fe 	stp	x30, x0, [sp, #-16]!\n"
            "  1c:	d29579a0 	mov	x0, #0xabcd\n"
            "  20:	f2b7c040 	movk	x0, #0xbe02, lsl #16\n"
            "  24:	f2dfd740 	movk	x0, #0xfeba, lsl #32\n"
            "  28:	f2eff940 	movk	x0, #0x7fca, lsl #48\n"
            "  2c:	f9000eb6 	str	x22, [x21, #24]\n"
            "  30:	f9002ab7 	str	x23, [x21, #80]\n"
            "  34:	d63f0200 	blr	x16\n"
            "  38:	aa0003e7 	mov	x7, x0\n"
            "  3c:	a8c103fe 	ldp	x30, x0, [sp], #16\n"
            "  40:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  44:	f9402ab7 	ldr	x23, [x21, #80]\n"
            "  48:	aa0003f0 	mov	x16, x0\n"
            "  4c:	f81f0ffe 	str	x30, [sp, #-16]!\n"
            "  50:	aa1503e0 	mov	x0, x21\n"
            "  54:	d2800001 	mov	x1, #0x0\n"
            "  58:	d2800022 	mov	x2, #0x1\n"
            "  5c:	f9402ea3 	ldr	x3, [x21, #88]\n"
            "  60:	aa0703e4 	mov	x4, x7\n"
            "  64:	f9000eb6 	str	x22, [x21, #24]\n"
            "  68:	f9002ab7 	str	x23, [x21, #80]\n"
            "  6c:	d63f0200 	blr	x16\n"
            "  70:	f84107fe 	ldr	x30, [sp], #16\n"
            "  74:	f9400eb6 	ldr	x22, [x21, #24]\n"
            "  78:	f9402ab7 	ldr	x23, [x21, #80]\n"
            "  7c:	b50000c0 	cbnz	x0, 0x94\n"
            "  80:	f9401a87 	ldr	x7, [x20, #48]\n"
            "  84:	d2801080 	mov	x0, #0x84\n"
            "  88:	f9000eb6 	str	x22, [x21, #24]\n"
            "  8c:	f9002ab7 	str	x23, [x21, #80]\n"
            "  90:	d61f00e0 	br	x7\n"
            "  94:	f9002ea0 	str	x0, [x21, #88]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

get_list_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg} = ?BACKEND:and_(State1, {free, Reg}, -4),
    State3 = ?BACKEND:move_array_element(State2, Reg, 1, {y_reg, 1}),
    State4 = ?BACKEND:move_array_element(State3, Reg, 0, {y_reg, 0}),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    Stream = ?BACKEND:stream(State5),
    Dump = <<
        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   4:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "   8:	f94004e8 	ldr	x8, [x7, #8]\n"
        "   c:	f90006e8 	str	x8, [x23, #8]\n"
        "  10:	f94000e8 	ldr	x8, [x7]\n"
        "  14:	f90002e8 	str	x8, [x23]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
                {
                    {free, Reg},
                    '&',
                    ?TERM_BOXED_TAG_MASK_NO_SIGN,
                    '!=',
                    ?TERM_BOXED_POSITIVE_INTEGER
                },
                fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            )
        end
    ),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    Offset = ?BACKEND:offset(State4),
    State5 = ?BACKEND:add_label(State4, Label, Offset + 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	14000001 	b	0x4\n"
        "   4:	14000050 	b	0x144\n"
        "   8:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   c:	92400ce8 	and	x8, x7, #0xf\n"
        "  10:	f1003d1f 	cmp	x8, #0xf\n"
        "  14:	54000180 	b.eq	0x44\n"
        "  18:	924004e8 	and	x8, x7, #0x3\n"
        "  1c:	f100091f 	cmp	x8, #0x2\n"
        "  20:	54000040 	b.eq	0x28\n"
        "  24:	14000048 	b	0x144\n"
        "  28:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "  2c:	f94000e7 	ldr	x7, [x7]\n"
        "  30:	d2800770 	mov	x16, #0x3b\n"
        "  34:	8a1000f0 	and	x16, x7, x16\n"
        "  38:	f100221f 	cmp	x16, #0x8\n"
        "  3c:	54000040 	b.eq	0x44\n"
        "  40:	14000041 	b	0x144"
    >>,
    ?assertStream(aarch64, Dump, Stream).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

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
                    {Reg, '&', ?TERM_BOXED_TAG_MASK_NO_SIGN, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
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
    Offset = ?BACKEND:offset(State4),
    State5 = ?BACKEND:add_label(State4, Label, Offset + 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	14000001 	b	0x4\n"
        "   4:	14000053 	b	0x150\n"
        "   8:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   c:	92400ce8 	and	x8, x7, #0xf\n"
        "  10:	f1003d1f 	cmp	x8, #0xf\n"
        "  14:	540001e0 	b.eq	0x50\n"
        "  18:	924004e8 	and	x8, x7, #0x3\n"
        "  1c:	f100091f 	cmp	x8, #0x2\n"
        "  20:	54000040 	b.eq	0x28\n"
        "  24:	1400004b 	b	0x150\n"
        "  28:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "  2c:	f94000e7 	ldr	x7, [x7]\n"
        "  30:	d2800768 	mov	x8, #0x3b\n"
        "  34:	8a0800e8 	and	x8, x7, x8\n"
        "  38:	f100211f 	cmp	x8, #0x8\n"
        "  3c:	540000a0 	b.eq	0x50\n"
        "  40:	924014f0 	and	x16, x7, #0x3f\n"
        "  44:	f100621f 	cmp	x16, #0x18\n"
        "  48:	54000040 	b.eq	0x50\n"
        "  4c:	14000041 	b	0x150"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
    Offset = ?BACKEND:offset(State4),
    State5 = ?BACKEND:add_label(State4, Label, Offset + 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:	14000001 	b	0x4\n"
        "   4:	14000047 	b	0x120\n"
        "   8:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   c:	f1012cff 	cmp	x7, #0x4b\n"
        "  10:	54000080 	b.eq	0x20\n"
        "  14:	f1002cff 	cmp	x7, #0xb\n"
        "  18:	54000040 	b.eq	0x20\n"
        "  1c:	14000041 	b	0x120"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% Test OP_WAIT_TIMEOUT pattern
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
    Dump = <<
        "   0:	10000147 	adr	x7, 0x28\n"
        "   4:	914000e7 	add	x7, x7, #0x0, lsl #12\n"
        "   8:	f9000667 	str	x7, [x19, #8]\n"
        "   c:	d2827107 	mov	x7, #0x1388\n"
        "  10:	f9407a88 	ldr	x8, [x20, #240]\n"
        "  14:	aa0703e0 	mov	x0, x7\n"
        "  18:	d2800541 	mov	x1, #0x2a\n"
        "  1c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  20:	f9002ab7 	str	x23, [x21, #80]\n"
        "  24:	d61f0100 	br	x8\n"
        "  28:	f9405690 	ldr	x16, [x20, #168]\n"
        "  2c:	f81f0ffe 	str	x30, [sp, #-16]!\n"
        "  30:	f9000eb6 	str	x22, [x21, #24]\n"
        "  34:	f9002ab7 	str	x23, [x21, #80]\n"
        "  38:	d63f0200 	blr	x16\n"
        "  3c:	f84107fe 	ldr	x30, [sp], #16\n"
        "  40:	f9400eb6 	ldr	x22, [x21, #24]\n"
        "  44:	f9402ab7 	ldr	x23, [x21, #80]\n"
        "  48:	eb15001f 	cmp	x0, x21\n"
        "  4c:	54000040 	b.eq	0x54\n"
        "  50:	d65f03c0 	ret\n"
        "  54:	f9408690 	ldr	x16, [x20, #264]\n"
        "  58:	f81f0ffe 	str	x30, [sp, #-16]!\n"
        "  5c:	d2800040 	mov	x0, #0x2\n"
        "  60:	d63f0200 	blr	x16\n"
        "  64:	f84107fe 	ldr	x30, [sp], #16\n"
        "  68:	b50000c0 	cbnz	x0, 0x80\n"
        "  6c:	f9407e87 	ldr	x7, [x20, #248]\n"
        "  70:	d2800540 	mov	x0, #0x2a\n"
        "  74:	f9000eb6 	str	x22, [x21, #24]\n"
        "  78:	f9002ab7 	str	x23, [x21, #80]\n"
        "  7c:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
    Dump = <<
        "   0:	14000001 	b	0x4\n"
        "   4:	14000005 	b	0x18\n"
        "   8:	1400003e 	b	0x100\n"
        "   c:	14000001 	b	0x10\n"
        "  10:	14000001 	b	0x14\n"
        "  14:	14000001 	b	0x18\n"
        "  18:	10000747 	adr	x7, 0x100\n"
        "  1c:	914000e7 	add	x7, x7, #0x0, lsl #12\n"
        "  20:	f9000667 	str	x7, [x19, #8]\n"
        "  24:	f9407687 	ldr	x7, [x20, #232]\n"
        "  28:	f9000eb6 	str	x22, [x21, #24]\n"
        "  2c:	f9002ab7 	str	x23, [x21, #80]\n"
        "  30:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
    Dump = <<
        "   0:	14000001 	b	0x4\n"
        "   4:	14000005 	b	0x18\n"
        "   8:	1400003e 	b	0x100\n"
        "   c:	14000001 	b	0x10\n"
        "  10:	14000001 	b	0x14\n"
        "  14:	14000001 	b	0x18\n"
        "  18:	10000747 	adr	x7, 0x100\n"
        "  1c:	f9000667 	str	x7, [x19, #8]\n"
        "  20:	f9407687 	ldr	x7, [x20, #232]\n"
        "  24:	f9000eb6 	str	x22, [x21, #24]\n"
        "  28:	f9002ab7 	str	x23, [x21, #80]\n"
        "  2c:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% set_continuation_to_label with a known label beyond ADR's ±1MB range
wait_known_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:add_label(State2, Label, 16#200000),
    State4 = ?BACKEND:set_continuation_to_label(State3, Label),
    Stream = ?BACKEND:stream(State4),
    % Rel = 16#200000 - 16#18 = 16#1FFFE8:
    % adr x7, 0xfe8; add x7, x7, #0x1ff, lsl #12; str x7, [x1, #8]
    ?assertEqual(
        <<
            16#10007f47:32/little,
            16#9147fce7:32/little,
            16#f9000667:32/little
        >>,
        binary:part(Stream, 16#18, 12)
    ).

%% set_continuation_to_label placeholder patched to a label beyond ADR range
wait_forward_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:set_continuation_to_label(State2, Label),
    State4 = ?BACKEND:add_label(State3, Label, 16#200000),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    % Same code as wait_known_far_test, but patched in place
    ?assertEqual(
        <<
            16#10007f47:32/little,
            16#9147fce7:32/little,
            16#f9000667:32/little
        >>,
        binary:part(Stream, 16#18, 12)
    ).

%% set_continuation_to_offset resolved beyond ADR range (the unicode_util case)
set_continuation_to_offset_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, OffsetRef} = ?BACKEND:set_continuation_to_offset(State0),
    % Grow the stream past ADR's ±1MB range before resolving the continuation
    FillerSize = 16#1A0000,
    Filler = binary:copy(<<0, 0, 0, 0>>, FillerSize div 4),
    Stream1 = jit_stream_binary:append(?BACKEND:stream(State1), Filler),
    % element 3 of #state{} is the stream
    State2 = setelement(3, State1, Stream1),
    State3 = ?BACKEND:add_label(State2, OffsetRef),
    State4 = ?BACKEND:update_branches(State3),
    Stream = ?BACKEND:stream(State4),
    % Continuation is at 12 + 16#1A0000 = 16#1A000C:
    % adr x7, 0xc; add x7, x7, #0x1a0, lsl #12; str x7, [x1, #8]
    ?assertEqual(
        <<
            16#10000067:32/little,
            16#914680e7:32/little,
            16#f9000667:32/little
        >>,
        binary:part(Stream, 0, 12)
    ).

%% set_continuation_to_label with a known label far backwards
wait_known_far_backwards_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 5),
    Label = 2,
    State2 = ?BACKEND:add_label(State1, Label, 4),
    FillerSize = 16#200000,
    Filler = binary:copy(<<0, 0, 0, 0>>, FillerSize div 4),
    Stream1 = jit_stream_binary:append(?BACKEND:stream(State2), Filler),
    % element 3 of #state{} is the stream
    State3 = setelement(3, State2, Stream1),
    State4 = ?BACKEND:set_continuation_to_label(State3, Label),
    Stream = ?BACKEND:stream(State4),
    % Rel = 4 - 16#200018 = -16#200014 = -16#200 * 4096 - 16#14:
    % adr x7, -0x14; sub x7, x7, #0x200, lsl #12; str x7, [x1, #8]
    ?assertEqual(
        <<
            16#10ffff67:32/little,
            16#d14800e7:32/little,
            16#f9000667:32/little
        >>,
        binary:part(Stream, 16#18 + FillerSize, 12)
    ).

return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),

    % Test return_labels_and_lines with some sample labels and lines
    State2 = ?BACKEND:add_label(State1, 1, 16),
    % {Line, Offset} pairs
    SortedLines = [{10, 16}, {20, 32}],

    State3 = ?BACKEND:add_label(State2, 0),

    State4 = ?BACKEND:return_labels_and_lines(State3, SortedLines),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),

    ?assert(byte_size(Stream) >= 44),

    Dump = <<
        "   0:	14000003 	b	0xc\n"
        "   4:	14000003 	b	0x10\n"
        "   8:	14000001 	b	0xc\n"
        "   c:	b4000040 	cbz	x0, 0x14\n"
        "  10:	d42175a0 	brk	#0xbad\n"
        "  14:	10000040 	adr	x0, 0x1c\n"
        "  18:	d65f03c0 	ret\n"
        "  1c:	00000200 	.inst	0x00000200\n"
        "  20:	0c000000 	st4	{v0.8b-v3.8b}, [x0]\n"
        "  24:	00000100 	.inst	0x00000100\n"
        "  28:	02001000 	.inst	0x02001000\n"
        "  2c:	00000a00 	.inst	0x00000a00\n"
        "  30:	14001000 	b	0x4030\n"
        "  34:	20000000 	.inst	0x20000000"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% Test call_primitive with {free, {x_reg, X}}
gc_bif2_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_GET_IMPORTED_BIF, [jit_state, 42]),
    {State2, _ResultReg} = ?BACKEND:call_func_ptr(State1, {free, FuncPtr}, [
        ctx, 0, 3, {y_reg, 0}, {free, {x_reg, 0}}
    ]),

    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9402290 	ldr	x16, [x20, #64]\n"
        "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
        "   8:	d2800540 	mov	x0, #0x2a\n"
        "   c:	d63f0200 	blr	x16\n"
        "  10:	f84107fe 	ldr	x30, [sp], #16\n"
        "  14:	aa0003f0 	mov	x16, x0\n"
        "  18:	f81f0ffe 	str	x30, [sp, #-16]!\n"
        "  1c:	aa1503e0 	mov	x0, x21\n"
        "  20:	d2800001 	mov	x1, #0x0\n"
        "  24:	d2800062 	mov	x2, #0x3\n"
        "  28:	f94002e3 	ldr	x3, [x23]\n"
        "  2c:	f9402ea4 	ldr	x4, [x21, #88]\n"
        "  30:	f9000eb6 	str	x22, [x21, #24]\n"
        "  34:	f9002ab7 	str	x23, [x21, #80]\n"
        "  38:	d63f0200 	blr	x16\n"
        "  3c:	f84107fe 	ldr	x30, [sp], #16\n"
        "  40:	f9400eb6 	ldr	x22, [x21, #24]\n"
        "  44:	f9402ab7 	ldr	x23, [x21, #80]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% Test case where parameter value is in r1
memory_ensure_free_with_roots_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, {free, r1}, 4, 1
    ]),

    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	f940b290 	ldr	x16, [x20, #352]\n"
        "   4:	f81f0ffe 	str	x30, [sp, #-16]!\n"
        "   8:	aa0103e0 	mov	x0, x1\n"
        "   c:	d2800081 	mov	x1, #0x4\n"
        "  10:	d2800022 	mov	x2, #0x1\n"
        "  14:	f9000eb6 	str	x22, [x21, #24]\n"
        "  18:	f9002ab7 	str	x23, [x21, #80]\n"
        "  1c:	d63f0200 	blr	x16\n"
        "  20:	f84107fe 	ldr	x30, [sp], #16\n"
        "  24:	f9400eb6 	ldr	x22, [x21, #24]\n"
        "  28:	f9402ab7 	ldr	x23, [x21, #80]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% After a call that returns (call_primitive_with_cp), code is reachable
%% again: a later if_else_block merge must intersect both arms' register
%% caches instead of taking one arm verbatim because the other is flagged
%% unreachable. Regression test: gc_bif to a y_reg followed by a compare
%% read back a stale cached register on the inline fast path.
call_primitive_with_cp_resumes_reachable_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_with_cp(State0, 4, [ctx, jit_state]),
    % element 9 of #state{} is the regs cache
    RegsAfter = element(9, State1),
    Other = jit_regs:set_contents(jit_regs:new(0, 0), r7, {y_reg, 0}),
    Merged = jit_regs:merge(Other, RegsAfter, 16#FFFF),
    ?assertEqual(#{}, jit_regs:get_all_contents(Merged)).

call_ext_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	b9401267 	ldr	w7, [x19, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001267 	str	w7, [x19, #16]\n"
        "   c:	540000e1 	b.ne	0x28\n"
        "  10:	100000c7 	adr	x7, 0x28\n"
        "  14:	f9000667 	str	x7, [x19, #8]\n"
        "  18:	f9400a87 	ldr	x7, [x20, #16]\n"
        "  1c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  20:	f9002ab7 	str	x23, [x21, #80]\n"
        "  24:	d61f00e0 	br	x7\n"
        "  28:	f9401667 	ldr	x7, [x19, #40]\n"
        "  2c:	d2802a10 	mov	x16, #0x150\n"
        "  30:	aa1000e7 	orr	x7, x7, x16\n"
        "  34:	f90072a7 	str	x7, [x21, #224]\n"
        "  38:	f9401287 	ldr	x7, [x20, #32]\n"
        "  3c:	d2800040 	mov	x0, #0x2\n"
        "  40:	d28000a1 	mov	x1, #0x5\n"
        "  44:	92800002 	mov	x2, #0xffffffffffffffff\n"
        "  48:	f9000eb6 	str	x22, [x21, #24]\n"
        "  4c:	f9002ab7 	str	x23, [x21, #80]\n"
        "  50:	d61f00e0 	br	x7"
    >>,
    ?assertStream(aarch64, Dump, Stream).

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
    Dump = <<
        "   0:	b9401267 	ldr	w7, [x19, #16]\n"
        "   4:	f10004e7 	subs	x7, x7, #0x1\n"
        "   8:	b9001267 	str	w7, [x19, #16]\n"
        "   c:	540000e1 	b.ne	0x28\n"
        "  10:	100000c7 	adr	x7, 0x28\n"
        "  14:	f9000667 	str	x7, [x19, #8]\n"
        "  18:	f9400a87 	ldr	x7, [x20, #16]\n"
        "  1c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  20:	f9002ab7 	str	x23, [x21, #80]\n"
        "  24:	d61f00e0 	br	x7\n"
        "  28:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "  2c:	aa0703e8 	mov	x8, x7\n"
        "  30:	92400509 	and	x9, x8, #0x3\n"
        "  34:	f100093f 	cmp	x9, #0x2\n"
        "  38:	54000100 	b.eq	0x58\n"
        "  3c:	f9404e87 	ldr	x7, [x20, #152]\n"
        "  40:	d2800800 	mov	x0, #0x40\n"
        "  44:	d2803161 	mov	x1, #0x18b\n"
        "  48:	aa0803e2 	mov	x2, x8\n"
        "  4c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  50:	f9002ab7 	str	x23, [x21, #80]\n"
        "  54:	d61f00e0 	br	x7\n"
        "  58:	927ef508 	and	x8, x8, #0xfffffffffffffffc\n"
        "  5c:	f9400108 	ldr	x8, [x8]\n"
        "  60:	92401509 	and	x9, x8, #0x3f\n"
        "  64:	f100513f 	cmp	x9, #0x14\n"
        "  68:	54000100 	b.eq	0x88\n"
        "  6c:	f9404e87 	ldr	x7, [x20, #152]\n"
        "  70:	d2800e00 	mov	x0, #0x70\n"
        "  74:	d2803161 	mov	x1, #0x18b\n"
        "  78:	aa0803e2 	mov	x2, x8\n"
        "  7c:	f9000eb6 	str	x22, [x21, #24]\n"
        "  80:	f9002ab7 	str	x23, [x21, #80]\n"
        "  84:	d61f00e0 	br	x7\n"
        "  88:	f9401668 	ldr	x8, [x19, #40]\n"
        "  8c:	d2805810 	mov	x16, #0x2c0\n"
        "  90:	aa100108 	orr	x8, x8, x16\n"
        "  94:	f90072a8 	str	x8, [x21, #224]\n"
        "  98:	f9408288 	ldr	x8, [x20, #256]\n"
        "  9c:	aa0703e0 	mov	x0, x7\n"
        "  a0:	d2800001 	mov	x1, #0x0\n"
        "  a4:	f9000eb6 	str	x22, [x21, #24]\n"
        "  a8:	f9002ab7 	str	x23, [x21, #80]\n"
        "  ac:	d61f0100 	br	x8"
    >>,
    ?assertStream(aarch64, Dump, Stream).

decrement_reductions_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [Reg]),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    {State4, Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   4:	b9401267 	ldr	w7, [x19, #16]\n"
        "   8:	f10004e7 	subs	x7, x7, #0x1\n"
        "   c:	b9001267 	str	w7, [x19, #16]\n"
        "  10:	540000e1 	b.ne	0x2c\n"
        "  14:	100000c7 	adr	x7, 0x2c\n"
        "  18:	f9000667 	str	x7, [x19, #8]\n"
        "  1c:	f9400a87 	ldr	x7, [x20, #16]\n"
        "  20:	f9000eb6 	str	x22, [x21, #24]\n"
        "  24:	f9002ab7 	str	x23, [x21, #80]\n"
        "  28:	d61f00e0 	br	x7\n"
        "  2c:	f9402ea7 	ldr	x7, [x21, #88]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

move_to_vm_register_test0(State, Source, Dest, Dump) ->
    State1 = ?BACKEND:move_to_vm_register(State, Source, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(aarch64, Dump, Stream).

move_to_vm_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, 0}, <<
                        "   0:	f9002ebf 	str	xzr, [x21, #88]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	f9006ebf 	str	xzr, [x21, #216]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, r10}, <<
                        "   0:	f900015f 	str	xzr, [x10]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	f9000aff 	str	xzr, [x23, #16]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	f90052ff 	str	xzr, [x23, #160]"
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	d2800547 	mov	x7, #0x2a\n"
                        "   4:	f9002ea7 	str	x7, [x21, #88]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	d2800547 	mov	x7, #0x2a\n"
                        "   4:	f9006ea7 	str	x7, [x21, #216]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	d2800547 	mov	x7, #0x2a\n"
                        "   4:	f9000ae7 	str	x7, [x23, #16]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	d2800547 	mov	x7, #0x2a\n"
                        "   4:	f90052e7 	str	x7, [x23, #160]"
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, r10}, <<
                        "   0:	d2800c67 	mov	x7, #0x63                  	// #99\n"
                        "   4:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	f94032a7 	ldr	x7, [x21, #96]\n"
                        "   4:	f90036a7 	str	x7, [x21, #104]"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, r8}, <<
                        "   0:	f94032a7 	ldr	x7, [x21, #96]\n"
                        "   4:	f9000107 	str	x7, [x8]"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, r9}, {x_reg, 3}, <<
                        "   0:	f9400127 	ldr	x7, [x9]\n"
                        "   4:	f9003aa7 	str	x7, [x21, #112]"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f90006e7 	str	x7, [x23, #8]"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	f94002e7 	ldr	x7, [x23]\n"
                        "   4:	f9003aa7 	str	x7, [x21, #112]"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	f94006e7 	ldr	x7, [x23, #8]\n"
                        "   4:	f9003aa7 	str	x7, [x21, #112]"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, r10, {x_reg, 0}, <<
                        "   0:	f9002eaa 	str	x10, [x21, #88]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, r10, {x_reg, extra}, <<
                        "   0:	f9006eaa 	str	x10, [x21, #216]"
                    >>)
                end),
                %% Test: Native register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, r9, {ptr, r10}, <<
                        "   0:	f9000149 	str	x9, [x10]"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, r10, {y_reg, 0}, <<
                        "   0:	f90002ea 	str	x10, [x23]"
                    >>)
                end),
                %% Test: Large immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, 0}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9002ea7 	str	x7, [x21, #88]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, extra}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9006ea7 	str	x7, [x21, #216]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 2}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9000ae7 	str	x7, [x23, #16]"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 20}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f90052e7 	str	x7, [x23, #160]"
                    >>)
                end),
                %% Test: Large immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {ptr, r10}, <<
                        "   0:	d29bde07 	mov	x7, #0xdef0                	// #57072\n"
                        "   4:	f2b35787 	movk	x7, #0x9abc, lsl #16\n"
                        "   8:	f2cacf07 	movk	x7, #0x5678, lsl #32\n"
                        "   c:	f2e24687 	movk	x7, #0x1234, lsl #48\n"
                        "  10:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	f9406aa7 	ldr	x7, [x21, #208]\n"
                        "   4:	f9007ee7 	str	x7, [x23, #248]"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	f9407ee7 	ldr	x7, [x23, #248]\n"
                        "   4:	f9006aa7 	str	x7, [x21, #208]"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	92800007 	mov	x7, #0xffffffffffffffff\n"
                        "   4:	f9002ea7 	str	x7, [x21, #88]"
                    >>)
                end),
                %% Test: ptr with offset to fp_reg (term_to_float)
                ?_test(begin
                    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    State2 = ?BACKEND:move_to_vm_register(
                        State1, {free, {ptr, RegA, 1}}, {fp_reg, 3}
                    ),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f94004e7 	ldr	x7, [x7, #8]\n"
                        "   8:	f9400e68 	ldr	x8, [x19, #24]\n"
                        "   c:	f9000d07 	str	x7, [x8, #24]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end)
            ]
        end}.

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(aarch64, Dump, Stream).

move_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 2, {x_reg, 0}, <<
                        "   0:	f9400907 	ldr	x7, [x8, #16]\n"
                        "   4:	f9002ea7 	str	x7, [x21, #88]"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r8, 3, {ptr, r10}, <<
                        "   0:	f9400d07 	ldr	x7, [x8, #24]\n"
                        "   4:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, {y_reg, 2}, <<
                        "   0:	f9400507 	ldr	x7, [x8, #8]\n"
                        "   4:	f9000ae7 	str	x7, [x23, #16]"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (r10)
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, r10, <<
                        "   0:	f940050a 	ldr	x10, [x8, #8]"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {y_reg, 31}, <<
                        "   0:	f9401d07 	ldr	x7, [x8, #56]\n"
                        "   4:	f9007ee7 	str	x7, [x23, #248]"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {x_reg, 15}, <<
                        "   0:	f9401d07 	ldr	x7, [x8, #56]\n"
                        "   4:	f9006aa7 	str	x7, [x21, #208]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {x_reg, 2}, <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]\n"
                        "   4:	f8677907 	ldr	x7, [x8, x7, lsl #3]\n"
                        "   8:	f90036a7 	str	x7, [x21, #104]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {ptr, r10}, <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]\n"
                        "   4:	f8677907 	ldr	x7, [x8, x7, lsl #3]\n"
                        "   8:	f9000147 	str	x7, [x10]"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {y_reg, 31}, <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]\n"
                        "   4:	f8677907 	ldr	x7, [x8, x7, lsl #3]\n"
                        "   8:	f9007ee7 	str	x7, [x23, #248]"
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
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9401107 	ldr	x7, [x8, #32]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream),
                    ?assertEqual(r7, Reg)
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
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f9000907 	str	x7, [x8, #16]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, r9),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f8297907 	str	x7, [x8, x9, lsl #3]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_array_element/4: ptr to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {ptr, r7}, r8, r9),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f94000e7 	ldr	x7, [x7]\n"
                        "   4:	f8297907 	str	x7, [x8, x9, lsl #3]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_array_element/4: y_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 2}, r8, r9),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9400ae7 	ldr	x7, [x23, #16]\n"
                        "   4:	f8297907 	str	x7, [x8, x9, lsl #3]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	f9000d07 	str	x7, [x8, #24]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    Regs0 = element(9, State0),
                    State2 = setelement(
                        9,
                        State0,
                        jit_regs:set_masks(
                            Regs0,
                            jit_regs:available_regs(Regs0) band
                                (bnot ((1 bsl 8) bor (1 bsl 9))),
                            (1 bsl 8) bor (1 bsl 9)
                        )
                    ),
                    [r8, r9] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, {x_reg, 0}, r8, r9, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
                        "   4:	9100052a 	add	x10, x9, #0x1\n"
                        "   8:	f82a7907 	str	x7, [x8, x10, lsl #3]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_array_element/5: imm to reg[x+offset]
                ?_test(begin
                    Regs0 = element(9, State0),
                    State2 = setelement(
                        9,
                        State0,
                        jit_regs:set_masks(
                            Regs0,
                            jit_regs:available_regs(Regs0) band
                                (bnot ((1 bsl 8) bor (1 bsl 9))),
                            (1 bsl 8) bor (1 bsl 9)
                        )
                    ),
                    [r8, r9] = ?BACKEND:used_regs(State2),
                    State3 = ?BACKEND:move_to_array_element(State2, 42, r8, r9, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42\n"
                        "   4:	9100052a 	add	x10, x9, #0x1\n"
                        "   8:	f82a7907 	str	x7, [x8, x10, lsl #3]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
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
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	d2800547 	mov	x7, #0x2a                  	// #42"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, r6}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r6, Reg),
                    Dump = <<
                        "   0:	f94000c6 	ldr	x6, [x6]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/2: {x_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	f9403aa7 	ldr	x7, [x21, #112]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/2: {y_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(r7, Reg),
                    Dump = <<
                        "   0:	f9400ee7 	ldr	x7, [x23, #24]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	d2800548 	mov	x8, #0x2a                  	// #42"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, r7, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	aa0703e8 	mov	x8, x7"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, r7}, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f94000e8 	ldr	x8, [x7]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/3: {x_reg, x} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f94036a8 	ldr	x8, [x21, #104]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end),
                %% move_to_native_register/3: {y_reg, y} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r8),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	f9400ae8 	ldr	x8, [x23, #16]"
                    >>,
                    ?assertStream(aarch64, Dump, Stream)
                end)
            ]
        end}.

add_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:add(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(aarch64, Dump, Stream).

add_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    add_test0(State0, r2, 2, <<
                        "   0:	91000842 	add	x2, x2, #0x2"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, r2, 256, <<
                        "   0:	91040042 	add	x2, x2, #0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, r2, r3, <<
                        "   0:	8b030042 	add	x2, x2, x3"
                    >>)
                end)
            ]
        end}.

sub_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:sub(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(aarch64, Dump, Stream).

sub_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    sub_test0(State0, r2, 2, <<
                        "   0:	d1000842 	sub	x2, x2, #0x2"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, r2, 256, <<
                        "   0:	d1040042 	sub	x2, x2, #0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, r2, r3, <<
                        "   0:	cb030042 	sub	x2, x2, x3"
                    >>)
                end)
            ]
        end}.

mul_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:mul(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(aarch64, Dump, Stream).

mul_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    mul_test0(State0, r2, 2, <<
                        "0:	d37ff842 	lsl	x2, x2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 3, <<
                        "   0:	d37ff847 	lsl	x7, x2, #1\n"
                        "   4:	8b0200e2 	add	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 4, <<
                        "0:	d37ef442 	lsl	x2, x2, #2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 5, <<
                        "   0:	d37ef447 	lsl	x7, x2, #2\n"
                        "   4:	8b0200e2 	add	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 6, <<
                        "   0:	d37ff847 	lsl	x7, x2, #1\n"
                        "   4:	8b0200e2 	add	x2, x7, x2\n"
                        "   8:	d37ff842 	lsl	x2, x2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 7, <<
                        "   0:	d37df047 	lsl	x7, x2, #3\n"
                        "   4:	cb0200e2 	sub	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 8, <<
                        "0:	d37df042 	lsl	x2, x2, #3"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 9, <<
                        "   0:	d37df047 	lsl	x7, x2, #3\n"
                        "   4:	8b0200e2 	add	x2, x7, x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 10, <<
                        "   0:	d37ef447 	lsl	x7, x2, #2\n"
                        "   4:	8b0200e2 	add	x2, x7, x2\n"
                        "   8:	d37ff842 	lsl	x2, x2, #1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, r2, 11, <<
                        "   0:	d2800167 	mov	x7, #0xb                   	// #11\n"
                        "   4:	9b077c42 	mul	x2, x2, x7"
                    >>)
                end)
            ]
        end}.

%% Test jump_to_continuation optimization for intra-module returns
jump_to_continuation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_to_continuation(State0, {free, r0}),
    Stream = ?BACKEND:stream(State1),
    % Expected: adr x7, NetOffset; add x7, x7, x0; br x7
    % With default offset 0, NetOffset = 0 - 0 = 0, temp register is r7
    Dump =
        <<
            "   0:	10000007 	adr	x7, 0x0\n"
            "   4:	8b0000e7 	add	x7, x7, x0\n"
            "   8:	d61f00e0 	br	x7"
        >>,
    ?assertStream(aarch64, Dump, Stream).

%% Continuation jump emitted beyond ADR's ±1MB range needs a longer sequence
jump_to_continuation_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Simulate a module whose native code grew past ADR's ±1MB range
    FillerSize = 1400000,
    Filler = binary:copy(<<0, 0, 0, 0>>, FillerSize div 4),
    Stream0 = jit_stream_binary:append(?BACKEND:stream(State0), Filler),
    % element 3 of #state{} is the stream
    State1 = setelement(3, State0, Stream0),
    State2 = ?BACKEND:jump_to_continuation(State1, {free, r0}),
    Stream = ?BACKEND:stream(State2),
    Code = binary:part(Stream, FillerSize, byte_size(Stream) - FillerSize),
    % NetOffset = -1400000 = -16#155CC0:
    % adr x7, 0; sub x7, x7, #0x155, lsl #12; sub x7, x7, #0xcc0;
    % add x7, x7, x0; br x7
    ?assertEqual(
        <<
            16#10000007:32/little,
            16#d14554e7:32/little,
            16#d13300e7:32/little,
            16#8b0000e7:32/little,
            16#d61f00e0:32/little
        >>,
        Code
    ).

%% After freeing a register, cache is preserved so reload is elided
cached_load_after_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, r7} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [r7]),
    {State3, r7} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	f9402ea7 	ldr	x7, [x21, #88]"
        >>,
    ?assertStream(aarch64, Dump, Stream).

fixed_dst_x_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, r8),
    Offset1 = ?BACKEND:offset(State1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 2}),
    ?assertEqual(r8, Reg),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f94036a8 	ldr	x8, [x21, #104]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

fixed_dst_y_reg_load_preserves_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, r8),
    Offset1 = ?BACKEND:offset(State1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {y_reg, 2}),
    ?assertEqual(r8, Reg),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9400ae8 	ldr	x8, [x23, #16]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% After copying an x_reg to another vm location, the temp register holding the
%% x_reg value is cached so a subsequent load of the same x_reg skips the ldr
cached_move_to_vm_x_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {x_reg, 1}, {x_reg, 0}),
    Offset1 = ?BACKEND:offset(State1),
    {State2, r7} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f94032a7 	ldr	x7, [x21, #96]\n"
        "   4:	f9002ea7 	str	x7, [x21, #88]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% After copying a y_reg to an x_reg, the temp register holding the y_reg value
%% is cached so a subsequent load of the same y_reg skips the ldrs
cached_move_to_vm_y_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {y_reg, 0}, {x_reg, 0}),
    Offset1 = ?BACKEND:offset(State1),
    {State2, r7} = ?BACKEND:move_to_native_register(State1, {y_reg, 0}),
    ?assertEqual(Offset1, ?BACKEND:offset(State2)),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f94002e7 	ldr	x7, [x23]\n"
        "   4:	f9002ea7 	str	x7, [x21, #88]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

float_op_fadd_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:float_op(State0, ?PRIM_FADD, 1, 2, 3),
    ?assertEqual(r7, Reg),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	f9400e68 	ldr	x8, [x19, #24]\n"
        "   4:	fd400500 	ldr	d0, [x8, #8]\n"
        "   8:	fd400901 	ldr	d1, [x8, #16]\n"
        "   c:	1e612800 	fadd	d0, d0, d1\n"
        "  10:	fd000d00 	str	d0, [x8, #24]\n"
        "  14:	9e660007 	fmov	x7, d0\n"
        "  18:	d2effe08 	mov	x8, #0x7ff0000000000000\n"
        "  1c:	8a0800e7 	and	x7, x7, x8\n"
        "  20:	eb0800ff 	cmp	x7, x8\n"
        "  24:	9a9f07e7 	cset	x7, ne"
    >>,
    ?assertStream(aarch64, Dump, Stream).

float_op_fmul_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:float_op(State0, ?PRIM_FMUL, 1, 2, 3),
    ?assertEqual(r7, Reg),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	f9400e68 	ldr	x8, [x19, #24]\n"
        "   4:	fd400500 	ldr	d0, [x8, #8]\n"
        "   8:	fd400901 	ldr	d1, [x8, #16]\n"
        "   c:	1e610800 	fmul	d0, d0, d1\n"
        "  10:	fd000d00 	str	d0, [x8, #24]\n"
        "  14:	9e660007 	fmov	x7, d0\n"
        "  18:	d2effe08 	mov	x8, #0x7ff0000000000000\n"
        "  1c:	8a0800e7 	and	x7, x7, x8\n"
        "  20:	eb0800ff 	cmp	x7, x8\n"
        "  24:	9a9f07e7 	cset	x7, ne"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% The single-precision (FLOAT32) variant has no inline support and must fall
%% back to the C primitive.
float_op_float32_unsupported_test() ->
    State0 = ?BACKEND:new(
        ?JIT_VARIANT_PIC bor ?JIT_VARIANT_FLOAT32, jit_stream_binary, jit_stream_binary:new(0)
    ),
    ?assertEqual(false, ?BACKEND:supports_fp(State0)).

float_op_supported_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual(true, ?BACKEND:supports_fp(State0)).

float_conv_int_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, IntReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:float_conv_int(State1, IntReg, 1),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   4:	f9400e68 	ldr	x8, [x19, #24]\n"
        "   8:	9e6200e0 	scvtf	d0, x7\n"
        "   c:	fd000500 	str	d0, [x8, #8]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

float_conv_float_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, BoxedReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:float_conv_float(State1, {free, BoxedReg}, 1),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	f9402ea7 	ldr	x7, [x21, #88]\n"
        "   4:	927ef4e7 	and	x7, x7, #0xfffffffffffffffc\n"
        "   8:	fd4004e0 	ldr	d0, [x7, #8]\n"
        "   c:	f9400e68 	ldr	x8, [x19, #24]\n"
        "  10:	fd000500 	str	d0, [x8, #8]"
    >>,
    ?assertStream(aarch64, Dump, Stream).

%% call_only_or_schedule_next to a label beyond bcc's ±1MB range must use
%% an inverted-condition + unconditional branch pair
call_only_or_schedule_next_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#200000),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:update_branches(State4),
    Stream = ?BACKEND:stream(State5),
    % After the jump table (3 * 4 bytes) and the reduction decrement
    % (3 instructions), at 16#18: b.eq +8; b 0x200000
    <<_:16#18/binary, Code:8/binary, _/binary>> = Stream,
    % b.eq 0x28 (skip over the b); b 0x200000 (Rel = 16#200000 - 16#1c)
    ?assertEqual(
        <<
            16#54000040:32/little,
            (16#14000000 bor ((16#200000 - 16#1c) div 4)):32/little
        >>,
        Code
    ).

%% jump_to_label_cond fuses a widenable guard into a single conditional branch
%% to a backward label, in place of the two-branch skip + jump form.
jump_to_label_cond_fused_backward_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 4),
    LabelOffset = ?BACKEND:offset(State1),
    %% Label 1 sits here; the guard below jumps back to it.
    State2 = ?BACKEND:add_label(State1, 1),
    {State3, RegA} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% "jump to label 1 when RegA != 5": a single b.ne back to label 1.
    Fused = ?BACKEND:stream(?BACKEND:jump_to_label_cond(State3, {RegA, '!=', 5}, 1)),
    Fallback = ?BACKEND:stream(
        ?BACKEND:if_block(State3, {RegA, '!=', 5}, fun(BSt0) ->
            ?BACKEND:jump_to_label(BSt0, 1)
        end)
    ),
    ?assert(byte_size(Fused) < byte_size(Fallback)),
    %% The fused guard ends with exactly one b.ne straight to label 1.
    BranchOffset = byte_size(Fused) - 4,
    Rel = LabelOffset - BranchOffset,
    ?assertEqual(jit_aarch64_asm:bcc(ne, Rel), binary:part(Fused, BranchOffset, 4)).

%% The register-test conditions (cbz/cbnz/tbz) are invertible too, so a guard
%% like "jump when Reg != 0" fuses into a single cbnz to a backward label.
jump_to_label_cond_fused_backward_cbnz_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 4),
    LabelOffset = ?BACKEND:offset(State1),
    State2 = ?BACKEND:add_label(State1, 1),
    {State3, RegA} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% {Reg, '!=', 0} compiles to a cbz skip; fused+inverted it is one cbnz.
    Fused = ?BACKEND:stream(?BACKEND:jump_to_label_cond(State3, {RegA, '!=', 0}, 1)),
    BranchOffset = byte_size(Fused) - 4,
    Rel = LabelOffset - BranchOffset,
    ?assertEqual(jit_aarch64_asm:cbnz(RegA, Rel), binary:part(Fused, BranchOffset, 4)).

%% A forward fused guard branch is emitted optimistically and resolved at
%% finalize (update_branches) to a single conditional branch once the target
%% label offset is known. jit_stream_binary is backtrackable, so the forward
%% path is taken.
jump_to_label_cond_fused_forward_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 8),
    {State2, RegA} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    %% Forward guard jump to label 5 (not yet defined): optimistic 4-byte branch.
    State3 = ?BACKEND:jump_to_label_cond(State2, {RegA, '!=', 5}, 5),
    BranchOffset = ?BACKEND:offset(State3) - 4,
    %% Some more code, then define label 5 a short (in-range) distance ahead.
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 2}),
    LabelOffset = ?BACKEND:offset(State4),
    State5 = ?BACKEND:add_label(State4, 5),
    State6 = ?BACKEND:update_branches(State5),
    %% Fit -> no overflow, and the placeholder is now a resolved b.ne to label 5.
    ?assertEqual(#{}, ?BACKEND:take_overflows(State6)),
    Stream = ?BACKEND:stream(State6),
    Rel = LabelOffset - BranchOffset,
    ?assertEqual(jit_aarch64_asm:bcc(ne, Rel), binary:part(Stream, BranchOffset, 4)).

%% A forward fused branch whose target lands beyond the bcc reach overflows its
%% 4-byte reservation: update_branches reports it in take_overflows, and the
%% re-emit pass (branch hints pinning it to 8 bytes) produces the
%% inverted-skip + b pair -- the full backtrack contract at unit level.
jump_to_label_cond_fused_forward_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 8),
    {State2, RegA} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:jump_to_label_cond(State2, {RegA, '!=', 5}, 5),
    BranchOffset = ?BACKEND:offset(State3) - 4,
    %% Place label 5 beyond the +/-1MB bcc reach.
    LabelOffset = 16#200000,
    State4 = ?BACKEND:add_label(State3, 5, LabelOffset),
    State5 = ?BACKEND:update_branches(State4),
    ?assertEqual(#{0 => 8}, ?BACKEND:take_overflows(State5)),
    %% Second pass, as driven by jit:compile's emit_finalize_loop: same emission
    %% with the overflowing branch pinned to 8 bytes.
    StateR0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    StateR1 = ?BACKEND:jump_table(StateR0, 8),
    StateR2 = ?BACKEND:set_branch_hints(StateR1, #{0 => 8}),
    {StateR3, RegA} = ?BACKEND:move_to_native_register(StateR2, {x_reg, 0}),
    StateR4 = ?BACKEND:jump_to_label_cond(StateR3, {RegA, '!=', 5}, 5),
    ?assertEqual(BranchOffset + 8, ?BACKEND:offset(StateR4)),
    StateR5 = ?BACKEND:add_label(StateR4, 5, LabelOffset),
    StateR6 = ?BACKEND:update_branches(StateR5),
    ?assertEqual(#{}, ?BACKEND:take_overflows(StateR6)),
    Stream = ?BACKEND:stream(StateR6),
    %% b.eq skips over the b (8 bytes); b covers the full distance.
    Expected = <<
        (jit_aarch64_asm:bcc(eq, 8))/binary,
        (jit_aarch64_asm:b(LabelOffset - BranchOffset - 4))/binary
    >>,
    ?assertEqual(Expected, binary:part(Stream, BranchOffset, 8)).
