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

-module(jit_x86_64_tests).

-include_lib("eunit/include/eunit.hrl").

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit/src/opcodes.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_x86_64).

% disassembly obtained with:
% x86_64-elf-objdump -b binary -D dump.bin -M x86-64 -mi386

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	50                   	push   %rax\n"
            "   1:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "   5:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "   9:	48 8b 03             	mov    (%rbx),%rax\n"
            "   c:	ff d0                	call   *%rax\n"
            "   e:	41 5b                	pop    %r11\n"
            "  10:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  14:	4d 8b 7e 50          	mov    0x50(%r14),%r15"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	50                   	push   %rax\n"
            "   1:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "   5:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "   9:	48 8b 43 08          	mov    0x8(%rbx),%rax\n"
            "   d:	ff d0                	call   *%rax\n"
            "   f:	41 5b                	pop    %r11\n"
            "  11:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  15:	4d 8b 7e 50          	mov    0x50(%r14),%r15"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	50                   	push   %rax\n"
            "   1:	bf 2a 00 00 00       	mov    $0x2a,%edi\n"
            "   6:	be 2b 00 00 00       	mov    $0x2b,%esi\n"
            "   b:	ba 2c 00 00 00       	mov    $0x2c,%edx\n"
            "  10:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  14:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  18:	48 8b 43 10          	mov    0x10(%rbx),%rax\n"
            "  1c:	ff d0                	call   *%rax\n"
            "  1e:	41 5b                	pop    %r11\n"
            "  20:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  24:	4d 8b 7e 50          	mov    0x50(%r14),%r15"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% Regression: an immediate argument whose target parameter register is still
%% occupied by a *later* register argument used to crash set_args0 with
%% `xchgq(imm, ParamReg)` (function_clause in x86_64_x_reg). Here the immediate
%% 42 targets rdx (3rd parameter) while the 4th argument still lives in rdx, so
%% rdx must be moved out (to rcx) before the immediate is loaded into it.
call_primitive_immediate_param_conflict_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:call_primitive(State0, 0, [r8, r9, 42, rdx]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	50                   	push   %rax\n"
            "   1:	4c 89 c7             	mov    %r8,%rdi\n"
            "   4:	4c 89 ce             	mov    %r9,%rsi\n"
            "   7:	48 89 d1             	mov    %rdx,%rcx\n"
            "   a:	ba 2a 00 00 00       	mov    $0x2a,%edx\n"
            "   f:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  13:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  17:	48 8b 03             	mov    (%rbx),%rax\n"
            "  1a:	ff d0                	call   *%rax\n"
            "  1c:	41 5b                	pop    %r11\n"
            "  1e:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  22:	4d 8b 7e 50          	mov    0x50(%r14),%r15"
        >>,
    ?assertStream(x86_64, Dump, Stream).

add_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:add_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
            "   8:	4c 01 d8             	add    %r11,%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

add_overflow_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:add_overflow(State1, RegA, 32),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	48 83 c0 20          	add    $0x20,%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

sub_overflow_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:sub_overflow(State1, RegA, 32),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	48 83 e8 20          	sub    $0x20,%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

mul_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:mul_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
            "   8:	48 83 e0 f0          	and    $0xfffffffffffffff0,%rax\n"
            "   c:	49 c1 fb 04          	sar    $0x4,%r11\n"
            "  10:	49 0f af c3          	imul   %r11,%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

sub_overflow_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:sub_overflow(State2, RegA, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
            "   8:	4c 29 d8             	sub    %r11,%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
            "   8:	4c 01 d8             	add    %r11,%rax\n"
            "   b:	0f 81 04 00 00 00    	jno    0x15\n"
            "  11:	49 89 46 68          	mov    %rax,0x68(%r14)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_primitive_extended_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:call_primitive(
        State0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]
    ),
    {State2, RegB} = ?BACKEND:call_primitive(
        State1, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 20]
    ),
    {State3, RegC} = ?BACKEND:call_primitive(
        State2, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]
    ),
    {State4, ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_PUT_LIST, [
        ctx, {free, {ptr, RegA}}, {free, {ptr, RegB}}
    ]),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {ptr, RegC}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg, {ptr, RegC}]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:	50                   	push   %rax\n"
            "   1:	bf 13 00 00 00       	mov    $0x13,%edi\n"
            "   6:	48 8b 83 90 00 00 00 	mov    0x90(%rbx),%rax\n"
            "   d:	ff d0                	call   *%rax\n"
            "   f:	41 5b                	pop    %r11\n"
            "  11:	50                   	push   %rax\n"
            "  12:	bf 14 00 00 00       	mov    $0x14,%edi\n"
            "  17:	48 8b 83 90 00 00 00 	mov    0x90(%rbx),%rax\n"
            "  1e:	ff d0                	call   *%rax\n"
            "  20:	49 89 c3             	mov    %rax,%r11\n"
            "  23:	58                   	pop    %rax\n"
            "  24:	41 53                	push   %r11\n"
            "  26:	50                   	push   %rax\n"
            "  27:	50                   	push   %rax\n"
            "  28:	bf 13 00 00 00       	mov    $0x13,%edi\n"
            "  2d:	48 8b 83 90 00 00 00 	mov    0x90(%rbx),%rax\n"
            "  34:	ff d0                	call   *%rax\n"
            "  36:	41 5b                	pop    %r11\n"
            "  38:	49 89 c2             	mov    %rax,%r10\n"
            "  3b:	58                   	pop    %rax\n"
            "  3c:	41 5b                	pop    %r11\n"
            "  3e:	41 52                	push   %r10\n"
            "  40:	48 8b 38             	mov    (%rax),%rdi\n"
            "  43:	49 8b 33             	mov    (%r11),%rsi\n"
            "  46:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  4a:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  4e:	48 8b 43 68          	mov    0x68(%rbx),%rax\n"
            "  52:	ff d0                	call   *%rax\n"
            "  54:	41 5a                	pop    %r10\n"
            "  56:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  5a:	4d 8b 7e 50          	mov    0x50(%r14),%r15\n"
            "  5e:	49 89 02             	mov    %rax,(%r10)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_primitive_few_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r11} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r10} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r9} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r8} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6, rcx} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),

    CreatedBin = rax,
    Offset = r11,
    SrcReg = r8,
    SizeValue = r9,
    FlagsValue = rcx,

    {State7, r8} = ?BACKEND:call_primitive(State6, ?PRIM_BITSTRING_INSERT_INTEGER, [
        CreatedBin, Offset, {free, SrcReg}, SizeValue, {free, FlagsValue}
    ]),
    Stream = ?BACKEND:stream(State7),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
            "   8:	4d 8b 56 68          	mov    0x68(%r14),%r10\n"
            "   c:	4d 8b 4e 70          	mov    0x70(%r14),%r9\n"
            "  10:	4d 8b 46 78          	mov    0x78(%r14),%r8\n"
            "  14:	49 8b 8e 80 00 00 00 	mov    0x80(%r14),%rcx\n"
            "  1b:	41 51                	push   %r9\n"
            "  1d:	41 52                	push   %r10\n"
            "  1f:	41 53                	push   %r11\n"
            "  21:	50                   	push   %rax\n"
            "  22:	50                   	push   %rax\n"
            "  23:	48 89 c7             	mov    %rax,%rdi\n"
            "  26:	4c 89 de             	mov    %r11,%rsi\n"
            "  29:	4c 89 c2             	mov    %r8,%rdx\n"
            "  2c:	4c 87 c9             	xchg   %r9,%rcx\n"
            "  2f:	4d 89 c8             	mov    %r9,%r8\n"
            "  32:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  36:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  3a:	48 8b 83 c8 01 00 00 	mov    0x1c8(%rbx),%rax\n"
            "  41:	ff d0                	call   *%rax\n"
            "  43:	41 5b                	pop    %r11\n"
            "  45:	49 89 c0             	mov    %rax,%r8\n"
            "  48:	58                   	pop    %rax\n"
            "  49:	41 5b                	pop    %r11\n"
            "  4b:	41 5a                	pop    %r10\n"
            "  4d:	41 59                	pop    %r9\n"
            "  4f:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  53:	4d 8b 7e 50          	mov    0x50(%r14),%r15"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% Regression: the `{Reg, '&', Mask, '!=', Val}' condition (Reg is NOT `{free,_}',
%% so it must survive the test) needs a scratch register to hold a copy of Reg
%% while the AND/CMP destroys it. When every scratch register is already
%% allocated, the backend used to crash in first_avail/1 (function_clause on an
%% empty available mask). It must instead spill Reg via push/pop. This is hit in
%% practice by term_to_int on the bs_create_bin path while precompiling
%% sys_core_fold.beam for x86_64.
if_block_cond_and_neq_no_scratch_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r11} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r10} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, r9} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, r8} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {State6a, rcx} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    {State6b, rdx} = ?BACKEND:move_to_native_register(State6a, {x_reg, 6}),
    {State6c, rsi} = ?BACKEND:move_to_native_register(State6b, {x_reg, 7}),
    {State6, rdi} = ?BACKEND:move_to_native_register(State6c, {x_reg, 8}),
    %% Every allocatable scratch register is now in use.
    ?assertEqual([], ?BACKEND:available_regs(State6)),
    State7 = ?BACKEND:if_block(
        State6,
        {rax, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
        fun(BSt0) ->
            ?BACKEND:add(BSt0, r11, 2)
        end
    ),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
        "   8:	4d 8b 56 68          	mov    0x68(%r14),%r10\n"
        "   c:	4d 8b 4e 70          	mov    0x70(%r14),%r9\n"
        "  10:	4d 8b 46 78          	mov    0x78(%r14),%r8\n"
        "  14:	49 8b 8e 80 00 00 00 	mov    0x80(%r14),%rcx\n"
        "  1b:	49 8b 96 88 00 00 00 	mov    0x88(%r14),%rdx\n"
        "  22:	49 8b b6 90 00 00 00 	mov    0x90(%r14),%rsi\n"
        "  29:	49 8b be 98 00 00 00 	mov    0x98(%r14),%rdi\n"
        "  30:	50                   	push   %rax\n"
        "  31:	24 0f                	and    $0xf,%al\n"
        "  33:	80 f8 0f             	cmp    $0xf,%al\n"
        "  36:	58                   	pop    %rax\n"
        "  37:	0f 84 04 00 00 00    	je     0x41\n"
        "  3d:	49 83 c3 02          	add    $0x2,%r11"
    >>,
    ?assertStream(x86_64, Dump, Stream),
    %% Reg is restored by popq, so it stays allocated after the block.
    ?assertEqual([rcx, r8, r9, r10, r11, rax, rdx, rsi, rdi], ?BACKEND:used_regs(State7)).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	41 ff 4d 10          	decl   0x10(%r13)\n"
            "   4:	75 16                	jne    0x1c\n"
            "   6:	48 8d 05 0f 00 00 00 	lea    0xf(%rip),%rax        # 0x1c\n"
            "   d:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
            "  11:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  15:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  19:	ff 63 10             	jmp    *0x10(%rbx)\n"
            "  1c:	bf 02 00 00 00       	mov    $0x2,%edi\n"
            "  21:	48 89 fe             	mov    %rdi,%rsi\n"
            "  24:	48 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%rdx\n"
            "  2b:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  2f:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  33:	ff 63 20             	jmp    *0x20(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	41 ff 4d 10          	decl   0x10(%r13)\n"
            "   4:	75 16                	jne    0x1c\n"
            "   6:	48 8d 05 0f 00 00 00 	lea    0xf(%rip),%rax        # 0x1c\n"
            "   d:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
            "  11:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  15:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  19:	ff 63 10             	jmp    *0x10(%rbx)\n"
            "  1c:	bf 02 00 00 00       	mov    $0x2,%edi\n"
            "  21:	48 89 fe             	mov    %rdi,%rsi\n"
            "  24:	ba 0a 00 00 00       	mov    $0xa,%edx\n"
            "  29:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  2d:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  31:	ff 63 20             	jmp    *0x20(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	bf 2a 00 00 00       	mov    $0x2a,%edi\n"
            "   5:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "   9:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "   d:	ff 23                	jmp    *(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	b8 01 00 00 00       	mov    $0x1,%eax\n"
        "   5:	4d 8b 5e 58          	mov    0x58(%r14),%r11\n"
        "   9:	48 85 c0             	test   %rax,%rax\n"
        "   c:	75 0a                	jne    0x18\n"
        "   e:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  12:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  16:	ff 23                	jmp    *(%rbx)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

move_imported_bif_to_native_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_imported_bif_to_native_register(State0, 3),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	49 8b 45 00          	mov    0x0(%r13),%rax\n"
            "   4:	48 8b 80 90 00 00 00 	mov    0x90(%rax),%rax\n"
            "   b:	48 8b 40 18          	mov    0x18(%rax),%rax\n"
            "   f:	48 8b 40 08          	mov    0x8(%rax),%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

jump_to_label_cond_testb_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:jump_to_label_cond(State1, {{free, Reg}, '&', 16#3, '!=', 0}, 42),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	a8 03                	test   $0x3,%al\n"
        "   6:	0f 85 fc ff ff ff    	jne    0x8"
    >>,
    ?assertStream(x86_64, Dump, Stream).

jump_to_label_cond_andb_cmpb_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:jump_to_label_cond(State1, {{free, Reg}, '&', 16#3, '!=', 16#2}, 42),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	24 03                	and    $0x3,%al\n"
        "   6:	80 f8 02             	cmp    $0x2,%al\n"
        "   9:	0f 85 fc ff ff ff    	jne    0xb"
    >>,
    ?assertStream(x86_64, Dump, Stream).

jump_to_label_cond_backward_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 8),
    State2 = ?BACKEND:add_label(State1, 7),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:jump_to_label_cond(State3, {{free, Reg}, '&', 16#3, '!=', 0}, 7),
    ?BACKEND:assert_all_native_free(State4),
    Stream = ?BACKEND:stream(State4),
    %% Jump table = 9 slots of 5 bytes (labels 0..8) = 45 = 0x2d; label 7
    %% lands at 0x2d, the test/jne follow.
    <<_:16#2d/binary, Code/binary>> = Stream,
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	a8 03                	test   $0x3,%al\n"
        "   6:	75 f8                	jne    0x0"
    >>,
    ?assertStream(x86_64, Dump, Code).

jump_to_label_cond_fallback_test() ->
    %% Unsupported condition shape falls back to if_block + jmp.
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:jump_to_label_cond(State1, {{free, Reg}, '<', 0}, 42),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	48 85 c0             	test   %rax,%rax\n"
        "   7:	7d 05                	jge    0xe\n"
        "   9:	e9 fc ff ff ff       	jmp    0xa"
    >>,
    ?assertStream(x86_64, Dump, Stream).

jump_to_label_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_label(BSt0, 42)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	b8 01 00 00 00       	mov    $0x1,%eax\n"
        "   5:	4d 8b 5e 58          	mov    0x58(%r14),%r11\n"
        "   9:	48 85 c0             	test   %rax,%rax\n"
        "   c:	75 05                	jne    0x13\n"
        "   e:	e9 fc ff ff ff       	jmp    0xf"
    >>,
    ?assertStream(x86_64, Dump, Stream).

jump_to_offset_if_block_preserves_cache_test() ->
    State0 = terminal_if_preserves_cached_x_reg0(unreachable_test_state(), fun(BSt0) ->
        ?BACKEND:jump_to_offset(BSt0, 16#100)
    end),
    Stream = ?BACKEND:stream(State0),
    Dump = <<
        "   0:	b8 01 00 00 00       	mov    $0x1,%eax\n"
        "   5:	4d 8b 5e 58          	mov    0x58(%r14),%r11\n"
        "   9:	48 85 c0             	test   %rax,%rax\n"
        "   c:	75 05                	jne    0x13\n"
        "   e:	e9 ed 00 00 00       	jmp    0x100"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	b8 00 01 00 00       	mov    $0x100,%eax\n"
        "   5:	41 bb 01 00 00 00    	mov    $0x1,%r11d\n"
        "   b:	4d 8b 56 58          	mov    0x58(%r14),%r10\n"
        "   f:	4d 85 db             	test   %r11,%r11\n"
        "  12:	75 0d                	jne    0x21\n"
        "  14:	4c 8d 1d e5 ff ff ff 	lea    -0x1b(%rip),%r11        # 0x0\n"
        "  1b:	49 01 c3             	add    %rax,%r11\n"
        "  1e:	41 ff e3             	jmp    *%r11"
    >>,
    ?assertStream(x86_64, Dump, Stream).

move_array_element_x_reg_invalidates_vm_loc_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
    {State2, r11} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:move_array_element(State2, r11, 0, {x_reg, 5}),
    {State4, _Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 5}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	49 8b 86 80 00 00 00 	mov    0x80(%r14),%rax\n"
        "   7:	4d 8b 5e 58          	mov    0x58(%r14),%r11\n"
        "   b:	4d 8b 13             	mov    (%r11),%r10\n"
        "   e:	4d 89 96 80 00 00 00 	mov    %r10,0x80(%r14)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
                    ?assertEqual(rax, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:	50                   	push   %rax\n"
                            "   1:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
                            "   5:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
                            "   9:	48 8b 83 a8 00 00 00 	mov    0xa8(%rbx),%rax\n"
                            "  10:	ff d0                	call   *%rax\n"
                            "  12:	41 5b                	pop    %r11\n"
                            "  14:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
                            "  18:	4d 8b 7e 50          	mov    0x50(%r14),%r15\n"
                            "  1c:	4c 39 f0             	cmp    %r14,%rax\n"
                            "  1f:	74 01                	je     0x22\n"
                            "  21:	c3                   	ret"
                        >>,
                    ?assertStream(x86_64, Dump, Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(rax, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(r11, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:	50                   	push   %rax\n"
                            "   1:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
                            "   5:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
                            "   9:	48 8b 83 a8 00 00 00 	mov    0xa8(%rbx),%rax\n"
                            "  10:	ff d0                	call   *%rax\n"
                            "  12:	41 5b                	pop    %r11\n"
                            "  14:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
                            "  18:	4d 8b 7e 50          	mov    0x50(%r14),%r15\n"
                            "  1c:	49 89 c3             	mov    %rax,%r11\n"
                            "  1f:	4d 39 f3             	cmp    %r14,%r11\n"
                            "  22:	74 04                	je     0x28\n"
                            "  24:	4c 89 d8             	mov    %r11,%rax\n"
                            "  27:	c3                   	ret"
                        >>,
                    ?assertStream(x86_64, Dump, Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	49 8b 07             	mov    (%r15),%rax\n"
            "   3:	49 89 86 e0 00 00 00 	mov    %rax,0xe0(%r14)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	49 83 c7 38          	add    $0x38,%r15"
        >>,
    ?assertStream(x86_64, Dump, Stream).

heap_bump_alloc_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:heap_bump_alloc(State0, 2),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	4c 89 e0             	mov    %r12,%rax\n"
            "   3:	49 83 c4 10          	add    $0x10,%r12"
        >>,
    ?assertStream(x86_64, Dump, Stream).

read_heap_fragments_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:read_heap_fragments(State0),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	49 8b 46 08          	mov    0x8(%r14),%rax\n"
            "   4:	48 8b 00             	mov    (%rax),%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

allocate_frame_fast_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:allocate_frame_fast(State0, 2),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	49 83 ef 18          	sub    $0x18,%r15\n"
            "   4:	49 8b 86 e0 00 00 00 	mov    0xe0(%r14),%rax\n"
            "   b:	49 89 47 10          	mov    %rax,0x10(%r15)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	7d 04                	jge    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	4c 39 d8             	cmp    %r11,%rax\n"
                        "   b:	7d 04                	jge    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '(uint)>', 60},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 3c          	cmp    $0x3c,%rax\n"
                        "   c:	0f 86 04 00 00 00    	jbe    0x16\n"
                        "  12:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '(uint)>', 16#4000000000000000},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	49 ba 00 00 00 00 00 	movabs $0x4000000000000000,%r10\n"
                        "   f:	00 00 40 \n"
                        "  12:	4c 39 d0             	cmp    %r10,%rax\n"
                        "  15:	0f 86 04 00 00 00    	jbe    0x1f\n"
                        "  1b:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	85 c0                	test   %eax,%eax\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	85 c0                	test   %eax,%eax\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 85 c0             	test   %rax,%rax\n"
                        "   b:	74 04                	je     0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	85 c0                	test   %eax,%eax\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	74 04                	je     0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	74 04                	je     0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	74 04                	je     0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	74 04                	je     0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	75 04                	jne    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
                        "   c:	75 04                	jne    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	83 f8 2a             	cmp    $0x2a,%eax\n"
                        "   b:	75 04                	jne    0x11\n"
                        "   d:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	75 04                	jne    0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	84 c0                	test   %al,%al\n"
                        "   a:	74 04                	je     0x10\n"
                        "   c:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	a8 07                	test   $0x7,%al\n"
                        "   a:	0f 84 04 00 00 00    	je     0x14\n"
                        "  10:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	a8 07                	test   $0x7,%al\n"
                        "   a:	0f 84 04 00 00 00    	je     0x14\n"
                        "  10:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	49 89 c2             	mov    %rax,%r10\n"
                        "   b:	41 80 e2 0f          	and    $0xf,%r10b\n"
                        "   f:	41 80 fa 0f          	cmp    $0xf,%r10b\n"
                        "  13:	0f 84 04 00 00 00    	je     0x1d\n"
                        "  19:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	24 0f                	and    $0xf,%al\n"
                        "   a:	80 f8 0f             	cmp    $0xf,%al\n"
                        "   d:	0f 84 04 00 00 00    	je     0x17\n"
                        "  13:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 64          	cmp    $0x64,%rax\n"
                        "   c:	7e 04                	jle    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 64          	cmp    $0x64,%rax\n"
                        "   c:	7e 04                	jle    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 64          	cmp    $0x64,%rax\n"
                        "   c:	7d 04                	jge    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	48 83 f8 64          	cmp    $0x64,%rax\n"
                        "   c:	7d 04                	jge    0x12\n"
                        "   e:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 16#100000000},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	49 ba 00 00 00 00 01 	movabs $0x100000000,%r10\n"
                        "   f:	00 00 00 \n"
                        "  12:	4c 39 d0             	cmp    %r10,%rax\n"
                        "  15:	7d 04                	jge    0x1b\n"
                        "  17:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '<', 16#100000000},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	49 ba 00 00 00 00 01 	movabs $0x100000000,%r10\n"
                        "   f:	00 00 00 \n"
                        "  12:	4c 39 d0             	cmp    %r10,%rax\n"
                        "  15:	7d 04                	jge    0x1b\n"
                        "  17:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {16#100000000, '<', RegA},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	49 ba 00 00 00 00 01 	movabs $0x100000000,%r10\n"
                        "   f:	00 00 00 \n"
                        "  12:	4c 39 d0             	cmp    %r10,%rax\n"
                        "  15:	7e 04                	jle    0x1b\n"
                        "  17:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual([RegB, RegA], ?BACKEND:used_regs(State1))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {16#100000000, '<', {free, RegA}},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                        "   8:	49 ba 00 00 00 00 01 	movabs $0x100000000,%r10\n"
                        "   f:	00 00 00 \n"
                        "  12:	4c 39 d0             	cmp    %r10,%rax\n"
                        "  15:	7e 04                	jle    0x1b\n"
                        "  17:	49 83 c3 02          	add    $0x2,%r11"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
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
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
            "   8:	48 83 f8 3b          	cmp    $0x3b,%rax\n"
            "   c:	75 09                	jne    0x17\n"
            "   e:	49 83 c3 02          	add    $0x2,%r11\n"
            "  12:	e9 04 00 00 00       	jmp    0x1b\n"
            "  17:	49 83 c3 04          	add    $0x4,%r11"
        >>,
    ?assertStream(x86_64, Dump, Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 3),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 c1 e8 03          	shr    $0x3,%rax"
                >>,
            ?assertStream(x86_64, Dump, Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 3),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	49 89 c3             	mov    %rax,%r11\n"
                    "   7:	49 c1 eb 03          	shr    $0x3,%r11"
                >>,
            ?assertStream(x86_64, Dump, Stream)
        end)
    ].

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	48 c1 e0 03          	shl    $0x3,%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
            "   0:	e9 35 00 00 00       	jmp    0x3a\n"
            "   5:	e9 05 00 00 00       	jmp    0xf\n"
            "   a:	e9 21 00 00 00       	jmp    0x30\n"
            "   f:	41 ff 4d 10          	decl   0x10(%r13)\n"
            "  13:	74 05                	je     0x1a\n"
            "  15:	e9 16 00 00 00       	jmp    0x30\n"
            "  1a:	48 8d 05 0f 00 00 00 	lea    0xf(%rip),%rax        # 0x30\n"
            "  21:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
            "  25:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  29:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  2d:	ff 63 10             	jmp    *0x10(%rbx)\n"
            "  30:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  34:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  38:	ff 23                	jmp    *(%rbx)\n"
            "  3a:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  3e:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  42:	ff 63 08             	jmp    *0x8(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

call_only_or_schedule_next_known_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#2a),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:	e9 35 00 00 00       	jmp    0x3a\n"
            "   5:	e9 05 00 00 00       	jmp    0xf\n"
            "   a:	e9 1b 00 00 00       	jmp    0x2a\n"
            "   f:	41 ff 4d 10          	decl   0x10(%r13)\n"
            "  13:	74 05                	je     0x1a\n"
            "  15:	e9 10 00 00 00       	jmp    0x2a\n"
            "  1a:	48 8d 05 09 00 00 00 	lea    0x9(%rip),%rax        # 0x2a\n"
            "  21:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
            "  25:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  29:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  2d:	ff 63 10             	jmp    *0x10(%rbx)\n"
            "  30:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  34:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  38:	ff 23                	jmp    *(%rbx)\n"
            "  3a:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  3e:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  42:	ff 63 08             	jmp    *0x8(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
            "   0:	50                   	push   %rax\n"
            "   1:	bf 02 00 00 00       	mov    $0x2,%edi\n"
            "   6:	48 8b 43 40          	mov    0x40(%rbx),%rax\n"
            "   a:	ff d0                	call   *%rax\n"
            "   c:	41 5b                	pop    %r11\n"
            "   e:	50                   	push   %rax\n"
            "   f:	48 bf cd ab 02 be ba 	movabs $0x7fcafebabe02abcd,%rdi\n"
            "  16:	fe ca 7f \n"
            "  19:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  1d:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  21:	48 8b 43 78          	mov    0x78(%rbx),%rax\n"
            "  25:	ff d0                	call   *%rax\n"
            "  27:	49 89 c3             	mov    %rax,%r11\n"
            "  2a:	58                   	pop    %rax\n"
            "  2b:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  2f:	4d 8b 7e 50          	mov    0x50(%r14),%r15\n"
            "  33:	50                   	push   %rax\n"
            "  34:	4c 89 f7             	mov    %r14,%rdi\n"
            "  37:	31 f6                	xor    %esi,%esi\n"
            "  39:	ba 01 00 00 00       	mov    $0x1,%edx\n"
            "  3e:	49 8b 4e 58          	mov    0x58(%r14),%rcx\n"
            "  42:	4d 89 d8             	mov    %r11,%r8\n"
            "  45:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  49:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  4d:	ff d0                	call   *%rax\n"
            "  4f:	41 5b                	pop    %r11\n"
            "  51:	4d 8b 66 18          	mov    0x18(%r14),%r12\n"
            "  55:	4d 8b 7e 50          	mov    0x50(%r14),%r15\n"
            "  59:	48 85 c0             	test   %rax,%rax\n"
            "  5c:	75 10                	jne    0x6e\n"
            "  5e:	bf 5e 00 00 00       	mov    $0x5e,%edi\n"
            "  63:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  67:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  6b:	ff 63 30             	jmp    *0x30(%rbx)\n"
            "  6e:	49 89 46 58          	mov    %rax,0x58(%r14)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "   8:	4c 8b 58 08          	mov    0x8(%rax),%r11\n"
        "   c:	4d 89 5f 08          	mov    %r11,0x8(%r15)\n"
        "  10:	4c 8b 18             	mov    (%rax),%r11\n"
        "  13:	4d 89 1f             	mov    %r11,(%r15)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	e9 ff ff ff ff       	jmp    0x4\n"
        "   5:	e9 42 01 00 00       	jmp    0x14c\n"
        "   a:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   e:	49 89 c3             	mov    %rax,%r11\n"
        "  11:	41 80 e3 0f          	and    $0xf,%r11b\n"
        "  15:	41 80 fb 0f          	cmp    $0xf,%r11b\n"
        "  19:	0f 84 2d 00 00 00    	je     0x4c\n"
        "  1f:	49 89 c3             	mov    %rax,%r11\n"
        "  22:	41 80 e3 03          	and    $0x3,%r11b\n"
        "  26:	41 80 fb 02          	cmp    $0x2,%r11b\n"
        "  2a:	0f 84 05 00 00 00    	je     0x35\n"
        "  30:	e9 17 01 00 00       	jmp    0x14c\n"
        "  35:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "  39:	48 8b 00             	mov    (%rax),%rax\n"
        "  3c:	24 3b                	and    $0x3b,%al\n"
        "  3e:	80 f8 08             	cmp    $0x8,%al\n"
        "  41:	0f 84 05 00 00 00    	je     0x4c\n"
        "  47:	e9 00 01 00 00       	jmp    0x14c"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	e9 ff ff ff ff       	jmp    0x4\n"
        "   5:	e9 53 01 00 00       	jmp    0x15d\n"
        "   a:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   e:	49 89 c3             	mov    %rax,%r11\n"
        "  11:	41 80 e3 0f          	and    $0xf,%r11b\n"
        "  15:	41 80 fb 0f          	cmp    $0xf,%r11b\n"
        "  19:	0f 84 3e 00 00 00    	je     0x5d\n"
        "  1f:	49 89 c3             	mov    %rax,%r11\n"
        "  22:	41 80 e3 03          	and    $0x3,%r11b\n"
        "  26:	41 80 fb 02          	cmp    $0x2,%r11b\n"
        "  2a:	0f 84 05 00 00 00    	je     0x35\n"
        "  30:	e9 28 01 00 00       	jmp    0x15d\n"
        "  35:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "  39:	48 8b 00             	mov    (%rax),%rax\n"
        "  3c:	49 89 c3             	mov    %rax,%r11\n"
        "  3f:	41 80 e3 3b          	and    $0x3b,%r11b\n"
        "  43:	41 80 fb 08          	cmp    $0x8,%r11b\n"
        "  47:	0f 84 10 00 00 00    	je     0x5d\n"
        "  4d:	24 3f                	and    $0x3f,%al\n"
        "  4f:	80 f8 18             	cmp    $0x18,%al\n"
        "  52:	0f 84 05 00 00 00    	je     0x5d\n"
        "  58:	e9 00 01 00 00       	jmp    0x15d"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	e9 ff ff ff ff       	jmp    0x4\n"
        "   5:	e9 15 01 00 00       	jmp    0x11f\n"
        "   a:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   e:	48 83 f8 4b          	cmp    $0x4b,%rax\n"
        "  12:	74 0b                	je     0x1f\n"
        "  14:	48 83 f8 0b          	cmp    $0xb,%rax\n"
        "  18:	74 05                	je     0x1f\n"
        "  1a:	e9 00 01 00 00       	jmp    0x11f"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% After a call that returns (call_primitive_with_cp), code is reachable
%% again: a later if_else_block merge must intersect both arms' register
%% caches instead of taking one arm verbatim because the other is flagged
%% unreachable.
call_primitive_with_cp_resumes_reachable_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_with_cp(State0, 4, [ctx, jit_state]),
    % element 9 of #state{} is the regs cache
    RegsAfter = element(9, State1),
    Other = jit_regs:set_contents(jit_regs:new(0, 0), rdi, {y_reg, 0}),
    Merged = jit_regs:merge(Other, RegsAfter, 16#FFFF),
    ?assertEqual(#{}, jit_regs:get_all_contents(Merged)).

call_ext_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	41 ff 4d 10          	decl   0x10(%r13)\n"
        "   4:	75 16                	jne    0x1c\n"
        "   6:	48 8d 05 0f 00 00 00 	lea    0xf(%rip),%rax        # 0x1c\n"
        "   d:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
        "  11:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  15:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  19:	ff 63 10             	jmp    *0x10(%rbx)\n"
        "  1c:	49 8b 45 00          	mov    0x0(%r13),%rax\n"
        "  20:	8b 00                	mov    (%rax),%eax\n"
        "  22:	48 c1 e0 18          	shl    $0x18,%rax\n"
        "  26:	48 0d 3c 01 00 00    	or     $0x13c,%rax\n"
        "  2c:	49 89 86 e0 00 00 00 	mov    %rax,0xe0(%r14)\n"
        "  33:	bf 02 00 00 00       	mov    $0x2,%edi\n"
        "  38:	be 05 00 00 00       	mov    $0x5,%esi\n"
        "  3d:	48 c7 c2 ff ff ff ff 	mov    $0xffffffffffffffff,%rdx\n"
        "  44:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  48:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  4c:	ff 63 20             	jmp    *0x20(%rbx)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	41 ff 4d 10          	decl   0x10(%r13)\n"
        "   4:	75 16                	jne    0x1c\n"
        "   6:	48 8d 05 0f 00 00 00 	lea    0xf(%rip),%rax        # 0x1c\n"
        "   d:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
        "  11:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  15:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  19:	ff 63 10             	jmp    *0x10(%rbx)\n"
        "  1c:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "  20:	49 89 c3             	mov    %rax,%r11\n"
        "  23:	4d 89 da             	mov    %r11,%r10\n"
        "  26:	41 80 e2 03          	and    $0x3,%r10b\n"
        "  2a:	41 80 fa 02          	cmp    $0x2,%r10b\n"
        "  2e:	0f 84 1b 00 00 00    	je     0x4f\n"
        "  34:	bf 34 00 00 00       	mov    $0x34,%edi\n"
        "  39:	be 8b 01 00 00       	mov    $0x18b,%esi\n"
        "  3e:	4c 89 da             	mov    %r11,%rdx\n"
        "  41:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  45:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  49:	ff a3 98 00 00 00    	jmp    *0x98(%rbx)\n"
        "  4f:	49 83 e3 fc          	and    $0xfffffffffffffffc,%r11\n"
        "  53:	4d 8b 1b             	mov    (%r11),%r11\n"
        "  56:	4d 89 da             	mov    %r11,%r10\n"
        "  59:	41 80 e2 3f          	and    $0x3f,%r10b\n"
        "  5d:	41 80 fa 14          	cmp    $0x14,%r10b\n"
        "  61:	0f 84 1b 00 00 00    	je     0x82\n"
        "  67:	bf 67 00 00 00       	mov    $0x67,%edi\n"
        "  6c:	be 8b 01 00 00       	mov    $0x18b,%esi\n"
        "  71:	4c 89 da             	mov    %r11,%rdx\n"
        "  74:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  78:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  7c:	ff a3 98 00 00 00    	jmp    *0x98(%rbx)\n"
        "  82:	4d 8b 5d 00          	mov    0x0(%r13),%r11\n"
        "  86:	45 8b 1b             	mov    (%r11),%r11d\n"
        "  89:	49 c1 e3 18          	shl    $0x18,%r11\n"
        "  8d:	49 81 cb b8 02 00 00 	or     $0x2b8,%r11\n"
        "  94:	4d 89 9e e0 00 00 00 	mov    %r11,0xe0(%r14)\n"
        "  9b:	48 89 c7             	mov    %rax,%rdi\n"
        "  9e:	31 f6                	xor    %esi,%esi\n"
        "  a0:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  a4:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  a8:	ff a3 00 01 00 00    	jmp    *0x100(%rbx)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

decrement_reductions_invalidates_cache_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [Reg]),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    {State4, Reg} = ?BACKEND:move_to_native_register(State3, {x_reg, 0}),
    Stream = ?BACKEND:stream(State4),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	41 ff 4d 10          	decl   0x10(%r13)\n"
        "   8:	75 16                	jne    0x20\n"
        "   a:	48 8d 05 0f 00 00 00 	lea    0xf(%rip),%rax        # 0x20\n"
        "  11:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
        "  15:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
        "  19:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
        "  1d:	ff 63 10             	jmp    *0x10(%rbx)\n"
        "  20:	49 8b 46 58          	mov    0x58(%r14),%rax"
    >>,
    ?assertStream(x86_64, Dump, Stream).

move_to_vm_register_test0(State, Source, Dest, Dump) ->
    State1 = ?BACKEND:move_to_vm_register(State, Source, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(x86_64, Dump, Stream).

move_to_vm_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, 0}, <<
                        "   0:	49 83 66 58 00       	andq   $0x0,0x58(%r14)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:	49 83 a6 d8 00 00 00 	andq   $0x0,0xd8(%r14)\n"
                        "   7:	00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, r10}, <<
                        "   0:	49 83 22 00          	andq   $0x0,(%r10)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:	49 83 67 10 00       	andq   $0x0,0x10(%r15)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:	49 83 a7 a0 00 00 00 	andq   $0x0,0xa0(%r15)\n"
                        "   7:	00 "
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:	49 c7 46 58 2a 00 00 	movq   $0x2a,0x58(%r14)\n"
                        "   7:	00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:	49 c7 86 d8 00 00 00 	movq   $0x2a,0xd8(%r14)\n"
                        "   7:	2a 00 00 00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:	49 c7 47 10 2a 00 00 	movq   $0x2a,0x10(%r15)\n"
                        "   7:	00 "
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:	49 c7 87 a0 00 00 00 	movq   $0x2a,0xa0(%r15)\n"
                        "   7:	2a 00 00 00 "
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, r10}, <<
                        "   0:	49 c7 02 63 00 00 00 	movq   $0x63,(%r10)"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:	49 8b 46 60          	mov    0x60(%r14),%rax\n"
                        "   4:	49 89 46 68          	mov    %rax,0x68(%r14)"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, r8}, <<
                        "   0:	49 8b 46 60          	mov    0x60(%r14),%rax\n"
                        "   4:	49 89 00             	mov    %rax,(%r8)"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, r9}, {x_reg, 3}, <<
                        "   0:	49 8b 01             	mov    (%r9),%rax\n"
                        "   3:	49 89 46 70          	mov    %rax,0x70(%r14)"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	49 89 47 08          	mov    %rax,0x8(%r15)"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:	49 8b 07             	mov    (%r15),%rax\n"
                        "   3:	49 89 46 70          	mov    %rax,0x70(%r14)"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:	49 8b 47 08          	mov    0x8(%r15),%rax\n"
                        "   4:	49 89 46 70          	mov    %rax,0x70(%r14)"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {x_reg, 0}, <<
                        "   0:	49 89 46 58          	mov    %rax,0x58(%r14)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {x_reg, extra}, <<
                        "   0:	49 89 86 d8 00 00 00 	mov    %rax,0xd8(%r14)"
                    >>)
                end),
                %% Test: Atom register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {ptr, r10}, <<
                        "   0:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, rax, {y_reg, 0}, <<
                        "   0:	49 89 07             	mov    %rax,(%r15)"
                    >>)
                end),
                %% Test: Large immediate to x_reg (movabsq)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, 0}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	49 89 46 58          	mov    %rax,0x58(%r14)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {x_reg, extra}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	49 89 86 d8 00 00 00 	mov    %rax,0xd8(%r14)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 2}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	49 89 47 10          	mov    %rax,0x10(%r15)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {y_reg, 20}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	49 89 87 a0 00 00 00 	mov    %rax,0xa0(%r15)"
                    >>)
                end),
                %% Test: Large immediate to ptr (movabsq)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#123456789abcdef0, {ptr, r10}, <<
                        "   0:	48 b8 f0 de bc 9a 78 	movabs $0x123456789abcdef0,%rax\n"
                        "   7:	56 34 12 \n"
                        "   a:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:	49 8b 86 d0 00 00 00 	mov    0xd0(%r14),%rax\n"
                        "   7:	49 89 87 f8 00 00 00 	mov    %rax,0xf8(%r15)"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, extra}, {y_reg, 31}, <<
                        "   0:	49 8b 86 d8 00 00 00 	mov    0xd8(%r14),%rax\n"
                        "   7:	49 89 87 f8 00 00 00 	mov    %rax,0xf8(%r15)"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:	49 8b 87 f8 00 00 00 	mov    0xf8(%r15),%rax\n"
                        "   7:	49 89 86 d0 00 00 00 	mov    %rax,0xd0(%r14)"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:	49 c7 46 58 ff ff ff 	movq   $0xffffffffffffffff,0x58(%r14)\n"
                        "   7:	ff "
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	48 8b 40 08          	mov    0x8(%rax),%rax\n"
                        "   8:	4d 8b 5d 18          	mov    0x18(%r13),%r11\n"
                        "   c:	49 89 43 18          	mov    %rax,0x18(%r11)"
                    >>,
                    ?assertStream(x86_64, Dump, Stream)
                end)
            ]
        end}.

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    ?assertStream(x86_64, Dump, Stream).

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
                        "   0:	49 8b 40 10          	mov    0x10(%r8),%rax\n"
                        "   4:	49 89 46 58          	mov    %rax,0x58(%r14)"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, r8, 3, {ptr, r10}, <<
                        "   0:	49 8b 40 18          	mov    0x18(%r8),%rax\n"
                        "   4:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, {y_reg, 2}, <<
                        "   0:	49 8b 40 08          	mov    0x8(%r8),%rax\n"
                        "   4:	49 89 47 10          	mov    %rax,0x10(%r15)"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (r10)
                ?_test(begin
                    move_array_element_test0(State0, r8, 1, r10, <<
                        "   0:	4d 8b 50 08          	mov    0x8(%r8),%r10"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg (high index)
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {y_reg, 31}, <<
                        "   0:	49 8b 40 38          	mov    0x38(%r8),%rax\n"
                        "   4:	49 89 87 f8 00 00 00 	mov    %rax,0xf8(%r15)"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg (high index)
                ?_test(begin
                    move_array_element_test0(State0, r8, 7, {x_reg, 15}, <<
                        "   0:	49 8b 40 38          	mov    0x38(%r8),%rax\n"
                        "   4:	49 89 86 d0 00 00 00 	mov    %rax,0xd0(%r14)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {x_reg, 2}, <<
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax\n"
                        "   4:	48 c1 e0 03          	shl    $0x3,%rax\n"
                        "   8:	4c 01 c0             	add    %r8,%rax\n"
                        "   b:	48 8b 00             	mov    (%rax),%rax\n"
                        "   e:	49 89 46 68          	mov    %rax,0x68(%r14)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {ptr, r10}, <<
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax\n"
                        "   4:	48 c1 e0 03          	shl    $0x3,%rax\n"
                        "   8:	4c 01 c0             	add    %r8,%rax\n"
                        "   b:	48 8b 00             	mov    (%rax),%rax\n"
                        "   e:	49 89 02             	mov    %rax,(%r10)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, r8, 4),
                    move_array_element_test0(State1, r8, {free, Reg}, {y_reg, 31}, <<
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax\n"
                        "   4:	48 c1 e0 03          	shl    $0x3,%rax\n"
                        "   8:	4c 01 c0             	add    %r8,%rax\n"
                        "   b:	48 8b 00             	mov    (%rax),%rax\n"
                        "   e:	49 89 87 f8 00 00 00 	mov    %rax,0xf8(%r15)"
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
                        "   0:	49 8b 40 20          	mov    0x20(%r8),%rax"
                    >>,
                    ?assertStream(x86_64, Dump, Stream),
                    ?assertEqual(rax, Reg)
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
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	49 89 40 10          	mov    %rax,0x10(%r8)"
                    >>,
                    ?assertStream(x86_64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r8, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                        "   4:	49 89 40 18          	mov    %rax,0x18(%r8)"
                    >>,
                    ?assertStream(x86_64, Dump, Stream)
                end)
            ]
        end}.

%% Test jump_to_continuation optimization for intra-module returns
jump_to_continuation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_to_continuation(State0, {free, rax}),
    Stream = ?BACKEND:stream(State1),
    % Expected: leaq -0x7(%rip), %rax; addq %rax, %rax; jmpq *%rax
    % With default offset 0, NetOffset = 0 - 0 = 0, but RIP-relative needs adjustment for instruction length
    Dump =
        <<
            "   0:	48 8d 05 f9 ff ff ff 	lea    -0x7(%rip),%rax\n"
            "   7:	48 01 c0             	add    %rax,%rax\n"
            "   a:	ff e0                	jmpq   *%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% Test set_continuation_to_label with unknown label
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
            "   0:	e9 ff ff ff ff       	jmp    0x4\n"
            "   5:	e9 14 00 00 00       	jmp    0x1e\n"
            "   a:	e9 f1 00 00 00       	jmp    0x100\n"
            "   f:	e9 ff ff ff ff       	jmp    0x13\n"
            "  14:	e9 ff ff ff ff       	jmp    0x18\n"
            "  19:	e9 ff ff ff ff       	jmp    0x1d\n"
            "  1e:	48 8d 05 db 00 00 00 	lea    0xdb(%rip),%rax        # 0x100\n"
            "  25:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
            "  29:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  2d:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  31:	ff a3 e8 00 00 00    	jmp    *0xe8(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

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
            "   0:	e9 ff ff ff ff       	jmp    0x4\n"
            "   5:	e9 14 00 00 00       	jmp    0x1e\n"
            "   a:	e9 f1 00 00 00       	jmp    0x100\n"
            "   f:	e9 ff ff ff ff       	jmp    0x13\n"
            "  14:	e9 ff ff ff ff       	jmp    0x18\n"
            "  19:	e9 ff ff ff ff       	jmp    0x1d\n"
            "  1e:	48 8d 05 db 00 00 00 	lea    0xdb(%rip),%rax        # 0x100\n"
            "  25:	49 89 45 08          	mov    %rax,0x8(%r13)\n"
            "  29:	4d 89 66 18          	mov    %r12,0x18(%r14)\n"
            "  2d:	4d 89 7e 50          	mov    %r15,0x50(%r14)\n"
            "  31:	ff a3 e8 00 00 00    	jmp    *0xe8(%rbx)"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% Loading the same x_reg twice should skip the second load
cached_load_same_xreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, rax} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% Loading a different x_reg should emit a load, loading same again should skip
cached_load_different_xreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, r11} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, r11} = ?BACKEND:move_to_native_register(State2, {x_reg, 1}),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
            "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% Loading cp twice should skip the second load
cached_load_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, cp),
    {State2, rax} = ?BACKEND:move_to_native_register(State1, cp),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:	49 8b 86 e0 00 00 00 	mov    0xe0(%r14),%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% After freeing a register, cache is preserved so reload is elided
cached_load_after_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [rax]),
    {State3, rax} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:	49 8b 46 58          	mov    0x58(%r14),%rax"
        >>,
    ?assertStream(x86_64, Dump, Stream).

%% After storing a large immediate to an x_reg, the temp register holding the
%% immediate is cached so a subsequent load of the same value skips the movabsq
cached_move_to_vm_large_imm_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 16#100000000, {x_reg, 0}),
    {State2, rax} = ?BACKEND:move_to_native_register(State1, 16#100000000),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	48 b8 00 00 00 00 01 	movabs $0x100000000,%rax\n"
        "   7:	00 00 00 \n"
        "   a:	49 89 46 58          	mov    %rax,0x58(%r14)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% After copying an x_reg to another vm location, the temp register holding the
%% x_reg value is cached so a subsequent load of the same x_reg skips the mov
cached_move_to_vm_x_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {x_reg, 1}, {x_reg, 0}),
    {State2, rax} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 60          	mov    0x60(%r14),%rax\n"
        "   4:	49 89 46 58          	mov    %rax,0x58(%r14)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% After copying a y_reg to an x_reg, the temp register holding the y_reg value
%% is cached so a subsequent load of the same y_reg skips the movs
cached_move_to_vm_y_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, {y_reg, 0}, {x_reg, 0}),
    {State2, rax} = ?BACKEND:move_to_native_register(State1, {y_reg, 0}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 07             	mov    (%r15),%rax\n"
        "   3:	49 89 46 58          	mov    %rax,0x58(%r14)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% After storing an x_reg value to an array element, the temp register holding
%% the x_reg value is cached so a subsequent load of that x_reg skips the mov
cached_move_to_array_element_x_reg_reuse_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, r11, 2),
    {State2, rax} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	49 89 43 10          	mov    %rax,0x10(%r11)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% After an if_block with a large-immediate condition, the temp register loaded
%% with that immediate is cached, so the block body can reuse it without emitting
%% a redundant movabsq
if_block_large_cond_reuse_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:if_block(State1, {rax, '<', 16#100000000}, fun(BSt0) ->
        {BSt1, _Reg} = ?BACKEND:move_to_native_register(BSt0, 16#100000000),
        BSt1
    end),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	49 bb 00 00 00 00 01 	movabs $0x100000000,%r11\n"
        "   b:	00 00 00 \n"
        "   e:	4c 39 d8             	cmp    %r11,%rax\n"
        "  11:	7d 00                	jge    0x13"
    >>,
    ?assertStream(x86_64, Dump, Stream).

float_op_fadd_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:float_op(State0, ?PRIM_FADD, 1, 2, 3),
    ?assertEqual(rax, Reg),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	4d 8b 5d 18          	mov    0x18(%r13),%r11\n"
        "   4:	f2 41 0f 10 43 08    	movsd  0x8(%r11),%xmm0\n"
        "   a:	f2 41 0f 10 4b 10    	movsd  0x10(%r11),%xmm1\n"
        "  10:	f2 0f 58 c1          	addsd  %xmm1,%xmm0\n"
        "  14:	f2 41 0f 11 43 18    	movsd  %xmm0,0x18(%r11)\n"
        "  1a:	66 48 0f 7e c0       	movq   %xmm0,%rax\n"
        "  1f:	49 bb 00 00 00 00 00 	movabs $0x7ff0000000000000,%r11\n"
        "  26:	00 f0 7f \n"
        "  29:	4c 21 d8             	and    %r11,%rax\n"
        "  2c:	4c 31 d8             	xor    %r11,%rax\n"
        "  2f:	0f 95 c0             	setne  %al"
    >>,
    ?assertStream(x86_64, Dump, Stream).

float_op_fdiv_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:float_op(State0, ?PRIM_FDIV, 1, 2, 3),
    ?assertEqual(rax, Reg),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	4d 8b 5d 18          	mov    0x18(%r13),%r11\n"
        "   4:	f2 41 0f 10 43 08    	movsd  0x8(%r11),%xmm0\n"
        "   a:	f2 41 0f 10 4b 10    	movsd  0x10(%r11),%xmm1\n"
        "  10:	f2 0f 5e c1          	divsd  %xmm1,%xmm0\n"
        "  14:	f2 41 0f 11 43 18    	movsd  %xmm0,0x18(%r11)\n"
        "  1a:	66 48 0f 7e c0       	movq   %xmm0,%rax\n"
        "  1f:	49 bb 00 00 00 00 00 	movabs $0x7ff0000000000000,%r11\n"
        "  26:	00 f0 7f \n"
        "  29:	4c 21 d8             	and    %r11,%rax\n"
        "  2c:	4c 31 d8             	xor    %r11,%rax\n"
        "  2f:	0f 95 c0             	setne  %al"
    >>,
    ?assertStream(x86_64, Dump, Stream).

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
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	4d 8b 5d 18          	mov    0x18(%r13),%r11\n"
        "   8:	f2 48 0f 2a c0       	cvtsi2sd %rax,%xmm0\n"
        "   d:	f2 41 0f 11 43 08    	movsd  %xmm0,0x8(%r11)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

float_conv_float_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, BoxedReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:float_conv_float(State1, {free, BoxedReg}, 1),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
        "   4:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax\n"
        "   8:	f2 0f 10 40 08       	movsd  0x8(%rax),%xmm0\n"
        "   d:	4d 8b 5d 18          	mov    0x18(%r13),%r11\n"
        "  11:	f2 41 0f 11 43 08    	movsd  %xmm0,0x8(%r11)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% jit.erl-driven test: compile a minimal BEAM code chunk through the real
%% jit:compile/8 and assert on the emitted body. This exercises jit.erl's
%% opcode dispatch and register orchestration around the backend emit calls,
%% as opposed to the per-op emission tests above which call ?BACKEND directly.
%%
%% Chunk: OP_LABEL 1 ; OP_MOVE x0 x1 ; OP_TEST_HEAP 2 2 ; OP_INT_CALL_END.
%% The test_heap with Live = 2 keeps x1 observed (GC root walk), so the
%% move must be emitted. Compact-term encodings: literal 1 = 16#10,
%% literal 2 = 16#20, {x_reg,0} = 16#03, {x_reg,1} = 16#13.
jit_move_x0_x1_test() ->
    Chunk =
        <<16:32, 0:32, ?OP_MOVE:32, 1:32, 1:32, ?OP_LABEL, 16#10, ?OP_MOVE, 16#03, 16#13,
            ?OP_TEST_HEAP, 16#20, 16#20, ?OP_INT_CALL_END>>,
    Code = jit_tests_common:compile_chunk(?BACKEND, Chunk),
    %% OP_MOVE x0,x1 emits: mov 0x58(%r14),%rax ; mov %rax,0x60(%r14)
    %%   49 8b 46 58   mov 0x58(%r14),%rax   (load x[0])
    %%   49 89 46 60   mov %rax,0x60(%r14)   (store x[1])
    ?assertMatch(
        {_, _},
        binary:match(Code, <<16#49, 16#8B, 16#46, 16#58, 16#49, 16#89, 16#46, 16#60>>)
    ).

%% Same chunk without the test_heap: nothing observes x1 before int_call_end,
%% so jit_liveness marks the move dead and jit.erl emits no code for it.
jit_dead_move_skipped_test() ->
    Chunk =
        <<16:32, 0:32, ?OP_MOVE:32, 1:32, 1:32, ?OP_LABEL, 16#10, ?OP_MOVE, 16#03, 16#13,
            ?OP_INT_CALL_END>>,
    Code = jit_tests_common:compile_chunk(?BACKEND, Chunk),
    ?assertEqual(
        nomatch,
        binary:match(Code, <<16#48, 16#8B, 16#47, 16#58, 16#48, 16#89, 16#47, 16#60>>)
    ).

%% A compile-time float constant is stored into fr[1] as its raw IEEE-754
%% bits: load the bits, load the fr array base (ctx->fr), store.
move_float_to_fp_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_float_to_fp_reg(State0, 4.0, 1),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	48 b8 00 00 00 00 00 	movabs $0x4010000000000000,%rax\n"
        "   7:	00 10 40 \n"
        "   a:	4d 8b 5d 18          	mov    0x18(%r13),%r11\n"
        "   e:	49 89 43 08          	mov    %rax,0x8(%r11)"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% The gc_bif func pointer is resolved inline rather than via the
%% PRIM_GET_IMPORTED_GCBIF primitive call: an inline extended-register
%% emptiness check (calling PRIM_TRIM_LIVE_REGS only when non-empty) followed
%% by module->imported_funcs[Bif]->bif0_ptr loads.
move_imported_gcbif_to_native_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, rax} = ?BACKEND:move_imported_gcbif_to_native_register(State0, 2, 5),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:	49 8d 86 f8 00 00 00 	lea    0xf8(%r14),%rax\n"
        "   7:	4c 8b 18             	mov    (%rax),%r11\n"
        "   a:	49 39 c3             	cmp    %rax,%r11\n"
        "   d:	74 0e                	je     0x1d\n"
        "   f:	50                   	push   %rax\n"
        "  10:	bf 02 00 00 00       	mov    $0x2,%edi\n"
        "  15:	48 8b 43 38          	mov    0x38(%rbx),%rax\n"
        "  19:	ff d0                	call   *%rax\n"
        "  1b:	41 5b                	pop    %r11\n"
        "  1d:	49 8b 45 00          	mov    0x0(%r13),%rax\n"
        "  21:	48 8b 80 90 00 00 00 	mov    0x90(%rax),%rax\n"
        "  28:	48 8b 40 28          	mov    0x28(%rax),%rax\n"
        "  2c:	48 8b 40 08          	mov    0x8(%rax),%rax"
    >>,
    ?assertStream(x86_64, Dump, Stream).

%% jit.erl-driven test: OP_FMOVE of a float literal into fr[0] resolves the
%% literal at compile time and embeds its IEEE-754 bits as an immediate (no
%% PRIM_MODULE_LOAD_LITERAL call).
%%
%% Chunk: OP_LABEL 1 ; OP_FMOVE literal[1] fr[0] ; OP_INT_CALL_END.
%% fmove encoding: extended-literal tag 16#47, literal 1 = 16#10,
%% extended-fp-register tag 16#27, fr 0 = 16#00.
jit_fmove_literal_test() ->
    Chunk =
        <<16:32, 0:32, ?OP_FMOVE:32, 1:32, 1:32, ?OP_LABEL, 16#10, ?OP_FMOVE, 16#47, 16#10, 16#27,
            16#00, ?OP_INT_CALL_END>>,
    Code = jit_tests_common:compile_chunk(
        ?BACKEND,
        Chunk,
        fun(_) -> undefined end,
        %% LiteralResolver
        fun(1) -> 4.0 end,
        fun(_) -> any end,
        fun(_) -> undefined end,
        fun(_) -> false end
    ),
    %% movabs $0x4010000000000000 (the bits of 4.0) must appear; its 8-byte
    %% little-endian immediate is distinctive.
    ?assertMatch(
        {_, _},
        binary:match(Code, <<0, 0, 0, 0, 0, 0, 16#10, 16#40>>)
    ).

%% Operands past the architecture's immediate / displacement encodings: a
%% 1024-element tuple index, a 200-slot frame, masks and multipliers that need
%% a literal. Each has its own backend clause, and nothing in the test corpus
%% is big enough to reach them.
large_operand_test_() ->
    [
        {"get_array_element at index 1024", fun() ->
            State0 = large_operand_state(),
            {State1, Base} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, _Reg} = ?BACKEND:get_array_element(State1, Base, 1024),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	4c 8b 98 00 20 00 00 	mov    0x2000(%rax),%r11"
                >>
            )
        end},
        {"move_array_element from index 1024 to an x register", fun() ->
            State0 = large_operand_state(),
            {State1, Base} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:move_array_element(State1, Base, 1024, {x_reg, 1}),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	4c 8b 98 00 20 00 00 	mov    0x2000(%rax),%r11\n"
                    "   b:	4d 89 5e 60          	mov    %r11,0x60(%r14)"
                >>
            )
        end},
        {"move_array_element from index 1024 to a y register", fun() ->
            State0 = large_operand_state(),
            {State1, Base} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:move_array_element(State1, Base, 1024, {y_reg, 1}),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	4c 8b 98 00 20 00 00 	mov    0x2000(%rax),%r11\n"
                    "   b:	4d 89 5f 08          	mov    %r11,0x8(%r15)"
                >>
            )
        end},
        {"move_to_array_element at index 1024", fun() ->
            State0 = large_operand_state(),
            {State1, Base} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:move_to_array_element(State1, {x_reg, 1}, Base, 1024),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	4d 8b 5e 60          	mov    0x60(%r14),%r11\n"
                    "   8:	4c 89 98 00 20 00 00 	mov    %r11,0x2000(%rax)"
                >>
            )
        end},
        {"move_to_native_register from a deep y register", fun() ->
            State0 = large_operand_state(),
            {State2, _Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 200}),
            large_operand_dump(State2, <<"   0:	49 8b 87 40 06 00 00 	mov    0x640(%r15),%rax">>)
        end},
        {"move_to_vm_register to a deep y register", fun() ->
            State0 = large_operand_state(),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:move_to_vm_register(State1, Reg, {y_reg, 200}),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	49 89 87 40 06 00 00 	mov    %rax,0x640(%r15)"
                >>
            )
        end},
        {"move_to_vm_register of a large immediate", fun() ->
            State0 = large_operand_state(),
            State2 = ?BACKEND:move_to_vm_register(State0, 16#12345678, {x_reg, 1}),
            large_operand_dump(
                State2, <<"   0:	49 c7 46 60 78 56 34 	movq   $0x12345678,0x60(%r14)\n   7:	12 ">>
            )
        end},
        {"and_ with a mask needing a literal", fun() ->
            State0 = large_operand_state(),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, _} = ?BACKEND:and_(State1, {free, Reg}, 16#12345),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	25 45 23 01 00       	and    $0x12345,%eax"
                >>
            )
        end},
        {"or_ with a mask needing a literal", fun() ->
            State0 = large_operand_state(),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:or_(State1, Reg, 16#12345),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 0d 45 23 01 00    	or     $0x12345,%rax"
                >>
            )
        end},
        {"xor_ with a mask needing a literal", fun() ->
            State0 = large_operand_state(),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:xor_(State1, Reg, 16#12345),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 35 45 23 01 00    	xor    $0x12345,%rax"
                >>
            )
        end},
        {"mul by a constant needing a literal", fun() ->
            State0 = large_operand_state(),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:mul(State1, Reg, 12345),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 69 c0 39 30 00 00 	imul   $0x3039,%rax,%rax"
                >>
            )
        end},
        {"if_block on a large immediate", fun() ->
            State0 = large_operand_state(),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            State2 = ?BACKEND:if_block(State1, {Reg, '==', 16#12345678}, fun(BSt) ->
                ?BACKEND:move_to_vm_register(BSt, 0, {x_reg, 1})
            end),
            large_operand_dump(
                State2,
                <<
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 3d 78 56 34 12    	cmp    $0x12345678,%rax\n"
                    "   a:	75 05                	jne    0x11\n"
                    "   c:	49 83 66 60 00       	andq   $0x0,0x60(%r14)"
                >>
            )
        end}
    ].

large_operand_state() ->
    ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)).

large_operand_dump(State, Dump) ->
    ?assertStream(x86_64, Dump, ?BACKEND:stream(?BACKEND:flush(State))).

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
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 8b 80 00 20 00 00 	mov    0x2000(%rax),%rax"
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
                    "   0:	49 8b 46 58          	mov    0x58(%r14),%rax\n"
                    "   4:	48 c7 80 00 20 00 00 	movq   $0x2a,0x2000(%rax)\n"
                    "   b:	2a 00 00 00 "
                >>
            )
        end}
    ].
