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

%% @doc Backend-level tests for jit_armv6m with the thumb2 variant enabled.
%% These tests exercise Thumb-2 specific code paths in the backend:
%% jump table entries, mov_immediate, and branch generation.

-module(jit_armv7m_tests).

-include_lib("eunit/include/eunit.hrl").

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_armv6m).
-define(THUMB2_VARIANT, ?JIT_VARIANT_PIC bor ?JIT_VARIANT_THUMB2).

%% Jump table entries are 6 bytes with Thumb-2 (push + b.w)
%% vs 12 bytes with Thumb-1 (ldr + push + add pc + nop + literal)
jump_table_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    ?assertEqual((512 + 1) * 6, byte_size(Stream)).

%% mov_immediate with Thumb-2: values 256-65535 use movw (4 bytes)
%% instead of Thumb-1 movs+adds (4 bytes) or ldr from literal pool.
%% The third argument to call_primitive goes into r2.
mov_immediate_thumb2_movw_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state, 1000]),
    Stream = ?BACKEND:stream(State1),
    MovwR2_1000 = jit_armv7m_asm:movw(r2, 1000),
    ?assertNotEqual(nomatch, binary:match(Stream, MovwR2_1000)).

%% mov_immediate with Thumb-2: 32-bit values use movw+movt (8 bytes)
%% instead of ldr from literal pool (2 bytes + 4 bytes data, needs pool flush).
mov_immediate_thumb2_movw_movt_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state, 16#12345678]),
    Stream = ?BACKEND:stream(State1),
    MovwR2_Lo = jit_armv7m_asm:movw(r2, 16#5678),
    MovtR2_Hi = jit_armv7m_asm:movt(r2, 16#1234),
    MovwMovt = <<MovwR2_Lo/binary, MovtR2_Hi/binary>>,
    ?assertNotEqual(nomatch, binary:match(Stream, MovwMovt)).

%% Verify that call_primitive produces the same instructions as Thumb-1
%% for small values (movs path is shared)
call_primitive_0_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(r7, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:	6817      	ldr	r7, [r2, #0]\n"
            "   2:	b405      	push	{r0, r2}\n"
            "   4:	9902      	ldr	r1, [sp, #8]\n"
            "   6:	47b8      	blx	r7\n"
            "   8:	4607      	mov	r7, r0\n"
            "   a:	bc05      	pop	{r0, r2}"
        >>,
    ?assertStream(arm_thumb2, Dump, Stream).

%% if_block with '== 0' uses the fused cbnz
if_block_eq0_cbnz_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(State2, {RegA, '==', 0}, fun(BSt0) ->
        ?BACKEND:add(BSt0, RegB, 2)
    end),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	6ac7      	ldr	r7, [r0, #44]	@ 0x2c\n"
        "   2:	6b06      	ldr	r6, [r0, #48]	@ 0x30\n"
        "   4:	b907      	cbnz	r7, 0x8\n"
        "   6:	3602      	adds	r6, #2"
    >>,
    ?assertStream(arm_thumb2, Dump, Stream),
    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State3)).

%% if_block with '!= 0' uses the fused cbz
if_block_ne0_cbz_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_block(State2, {RegA, '!=', 0}, fun(BSt0) ->
        ?BACKEND:add(BSt0, RegB, 2)
    end),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	6ac7      	ldr	r7, [r0, #44]	@ 0x2c\n"
        "   2:	6b06      	ldr	r6, [r0, #48]	@ 0x30\n"
        "   4:	b107      	cbz	r7, 0x8\n"
        "   6:	3602      	adds	r6, #2"
    >>,
    ?assertStream(arm_thumb2, Dump, Stream),
    ?assertEqual([RegA, RegB], ?BACKEND:used_regs(State3)).

%% supports_div should be true with thumb2 variant, false otherwise.
supports_div_thumb2_test() ->
    StateT2 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    ?assert(?BACKEND:supports_div(StateT2)),
    StateNoT2 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertNot(?BACKEND:supports_div(StateNoT2)).

%% div_/3 emits a Thumb-2 SDIV instruction on registers.
div_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, RegA} = ?BACKEND:div_(State2, RegA, RegB),
    ?assertEqual(r7, RegA),
    ?assertEqual(r6, RegB),
    Stream = ?BACKEND:stream(State3),
    Dump = <<
        "   0:	6ac7      	ldr	r7, [r0, #44]\n"
        "   2:	6b06      	ldr	r6, [r0, #48]\n"
        "   4:	fb97 f7f6 	sdiv	r7, r7, r6"
    >>,
    ?assertStream(arm_thumb2, Dump, Stream).

%% rem_/3 emits SDIV followed by MLS (multiply-subtract) for remainder.
rem_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, RegA} = ?BACKEND:rem_(State2, RegA, RegB),
    ?assertEqual(r7, RegA),
    ?assertEqual(r6, RegB),
    Stream = ?BACKEND:stream(State3),
    %% sdiv tmp, r7, r6  +  mls r7, tmp, r6, r7
    %% first_avail picks r5 (r7 and r6 are taken; first_avail prefers r7>r6>r5>...).
    Dump = <<
        "   0:	6ac7      	ldr	r7, [r0, #44]\n"
        "   2:	6b06      	ldr	r6, [r0, #48]\n"
        "   4:	fb97 f5f6 	sdiv	r5, r7, r6\n"
        "   8:	fb05 7716 	mls	r7, r5, r6, r7"
    >>,
    ?assertStream(arm_thumb2, Dump, Stream).

%% jump_to_label_cond fuses a widenable guard to a backward label into the
%% minimal exact form -- here the distance is tiny, so a single 2-byte
%% Thumb-1 bcc (the old proxy always reserved a 4-byte B<cond>.W).
jump_to_label_cond_fused_thumb2_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 4),
    %% Label 1 sits at the current (4-aligned) offset; the guard jumps back to it.
    State2 = ?BACKEND:add_label(State1, 1),
    {State3, RegA} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% "jump to label 1 when RegA != 5": cmp r7,#5 + a single bne back to it.
    Fused = ?BACKEND:stream(?BACKEND:jump_to_label_cond(State3, {RegA, '!=', 5}, 1)),
    Fallback = ?BACKEND:stream(
        ?BACKEND:if_block(State3, {RegA, '!=', 5}, fun(BSt0) ->
            ?BACKEND:jump_to_label(BSt0, 1)
        end)
    ),
    %% Single fused branch is strictly smaller than the two-branch fallback.
    ?assert(byte_size(Fused) < byte_size(Fallback)),
    %% ...and it ends with exactly one bne to label 1, four bytes back
    %% (ldr + cmp after the aligned label leaves the branch at label+4).
    ?assertEqual(jit_armv6m_asm:bcc(ne, -4), binary:part(Fused, byte_size(Fused) - 2, 2)).

%% A backward fused guard branch beyond the Thumb-1 bcc window widens to a
%% single B<cond>.W (the 2-byte placeholder reservation grows in place).
jump_to_label_cond_fused_backward_wide_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 4),
    State2 = ?BACKEND:add_label(State1, 1),
    %% add_label pads to 4-byte alignment; the label sits at the padded offset.
    LabelOffset = ?BACKEND:offset(State2),
    {State3, RegA} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% ~400 bytes of padding so label 1 is beyond the 2-byte bcc reach.
    State4 = lists:foldl(
        fun(_, S) -> ?BACKEND:move_to_vm_register(S, 1, {x_reg, 0}) end,
        State3,
        lists:seq(1, 100)
    ),
    Fused = ?BACKEND:stream(?BACKEND:jump_to_label_cond(State4, {RegA, '!=', 5}, 1)),
    BranchOffset = byte_size(Fused) - 4,
    Rel = LabelOffset - BranchOffset,
    ?assert(Rel < -252),
    ?assertEqual(jit_armv7m_asm:b_w(ne, Rel - 4), binary:part(Fused, BranchOffset, 4)).

%% A forward fused guard branch is emitted optimistically (4-byte reservation)
%% and resolved at finalize once the target label offset is known; a near
%% target resolves to the 2-byte bcc padded with a nop.
jump_to_label_cond_fused_forward_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 8),
    {State2, RegA} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    %% Forward guard jump to label 5 (not yet defined): optimistic 4-byte form.
    State3 = ?BACKEND:jump_to_label_cond(State2, {RegA, '!=', 5}, 5),
    BranchOffset = ?BACKEND:offset(State3) - 4,
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 2}),
    State5 = ?BACKEND:add_label(State4, 5),
    %% add_label pads to 4-byte alignment; the label sits at the padded offset.
    LabelOffset = ?BACKEND:offset(State5),
    State6 = ?BACKEND:update_branches(State5),
    %% Fit -> no overflow; near target = bne + nop filling the reservation.
    ?assertEqual(#{}, ?BACKEND:take_overflows(State6)),
    Stream = ?BACKEND:stream(State6),
    Rel = LabelOffset - BranchOffset,
    Expected = <<(jit_armv6m_asm:bcc(ne, Rel))/binary, (jit_armv6m_asm:nop())/binary>>,
    ?assertEqual(Expected, binary:part(Stream, BranchOffset, 4)).

%% The cbz/cbnz zero-compare guards (forward-only) fuse too: a near forward
%% target resolves to a single inverted cbz straight to the label.
jump_to_label_cond_fused_forward_cbz_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 8),
    {State2, RegA} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    %% "jump to label 5 when RegA == 0": if_block_cond yields a cbnz skip;
    %% fused + inverted it is one cbz to the label.
    State3 = ?BACKEND:jump_to_label_cond(State2, {RegA, '==', 0}, 5),
    BranchOffset = ?BACKEND:offset(State3) - 4,
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 2}),
    State5 = ?BACKEND:add_label(State4, 5),
    %% add_label pads to 4-byte alignment; the label sits at the padded offset.
    LabelOffset = ?BACKEND:offset(State5),
    State6 = ?BACKEND:update_branches(State5),
    ?assertEqual(#{}, ?BACKEND:take_overflows(State6)),
    Stream = ?BACKEND:stream(State6),
    Rel = LabelOffset - BranchOffset,
    Expected = <<(jit_armv7m_asm:cbz(RegA, Rel))/binary, (jit_armv6m_asm:nop())/binary>>,
    ?assertEqual(Expected, binary:part(Stream, BranchOffset, 4)).

%% A forward fused branch whose target lands beyond the B<cond>.W reach
%% overflows its 4-byte reservation: update_branches reports it, and the
%% re-emit pass (branch hints pinning it to 6 bytes) produces the 2-byte
%% skip over an unconditional B.W -- the full backtrack contract.
jump_to_label_cond_fused_forward_far_test() ->
    State0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 8),
    {State2, RegA} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:jump_to_label_cond(State2, {RegA, '!=', 5}, 5),
    BranchOffset = ?BACKEND:offset(State3) - 4,
    %% Place label 5 beyond the +/-1MB B<cond>.W reach.
    LabelOffset = 16#200000,
    State4 = ?BACKEND:add_label(State3, 5, LabelOffset),
    State5 = ?BACKEND:update_branches(State4),
    ?assertEqual(#{0 => 6}, ?BACKEND:take_overflows(State5)),
    %% Second pass, as driven by jit:compile's emit_finalize_loop: same emission
    %% with the overflowing branch pinned to 6 bytes.
    StateR0 = ?BACKEND:new(?THUMB2_VARIANT, jit_stream_binary, jit_stream_binary:new(0)),
    StateR1 = ?BACKEND:jump_table(StateR0, 8),
    StateR2 = ?BACKEND:set_branch_hints(StateR1, #{0 => 6}),
    {StateR3, RegA} = ?BACKEND:move_to_native_register(StateR2, {x_reg, 0}),
    StateR4 = ?BACKEND:jump_to_label_cond(StateR3, {RegA, '!=', 5}, 5),
    ?assertEqual(BranchOffset + 6, ?BACKEND:offset(StateR4)),
    StateR5 = ?BACKEND:add_label(StateR4, 5, LabelOffset),
    StateR6 = ?BACKEND:update_branches(StateR5),
    ?assertEqual(#{}, ?BACKEND:take_overflows(StateR6)),
    Stream = ?BACKEND:stream(StateR6),
    %% beq skips over the B.W (6 bytes from the branch); B.W covers the rest.
    Expected = <<
        (jit_armv6m_asm:bcc(eq, 6))/binary,
        (jit_armv7m_asm:b_w(LabelOffset - BranchOffset - 6))/binary
    >>,
    ?assertEqual(Expected, binary:part(Stream, BranchOffset, 6)).
