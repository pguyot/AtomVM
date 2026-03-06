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

-define(BACKEND, jit_arm32).

% disassembly obtained with:
%  arm-elf-objdump -D -b binary -marm -z

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_test() ->
    State = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual([], ?BACKEND:used_regs(State)),
    Available = ?BACKEND:available_regs(State),
    ?assertEqual(10, length(Available)),
    ?assert(lists:member(r1, Available)),
    ?assert(lists:member(r3, Available)),
    ?assert(lists:member(r4, Available)),
    ?assert(lists:member(r5, Available)),
    ?assert(lists:member(r6, Available)),
    ?assert(lists:member(r7, Available)),
    ?assert(lists:member(r8, Available)),
    ?assert(lists:member(r9, Available)),
    ?assert(lists:member(r10, Available)),
    ?assert(lists:member(r11, Available)).

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assert(is_atom(ResultReg)),
    Stream = ?BACKEND:stream(State1),
    ?assert(is_binary(Stream)),
    ?assert(byte_size(Stream) > 0),
    % All ARM32 instructions are 4 bytes
    ?assertEqual(0, byte_size(Stream) rem 4).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assert(is_atom(ResultReg)),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

move_to_vm_register_x_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {x_reg, 1}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

move_to_vm_register_y_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:move_to_vm_register(State1, Reg, {y_reg, 0}),
    State3 = ?BACKEND:free_native_registers(State2, [Reg]),
    ?BACKEND:assert_all_native_free(State3),
    Stream = ?BACKEND:stream(State3),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

jump_table_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    Stream = ?BACKEND:stream(State1),
    % 4 entries (0..3) * 8 bytes each = 32 bytes
    ?assertEqual(32, byte_size(Stream)).

add_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:add_label(State0, 1),
    State2 = ?BACKEND:add_label(State1, 2),
    ?assertEqual(0, ?BACKEND:offset(State2)).

and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:and_(State1, {free, Reg}, 16#FC),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

or_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:or_(State1, {free, Reg}, 16#0F),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:shift_left(State1, {free, Reg}, 2),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

shift_right_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:shift_right(State1, {free, Reg}, 4),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

add_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:add(State1, {free, Reg}, 42),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

sub_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:sub(State1, {free, Reg}, 42),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

decrement_reductions_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:add_label(State0, 1),
    State2 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State1),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

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
    ?assertEqual(<<>>, Stream).

xor_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:xor_(State1, {free, Reg}, 16#FF),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).

mul_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _Reg2} = ?BACKEND:mul(State1, {free, Reg}, 4),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0),
    ?assertEqual(0, byte_size(Stream) rem 4).
