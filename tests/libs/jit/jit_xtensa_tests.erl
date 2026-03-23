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

-module(jit_xtensa_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").
-include("jit_tests_common.hrl").

-define(BACKEND, jit_xtensa).

%% Test that word_size returns 4 (Xtensa is a 32-bit architecture)
word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

%% Test that new/3 creates a valid state
new_state_test() ->
    State = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Stream = ?BACKEND:stream(State),
    ?assertEqual(0, byte_size(Stream)),
    ?assertEqual(0, ?BACKEND:offset(State)).

%% Test that jump_table creates entries in the stream
jump_table_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0).

%% Test that add_label works with jump_table
add_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2),
    State4 = ?BACKEND:add_label(State3, 0),
    Stream = ?BACKEND:stream(State4),
    ?assert(byte_size(Stream) > 0).

%% Test that add_label/3 works with a known offset
add_label_with_offset_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#20),
    Stream = ?BACKEND:stream(State3),
    ?assert(byte_size(Stream) > 0).

%% Test move_to_native_register/2 with an x_reg produces output
move_to_native_register_xreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    ?assert(is_atom(Reg)).

%% Test move_to_native_register/2 with an immediate value
move_to_native_register_imm_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, 42),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    ?assert(is_atom(Reg)).

%% Test move_to_native_register/2 with a y_reg
move_to_native_register_yreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    ?assert(is_atom(Reg)).

%% Test move_to_vm_register/3 produces output
move_to_vm_register_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {x_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0).

%% Test move_to_vm_register/3 with y_reg destination
move_to_vm_register_yreg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {y_reg, 2}),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0).

%% Test and_ operation produces output
and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:and_(State1, {free, RegA}, 16#3F),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0).

%% Test or_ operation produces output
or_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:or_(State1, {free, RegA}, 16#0F),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0).

%% Test add operation produces output
add_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:add(State1, {free, RegA}, 4),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0).

%% Test sub operation produces output
sub_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:sub(State1, {free, RegA}, 4),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0).

%% Test debugger instruction produces output
debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0).

%% Test call_primitive produces output and returns a register
call_primitive_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0),
    ?assert(is_atom(ResultReg)).

%% Test call_primitive_last produces output
call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state]),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0).

%% Test shift_right produces output
shift_right_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:shift_right(State1, {free, RegA}, 2),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0).

%% Test shift_left produces output
shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:shift_left(State1, {free, RegA}, 2),
    Stream = ?BACKEND:stream(State2),
    ?assert(byte_size(Stream) > 0).

%% Test that multiple move_to_native_register calls allocate different registers
register_allocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {_State3, Reg3} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    ?assertNotEqual(Reg1, Reg2),
    ?assertNotEqual(Reg2, Reg3),
    ?assertNotEqual(Reg1, Reg3).

%% Test update_branches with jump_table and labels
update_branches_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_primitive_last(State2, 0, [ctx, jit_state]),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 1, [ctx, jit_state]),
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 0, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    ?assert(byte_size(Stream) > 0).

%% Test move_to_cp produces output
move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    ?assert(byte_size(Stream) > 0).
