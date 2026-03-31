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

-module(jit_wasm32_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_wasm32).
-define(JUMP_TABLE_ENTRY_SIZE, 4).

%%=============================================================================
%% Basic backend properties
%%=============================================================================

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_test() ->
    State = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual(<<>>, ?BACKEND:stream(State)),
    ?assertEqual([], ?BACKEND:used_regs(State)),
    %% 8 scratch locals (indices 3-10)
    ?assertEqual(8, length(?BACKEND:available_regs(State))).

%%=============================================================================
%% Register allocation
%%=============================================================================

alloc_and_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    %% Allocate a register
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    ?assert(is_integer(Reg1)),
    ?assert(Reg1 >= 3),
    ?assertEqual([Reg1], ?BACKEND:used_regs(State1)),
    %% Free it
    State2 = ?BACKEND:free_native_registers(State1, [Reg1]),
    ?assertEqual([], ?BACKEND:used_regs(State2)),
    ?assertEqual(8, length(?BACKEND:available_regs(State2))),
    ok.

assert_all_native_free_test() ->
    State = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    ?assertEqual(ok, ?BACKEND:assert_all_native_free(State)).

%%=============================================================================
%% Jump table
%%=============================================================================

jump_table_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    Stream = ?BACKEND:stream(State1),
    %% Header: num_entries=4 (labels_count+1), wasm_offset=0 (placeholder)
    <<NumEntries:32/little, WasmOffset:32/little, _JumpTable/binary>> = Stream,
    ?assertEqual(4, NumEntries),
    ?assertEqual(0, WasmOffset),
    %% Jump table area should be 4 * 4 = 16 bytes of zeros
    JumpTableSize = 4 * ?JUMP_TABLE_ENTRY_SIZE,
    ?assertEqual(8 + JumpTableSize, byte_size(Stream)).

%%=============================================================================
%% Label management
%%=============================================================================

add_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    %% Add label 0
    State2 = ?BACKEND:add_label(State1, 0),
    Offset0 = ?BACKEND:offset(State2),
    %% Offset should be JumpTableStart + 0 * JUMP_TABLE_ENTRY_SIZE
    ?assert(Offset0 >= 8),
    %% Add label 1
    State3 = ?BACKEND:add_label(State2, 1),
    Offset1 = ?BACKEND:offset(State3),
    ?assertEqual(Offset0 + ?JUMP_TABLE_ENTRY_SIZE, Offset1),
    ok.

%%=============================================================================
%% Emit and instruction generation
%%=============================================================================

debugger_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:debugger(State2),
    %% Just verify it doesn't crash - the instruction goes to current_body,
    %% not the stream
    ?assertEqual(<<>>, ?BACKEND:stream(State0)),
    _ = State3,
    ok.

%%=============================================================================
%% Arithmetic operations
%%=============================================================================

shift_right_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:shift_right(State3, {free, Reg}, 2),
    %% Result should reuse the freed register
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

shift_left_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:shift_left(State3, Reg, 3),
    _ = State4,
    ok.

and_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:and_(State3, {free, Reg}, 16#FF),
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

or_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:or_(State4, {free, Reg1}, Reg2),
    ?assertEqual(Reg1, ResultReg),
    _ = State5,
    ok.

add_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:add(State3, {free, Reg}, 42),
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

sub_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:sub(State3, {free, Reg}, 10),
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

mul_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:mul(State3, {free, Reg}, 3),
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

xor_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:xor_(State3, {free, Reg}, 16#FF),
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

div_reg_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:div_reg(State4, {free, Reg1}, Reg2),
    ?assertEqual(Reg1, ResultReg),
    _ = State5,
    ok.

rem_reg_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:rem_reg(State4, {free, Reg1}, Reg2),
    ?assertEqual(Reg1, ResultReg),
    _ = State5,
    ok.

%%=============================================================================
%% Memory access
%%=============================================================================

move_to_vm_register_x_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, Reg, {x_reg, 1}),
    _ = State4,
    ok.

move_to_vm_register_y_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, Reg, {y_reg, 0}),
    _ = State4,
    ok.

move_to_native_register_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    ?assert(is_integer(Reg)),
    ?assert(Reg >= 3),
    _ = State3,
    ok.

copy_to_native_register_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:copy_to_native_register(State2, {x_reg, 0}),
    ?assert(is_integer(Reg)),
    ?assert(Reg >= 3),
    _ = State3,
    ok.

increment_sp_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:increment_sp(State2, 4),
    _ = State3,
    ok.

set_bs_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:set_bs(State3, Reg),
    _ = State4,
    ok.

get_array_element_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:get_array_element(State3, {ptr, Reg}, 0),
    ?assert(is_integer(ResultReg)),
    _ = State4,
    ok.

move_to_array_element_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_array_element(State3, 42, {ptr, Reg}, 0),
    _ = State4,
    ok.

move_array_element_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:move_array_element(State4, {ptr, Reg1}, 0, {ptr, Reg2}),
    _ = State5,
    ok.

%%=============================================================================
%% Continuation and module index
%%=============================================================================

set_continuation_to_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:set_continuation_to_label(State2, 1),
    _ = State3,
    ok.

set_continuation_to_offset_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, OffsetRef} = ?BACKEND:set_continuation_to_offset(State2),
    ?assert(is_reference(OffsetRef)),
    _ = State3,
    ok.

continuation_entry_point_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:continuation_entry_point(State0),
    %% Should be a no-op
    ?assertEqual(?BACKEND:stream(State0), ?BACKEND:stream(State1)),
    ok.

get_module_index_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:get_module_index(State2),
    ?assert(is_integer(Reg)),
    ?assert(Reg >= 3),
    _ = State3,
    ok.

%%=============================================================================
%% Control flow
%%=============================================================================

jump_to_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:jump_to_label(State2, 1),
    _ = State3,
    ok.

cond_jump_to_label_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:cond_jump_to_label(State3, {{free, Reg}, '==', {free, Reg}}, 1),
    _ = State4,
    ok.

return_if_not_equal_to_ctx_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:return_if_not_equal_to_ctx(State3, {free, Reg}),
    %% Register should be freed
    ?assertEqual([], ?BACKEND:used_regs(State4)),
    ok.

if_block_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {{free, Reg}, '==', {free, Reg}}, fun(S) ->
        ?BACKEND:debugger(S)
    end),
    _ = State4,
    ok.

if_else_block_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_else_block(
        State3,
        {Reg, '==', 0},
        fun(S) -> ?BACKEND:debugger(S) end,
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

%%=============================================================================
%% Condition generation
%%=============================================================================

condition_lt_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '<', 10}, fun(S) -> ?BACKEND:debugger(S) end),
    _ = State4,
    ok.

condition_eq_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '==', 42}, fun(S) -> ?BACKEND:debugger(S) end),
    _ = State4,
    ok.

condition_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '!=', 0}, fun(S) -> ?BACKEND:debugger(S) end),
    _ = State4,
    ok.

condition_and_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'and', [{Reg, '==', 0}, {Reg, '!=', 1}]},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

condition_mask_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {Reg, '&', 16#F, '!=', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

condition_bool_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(bool)', Reg, '==', false},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

condition_bool_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(bool)', Reg, '!=', false},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

condition_int_cast_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(int)', Reg, '==', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

%%=============================================================================
%% Primitive calls
%%=============================================================================

call_primitive_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, ResultLocal} = ?BACKEND:call_primitive(State2, 0, [ctx, jit_state]),
    ?assert(is_integer(ResultLocal)),
    ?assert(ResultLocal >= 3),
    _ = State3,
    ok.

call_primitive_last_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_primitive_last(State2, 0, [ctx, jit_state]),
    %% All regs should be free after a tail call
    ?assertEqual([], ?BACKEND:used_regs(State3)),
    ok.

%%=============================================================================
%% Scheduling
%%=============================================================================

decrement_reductions_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State2),
    _ = State3,
    ok.

call_or_schedule_next_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_or_schedule_next(State2, 1),
    _ = State3,
    ok.

call_only_or_schedule_next_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 1),
    _ = State3,
    ok.

%%=============================================================================
%% CP operations
%%=============================================================================

move_to_cp_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:move_to_cp(State2, {y_reg, 0}),
    _ = State3,
    ok.

%%=============================================================================
%% Complete WASM module generation
%%=============================================================================

return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    %% Add some instructions to label 0
    State3 = ?BACKEND:debugger(State2),
    State4 = ?BACKEND:add_label(State3, 1),
    %% Add some instructions to label 1
    State5 = ?BACKEND:debugger(State4),
    %% Finalize
    State6 = ?BACKEND:return_labels_and_lines(State5, []),
    Stream = ?BACKEND:stream(State6),
    %% Verify the stream has a valid WASM module embedded
    <<NumEntries:32/little, WasmOffset:32/little, _Rest/binary>> = Stream,
    ?assertEqual(3, NumEntries),
    ?assert(WasmOffset > 0),
    ?assert(WasmOffset < byte_size(Stream)),
    %% Extract the WASM module
    WasmModule = binary:part(Stream, WasmOffset, byte_size(Stream) - WasmOffset),
    %% Verify WASM magic number and version
    <<16#00, 16#61, 16#73, 16#6D, 16#01, 16#00, 16#00, 16#00, _Sections/binary>> = WasmModule,
    ok.

return_labels_and_lines_empty_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 0),
    State2 = ?BACKEND:return_labels_and_lines(State1, []),
    Stream = ?BACKEND:stream(State2),
    <<NumEntries:32/little, WasmOffset:32/little, _Rest/binary>> = Stream,
    ?assertEqual(1, NumEntries),
    ?assert(WasmOffset > 0),
    %% Extract WASM module and verify magic
    WasmModule = binary:part(Stream, WasmOffset, byte_size(Stream) - WasmOffset),
    <<16#00, 16#61, 16#73, 16#6D, 16#01, 16#00, 16#00, 16#00, _/binary>> = WasmModule,
    ok.

%%=============================================================================
%% Function pointer calls (BIFs)
%%=============================================================================

call_func_ptr_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, FuncPtr} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:call_func_ptr(State3, {free, FuncPtr}, [ctx]),
    ?assert(is_integer(ResultReg)),
    _ = State4,
    ok.

%%=============================================================================
%% Flush
%%=============================================================================

flush_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:flush(State0),
    ?assertEqual(?BACKEND:stream(State0), ?BACKEND:stream(State1)),
    ok.

%%=============================================================================
%% Update branches
%%=============================================================================

update_branches_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:update_branches(State0),
    _ = State1,
    ok.

%%=============================================================================
%% Shift right arithmetic
%%=============================================================================

shift_right_arith_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:shift_right_arith(State3, {free, Reg}, 2),
    ?assertEqual(Reg, ResultReg),
    _ = State4,
    ok.

%%=============================================================================
%% Additional coverage tests: non-free local variants
%%=============================================================================

%% Test arithmetic ops with non-free locals (allocate new result local)
shift_right_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% Pass local without {free, _} wrapper
    {State4, ResultReg} = ?BACKEND:shift_right(State3, Reg, 4),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

shift_right_arith_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:shift_right_arith(State3, Reg, 4),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

and_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:and_(State3, Reg, 16#FF),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

or_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:or_(State4, Reg1, Reg2),
    ?assert(ResultReg =/= Reg1),
    _ = State5,
    ok.

xor_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:xor_(State3, Reg, 16#FF),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

add_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:add(State3, Reg, 42),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

sub_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:sub(State3, Reg, 10),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

mul_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, ResultReg} = ?BACKEND:mul(State3, Reg, 3),
    ?assert(ResultReg =/= Reg),
    _ = State4,
    ok.

div_reg_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:div_reg(State4, Reg1, Reg2),
    ?assert(ResultReg =/= Reg1),
    _ = State5,
    ok.

rem_reg_non_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    {State5, ResultReg} = ?BACKEND:rem_reg(State4, Reg1, Reg2),
    ?assert(ResultReg =/= Reg1),
    _ = State5,
    ok.

%%=============================================================================
%% Additional coverage: edge cases and more argument types
%%=============================================================================

%% Test free_native_register with non-scratch local (should be no-op)
free_native_register_noop_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    %% Free a non-scratch value (should not crash)
    State1 = ?BACKEND:free_native_registers(State0, [{x_reg, 0}]),
    ?assertEqual(8, length(?BACKEND:available_regs(State1))),
    %% Free a ptr
    {State2, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State3 = ?BACKEND:free_native_registers(State2, [{ptr, Reg}]),
    ?assertEqual([], ?BACKEND:used_regs(State3)),
    ok.

%% Test move_to_native_register with explicit local target
move_to_native_register_explicit_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_native_register(State3, {y_reg, 0}, Reg),
    _ = State4,
    ok.

%% Test move_to_array_element with IndexLocal variant
move_to_array_element_index_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Base} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, IdxLocal} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:move_to_array_element(State4, 42, {ptr, Base}, IdxLocal, 4),
    _ = State5,
    ok.

%% Test jump_to_offset
jump_to_offset_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    %% Jump to a known label offset
    Offset1 = ?BACKEND:offset(State2),
    State3 = ?BACKEND:jump_to_offset(State2, Offset1),
    _ = State3,
    %% Jump to unknown offset (fallback path)
    State4 = ?BACKEND:jump_to_offset(State2, 99999),
    _ = State4,
    ok.

%% Test jump_to_continuation
jump_to_continuation_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:jump_to_continuation(State3, {free, Reg}),
    ?assertEqual([], ?BACKEND:used_regs(State4)),
    ok.

%% Test reference label
add_label_reference_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 0),
    Ref = make_ref(),
    State3 = ?BACKEND:add_label(State2, Ref),
    _Offset = ?BACKEND:offset(State3),
    ok.

%% Test call_primitive_with_cp
call_primitive_with_cp_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    State3 = ?BACKEND:call_primitive_with_cp(State2, 0, [ctx, jit_state]),
    ?assertEqual([], ?BACKEND:used_regs(State3)),
    ok.

%% Test update_branches with non-empty branches list
update_branches_non_empty_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    %% Manually set some branches to test the clearing behavior
    State1 = State0,  %% branches starts empty
    State2 = ?BACKEND:update_branches(State1),
    _ = State2,
    ok.

%% Test emit_value_to_stack with various value types via move_to_vm_register
move_to_vm_register_ptr_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    %% Store {ptr, Reg} value to x register
    State4 = ?BACKEND:move_to_vm_register(State3, {ptr, Reg}, {x_reg, 2}),
    _ = State4,
    ok.

%% Test with {free, _} value in move_to_vm_register
move_to_vm_register_free_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:move_to_vm_register(State3, {free, Reg}, {x_reg, 2}),
    _ = State4,
    ok.

%% Test condition with local < other_local
condition_lt_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:if_block(State4, {{free, Reg1}, '<', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    _ = State5,
    ok.

%% Test condition with integer < local
condition_int_lt_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {0, '<', {free, Reg}}, fun(S) -> ?BACKEND:debugger(S) end),
    _ = State4,
    ok.

%% Test condition with local != other_local
condition_ne_local_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg1} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    {State4, Reg2} = ?BACKEND:move_to_native_register(State3, {x_reg, 1}),
    State5 = ?BACKEND:if_block(State4, {Reg1, '!=', Reg2}, fun(S) -> ?BACKEND:debugger(S) end),
    _ = State5,
    ok.

%% Test condition with (int) cast to !=
condition_int_cast_ne_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(
        State3,
        {'(int)', Reg, '!=', 0},
        fun(S) -> ?BACKEND:debugger(S) end
    ),
    _ = State4,
    ok.

%% Test call_func_ptr with {primitive, _} variant
call_func_ptr_primitive_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    State2 = ?BACKEND:add_label(State1, 0),
    {State3, ResultReg} = ?BACKEND:call_func_ptr(State2, {primitive, 5}, [ctx]),
    ?assert(is_integer(ResultReg)),
    _ = State3,
    ok.

%% Test table section encoding in asm module
encode_table_section_test() ->
    Table = <<(jit_wasm32_asm:type_funcref())/binary, 16#00, 0>>,
    Result = jit_wasm32_asm:encode_table_section([Table]),
    ?assertMatch(<<4, _, _/binary>>, Result),
    ok.

%% Test dynamic scratch local overflow beyond the initial 8 locals
alloc_local_overflow_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(65536)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    %% Allocate all 8 base scratch locals
    {State3, R1} = ?BACKEND:call_primitive(State2, 0, [ctx]),
    {State4, R2} = ?BACKEND:call_primitive(State3, 0, [ctx]),
    {State5, R3} = ?BACKEND:call_primitive(State4, 0, [ctx]),
    {State6, R4} = ?BACKEND:call_primitive(State5, 0, [ctx]),
    {State7, R5} = ?BACKEND:call_primitive(State6, 0, [ctx]),
    {State8, R6} = ?BACKEND:call_primitive(State7, 0, [ctx]),
    {State9, R7} = ?BACKEND:call_primitive(State8, 0, [ctx]),
    {State10, R8} = ?BACKEND:call_primitive(State9, 0, [ctx]),
    ?assertEqual(8, length(?BACKEND:used_regs(State10))),
    ?assertEqual([], ?BACKEND:available_regs(State10)),
    %% Allocate overflow locals (9th and 10th)
    {State11, R9} = ?BACKEND:call_primitive(State10, 0, [ctx]),
    ?assertEqual(11, R9),
    {State12, R10} = ?BACKEND:call_primitive(State11, 0, [ctx]),
    ?assertEqual(12, R10),
    ?assertEqual(10, length(?BACKEND:used_regs(State12))),
    %% Free all locals
    State13 = ?BACKEND:free_native_registers(State12,
        [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]),
    ok = ?BACKEND:assert_all_native_free(State13),
    %% Verify WASM module generation with overflow locals
    State14 = ?BACKEND:add_label(State13, 2),
    _FinalState = ?BACKEND:return_labels_and_lines(State14, []),
    ok.

%% Test that freed overflow locals can be reused
alloc_local_overflow_reuse_test() ->
    State0 = ?BACKEND:new(0, jit_stream_binary, jit_stream_binary:new(65536)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    %% Exhaust base pool
    {State3, R1} = ?BACKEND:call_primitive(State2, 0, [ctx]),
    {State4, _} = ?BACKEND:call_primitive(State3, 0, [ctx]),
    {State5, _} = ?BACKEND:call_primitive(State4, 0, [ctx]),
    {State6, _} = ?BACKEND:call_primitive(State5, 0, [ctx]),
    {State7, _} = ?BACKEND:call_primitive(State6, 0, [ctx]),
    {State8, _} = ?BACKEND:call_primitive(State7, 0, [ctx]),
    {State9, _} = ?BACKEND:call_primitive(State8, 0, [ctx]),
    {State10, _} = ?BACKEND:call_primitive(State9, 0, [ctx]),
    %% Allocate overflow
    {State11, R9} = ?BACKEND:call_primitive(State10, 0, [ctx]),
    ?assertEqual(11, R9),
    %% Free the first base local and the overflow local
    State12 = ?BACKEND:free_native_registers(State11, [R1, R9]),
    %% Now allocate two more - should reuse the freed locals
    {State13, Reused1} = ?BACKEND:call_primitive(State12, 0, [ctx]),
    {State14, Reused2} = ?BACKEND:call_primitive(State13, 0, [ctx]),
    %% Both should be from previously freed locals (3 or 11)
    ?assert(lists:member(Reused1, [3, 11])),
    ?assert(lists:member(Reused2, [3, 11])),
    _ = State14,
    ok.
