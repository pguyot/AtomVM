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

-module(jit_wasm_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_wasm).

%% Helper to disassemble WASM binary for debugging
-ifdef(TEST).
wasm_helper_disassemble(Binary) ->
    % Could use wasm-dis here if needed for debugging
    Binary.
-endif.

%%-----------------------------------------------------------------------------
%% Basic state tests
%%-----------------------------------------------------------------------------

word_size_test() ->
    ?assertEqual(4, ?BACKEND:word_size()).

new_test() ->
    State = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Verify we have 6 available locals
    ?assertEqual(6, length(?BACKEND:available_regs(State))),
    % Verify no locals are used initially
    ?assertEqual([], ?BACKEND:used_regs(State)),
    % Verify offset starts at 0
    ?assertEqual(0, ?BACKEND:offset(State)).

%%-----------------------------------------------------------------------------
%% move_to_native_register tests
%%-----------------------------------------------------------------------------

move_immediate_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, 42),
    % Should allocate first available local (local 3)
    ?assertEqual({local, 3}, Local),
    % Should have 5 available locals left
    ?assertEqual(5, length(?BACKEND:available_regs(State1))),
    % Should have 1 used local
    ?assertEqual([{local, 3}], ?BACKEND:used_regs(State1)),
    % Verify the generated code
    Stream = ?BACKEND:stream(State1),
    Expected = <<
        % i32.const 42
        16#41,
        16#2A,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

move_immediate_to_specific_local_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_native_register(State0, 123, {local, 5}),
    Stream = ?BACKEND:stream(State1),
    Expected = <<
        % i32.const 123 (encoded as SLEB128: 0xFB 0x00)
        16#41,
        16#FB,
        16#00,
        % local.set 5
        16#21,
        16#05
    >>,
    ?assertEqual(Expected, Stream).

move_from_x_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    ?assertEqual({local, 3}, Local),
    Stream = ?BACKEND:stream(State1),
    % Expected: local.get 0 (ctx), i32.load align=2 offset=24, local.set 3
    % X reg 0 is at offset 0x18 (24)
    Expected = <<
        % local.get 0 (ctx)
        16#20,
        16#00,
        % i32.load align=2 offset=24
        16#28,
        16#02,
        16#18,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

move_from_y_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, {y_reg, 2}),
    ?assertEqual({local, 3}, Local),
    Stream = ?BACKEND:stream(State1),
    % Expected: local.get 0, i32.load align=2 offset=0x14, i32.load align=2 offset=8, local.set 3
    % Y regs array is at offset 0x14, Y[2] is at offset 2*4=8 in that array
    Expected = <<
        % local.get 0 (ctx)
        16#20,
        16#00,
        % i32.load align=2 offset=0x14 (load Y array pointer)
        16#28,
        16#02,
        16#14,
        % i32.load align=2 offset=8 (load Y[2])
        16#28,
        16#02,
        16#08,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

%%-----------------------------------------------------------------------------
%% move_to_vm_register tests
%%-----------------------------------------------------------------------------

move_to_x_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, 99),
    State2 = ?BACKEND:move_to_vm_register(State1, {x_reg, 1}, Local),
    Stream = ?BACKEND:stream(State2),
    % First part: i32.const 99, local.set 3
    % Second part: local.get 0, local.get 3, i32.store align=2 offset=28
    % X reg 1 is at offset 0x18 + 1*4 = 28
    Expected = <<
        % i32.const 99 (encoded as SLEB128: 0xE3 0x00)
        16#41,
        16#E3,
        16#00,
        % local.set 3
        16#21,
        16#03,
        % local.get 0 (ctx)
        16#20,
        16#00,
        % local.get 3 (value)
        16#20,
        16#03,
        % i32.store align=2 offset=28
        16#36,
        16#02,
        16#1C
    >>,
    ?assertEqual(Expected, Stream).

move_to_y_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, 77),
    State2 = ?BACKEND:move_to_vm_register(State1, {y_reg, 1}, Local),
    Stream = ?BACKEND:stream(State2),
    % Expected: load Y array, store value at offset 4
    Expected = <<
        % i32.const 77 (encoded as SLEB128: 0xCD 0x00)
        16#41,
        16#CD,
        16#00,
        % local.set 3
        16#21,
        16#03,
        % local.get 0 (ctx)
        16#20,
        16#00,
        % i32.load align=2 offset=0x14 (load Y array pointer)
        16#28,
        16#02,
        16#14,
        % local.get 3 (value)
        16#20,
        16#03,
        % i32.store align=2 offset=4 (store to Y[1])
        16#36,
        16#02,
        16#04
    >>,
    ?assertEqual(Expected, Stream).

%%-----------------------------------------------------------------------------
%% copy_to_native_register tests
%%-----------------------------------------------------------------------------

copy_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 55),
    {State2, Local2} = ?BACKEND:copy_to_native_register(State1, Local1),
    ?assertEqual({local, 3}, Local1),
    ?assertEqual({local, 4}, Local2),
    % Should have 4 available locals left
    ?assertEqual(4, length(?BACKEND:available_regs(State2))),
    % Should have 2 used locals
    ?assertEqual(2, length(?BACKEND:used_regs(State2))),
    Stream = ?BACKEND:stream(State2),
    Expected = <<
        % i32.const 55
        16#41,
        16#37,
        % local.set 3
        16#21,
        16#03,
        % local.get 3
        16#20,
        16#03,
        % local.set 4
        16#21,
        16#04
    >>,
    ?assertEqual(Expected, Stream).

%%-----------------------------------------------------------------------------
%% Arithmetic operation tests
%%-----------------------------------------------------------------------------

add_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 10),
    {State2, Local2} = ?BACKEND:move_to_native_register(State1, 20),
    State3 = ?BACKEND:add(State2, Local1, Local2),
    Stream = ?BACKEND:stream(State3),
    Expected = <<
        % i32.const 10
        16#41,
        16#0A,
        % local.set 3
        16#21,
        16#03,
        % i32.const 20
        16#41,
        16#14,
        % local.set 4
        16#21,
        16#04,
        % local.get 3
        16#20,
        16#03,
        % local.get 4
        16#20,
        16#04,
        % i32.add
        16#6A,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

sub_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 50),
    {State2, Local2} = ?BACKEND:move_to_native_register(State1, 30),
    State3 = ?BACKEND:sub(State2, Local1, Local2),
    Stream = ?BACKEND:stream(State3),
    Expected = <<
        % i32.const 50
        16#41,
        16#32,
        % local.set 3
        16#21,
        16#03,
        % i32.const 30
        16#41,
        16#1E,
        % local.set 4
        16#21,
        16#04,
        % local.get 3
        16#20,
        16#03,
        % local.get 4
        16#20,
        16#04,
        % i32.sub
        16#6B,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

mul_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 7),
    {State2, Local2} = ?BACKEND:move_to_native_register(State1, 8),
    State3 = ?BACKEND:mul(State2, Local1, Local2),
    Stream = ?BACKEND:stream(State3),
    Expected = <<
        % i32.const 7
        16#41,
        16#07,
        % local.set 3
        16#21,
        16#03,
        % i32.const 8
        16#41,
        16#08,
        % local.set 4
        16#21,
        16#04,
        % local.get 3
        16#20,
        16#03,
        % local.get 4
        16#20,
        16#04,
        % i32.mul
        16#6C,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

and_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 15),
    {State2, Local2} = ?BACKEND:move_to_native_register(State1, 7),
    State3 = ?BACKEND:and_(State2, Local1, Local2),
    Stream = ?BACKEND:stream(State3),
    Expected = <<
        % i32.const 15
        16#41,
        16#0F,
        % local.set 3
        16#21,
        16#03,
        % i32.const 7
        16#41,
        16#07,
        % local.set 4
        16#21,
        16#04,
        % local.get 3
        16#20,
        16#03,
        % local.get 4
        16#20,
        16#04,
        % i32.and
        16#71,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

or_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 8),
    {State2, Local2} = ?BACKEND:move_to_native_register(State1, 4),
    State3 = ?BACKEND:or_(State2, Local1, Local2),
    Stream = ?BACKEND:stream(State3),
    Expected = <<
        % i32.const 8
        16#41,
        16#08,
        % local.set 3
        16#21,
        16#03,
        % i32.const 4
        16#41,
        16#04,
        % local.set 4
        16#21,
        16#04,
        % local.get 3
        16#20,
        16#03,
        % local.get 4
        16#20,
        16#04,
        % i32.or
        16#72,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

%%-----------------------------------------------------------------------------
%% Shift operation tests
%%-----------------------------------------------------------------------------

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Local, 3),
    Stream = ?BACKEND:stream(State2),
    Expected = <<
        % local.get 0, i32.load align=2 offset=24, local.set 3
        16#20,
        16#00,
        16#28,
        16#02,
        16#18,
        16#21,
        16#03,
        % local.get 3
        16#20,
        16#03,
        % i32.const 3
        16#41,
        16#03,
        % i32.shl
        16#74,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

shift_right_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_right(State1, Local, 2),
    Stream = ?BACKEND:stream(State2),
    Expected = <<
        % local.get 0, i32.load align=2 offset=24, local.set 3
        16#20,
        16#00,
        16#28,
        16#02,
        16#18,
        16#21,
        16#03,
        % local.get 3
        16#20,
        16#03,
        % i32.const 2
        16#41,
        16#02,
        % i32.shr_u
        16#76,
        % local.set 3
        16#21,
        16#03
    >>,
    ?assertEqual(Expected, Stream).

%%-----------------------------------------------------------------------------
%% Free registers test
%%-----------------------------------------------------------------------------

free_registers_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Local1} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, Local2} = ?BACKEND:move_to_native_register(State1, 2),
    % Should have 4 available
    ?assertEqual(4, length(?BACKEND:available_regs(State2))),
    % Free both
    State3 = ?BACKEND:free_native_registers(State2, [Local1, Local2]),
    % Should have 6 available again
    ?assertEqual(6, length(?BACKEND:available_regs(State3))),
    % Should have 0 used
    ?assertEqual([], ?BACKEND:used_regs(State3)).

assert_all_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Initially all free
    ?assertEqual(true, ?BACKEND:assert_all_native_free(State0)),
    % Allocate one
    {State1, _Local} = ?BACKEND:move_to_native_register(State0, 1),
    % Should fail assertion
    ?assertError({used_locals_not_freed, _}, ?BACKEND:assert_all_native_free(State1)).
