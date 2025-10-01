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

%%-----------------------------------------------------------------------------
%% @doc WebAssembly assembler module
%%
%% This module generates WebAssembly binary encoding for individual instructions.
%% It follows WASM text format syntax where possible for readability.
%%
%% WASM uses LEB128 encoding for all integer immediates.
%% @end
%%-----------------------------------------------------------------------------
-module(jit_wasm_asm).

-export([
    % Control flow
    block/1,
    loop/1,
    if_/1,
    else_/0,
    end_/0,
    br/1,
    br_if/1,
    return_/0,
    % Variable access
    local_get/1,
    local_set/1,
    local_tee/1,
    % Constants
    i32_const/1,
    % Arithmetic
    i32_add/0,
    i32_sub/0,
    i32_mul/0,
    i32_and/0,
    i32_or/0,
    i32_xor/0,
    i32_shl/0,
    i32_shr_s/0,
    i32_shr_u/0,
    % Comparison
    i32_eqz/0,
    i32_eq/0,
    i32_ne/0,
    i32_lt_s/0,
    i32_lt_u/0,
    i32_gt_s/0,
    i32_gt_u/0,
    i32_le_s/0,
    i32_le_u/0,
    i32_ge_s/0,
    i32_ge_u/0,
    % Memory access
    i32_load/2,
    i32_store/2,
    i32_load8_s/2,
    i32_load8_u/2,
    i32_load16_s/2,
    i32_load16_u/2,
    i32_store8/2,
    i32_store16/2,
    % Helper functions
    encode_sleb128/1,
    encode_uleb128/1
]).

-export_type([
    blocktype/0,
    localidx/0,
    labelidx/0
]).

-type blocktype() :: empty | i32 | i64 | f32 | f64.
-type localidx() :: non_neg_integer().
-type labelidx() :: non_neg_integer().

%%-----------------------------------------------------------------------------
%% LEB128 Encoding Helpers
%%-----------------------------------------------------------------------------

%% @doc Encode signed integer as LEB128
-spec encode_sleb128(integer()) -> binary().
encode_sleb128(Value) ->
    encode_sleb128(Value, <<>>).

encode_sleb128(Value, Acc) when Value >= -64, Value < 64 ->
    <<Acc/binary, (Value band 16#7F)>>;
encode_sleb128(Value, Acc) ->
    Byte = (Value band 16#7F) bor 16#80,
    encode_sleb128(Value bsr 7, <<Acc/binary, Byte>>).

%% @doc Encode unsigned integer as LEB128
-spec encode_uleb128(non_neg_integer()) -> binary().
encode_uleb128(Value) ->
    encode_uleb128(Value, <<>>).

encode_uleb128(Value, Acc) when Value < 128 ->
    <<Acc/binary, Value>>;
encode_uleb128(Value, Acc) ->
    Byte = (Value band 16#7F) bor 16#80,
    encode_uleb128(Value bsr 7, <<Acc/binary, Byte>>).

%%-----------------------------------------------------------------------------
%% Control Flow Instructions
%%-----------------------------------------------------------------------------

%% @doc block blocktype - Begin a block construct
-spec block(blocktype()) -> binary().
block(empty) -> <<16#02, 16#40>>;
block(i32) -> <<16#02, 16#7F>>;
block(i64) -> <<16#02, 16#7E>>;
block(f32) -> <<16#02, 16#7D>>;
block(f64) -> <<16#02, 16#7C>>.

%% @doc loop blocktype - Begin a loop construct
-spec loop(blocktype()) -> binary().
loop(empty) -> <<16#03, 16#40>>;
loop(i32) -> <<16#03, 16#7F>>;
loop(i64) -> <<16#03, 16#7E>>;
loop(f32) -> <<16#03, 16#7D>>;
loop(f64) -> <<16#03, 16#7C>>.

%% @doc if blocktype - Begin an if construct
-spec if_(blocktype()) -> binary().
if_(empty) -> <<16#04, 16#40>>;
if_(i32) -> <<16#04, 16#7F>>;
if_(i64) -> <<16#04, 16#7E>>;
if_(f32) -> <<16#04, 16#7D>>;
if_(f64) -> <<16#04, 16#7C>>.

%% @doc else - Begin else clause
-spec else_() -> binary().
else_() -> <<16#05>>.

%% @doc end - End a block, loop, if, or function
-spec end_() -> binary().
end_() -> <<16#0B>>.

%% @doc br labelidx - Branch to label
-spec br(labelidx()) -> binary().
br(LabelIdx) ->
    <<16#0C, (encode_uleb128(LabelIdx))/binary>>.

%% @doc br_if labelidx - Conditional branch
-spec br_if(labelidx()) -> binary().
br_if(LabelIdx) ->
    <<16#0D, (encode_uleb128(LabelIdx))/binary>>.

%% @doc return - Return from function
-spec return_() -> binary().
return_() -> <<16#0F>>.

%%-----------------------------------------------------------------------------
%% Variable Access Instructions
%%-----------------------------------------------------------------------------

%% @doc local.get localidx - Get local variable
-spec local_get(localidx()) -> binary().
local_get(LocalIdx) ->
    <<16#20, (encode_uleb128(LocalIdx))/binary>>.

%% @doc local.set localidx - Set local variable
-spec local_set(localidx()) -> binary().
local_set(LocalIdx) ->
    <<16#21, (encode_uleb128(LocalIdx))/binary>>.

%% @doc local.tee localidx - Tee local variable (set and keep value on stack)
-spec local_tee(localidx()) -> binary().
local_tee(LocalIdx) ->
    <<16#22, (encode_uleb128(LocalIdx))/binary>>.

%%-----------------------------------------------------------------------------
%% Constant Instructions
%%-----------------------------------------------------------------------------

%% @doc i32.const value - Push i32 constant
-spec i32_const(integer()) -> binary().
i32_const(Value) ->
    <<16#41, (encode_sleb128(Value))/binary>>.

%%-----------------------------------------------------------------------------
%% Arithmetic Instructions
%%-----------------------------------------------------------------------------

%% @doc i32.add - Add two i32 values
-spec i32_add() -> binary().
i32_add() -> <<16#6A>>.

%% @doc i32.sub - Subtract two i32 values
-spec i32_sub() -> binary().
i32_sub() -> <<16#6B>>.

%% @doc i32.mul - Multiply two i32 values
-spec i32_mul() -> binary().
i32_mul() -> <<16#6C>>.

%% @doc i32.and - Bitwise AND
-spec i32_and() -> binary().
i32_and() -> <<16#71>>.

%% @doc i32.or - Bitwise OR
-spec i32_or() -> binary().
i32_or() -> <<16#72>>.

%% @doc i32.xor - Bitwise XOR
-spec i32_xor() -> binary().
i32_xor() -> <<16#73>>.

%% @doc i32.shl - Shift left
-spec i32_shl() -> binary().
i32_shl() -> <<16#74>>.

%% @doc i32.shr_s - Shift right (signed)
-spec i32_shr_s() -> binary().
i32_shr_s() -> <<16#75>>.

%% @doc i32.shr_u - Shift right (unsigned)
-spec i32_shr_u() -> binary().
i32_shr_u() -> <<16#76>>.

%%-----------------------------------------------------------------------------
%% Comparison Instructions
%%-----------------------------------------------------------------------------

%% @doc i32.eqz - Test if zero
-spec i32_eqz() -> binary().
i32_eqz() -> <<16#45>>.

%% @doc i32.eq - Test equality
-spec i32_eq() -> binary().
i32_eq() -> <<16#46>>.

%% @doc i32.ne - Test inequality
-spec i32_ne() -> binary().
i32_ne() -> <<16#47>>.

%% @doc i32.lt_s - Test less than (signed)
-spec i32_lt_s() -> binary().
i32_lt_s() -> <<16#48>>.

%% @doc i32.lt_u - Test less than (unsigned)
-spec i32_lt_u() -> binary().
i32_lt_u() -> <<16#49>>.

%% @doc i32.gt_s - Test greater than (signed)
-spec i32_gt_s() -> binary().
i32_gt_s() -> <<16#4A>>.

%% @doc i32.gt_u - Test greater than (unsigned)
-spec i32_gt_u() -> binary().
i32_gt_u() -> <<16#4B>>.

%% @doc i32.le_s - Test less than or equal (signed)
-spec i32_le_s() -> binary().
i32_le_s() -> <<16#4C>>.

%% @doc i32.le_u - Test less than or equal (unsigned)
-spec i32_le_u() -> binary().
i32_le_u() -> <<16#4D>>.

%% @doc i32.ge_s - Test greater than or equal (signed)
-spec i32_ge_s() -> binary().
i32_ge_s() -> <<16#4E>>.

%% @doc i32.ge_u - Test greater than or equal (unsigned)
-spec i32_ge_u() -> binary().
i32_ge_u() -> <<16#4F>>.

%%-----------------------------------------------------------------------------
%% Memory Instructions
%%-----------------------------------------------------------------------------

%% @doc i32.load align offset - Load i32 from memory
-spec i32_load(non_neg_integer(), non_neg_integer()) -> binary().
i32_load(Align, Offset) ->
    <<16#28, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.store align offset - Store i32 to memory
-spec i32_store(non_neg_integer(), non_neg_integer()) -> binary().
i32_store(Align, Offset) ->
    <<16#36, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.load8_s align offset - Load signed 8-bit and sign-extend
-spec i32_load8_s(non_neg_integer(), non_neg_integer()) -> binary().
i32_load8_s(Align, Offset) ->
    <<16#2C, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.load8_u align offset - Load unsigned 8-bit and zero-extend
-spec i32_load8_u(non_neg_integer(), non_neg_integer()) -> binary().
i32_load8_u(Align, Offset) ->
    <<16#2D, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.load16_s align offset - Load signed 16-bit and sign-extend
-spec i32_load16_s(non_neg_integer(), non_neg_integer()) -> binary().
i32_load16_s(Align, Offset) ->
    <<16#2E, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.load16_u align offset - Load unsigned 16-bit and zero-extend
-spec i32_load16_u(non_neg_integer(), non_neg_integer()) -> binary().
i32_load16_u(Align, Offset) ->
    <<16#2F, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.store8 align offset - Store 8-bit
-spec i32_store8(non_neg_integer(), non_neg_integer()) -> binary().
i32_store8(Align, Offset) ->
    <<16#3A, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc i32.store16 align offset - Store 16-bit
-spec i32_store16(non_neg_integer(), non_neg_integer()) -> binary().
i32_store16(Align, Offset) ->
    <<16#3B, (encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.
