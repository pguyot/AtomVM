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

-module(jit_wasm32_asm).

%% WASM32 binary encoding module for AtomVM JIT.
%%
%% This module encodes WebAssembly instructions and module structures
%% in the binary format specified by the WebAssembly specification.
%%
%% References:
%%   WebAssembly Specification: https://webassembly.github.io/spec/core/
%%   Binary encoding: https://webassembly.github.io/spec/core/binary/
%%
%% WASM is a stack-based virtual machine. Instead of register operands,
%% instructions push/pop values from an implicit operand stack.
%% Integers use LEB128 variable-length encoding.

-export([
    %% LEB128 encoding
    encode_uleb128/1,
    encode_sleb128/1,

    %% WASM types
    type_i32/0,
    type_i64/0,
    type_f32/0,
    type_f64/0,
    type_funcref/0,
    type_externref/0,
    blocktype_void/0,
    blocktype_i32/0,

    %% Control flow instructions
    unreachable/0,
    nop/0,
    block/1,
    loop/1,
    if_/1,
    else_/0,
    end_/0,
    br/1,
    br_if/1,
    br_table/2,
    return/0,
    call/1,
    call_indirect/2,

    %% Variable instructions
    local_get/1,
    local_set/1,
    local_tee/1,
    global_get/1,
    global_set/1,

    %% Memory instructions
    i32_load/2,
    i32_store/2,
    i32_load8_s/2,
    i32_load8_u/2,
    i32_load16_s/2,
    i32_load16_u/2,
    i32_store8/2,
    i32_store16/2,

    %% Numeric instructions - constants
    i32_const/1,
    i64_const/1,

    %% Numeric instructions - i32 operations
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
    i32_clz/0,
    i32_ctz/0,
    i32_add/0,
    i32_sub/0,
    i32_mul/0,
    i32_div_s/0,
    i32_div_u/0,
    i32_rem_s/0,
    i32_rem_u/0,
    i32_and/0,
    i32_or/0,
    i32_xor/0,
    i32_shl/0,
    i32_shr_s/0,
    i32_shr_u/0,

    %% Module structure encoding
    encode_section/2,
    encode_type_section/1,
    encode_function_section/1,
    encode_table_section/1,
    encode_export_section/1,
    encode_code_section/1,
    encode_func_type/2,
    encode_func_body/2,
    encode_vector/1,
    encode_name/1,
    wasm_magic/0,
    wasm_version/0,

    %% WASM local variable atom mapping
    wasm_local_index/1,
    index_to_wasm_local/1,
    is_wasm_local/1
]).

-compile([warnings_as_errors]).

%%=============================================================================
%% LEB128 encoding
%%=============================================================================

%% @doc Encode an unsigned integer in LEB128 format.
-spec encode_uleb128(non_neg_integer()) -> binary().
encode_uleb128(Value) when Value < 128 ->
    <<Value:8>>;
encode_uleb128(Value) ->
    <<1:1, (Value band 16#7F):7, (encode_uleb128(Value bsr 7))/binary>>.

%% @doc Encode a signed integer in LEB128 format.
-spec encode_sleb128(integer()) -> binary().
encode_sleb128(Value) when Value >= -64, Value < 64 ->
    <<(Value band 16#7F):8>>;
encode_sleb128(Value) ->
    <<1:1, (Value band 16#7F):7, (encode_sleb128(Value bsr 7))/binary>>.

%%=============================================================================
%% WASM value types
%%=============================================================================

type_i32() -> <<16#7F>>.
type_i64() -> <<16#7E>>.
type_f32() -> <<16#7D>>.
type_f64() -> <<16#7C>>.
type_funcref() -> <<16#70>>.
type_externref() -> <<16#6F>>.

%% Block types
blocktype_void() -> <<16#40>>.
blocktype_i32() -> <<16#7F>>.

%%=============================================================================
%% Control flow instructions (0x00-0x11)
%%=============================================================================

%% @doc Emit an unreachable instruction (trap).
-spec unreachable() -> binary().
unreachable() -> <<16#00>>.

%% @doc Emit a nop instruction.
-spec nop() -> binary().
nop() -> <<16#01>>.

%% @doc Emit a block instruction with the given block type.
-spec block(binary()) -> binary().
block(BlockType) -> <<16#02, BlockType/binary>>.

%% @doc Emit a loop instruction with the given block type.
-spec loop(binary()) -> binary().
loop(BlockType) -> <<16#03, BlockType/binary>>.

%% @doc Emit an if instruction with the given block type.
-spec if_(binary()) -> binary().
if_(BlockType) -> <<16#04, BlockType/binary>>.

%% @doc Emit an else instruction.
-spec else_() -> binary().
else_() -> <<16#05>>.

%% @doc Emit an end instruction (terminates block/loop/if/function).
-spec end_() -> binary().
end_() -> <<16#0B>>.

%% @doc Emit a branch to a label (depth index).
-spec br(non_neg_integer()) -> binary().
br(LabelIdx) -> <<16#0C, (encode_uleb128(LabelIdx))/binary>>.

%% @doc Emit a conditional branch to a label.
-spec br_if(non_neg_integer()) -> binary().
br_if(LabelIdx) -> <<16#0D, (encode_uleb128(LabelIdx))/binary>>.

%% @doc Emit a branch table with label indices and a default label.
-spec br_table([non_neg_integer()], non_neg_integer()) -> binary().
br_table(Labels, DefaultLabel) ->
    LabelsBin = lists:foldl(
        fun(L, Acc) -> <<Acc/binary, (encode_uleb128(L))/binary>> end,
        <<>>,
        Labels
    ),
    <<16#0E, (encode_uleb128(length(Labels)))/binary,
      LabelsBin/binary, (encode_uleb128(DefaultLabel))/binary>>.

%% @doc Emit a return instruction.
-spec return() -> binary().
return() -> <<16#0F>>.

%% @doc Emit a call to a function by index.
-spec call(non_neg_integer()) -> binary().
call(FuncIdx) -> <<16#10, (encode_uleb128(FuncIdx))/binary>>.

%% @doc Emit an indirect call through a table.
-spec call_indirect(non_neg_integer(), non_neg_integer()) -> binary().
call_indirect(TypeIdx, TableIdx) ->
    <<16#11, (encode_uleb128(TypeIdx))/binary, (encode_uleb128(TableIdx))/binary>>.

%%=============================================================================
%% Variable instructions (0x20-0x24)
%%=============================================================================

%% @doc Get a local variable by index.
-spec local_get(non_neg_integer() | atom()) -> binary().
local_get(LocalIdx) when is_integer(LocalIdx) -> <<16#20, (encode_uleb128(LocalIdx))/binary>>;
local_get(LocalAtom) when is_atom(LocalAtom) -> <<16#20, (encode_uleb128(wasm_local_index(LocalAtom)))/binary>>.

%% @doc Set a local variable by index.
-spec local_set(non_neg_integer() | atom()) -> binary().
local_set(LocalIdx) when is_integer(LocalIdx) -> <<16#21, (encode_uleb128(LocalIdx))/binary>>;
local_set(LocalAtom) when is_atom(LocalAtom) -> <<16#21, (encode_uleb128(wasm_local_index(LocalAtom)))/binary>>.

%% @doc Tee a local variable (set and keep value on stack).
-spec local_tee(non_neg_integer() | atom()) -> binary().
local_tee(LocalIdx) when is_integer(LocalIdx) -> <<16#22, (encode_uleb128(LocalIdx))/binary>>;
local_tee(LocalAtom) when is_atom(LocalAtom) -> <<16#22, (encode_uleb128(wasm_local_index(LocalAtom)))/binary>>.

%% @doc Get a global variable by index.
-spec global_get(non_neg_integer()) -> binary().
global_get(GlobalIdx) -> <<16#23, (encode_uleb128(GlobalIdx))/binary>>.

%% @doc Set a global variable by index.
-spec global_set(non_neg_integer()) -> binary().
global_set(GlobalIdx) -> <<16#24, (encode_uleb128(GlobalIdx))/binary>>.

%%=============================================================================
%% Memory instructions (0x28-0x3E)
%%=============================================================================

%% Memory arguments: alignment (log2) and offset, both as uleb128
-spec memarg(non_neg_integer(), non_neg_integer()) -> binary().
memarg(Align, Offset) ->
    <<(encode_uleb128(Align))/binary, (encode_uleb128(Offset))/binary>>.

%% @doc Load a 32-bit integer from memory.
-spec i32_load(non_neg_integer(), non_neg_integer()) -> binary().
i32_load(Align, Offset) -> <<16#28, (memarg(Align, Offset))/binary>>.

%% @doc Store a 32-bit integer to memory.
-spec i32_store(non_neg_integer(), non_neg_integer()) -> binary().
i32_store(Align, Offset) -> <<16#36, (memarg(Align, Offset))/binary>>.

%% @doc Load an 8-bit signed integer from memory, sign-extended to i32.
-spec i32_load8_s(non_neg_integer(), non_neg_integer()) -> binary().
i32_load8_s(Align, Offset) -> <<16#2C, (memarg(Align, Offset))/binary>>.

%% @doc Load an 8-bit unsigned integer from memory, zero-extended to i32.
-spec i32_load8_u(non_neg_integer(), non_neg_integer()) -> binary().
i32_load8_u(Align, Offset) -> <<16#2D, (memarg(Align, Offset))/binary>>.

%% @doc Load a 16-bit signed integer from memory, sign-extended to i32.
-spec i32_load16_s(non_neg_integer(), non_neg_integer()) -> binary().
i32_load16_s(Align, Offset) -> <<16#2E, (memarg(Align, Offset))/binary>>.

%% @doc Load a 16-bit unsigned integer from memory, zero-extended to i32.
-spec i32_load16_u(non_neg_integer(), non_neg_integer()) -> binary().
i32_load16_u(Align, Offset) -> <<16#2F, (memarg(Align, Offset))/binary>>.

%% @doc Store the low 8 bits of an i32 to memory.
-spec i32_store8(non_neg_integer(), non_neg_integer()) -> binary().
i32_store8(Align, Offset) -> <<16#3A, (memarg(Align, Offset))/binary>>.

%% @doc Store the low 16 bits of an i32 to memory.
-spec i32_store16(non_neg_integer(), non_neg_integer()) -> binary().
i32_store16(Align, Offset) -> <<16#3B, (memarg(Align, Offset))/binary>>.

%%=============================================================================
%% Numeric instructions - constants (0x41-0x44)
%%=============================================================================

%% @doc Push an i32 constant onto the stack.
-spec i32_const(integer()) -> binary().
i32_const(Value) -> <<16#41, (encode_sleb128(Value))/binary>>.

%% @doc Push an i64 constant onto the stack.
-spec i64_const(integer()) -> binary().
i64_const(Value) -> <<16#42, (encode_sleb128(Value))/binary>>.

%%=============================================================================
%% Numeric instructions - i32 comparison (0x45-0x50)
%%=============================================================================

i32_eqz() -> <<16#45>>.
i32_eq() -> <<16#46>>.
i32_ne() -> <<16#47>>.
i32_lt_s() -> <<16#48>>.
i32_lt_u() -> <<16#49>>.
i32_gt_s() -> <<16#4A>>.
i32_gt_u() -> <<16#4B>>.
i32_le_s() -> <<16#4C>>.
i32_le_u() -> <<16#4D>>.
i32_ge_s() -> <<16#4E>>.
i32_ge_u() -> <<16#4F>>.

%%=============================================================================
%% Numeric instructions - i32 arithmetic/bitwise (0x67-0x78)
%%=============================================================================

i32_clz() -> <<16#67>>.
i32_ctz() -> <<16#68>>.
i32_add() -> <<16#6A>>.
i32_sub() -> <<16#6B>>.
i32_mul() -> <<16#6C>>.
i32_div_s() -> <<16#6D>>.
i32_div_u() -> <<16#6E>>.
i32_rem_s() -> <<16#6F>>.
i32_rem_u() -> <<16#70>>.
i32_and() -> <<16#71>>.
i32_or() -> <<16#72>>.
i32_xor() -> <<16#73>>.
i32_shl() -> <<16#74>>.
i32_shr_s() -> <<16#75>>.
i32_shr_u() -> <<16#76>>.

%%=============================================================================
%% Module structure encoding
%%=============================================================================

%% @doc WASM binary magic number.
-spec wasm_magic() -> binary().
wasm_magic() -> <<16#00, 16#61, 16#73, 16#6D>>.

%% @doc WASM binary version (1).
-spec wasm_version() -> binary().
wasm_version() -> <<16#01, 16#00, 16#00, 16#00>>.

%% @doc Encode a section with a section ID and content.
-spec encode_section(non_neg_integer(), binary()) -> binary().
encode_section(SectionId, Content) ->
    <<SectionId:8, (encode_uleb128(byte_size(Content)))/binary, Content/binary>>.

%% @doc Encode the type section (section ID 1).
-spec encode_type_section([binary()]) -> binary().
encode_type_section(FuncTypes) ->
    encode_section(1, encode_vector(FuncTypes)).

%% @doc Encode the function section (section ID 3).
-spec encode_function_section([non_neg_integer()]) -> binary().
encode_function_section(TypeIndices) ->
    Encoded = lists:foldl(
        fun(Idx, Acc) -> <<Acc/binary, (encode_uleb128(Idx))/binary>> end,
        <<>>,
        TypeIndices
    ),
    encode_section(3, <<(encode_uleb128(length(TypeIndices)))/binary, Encoded/binary>>).

%% @doc Encode the table section (section ID 4).
-spec encode_table_section([binary()]) -> binary().
encode_table_section(Tables) ->
    encode_section(4, encode_vector(Tables)).

%% @doc Encode the export section (section ID 7).
-spec encode_export_section([binary()]) -> binary().
encode_export_section(Exports) ->
    encode_section(7, encode_vector(Exports)).

%% @doc Encode the code section (section ID 10).
-spec encode_code_section([binary()]) -> binary().
encode_code_section(FuncBodies) ->
    encode_section(10, encode_vector(FuncBodies)).

%% @doc Encode a function type (0x60 prefix).
-spec encode_func_type([binary()], [binary()]) -> binary().
encode_func_type(ParamTypes, ResultTypes) ->
    Params = iolist_to_binary(ParamTypes),
    Results = iolist_to_binary(ResultTypes),
    <<16#60,
      (encode_uleb128(length(ParamTypes)))/binary, Params/binary,
      (encode_uleb128(length(ResultTypes)))/binary, Results/binary>>.

%% @doc Encode a function body (locals + expression).
-spec encode_func_body([{non_neg_integer(), binary()}], binary()) -> binary().
encode_func_body(Locals, Expression) ->
    LocalsBin = lists:foldl(
        fun({Count, Type}, Acc) ->
            <<Acc/binary, (encode_uleb128(Count))/binary, Type/binary>>
        end,
        <<>>,
        Locals
    ),
    Body = <<(encode_uleb128(length(Locals)))/binary, LocalsBin/binary, Expression/binary>>,
    <<(encode_uleb128(byte_size(Body)))/binary, Body/binary>>.

%% @doc Encode a vector (count + elements).
-spec encode_vector([binary()]) -> binary().
encode_vector(Elements) ->
    Encoded = iolist_to_binary(Elements),
    <<(encode_uleb128(length(Elements)))/binary, Encoded/binary>>.

%% @doc Encode a name as a length-prefixed UTF-8 string.
-spec encode_name(string()) -> binary().
encode_name(Name) ->
    Bin = unicode:characters_to_binary(Name),
    <<(encode_uleb128(byte_size(Bin)))/binary, Bin/binary>>.

%%=============================================================================
%% WASM local variable atom mapping
%%=============================================================================

%% @doc Convert a WASM local atom (local0, local1, ...) to its integer index.
-spec wasm_local_index(atom()) -> non_neg_integer().
wasm_local_index(local0) -> 0;
wasm_local_index(local1) -> 1;
wasm_local_index(local2) -> 2;
wasm_local_index(local3) -> 3;
wasm_local_index(local4) -> 4;
wasm_local_index(local5) -> 5;
wasm_local_index(local6) -> 6;
wasm_local_index(local7) -> 7;
wasm_local_index(local8) -> 8;
wasm_local_index(local9) -> 9;
wasm_local_index(local10) -> 10;
wasm_local_index(local11) -> 11;
wasm_local_index(local12) -> 12;
wasm_local_index(local13) -> 13;
wasm_local_index(local14) -> 14;
wasm_local_index(local15) -> 15;
wasm_local_index(local16) -> 16;
wasm_local_index(local17) -> 17;
wasm_local_index(local18) -> 18;
wasm_local_index(local19) -> 19;
wasm_local_index(local20) -> 20;
wasm_local_index(local21) -> 21;
wasm_local_index(local22) -> 22;
wasm_local_index(local23) -> 23;
wasm_local_index(local24) -> 24;
wasm_local_index(local25) -> 25;
wasm_local_index(local26) -> 26;
wasm_local_index(local27) -> 27;
wasm_local_index(local28) -> 28;
wasm_local_index(local29) -> 29;
wasm_local_index(local30) -> 30;
wasm_local_index(local31) -> 31;
wasm_local_index(Atom) ->
    %% For dynamically created local atoms beyond local31
    "local" ++ IndexStr = atom_to_list(Atom),
    list_to_integer(IndexStr).

%% @doc Convert an integer index to a WASM local atom (local0, local1, ...).
-spec index_to_wasm_local(non_neg_integer()) -> atom().
index_to_wasm_local(0) -> local0;
index_to_wasm_local(1) -> local1;
index_to_wasm_local(2) -> local2;
index_to_wasm_local(3) -> local3;
index_to_wasm_local(4) -> local4;
index_to_wasm_local(5) -> local5;
index_to_wasm_local(6) -> local6;
index_to_wasm_local(7) -> local7;
index_to_wasm_local(8) -> local8;
index_to_wasm_local(9) -> local9;
index_to_wasm_local(10) -> local10;
index_to_wasm_local(11) -> local11;
index_to_wasm_local(12) -> local12;
index_to_wasm_local(13) -> local13;
index_to_wasm_local(14) -> local14;
index_to_wasm_local(15) -> local15;
index_to_wasm_local(16) -> local16;
index_to_wasm_local(17) -> local17;
index_to_wasm_local(18) -> local18;
index_to_wasm_local(19) -> local19;
index_to_wasm_local(20) -> local20;
index_to_wasm_local(21) -> local21;
index_to_wasm_local(22) -> local22;
index_to_wasm_local(23) -> local23;
index_to_wasm_local(24) -> local24;
index_to_wasm_local(25) -> local25;
index_to_wasm_local(26) -> local26;
index_to_wasm_local(27) -> local27;
index_to_wasm_local(28) -> local28;
index_to_wasm_local(29) -> local29;
index_to_wasm_local(30) -> local30;
index_to_wasm_local(31) -> local31;
index_to_wasm_local(N) ->
    list_to_atom("local" ++ integer_to_list(N)).

%% @doc Check if an atom is a WASM local variable atom.
-spec is_wasm_local(term()) -> boolean().
is_wasm_local(Atom) when is_atom(Atom) ->
    case atom_to_list(Atom) of
        "local" ++ Rest ->
            case catch list_to_integer(Rest) of
                N when is_integer(N), N >= 0 -> true;
                _ -> false
            end;
        _ -> false
    end;
is_wasm_local(_) -> false.
