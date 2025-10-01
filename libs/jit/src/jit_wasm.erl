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

-module(jit_wasm).

-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    flush/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/1,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_equal_to_ctx/2,
    jump_to_label/2,
    jump_to_continuation/2,
    jump_to_offset/2,
    if_block/3,
    if_else_block/4,
    shift_right/3,
    shift_left/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_native_register/3,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    move_to_array_element/5,
    set_bs/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    get_module_index/1,
    and_/3,
    or_/3,
    add/3,
    sub/3,
    mul/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3
]).

-compile([warnings_as_errors]).

-include_lib("jit.hrl").

-include("primitives.hrl").

-define(ASSERT(Expr), true = Expr).

%% WASM32 uses 32-bit pointers
%% Local allocation strategy:
%%   Local 0: ctx (i32 pointer) - parameter
%%   Local 1: jit_state (i32 pointer) - parameter
%%   Local 2: native_interface (i32 pointer) - parameter
%%   Local 3-8: Scratch locals (6 total, matching ARMv6-M)
%%
%% Function signature: (param i32 i32 i32) (result i32)

-type wasm_local() :: {local, 0..8}.

-type stream() :: any().

-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    available_locals :: [wasm_local()],
    used_locals :: [wasm_local()],
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    control_stack :: [wasm_label()]
}).

-type state() :: #state{}.
-type wasm_label() :: {block | loop | if_, non_neg_integer()}.
-type immediate() :: non_neg_integer().
-type vm_register() ::
    {x_reg, non_neg_integer()} | {y_reg, non_neg_integer()} | {ptr, wasm_local()}.
-type value() :: immediate() | vm_register() | wasm_local() | {ptr, wasm_local()}.
-type arg() :: ctx | jit_state | offset | value() | {free, value()} | {avm_int64_t, integer()}.

-type maybe_free_wasm_local() ::
    {free, wasm_local()} | wasm_local().

-type condition() ::
    {wasm_local(), '<', integer()}
    | {maybe_free_wasm_local(), '<', wasm_local()}
    | {maybe_free_wasm_local(), '==', integer()}
    | {maybe_free_wasm_local(), '!=', wasm_local() | integer()}
    | {'(int)', maybe_free_wasm_local(), '==', integer()}
    | {'(int)', maybe_free_wasm_local(), '!=', wasm_local() | integer()}
    | {'(bool)', maybe_free_wasm_local(), '==', false}
    | {'(bool)', maybe_free_wasm_local(), '!=', false}
    | {maybe_free_wasm_local(), '&', non_neg_integer(), '!=', integer()}
    | {{free, wasm_local()}, '==', {free, wasm_local()}}.

% Memory layout (same as other backends)
-define(CTX_LOCAL, {local, 0}).
-define(JITSTATE_LOCAL, {local, 1}).
-define(NATIVE_INTERFACE_LOCAL, {local, 2}).

% Context structure offsets (match ARMv6-M)
-define(Y_REGS_OFFSET, 16#14).
-define(X_REG_OFFSET(N), 16#18 + (N * 4)).
-define(CP_OFFSET, 16#5C).
-define(FP_REGS_OFFSET, 16#60).
-define(BS_OFFSET, 16#64).
-define(BS_OFFSET_OFFSET, 16#68).

% JITState structure offsets
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 16#4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 16#8).

-define(JUMP_TABLE_ENTRY_SIZE, 12).

% Available scratch locals (6 total, matching ARMv6-M)
-define(AVAILABLE_LOCALS, [{local, 3}, {local, 4}, {local, 5}, {local, 6}, {local, 7}, {local, 8}]).

%%-----------------------------------------------------------------------------
%% @doc Return the word size in bytes, i.e. the sizeof(term) i.e.
%% sizeof(uintptr_t)
%%
%% For WASM32, this is 4 bytes (32-bit pointers)
%%
%% @end
%% @return Word size in bytes
%%-----------------------------------------------------------------------------
-spec word_size() -> 4 | 8.
word_size() -> 4.

%%-----------------------------------------------------------------------------
%% @doc Create a new backend state for provided variant, module and stream.
%% @end
%% @param Variant JIT variant to use (currently ?JIT_VARIANT_PIC)
%% @param StreamModule module to stream instructions
%% @param Stream stream state
%% @return New backend state
%%-----------------------------------------------------------------------------
-spec new(any(), module(), stream()) -> state().
new(Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        offset = StreamModule:offset(Stream),
        available_locals = ?AVAILABLE_LOCALS,
        used_locals = [],
        labels = [],
        variant = Variant,
        control_stack = []
    }.

%%-----------------------------------------------------------------------------
%% @doc Access the stream object.
%% @end
%% @param State current backend state
%% @return The stream object
%%-----------------------------------------------------------------------------
-spec stream(state()) -> stream().
stream(#state{stream = Stream}) ->
    Stream.

%%-----------------------------------------------------------------------------
%% @doc Get the current offset in the stream
%% @end
%% @param State current backend state
%% @return The current offset
%%-----------------------------------------------------------------------------
-spec offset(state()) -> non_neg_integer().
offset(#state{stream_module = StreamModule, stream = Stream}) ->
    StreamModule:offset(Stream).

%%-----------------------------------------------------------------------------
%% @doc Flush the stream.
%% @end
%% @param State current backend state
%% @return The new state
%%-----------------------------------------------------------------------------
-spec flush(state()) -> stream().
flush(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    Stream1 = StreamModule:flush(Stream0),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Emit a debugger breakpoint instruction.
%% For WASM, we use the unreachable instruction.
%% @end
%% @param State current backend state
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec debugger(state()) -> state().
debugger(#state{stream_module = StreamModule, stream = Stream0} = State) ->
    % WASM unreachable instruction (opcode 0x00)
    Stream1 = StreamModule:append(Stream0, <<16#00>>),
    State#state{stream = Stream1}.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently used native locals.
%% @end
%% @param State current backend state
%% @return The list of used locals
%%-----------------------------------------------------------------------------
-spec used_regs(state()) -> [wasm_local()].
used_regs(#state{used_locals = Used}) -> Used.

%%-----------------------------------------------------------------------------
%% @doc Return the list of currently available native scratch locals.
%% @end
%% @param State current backend state
%% @return The list of available locals
%%-----------------------------------------------------------------------------
-spec available_regs(state()) -> [wasm_local()].
available_regs(#state{available_locals = Available}) -> Available.

%%-----------------------------------------------------------------------------
%% @doc Free native locals. The passed list can contain locals, pointers to
%% locals or other values that are ignored.
%% @end
%% @param State current backend state
%% @param Locals list of locals or other values
%% @return The updated backend state
%%-----------------------------------------------------------------------------
-spec free_native_registers(state(), [value()]) -> state().
free_native_registers(State, []) ->
    State;
free_native_registers(State, [Local | Rest]) ->
    State1 = free_native_register(State, Local),
    free_native_registers(State1, Rest).

%% Private helper to free a single register
-spec free_native_register(state(), value()) -> state().
free_native_register(
    #state{available_locals = Available, used_locals = Used} = State, {local, _} = Local
) ->
    State#state{
        available_locals = [Local | Available],
        used_locals = lists:delete(Local, Used)
    };
free_native_register(State, {ptr, Local}) ->
    free_native_register(State, Local);
free_native_register(State, _Other) ->
    State.

%%-----------------------------------------------------------------------------
%% @doc Assert that all native registers are free. This is used for debugging.
%% @end
%% @param State current backend state
%% @return true if all registers are free
%%-----------------------------------------------------------------------------
-spec assert_all_native_free(state()) -> true.
assert_all_native_free(#state{used_locals = []}) ->
    true;
assert_all_native_free(#state{used_locals = Used}) ->
    error({used_locals_not_freed, Used}).

%%-----------------------------------------------------------------------------
%% Stubs for remaining functions - to be implemented in subsequent commits
%%-----------------------------------------------------------------------------

-spec jump_table(state(), non_neg_integer()) -> {state(), wasm_local()}.
jump_table(_State, _Size) ->
    error(not_implemented).

-spec update_branches(state()) -> state().
update_branches(State) ->
    % TODO: implement branch resolution
    State.

-spec call_primitive(state(), non_neg_integer(), [arg()]) -> state().
call_primitive(_State, _Index, _Args) ->
    error(not_implemented).

-spec call_primitive_last(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_last(_State, _Index, _Args) ->
    error(not_implemented).

-spec call_primitive_with_cp(state(), non_neg_integer(), [arg()]) -> state().
call_primitive_with_cp(_State, _Index, _Args) ->
    error(not_implemented).

-spec return_if_not_equal_to_ctx(state(), wasm_local()) -> state().
return_if_not_equal_to_ctx(_State, _Local) ->
    error(not_implemented).

-spec jump_to_label(state(), integer() | reference()) -> state().
jump_to_label(_State, _Label) ->
    error(not_implemented).

-spec jump_to_continuation(state(), wasm_local()) -> state().
jump_to_continuation(_State, _Local) ->
    error(not_implemented).

-spec jump_to_offset(state(), non_neg_integer()) -> state().
jump_to_offset(_State, _Offset) ->
    error(not_implemented).

-spec if_block(state(), condition(), fun((state()) -> state())) -> state().
if_block(_State, _Condition, _ThenFun) ->
    error(not_implemented).

-spec if_else_block(
    state(), condition(), fun((state()) -> state()), fun((state()) -> state())
) -> state().
if_else_block(_State, _Condition, _ThenFun, _ElseFun) ->
    error(not_implemented).

-spec shift_right(state(), wasm_local(), non_neg_integer()) -> state().
shift_right(_State, _Local, _Shift) ->
    error(not_implemented).

-spec shift_left(state(), wasm_local(), non_neg_integer()) -> state().
shift_left(_State, _Local, _Shift) ->
    error(not_implemented).

-spec move_to_vm_register(state(), vm_register(), wasm_local()) -> state().
move_to_vm_register(_State, _VMReg, _Local) ->
    error(not_implemented).

-spec move_to_native_register(state(), value()) -> {state(), wasm_local()}.
move_to_native_register(_State, _Value) ->
    error(not_implemented).

-spec move_to_native_register(state(), value(), wasm_local()) -> state().
move_to_native_register(_State, _Value, _Local) ->
    error(not_implemented).

-spec move_to_cp(state(), wasm_local()) -> state().
move_to_cp(_State, _Local) ->
    error(not_implemented).

-spec move_array_element(state(), wasm_local(), wasm_local(), non_neg_integer()) -> state().
move_array_element(_State, _Dest, _Base, _Offset) ->
    error(not_implemented).

-spec move_to_array_element(state(), wasm_local(), wasm_local(), non_neg_integer()) -> state().
move_to_array_element(_State, _Src, _Base, _Offset) ->
    error(not_implemented).

-spec move_to_array_element(
    state(), wasm_local(), wasm_local(), wasm_local(), non_neg_integer()
) -> state().
move_to_array_element(_State, _Src, _Base, _Index, _Scale) ->
    error(not_implemented).

-spec set_bs(state(), wasm_local()) -> state().
set_bs(_State, _Local) ->
    error(not_implemented).

-spec copy_to_native_register(state(), wasm_local()) -> {state(), wasm_local()}.
copy_to_native_register(_State, _Src) ->
    error(not_implemented).

-spec get_array_element(state(), wasm_local(), non_neg_integer()) -> {state(), wasm_local()}.
get_array_element(_State, _Base, _Offset) ->
    error(not_implemented).

-spec increment_sp(state(), integer()) -> state().
increment_sp(_State, _Amount) ->
    error(not_implemented).

-spec set_continuation_to_label(state(), integer() | reference()) -> state().
set_continuation_to_label(_State, _Label) ->
    error(not_implemented).

-spec set_continuation_to_offset(non_neg_integer()) -> state().
set_continuation_to_offset(_Offset) ->
    error(not_implemented).

-spec continuation_entry_point(state()) -> state().
continuation_entry_point(_State) ->
    error(not_implemented).

-spec get_module_index(state()) -> {state(), wasm_local()}.
get_module_index(_State) ->
    error(not_implemented).

-spec and_(state(), wasm_local(), wasm_local()) -> state().
and_(_State, _Dest, _Src) ->
    error(not_implemented).

-spec or_(state(), wasm_local(), wasm_local()) -> state().
or_(_State, _Dest, _Src) ->
    error(not_implemented).

-spec add(state(), wasm_local(), wasm_local()) -> state().
add(_State, _Dest, _Src) ->
    error(not_implemented).

-spec sub(state(), wasm_local(), wasm_local()) -> state().
sub(_State, _Dest, _Src) ->
    error(not_implemented).

-spec mul(state(), wasm_local(), wasm_local()) -> state().
mul(_State, _Dest, _Src) ->
    error(not_implemented).

-spec decrement_reductions_and_maybe_schedule_next(state()) -> state().
decrement_reductions_and_maybe_schedule_next(_State) ->
    error(not_implemented).

-spec call_or_schedule_next(state(), integer() | reference()) -> state().
call_or_schedule_next(_State, _Label) ->
    error(not_implemented).

-spec call_only_or_schedule_next(state(), integer() | reference()) -> state().
call_only_or_schedule_next(_State, _Label) ->
    error(not_implemented).

-spec call_func_ptr(state(), wasm_local(), [arg()]) -> state().
call_func_ptr(_State, _FuncPtr, _Args) ->
    error(not_implemented).

-spec return_labels_and_lines(state(), [{integer(), integer()}]) ->
    {state(), wasm_local(), [{integer(), integer()}]}.
return_labels_and_lines(_State, _LabelsAndLines) ->
    error(not_implemented).

-spec add_label(state(), integer() | reference()) -> state().
add_label(_State, _Label) ->
    error(not_implemented).

-spec add_label(state(), integer() | reference(), atom()) -> state().
add_label(_State, _Label, _Type) ->
    error(not_implemented).
