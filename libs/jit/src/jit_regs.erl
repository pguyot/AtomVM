%
% This file is part of AtomVM.
%
% Copyright 2025-2026 Paul Guyot <pguyot@kallisys.net>
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

%% @doc Track CPU register contents and term types for the JIT backend.
%%
%% This module maintains knowledge about what each CPU register holds
%% (VM x/y register values, immediates, pointers) and what term types
%% are known, enabling the backend to skip redundant loads and expensive
%% type checks.
%%
%% Tracking must be invalidated at:
%% - Labels (any offset can be a branch target)
%% - Function calls (ABI clobbers caller-saved registers)
%% - Branches (the target has unknown incoming register state)
%%
%% The tracked information includes:
%% - `contents`: maps cpu_reg -> what the register holds
%% - `types`: maps vm_loc -> known term type information

-module(jit_regs).

-export([
    new/0,
    get_contents/2,
    set_contents/3,
    invalidate_reg/2,
    invalidate_all/1,
    invalidate_volatile/2,
    invalidate_vm_loc/2,
    find_reg_with_contents/2,
    set_type/3,
    get_type/2,
    invalidate_type/2,
    invalidate_all_types/1,
    merge/2,
    stack_push/2,
    stack_pop/1,
    stack_clear/1,
    stack_contents/1,
    value_to_contents/2,
    vm_dest_to_contents/2
]).

-export_type([regs/0, contents/0, term_type/0]).

%% What a CPU register holds
-type contents() ::
    %% Register holds the value of VM x register N
    {x_reg, non_neg_integer()}
    %% Register holds the value of VM y register N
    | {y_reg, non_neg_integer()}
    %% Register holds a pointer to unboxed data of x/y reg
    | {ptr, {x_reg, non_neg_integer()}}
    | {ptr, {y_reg, non_neg_integer()}}
    %% Register holds a known immediate value
    | {imm, integer()}
    %% Register holds the address of CP
    | cp
    %% Register holds the module index
    | module_index
    %% Unknown / clobbered
    | unknown.

%% Term type information
-type term_type() ::
    %% Known to be a small integer (fixnum)
    integer
    %% Known to be any integer (small or bignum)
    | any_integer
    %% Known to be an atom
    | atom
    %% Known to be a list (nonempty)
    | nonempty_list
    %% Known to be a list (possibly empty, i.e. list | nil)
    | list
    %% Known to be nil
    | nil
    %% Known to be a tuple
    | tuple
    %% Known to be a tuple with given arity
    | {tuple, non_neg_integer()}
    %% Known to be a binary
    | binary
    %% Known to be a pid
    | pid
    %% Known to be a boolean (true | false)
    | boolean
    %% Known to be a number (integer or float)
    | number
    %% Known to be a float
    | float
    %% No type info
    | unknown.

-type vm_loc() ::
    {x_reg, non_neg_integer()}
    | {y_reg, non_neg_integer()}.

-record(regs, {
    %% Map from CPU register atom -> contents()
    contents = #{} :: #{atom() => contents()},
    %% Map from VM location -> term_type()
    types = #{} :: #{vm_loc() => term_type()},
    %% C stack contents (list, top first)
    stack = [] :: [atom() | contents()]
}).

-opaque regs() :: #regs{}.

%% @doc Create a new empty register tracking state.
-spec new() -> regs().
new() ->
    #regs{}.

%% @doc Get what a CPU register currently holds.
-spec get_contents(regs(), atom()) -> contents().
get_contents(#regs{contents = C}, Reg) ->
    maps:get(Reg, C, unknown).

%% @doc Record that a CPU register now holds the given contents.
-spec set_contents(regs(), atom(), contents()) -> regs().
set_contents(#regs{contents = C} = Regs, Reg, Contents) ->
    Regs#regs{contents = C#{Reg => Contents}}.

%% @doc Invalidate tracking for a single CPU register (e.g. it was clobbered).
-spec invalidate_reg(regs(), atom()) -> regs().
invalidate_reg(#regs{contents = C} = Regs, Reg) ->
    Regs#regs{contents = maps:remove(Reg, C)}.

%% @doc Invalidate all register tracking (e.g. at a label or unknown branch target).
-spec invalidate_all(regs()) -> regs().
invalidate_all(Regs) ->
    Regs#regs{contents = #{}, stack = []}.

%% @doc Invalidate registers that are volatile across a C function call.
%% On x86-64 System V ABI, all our scratch registers (rax, rcx, rdx, rsi, rdi,
%% r8, r9, r10, r11) are caller-saved, so after a C call they're all clobbered.
%% However, the special registers (rdi=ctx, rsi=jit_state, rdx=native_interface)
%% are restored by the JIT after the call via push/pop, so we keep their tracking.
-spec invalidate_volatile(regs(), [atom()]) -> regs().
invalidate_volatile(#regs{contents = C0} = Regs, PreservedRegs) ->
    C1 = maps:filter(fun(Reg, _) -> lists:member(Reg, PreservedRegs) end, C0),
    Regs#regs{contents = C1}.

%% @doc Invalidate all CPU registers that reference a given VM location.
%% Call this when a VM register is written to, so that any CPU register
%% that was caching its old value is invalidated.
-spec invalidate_vm_loc(regs(), contents()) -> regs().
invalidate_vm_loc(#regs{contents = C} = Regs, VmLoc) ->
    C1 = maps:filter(fun(_Reg, Val) -> Val =/= VmLoc end, C),
    Regs#regs{contents = C1}.

%% @doc Find a CPU register that holds the given contents.
%% Returns `{ok, Reg}` or `none`.
-spec find_reg_with_contents(regs(), contents()) -> {ok, atom()} | none.
find_reg_with_contents(#regs{contents = C}, Contents) ->
    find_in_map(maps:iterator(C), Contents).

find_in_map(Iterator, Contents) ->
    case maps:next(Iterator) of
        {Reg, Contents, _Next} -> {ok, Reg};
        {_Reg, _Other, Next} -> find_in_map(Next, Contents);
        none -> none
    end.

%% @doc Set the known type for a VM location.
-spec set_type(regs(), vm_loc(), term_type()) -> regs().
set_type(#regs{types = T} = Regs, Loc, Type) ->
    Regs#regs{types = T#{Loc => Type}}.

%% @doc Get the known type for a VM location.
-spec get_type(regs(), vm_loc()) -> term_type().
get_type(#regs{types = T}, Loc) ->
    maps:get(Loc, T, unknown).

%% @doc Invalidate type information for a VM location (e.g. it was written to).
-spec invalidate_type(regs(), vm_loc()) -> regs().
invalidate_type(#regs{types = T} = Regs, Loc) ->
    Regs#regs{types = maps:remove(Loc, T)}.

%% @doc Invalidate all type information (e.g. after a GC or function call).
-spec invalidate_all_types(regs()) -> regs().
invalidate_all_types(Regs) ->
    Regs#regs{types = #{}}.

%% @doc Merge two register tracking states (for control flow merge points).
%% Only keeps information that is consistent in both states.
-spec merge(regs(), regs()) -> regs().
merge(#regs{contents = C1, types = T1}, #regs{contents = C2, types = T2}) ->
    %% Keep only entries that match in both maps
    MergedContents = maps:filter(
        fun(Reg, Val) -> maps:get(Reg, C2, undefined) =:= Val end,
        C1
    ),
    MergedTypes = maps:filter(
        fun(Loc, Type) -> maps:get(Loc, T2, undefined) =:= Type end,
        T1
    ),
    #regs{contents = MergedContents, types = MergedTypes, stack = []}.

%% @doc Record a push to the C stack.
-spec stack_push(regs(), atom() | contents()) -> regs().
stack_push(#regs{stack = S} = Regs, Value) ->
    Regs#regs{stack = [Value | S]}.

%% @doc Record a pop from the C stack.
-spec stack_pop(regs()) -> {atom() | contents(), regs()}.
stack_pop(#regs{stack = [Top | Rest]} = Regs) ->
    {Top, Regs#regs{stack = Rest}};
stack_pop(#regs{stack = []} = Regs) ->
    {unknown, Regs}.

%% @doc Clear the C stack tracking.
-spec stack_clear(regs()) -> regs().
stack_clear(Regs) ->
    Regs#regs{stack = []}.

%% @doc Get the current C stack contents.
-spec stack_contents(regs()) -> [atom() | contents()].
stack_contents(#regs{stack = S}) -> S.

%% @doc Convert a backend value to a contents descriptor for tracking.
%% This is shared across all backends since the value types are the same.
%% MaxReg is the maximum number of x registers (typically ?MAX_REG from jit.hrl).
-spec value_to_contents(term(), non_neg_integer()) -> contents().
value_to_contents(cp, _MaxReg) -> cp;
value_to_contents({x_reg, N}, _MaxReg) when is_integer(N) -> {x_reg, N};
value_to_contents({x_reg, extra}, MaxReg) -> {x_reg, MaxReg};
value_to_contents({y_reg, N}, _MaxReg) -> {y_reg, N};
value_to_contents(Imm, _MaxReg) when is_integer(Imm) -> {imm, Imm};
value_to_contents({ptr, _}, _MaxReg) -> unknown;
value_to_contents(_, _MaxReg) -> unknown.

%% @doc Convert a VM destination register to a contents descriptor for tracking.
%% MaxReg is the maximum number of x registers (typically ?MAX_REG from jit.hrl).
-spec vm_dest_to_contents(term(), non_neg_integer()) -> contents().
vm_dest_to_contents({x_reg, X}, MaxReg) when is_integer(X), X < MaxReg -> {x_reg, X};
vm_dest_to_contents({x_reg, extra}, MaxReg) -> {x_reg, MaxReg};
vm_dest_to_contents({y_reg, Y}, _MaxReg) -> {y_reg, Y};
vm_dest_to_contents(_, _MaxReg) -> unknown.
