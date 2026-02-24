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

%% Register set bitmap operations for JIT backends
%%
%% This header provides efficient register set operations using integer bitmaps.
%% Each bit position corresponds to a CPU register. The bit ordering defines
%% allocation priority: bit 0 = highest priority (first to be allocated).
%%
%% Prerequisites for including modules:
%%   - Define ?ALL_REGS as the bitmap of all allocatable registers
%%   - Define ?ALL_PARAM_REGS as the bitmap of all parameter registers
%%   - Define ?ALL_SCRATCH_REGS as the bitmap of all scratch registers
%%   - Define bit_to_reg/1 mapping single-bit values to register atoms
%%   - Define reg_to_bit/1 mapping register atoms to single-bit values
%%
%% Include this file AFTER the above definitions.

-ifndef(JIT_REGS_HRL).
-define(JIT_REGS_HRL, true).

%%=============================================================================
%% Core bitmap operations
%%=============================================================================

%% Empty register set
-define(REGS_EMPTY, 0).

%% Check if register set is empty
-define(REGS_IS_EMPTY(Set), ((Set) =:= 0)).

%% Isolate lowest set bit (highest priority available register)
-define(REGS_FIRST_BIT(Set), ((Set) band (-(Set)))).

%% Remove lowest set bit
-define(REGS_REMOVE_FIRST(Set), ((Set) band ((Set) - 1))).

%% Isolate second lowest set bit
-define(REGS_SECOND_BIT(Set), ?REGS_FIRST_BIT(?REGS_REMOVE_FIRST(Set))).

%% Check if a register bit is set
-define(REGS_HAS(Set, Bit), ((Set) band (Bit) =/= 0)).

%% Add a register bit to a set
-define(REGS_ADD(Set, Bit), ((Set) bor (Bit))).

%% Remove a register bit from a set
-define(REGS_DEL(Set, Bit), ((Set) band (bnot (Bit)))).

%% Set union: A ∪ B
-define(REGS_UNION(A, B), ((A) bor (B))).

%% Set difference: A \ B
-define(REGS_DIFF(A, B), ((A) band (bnot (B)))).

%% Set intersection: A ∩ B
-define(REGS_INTERSECT(A, B), ((A) band (B))).

%%=============================================================================
%% High-level register operations (use bit_to_reg/1 defined by backend)
%%=============================================================================

%% Peek at first available register (returns atom)
-define(REGS_PEEK(Bitmap), bit_to_reg(?REGS_FIRST_BIT(Bitmap))).

%%=============================================================================
%% Data flow analysis - register value tracking
%%=============================================================================

%% Type for tracking what value each CPU register currently holds.
%% Used at compile time (during code generation) to enable optimizations.
%%
%% Tracked values:
%%   {vm_reg, VmReg}           - holds value from a VM register
%%   {immediate, Value}        - holds a known immediate integer
%%   {ptr_offset, Base, Off}   - holds Base + Offset (computed address)
%%   unknown                   - value is unknown or complex
%%
-type reg_value() ::
    {vm_reg,
        {x_reg, non_neg_integer()}
        | {x_reg, extra}
        | {y_reg, non_neg_integer()}
        | {fp_reg, non_neg_integer()}}
    | {immediate, integer()}
    | {ptr_offset, atom(), non_neg_integer()}
    | unknown.

-type reg_values() :: #{atom() => reg_value()}.

%% Initialize empty data flow tracking
-define(DFA_INIT, #{}).

%% Track a register value
-define(DFA_SET(Map, Reg, Value), maps:put(Reg, Value, Map)).

%% Query a register value (returns unknown if not tracked)
-define(DFA_GET(Map, Reg), maps:get(Reg, Map, unknown)).

%% Remove tracking for a register (when freed)
-define(DFA_REMOVE(Map, Reg), maps:remove(Reg, Map)).

%% Invalidate all tracked values (e.g. after a function call)
-define(DFA_INVALIDATE_ALL, #{}).

%% Invalidate a specific register
-define(DFA_INVALIDATE(Map, Reg), maps:put(Reg, unknown, Map)).

%%=============================================================================
%% Helper functions (included into each backend module)
%%=============================================================================

%% @doc Convert a register bitmap to a list of register atoms in priority order
%% (lowest bit first = highest allocation priority).
%% Requires bit_to_reg/1 to be defined in the including module.
-spec regs_to_list(non_neg_integer()) -> [atom()].
regs_to_list(0) -> [];
regs_to_list(Bitmap) ->
    Bit = Bitmap band (-Bitmap),
    [bit_to_reg(Bit) | regs_to_list(Bitmap bxor Bit)].

%% @doc Convert a list of register atoms to a bitmap.
%% Requires reg_to_bit/1 to be defined in the including module.
-spec list_to_regs([atom()]) -> non_neg_integer().
list_to_regs([]) -> 0;
list_to_regs([Reg | T]) -> reg_to_bit(Reg) bor list_to_regs(T).

%% @doc Peek at first two available registers from bitmap (returns {Reg1, Reg2}).
-spec regs_peek2(non_neg_integer()) -> {atom(), atom()}.
regs_peek2(Bitmap) ->
    Bit1 = Bitmap band (-Bitmap),
    Rest = Bitmap bxor Bit1,
    Bit2 = Rest band (-Rest),
    {bit_to_reg(Bit1), bit_to_reg(Bit2)}.

%% @doc Merge used registers from another code path into the current state.
%% Ensures registers used in either path are marked as used.
-spec regs_merge_used(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
regs_merge_used(AvailRegs, UsedRegs, OtherUsed) ->
    NewUsed = UsedRegs bor OtherUsed,
    NewAvail = AvailRegs band (bnot OtherUsed),
    {NewAvail, NewUsed}.

%% @doc Free a register: remove from used set, add to available set.
%% Returns {NewAvail, NewUsed}.
-spec regs_free(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
    {non_neg_integer(), non_neg_integer()}.
regs_free(AvailRegs, UsedRegs, RegBit) ->
    {AvailRegs bor RegBit, UsedRegs band (bnot RegBit)}.

%% @doc Invalidate DFA entries for registers in a bitmap.
-spec dfa_invalidate_regs(reg_values(), non_neg_integer()) -> reg_values().
dfa_invalidate_regs(DFA, 0) -> DFA;
dfa_invalidate_regs(DFA, Bitmap) ->
    Bit = Bitmap band (-Bitmap),
    Reg = bit_to_reg(Bit),
    dfa_invalidate_regs(maps:remove(Reg, DFA), Bitmap bxor Bit).

%% @doc Check if a register currently holds a specific value.
%% Returns true if the register is tracked and holds exactly the given value.
-spec dfa_check(reg_values(), atom(), reg_value()) -> boolean().
dfa_check(DFA, Reg, ExpectedValue) ->
    case maps:find(Reg, DFA) of
        {ok, ExpectedValue} -> true;
        _ -> false
    end.

%% @doc Find a register that currently holds a specific value.
%% Returns {ok, RegAtom} if found, or 'none' if no register holds this value.
%% Only searches among registers in the given bitmap (typically used_regs).
-spec dfa_find_reg(reg_values(), non_neg_integer(), reg_value()) -> {ok, atom()} | none.
dfa_find_reg(_DFA, 0, _Value) -> none;
dfa_find_reg(DFA, Bitmap, Value) ->
    Bit = Bitmap band (-Bitmap),
    Reg = bit_to_reg(Bit),
    case maps:find(Reg, DFA) of
        {ok, Value} -> {ok, Reg};
        _ -> dfa_find_reg(DFA, Bitmap bxor Bit, Value)
    end.

%% @doc Merge DFA states from two code paths (e.g., if/else branches).
%% A register value is kept only if both paths agree on it.
-spec dfa_merge(reg_values(), reg_values()) -> reg_values().
dfa_merge(DFA1, DFA2) ->
    maps:fold(
        fun(Reg, Value, Acc) ->
            case maps:find(Reg, DFA2) of
                {ok, Value} -> maps:put(Reg, Value, Acc);
                _ -> Acc
            end
        end,
        #{},
        DFA1
    ).

-endif.
