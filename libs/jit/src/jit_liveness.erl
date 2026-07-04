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

%% X-register liveness pre-analysis for the JIT (pass A).
%%
%% Walks a BEAM code chunk with a skip-only decoder and computes, for every
%% label, the set of x registers that may be READ before being written when
%% execution enters at that label ("live-in" mask, bit N = x[N]). The
%% emitter can then elide a write-back to ctx->x[N] when N is not live at
%% any reachable point before the register is overwritten and N >= Live at
%% the closing allocation boundary.
%%
%% Design constraints (runtime JIT runs on MCUs):
%%   - memory: one {Gen, Kill, Succs} triple per label during analysis and
%%     one integer mask per label as the result;
%%   - conservatism: any opcode this scanner does not understand makes the
%%     current block read EVERYTHING (mask ?ALL_X). Coverage can grow
%%     opcode by opcode without ever being wrong.
-module(jit_liveness).

-export([label_read_masks/1]).

-include("opcodes.hrl").
-include("compact_term.hrl").

-define(ALL_X, 16#FFFFFFFF).
-define(X_BIT(N), (1 bsl (N))).

%%-----------------------------------------------------------------------------
%% @doc Compute the per-label live-in x-register masks for a code chunk.
%% Takes the code binary as consumed by jit:first_pass (after the header).
%% Returns a map #{Label => Mask}. Absent labels must be treated as ?ALL_X.
%% @end
%%-----------------------------------------------------------------------------
-spec label_read_masks(binary()) -> #{non_neg_integer() => non_neg_integer()}.
label_read_masks(Code) ->
    Blocks = collect_blocks(Code, none, 0, 0, [], #{}),
    fixpoint(Blocks, init_masks(Blocks)).

%% Walk the whole chunk, accumulating per-label blocks. A block records
%% Gen (x regs read before written), Kill (x regs written), and its
%% successors: a list of labels plus 'fallthrough' (the next label in
%% program order) and/or 'exit' (return/badmatch/tail call: no x reads
%% beyond those already accounted).
collect_blocks(<<>>, CurLabel, Gen, Kill, Succs, Acc) ->
    close_block(CurLabel, Gen, Kill, Succs, [exit], Acc);
collect_blocks(Bin, CurLabel, Gen, Kill, Succs, Acc) ->
    case op_scan(Bin) of
        {label, N, Rest} ->
            %% Close the current block: falls through into label N.
            Acc1 = close_block(CurLabel, Gen, Kill, Succs, [{label, N}], Acc),
            collect_blocks(Rest, N, 0, 0, [], Acc1);
        {plain, Reads, Writes, Rest} ->
            {Gen1, Kill1} = account(Reads, Writes, Gen, Kill),
            collect_blocks(Rest, CurLabel, Gen1, Kill1, Succs, Acc);
        {branch, Reads, Labels, Rest} ->
            {Gen1, Kill1} = account(Reads, [], Gen, Kill),
            Succs1 = [{label, L} || L <- Labels] ++ Succs,
            collect_blocks(Rest, CurLabel, Gen1, Kill1, Succs1, Acc);
        {terminator, Reads, Labels, Rest} ->
            {Gen1, Kill1} = account(Reads, [], Gen, Kill),
            Ends = [{label, L} || L <- Labels] ++ [exit || Labels =:= []],
            Acc1 = close_block(CurLabel, Gen1, Kill1, Succs, Ends, Acc),
            skip_to_label(Rest, Acc1);
        unknown ->
            %% Unknown opcode: poison the current block and every later
            %% one (we can no longer find opcode boundaries).
            Acc1 = close_block(CurLabel, Gen bor ?ALL_X, Kill, Succs, [exit], Acc),
            poison(Acc1)
    end.

%% After a terminator, only a label can start meaningful code again; scan
%% forward for it (opcodes between are unreachable but must still be
%% skipped correctly, so reuse op_scan and ignore effects).
skip_to_label(<<>>, Acc) ->
    Acc;
skip_to_label(Bin, Acc) ->
    case op_scan(Bin) of
        {label, N, Rest} ->
            collect_blocks(Rest, N, 0, 0, [], Acc);
        {plain, _, _, Rest} ->
            skip_to_label(Rest, Acc);
        {branch, _, _, Rest} ->
            skip_to_label(Rest, Acc);
        {terminator, _, _, Rest} ->
            skip_to_label(Rest, Acc);
        unknown ->
            poison(Acc)
    end.

account(Reads, Writes, Gen, Kill) ->
    RMask = lists:foldl(fun(N, M) -> M bor ?X_BIT(N) end, 0, Reads),
    WMask = lists:foldl(fun(N, M) -> M bor ?X_BIT(N) end, 0, Writes),
    %% Reads count only where not already written in this block.
    {Gen bor (RMask band (bnot Kill)), Kill bor WMask}.

close_block(none, _Gen, _Kill, _Succs, _Ends, Acc) ->
    Acc;
close_block(Label, Gen, Kill, Succs, Ends, Acc) ->
    Acc#{Label => {Gen, Kill, Ends ++ Succs}}.

%% Unknown opcode encountered: every mask becomes ?ALL_X.
poison(Acc) ->
    maps:map(fun(_L, {_G, _K, _S}) -> {?ALL_X, 0, [exit]} end, Acc).

init_masks(Blocks) ->
    maps:map(fun(_L, {Gen, _Kill, _Succs}) -> Gen end, Blocks).

%% Iterate live-in masks to a fixpoint: in(L) = Gen(L) | (union of in(S)
%% for successors S) & ~Kill(L). Monotone and bounded (32 bits per label).
fixpoint(Blocks, Masks0) ->
    {Masks1, Changed} = maps:fold(
        fun(L, {Gen, Kill, Succs}, {MAcc, Ch}) ->
            SuccMask = lists:foldl(
                fun
                    (exit, M) ->
                        M;
                    ({label, S}, M) ->
                        M bor maps:get(S, MAcc, ?ALL_X)
                end,
                0,
                Succs
            ),
            NewMask = Gen bor (SuccMask band (bnot Kill)),
            Old = maps:get(L, MAcc),
            case NewMask of
                Old -> {MAcc, Ch};
                _ -> {MAcc#{L => NewMask}, true}
            end
        end,
        {Masks0, false},
        Blocks
    ),
    case Changed of
        true -> fixpoint(Blocks, Masks1);
        false -> Masks1
    end.

%%-----------------------------------------------------------------------------
%% One-opcode scanner. Returns:
%%   {label, N, Rest}
%%   {plain, Reads, Writes, Rest}          straight-line op
%%   {branch, Reads, Labels, Rest}         conditional exits, falls through
%%   {terminator, Reads, Labels, Rest}     no fallthrough ([] = exit)
%%   unknown
%% Reads/Writes are x-register indexes; y registers are ignored (they are
%% not affected by x write-back elision).
%%-----------------------------------------------------------------------------
op_scan(<<?OP_LABEL, Rest0/binary>>) ->
    {N, Rest1} = decode_value(Rest0),
    {label, N, Rest1};
op_scan(<<?OP_LINE, Rest0/binary>>) ->
    case skip_operand(Rest0) of
        {_, Rest1} -> {plain, [], [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_MOVE, Rest0/binary>>) ->
    rw1w1(Rest0);
op_scan(<<?OP_GET_HD, Rest0/binary>>) ->
    rw1w1(Rest0);
op_scan(<<?OP_GET_TL, Rest0/binary>>) ->
    rw1w1(Rest0);
op_scan(<<?OP_GET_LIST, Rest0/binary>>) ->
    scan_ops(Rest0, [read, write, write]);
op_scan(<<?OP_GET_TUPLE_ELEMENT, Rest0/binary>>) ->
    scan_ops(Rest0, [read, skip, write]);
op_scan(<<?OP_SET_TUPLE_ELEMENT, Rest0/binary>>) ->
    scan_ops(Rest0, [read, read, skip]);
op_scan(<<?OP_PUT_LIST, Rest0/binary>>) ->
    scan_ops(Rest0, [read, read, write]);
op_scan(<<?OP_DEALLOCATE, Rest0/binary>>) ->
    scan_ops(Rest0, [skip]);
op_scan(<<?OP_KILL, Rest0/binary>>) ->
    scan_ops(Rest0, [skip]);
op_scan(<<?OP_TRIM, Rest0/binary>>) ->
    scan_ops(Rest0, [skip, skip]);
op_scan(<<?OP_BADMATCH, Rest0/binary>>) ->
    case scan_ops(Rest0, [read]) of
        {plain, R, W, Rest1} -> {terminator, R ++ W, [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_RETURN, Rest/binary>>) ->
    {terminator, [0], [], Rest};
op_scan(<<?OP_JUMP, Rest0/binary>>) ->
    {L, Rest1} = decode_value(Rest0),
    {terminator, [], [L], Rest1};
%% Tests: fail label + read operands.
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_IS_LT;
    Op =:= ?OP_IS_GE;
    Op =:= ?OP_IS_EQ_EXACT;
    Op =:= ?OP_IS_NOT_EQ_EXACT
->
    branch_reads(Rest0, 2);
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_IS_INTEGER;
    Op =:= ?OP_IS_ATOM;
    Op =:= ?OP_IS_NIL;
    Op =:= ?OP_IS_NONEMPTY_LIST;
    Op =:= ?OP_IS_TUPLE
->
    branch_reads(Rest0, 1);
op_scan(<<?OP_TEST_ARITY, Rest0/binary>>) ->
    case branch_reads(Rest0, 1) of
        {branch, R, L, Rest1} ->
            case skip_operand(Rest1) of
                {_, Rest2} -> {branch, R, L, Rest2};
                unknown -> unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_SELECT_VAL, Rest0/binary>>) ->
    case skip_operand(Rest0) of
        {Src, Rest1} ->
            {DefLabel, Rest2} = decode_value(Rest1),
            case skip_ext_list_labels(Rest2) of
                {Labels, Rest3} ->
                    {terminator, reads_of([Src]), [DefLabel | Labels], Rest3};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
%% Calls: read x0..arity-1; a call is also a block end for elision purposes
%% (everything not live is dead per the convention), modeled as branch to
%% nothing plus the reads. Non-tail calls fall through.
op_scan(<<Op, Rest0/binary>>) when Op =:= ?OP_CALL; Op =:= ?OP_CALL_ONLY ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Label, Rest2} = decode_value(Rest1),
    Reads = lists:seq(0, Arity - 1),
    case Op of
        ?OP_CALL -> {plain, Reads, [], Rest2};
        ?OP_CALL_ONLY -> {terminator, Reads, [], Rest2}
    end;
op_scan(<<?OP_CALL_LAST, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Label, Rest2} = decode_value(Rest1),
    {_N, Rest3} = decode_value(Rest2),
    {terminator, lists:seq(0, Arity - 1), [], Rest3};
op_scan(<<?OP_CALL_EXT, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Index, Rest2} = decode_value(Rest1),
    {plain, lists:seq(0, Arity - 1), [], Rest2};
op_scan(<<Op, Rest0/binary>>) when Op =:= ?OP_CALL_EXT_ONLY ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Index, Rest2} = decode_value(Rest1),
    {terminator, lists:seq(0, Arity - 1), [], Rest2};
op_scan(<<?OP_CALL_EXT_LAST, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Index, Rest2} = decode_value(Rest1),
    {_N, Rest3} = decode_value(Rest2),
    {terminator, lists:seq(0, Arity - 1), [], Rest3};
op_scan(<<?OP_CALL_FUN, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {plain, lists:seq(0, Arity), [], Rest1};
op_scan(_) ->
    unknown.

%% src (read) then dest (write)
rw1w1(Rest0) ->
    scan_ops(Rest0, [read, write]).

%% Generic operand walker per spec list.
scan_ops(Bin, Spec) ->
    scan_ops(Bin, Spec, [], []).

scan_ops(Bin, [], Reads, Writes) ->
    {plain, Reads, Writes, Bin};
scan_ops(Bin, [Kind | Spec], Reads, Writes) ->
    case skip_operand(Bin) of
        {Operand, Rest} ->
            case {Kind, Operand} of
                {read, {x, N}} -> scan_ops(Rest, Spec, [N | Reads], Writes);
                {write, {x, N}} -> scan_ops(Rest, Spec, Reads, [N | Writes]);
                {_, _} -> scan_ops(Rest, Spec, Reads, Writes)
            end;
        unknown ->
            unknown
    end.

branch_reads(Rest0, NumArgs) ->
    {Label, Rest1} = decode_value(Rest0),
    branch_reads0(Rest1, NumArgs, Label, []).

branch_reads0(Bin, 0, Label, Reads) ->
    {branch, Reads, [Label], Bin};
branch_reads0(Bin, N, Label, Reads) ->
    case skip_operand(Bin) of
        {{x, X}, Rest} -> branch_reads0(Rest, N - 1, Label, [X | Reads]);
        {_, Rest} -> branch_reads0(Rest, N - 1, Label, Reads);
        unknown -> unknown
    end.

reads_of(Operands) ->
    [N || {x, N} <- Operands].

%% select_val extended list: Size operands alternating value/label.
skip_ext_list_labels(<<?COMPACT_EXTENDED_LIST, Rest0/binary>>) ->
    {Size, Rest1} = decode_value(Rest0),
    skip_ext_list_labels0(Rest1, Size, []).

skip_ext_list_labels0(Bin, 0, Labels) ->
    {Labels, Bin};
skip_ext_list_labels0(Bin, N, Labels) ->
    case skip_operand(Bin) of
        {{label, L}, Rest} -> skip_ext_list_labels0(Rest, N - 1, [L | Labels]);
        {_, Rest} -> skip_ext_list_labels0(Rest, N - 1, Labels);
        unknown -> unknown
    end.

%%-----------------------------------------------------------------------------
%% Compact operand skipper. Returns {{x,N} | {y,N} | {label,N} | other, Rest}
%% or unknown.
%%-----------------------------------------------------------------------------
skip_operand(<<7:3, ?COMPACT_LARGE_INTEGER_NBITS:5, _/binary>>) ->
    %% 9+ byte integers carry a nested length; bail.
    unknown;
skip_operand(<<?COMPACT_EXTENDED_LITERAL, Rest0/binary>>) ->
    {_V, Rest1} = decode_value(Rest0),
    {other, Rest1};
skip_operand(<<?COMPACT_EXTENDED_TYPED_REGISTER, Rest0/binary>>) ->
    case skip_operand(Rest0) of
        {Reg, Rest1} ->
            {_Type, Rest2} = decode_value(Rest1),
            {Reg, Rest2};
        unknown ->
            unknown
    end;
skip_operand(<<?COMPACT_EXTENDED_LIST, _/binary>>) ->
    unknown;
skip_operand(<<?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>) ->
    {_V, Rest1} = decode_value(Rest0),
    {other, Rest1};
skip_operand(<<?COMPACT_EXTENDED_ALLOCATION_LIST, _/binary>>) ->
    unknown;
skip_operand(<<_:5, Tag:3, _/binary>> = Bin) when Tag =/= 7 ->
    {Value, Rest} = decode_value(Bin),
    Classified =
        case Tag of
            ?COMPACT_XREG -> {x, Value};
            ?COMPACT_YREG -> {y, Value};
            ?COMPACT_LABEL -> {label, Value};
            _ -> other
        end,
    {Classified, Rest};
skip_operand(_) ->
    unknown.

%% decode_value64 clone (pure).
decode_value(<<Val:4, 0:1, _:3, Rest/binary>>) -> {Val, Rest};
decode_value(<<Val:3, 1:2, _:3, NextByte, Rest/binary>>) -> {(Val bsl 8) bor NextByte, Rest};
decode_value(<<Size0:3, 3:2, _:3, Value:(8 * (Size0 + 2)), Rest/binary>>) -> {Value, Rest}.
