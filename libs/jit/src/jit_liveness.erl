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
%% any reachable point before the register is overwritten.
%%
%% The analysis exploits the BEAM conventions that make this precise:
%%   - test_heap/allocate/gc_bif carry a Live count: x[i], i >= Live is
%%     dead at that point (killed);
%%   - a (non-tail) call reads x0..arity-1 and clobbers every x register;
%%   - func_info (clause failure) reads only x0..arity-1.
%%
%% Mid-block branch targets contribute their live-in mask filtered by the
%% kills accumulated *up to the branch* (a later write in the block must
%% not hide a read on the taken path), so successors are recorded with a
%% kill-mask snapshot.
%%
%% Design constraints (runtime JIT runs on MCUs):
%%   - memory: one {Gen, Succs} pair per label during analysis and one
%%     integer mask per label as the result;
%%   - conservatism: any opcode this scanner does not understand poisons
%%     the analysis to all-registers-live. Coverage can grow opcode by
%%     opcode without ever being wrong.
-module(jit_liveness).

-export([label_read_masks/1, first_unknown/1]).

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
    fixpoint(Blocks, maps:map(fun(_L, {Gen, _Succs}) -> Gen end, Blocks)).

%% @doc Debug helper: returns {UnknownOpcodeByte, BytesRemaining} for the
%% first opcode the scanner cannot skip, or 'complete'. Drives coverage.
first_unknown(<<>>) ->
    complete;
first_unknown(<<Op, _/binary>> = Bin) ->
    case op_scan(Bin) of
        unknown -> {Op, byte_size(Bin)};
        {label, _, Rest} -> first_unknown(Rest);
        {plain, _, _, Rest} -> first_unknown(Rest);
        {branch, _, _, _, Rest} -> first_unknown(Rest);
        {terminator, _, _, Rest} -> first_unknown(Rest)
    end.

%% Per-block state while scanning: Gen (read-before-written mask), Kill
%% (written mask), Succs ([{label, L, KillSnapshot} | exit]).
collect_blocks(<<>>, CurLabel, Gen, _Kill, Succs, Acc) ->
    close_block(CurLabel, Gen, Succs, Acc);
collect_blocks(Bin, CurLabel, Gen, Kill, Succs, Acc) ->
    case op_scan(Bin) of
        {label, N, Rest} ->
            %% Current block falls through into label N.
            Acc1 = close_block(CurLabel, Gen, [{label, N, Kill} | Succs], Acc),
            collect_blocks(Rest, N, 0, 0, [], Acc1);
        {plain, Reads, Writes, Rest} ->
            {Gen1, Kill1} = account(Reads, Writes, Gen, Kill),
            collect_blocks(Rest, CurLabel, Gen1, Kill1, Succs, Acc);
        {branch, Reads, Writes, Labels, Rest} ->
            Gen1 = Gen bor (mask_of(Reads) band bnot Kill),
            %% Taken paths see kills up to (but not beyond) this op.
            Succs1 = [{label, L, Kill} || L <- Labels] ++ Succs,
            Kill1 = Kill bor mask_of(Writes),
            collect_blocks(Rest, CurLabel, Gen1, Kill1, Succs1, Acc);
        {terminator, Reads, Labels, Rest} ->
            Gen1 = Gen bor (mask_of(Reads) band bnot Kill),
            Ends = [{label, L, Kill} || L <- Labels] ++ [exit || Labels =:= []],
            Acc1 = close_block(CurLabel, Gen1, Ends ++ Succs, Acc),
            skip_to_label(Rest, Acc1);
        unknown ->
            Acc1 = close_block(CurLabel, Gen bor ?ALL_X, [exit | Succs], Acc),
            poison(Acc1)
    end.

%% After a terminator only a label starts live code again; opcodes in
%% between are unreachable but must still be skipped correctly.
skip_to_label(<<>>, Acc) ->
    Acc;
skip_to_label(Bin, Acc) ->
    case op_scan(Bin) of
        {label, N, Rest} -> collect_blocks(Rest, N, 0, 0, [], Acc);
        {plain, _, _, Rest} -> skip_to_label(Rest, Acc);
        {branch, _, _, _, Rest} -> skip_to_label(Rest, Acc);
        {terminator, _, _, Rest} -> skip_to_label(Rest, Acc);
        unknown -> poison(Acc)
    end.

account(Reads, Writes, Gen, Kill) ->
    {Gen bor (mask_of(Reads) band bnot Kill), Kill bor mask_of(Writes)}.

mask_of(all) ->
    ?ALL_X;
mask_of({ge, Live}) when Live >= 32 -> 0;
mask_of({ge, Live}) ->
    ?ALL_X band bnot ((1 bsl Live) - 1);
mask_of({lt, Arity}) when Arity >= 32 -> ?ALL_X;
mask_of({lt, Arity}) ->
    (1 bsl Arity) - 1;
mask_of(Items) when is_list(Items) ->
    lists:foldl(
        fun
            (N, M) when is_integer(N), N < 32 -> M bor ?X_BIT(N);
            (N, M) when is_integer(N) -> M;
            (Tagged, M) -> M bor mask_of(Tagged)
        end,
        0,
        Items
    ).

close_block(none, _Gen, _Succs, Acc) ->
    Acc;
close_block(Label, Gen, Succs, Acc) ->
    Acc#{Label => {Gen band ?ALL_X, Succs}}.

%% Unknown opcode encountered: every mask becomes ?ALL_X.
poison(Acc) ->
    maps:map(fun(_L, {_G, _S}) -> {?ALL_X, [exit]} end, Acc).

%% in(L) = Gen(L) | union over successors S of (in(S) & ~KillSnapshot(S)).
%% Monotone and bounded (32 bits per label).
fixpoint(Blocks, Masks0) ->
    {Masks1, Changed} = maps:fold(
        fun(L, {Gen, Succs}, {MAcc, Ch}) ->
            NewMask = lists:foldl(
                fun
                    (exit, M) ->
                        M;
                    ({label, S, KillSnap}, M) ->
                        M bor (maps:get(S, MAcc, ?ALL_X) band bnot KillSnap)
                end,
                Gen,
                Succs
            ),
            case maps:get(L, MAcc) of
                NewMask -> {MAcc, Ch};
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
%%   {plain, Reads, Writes, Rest}              straight-line op
%%   {branch, Reads, Writes, Labels, Rest}     conditional exits, falls through
%%   {terminator, Reads, Labels, Rest}         no fallthrough ([] = exit)
%%   unknown
%% Reads is a list of x indexes or {lt, Arity} (x0..Arity-1) or 'all';
%% Writes is a list of x indexes or {ge, Live} (everything from x[Live] up)
%% or 'all'. y registers are ignored (x write-back elision only).
%%-----------------------------------------------------------------------------
%% int_code_end (opcode 3) closes the chunk.
op_scan(<<3, Rest/binary>>) ->
    {terminator, [], [], Rest};
op_scan(<<?OP_LABEL, Rest0/binary>>) ->
    {N, Rest1} = decode_value(Rest0),
    {label, N, Rest1};
op_scan(<<?OP_FUNC_INFO, Rest0/binary>>) ->
    case skip_operands(Rest0, 2) of
        {ok, Rest1} ->
            {Arity, Rest2} = decode_value(Rest1),
            {terminator, {lt, Arity}, [], Rest2};
        unknown ->
            unknown
    end;
op_scan(<<?OP_LINE, Rest0/binary>>) ->
    case skip_operands(Rest0, 1) of
        {ok, Rest1} -> {plain, [], [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_MOVE, Rest0/binary>>) ->
    scan_ops(Rest0, [read, write]);
op_scan(<<?OP_SWAP, Rest0/binary>>) ->
    %% Both operands are read and written.
    case scan_ops(Rest0, [read, read]) of
        {plain, R, _W, Rest1} -> {plain, R, R, Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_GET_HD, Rest0/binary>>) ->
    scan_ops(Rest0, [read, write]);
op_scan(<<?OP_GET_TL, Rest0/binary>>) ->
    scan_ops(Rest0, [read, write]);
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
op_scan(<<?OP_INIT_YREGS, Rest0/binary>>) ->
    case skip_ext_list(Rest0) of
        {_Ops, Rest1} -> {plain, [], [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_BADMATCH, Rest0/binary>>) ->
    case scan_ops(Rest0, [read]) of
        {plain, R, _W, Rest1} -> {terminator, R, [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_IF_END, Rest/binary>>) ->
    {terminator, [], [], Rest};
op_scan(<<?OP_CASE_END, Rest0/binary>>) ->
    case scan_ops(Rest0, [read]) of
        {plain, R, _W, Rest1} -> {terminator, R, [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_RETURN, Rest/binary>>) ->
    {terminator, [0], [], Rest};
op_scan(<<?OP_JUMP, Rest0/binary>>) ->
    {L, Rest1} = decode_value(Rest0),
    {terminator, [], [L], Rest1};
%% Allocation boundaries: x[i], i >= Live is dead here.
op_scan(<<?OP_ALLOCATE, Rest0/binary>>) ->
    {_S, Rest1} = decode_value(Rest0),
    {Live, Rest2} = decode_value(Rest1),
    {plain, [], {ge, Live}, Rest2};
op_scan(<<?OP_ALLOCATE_HEAP, Rest0/binary>>) ->
    {_S, Rest1} = decode_value(Rest0),
    case skip_alloc_list(Rest1) of
        {ok, Rest2} ->
            {Live, Rest3} = decode_value(Rest2),
            {plain, [], {ge, Live}, Rest3};
        unknown ->
            unknown
    end;
op_scan(<<?OP_TEST_HEAP, Rest0/binary>>) ->
    case skip_alloc_list(Rest0) of
        {ok, Rest1} ->
            {Live, Rest2} = decode_value(Rest1),
            {plain, [], {ge, Live}, Rest2};
        unknown ->
            unknown
    end;
%% Guard BIFs preserve registers; gc BIFs kill above Live.
op_scan(<<?OP_BIF0, Rest0/binary>>) ->
    {_Bif, Rest1} = decode_value(Rest0),
    scan_ops(Rest1, [write]);
op_scan(<<?OP_BIF1, Rest0/binary>>) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    {_Bif, Rest2} = decode_value(Rest1),
    case scan_ops(Rest2, [read, write]) of
        {plain, R, W, Rest3} -> branch_or_plain(FailLabel, R, W, Rest3);
        unknown -> unknown
    end;
op_scan(<<?OP_BIF2, Rest0/binary>>) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    {_Bif, Rest2} = decode_value(Rest1),
    case scan_ops(Rest2, [read, read, write]) of
        {plain, R, W, Rest3} -> branch_or_plain(FailLabel, R, W, Rest3);
        unknown -> unknown
    end;
op_scan(<<?OP_GC_BIF1, Rest0/binary>>) ->
    gc_bif(Rest0, 1);
op_scan(<<?OP_GC_BIF2, Rest0/binary>>) ->
    gc_bif(Rest0, 2);
%% Tests: fail label + read operands.
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_IS_LT;
    Op =:= ?OP_IS_GE;
    Op =:= ?OP_IS_EQUAL;
    Op =:= ?OP_IS_NOT_EQUAL;
    Op =:= ?OP_IS_EQ_EXACT;
    Op =:= ?OP_IS_NOT_EQ_EXACT
->
    test_op(Rest0, 2);
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_IS_INTEGER;
    Op =:= ?OP_IS_FLOAT;
    Op =:= ?OP_IS_NUMBER;
    Op =:= ?OP_IS_ATOM;
    Op =:= ?OP_IS_PID;
    Op =:= ?OP_IS_REFERENCE;
    Op =:= ?OP_IS_PORT;
    Op =:= ?OP_IS_NIL;
    Op =:= ?OP_IS_BINARY;
    Op =:= ?OP_IS_LIST;
    Op =:= ?OP_IS_NONEMPTY_LIST;
    Op =:= ?OP_IS_TUPLE;
    Op =:= ?OP_IS_MAP;
    Op =:= ?OP_IS_BOOLEAN;
    Op =:= ?OP_IS_BITSTR;
    Op =:= ?OP_IS_FUNCTION
->
    test_op(Rest0, 1);
op_scan(<<?OP_TEST_ARITY, Rest0/binary>>) ->
    test_op_extra_skips(Rest0, 1, 1);
op_scan(<<?OP_IS_FUNCTION2, Rest0/binary>>) ->
    test_op_extra_skips(Rest0, 1, 1);
op_scan(<<?OP_IS_TAGGED_TUPLE, Rest0/binary>>) ->
    test_op_extra_skips(Rest0, 1, 2);
op_scan(<<?OP_SELECT_VAL, Rest0/binary>>) ->
    select_op(Rest0);
op_scan(<<?OP_SELECT_TUPLE_ARITY, Rest0/binary>>) ->
    select_op(Rest0);
op_scan(<<Op, Rest0/binary>>) when Op =:= ?OP_PUT_MAP_ASSOC; Op =:= ?OP_PUT_MAP_EXACT ->
    {FailLabel, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read, write]) of
        {plain, R, W, Rest2} ->
            {Live, Rest3} = decode_value(Rest2),
            case skip_ext_list(Rest3) of
                {Ops, Rest4} ->
                    branch_or_plain(FailLabel, R ++ reads_of(Ops), [{ge, Live} | W], Rest4);
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_HAS_MAP_FIELDS, Rest0/binary>>) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read]) of
        {plain, R, _W, Rest2} ->
            case skip_ext_list(Rest2) of
                {Ops, Rest3} -> {branch, R ++ reads_of(Ops), [], [FailLabel], Rest3};
                unknown -> unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_PUT_TUPLE, Rest0/binary>>) ->
    {_Size, Rest1} = decode_value(Rest0),
    scan_ops(Rest1, [write]);
op_scan(<<?OP_PUT, Rest0/binary>>) ->
    scan_ops(Rest0, [read]);
op_scan(<<?OP_GET_MAP_ELEMENTS, Rest0/binary>>) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    case skip_operand(Rest1) of
        {Src, Rest2} ->
            case skip_ext_list(Rest2) of
                {Ops, Rest3} ->
                    %% Alternating key (read), dest (write) operands.
                    {Reads, Writes} = kv_reads_writes(Ops, [], []),
                    {branch, reads_of([Src]) ++ Reads, Writes, [FailLabel], Rest3};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_PUT_TUPLE2, Rest0/binary>>) ->
    case skip_operand(Rest0) of
        {Dest, Rest1} ->
            case skip_ext_list(Rest1) of
                {Ops, Rest2} ->
                    {plain, reads_of(Ops), reads_of([Dest]), Rest2};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_MAKE_FUN3, Rest0/binary>>) ->
    {_FunIdx, Rest1} = decode_value(Rest0),
    case skip_operand(Rest1) of
        {Dest, Rest2} ->
            case skip_ext_list(Rest2) of
                {Ops, Rest3} ->
                    {plain, reads_of(Ops), reads_of([Dest]), Rest3};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_BADRECORD, Rest0/binary>>) ->
    case scan_ops(Rest0, [read]) of
        {plain, R, _W, Rest1} -> {terminator, R, [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_RECV_MARKER_BIND, Rest0/binary>>) ->
    scan_ops(Rest0, [read, read]);
op_scan(<<?OP_RECV_MARKER_CLEAR, Rest0/binary>>) ->
    scan_ops(Rest0, [read]);
op_scan(<<?OP_RECV_MARKER_RESERVE, Rest0/binary>>) ->
    scan_ops(Rest0, [write]);
op_scan(<<?OP_RECV_MARKER_USE, Rest0/binary>>) ->
    scan_ops(Rest0, [read]);
op_scan(<<?OP_CALL_FUN2, Rest0/binary>>) ->
    case skip_operands(Rest0, 1) of
        {ok, Rest1} ->
            {Arity, Rest2} = decode_value(Rest1),
            case scan_ops(Rest2, [read]) of
                {plain, R, _W, Rest3} -> {plain, [{lt, Arity} | R], all, Rest3};
                unknown -> unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_UPDATE_RECORD, Rest0/binary>>) ->
    case skip_operands(Rest0, 1) of
        {ok, Rest1} ->
            {_Size, Rest2} = decode_value(Rest1),
            case scan_ops(Rest2, [read, write]) of
                {plain, R, W, Rest3} ->
                    case skip_ext_list(Rest3) of
                        {Ops, Rest4} -> {plain, R ++ reads_of(Ops), W, Rest4};
                        unknown -> unknown
                    end;
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_CATCH, Rest0/binary>>) ->
    %% catch Yreg FailLabel: the handler label is a successor; its block
    %% accounts its own reads (the runtime materializes x0 there).
    case skip_operand(Rest0) of
        {_Y, Rest1} ->
            {L, Rest2} = decode_value(Rest1),
            {branch, [], [], [L], Rest2};
        unknown ->
            unknown
    end;
op_scan(<<?OP_CATCH_END, Rest0/binary>>) ->
    %% catch_end reads/normalizes x0 and clobbers x1/x2 while building
    %% the {'EXIT', _} result.
    case skip_operand(Rest0) of
        {_Y, Rest1} -> {plain, [0], [1, 2], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_MAKE_FUN2, Rest0/binary>>) ->
    %% make_fun2 FunIndex: captures live x registers into the fun; the
    %% frozen count is not in the opcode, so treat as reading all.
    {_FunIdx, Rest1} = decode_value(Rest0),
    {plain, all, [0], Rest1};
%% Float ops: fp registers are not x registers; only FMOVE/FCONV can
%% touch an x register (as source or destination).
op_scan(<<?OP_FCLEARERROR, Rest/binary>>) ->
    {plain, [], [], Rest};
op_scan(<<?OP_FCHECKERROR, Rest0/binary>>) ->
    case skip_operands(Rest0, 1) of
        {ok, Rest1} -> {plain, [], [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_FMOVE, Rest0/binary>>) ->
    scan_ops(Rest0, [read, write]);
op_scan(<<?OP_FCONV, Rest0/binary>>) ->
    scan_ops(Rest0, [read, skip]);
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_FADD; Op =:= ?OP_FSUB; Op =:= ?OP_FMUL; Op =:= ?OP_FDIV
->
    case skip_operands(Rest0, 4) of
        {ok, Rest1} -> {plain, [], [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_FNEGATE, Rest0/binary>>) ->
    case skip_operands(Rest0, 3) of
        {ok, Rest1} -> {plain, [], [], Rest1};
        unknown -> unknown
    end;
%% try/catch and apply.
op_scan(<<?OP_TRY, Rest0/binary>>) ->
    %% try Yreg FailLabel: registers a catch label; the handler entry is a
    %% successor whose live-in is x0..x2 (class/reason/stacktrace are
    %% materialized there by the runtime, everything else is dead).
    case skip_operand(Rest0) of
        {_Y, Rest1} ->
            {L, Rest2} = decode_value(Rest1),
            {branch, [], [], [L], Rest2};
        unknown ->
            unknown
    end;
op_scan(<<?OP_TRY_END, Rest0/binary>>) ->
    scan_ops(Rest0, [skip]);
op_scan(<<?OP_TRY_CASE, Rest0/binary>>) ->
    case skip_operand(Rest0) of
        {_Y, Rest1} -> {plain, [0, 1, 2], [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_TRY_CASE_END, Rest0/binary>>) ->
    case scan_ops(Rest0, [read]) of
        {plain, R, _W, Rest1} -> {terminator, R, [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_RAISE, Rest0/binary>>) ->
    case scan_ops(Rest0, [read, read]) of
        {plain, R, _W, Rest1} -> {terminator, R, [], Rest1};
        unknown -> unknown
    end;
op_scan(<<?OP_APPLY, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    %% apply reads x0..arity-1 plus module/function in x[arity], x[arity+1].
    {plain, {lt, Arity + 2}, all, Rest1};
op_scan(<<?OP_APPLY_LAST, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_N, Rest2} = decode_value(Rest1),
    {terminator, {lt, Arity + 2}, [], Rest2};
op_scan(<<?OP_RAW_RAISE, Rest/binary>>) ->
    %% raw_raise: no operands; reads class/reason/stacktrace from x0..x2.
    {terminator, [0, 1, 2], [], Rest};
op_scan(<<?OP_BUILD_STACKTRACE, Rest/binary>>) ->
    %% No operands: consumes the raw stacktrace in x0, writes x0.
    {plain, [0], [0], Rest};
op_scan(<<?OP_BIF3, Rest0/binary>>) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    {_Bif, Rest2} = decode_value(Rest1),
    case scan_ops(Rest2, [read, read, read, write]) of
        {plain, R, W, Rest3} -> branch_or_plain(FailLabel, R, W, Rest3);
        unknown -> unknown
    end;
%% Bitstring ops (subset sufficient for compiler/stdlib coverage).
op_scan(<<?OP_BS_START_MATCH3, Rest0/binary>>) ->
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read]) of
        {plain, R, _, Rest2} ->
            {Live, Rest3} = decode_value(Rest2),
            case scan_ops(Rest3, [write]) of
                {plain, _, W, Rest4} ->
                    {branch, R, [{ge, Live} | W], [Fail], Rest4};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_BS_START_MATCH4, Rest0/binary>>) ->
    case skip_operand(Rest0) of
        {FailOp, Rest1} ->
            {Live, Rest2} = decode_value(Rest1),
            case scan_ops(Rest2, [read, write]) of
                {plain, R, W, Rest3} ->
                    Labels =
                        case FailOp of
                            {label, L} -> [L];
                            _ -> []
                        end,
                    {branch, R, [{ge, Live} | W], Labels, Rest3};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_BS_INIT_WRITABLE, Rest/binary>>) ->
    {plain, [0], [0], Rest};
op_scan(<<?OP_BS_CREATE_BIN, Rest0/binary>>) ->
    {Fail, Rest1} = decode_value(Rest0),
    case skip_alloc_list(Rest1) of
        {ok, Rest2} ->
            {Live, Rest3} = decode_value(Rest2),
            {_Unit, Rest4} = decode_value(Rest3),
            case scan_ops(Rest4, [write]) of
                {plain, _, W, Rest5} ->
                    case skip_ext_list(Rest5) of
                        {Ops, Rest6} ->
                            %% Segments: type atom, seg, unit, flags, Src,
                            %% Size — any x operand among them is a read.
                            R = reads_of(Ops),
                            case Fail of
                                0 -> {plain, R, [{ge, Live} | W], Rest6};
                                _ -> {branch, R, [{ge, Live} | W], [Fail], Rest6}
                            end;
                        unknown ->
                            unknown
                    end;
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<Op, Rest0/binary>>) when Op =:= ?OP_BS_GET_POSITION; Op =:= ?OP_BS_GET_TAIL ->
    case scan_ops(Rest0, [read, write]) of
        {plain, R, W, Rest1} ->
            {Live, Rest2} = decode_value(Rest1),
            {plain, R, [{ge, Live} | W], Rest2};
        unknown ->
            unknown
    end;
op_scan(<<?OP_BS_SET_POSITION, Rest0/binary>>) ->
    scan_ops(Rest0, [read, read]);
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_BS_GET_INTEGER2; Op =:= ?OP_BS_GET_FLOAT2; Op =:= ?OP_BS_GET_BINARY2
->
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read]) of
        {plain, R1, _, Rest2} ->
            {Live, Rest3} = decode_value(Rest2),
            case scan_ops(Rest3, [read]) of
                {plain, R2, _, Rest4} ->
                    {_Unit, Rest5} = decode_value(Rest4),
                    {_Flags, Rest6} = decode_value(Rest5),
                    case scan_ops(Rest6, [write]) of
                        {plain, _, W, Rest7} ->
                            {branch, R1 ++ R2, [{ge, Live} | W], [Fail], Rest7};
                        unknown ->
                            unknown
                    end;
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_BS_MATCH, Rest0/binary>>) ->
    %% bs_match Fail Ms {commands...}: the command list mixes atoms,
    %% literals and registers; every x operand is conservatively a read
    %% (missing a write only overstates liveness).
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read]) of
        {plain, R, _, Rest2} ->
            case skip_ext_list(Rest2) of
                {Ops, Rest3} -> {branch, R ++ reads_of(Ops), [], [Fail], Rest3};
                unknown -> unknown
            end;
        unknown ->
            unknown
    end;
op_scan(<<Op, Rest0/binary>>) when
    Op =:= ?OP_BS_GET_UTF8;
    Op =:= ?OP_BS_GET_UTF16;
    Op =:= ?OP_BS_GET_UTF32
->
    %% Fail, Src, live/flags (skipped), Dest.
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read, skip, skip, write]) of
        {plain, R, W, Rest2} -> {branch, R, W, [Fail], Rest2};
        unknown -> unknown
    end;
op_scan(<<Op, Rest0/binary>>) when Op =:= ?OP_BS_SKIP_UTF8; Op =:= ?OP_BS_SKIP_UTF16 ->
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read, skip, skip]) of
        {plain, R, _W, Rest2} -> {branch, R, [], [Fail], Rest2};
        unknown -> unknown
    end;
op_scan(<<?OP_BS_SKIP_BITS2, Rest0/binary>>) ->
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read, read]) of
        {plain, R, _W, Rest2} ->
            {_Unit, Rest3} = decode_value(Rest2),
            {_Flags, Rest4} = decode_value(Rest3),
            {branch, R, [], [Fail], Rest4};
        unknown ->
            unknown
    end;
op_scan(<<?OP_BS_MATCH_STRING, Rest0/binary>>) ->
    {Fail, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read]) of
        {plain, R, _W, Rest2} ->
            {_Bits, Rest3} = decode_value(Rest2),
            {_Off, Rest4} = decode_value(Rest3),
            {branch, R, [], [Fail], Rest4};
        unknown ->
            unknown
    end;
op_scan(<<?OP_GC_BIF3, Rest0/binary>>) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    {Live, Rest2} = decode_value(Rest1),
    {_Bif, Rest3} = decode_value(Rest2),
    case scan_ops(Rest3, [read, read, read, write]) of
        {plain, R, W, Rest4} ->
            case FailLabel of
                0 -> {plain, R, [{ge, Live} | W], Rest4};
                _ -> {branch, R, [{ge, Live} | W], [FailLabel], Rest4}
            end;
        unknown ->
            unknown
    end;
op_scan(<<?OP_IS_ANY_NATIVE_RECORD, Rest0/binary>>) ->
    test_op(Rest0, 1);
%% Receive/message ops.
op_scan(<<?OP_SEND, Rest/binary>>) ->
    %% send: like a call of arity 2 (reads x0, x1; clobbers x registers).
    {plain, {lt, 2}, all, Rest};
op_scan(<<?OP_REMOVE_MESSAGE, Rest/binary>>) ->
    {plain, [], [], Rest};
op_scan(<<?OP_TIMEOUT, Rest/binary>>) ->
    {plain, [], [], Rest};
op_scan(<<?OP_LOOP_REC, Rest0/binary>>) ->
    {L, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [write]) of
        {plain, _R, W, Rest2} -> {branch, [], W, [L], Rest2};
        unknown -> unknown
    end;
op_scan(<<?OP_LOOP_REC_END, Rest0/binary>>) ->
    {L, Rest1} = decode_value(Rest0),
    {terminator, [], [L], Rest1};
op_scan(<<?OP_WAIT, Rest0/binary>>) ->
    {L, Rest1} = decode_value(Rest0),
    {terminator, [], [L], Rest1};
op_scan(<<?OP_WAIT_TIMEOUT, Rest0/binary>>) ->
    {L, Rest1} = decode_value(Rest0),
    case scan_ops(Rest1, [read]) of
        {plain, R, _W, Rest2} -> {branch, R, [], [L], Rest2};
        unknown -> unknown
    end;
%% Calls: read x0..arity-1; non-tail calls clobber every x register.
op_scan(<<?OP_CALL, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Label, Rest2} = decode_value(Rest1),
    {plain, {lt, Arity}, all, Rest2};
op_scan(<<?OP_CALL_ONLY, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Label, Rest2} = decode_value(Rest1),
    {terminator, {lt, Arity}, [], Rest2};
op_scan(<<?OP_CALL_LAST, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Label, Rest2} = decode_value(Rest1),
    {_N, Rest3} = decode_value(Rest2),
    {terminator, {lt, Arity}, [], Rest3};
op_scan(<<?OP_CALL_EXT, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Index, Rest2} = decode_value(Rest1),
    {plain, {lt, Arity}, all, Rest2};
op_scan(<<?OP_CALL_EXT_ONLY, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Index, Rest2} = decode_value(Rest1),
    {terminator, {lt, Arity}, [], Rest2};
op_scan(<<?OP_CALL_EXT_LAST, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {_Index, Rest2} = decode_value(Rest1),
    {_N, Rest3} = decode_value(Rest2),
    {terminator, {lt, Arity}, [], Rest3};
op_scan(<<?OP_CALL_FUN, Rest0/binary>>) ->
    {Arity, Rest1} = decode_value(Rest0),
    {plain, {lt, Arity + 1}, all, Rest1};
op_scan(_) ->
    unknown.

test_op(Rest0, NumArgs) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    collect_reads(Rest1, NumArgs, FailLabel, []).

test_op_extra_skips(Rest0, NumArgs, NumSkips) ->
    case test_op(Rest0, NumArgs) of
        {branch, R, W, L, Rest1} ->
            case skip_operands(Rest1, NumSkips) of
                {ok, Rest2} -> {branch, R, W, L, Rest2};
                unknown -> unknown
            end;
        unknown ->
            unknown
    end.

collect_reads(Bin, 0, FailLabel, Reads) ->
    {branch, Reads, [], [FailLabel], Bin};
collect_reads(Bin, N, FailLabel, Reads) ->
    case skip_operand(Bin) of
        {{x, X}, Rest} -> collect_reads(Rest, N - 1, FailLabel, [X | Reads]);
        {_, Rest} -> collect_reads(Rest, N - 1, FailLabel, Reads);
        unknown -> unknown
    end.

branch_or_plain(0, R, W, Rest) ->
    {plain, R, W, Rest};
branch_or_plain(FailLabel, R, W, Rest) ->
    {branch, R, W, [FailLabel], Rest}.

gc_bif(Rest0, NumArgs) ->
    {FailLabel, Rest1} = decode_value(Rest0),
    {Live, Rest2} = decode_value(Rest1),
    {_Bif, Rest3} = decode_value(Rest2),
    Spec = lists:duplicate(NumArgs, read) ++ [write],
    case scan_ops(Rest3, Spec) of
        {plain, R, W, Rest4} ->
            %% x[i], i >= Live is dead at the op; the destination write and
            %% the Live kill both apply on the fallthrough path.
            case FailLabel of
                0 -> {plain, R, [{ge, Live} | W], Rest4};
                _ -> {branch, R, [{ge, Live} | W], [FailLabel], Rest4}
            end;
        unknown ->
            unknown
    end.

select_op(Rest0) ->
    case skip_operand(Rest0) of
        {Src, Rest1} ->
            {DefLabel, Rest2} = decode_value(Rest1),
            case skip_ext_list(Rest2) of
                {Ops, Rest3} ->
                    Labels = [L || {label, L} <- Ops],
                    {terminator, reads_of([Src]), [DefLabel | Labels], Rest3};
                unknown ->
                    unknown
            end;
        unknown ->
            unknown
    end.

%% Generic operand walker per spec list. Writes may contain {ge, _} tags
%% merged in by callers; here only x indexes are produced.
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

reads_of(Operands) ->
    [N || {x, N} <- Operands].

kv_reads_writes([], Reads, Writes) ->
    {Reads, Writes};
kv_reads_writes([K, V | Rest], Reads, Writes) ->
    Reads1 =
        case K of
            {x, N} -> [N | Reads];
            _ -> Reads
        end,
    Writes1 =
        case V of
            {x, M} -> [M | Writes];
            _ -> Writes
        end,
    kv_reads_writes(Rest, Reads1, Writes1);
kv_reads_writes([_], Reads, Writes) ->
    {Reads, Writes}.

skip_operands(Bin, 0) ->
    {ok, Bin};
skip_operands(Bin, N) ->
    case skip_operand(Bin) of
        {_, Rest} -> skip_operands(Rest, N - 1);
        unknown -> unknown
    end.

%% Extended list: Size operands in sequence.
skip_ext_list(<<?COMPACT_EXTENDED_LIST, Rest0/binary>>) ->
    {Size, Rest1} = decode_value(Rest0),
    skip_ext_list0(Rest1, Size, []);
skip_ext_list(_) ->
    unknown.

skip_ext_list0(Bin, 0, Acc) ->
    {lists:reverse(Acc), Bin};
skip_ext_list0(Bin, N, Acc) ->
    case skip_operand(Bin) of
        {Op, Rest} -> skip_ext_list0(Rest, N - 1, [Op | Acc]);
        unknown -> unknown
    end.

%% test_heap/allocate_heap heap-need operand: plain literal or allocation
%% list (Size, then Size x (tag literal, size literal)).
skip_alloc_list(<<?COMPACT_EXTENDED_ALLOCATION_LIST, Rest0/binary>>) ->
    {Size, Rest1} = decode_value(Rest0),
    skip_alloc_list0(Rest1, Size);
skip_alloc_list(Bin) ->
    case skip_operand(Bin) of
        {_, Rest} -> {ok, Rest};
        unknown -> unknown
    end.

skip_alloc_list0(Bin, 0) ->
    {ok, Bin};
skip_alloc_list0(Bin, N) ->
    {_Tag, Rest1} = decode_value(Bin),
    {_Size, Rest2} = decode_value(Rest1),
    skip_alloc_list0(Rest2, N - 1).

%%-----------------------------------------------------------------------------
%% Compact operand skipper. Returns {{x,N} | {y,N} | {label,N} | other, Rest}
%% or unknown.
%%-----------------------------------------------------------------------------
skip_operand(<<7:3, ?COMPACT_LARGE_INTEGER_NBITS:5, Rest0/binary>>) ->
    %% 9+ byte integer: nested length literal, then (length + 9) bytes.
    {Len, Rest1} = decode_value(Rest0),
    case Rest1 of
        <<_Big:((Len + 9) * 8), Rest2/binary>> -> {other, Rest2};
        _ -> unknown
    end;
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
skip_operand(<<?COMPACT_EXTENDED_FP_REGISTER, Rest0/binary>>) ->
    {_V, Rest1} = decode_value(Rest0),
    {other, Rest1};
skip_operand(<<?COMPACT_EXTENDED_LIST, _/binary>>) ->
    unknown;
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
