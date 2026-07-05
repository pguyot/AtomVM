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

-module(jit_liveness_tests).

-include_lib("eunit/include/eunit.hrl").

-define(ALL_X, 16#FFFFFFFF).

%% Hand-encoded compact terms (value < 16): byte = (Value bsl 4) bor Tag,
%% tags: 0 literal, 2 atom, 3 x reg, 4 y reg, 5 label.
-define(LIT(N), ((N bsl 4) bor 0)).
-define(ATOM(N), ((N bsl 4) bor 2)).
-define(X(N), ((N bsl 4) bor 3)).
-define(LABEL_OP(N), ((N bsl 4) bor 5)).
-define(EXT_LIST, 16#17).

%% Opcodes used below.
-define(INT_CALL_END, 3).
-define(LABEL, 1).
-define(CALL, 4).
-define(TEST_HEAP, 16).
-define(RETURN, 19).
-define(IS_NIL, 52).
-define(MOVE, 64).
-define(BADMATCH, 72).
-define(BS_TEST_TAIL2, 121).
-define(BS_SKIP_UTF32, 143).
-define(NIF_START, 179).
-define(EXECUTABLE_LINE, 183).
-define(DEBUG_LINE, 184).
-define(IS_NATIVE_RECORD, 187).
-define(GET_RECORD_ELEMENTS, 188).
-define(PUT_RECORD, 189).
-define(IS_RECORD_ACCESSIBLE, 190).
-define(GET_RECORD_FIELD, 191).

masks(Chunk) ->
    ?assertEqual(complete, jit_liveness:first_unknown(Chunk)),
    jit_liveness:label_read_masks(Chunk).

%%-----------------------------------------------------------------------------
%% Core dataflow behavior
%%-----------------------------------------------------------------------------

basic_move_test() ->
    %% label 1; move x0, x1; return.  in(1) = {x0}.
    Masks = masks(<<?LABEL, ?LIT(1), ?MOVE, ?X(0), ?X(1), ?RETURN, ?INT_CALL_END>>),
    ?assertEqual(2#1, maps:get(1, Masks)).

branch_kill_snapshot_before_write_test() ->
    %% label 1; is_nil x0 -> 2; move x0, x1; return; label 2; badmatch x1.
    %% The branch to 2 is taken BEFORE the x1 write, so in(2)'s x1 read
    %% must propagate: in(1) = {x0, x1}.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?IS_NIL,
        ?LABEL_OP(2),
        ?X(0),
        ?MOVE,
        ?X(0),
        ?X(1),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?BADMATCH,
        ?X(1),
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)),
    ?assertEqual(2#10, maps:get(2, Masks)).

branch_kill_snapshot_after_write_test() ->
    %% label 1; move x0, x1; is_nil x0 -> 2; return; label 2; badmatch x1.
    %% The x1 write happens before the branch, so in(2)'s x1 read is
    %% filtered by the kill snapshot: in(1) = {x0}.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(1),
        ?IS_NIL,
        ?LABEL_OP(2),
        ?X(0),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?BADMATCH,
        ?X(1),
        ?INT_CALL_END
    >>),
    ?assertEqual(2#1, maps:get(1, Masks)).

test_heap_live_kill_test() ->
    %% label 1; test_heap 2, 1; badmatch x1.  Live = 1 kills x1 and up, so
    %% the badmatch read of x1 does not reach the entry; the GC walk of
    %% x0..Live-1 reads x0: in(1) = {x0}.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?TEST_HEAP,
        ?LIT(2),
        ?LIT(1),
        ?BADMATCH,
        ?X(1),
        ?INT_CALL_END
    >>),
    ?assertEqual(2#1, maps:get(1, Masks)).

gc_walk_reads_live_regs_test() ->
    %% label 1; test_heap 2, 3; return.  A GC at the allocation point walks
    %% (dereferences and updates) x0..x2 as roots, so they must all count
    %% as read even though only x0 has a dataflow read (return):
    %% in(1) = {x0, x1, x2}.  Regression test: eliding a store to x1/x2
    %% across this point would make GC walk a stale pointer.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?TEST_HEAP,
        ?LIT(2),
        ?LIT(3),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#111, maps:get(1, Masks)).

gc_bif_walks_live_regs_test() ->
    %% label 1; gc_bif1 fail=0, live=2, bif=0, x1 -> x0; badmatch x3.
    %% The gc_bif's GC walks x0..x1; x3 is killed by {ge, 2}:
    %% in(1) = {x0, x1}.
    GcBif1 = 124,
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        GcBif1,
        ?LABEL_OP(0),
        ?LIT(2),
        ?LIT(0),
        ?X(1),
        ?X(0),
        ?BADMATCH,
        ?X(3),
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

call_clobbers_test() ->
    %% label 1; call 1, 2; return; label 2; return.  The non-tail call reads
    %% x0 (arity 1) and clobbers everything, hiding return's x0 read:
    %% in(1) = {x0} from the call itself.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?CALL,
        ?LIT(1),
        ?LABEL_OP(2),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#1, maps:get(1, Masks)).

unknown_opcode_poisons_test() ->
    %% Opcode 254 does not exist: every mask collapses to ALL_X and
    %% first_unknown reports it.
    Chunk = <<?LABEL, ?LIT(1), 254, ?RETURN, ?INT_CALL_END>>,
    ?assertMatch({254, _}, jit_liveness:first_unknown(Chunk)),
    Masks = jit_liveness:label_read_masks(Chunk),
    ?assertEqual(?ALL_X, maps:get(1, Masks)).

%%-----------------------------------------------------------------------------
%% Scanner coverage: opcodes observed to poison the test-suite corpus
%%-----------------------------------------------------------------------------

nif_start_test() ->
    %% label 1; nif_start; return.  No operands, no reads/writes.
    Masks = masks(<<?LABEL, ?LIT(1), ?NIF_START, ?RETURN, ?INT_CALL_END>>),
    ?assertEqual(2#1, maps:get(1, Masks)).

executable_line_test() ->
    %% label 1; executable_line Location=5, Line=1; return.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?EXECUTABLE_LINE,
        ?LIT(5),
        ?LIT(1),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#1, maps:get(1, Masks)).

debug_line_test() ->
    %% label 1; debug_line Kind=0, Location=0, Index=0, Live=2; badmatch x3.
    %% The debugger may observe x0..x[Live-1] at the stop point, so the op
    %% must read {lt, Live}; x3 is read by badmatch: in(1) = {x0, x1, x3}.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?DEBUG_LINE,
        ?LIT(0),
        ?LIT(0),
        ?LIT(0),
        ?LIT(2),
        ?BADMATCH,
        ?X(3),
        ?INT_CALL_END
    >>),
    ?assertEqual(2#1011, maps:get(1, Masks)).

bs_test_tail2_test() ->
    %% label 1; bs_test_tail2 fail=2, src=x1, bits=8; return; label 2; return.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?BS_TEST_TAIL2,
        ?LABEL_OP(2),
        ?X(1),
        ?LIT(8),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

bs_skip_utf32_test() ->
    %% label 1; bs_skip_utf32 fail=2, src=x2, live=0, flags=0; return;
    %% label 2; return.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?BS_SKIP_UTF32,
        ?LABEL_OP(2),
        ?X(2),
        ?LIT(0),
        ?LIT(0),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#101, maps:get(1, Masks)).

is_native_record_test() ->
    %% label 1; is_native_record fail=2, src=x1, mod=atom1, name=atom2;
    %% return; label 2; return.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?IS_NATIVE_RECORD,
        ?LABEL_OP(2),
        ?X(1),
        ?ATOM(1),
        ?ATOM(2),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

get_record_elements_test() ->
    %% label 1; get_record_elements fail=2, src=x1, [atom3 => x3]; return;
    %% label 2; return.  Field names are reads (none here are x regs),
    %% destinations are writes: in(1) = {x0, x1}, x3 not live.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?GET_RECORD_ELEMENTS,
        ?LABEL_OP(2),
        ?X(1),
        ?EXT_LIST,
        ?LIT(2),
        ?ATOM(3),
        ?X(3),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

put_record_test() ->
    %% label 1; put_record fail=0, id=lit1, src=x1, dest=x2, live=3,
    %% [atom1 => x0]; return.  Reads src x1, pair value x0 and the GC walk
    %% of x0..x2; writes x2; kills x3 and up (Live = 3):
    %% in(1) = {x0, x1, x2}.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?PUT_RECORD,
        ?LABEL_OP(0),
        ?LIT(1),
        ?X(1),
        ?X(2),
        ?LIT(3),
        ?EXT_LIST,
        ?LIT(2),
        ?ATOM(1),
        ?X(0),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#111, maps:get(1, Masks)).

put_record_live_kill_test() ->
    %% label 1; put_record fail=0, id=lit1, src=x0, dest=x1, live=2,
    %% []; badmatch x2.  Live = 2 kills x2 and up, hiding the badmatch
    %% read; the GC walk reads x0..x1: in(1) = {x0, x1}.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?PUT_RECORD,
        ?LABEL_OP(0),
        ?LIT(1),
        ?X(0),
        ?X(1),
        ?LIT(2),
        ?EXT_LIST,
        ?LIT(0),
        ?BADMATCH,
        ?X(2),
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

is_record_accessible_test() ->
    %% label 1; is_record_accessible fail=2, src=x1, scope=atom1; return;
    %% label 2; return.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?IS_RECORD_ACCESSIBLE,
        ?LABEL_OP(2),
        ?X(1),
        ?ATOM(1),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

get_record_field_test() ->
    %% label 1; get_record_field fail=2, src=x1, id=lit1, field=atom1,
    %% dest=x2; return; label 2; return.  in(1) = {x0, x1}, x2 written.
    Masks = masks(<<
        ?LABEL,
        ?LIT(1),
        ?GET_RECORD_FIELD,
        ?LABEL_OP(2),
        ?X(1),
        ?LIT(1),
        ?ATOM(1),
        ?X(2),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(2#11, maps:get(1, Masks)).

%%-----------------------------------------------------------------------------
%% Dead-move analysis (analysis/1 third element): byte offsets of OP_MOVE
%% ops whose x destination is provably never observed (no dataflow read, no
%% GC root walk) before being overwritten.
%%-----------------------------------------------------------------------------

dead_moves(Chunk) ->
    ?assertEqual(complete, jit_liveness:first_unknown(Chunk)),
    {_Masks, _CallTargets, DeadMoves} = jit_liveness:analysis(Chunk),
    DeadMoves.

dead_move_simple_test() ->
    %% label 1; move x0, x3; test_heap 2, 1; return.  x3 is above Live = 1
    %% (not walked, killed) and never read: the move at offset 2 is dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?TEST_HEAP,
        ?LIT(2),
        ?LIT(1),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(#{2 => true}, Dead).

move_read_not_dead_test() ->
    %% label 1; move x0, x3; badmatch x3.  x3 is read: not dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?BADMATCH,
        ?X(3),
        ?INT_CALL_END
    >>),
    ?assertEqual(#{}, Dead).

move_gc_walked_not_dead_test() ->
    %% label 1; move x0, x3; test_heap 2, 4; return.  x3 < Live = 4: the GC
    %% walks it as a root, so the store must happen: not dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?TEST_HEAP,
        ?LIT(2),
        ?LIT(4),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(#{}, Dead).

move_live_via_branch_not_dead_test() ->
    %% label 1; move x0, x3; is_nil x0 -> 2; return; label 2; badmatch x3.
    %% The taken path reads x3: not dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?IS_NIL,
        ?LABEL_OP(2),
        ?X(0),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?BADMATCH,
        ?X(3),
        ?INT_CALL_END
    >>),
    ?assertEqual(#{}, Dead).

move_dead_across_branch_test() ->
    %% label 1; move x0, x3; is_nil x0 -> 2; return; label 2; return.
    %% Neither path observes x3: dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?IS_NIL,
        ?LABEL_OP(2),
        ?X(0),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(#{2 => true}, Dead).

move_dead_across_fallthrough_label_test() ->
    %% label 1; move x0, x3; label 2; return.  in(2) = {x0}: dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(#{2 => true}, Dead).

move_overwritten_same_block_test() ->
    %% label 1; move x0, x3; move x1, x3; badmatch x3.  The first move is
    %% superseded before any observation: only it is dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        ?X(3),
        ?MOVE,
        ?X(1),
        ?X(3),
        ?BADMATCH,
        ?X(3),
        ?INT_CALL_END
    >>),
    ?assertEqual(#{2 => true}, Dead).

move_to_y_reg_ignored_test() ->
    %% label 1; move x0, y0; return.  y regs are out of scope: never dead.
    YReg0 = (0 bsl 4) bor 4,
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(0),
        YReg0,
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(#{}, Dead).

move_call_clobber_dead_test() ->
    %% label 1; move x1, x3; call 1, 2; return; label 2; return.  The call
    %% reads x0 only and clobbers everything: the move is dead.
    Dead = dead_moves(<<
        ?LABEL,
        ?LIT(1),
        ?MOVE,
        ?X(1),
        ?X(3),
        ?CALL,
        ?LIT(1),
        ?LABEL_OP(2),
        ?RETURN,
        ?LABEL,
        ?LIT(2),
        ?RETURN,
        ?INT_CALL_END
    >>),
    ?assertEqual(#{2 => true}, Dead).

poisoned_chunk_no_dead_moves_test() ->
    %% Unknown opcode: analysis is poisoned, no move may be skipped.
    Chunk = <<?LABEL, ?LIT(1), ?MOVE, ?X(0), ?X(3), 254, ?RETURN, ?INT_CALL_END>>,
    {_Masks, _CT, Dead} = jit_liveness:analysis(Chunk),
    ?assertEqual(#{}, Dead).
