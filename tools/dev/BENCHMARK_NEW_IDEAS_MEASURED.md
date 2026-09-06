<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Eight new benchmark experiments — measured — 2026-09-06

Companion to [BENCHMARK_NEW_IDEAS_2026-09-06.md](BENCHMARK_NEW_IDEAS_2026-09-06.md),
which proposed the eight experiments without changing VM or JIT source. This
records what each one measured on aarch64 and what shipped.

Baseline for every measurement is `c669cb66f`. The repair the proposal asked
for first was written and measured, turned out to be worth nothing, and was
dropped — see section 0, whose method consequence applies to everything else.
Release/SMP/JIT, OTP 29 reference, Mac mini M4. Drivers: [`bench_ab_atomvm.py`](bench_ab_atomvm.py) and the
publication harness.

## Verdicts

| # | Idea | Verdict | Measured |
|---|---|---|---|
| 0 | Message-regression repair (prerequisite) | **dropped** | the "regression" was mostly build layout; repair measures 0 |
| 1 | Receive markers actually skip old messages | assessed, not implemented | premise confirmed; largest gap in the doc |
| 2 | Lazy B-tree map iterator | **shipped** `15a6a6501` | up to **138x**; take-eight 116x |
| 3 | Forward floating values between FP instructions | **drop** | whole FP gap is 8%; ceiling too small |
| 4 | Private binary append bypasses allocation prep | assessed, not implemented | premise confirmed; ~11% of one 0.9% test |
| 5 | Effect contracts for small NIFs | **drop** | real-workload boundary is 38/1222 samples |
| 6 | B-tree node layout instead of a new B-tree | assessed, best next bet | `node_find` is 41% of a real compile |
| 7 | Reciprocal bigint quotient estimation | **shipped** `f6b2906a6` | **1.090x** bigint; `__udivmodti4` gone |
| 8 | Signed power-of-two division | **drop** (author screened) | 1.004x, accepted without re-running |

Two shipped, four dropped with data (including the prerequisite repair), two
assessed and left with a sized recommendation.

## 0. The prerequisite repair — dropped, and why it matters for the rest

The proposal was right that the message regression should be settled first. It
was, and the answer was not the expected one.

Comparing separate builds attributes their code-layout differences to whatever
source change separates them, and ESTONE's message micros are unusually
sensitive to layout. Four builds containing identical message-path source ran
772, 792, 918 and 926 ms — a 154 ms spread. Disassembling
`mailbox_message_create_from_term` in two of them showed identical instruction
sequences differing only in call-target addresses.

Rebuilt in place from one tree as one-line variants, 27 rotating rounds:

| ESTONE component | hint disabled | unconditional stores | conditional stores |
|---|---:|---:|---:|
| medium messages | 249 ms | 290 ms | 290 ms |
| small messages | 315 ms | 299 ms | 301 ms |
| **total measured time** | **768 ms** | **791 ms** | **792 ms** |

Making the hint stores conditional — the repair — is worth 791 -> 792 ms, i.e.
nothing, so it was dropped instead of committed. The cache-line reasoning behind
it is factually right (`sizeof(Mailbox)` 64, `outer_first` at offset 0, hint
fields at 48/56/60, 128-byte line) and still did not produce a measurable
effect.

What is real: the size hint itself costs about 24 ms (3%) on this benchmark,
winning 16 ms on small messages and losing 41 ms on medium ones. That coexists
with the 1.076x/1.154x the dedicated round-trip probes measured, because
ESTONE's `msgp` rebuilds a four-process ring every iteration and gives each
mailbox 100 messages, while those probes use one long-lived mailbox and one
shape. Keeping or reverting the hint is a workload judgement, left open.

**Method consequence for everything below:** a message-micro difference is
attributed to a source change only from a controlled build pair — same tree,
minimal diff, rebuilt in place. The two shipped changes below were re-checked
that way; the bigint one repeated at 1.088x against 1.090x from the
cross-build measurement, and the map cursor's effect is orders of magnitude
above the layout band.

## 2. Lazy map iterator — shipped

Confirmed exactly as described: `nif_maps_next` reserved `4 * size + 8` words
and called `termtree_to_kv_list` on the first step — 320KB to read one key from
a 10,000-entry map. Replaced with a cursor of `{Node, Index}` frames, deepest
first, allocating at most one frame per tree level per step.

1,000 fresh iterators, 11 interleaved rounds (BEAM for scale):

| probe | before | after | speedup | vs BEAM after |
|---|---:|---:|---:|---:|
| 10,000-entry, take one | 7,454us | 54us | **138x** | 5.78x |
| 10,000-entry, take eight | 17,451us | 150us | **116x** | 1.95x |
| 1,000-entry, take one | 801us | 61us | 13.1x | 3.62x |
| 100-entry, take one | 49us | 48us | 1.02x | 5.27x |
| 1,000-entry, consume all | 164us | 152us | 1.08x | 1.09x |
| 10,000-entry, consume all | 1,339us | 1,303us | 1.03x | 0.58x |

The gate held: full traversals are not regressed, because the cursor allocates
per step roughly what the flat list allocated up front. Benchmark app 0.995x
(CI 0.976-1.018, 31 rounds); batch `erlc` over stdlib 0.999x and compiler
1.003x, so the compiler does not exercise this path.

Correctness: sizes 0/1/2/47/48/49/100/128/129/1000/5000 across full traversal,
iterator forking, stepping across `erlang:garbage_collect/0`, partial takes,
mixed keys including `1` and `1.0`, and ordered/reversed iterators — output
identical to OTP 29 on every check. `{test_maps,ok}` and `{test_sets,ok}`.

**Left undone:** `maps:iterator/2` still collects and sorts all keys for
`ordered`/`reversed`, then looks each value up again, even though a B-tree is
already in key order. Doing better needs a way to tell a tree-backed map from a
flat one in Erlang, which is API surface this change did not want to add.

## 3. FP value forwarding — drop

The code is as described: every `float_op` reloads `jit_state->fr`, loads two
doubles, computes, and stores back — 4 memory operations in a 10-instruction
sequence. The mechanism is real and the control flow permits a cache (the only
branch between consecutive float ops goes to a non-returning badarith path).

But the end-to-end ceiling is too small. A two-million-iteration **dependent**
float chain — the case forwarding targets, where each result feeds the next —
runs 16,909us on BEAM against 18,411us on AtomVM: **0.918x**. Mixed float work
is 0.984x. So the entire FP gap, memory traffic and boxing and loop overhead
together, is about 8%, and forwarding addresses only part of it. `pi_test` is
10% of the app aggregate, so the whole idea is worth well under 1% there.

Not worth a block-scoped FP register cache with invalidation at every helper,
branch and GC, in the part of the JIT that previously shipped a heap-corrupting
float bug. Revisit only if a float-dominated workload becomes a target.

## 4. Private binary append — assessed, not implemented

Premise confirmed by profile. Running `binary_test` in a loop, the top C
symbols are `term_reuse_binary` (227 samples) and
`memory_ensure_free_with_roots` (217) — that second one is exactly the heap
preparation the idea wants to skip when the append will reuse the accumulator.

Sizing stops it for now: eliminating that preparation entirely is worth roughly
11% of `binary_test`, which is 0.9% of the app aggregate — about 0.1% overall.
It would move `binary_test` from 0.72x to perhaps 0.80x, still short of the
0.95x gate, in exchange for reordering `OP_BS_CREATE_BIN` across every backend.
Worth doing only as part of a broader attack on the binary result, and the
proposal's own advice to split the test into build/parse/hash phases should
come first — the profile shows the parse and hash loops dominate the JIT-code
samples, so the build phase may not be where that test loses.

## 5. NIF effect contracts — drop

The ESTONE number that motivates this does not survive inspection. Its
`bif_dispatch` micro calls `erts_debug:flat_size(true)` — flat size of an
immediate — and `demonitor/1` on an already-flushed monitor, repeatedly. The
proposal's own gate rules out optimising precisely that, and earlier sessions
already resolved not to trade real workloads for the synthetic score.

The general mechanism was then sized on a real workload: in a profile of four
`unicode_util.erl` compiles, `jit_call_ext0` is 38 samples against `node_find`'s
499 — about 3% of attributed C symbols, agreeing with the 1.8% the August
diagnostic measured. Halving the call boundary would return ~1%. The +111k
recoverable ESTONES is not a forecast of that.

## 6. B-tree node layout — assessed, the best next bet

This has the strongest remaining evidence. In the same real-compile profile:

| symbol | samples |
|---|---:|
| `node_find` | 499 |
| `term_compare0` | 309 |
| `bt_insert` | 128 |

`node_find` alone is 41% of attributed C samples, and batch `stdlib` (0.978x)
is the standing gate failure.

`node_find` is already well optimised — invariants hoisted, a small-integer
fast path covering ~84% of compiler key comparisons, a 2-tuple probe path — so
the remaining cost is the access pattern the layout change targets. With
`BT_T = 24`, a full node's 47 keys are interleaved with their values across
about 752 bytes, so a binary search touches ~6 cache lines; a separate keys
tuple would put the same keys in ~376 bytes. That is a real halving of the
searched footprint, and it is not something a cheaper local change to
`node_find` can reach.

It is also a rewrite of the node representation across `make_node`, insertion,
splitting, deletion, update, iteration and rank. It was not attempted here
rather than attempted hastily against the core map structure. Do it on its own,
against the gate the proposal set (>=3% batch stdlib), measuring lookup, update
and insert separately as it advises.

## 1. Receive markers — assessed, not implemented

Premise verified: `OP_RECV_MARKER_BIND`, `CLEAR` and `USE` decode their
operands and do nothing, and `RESERVE` writes `nil`, in both
`opcodesswitch.h` and `jit.erl`. The proposal's measured backlog gap (up to
~63x at 10,000 queued messages) is a genuine workload hole rather than a score
artifact, and this is the most valuable item in the document.

It is also the one that cannot be done carelessly. A marker is a saved position
in a mailbox whose nodes are removed by `remove_message`, moved from the outer
list to the inner list by signal processing, and traversed by GC; the proposal
is right that "a saved pointer to a removed queue node is unsafe" and that
marker lifetime has to become part of mailbox logic. That is a focused piece of
work in the most correctness-critical part of the VM, with a test matrix
(timeouts, marker reuse and clearing, references surviving GC, signals
interleaved with messages, nested outstanding requests) that deserves its own
session.

Recommended next, together with idea 6.

## Reproduction

The probes referenced by the original document (`bench_new_ideas.py`,
`new_ideas_probe.erl`, `census_new_ideas.escript`) were not present in the
checkout, so the map, float, bigint and binary probes used here were written
fresh. Correctness harnesses that are worth keeping — the map iterator
differential test and the bignum division differential test against OTP — are
described in the commit messages of `15a6a6501` and `f6b2906a6`.
