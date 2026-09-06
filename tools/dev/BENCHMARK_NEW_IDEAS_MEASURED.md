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
| 4 | Private binary append bypasses allocation prep | assessed, resized | build phase is **0.150x** of BEAM, not parse/hash |
| 5 | Effect contracts for small NIFs | **drop** | real-workload boundary is 38/1222 samples |
| 6 | B-tree node layout instead of a new B-tree | **implemented, then reverted** | gate wanted 3% batch stdlib, got 0.6% |
| 7 | Reciprocal bigint quotient estimation | **shipped** `f6b2906a6` | **1.090x** bigint; `__udivmodti4` gone |
| 8 | Signed power-of-two division | **drop** (author screened) | 1.004x, accepted without re-running |

Two shipped, five dropped with data (including the prerequisite repair and one
written in full before its gate rejected it), one assessed and left with a
sized recommendation.

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

## 4. Private binary append — resized, and the proposal's guess was wrong

The document suggested splitting the benchmark into build/parse/hash phases
before acting, and warned that the parse and hash loops might be where the test
actually loses. Doing that split says the opposite, emphatically:

| phase | BEAM | AtomVM | ratio |
|---|---:|---:|---:|
| build (100 appends) | 7,537 us | 50,308 us | **0.150x** |
| parse (400 bytes) | 4,410 us | 6,730 us | 0.655x |
| hash (400 bytes) | 16,294 us | 25,433 us | 0.641x |

Building is 6.7x slower than BEAM and is 61% of AtomVM's time across the three
phases, against 27% of BEAM's. That is where `binary_test`'s 0.72x comes from,
and it is exactly the path this idea targets.

Profiling the build loop alone attributes it roughly as:

| symbol | share |
|---|---:|
| `memory_ensure_free_with_roots` (+ its pinned wrapper) | 26% |
| zeroing (`memset` / `bzero`) | 24% |
| `term_reuse_binary` | 23% |
| `term_binary_data` | 8% |
| `jit_bs_create_bin_wrap` | 6% |

The cheap half of the idea was tried and rejected. `term_reuse_binary` clears
the newly exposed bytes on every append -- four bytes here, so nearly all call
overhead -- and clearing a short tail with inline stores instead measured
**1.015x on the build phase**, so the sampled `memset` share does not convert
into time. Reverted.

The remaining 26% is the idea as written: test the capacity hit before
preparing the heap, and skip `PRIM_TRIM_LIVE_REGS` and
`PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS` when the accumulator will be reused. The
emission is in the shared frontend, so it need not be written per backend, but
it means branching around the preparation inside `OP_BS_CREATE_BIN` -- an
opcode that has shipped miscompiles before -- and it was left rather than
started without room to test it properly.

Worth doing next, and worth more than the earlier estimate in this file's first
revision, which sized it against the whole test rather than the build phase.

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

## 6. B-tree node layout — implemented in full, then reverted

The evidence pointed here hardest, so it was written: nodes changed from
`{Size, KV, Children}` with an interleaved `{K0,V0,K1,V1,..}` tuple to
`{Size, Keys, Values, Children}` with separate arrays, across `make_node`,
`node_replace_value`, `node_replace_child`, `node_find`, both insert paths and
the accessors. All of it is contained in `termmap_tree.c`; nothing outside the
file knows the node shape.

**Both predicted mechanisms worked.** Profiling the same four `unicode_util.erl`
compiles before and after:

| symbol | interleaved | split |
|---|---:|---:|
| `node_find` | 693 | 637 (−8%) |
| `bt_insert` | 202 | 125 (**−38%**) |
| `term_compare0` | 436 | 426 (−2%) |

The search reads a denser key array, and a value update now shares the keys
tuple instead of copying every key beside the values -- which is where the
insert saving comes from.

**The end-to-end gain did not follow.** Controlled build pair, same tree,
9 interleaved rounds per application:

| batch | interleaved | split | speedup |
|---|---:|---:|---:|
| stdlib | 15.193 s | 15.097 s | 1.0064x |
| compiler | 7.303 s | 7.277 s | 1.0036x |
| kernel | 6.442 s | 6.402 s | 1.0062x |

0.4-0.6%, consistently and in the right direction, against a gate of **3% on
batch stdlib**. The extra wrapper slot and tuple header also cost two words per
node: measured with `erts_debug:flat_size/1`, a 10,000-entry map grows from
21,299 to 21,729 words (**+2.0%**), and a 1,000-entry map by the same
proportion.

So: the C-symbol profile overstates what map work is worth end-to-end, because
much of a compile runs in JIT-generated code that a `sample` profile attributes
elsewhere. Five times short of its gate and costing 2% of map memory on a VM
that targets microcontrollers, it was reverted rather than shipped.

That result also retires the hypothesis this idea shared with the two failed
HAMT attempts -- that `node_find`'s cost is reachable by rearranging the map.
`node_find` did get 8% faster and it bought almost nothing. Anything further
here should target the number of comparisons, not their layout.

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

Recommended next, together with the remaining half of idea 4. Idea 6 is now
closed: see above.

## Reproduction

The probes referenced by the original document (`bench_new_ideas.py`,
`new_ideas_probe.erl`, `census_new_ideas.escript`) were not in the checkout when
this work started -- they appeared later, untracked -- so the map, float, bigint
and binary probes used here were written fresh and are independent of them. Correctness harnesses that are worth keeping — the map iterator
differential test and the bignum division differential test against OTP — are
described in the commit messages of `15a6a6501` and `f6b2906a6`.
