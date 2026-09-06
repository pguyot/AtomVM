<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Eight new benchmark experiments — 2026-09-06

The most compelling new findings are **unimplemented receive markers** and
**eager materialization of large-map iterators**. Both have measurable costs
that the headline suites largely miss. Floating-register forwarding, a
nonallocating binary-append path, effect-specific NIF calls, B-tree layout,
and bigint quotient estimation are additional experiments. Signed division
by two was prototyped and was **neutral** in the first A/B screen.

These are experiments, not eight established speedups. No production VM or
JIT source was changed. Supporting probes and this report are the durable
changes; the division prototype and generated artifacts are isolated under
`build.new-ideas-2026-09-06/`.

## Baseline and scope

Read together with [latest status](BENCHMARK_STATUS_2026-09-06.md),
[previous proposals](ESTONE_JIT_PERFORMANCE_HANDOVER.md),
[their measured verdicts](ESTONE_JIT_IDEAS_MEASURED.md), and the
[August diagnostic and plans](BENCHMARK_DIAGNOSTIC_2026-08-29.md).

The checkout is `c669cb66fda6cb2b4dee3f6510866753706a82f7`. The existing
Release/SMP/AArch64 JIT build's `AtomVM` SHA-256 exactly matches the September
status: `8db84bbfe45f3b233f895195e8d38196693e4079675a5550ca9e9c37a417265b`.
New probe modules were compiled and AOT-precompiled locally. The shell's
default `erl` is Homebrew **OTP 28**, so all compilation and reference runs
explicitly use `/opt/local/bin/erl`, `erlc`, and `escript`: **OTP 29, JIT**.
OTP sources were inspected in `/Users/paul/otp/`.

No applicable `CLAUDE.md` was found in the checkout or checked parent/global
locations. The old handover's static-only restriction belonged to its earlier
request; this investigation uses the current user's permission to execute.

The conditional-store message regression fix is still the immediate baseline
repair. It, mailbox-hint relocation, the three shipped ideas, the rejected
ideas, and August's existing cache/HAMT/MIR/run-queue proposals are **not counted
as new ideas here**. Repair that regression before evaluating new SMP changes.

## What was measured now

[Probe source](new_ideas_probe.erl) and [driver](bench_new_ideas.py): 11 rotating,
interleaved rounds after two warmups, both engines using one online scheduler.
Timings are internal microseconds, excluding process startup and data creation;
an explicit GC follows setup. They include GCs and scheduling during the timed
work. These are mechanism probes, not directly comparable to the publication
suite's ten-scheduler results.

| Probe | Work per sample | BEAM median us | AtomVM median us |
|---|---|---:|---:|
| Receive, no old messages | 3,000 fresh-reference replies | 331 | 375 |
| Receive, 100 old messages | same | 407 | 3,414 |
| Receive, 1,000 old messages | same | 770 | 28,822 |
| Receive, 10,000 old messages | same | 4,820 | 302,593 |
| 100-entry map, take one | 1,000 new iterators | 403 | 145 |
| 1,000-entry map, take one | same | 338 | 819 |
| 10,000-entry map, take one | same | 635 | 8,346 |
| 10,000-entry map, take eight | 1,000 new iterators | 970 | 18,567 |
| 1,000-entry map, consume all | 10 complete traversals | 223 | 176 |
| 10,000-entry map, consume all | same | 1,918 | 1,460 |

Raw samples, commands and artifact hashes:
[`long-probes/results.json`](../../build.new-ideas-2026-09-06/long-probes/results.json).
All expected rows were required and every process had to exit successfully.
The receive test checks the reply count and drains/checks all old messages.
Map probes check each key/value pair; full traversals also check the exact sum.
Default map iteration order is unspecified: partial traversals visit the same
number of entries, not necessarily the same keys on each VM.

The large receive gap is approximately **63x** and the large-map take-eight
gap approximately **19x**, but neither is a promised optimization speedup.
BEAM's receive time also rises with backlog; marker handling does not remove
all queue, GC, and reference-allocation costs. The small/full-map controls
show why globally replacing the current iterator strategy needs care.

## 1. Make receive markers actually skip old messages

**Evidence:** `OP_RECV_MARKER_BIND`, `CLEAR`, and `USE` just decode operands;
`RESERVE` writes `nil`. This is true in both [jit.erl](../../libs/jit/src/jit.erl)
and [opcodesswitch.h](../../src/libAtomVM/opcodesswitch.h). OTP's
`erts/emulator/beam/jit/arm/instr_msg.cpp` instead calls
`erts_msgq_recv_marker_insert/bind/clear/set_save`.

OTP 29's compiler explicitly reports the probe's `make_ref`/`receive` pair as
optimized. A static census of 1,079 installed OTP BEAM files found **184
`recv_marker_use` instructions**. This is real compiler output, not a source
idiom eliminated before AtomVM sees it.

**Experiment:** implement a bounded number of mailbox-position markers, bind
them to fresh references, and start a qualifying receive after the marker.
Falling back to a full scan when marker slots are exhausted remains correct.
This removes repeated work proportional to an irrelevant mailbox prefix; it
does not change notification coalescing or global run queues.

**Test next:** the supplied self-send probe, then actual client/server RPC with
backlogs of 0/100/1,000/10,000, nested outstanding requests, and selective
receives used by OTP `gen`/`rpc`. Count message inspections. Include timeout,
marker reuse/clear, reference movement through GC, exceptions, outer-to-inner
queue transfer, and signals interleaved with messages. A saved pointer to a
removed queue node is unsafe; marker lifetime must be part of mailbox logic.

**Gate:** eliminate repeated prefix inspections without hurting empty-mailbox
traffic or losing signals/wakeups. No ESTONE score improvement is required:
this addresses a workload coverage hole.

## 2. Give map iterators a lazy B-tree traversal stack

**Evidence:** [nif_maps_next](../../src/libAtomVM/nifs.c) initially reserves
`4 * size + 8` heap words and calls `termtree_to_kv_list`. The existing cursor
is `{RemainingKVList}`, so returning the first item already constructs the
entire list. At 10,000 entries that reservation is **320,064 bytes** on this
64-bit build. The map itself remains live too. Representation also depends on
construction history: the general tree threshold is 128, but single-key growth
has a separate cutoff of 48. Do not assume the 100-entry control is flat.

**Experiment:** store a persistent stack of `(node, child/key position)` frames
as GC-visible Erlang terms. Descend only far enough to produce the requested
entry. Try small leaf batches if one-entry stepping loses full-scan throughput.
This changes iteration's upfront allocation, not the map representation or
its lookup cache.

There is a second opportunity within the same cursor design:
[maps:iterator/2](../../libs/estdlib/src/maps.erl) currently collects and sorts
all keys for `ordered`, and sorts/reverses for `reversed`, then looks values up
again. A B-tree already has the required order. Forward/reverse cursors can
avoid that work; custom comparator functions still need their generic path.

**Test next:** first/first-eight/all entries; forward/reverse order; pausing and
forking an immutable iterator; GC between steps; mixed keys, including `1`
and `1.0`. Real callers include early-exit `sets:is_disjoint/2`,
`sets:is_subset/2`, depth-limited map formatting, and compiler type-map walks.
The installed OTP compiler also uses ordered iterators explicitly.

**Gate:** remove O(map-size) first-step allocation and retain the measured
full-traversal win. Measure allocated words, time to first result, complete
traversal, and batch `stdlib`; do not assume a synthetic 19x gap transfers to
compiler throughput.

## 3. Forward floating values between native FP instructions

**Evidence:** [jit_aarch64:float_op/5](../../libs/jit/src/jit_aarch64.erl)
loads `jit_state->fr`, loads two doubles, performs one arithmetic instruction,
and stores the result for every operation. Arithmetic is already native; the
gap is the memory traffic between operations. A corpus census found **400 FP
arithmetic instructions in total and 65 functions with more than one**.
Newly compiled `pi_test` has `fdiv`, `fmul`, and `fadd` feeding the same `fr0`,
with intervening conversions to `fr1`.

**Experiment:** a small basic-block FP register cache, initially with
write-through stores, forwards those producer results directly into consumers.
Conversions must participate in the mapping too: they currently use scratch
FP registers and can clobber a cached value. Once load forwarding proves useful,
consider removing stores only for dead FP temporaries within that block.

This is distinct from the rejected integer loop-residency proposal: the memory
round trips are visible between dependent FP instructions inside a block.
It also avoids starting with cross-call or cross-schedule register contracts.

**Test next:** amplified `pi_test`, polynomial/vector calculations, and ESTONE
float arithmetic, checking generated loads/stores and preserving exact results.
Keep operation order and each non-finite/error check. Do not fuse multiply/add
or enable fast-math as a shortcut. Test conversion fallback, division by zero,
overflow, signed zero, and FP cache invalidation at helpers/branches/GC.

**Gate:** a repeatable gain in real floating workloads with bounded code growth;
the existing 0.91x pi result is the relevant publication target.

## 4. Let successful private binary append bypass allocation preparation

**Evidence:** [OP_BS_CREATE_BIN](../../libs/jit/src/jit.erl) calls
`PRIM_TRIM_LIVE_REGS` and `PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS` before it knows
whether private append will reuse the accumulator. Meanwhile
[term_reuse_binary](../../src/libAtomVM/term.c) already has a capacity-hit path
that merely clears newly exposed bytes, updates logical size, and returns the
same term. Geometric capacity growth is already implemented; proposing it
again would add nothing.

**Experiment:** a tightly guarded, byte-aligned, uniquely owned refc-binary
capacity-hit path before heap preparation, starting with `Alloc == 0` and no
sub-binary wrapper requirement. Keep the source rooted on the fallback path.
Initially perform the reuse test in one noncollecting primitive; only inline
it if that experiment earns the additional backend complexity.

For fully covered byte-aligned stores, separately test omitting the clearing
of bytes that the following segment writes will overwrite. Bit insertions can
OR into partial bytes, so clearing cannot be dropped generally. Validate all
potentially failing segment operations before making mutated data observable.

**Test next:** split the existing binary test into build/parse/hash phases.
Its build loop demonstrably compiles to `private_append`. Sweep appended widths,
capacity hits/misses, escaped/shared binaries, bitstrings, and heap limits.
Count successful zero-allocation appends versus helper/GC calls. Reservation
size for a large refc binary is a descriptor size, not its whole byte payload;
do not claim each append reserves another full off-heap binary on the heap.

**Gate:** lower overhead on binary building with identical fallback/error
semantics. This targets the app's 0.72x binary result, subject to the phase split.

## 5. Give small built-in NIFs explicit effect contracts

**Evidence:** [struct Nif](../../src/libAtomVM/exportedfunction.h) contains a
type and function pointer, with no effect classification. Resolved native Erlang
calls have a direct path, but NIF calls go through
[jit_call_ext0](../../src/libAtomVM/jit.c). Its generic protocol sets NIF root
arity, handles traps/context changes, checks heap fragments, and reconstructs
the continuation; the backend synchronizes reductions and VM state around it.
The existing primitive effect table does not classify these imported NIFs.

**Experiment:** classify a small audited subset of built-ins as unable to
collect, trap, reschedule, or change heap/stack state. Give that subset a
term-returning call path with a smaller synchronization/return protocol.
Keep error-capable and allocating cases distinct. The local
`nif_erlang_is_process_alive_1` body is one concrete non-debug candidate: it
checks the process table and returns an immediate boolean without allocating.
This is a narrower new
mechanism than August's proposal to bind imported function pointers: measure
pointer-resolution savings and effect-protocol savings separately.

**Test next:** first decompose `bif_dispatch` into dictionary, debug-size,
monitor creation, successful demonitor, and absent demonitor work. Profile the
full benchmark too. Its absent-monitor repetitions and immediate debug terms
are poor grounds for a score-only specialization. `demonitor/2` with `flush`
explicitly traps and must retain that protocol. Even apparently read-only
calls need the existing fragment/GC invariant understood before bypassing it.
Require useful non-debug built-in call sites in compiler/application profiles.

**Gate:** a measurable general boundary-cost reduction, not constant-folding
`flat_size(true)`. BIF dispatch's +111k recoverable ESTONES makes this worth
investigating, but that number is not an estimate of this experiment's gain.

## 6. Change B-tree node layout instead of replacing the B-tree

**Evidence:** [termmap_tree.c](../../src/libAtomVM/termmap_tree.c) uses
`BT_T = 24`: up to **47 keys**, despite the stale `// 15` beside the macro.
Each node stores an interleaved tuple `{K0,V0,K1,V1,...}`. `node_find` probes
every other word using a branchy binary search. Existing integer and two-tuple
comparison fast paths already exist and are not new proposals.

**Experiment:** split node keys and values into separate tuples, keeping the
same ordered persistent B-tree. Searching reads a denser key array; replacing
a value can share the entire keys tuple and copy only values. This adds two
words per node for the extra wrapper slot and tuple header. It can also add
pointer loads and change allocation behavior, so it is not automatically better.

Independently test branchless lower-bound search on nodes certified to contain
only small integers, then a small SIMD rank search if scalar results justify
it. Certification must be maintained on insertion/split, and mixed nodes use
the generic comparator. Signed tagged small integers retain numeric order;
atom IDs do not implement Erlang atom ordering.

**Test next:** lookup/update/insert separately, node occupancy, integer versus
mixed keys, and `unicode_util` plus full batch `stdlib`. Count bytes copied per
update and comparisons per lookup. Do not bundle layout and search changes into
one unexplained timing result.

**Gate:** at least a repeatable 3% batch-stdlib gain and acceptable heap growth.
This attacks the reported `node_find` cost without repeating the two failed
global HAMT replacements or the already proposed map-probe sidecar.

## 7. Remove software wide division from repeated bigint quotient estimation

**Evidence:** [intn.c](../../src/libAtomVM/intn.c) already has the wider
64-bit-limb division improvement. Its Knuth-D loop nevertheless evaluates
`qhat = num / vn[n - 1]` with a 128-bit numerator. The current binary imports
`___udivti3` (checked with `nm`). This establishes a software wide-division
dependency, not its dynamic share in the current app. The app's modular-power
test repeatedly uses the same modulus, while division renormalizes it each call.

**Experiment:** compute a normalized divisor reciprocal and use multiply-high
plus bounded quotient correction in place of repeated software division.
First amortize within one division call; only then try a tiny per-process or
per-scheduler cache keyed by copied divisor limbs, not an unrooted term pointer.
The cache must prove useful for arbitrary repeated divisors, not just the
benchmark's Mersenne modulus. This leaves the general term representation and
the 256-bit limit unchanged.

**Test next:** profile current bigint time to establish the ceiling; then
1/2/4-limb divisors, repeated versus varying moduli, equal numerator/divisor,
quotient estimates needing correction, maximum magnitudes, and negative
Erlang operands. Differential-test quotient and remainder against OTP, including
the identity `A = Q*B + R` and remainder sign. Keep the portable core for targets
without efficient multiply-high.

**Gate:** improve the app's 0.74x bigint result without regressing varying
divisors. Do not revive "use wider limbs"; that part has already shipped.

## 8. Signed power-of-two division/remainder — first screen neutral

**Evidence:** the existing shift/mask lowering requires a statically bounded,
nonnegative dividend. Unknown-sign small integers still use `sdiv` even for
literal two. The freshly compiled ESTONE `do_arith2` contains two such `div 2`
operations with integer-any range. The installed OTP corpus contains **278
literal positive-power-of-two div/rem instructions**, including **89 div-by-two
sites**. These are opportunity counts, not 278 missed optimizations: some already
meet the existing range specialization.

**Experiment actually run:** replace only the AArch64 runtime-small-integer
`div 2` body in an isolated copy of `jit.erl`. With tagged operand `t`, emit
`((t + (t logical_shift_right 63)) arithmetic_shift_right 1) OR 15`.
The sign correction preserves Erlang's truncation toward zero, unlike a bare
arithmetic shift for negative odd values. Bignums retain the existing fallback.
OTP's `jit/arm/instr_arith.cpp:emit_div_rem_literal` uses this identity too.

| Two-million-call signed loop | Median us |
|---|---:|
| Baseline | 5,578 |
| Prototype | 5,554 |

21 interleaved rounds after three warmups: **1.004x**, paired bootstrap 95%
interval **0.991–1.018**. Native BEAM artifacts differ but have equal byte size.
Fourteen boundary/positive/negative/bignum cases matched OTP; the timed loop's
checksum matched too. This is a limited screen, not full semantic validation.
Raw data and hashes: [div2-results.json](../../build.new-ideas-2026-09-06/div2-results.json).

**Verdict:** do not implement this merely on instruction-count grounds.
A division-dominated dependency chain, general signed power-of-two remainder,
or another physical CPU might justify a further experiment. The native-call
loop used here does not establish the effect on ESTONE or real applications.
Unlike the earlier shipped `bsr` optimization, there is no demonstrated win yet.

## Suggested sequence and reproduction

Start with receive markers and a lazy/batched map cursor because their missing
mechanisms have direct evidence. For the existing publication losses, profile
the binary phases and BIF decomposition, then test FP forwarding. B-tree layout
and reciprocal bigint division are the larger bets. Keep signed div-by-two low
priority after its neutral screen.

Reproduce the new baseline probes from the repository root using a fresh output
directory:

```sh
python3 tools/dev/bench_new_ideas.py --work /tmp/atomvm-new-ideas-run --runs 11
/opt/local/bin/escript tools/dev/census_new_ideas.escript
/opt/local/bin/erlc +recv_opt_info -o /tmp tools/dev/new_ideas_probe.erl
```

The census is static disassembly of installed OTP 29 BEAM files and fails on
disassembly errors. It is not a dynamic profile and does not include the
separate benchmark project. Existing VM/library images are reused by the
probe driver; it records their hashes and recompiles/precompiles the probe.

Before accepting any runtime/JIT patch, use the established rotating A/B
publication harness, check equal benchmark inclusion, and report ESTONE total
time as well as score. Check full-map traversal and ordinary empty-mailbox
traffic alongside the new adversarial probes. Scale the correctness and MCU
RAM/code-size checks to the mechanism changed; this investigation itself does
not establish gains on arm32, x86-64, or RISC-V.
