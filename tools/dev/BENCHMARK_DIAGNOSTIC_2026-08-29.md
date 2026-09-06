<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AArch64 AOT diagnostic benchmark — 2026-08-29

This is a diagnostic baseline, not a publication figure.  The important new
measurement is `erlc` in both modes: one fresh VM per source file and one VM per
application batch.  Previous reports measured only the former, which mixes
compiler throughput with process/module startup.

## System and builds

- Host: Mac mini, Apple M4 (4 performance + 6 efficiency cores), 24 GB RAM,
  macOS 26.6.2, AC power.
- AtomVM: `abc35e567d1802cc365690ca0dc46bb5dd4b6e17`, Release (`-O3`), SMP and
  JIT enabled; benchmark and compiler modules AOT-precompiled for `aarch64`.
- AtomVM compiler bundle: 94 OTP compiler/stdlib modules plus the
  `atomvm_erlc` front end, all regenerated after the branch switch.
- Reference: OTP 29, BEAM `emu_flavor=jit`, 10 schedulers.  OTP source commit
  `8ce4b017ba3bad0a09ec067b14cf0b12a227022c` (`OTP_VERSION` 29.0).
- AtomVM had two unrelated pre-existing tracked edits, so `git describe` says
  dirty; neither file participates in this host build or benchmark.

The regenerated artifact SHA-256 values are:

- `src/AtomVM`:
  `f0cd84a4e91496c826cad368ccbbbd9b367b6119ffe8ca5d86ae11a899a39e5a`
- `atomvmlib-aarch64.avm`:
  `6c55fa190c404176d8a813a88dd8e6588cb63e48af0ee7a61561ec90b1223c99`
- `benchmark-aarch64.avm`:
  `1f03032037b781b9f35dea8c78df1d0db2d6b116481d291808a399e6e5d79a8a`
- `estone-aarch64.avm`:
  `00fd3480ac7782be31b23a7c20160f9a30f6250edb50d2549796400b7c141b4b`
- `erlc-atomvm-aot`:
  `0aa93fe583bee87f825cd966e5c4c9cc885ecb4e3d4f1f43e8582f556f596359`

Raw samples and exact commands are in
[`erlc.json`](../../build.jit.rebase/publication/erlc.json),
[`estone.json`](../../build.jit.rebase/publication/estone.json), and
[`app.json`](../../build.jit.rebase/publication/app.json).  The rotating A/B
driver is [`bench_publication.py`](bench_publication.py).

## Method

- Every round interleaves BEAM and AtomVM and reverses their order on alternate
  rounds (also staggered by file/application).  This controls thermal drift and
  first-position cache bias.
- Timing uses `perf_counter` around the complete OS process.  VM startup is
  included.
- Compiler support is discovered before timing, warming the filesystem cache.
  A source is counted only when both compilers produce its `.beam`.
- Corpus: OTP 29 `compiler`, `stdlib`, `kernel`, `sasl`, and `crypto`: 280
  files.  Both compilers built all 280; there are no exclusions.
- Per-file mode: seven samples per compiler per file; the reported application
  value is the sum of per-file medians.
- Batch mode: all common sources in an application are passed to one compiler
  invocation; 15 samples per compiler per application.  The total is the sum
  of application medians.
- ESTONE: 31 measured interleaved runs after three warmups.  It is the local
  common-subset port: `port_io` and ETS are excluded on both engines and
  `erts_debug:flat_size/1` substitutes for `size_shared/1`.  It must not be
  presented as the complete upstream ESTONE suite.
- Benchmark app: 21 measured interleaved runs after three warmups; aggregate is
  the sum of the 11 base-test medians, excluding the duplicate scheduler=1
  rows and process startup.

Ratios below are `BEAM time / AtomVM time`; greater than 1 means AtomVM wins.

## erlc

### One file per process

| application | files | BEAM | AtomVM | ratio |
|---|---:|---:|---:|---:|
| compiler | 59 | 21.176 s | 10.145 s | **2.087x** |
| stdlib | 98 | 36.734 s | 19.289 s | **1.904x** |
| kernel | 104 | 27.777 s | 9.828 s | **2.826x** |
| sasl | 17 | 4.233 s | 1.271 s | **3.331x** |
| crypto | 2 | 0.673 s | 0.344 s | **1.954x** |
| **total** | **280** | **90.593 s** | **40.878 s** | **2.216x** |

The aggregate ratio in each of the seven rounds was 2.202–2.237x.  AtomVM won
279/280 files; only `stdlib/unicode_util.erl` lost (0.843x).  The median of the
280 individual ratios was 3.105x.

### One application per process (batch)

| application | files | BEAM | AtomVM | ratio |
|---|---:|---:|---:|---:|
| compiler | 59 | 7.540 s | 7.444 s | 1.013x |
| stdlib | 98 | 14.970 s | 15.500 s | **0.966x** |
| kernel | 104 | 6.920 s | 6.917 s | 1.000x |
| sasl | 17 | 1.031 s | 0.845 s | **1.219x** |
| crypto | 2 | 0.508 s | 0.350 s | **1.449x** |
| **total** | **280** | **30.970 s** | **31.056 s** | **0.997x** |

The full-corpus paired ratios by round span 0.968–1.002x.  `stdlib` lost in
every batch round (paired range 0.933–0.976x), so that deficit is real; the
overall/compiler/kernel differences are effectively parity on this run.

The difference between modes is startup plus module-loading/cache
amortisation.  Estimated marginal cost per extra invocation (per-file sum
minus batch, divided by extra invocations) is 200–235 ms for BEAM versus
27–47 ms for AtomVM on the four applications with enough files.  The 2.216x
per-file result is a valuable real user-facing win, but it is not evidence that
AtomVM executes the compiler twice as fast.  The batch result says steady work
is currently at parity.

## ESTONE common subset

- BEAM: median **2,561,006 ESTONES** (range 2,436,752–2,688,482).
- AtomVM: median **1,948,812 ESTONES** (range 1,864,863–2,052,184).
- AtomVM/BEAM: **0.761x**; paired bootstrap 95% interval **0.750–0.767x**.
- Median process wall: 628 ms BEAM, 744 ms AtomVM.

Largest score deficits explain more than the entire net gap:

| micro | BEAM | AtomVM | AtomVM/BEAM |
|---|---:|---:|---:|
| pattern matching | 1,190,476 | 899,071 | 0.76x |
| small integer arithmetic | 243,031 | 80,241 | 0.33x |
| BIF dispatch | 316,225 | 191,339 | 0.61x |
| links | 57,835 | 3,709 | 0.06x |

AtomVM already wins list manipulation (1.51x), function calls (1.37x),
allocation (1.15x), and the generic-server micro (1.15x).  Optimising the
winning paths further would raise the score but would not address the
structural losses.

## Supplemental benchmark app

The 11-test compute aggregate is **87.878 ms BEAM vs 70.045 ms AtomVM =
1.255x**.  Median complete-process wall is 384 ms vs 103 ms.

Strong AtomVM wins: Sudoku solution 2.00x, list 1.78x, ping-pong 1.75x, PRNG
1.49x, map 1.34x.  Losses: bigint 0.77x, binary 0.77x, Sudoku puzzle 0.80x,
prime 0.89x, crypto 0.91x, float/pi 0.92x.

Scheduler evidence is particularly clear.  With normal scheduler counts,
prime is 0.89x.  In the suite's scheduler=1 rerun, AtomVM beats BEAM by 1.48x
(6.282 ms vs 9.305 ms).  Ping-pong is 1.75x normally but 7.01x with one
scheduler.  AtomVM's global ready queue and scheduler wake/lock protocol are
turning a compute win into an SMP loss.

## Current profile evidence

A current-build macOS native/C sample of three repeated
`unicode_util.erl` compiles collected 8,533 attributed self samples.  Top
AtomVM C symbols:

| symbol | self share |
|---|---:|
| `node_find` (large ordered-map B-tree lookup) | 24.9% |
| `term_compare0` | 16.2% |
| `bt_insert` | 6.8% |
| `memory_shallow_copy_term_impl` | 2.5% |
| `jit_call_ext0` | 1.8% |

Large ordered-map lookup/comparison/insertion therefore consumes about 48% of
the sampled compile.  This agrees with the older detailed profile after the
newer map, register, and reduction-counter improvements.  The simple HAMT
replacement has already measured net-worse twice, and deferred X-register
writeback was measured at only about 1.5–1.8%; neither should be repeated
without a materially different design.

# Plan to beat BEAM consistently and usefully

“Consistent” should be a gate, not a favourable aggregate:

1. Batch `erlc`: at least 1.10x on **each** of compiler, stdlib, kernel, sasl,
   and crypto, with the lower confidence bound above 1.05; retain the current
   >2x per-file win and 100% corpus support.
2. Runtime app: retain >=1.20x aggregate; no substantial base test below 0.95x.
3. ESTONE common subset: >=1.00x overall, with pattern, integer arithmetic,
   BIF dispatch, and links each >=0.90x.  Do not trade real workloads for the
   synthetic score.
4. No BEAM-output mismatch, differential-fuzz regression, test regression, or
   unacceptable embedded code/RAM growth.

## Phase 1 — make map work adaptive, not a third global HAMT attempt

This is the direct route to winning batch `stdlib` and `unicode_util`.

1. Add counters for large-map operation mix, size, key shapes, repeated
   `(map identity,key)` probes, successful update vs insertion, and B-tree
   comparisons/path copies.  Capture compiler and non-compiler workloads.
2. Prototype a tiny per-scheduler immutable-map probe cache keyed by map root
   identity plus key identity/hash.  Clear it by GC epoch initially, so it
   holds no untracked moving roots.  Compiler analysis repeatedly probes the
   same persistent maps; even a 4–16-entry cache can bypass `node_find` without
   changing representation or semantics.  Gate: >=5% on batch stdlib and no
   meaningful general regression.
3. If hit-rate evidence supports it, add an optional off-heap hash/path sidecar
   only for sufficiently large maps.  Keep the B-tree as the canonical ordered
   representation, so ordered iteration and term ordering are unchanged.
   The sidecar accelerates exact reads and existing-key updates; it is lazy,
   refcounted with the map tree, and disabled below a measured threshold.  This
   is materially different from replacing all large maps with a HAMT.
4. Independently test specialised representations selected by observed key
   shape: dense small-integer compiler maps can use a persistent vector/radix
   form; homogeneous immediate/2-tuple maps can cache a compact comparison
   prefix.  Retain the generic B-tree fallback.
5. Improve bulk operations structurally: reuse unchanged subtrees for
   `maps:merge`, set union/intersection and deletion instead of materialising
   and rebuilding whole sorted arrays where the operands share ancestry.

Kill any representation/cache experiment below a 3% batch-stdlib win or with
an excessive memory/code-size cost.  Keep the instrumentation.

## Phase 2 — exploit AOT information that BEAM's runtime JIT cannot

Per-opcode micro-fusions are close to exhausted.  A pack-level optimiser is
the plausible route past BeamAsm rather than merely approaching it.

1. Add an AOT link step after all modules are precompiled.  Give native
   functions stable pack-local symbols and relocation records; patch known
   intra-pack `call_ext`, tail calls, and returns to direct branches/veneers.
   Keep the existing dynamic slow path for code replacement and unresolved
   modules.  Measure dynamic instructions and `jit_call_ext*`, not just wall.
2. Lower one whole BEAM function/basic-block graph at a time into a small MIR
   before emitting AArch64.  Use Type + liveness chunks for linear-scan
   allocation, common-test elimination, fall-through layout, and retaining
   temporaries across opcode boundaries.  This attacks ESTONE pattern matching
   without retrying the reverted one-op list fusion.
3. Add profile-guided, pack-local inlining of small leaf functions and hot
   call chains.  Start with the ESTONE integer-arithmetic shape and real hot
   compiler helpers; preserve reduction accounting and exception/GC safe
   points.  AOT can inline across modules in a closed compiler bundle, which
   BeamAsm deliberately does not generally do.
4. If a custom MIR stalls, prototype the same VM ABI through an optional LLVM
   AOT backend on desktop AArch64.  Treat it as an experiment with a 10% batch
   gate, not a required embedded dependency.

## Phase 3 — replace the global SMP run queue

1. Give each scheduler a local deque; enqueue wakeups locally where possible,
   steal only when idle, and reserve the global path for external/task wakeups.
2. Make the poller role independent of ownership of the runnable queue and
   coalesce `sys_signal`/condition-variable wakes.  Preserve scheduler affinity
   and add counters for steals, failed steals, wake syscalls, queue-lock
   contention, and migrations.
3. Gate on prime/ping-pong at 1, 2, 4, and 10 schedulers plus realistic OTP
   supervisor/message workloads.  The immediate goal is to retain the observed
   one-scheduler compute win when SMP is enabled.

## Phase 4 — links/monitors and BIF boundary cost

The 0.06x link result is too large to ignore, but optimise the data structure,
not the benchmark loop.

1. Replace linear per-process link/monitor searches with a small-inline table
   that promotes to a PID/ref-keyed hash table; pool monitor/link nodes per
   scheduler to remove malloc/free traffic.
2. Batch or coalesce symmetric local link/unlink acknowledgements while
   retaining OTP ordering/race semantics.  Profile supervisor trees and mass
   monitor shutdowns as the useful acceptance workload.
3. Measure the imported NIF/BIF boundary.  Directly bind known pack-local NIF
   pointers and add narrowly useful fast paths (small immediate-key process
   dictionary, monitor lookup), rather than constant-folding
   `erts_debug:flat_size(true)` just to inflate ESTONE.

## Phase 5 — close remaining real-workload losses

1. Profile Sudoku puzzle and bigint/binary tests separately.  The generational
   collector is already present; tune promotion/young-heap sizing only from
   allocation-survival data.  Avoid reopening deferred-root-store work unless
   new counters exceed its previous <2% ceiling.
2. Add compiler-server/daemon mode to `atomvm_erlc`.  AtomVM already wins the
   one-shot UX; a persistent protocol makes incremental builds and editor
   integration genuinely fast while the VM core work raises batch throughput.
3. Run differential correctness continuously: AtomVM-vs-BEAM output hashes
   where deterministic, `beam_lib` validation otherwise, the complete tests,
   erlfuzz, and mixed AOT/emulated modules.  Benchmark every optimisation A/B
   against the fixed current baseline with rotating order.

The recommended order is Phase 1 probe-cache instrumentation/prototype, then
Phase 3 local scheduler queues in parallel conceptually (but measured in
separate builds), followed by the larger Phase 2 AOT linker/MIR effort.  Phase
1 is the shortest route from 0.966x stdlib batch to a consistent compiler win;
Phase 2 is the architectural route to a durable advantage over BeamAsm.
