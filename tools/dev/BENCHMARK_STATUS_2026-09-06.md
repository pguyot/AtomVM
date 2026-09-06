<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AArch64 AtomVM vs BEAM — status — 2026-09-06

Successor to [`BENCHMARK_DIAGNOSTIC_2026-08-29.md`](BENCHMARK_DIAGNOSTIC_2026-08-29.md),
re-measured after the 29 commits that landed since it.  Headline: two of the
three benchmarks improved, and the run **found a regression in the message path
introduced by `c669cb66f` eight commits ago** that the ESTONES score cannot see.

## Summary

Ratios are `BEAM time / AtomVM time`; greater than 1 means AtomVM wins.

| benchmark | 2026-08-29 | 2026-09-06 | note |
|---|---:|---:|---|
| ESTONE common subset (score) | 0.761x | **0.907x** | real, CI 0.889–0.920 |
| benchmark app (11-test aggregate) | 1.255x | **1.282x** | inside noise; "held" |
| `erlc` batch, 280 files | 0.997x | **1.007x** | parity |
| `erlc` per-file, 280 files | 2.216x | 1.861x | **not comparable across dates** |
| ESTONE total measured time | — | **regressed 29%** | see the regression section |

## System and builds

- Host: Mac mini, Apple M4 (4 performance + 6 efficiency cores), 24 GB RAM,
  macOS 26.6.2, AC power.
- AtomVM `c669cb66fda6cb2b4dee3f6510866753706a82f7`, Release (`-O3`), SMP and
  JIT on, `AVM_DISABLE_JIT_DWARF=ON`.  Every artifact was rebuilt at this
  commit before measuring, including both AOT `.avm` images and the `erlc`
  executable.
- Reference: OTP 29, BEAM `emu_flavor=jit`, 10 schedulers, MacPorts
  `erlang @29.0.3_0` (installed 2026-07-17, unchanged since).  OTP source
  `8ce4b017ba3bad0a09ec067b14cf0b12a227022c`.
- `atomvm_erlc` at `ebe36b7`.

SHA-256 of the measured artifacts:

- `AtomVM`: `8db84bbfe45f3b233f895195e8d38196693e4079675a5550ca9e9c37a417265b`
- `atomvmlib-aarch64.avm`: `9dc865b45b05882a3a9652da8be2359fece5483eb2b3e601e40d4c8f257362a6`
- `benchmark-aarch64.avm`: `96d4ad6306e22242f672bd6eb0c43bef07caecaa3f57749d4113c21796577478`
- `estone-aarch64.avm`: `5a7daa8d841bc1f9c17da6678dd86374f30a51d27fe215cd3b354c3547258529`
- `erlc-atomvm-aot`: `9915865151e7d2c1f73481a5a1e691a2130980f2eb5511a8df55ce2016c52d49`

Raw samples: `build.jit.rebase/publication/{estone,app,erlc}-2026-09-06.json`
and `build.ab/estone-aug-vs-sep.json`.  Drivers
[`bench_publication.py`](bench_publication.py) and
[`bench_ab_atomvm.py`](bench_ab_atomvm.py).

Method is unchanged from the August document: every round interleaves the two
engines and reverses their order on alternate rounds, timing wraps the complete
OS process, and a source is counted only when both compilers emit its `.beam`.

### Measurement conditions

The machine was recovering from concurrent Time Machine, XProtect and Photos
analysis when the session began; load average peaked at 17.  Artifacts were
rebuilt while it drained and measurement started at load ~2 with one steady
background process.  **BEAM's own absolute ESTONE score is the control**: 2,536k
here against 2,561k in August, so the machine was not materially degraded.  The
app aggregate is still the least trustworthy figure of the three.

## ESTONE common subset — 0.907x

- BEAM median **2,536,137 ESTONES**, AtomVM median **2,301,158**.
- AtomVM/BEAM **0.907x**, paired bootstrap 95% interval **0.889–0.920**.

| component | Aug | now | recoverable estones |
|---|---:|---:|---:|
| Bif dispatch | 0.61x | 0.65x | **+111k** |
| pattern matching | 0.76x | 0.92x | +98k |
| Small Integer arithmetic | 0.33x | 0.61x | +96k |
| small/medium/huge messages | — | 0.39–0.46x | +47k |
| Links | 0.06x | 0.34x | +39k |
| Binary handling | — | 0.80x | +4k |
| Function calls | 1.37x | **1.97x** | −128k (ahead) |
| list manipulation | 1.51x | 1.42x | −41k (ahead) |
| traverse | 0.88x | 1.17x | −16k (ahead) |
| Alloc and dealloc | 1.15x | 1.15x | −12k (ahead) |

Ranked by absolute estones rather than by ratio, **BIF dispatch is now the
largest single item**, and it is the one that barely moved while everything
around it improved.  Pattern matching and small-integer arithmetic follow.

The ESTONES score is a weak instrument and should not be quoted alone.  Its
heaviest component, pattern matching, completes in 0–1 ms per run, so its
estones figure is set by timer resolution: single runs of the same binary have
produced 424k and 1,067k.  The 31-round median controls this, but only just.

## Message copying, `c669cb66f` — and a measurement lesson

The score improved while **ESTONE's total measured time got 29% worse**, which
is what prompted this section. BEAM's total was flat across the two dates
(0.5054 s -> 0.5085 s); AtomVM's went 0.734 s -> 0.986 s, and per-component
milliseconds put all of it in medium (+165 ms) and small (+52 ms) messages.

**Most of that difference was code layout, not the change.** Comparing separate
builds attributes their layout differences to whatever source change happens to
separate them, and ESTONE's message micros are unusually sensitive to it: four
builds that all contain the same message-path source ran 772, 792, 918 and
926 ms — a 154 ms spread. Two of them differed only in call-target addresses,
verified by disassembling `mailbox_message_create_from_term` in both and finding
identical instruction sequences.

The controlled form of the question is one source tree, one-line variants,
rebuilt in place. Over 27 rotating rounds:

| ESTONE component | hint disabled | hint, unconditional stores | hint, conditional stores |
|---|---:|---:|---:|
| medium messages | 249 ms | 290 ms | 290 ms |
| small messages | 315 ms | 299 ms | 301 ms |
| **total measured time** | **768 ms** | **791 ms** | **792 ms** |

So the size hint added in `c669cb66f` **costs about 24 ms (3%) here**: it wins
16 ms on small messages and loses 41 ms on medium ones. That is a real effect
and a far smaller one than the cross-build comparison suggested. It also runs
against the dedicated round-trip probes that justified the change, which
measured 1.076x and 1.154x on a long-lived mailbox receiving one shape
repeatedly. The two are consistent: ESTONE's `msgp` builds a fresh four-process
ring per iteration and gives each mailbox only 100 messages, so a per-mailbox
hint barely amortises, while a long-lived gen_server-style mailbox is exactly
the case it is built for. Whether to keep it is a workload judgement, and it is
left open here.

An earlier revision of this document attributed the whole 29% to that commit and
root-caused it to the hint fields sharing a cache line with `outer_first`. The
struct facts are right — `sizeof(Mailbox)` is 64, `outer_first` at offset 0, the
hint fields at 48/56/60, cache line 128 bytes — but making both stores
conditional on the value changing is worth 791 ms -> 792 ms, i.e. nothing. That
change was written, measured, found to do nothing, and dropped rather than
committed.

**Rule for this benchmark from now on:** attribute a message-micro difference to
a source change only from a controlled build pair — same tree, minimal diff,
rebuilt in place — and treat a cross-build difference below ~20% as
uninformative.

## Benchmark app — 1.282x

Aggregate of the 11 base tests: **86.9 ms BEAM vs 67.8 ms AtomVM**.  The move
from 1.255x is within the ±3% noise floor for this suite, so this is "held",
not "improved".

| test | BEAM | AtomVM | ratio |
|---|---:|---:|---:|
| sudoku_solution_test | 884 us | 425 us | **2.08x** |
| list_test | 14,491 us | 8,012 us | **1.81x** |
| pingpong_speed_test | 38,402 us | 21,709 us | **1.77x** |
| prng_test | 364 us | 233 us | **1.56x** |
| map_test | 1,337 us | 1,049 us | **1.27x** |
| pi_test | 6,614 us | 7,293 us | 0.91x |
| prime_speed_test | 3,167 us | 3,464 us | 0.91x |
| crypto_test | 1,735 us | 1,979 us | 0.88x |
| sudoku_puzzle_test | 17,815 us | 20,751 us | 0.86x |
| bigint_test | 1,656 us | 2,232 us | 0.74x |
| binary_test | 460 us | 640 us | 0.72x |

The loss column is unchanged from August; nothing that landed since addressed
it.  Note that `pingpong_speed_test`, which is message round trips between two
processes, went 22,102 us -> 21,709 us — it does **not** show the regression
above, because two processes do not reproduce the four-process ring's sharing.

## `erlc` over the OTP corpus

Corpus: OTP 29 `compiler`, `stdlib`, `kernel`, `sasl`, `crypto` — 280 files.
Both compilers built all 280; there are no exclusions.

### One application per process (batch)

| application | files | BEAM | AtomVM | ratio |
|---|---:|---:|---:|---:|
| compiler | 59 | 7.534 s | 7.338 s | 1.027x |
| stdlib | 98 | 14.859 s | 15.201 s | **0.978x** |
| kernel | 104 | 6.548 s | 6.447 s | 1.016x |
| sasl | 17 | 0.966 s | 0.854 s | **1.131x** |
| crypto | 2 | 0.470 s | 0.323 s | **1.455x** |
| **total** | **280** | **30.377 s** | **30.162 s** | **1.007x** |

Round ratios span 0.995–1.009x.  `stdlib` is still the one real loss, as it was
in August, and it is still the module set dominated by large ordered-map work.

### One file per process (per-file)

| application | files | BEAM | AtomVM | ratio |
|---|---:|---:|---:|---:|
| compiler | 59 | 17.712 s | 9.961 s | 1.778x |
| stdlib | 98 | 31.932 s | 19.957 s | 1.600x |
| kernel | 104 | 23.907 s | 10.235 s | 2.336x |
| sasl | 17 | 3.592 s | 1.289 s | 2.787x |
| crypto | 2 | 0.605 s | 0.345 s | 1.751x |
| **total** | **280** | **77.748 s** | **41.787 s** | **1.861x** |

AtomVM won 279/280 files; only `stdlib/unicode_util.erl` lost (0.841x).  The
median individual ratio was 2.261x.

**This number must not be compared with August's 2.216x.**  Between the two
runs AtomVM's per-file total was flat (40.88 s -> 41.79 s) while BEAM's fell
15% (90.59 s -> 77.75 s), and it fell uniformly: the today/August ratio has
median 0.845, p90 0.961 and max 1.020, so essentially every one of the 280
files got faster on BEAM's side.  Ruled out: an OTP change (binaries untouched
since July), the `erlc` compile server (no process, `ERLC_USE_SERVER` unset)
and disk pressure (155 GiB free).  The cause was not identified; it is a
system-state difference in BEAM *startup*, and per-file mode is startup-
dominated.  Bare startup on a trivial file, measured the same day: BEAM
122.5 ms against AtomVM 9.9 ms.

Use **batch mode for any cross-date comparison**.  Per-file remains a valid
same-session, user-facing figure.

## Against the August gates

The August document proposed four gates.  Status:

1. Batch `erlc` >=1.10x on **each** application — **not met**.  `sasl` 1.131x
   and `crypto` 1.455x pass; `compiler` 1.027x, `kernel` 1.016x and `stdlib`
   0.978x do not.  The >2x per-file win and 100% corpus support are retained.
2. Runtime app >=1.20x aggregate — **met** at 1.282x.  "No base test below
   0.95x" is **not met**: six tests sit between 0.72x and 0.91x.
3. ESTONE >=1.00x — **not met** at 0.907x.  Of the four named components,
   pattern matching reaches 0.92x; BIF dispatch (0.65x), small-integer
   arithmetic (0.61x) and links (0.34x) do not.
4. No regressions — **not met**, see the message regression above.

## What the evidence says to do next

1. **Land the conditional-store fix.**  It is one commit, recovers ~85% of a
   shipped regression, and the remaining ~26% on medium messages is a
   cache-line-separation experiment worth running immediately after.
2. **Report ESTONE total measured time alongside the score, always.**  A 175 ms
   regression moved the score by less than its run-to-run spread.
3. **BIF dispatch is now the top ESTONE item** (+111k) and has resisted the
   work that moved everything around it.  It deserves its own investigation
   before more effort goes into the components already improving.
4. **Batch `stdlib`** remains the single blocking item for the `erlc` gate, and
   the August analysis still applies: large ordered-map lookup and comparison.
   The adaptive-map plan in that document has not been started.
