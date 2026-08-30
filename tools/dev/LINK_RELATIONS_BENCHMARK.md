<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Link relations and message copying — 2026-08-30

Three changes to process relations and message delivery, measured against the
unmodified VM built from the same tree.

1. Local link lookup goes through one `context_find_link` entry point, and a
   process promotes it from the heterogeneous monitor list to an open-addressed
   PID index once it holds eight local links.  Below that, one tagged word acts
   as a Bloom filter over the local links, so an absent relation is rejected
   without walking anything.
2. `link/1` and `unlink/1` answer from the caller's own relations before taking
   the target's process-table lock or allocating link halves, which is what
   their idempotence allows.
3. A monitor is sent to its target as an intrusive mailbox signal rather than
   inside a separately allocated signal wrapper, removing one allocation and
   one free per relation.
4. Shallow message terms — built out of immediates, lists and tuples nested no
   deeper than eight — are copied in one recursive pass instead of the general
   estimate-then-scan traversal.

## Builds and method

- Host: Apple M4 Mac mini, macOS 26.6.2, AArch64, AC power.
- Source baseline: `abc35e567` on `w30/jit-edge`; Release `-O3`, SMP and JIT
  enabled; benchmark modules AOT-precompiled for `aarch64`.
- Reference: OTP 29, BEAM JIT, ten schedulers.
- The unmodified executable was kept aside and every focused round rotates
  among BEAM, unmodified AtomVM and changed AtomVM.  Timings come from
  `erlang:monotonic_time(microsecond)` around the Erlang loop, so VM startup is
  excluded.  31 measured rounds after three warmups.
- Focused driver: [`bench_links.py`](bench_links.py), workload
  [`link_bench.erl`](link_bench.erl).
- ESTONE and the application suite are 31 and 21 interleaved rounds with the
  local publication driver, on the common-subset ESTONE port (no `port_io`, no
  ETS on either engine).

## Link relations

Medians in microseconds, lower is better.  `A/B` is unmodified time divided by
changed time.

| workload | BEAM | unmodified | changed | A/B | changed/BEAM |
|---|---:|---:|---:|---:|---:|
| duplicate `link`, 100,000 | 742 | 3,585 | 683 | **5.25x** | 1.09x |
| absent `unlink`, 100,000 | 439 | 943 | 525 | **1.80x** | 0.84x |
| create 256 unique links | 23 | 312 | 234 | **1.33x** | 0.10x |
| remove 256 unique links | 14 | 460 | 390 | **1.18x** | 0.04x |
| spawn-link-stop, 500 children | 1,735 | 4,716 | 4,294 | **1.10x** | 0.40x |

Every workload improves and repeated `link/1` now beats BEAM.  New relation
creation does not: AtomVM remains about 10x slower than BEAM on a 256-link
fan-out.  Lookup is no longer the reason.  What is left is the two-sided
protocol — two heap allocations, target lookup and locking, signal delivery,
asynchronous installation and the unlink acknowledgement round trip.

The ESTONE Links component moves from 0.064x to 0.35x BEAM.  That loop links
processes that are already linked, so it mostly measures the idempotent fast
path; the focused unique-link and churn workloads above are what kept the
design honest.

## Message copying

The one-pass copy was measured on the two ESTONE message shapes, 200,000 round
trips per shape between two processes:

| message | unmodified | changed | speedup |
|---|---:|---:|---:|
| `{Self, {message, {Pid, true}}}` | 45,945 us | 37,507 us | **1.22x** |
| `{Self, {message, {Pid, funky_stuff, baby, {1, [123, true, []], "abcdef"}}}}` | 78,300 us | 52,530 us | **1.49x** |

The application suite's ping-pong test improves from 22,102 us to 20,391 us
(1.78x BEAM).

ESTONE's `msgp` and `msgp_medium` components move the other way, from 0.523x
and 0.741x down to 0.413x each.  Those two micros spawn four processes per
iteration and pass 100 messages down a five-process chain; sampling the VM
while they run puts the overwhelming majority of samples in `__psynch_cvwait`
and `__psynch_mutexwait`, not in copying.  They measure the scheduler
sleep/wake protocol, and speeding up the producer changes how often schedulers
park.  A build with only the shallow copy disabled restores those two
components exactly while keeping every other result, which is what identifies
the interaction.  Fixing it belongs to the per-scheduler run-queue work, not
here.

## Totals

| result | unmodified | changed |
|---|---:|---:|
| ESTONE common subset, AtomVM/BEAM | 0.761x | **0.770x** |
| application aggregate, AtomVM/BEAM | 1.255x | 1.243x |

The application aggregate is dominated by `prime_speed_test` and
`sudoku_puzzle_test`, whose run-to-run spread on this host exceeds the
difference between the two columns; a control run of the unmodified executable
in the same session measured 1.220x.  Neither total moved materially.

## Measured and rejected

**Inlining the `link/1`/`unlink/1` fast path into generated AArch64 code.**  A
prototype recognised `erlang:link/1` and `erlang:unlink/1` at `call_ext` and
emitted the filter test inline, skipping the NIF entirely on a hit.  Rotating
A/B of the two AOT images on one VM binary:

| workload | without | with | speedup |
|---|---:|---:|---:|
| duplicate `link` | 622 us | 202 us | 3.08x |
| absent `unlink` | 575 us | 205 us | 2.80x |
| unique link, 256 | 246 us | 241 us | 1.02x |
| unique unlink, 256 | 399 us | 372 us | 1.07x |
| supervisor churn, 500 | 4,515 us | 4,526 us | **1.00x** |

It only pays when a program calls `link/1` or `unlink/1` redundantly in a tight
loop, which is what the ESTONE micro does and what no real program does.  The
cost was around 150 lines of hand-encoded AArch64 in one backend plus
link-specific special-casing in the shared `emit_pass`, duplicating a C
predicate at a hardcoded `Context` offset.  Not kept.

**Caching the most recently added link in the high half of the filter word.**
Worth 1.19x on the repeated-`link/1` loop and nothing measurable elsewhere.  It
gave the state word a second, word-size-dependent role, and dropping it lets
the filter use every bit, so it is not kept either.

## Next

Lookup is done; the remaining link gap is the protocol.  Instrument the stages
of `link/1` — target lookup and locking, the two allocations, signal delivery
and the acknowledgement — then prototype a local-only symmetric install and
removal under ordered per-process locks, keeping the signal path for remote and
contended cases.  Pair it with a pooled relation store so the win also shows up
as lower peak RAM and less fragmentation on the MCU ports, which is where two
general-purpose allocator blocks per relation actually hurt.
