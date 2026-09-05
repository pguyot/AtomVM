<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Estone/JIT performance ideas — measured — 2026-09-05

Companion to [ESTONE_JIT_PERFORMANCE_HANDOVER.md](ESTONE_JIT_PERFORMANCE_HANDOVER.md),
whose eight ideas were explicitly unmeasured hypotheses.  Every one of them was
either measured directly, measured through an ablation, or refuted by a static
census.  Three survive.

## Method

- Host: Apple M4 Mac mini, macOS 26.6.2, AArch64, AC power.
- Baseline: `bf01b6b4d` on `w30/jit-edge`; Release `-O3`, SMP and JIT enabled;
  benchmark modules AOT-precompiled for `aarch64`; OTP 29 for the toolchain.
- A/B driver: [`bench_ab_atomvm.py`](bench_ab_atomvm.py) rotates the two engine
  snapshots each round so drift is shared; medians with a paired bootstrap 95%
  CI.  Microbenchmarks go through [`bench_micro_ab.py`](bench_micro_ab.py),
  which precompiles the same module with each snapshot's `jit` beams and also
  reports the native code-size delta.
- **Noise floor**, measured by running the identical snapshot against itself:
  ESTONE total ±1.3%, ESTONE per-component up to ±3.4%, application-suite
  aggregate ±3%.  `prime_speed_test` is bimodal (2.0–6.3 ms) and
  `pingpong_speed_test` is ±5%; neither can carry a small result on its own.
- **The ESTONES score is a poor instrument here.**  Its `pattern` component
  alone is ~47% of the total, and several components report `0ms`, so the score
  is dominated by timer quantisation.  Total measured time is reported
  alongside it and is the more stable of the two.

Where an idea proposes bringing to one backend a mechanism another backend
already has, it was measured by *ablating* that mechanism on aarch64.  That
turns "how much would arm32 gain" into a question this machine can answer.

## Verdicts

| # | Idea | Verdict | Evidence |
|---|---|---|---|
| 1 | Eliminate list reconstruction | **drop** | OTP's compiler already does it |
| 2 | Shift tagged small integers directly | **keep — shipped** | 1.20–1.25x on shift loops |
| 3 | Solve arithmetic guards backwards | **drop** | 0 instances in 2367 beam files |
| 4 | Inline 32-bit allocation | **kept — shipped** | arm32 1.021x, riscv64 1.045x |
| 5 | Caller-saved contracts for call-free loops | **drop** | ablation: 0.0% where it exists |
| 6 | Restricted native ABI for tiny leaves | **drop** | ≤14.5% ceiling, leaf ineligible |
| 7 | Avoid message sizing | **keep — real but costly** | sizing is 9.2–17.2% of a round trip |
| 8 | Coalesce readiness notifications | **drop** | census: ~0.4% ceiling on ESTONE |

## Idea 1 — reconstruction of a list just destructured — DROP

The handover asked for this to be checked in compiled artifacts first.  It was.
Compiling the benchmark port's `estone.erl` with OTP 29 `erlc -S`:

| loop | `put_list` in the recursive clause |
|---|---|
| `pat_loop1` | 1 (`{put_list,{integer,0},{x,2},{x,1}}`) |
| `pat_loop2` | 0 |
| `pat_loop3` | 0 |
| `pat_loop4` | 0 |
| `pat_loop5` | 0 |

`pat_loop2` compiles its `pat_loop2(I-1, [X, Y | Tail])` to a bare
`{call_only,2,{f,118}}` with `x1` untouched — the reconstruction the idea wants
to remove is not emitted.  The single surviving construction is in `pat_loop1`,
and it is exactly the case the handover flagged as needing a proof that the
first element is `0`, which is not available.  There is no redundant allocation
here to remove.

## Idea 2 — shift tagged small integers directly — KEEP, SHIPPED

The identity holds for any integer and any shift below the word width.  With
`t = v*16+15` and `v = q*2^S + r`:

```text
t >> S  ==  q*16 + (16*r + 15) div 2^S,   and the second term is in [0, 15]
```

so `or 15` rewrites it to `tag(q)`.  Arithmetic shift keeps this true for
negative `v`.  Four sites in `jit.erl` dropped their `lsl #4`, and the
runtime-amount `bsr` path also dropped the `add #4` that biased the shift
count:

- `op_gc_bif2_bsr` range-typed inline path,
- `op_gc_bif2_shift_lit_runtime` `bsr`,
- `op_gc_bif2_shift_reg_runtime2` `bsr`, both the in-range and the
  saturating (`asr #63`) branches.

Correctness was checked by running 30 operand values (0, ±1..±17, both small
integer boundaries, `1 bsl 62`, `1 bsl 70`, `16#7FFF…`, a bignum, an atom, a
string, floats, a binary, `[]`, a tuple) against 22 shift amounts (0..5, 30,
58..65, 100, 1000, negative, bignum, atom, float) plus the literal-amount and
range-typed paths — 1414 results, **identical to OTP 29** after normalising two
pre-existing AtomVM differences unrelated to shifting (`~p` float/string
formatting, and AtomVM raising `overflow` where BEAM raises `system_limit`).
`jit_tests` (207) and `test-erlang` pass.

| workload | baseline | changed | speedup |
|---|---:|---:|---:|
| `bsr` by a literal, unknown range | 2,890 us | 2,408 us | **1.200x** |
| `bsr` by a runtime amount | 2,863 us | 2,345 us | **1.221x** |
| `bsr` by a literal, range-typed inline | 2,557 us | 2,047 us | **1.249x** |

Whole-suite effect is at the noise floor, because neither suite shifts much:
ESTONE total time 1.020x (CI 0.988–1.065), application aggregate 0.994x
(CI 0.978–1.016).  `atomvmlib-aarch64.avm` shrinks by 1,460 bytes.

Kept: it is strictly less code doing strictly less work, and the loops that do
shift gain a fifth.

## Idea 3 — solve arithmetic guards backwards — DROP

The pattern does survive OTP's frontend — `pat_loop2` compiles to two `bsl`
gc_bifs feeding an `is_ne_exact` and a `select_val`.  A hand-solved version of
that loop (`Y bsl 1 == 0` → `Y == 0`, `Y bsl 2 == 4` → `Y == 1`) compiles to
exactly what the transformation would emit and runs **1.131x** faster
(4,190 us → 3,706 us).

That is the whole win, and it does not generalise.  A census over 2,367 beam
files (all of OTP 29's `lib` plus AtomVM's own libraries) counted every
constant-amount shift whose result is immediately tested against a constant:

| shift | constant-amount occurrences | immediately compared to a constant |
|---|---:|---:|
| `bsl` | 1,034 | **0** |
| `bsr` | 1,096 | 264 |

Not one `bsl` in the corpus matches, and `bsl` is the only case the rule solves
to a single exact test.  For `bsr` the preimage of a constant is an interval, so
the transformation trades one shift plus one compare for two compares — a wash
on a backend that already inlines the shift in two instructions.  `pat_loop2` is
the only instance, it is worth ~0.1% of ESTONE, and it is contrived.

## Idea 4 — inline 32-bit allocation — KEEP, HIGHEST VALUE

Confirmed absent: `jit_arm32`, `jit_riscv32`, `jit_riscv64`, `jit_armv6m` and
`jit_wasm32` export no `heap_bump_alloc/2`, so both `OP_PUT_LIST` and
`alloc_tuple` fall back to `PRIM_PUT_LIST` / `PRIM_TERM_ALLOC_TUPLE`.

To size the prize, the export was *removed* from `jit_aarch64` and the whole
tree rebuilt, putting aarch64 in the position those backends are in today:

| application test | without inline alloc | with | speedup |
|---|---:|---:|---:|
| `prime_speed_test` | 4,589 us | 3,769 us | 1.218x |
| `sudoku_puzzle_test` | 24,398 us | 20,610 us | 1.184x |
| `sudoku_solution_test` | 480 us | 421 us | 1.140x |
| `list_test` | 8,770 us | 8,073 us | 1.086x |
| `pingpong_speed_test` | 23,517 us | 22,447 us | 1.048x |
| **aggregate** | **75.3 ms** | **68.9 ms** | **1.093x** (CI 1.046–1.103) |

ESTONE score 1.100x (CI 1.083–1.126); ESTONE total time is unchanged, which is
what the score/time divergence noted above predicts.  `atomvmlib-aarch64.avm`
is also **1.58% smaller** with inlining on.

The handover's staging is right: load HP from `Context`, store the fields,
write HP back; a pinned heap register is an optimisation, not a prerequisite.
`alloc_tuple` is gated on the same export, so implementing it buys
`put_tuple2` as well as `put_list`.

### Implemented, and what it actually measured

`heap_bump_alloc/2` now exists on `jit_arm32` and, through
`jit_riscv_impl.hrl`, on `jit_riscv32` and `jit_riscv64`.  Measured directly
on the two backends this machine can execute, with `qemu-user` inside a Linux
container, seven interleaved rounds of the application suite:

| target | aggregate | best test | native code |
|---|---:|---:|---:|
| arm32 | **1.021x** | `sudoku_puzzle_test` 1.074x | −1.15% |
| riscv64 | **1.045x** | `sudoku_puzzle_test` 1.109x | −1.12% |
| riscv32 | not executable here | — | −0.60% |

**The aarch64 ablation over-predicted this**, and the reason is worth
recording: on aarch64 hp is pinned, so falling back to the primitive costs a
write-back and a reload of the pinned register *on top of* the call.  arm32
and RISC-V keep hp in the context, so their fallback never paid that, and the
saving is only the call itself.  An ablation of a pinned-register backend is
an upper bound for an unpinned one, not an estimate.

`qemu-user` timing weights instruction count rather than microarchitecture, so
it understates what removing an indirect call is worth on real in-order
hardware; treat 1.02x/1.05x as a floor.  Both backends produce byte-identical
output to OTP 29 on an allocation stress test (cons and tuple shapes, eight
live values at the allocation site, arities from 1 to 1024 — past every
backend's add-immediate range — and repeated GCs), and pass the 528-module
`test-erlang` suite with exactly the failures the unmodified backends have.

Not done here: `armv6m`, `armv7m`, `xtensa` and `wasm32` still call the
primitive.  armv6m in particular has severe register pressure and a literal
pool whose reach is already marginal — see the note below — so it needs its
own measurement rather than a copy of this patch.

### Unrelated defect found while testing

`jit_armv6m` fails to compile a `put_tuple2` of some wide arities built at run
time: a 511-element tuple gives `function_clause` in `jit_armv6m_asm:ldr/2`
with `{pc, 2576}`, past Thumb-1's 1020-byte pc-relative reach.  It reproduces
on the unmodified backend, is not monotonic in arity (511 fails, 512 and 1024
do not), and is unrelated to this change; it is the same class as the arm32
literal-pool bug fixed in `227f07ac2`.

## Idea 5 — caller-saved contracts for call-free loops — DROP

Ablating aarch64's `supports_loop_residency` measures what the mechanism is
worth on the one backend that has it:

| instrument | residency off | residency on | ratio |
|---|---:|---:|---:|
| ESTONE total time | 932.8 ms | 934.0 ms | 0.999x |
| ESTONE score | 2,308,016 | 2,293,816 | 0.994x |
| application aggregate | 69.2 ms | 70.6 ms | 0.980x (CI 0.954–1.024) |
| three tight arithmetic loops | — | — | 0.997–1.000x |

Zero, everywhere, and the tight loops' native code is **byte-identical** with
and without it, so they do not even trigger residency.  The handover's variant
(private caller-saved assignments across call-free backedges) is not the same
mechanism, but there is no measured loop on this machine that would pay for it,
and it is a large change with the reconciliation and shared-tail-block hazards
the handover itself lists.  Do not start it without a workload that first shows
loads and moves on a backedge dominating a real profile.

## Idea 6 — restricted native ABI for tiny local leaves — DROP

Two independent reasons.

**The ceiling is small.**  Keeping `int_arith`'s exact call structure (63
non-tail local calls per iteration) and varying only the leaf body:

| leaf | time |
|---|---:|
| estone's `do_arith2` | 9,039 us |
| `f(I) -> I.` | 1,313 us |

So calls plus the surrounding subtraction chain are at most 14.5% of the micro,
and a cheaper call sequence recovers only part of that.  `Small Integer
arithmetic` is ~3% of ESTONE at the noise floor of the whole suite.

**The motivating leaf is ineligible.**  `do_arith2` contains `*`, `div` and two
`bsl` on unbounded ranges; it can allocate a bignum and it can raise.  It does
not meet the "cannot allocate, collect, trap, or call Erlang" contract the idea
is built on, so on the benchmark that motivates it the fast entry would never
be taken.

## Idea 7 — avoid message sizing — KEEP, real but the costliest to build

Confirmed: `mailbox_message_create_from_term` still sizes with
`memory_estimate_shallow_usage` before copying with `memory_copy_shallow`, two
traversals of the same term.

A `sample(1)` profile of 3,000,000 round trips of estone's medium message
attributes **86 of 529 samples (16.3%)** to `memory_estimate_shallow` and 78
(14.7%) to `memory_copy_shallow`, with the malloc family at ~16%.

A first ablation that skipped sizing and allocated a fixed 128 words made things
*slower* (0.671x on the small shape) — the allocator's size class dominates, so
"skip the sizing" is only a win if the exact size is still known.  That is
precisely what idea 7 proposes, so the ceiling was measured instead by adding a
*second*, discarded sizing pass, leaving allocation untouched: the delta is the
marginal cost of exactly one pass.

| message shape | one pass | two passes | one pass costs | ceiling |
|---|---:|---:|---:|---:|
| `{Self, {message, {Self, true}}}` | 69,436 us | 75,822 us | 9.2% | **1.101x** |
| `{Self, {message, {Self, funky_stuff, baby, {1, [123, true, []], "abcdef"}}}}` | 91,926 us | 107,741 us | 17.2% | **1.208x** |

Real and worth having — `pingpong_speed_test` is 31% of the application
aggregate — but it is the only surviving idea that needs generated code and the
runtime to agree on a contract, and the fixed-size ablation above shows the
allocator will punish an approximate answer.  The aggressive extension
(constructing a non-escaping envelope directly in message storage) would also
attack the ~16% spent in malloc, at the cost of much stronger escape reasoning.

## Idea 8 — coalesce readiness notifications — DROP

Instrumented `scheduler_make_ready` with counters for total calls, calls where
`Ready` was already set, and ready-list entries walked by
`scheduler_has_runnable_ready`:

| workload | calls | already `Ready` | ready entries walked |
|---|---:|---:|---:|
| ESTONE | 2,148,719 | 212,965 (9.9%) | 291,256 (0.14 per call) |
| application suite | 400,457 | 185 (0.046%) | 713 (0.002 per call) |

The redundancy is real on ESTONE but small elsewhere, and the O(n) walk the idea
would avoid is not O(n) in practice — the ready list is almost always empty.
What a coalescing early return saves per skipped call is two list-pointer
updates: roughly 0.4% of ESTONE and nothing measurable on the application suite,
both under the noise floor.  The spinlock is still needed to read the flags
safely, so the only way to get more would be the unsynchronised `Ready` test the
handover itself warns can lose a wakeup.

The state machine does support the early return (`Ready` is set and cleared only
under `processes_spinlock`, and `scheduler_wait` preserves the "Ready implies in
`ready_processes`" invariant), so it is available as a tidiness change.  It is
not a performance change.

## Recommended sequence

1. ~~**Idea 4** on `jit_arm32` and `jit_riscv32`~~ — done, measured above.
   `jit_armv6m`, `jit_armv7m`, `jit_xtensa` and `jit_wasm32` remain.
2. **Idea 7**, sized at 1.10–1.21x on message-passing micros, if the
   generated-code size contract can be kept cheap.
3. The `jit_armv6m` literal-pool failure on wide `put_tuple2`, which this
   work uncovered but did not cause.
4. Nothing else in the handover survived measurement.
