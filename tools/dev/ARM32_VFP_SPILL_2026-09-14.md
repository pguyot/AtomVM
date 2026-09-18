<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Can arm32 use VFP registers as x-register homes?

2026-09-14, measured on mx2.local (Raspberry Pi 2, Cortex-A7, 32-bit, running at
600 MHz -- the part throttles and `scaling_cur_freq` lies about it, see
`vcgencmd measure_clock arm`).

## Why the question

aarch64 pins x0-x3 to callee-saved integer registers x25-x28 (`?X_HOME_COUNT` =
4). arm32 has no x homes at all and no integer register left to give one: r7 is
ctx, r8 is e, r9 is prims, r10 is jit_state, r11 became the reduction counter,
r12 is `?IP_REG`. Every read of x0 is an `ldr` from `ctx->x[]` and every write an
`str`. VFP offers 32 more registers that the integer allocator is not competing
for, so the question is whether `vmov` is cheap enough to make them homes.

## What was measured

`vfphome.c` / `vfp2.c`. Each benchmark is a hand-written asm loop modelling a
block that reads x0 four times and writes it once -- the shape a home register
exists to serve. Best of 5, pinned to one core.

    block of 4 reads + 1 write of x0        cycles/iter
      memory (what arm32 emits today)          14.05
      VFP home, write-through to memory        15.05
      VFP home, flushed only at boundaries      9.36
      integer register home (what aarch64 gets) 4.68

    dependent chain, one read per iteration
      read from ctx->x[]                        5.02
      read from a VFP register                  3.34

    read x0 and x1 together, twice
      two ldr fused into ldrd                   4.68
      vmov r4, r5, d0  (both halves at once)    4.68

## Results

**A VFP home has to be lazily flushed to be worth anything.** The first variant
stores to `ctx->x[]` on every write *and* keeps the VFP copy in sync; that is
slower than today's plain memory (15.05 vs 14.05). All the value is in not
touching memory inside the block, which means adopting aarch64's contract:
the home is authoritative, and it is flushed at calls, GC points and exceptions.

**With lazy flushing it recovers half the gap, not all of it.** 9.36 cycles
against 14.05 today and 4.68 for a real integer register. Per x-register access:
`ldr`/`str` costs ~1.87 cycles more than a `mov`, `vmov` costs ~0.94 more. So a
VFP home is worth about 50% of an integer home, not 100%.

**The 2-in-1 read is not a win.** `vmov r4, r5, d0` pulls two x registers out of
one d register in a single instruction, but it measures exactly the same as the
`ldrd` we already emit (4.68 both). Both saturate the A7's two-issue front end;
there is no second win to collect on top of the pairing already shipped.

**Latency is better, throughput is not.** In an isolated dependent chain the VFP
read is genuinely faster (3.34 vs 5.02 cycles), which is the load-use latency the
A7 cannot hide when the consumer is the very next instruction. In the realistic
block the loads pipeline and most of that advantage disappears.

## Verdict

Not worth building. The reasons, in order:

1. Half the payoff of an integer home for all of the machinery -- a full
   flush-at-boundary contract, which is where the aarch64 home registers have
   historically produced their bugs.
2. d8-d15 (s16-s31) are the only VFP registers callee-saved under AAPCS, so they
   are the only ones that can survive a call to a C primitive. Using them means
   the JIT's own entry has to `vpush {d8-d11}` / `vpop`, paid on every entry, to
   honour the same ABI towards its C caller. That cost lands on exactly the short
   functions where the home would otherwise help most.
3. It would be the first thing in the arm32 backend to require an FPU.
   `jit_arm32:supports_fp(_State)` returns `false` today and
   `AVM_JIT_ARM32_FP_CLOBBERS` compiles to nothing when `__ARM_FP` is undefined,
   so a soft-float build currently works and would stop. Every arm32 CI job is
   `-mcpu=cortex-a7 -mfloat-abi=hard`, so that `#else` branch is never built and
   the breakage would not be caught here.

The arm32 x-home problem is still an integer register-pressure problem. If it is
worth revisiting, the lever is freeing an integer register (r12/`?IP_REG` is used
in only three places), not borrowing a floating-point one.
