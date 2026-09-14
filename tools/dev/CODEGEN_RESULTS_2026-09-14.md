<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Acting on the codegen audit: what landed and what it bought — 2026-09-14

Companion to [CODEGEN_AUDIT_2026-09-14.md](CODEGEN_AUDIT_2026-09-14.md), which
listed ten differences between our generated code and BeamAsm's. Six changes
landed, covering five of the ten findings. This records what they bought and
what the other five are actually worth, now that they have been sized properly.

## Code size, all eight backends

Native code for a 260-module OTP corpus (stdlib + compiler + kernel),
`tools/dev` harness, label jump table excluded since it is not code and its
entry size differs per backend.

| backend | before | after | change |
|---|---:|---:|---:|
| aarch64 | 34,815,424 | 33,181,268 | **-4.69%** |
| xtensa | 29,376,531 | 29,132,802 | -0.83% |
| wasm32 | 35,773,933 | 35,519,386 | -0.71% |
| armv6m | 21,343,852 | 21,223,680 | -0.56% |
| riscv32 | 23,928,182 | 23,811,530 | -0.49% |
| arm32 | 31,611,848 | 31,458,788 | -0.48% |
| riscv64 | 26,403,328 | 26,286,784 | -0.44% |
| x86_64 | 31,720,999 | 31,607,718 | -0.36% |

aarch64 gets six to thirteen times the rest because seven of the ten changes
only pay where x0-x3 have home registers, which today is aarch64 alone. The
other three are frontend and every backend gets them; that is the -0.36% to
-0.83%. The seven other backends are byte-identical before and after the last
two changes, which is the check that they really are aarch64-only.

## What the code looks like now

`codegen_probe:tup4/1` — take four fields out of a tuple and build a new one —
on the real (non-DWARF) AArch64 stream. This is the destructure-and-rebuild
only; the type test and heap check are excluded.

```
before (34 instructions)             after (18, of which 4 are dead nops)
mov x7, x25                          and  x7, x25, #~3
and x7, x7, #~3                      ldp  x26, x27, [x7, #8]
ldr x8, [x7, #8]                     nop ; nop
mov x26, x8                          ldp  x28, x25, [x7, #24]
str x8, [x21, #96]                   nop ; nop
... three more times ...             mov  x7, x22 ; add x22, x22, #0x28
mov x7, x22 ; add x22, x22, #0x28    mov  x8, #0x100 ; str x8, [x7]
mov x8, #0x100 ; str x8, [x7]        str  x25, [x7, #8]
mov x8, x25 ; str x8, [x7, #8]       str  x28, [x7, #16]
... three more pairs ...             str  x27, [x7, #24]
orr x7, x7, #2                       str  x26, [x7, #32]
mov x25, x7 ; str x7, [x21, #88]     orr  x7, x7, #2
                                     mov  x25, x7 ; str x7, [x21, #88]
```

BeamAsm does the same work in 8. The four fields now cost exactly what they
cost there (`and` plus two `ldp`); the rebuild is still 11 against its 5,
because it pairs its stores with `stp` and bumps the heap inside them
(findings 3 and 9 on the store side, not done).

## Performance, AArch64

Seven interleaved ESTONE rounds against the pre-audit baseline (`ae29b507a`).

**ESTONE 1.0111.** The first eight changes were worth 1.0031, consistent across
two seven-round sessions; the bit test and the in-place test operand took it to
1.0111. The total is held down by the micros that dominate ESTONE's wall time
here being C-bound. The ones that do list and tuple work:

| micro | new/base | (8 changes) | (earlier session) |
|---|---:|---:|---:|
| lists | **0.923** | 0.923 | 0.961 |
| large_dataset_work | 0.973 | 0.957 | 0.995 |
| large_local_dataset_work | 0.982 | 0.969 | 0.987 |
| pattern | 0.984 | 0.997 | 0.999 |
| ets | 0.988 | 0.984 | 0.996 |
| binary_h | 0.994 | 0.984 | 1.009 |
| fcalls | 1.000 | 1.000 | 1.001 |

`msgp` and `msgp_medium` are left out on purpose: across three sessions on the
same binaries they have read 0.879, 1.049 and 1.028. They are the
scheduler-bound micros this bench has never been able to resolve, and no
reading of them should be quoted. `trav` (1.031) and `links` (1.025) moved
within the same band on a run where nothing touching them changed.

## Performance, arm32

**No measurable change, which is the expected answer.** Five interleaved ESTONE
rounds on `mx2.local` against `avm-red2` -- the arm32 baseline that already
carries the r11 reduction pinning and the ldrd/strd work:

| build | ESTONE | vs GRiSP |
|---|---:|---:|
| `avm-red2` (baseline) | 48,365 | 0.765 |
| `avm-cg` (this work) | 48,179 | 0.762 |
| GRiSP OTP 29 arm32 JIT | 63,246 | 1.000 |

0.9962, inside the noise. Five of the eight changes cannot reach arm32 at all
(no home registers), and the three that can are worth -0.48% of code size,
which is far below what this board can resolve: the per-round spread is
39,559-52,088 for the baseline alone, and the board was throttled to 600 MHz
throughout (`get_throttled` 0x50005, the same under-voltage documented in
ARM32_CATCHUP).

The per-micro table is not worth quoting either. Its two largest movers are
`bif_dispatch` (1.221) and `large_local_dataset_work` (1.171) -- two of the
four micros that the throttling window lands on -- and the next two are `msgp`
and `msgp_medium`, the scheduler-bound pair. Nothing in it is attributable.

Getting arm32 to move needs the changes it cannot currently reach: home
registers for x0-x3, which is the same blocker as the rest of finding 1 and
the reason `supports_loop_residency` is still false there.

## What landed

1. **Compare a tuple's whole header word** (`6f545d425`). A tuple header is the
   arity shifted above six tag bits and the tuple tag is 0, so a header IS the
   arity shifted up: `cmp x8, #256` replaces mask, branch, shift, compare. Only
   when both tests fail to the same label; BEAM keeps an
   `i_is_tuple_of_arity_ff` for the case where they do not, and so do we.
2. **Strip the boxed tag once per run of tuple reads** (`11abc65e9`). The
   existing fusions need the reads adjacent to the type test, and a `test_heap`
   between them -- which the compiler emits whenever the clause builds anything
   -- ends the run. BEAM re-establishes the pointer with `load_tuple_ptr`; now
   so do we.
3. **Load array elements into their home register** (`caccd4e50`) instead of a
   scratch plus a `mov`.
4. **End the deferred-store window at a return** (`76eb0ebd1`). Only x0 is live
   across a return, so the stores to everything else are dead. Costs nothing in
   size -- an elided store becomes a nop of the same width -- but takes the
   memory traffic out.
5. **Stop copying values that are already in a register** (`cf799c5dd`).
   `and_to_native_register/3` is the three-operand form the assembler always
   had and the backend interface did not, so stripping a tag is now
   `and x7, x25, -4` rather than `mov` then `and`; and a store reads its source
   straight from wherever it already is.
6. **Read two adjacent fields with one `ldp`** (`a6697cbd4`), BEAM's
   `get_two_tuple_elements`.
7. **Store x0-x3 to a y register or pointer straight from the home**
   (`29762db79`). Same argument as the array store, applied to the other store
   path; worth 1.8% of `erl_scan` on its own.
9. **Test a primary tag with one bit test** (`06dcb0e67`). Of the three tags a
   term can carry, BOXED is the only one with bit 0 clear and LIST the only one
   with bit 1 clear, so either test is one `tbz`/`tbnz`. This is an assumption,
   not a free win: it holds only because a CP never reaches a type test, which
   is exactly what BEAM's JIT assumes. Worth 2.03% of `erl_scan` against the
   2.08% the census predicted.
10. **Let a condition test a value where it already lives** (`a689cbf5d`), so a
   type test reads the x0-x3 home instead of a copy. A tuple type test is now
   `tbnz`/`and`/`ldr`/`cmp`/`b.ne` -- instruction for instruction what BeamAsm
   emits, down from eleven.
8. **Compute into the home register rather than a scratch** (`b2cf0b6ac`).
   `with_temp/3` emitted into a scratch and then copied, so every load or
   immediate landing in x0-x3 cost a mov. This one is a trade: the scratch used
   to keep the *source's* cache entry alive, so a later read of it was free. Net
   -0.98% on `erl_scan`, so the trade pays, and the two tests guarding the
   cache-reuse property now exercise it above x3 where the scratch still
   exists.

## What did not land, and what it is worth

Measured on the real AArch64 stream for `erl_scan` (86,537 instructions), which
is where these should be judged rather than in the abstract:

| finding | size on this module | why not done |
|---|---:|---|
| 1 remainder: scratch-to-scratch movs | 2.17% | The home-register copies are gone; what is left never touched a home. |
| 7: branch peephole | 0.58% inverted pairs + 0.98% chains | Smaller than the audit claimed -- see the trap below. The chain collapse wants a label-alias map threaded through every backend's branch resolution. |
| 8: outline slow paths | 0 | Size-neutral by construction: the code is still emitted, just elsewhere. It is an I-cache change and needs a different measurement. |
| 9: post-indexed heap stores | small | Pairs with the store half of finding 3; both belong in one go at `put_tuple2`/`put_list`. |
| 10: `i_mul_add` | small | Lowest value of the ten, unchanged. |

Findings 6 and the rest of 1 landed after this table was first written; the
two together took aarch64 from -2.98% to -4.69% and ESTONE from 1.0031 to
1.0111. Everything above still only reaches arm32 once x0-x3 have homes there.

## Two measurement traps, both of which caught me

**The audit's own numbers were taken on DWARF output, which is worse code.**
`jit_dwarf` does not export `committed_offset/1`, and that is what
`backtrack_enabled/1` checks before it will emit a fused forward conditional
branch. So a DWARF dump shows `b.cond skip ; b target` everywhere the real
build emits one `b.cond target`. Measured on DWARF output, inverted branch
pairs looked like 4.32% of `erl_scan`; on the stream the build actually uses
they are 0.58%. The audit overstated finding 7 sevenfold, and its absolute
instruction counts for `tup4` and `lst` were inflated too. The omission looks
deliberate -- rewinding the stream would desync the DWARF tables -- so the fix
is to know it, not to "fix" it. `tools/dev/jit_dwdump.sh` now says so.

**An elided store never shows up in code size.** The deferred-store machinery
replaces a dead store with a nop of the same width, so finding 4 measures as
exactly 0.00% on every backend while removing four stores per tuple rebuild.
It is a memory-traffic win, and the nops still cost fetch and decode -- visible
in the listing above. Compacting them out would need the two-pass sizing to
know the store is dead, which is a real follow-up.

And one plain bug, for the record: `jit_aarch64_asm:ldp/4` takes `{Base}` and
is **post-indexed** -- it writes the bumped address back to the base register.
`ldp/3` takes `{Base, Imm}` and is the offset form. Using the wrong one made
every field after the first pair read from the wrong place; the Erlang suite
caught it, and then I re-ran a `test-erlang` I had not rebuilt and believed the
stale pass for a while.
