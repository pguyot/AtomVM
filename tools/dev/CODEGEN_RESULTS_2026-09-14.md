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
| aarch64 | 34,815,424 | 33,776,812 | **-2.98%** |
| xtensa | 29,376,531 | 29,132,802 | -0.83% |
| wasm32 | 35,773,933 | 35,519,386 | -0.71% |
| armv6m | 21,343,852 | 21,223,680 | -0.56% |
| riscv32 | 23,928,182 | 23,811,530 | -0.49% |
| arm32 | 31,611,848 | 31,458,788 | -0.48% |
| riscv64 | 26,403,328 | 26,286,784 | -0.44% |
| x86_64 | 31,720,999 | 31,607,718 | -0.36% |

aarch64 gets six times the rest because five of the eight changes only pay
where x0-x3 have home registers, which today is aarch64 alone. The other three
are frontend and every backend gets them; that is the -0.36% to -0.83%.

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

**ESTONE 1.0031**, over two seven-round sessions that agreed to within 0.02%
(1.0033 and 1.0031). The total barely moves because the micros that dominate
ESTONE's wall time here are C-bound. The ones that do list and tuple work move,
and agree across both sessions:

| micro | new/base | (earlier session) |
|---|---:|---:|
| lists | **0.923** | 0.961 |
| large_dataset_work | 0.957 | 0.995 |
| large_local_dataset_work | 0.969 | 0.987 |
| links | 0.981 | 1.026 |
| binary_h | 0.984 | 1.009 |
| ets | 0.984 | 0.996 |
| trav | 0.996 | 1.002 |
| fcalls | 1.000 | 1.001 |
| pattern | 0.997 | 0.999 |

`msgp` and `msgp_medium` are left out on purpose: they read 0.897/0.879 in the
first session and 0.999/1.049 in the second, on a machine that had background
work in between. They are the scheduler-bound micros this bench has always been
unable to resolve, and neither reading should be quoted.

## Performance, arm32

Building and measuring on `mx2.local` (Pi 2) against `avm-red2`, the arm32
baseline that already carries the r11 reduction pinning and the ldrd/strd
work. Expect little: five of the six changes are either frontend (which arm32
gets, and which is where its -0.48% comes from) or aarch64-only, and arm32 has
no home registers for the rest to exploit. Numbers to follow.

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
| 1 remainder: reg-to-reg movs | **5.98%** | Still the largest single category. Removing the rest needs the condition emitters to take a VM register operand, so the tag test can read x25 directly -- a change across every clause of `if_block_cond` and every backend. |
| 6: tag tests as one bit test | 2.08% | Real, but it trades safety: `tbnz` cannot separate boxed (0b10) from CP (0b00), so it relies on a CP never reaching a type test. BEAM assumes exactly this, but our current form rejects a CP correctly and the new one would dereference it. Worth a deliberate decision rather than a quiet one. |
| 7: branch peephole | 0.58% inverted pairs + 0.98% chains | Smaller than the audit claimed -- see the trap below. The chain collapse wants a label-alias map threaded through every backend's branch resolution. |
| 8: outline slow paths | 0 | Size-neutral by construction: the code is still emitted, just elsewhere. It is an I-cache change and needs a different measurement. |
| 9: post-indexed heap stores | small | Pairs with the store half of finding 3; both belong in one go at `put_tuple2`/`put_list`. |
| 10: `i_mul_add` | small | Lowest value of the ten, unchanged. |

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
