<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Vector registers and bulk moves: what BeamAsm does, and what is worth copying

2026-09-14, branch `w30/jit-edge`. Prompted by "I've seen BeamAsm using vector
registers for faster fused moves. Are we doing this as well on aarch64?"

## Where BeamAsm actually uses SIMD

Two places in `erts/emulator/beam/jit/arm/instr_common.cpp`, and nowhere else.
There is no vector-register "fused move" of x or y registers to copy.

1. `emit_init_yregs` -- `movi v0.2d, -1` then `stp q0, q0`, filling four stack
   slots per instruction.
2. `emit_copy_words_increment` -- `ldp/stp q30, q31`, four words per pair. Its
   callers are all `update_record`'s copy fallback.

(`instr_float.cpp` uses d registers, but that is just the FPU.)

## init_yregs: the vector register is the wrong tool for us

We emitted two instructions per slot, re-materialising NIL every time:

    mov x7, #0x3b ; str x7, [x23] ; mov x7, #0x3b ; str x7, [x23, #8]

Census of `init_yregs` over the OTP-29 lib tree, by length of consecutive run:

    1 slot   27740      5 slots   466      9 slots    48
    2 slots   6239      6 slots   250     10 slots    32
    3 slots   2423      7 slots   138     ... down to a single 57-slot run
    4 slots   1040      8 slots    76

59145 slots over 34600 sites. Scoring the three lowerings in instructions:

    today, two per slot                       118290
    hoist NIL per site, plain paired stores    79120
    hoist NIL per site, q-register quad stores 108133

The distribution is overwhelmingly short runs, so a per-site vector setup costs
more than the wider store saves. BEAM can afford it because its NIL is all-ones
and `movi` alone builds it; AtomVM's NIL is 0x3b, which would need a `dup` from
a general register on top.

Shipped the non-SIMD form (commit fa95911f4): hoist the constant once per site
on every backend, and pair consecutive slots with `stp x7, x7` on aarch64. Two
slots go from four instructions to two.

wasm32 is a stack machine where `i32.const 59` and `local.get n` are both two
bytes, so hoisting bought nothing per use and still paid for the `local.set`
(+0.04%). It opts out through the new `constants_are_free/0` and stays
byte-identical.

## update_record: the real prize

`emit_pass_update_record` (the non-`inplace` path) rebuilds the whole record and
copied it a word at a time -- confirmed in disassembly as `ldr x10, [x7, #N]`
then `str x10, [x8, #N]`, over and over. Census over the same corpus: 7989
non-inplace sites, 121837 words copied, record sizes commonly 12 to 62.

    today, ldr+str per word                   243674
    x-register pairs (ldp/stp, ldrd/strd)     125594
    q-register quads                           66894

Unlike init_yregs the runs here are long, so the wider access has something to
work with. This is the largest single block of instructions I have found in our
output.

### aarch64: q registers measured, and rejected

The scoring above says q registers should nearly halve the paired version. They
do not, because of alignment. Our copy starts at word 1 of the record, and both
the source (an untagged boxed pointer) and the destination (hp) are only 8-byte
granular, so the base is arbitrary mod 16. Measured on Apple Silicon, ns per
copy, best of 7:

    12-word record        base 16B-aligned   base 8 mod 16
      one word at a time        1.507            1.535
      x pairs                   0.742            1.304
      q pairs                   0.734            1.268

    32-word record        base 16B-aligned   base 8 mod 16
      one word at a time        3.894            4.017
      x pairs                   2.145            3.256
      q pairs                   2.212            3.416

q pairs never beat x pairs -- slightly worse at both sizes, aligned or not. The
load/store unit already moves 16 bytes per pair operation whatever the register
width, and the 16-byte-crossing penalty hits the wider access harder. The
aarch64 dispatch block already declares `v0-v7` and `v16-v31` clobbered, so the
boundary contract was not the obstacle; there is simply no win to collect.

Shipped `ldp`/`stp` of x registers. A 9-word record goes from 18 instructions
to 10.

### arm32: ldrd is the trap, ldm/stm is the answer

`ldrd`/`strd` look like the obvious analogue, and that is what I shipped first
(commit 72a091c30). Measured on a Cortex-A7 (Pi 2 at 600 MHz), 32-word copy,
cycles, over the four alignment combinations of source and destination:

                           both 8B  both 4mod8  skewed  skewed   average
    one word at a time       46.10     46.10     46.10   46.10     46.10
    ldrd/strd                26.70     47.44     36.75   36.75     36.91
    ldm/stm, 2 registers     36.08     47.44     46.77   46.77     44.27
    ldm/stm, 3 registers     33.41     34.74     34.74   33.40     34.07
    ldm/stm, 4 registers     26.73     37.42     32.07   32.07     32.07

`ldrd` is the fastest form when both ends happen to be doubleword aligned and
*slower than scalar* when neither is -- and on a 32-bit heap that is one case in
four each way. `ldm`/`stm` are nearly alignment-insensitive.

There is also a correctness argument. `ldrd` at a 4-mod-8 address is
UNPREDICTABLE on ARMv6, and the `arm32` backend is selected for any `arm*`
processor, ARM1176 included. `ldm`/`stm` need only word alignment. GCC makes
exactly this distinction: it emits `ldrd`/`strd` for an 8-byte-aligned copy and
`ldm`/`stm` for a word-aligned one, on both `-march=armv6` and `-march=armv7-a`.

Shipped the three-register `ldm`/`stm` form: the copy streams off two scratch
bases with writeback. Three and not four because of the register budget --
`SrcReg` is live past the copy (the reuse path returns it as the result), so
after the two bases only three scratch registers remain. Moving the reuse-flag
materialisation to after the copy is what freed the third.

**Open, and not introduced here: `get_list_head_tail` emits `ldrd` at a cons
pointer, which has the same ARMv6 exposure.** Unnoticed because every arm32 CI
job is `-mcpu=cortex-a7`, which is ARMv7-A, where word-aligned `ldrd` is merely
slow rather than unpredictable.

### armv6m: the same lever, and the backend where it matters most

ARMv6-M has no `ldrd` at all -- it is Thumb-only, and all its accesses must be
word aligned -- so the alignment question above does not arise. What it does
have is Thumb-1 `LDMIA`/`STMIA` with writeback, low registers only, which is the
whole allocatable file on this backend. Code size is the metric that matters on
a microcontroller, and one word at a time costs four bytes per word.

Registers are the constraint. Only six are allocatable (`r7 r6 r5 r4 r3 r1`),
and `SrcReg` and `DestReg` are both live across the copy, leaving four. Spending
two on stream bases would leave two data registers. Instead the source streams
in `SrcReg` itself and is wound back afterwards with a single `subs`, which buys
a third data register -- the difference between two words per pair of
instructions and three. That is safe as long as the rewind is exact, which is
what the new test below is for.

A 9-word record: 18 instructions before, 9 after (two setup, six `ldmia`/`stmia`,
one rewind). Corpus code size -0.25%.

Note that the ARMv6 hazard described above is about ARMv6-**A** (ARM1176,
Raspberry Pi 1 and Zero), reached through the `arm32` backend. It has nothing to
do with the `armv6m` backend, which cannot emit `ldrd` in the first place.

## Testing

`update_record`'s rebuild path had no dedicated test -- `test_update_record_inplace`
covers only the other hint. `tests/erlang_tests/test_update_record_rebuild.erl`
now covers it, and the rp2 firmware test suite compiles the same module so it
runs on emulated Cortex-M0+ under rp2040js.

Two things were needed to make it a real test.

1. **An identity function is not an opaque barrier.** It gets inlined, the record
   is then a known literal, and an update to a field that already holds that
   value folds away entirely. Every site came out `copy`-hinted, and `copy`
   never reads the source pointer after the copy -- so the rewind was untested.
   Round-tripping through the process dictionary gives the compiler a term it
   knows nothing about; every site is then `reuse`-hinted, which is the path
   that does read it.
2. **Sizes have to straddle the register group.** The records are 3, 5, 19, 20
   and 21 words, which covers every tail length past a whole group of three and
   two runs shorter than one group.

Verified by mutation: shortening the armv6m rewind by one word makes
`test_update_record_rebuild` fail under rp2040js (crash report, "Expected 139
Was 843"), and restoring it makes it pass. Before the two fixes above, the same
mutation passed.

## Results

Corpus code size, 260 modules, jump table excluded, over both commits:

    arm32  -0.44%   aarch64 -0.39%   armv6m  -0.31%   riscv32 -0.12%
    riscv64 -0.10%  xtensa  -0.07%   x86_64  -0.07%   wasm32   0.00%

The four backends with no multi-word load take the frontend fallback for
update_record and only see the init_yregs change; wasm32 sees neither.

End to end on a loop of two updates to a 31-word record, each through an opaque
call so neither can go in place. Same VM binary on each side; only the
precompiled native code of the benchmark module differs, which isolates the
codegen exactly. Interleaved, median:

    aarch64 (Apple Silicon, 15 runs)   11512 us -> 10163 us   1.1327x
    arm32   (Cortex-A7, 11 runs)      337269 us -> 318810 us  1.0579x

The arm32 figure is smaller than the 1.35x the instruction-level measurement
predicts for the copy, and that is the expected shape: on a Pi 2 the rest of the
VM is proportionally much slower (small caches, memory-bound, throttled to
600 MHz), so the copy is a smaller share of the total. Both sides produce the
same checksum, and `test-erlang` passes natively on the Pi with the new codegen.

A caution on the arm32 number: taken while a build was running on the same
machine it read 1.015x. The 1.0579x above is with the machine quiescent and the
benchmark pinned to one core. Always check `vcgencmd measure_clock arm` --
`scaling_cur_freq` does not show this part throttling.

## Method notes

- Opcode censuses: `beam_disasm:file/1` over `/opt/local/lib/erlang/lib`.
  Scripts in the session scratchpad (`yregs_census.erl`, `urec.erl`).
- Read our own lowering with `tools/dev/jit_dwdump.sh <target> mod.beam`, and
  BeamAsm's with `erl +JDdump true`.
- **llvm-objdump picks a restricted subtarget for the arm32 ELF** and prints
  `<unknown>` for ordinary ARM encodings -- `ldrd`, `strd`, and plain `bx`. Use
  `OBJDUMP=arm-none-eabi-objdump`. This cost real time: the first arm32 dump
  looked like a bad encoder when the encoder was correct.
- The `*_asm_tests` comparison against a real assembler is **vacuous under
  AtomVM** (`erlang:system_info(machine)` is `"ATOM"`, and `asm/3` returns the
  input unchanged). Run them on host BEAM with binutils on PATH to get the
  cross-check; verify it is live by feeding a deliberately wrong expectation.

---

# Part 2: term construction (put_tuple2 / put_list)

The first half of this document was about *copying* runs of words. Construction
is the bigger target and was unpaired everywhere.

Census over the OTP-29 corpus:

    put_tuple2   80235 sites   286617 stores   ->  174451 paired
    put_list     81650 sites   163300 stores   ->   81650 paired
    total                      449917          ->  256101   (-43%)

That is ~194k instructions, more than the ~118k the update_record copy saved.
Tuple arities are heavily concentrated: 55614 sites at arity 2 and 14982 at
arity 3, so most of it is a single pair plus a remainder.

Shipped on aarch64: `stp` for both. A cons cell is now one instruction instead
of two, and a 5-tuple's block went from 8 instructions to 5.

    aarch64 corpus code size  -0.70%   (the largest single change this session)

Every other backend is byte-identical -- none of them has the capability.

## The speed result: none, and the reason

**On aarch64 this is a code-size optimisation only.** Two measurements agree.

An interleaved A/B over a construction-heavy loop (20 cons cells and three
tuples per iteration, 15 runs, same VM binary with only the benchmark's
precompiled native code differing):

    baseline 37783 us   paired 37662 us   1.0032x

And the instruction itself, on Apple Silicon:

    6-word tuple:   6 x str  0.694 ns     3 x stp  0.698 ns
    cons cell:      2 x str  0.680 ns     1 x stp  0.684 ns

`stp` is not faster than `str`. Both forms move the same number of bytes, and
the store unit is bandwidth-limited rather than issue-limited, so removing the
instruction removes nothing that was on the critical path. This is the same
shape as the q-register result in Part 1, and for the same underlying reason.

The change is kept for the code size: AtomVM ships precompiled native code, and
-0.70% of it is worth having when it costs nothing.

## Why it does not extend to arm32 or armv6m

The instructions exist on both (`stm`, `strd`), but measured on a Cortex-A7,
above the loop floor:

    16 cons cells (32 words)      32 x str    36.00 cycles
                                  16 x stm{2} 34.74
                                  16 x strd   31.04
    10 arity-2 tuples (30 words)  30 x str    24.20 cycles
                                  10 x stm{3} 29.40   <- worse than plain stores

`stm` has a per-instruction overhead that only amortises with a wide register
list, and a wide list is exactly what term construction cannot offer: arity 2
dominates. `strd` does win on cons cells, but it carries the ARMv6 alignment
hazard documented in Part 1 and a 32-bit heap is only word aligned.

Two further constraints make it worse than the measurements suggest:

  - `stm`'s register list is a bitmask, so it always stores in ascending
    register-number order. For a copy we choose both ends; for construction the
    values are wherever they already are, so half the time the order is wrong
    and fixing it costs the `mov` the pairing saved.
  - Thumb-1 `STM` always writes back, so the tuple pointer needed afterwards
    has to be copied first -- one extra instruction, which for the dominant
    arity-2 case cancels the entire saving (3 stores becomes mov + stm + str).

So the answer for these two backends is not "not yet", it is "no".

## x86_64: the one candidate left, and it is a different mechanism

x86_64 has no GPR pair store, so BEAM does something else there: when two
consecutive tuple elements are *adjacent in memory* it moves both with one
16-byte SSE load and one store (`vmovups`), and when they are adjacent but
reversed it loads-and-swaps with `vpermilpd`. Our x86_64 has no x-home
registers, so every element is loaded from `ctx->x[]` -- and the corpus is full
of tuples built from consecutive x registers, which is exactly the case it
detects. Our 5-tuple is 10 instructions where BEAM's is 6.

That is not the same trade as `stp`: it halves the number of memory *operations*
rather than just the instruction count, so the bandwidth argument above does not
dispose of it. It is worth doing -- but there is no x86_64 hardware here to
measure it on (Rosetta gives correct results and meaningless timings), so it
should be measured on a real machine before being trusted.

---

# Part 3: x86_64, and four instruction-family evaluations

## x86_64 SSE pairing (shipped, commit 9f70da643)

Measured and validated; see the commit message. A 5-tuple is 10 instructions
before and 7 after, an arity-2 tuple 5 and 3. Corpus size -0.09%, which
understates it: `movups` is a byte longer than the `mov` pair it replaces, so
the real win -- half the memory operations -- does not appear in a byte count.

Validated under Rosetta against a matched baseline (pairing on vs off, identical
11-test failure set, all explained by the mbedtls-less build). **Both sides were
verified by looking for `movups` bytes in a precompiled beam before the run was
trusted.** The ninja dependency from `libs/jit` to the precompiled test beams
does not fire reliably; it produced a "passing" result from stale codegen twice.
`rm -rf <build>/tests/erlang_tests/<arch>` before any such A/B.

## ubfm / sbfm / bfm

Counting uses, BeamAsm against us: `ubfx` 8/0, `bfi` 7/0, `ubfiz` 3/0,
`sbfx` 2/0. We emit `lsl`/`lsr` (which are themselves UBFM aliases) 11 times
each.

Census of adjacent instruction pairs over 96209 instructions of compiled
`lists` and `maps`:

    529   asr + orr      asr x9, x9, #63  |  orr x8, x8, x9
    305   and + lsr      and x7, x7, #0xffffff  |  lsr x7, x7, #2
    232   lsl + add      lsl x7, x7, #6  |  add x7, x7, #0xb

  - `and` + `lsr` is a textbook `ubfx x7, x7, #2, #22`. **305 sites, worth
    doing.**
  - `asr` + `orr` is not a bitfield op but *is* a single instruction: aarch64
    ALU ops take a shifted register operand, so this is
    `orr x8, x8, x9, asr #63`. **529 sites, the largest of the three, and it
    shortens a dependency chain rather than just removing an instruction.**
  - `lsl` + `add` is header construction, `(arity << 6) | tag`. `bfi` could do
    it in one only if a register already held the tag, which would cost the
    `mov` it saved. **Not worth it.**

So the real finding here is not the bitfield family specifically but the
**shifted-register operand form**, which we never emit and which covers the
largest pair.

## stp with the zero register

**No.** Over the same corpus there are 48 `str xzr` in total, all isolated
stores to the same slot. There is no run of zero words to pair: the thing we
write in runs is NIL (0x3b), not zero, which is why init_yregs materialises a
register for it in the first place.

## cset / csetm / csel for reducing branches

Measured on **non-DWARF** output, because `jit_dwdump.sh` splits fused
conditional branches and would have inflated this badly.

93062 instructions contain 3104 forward conditional branches, of which 1218
skip only 1-3 instructions. That looked like the opportunity. It is not --
sampling what they skip:

    784   tbnz +1  ||  b <far>              long-branch trampoline
    264   tbnz +2  ||  mov x0, x0 ; ret     conditional return
    131   tbnz +2  ||  and x0, #~3 ; br x0  conditional indirect branch
     27   branch pairs

**None of these are value selection.** They are all control flow, so `csel`
does not apply to any of them. Our JIT emits VM-level control flow as control
flow; the "compute one of two values" shape that `csel` exists for barely
occurs. Scanning the frontend: of 64 `if_else_block` sites, **2** have both arms
moving to the same destination, which is the only shape `csel` could collapse.
BeamAsm's 12 uses are hand-written at specific sites, one of which is
`update_record`'s reuse choice -- the same site is one of our 2.

Worth noting as a side effect: those 784 trampolines are a direct cost of the
cold-arm outlining, which moved fallback code to the module tail and put it out
of `tbnz`'s +/-32KB range. That trade still looks right on size, but it is where
the two-instruction branches come from.

## arm32 condition flags

The arm32 backend already uses predication heavily: about 5580 predicated
non-branch instructions in 86355. Counting literal condition atoms in the
backend source finds only 13, which is misleading -- most call sites pass the
condition in a variable.

Remaining opportunity: 5537 forward conditional branches, of which 717 skip 1-3
instructions (0.83% of all instructions) and could become predicated
instructions instead. That is smaller than it looks, and ARM deprecated wide
predication in ARMv8 for a reason: on an out-of-order ARMv7-A core a
well-predicted short branch costs about what the predicated instructions would.
**Low priority.**

## Ranking what is left

    529   orr/add/sub with a shifted register operand   (aarch64, dependency chain)
    327   consecutive str pairs still unpaired          (aarch64, size only)
    305   ubfx for and+lsr                              (aarch64)
    291   consecutive ldr pairs still unpaired          (aarch64, ldp)
      2   csel-shaped if_else_block sites               (aarch64)
      0   stp xzr                                       (no opportunity)

Given that `stp` measured as a pure size win, the two pairing rows should be
expected to behave the same way. The shifted-register and `ubfx` rows are the
ones that remove work from a dependency chain rather than just instruction
slots, so they are the ones worth measuring for speed.
