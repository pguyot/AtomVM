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
