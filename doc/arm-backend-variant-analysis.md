<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Analysis: ARMv7-M/ARMv8-M Backend Variant for AtomVM JIT

This document analyzes the benefits of adding a Thumb-2 variant to the existing
ARMv6-M JIT backend, targeting ARMv7-M and ARMv8-M cores found in most STM32
chipsets.

## Current State

The existing `jit_armv6m` backend targets the **Thumb-1 instruction set**
(ARMv6-M), the lowest common denominator across all ARM Cortex-M cores. This
ensures compatibility from the tiny Cortex-M0+ in STM32G0 to the Cortex-M33
in STM32H5/L5/U5, but at a significant cost in code size and complexity.

## The Mismatch: Most STM32 Chips Are NOT ARMv6-M

From `src/platforms/stm32/cmake/stm32_device.cmake`, out of 12 supported
STM32 families:

| ARMv6-M (Thumb-1 only) | ARMv7-M / ARMv7E-M (Thumb-2) | ARMv8-M (Thumb-2) |
|---|---|---|
| G0 (Cortex-M0+) | F2 (M3), F4 (M4), F7 (M7), G4 (M4), H7 (M7), L4 (M4), WB (M4) | H5 (M33), L5 (M33), U3 (M33), U5 (M33) |
| **1 family** | **7 families** | **4 families** |

**11 out of 12** STM32 families support Thumb-2 but currently run Thumb-1-only
JIT code. The RP2350 (Cortex-M33) on the Pico 2 is also ARMv8-M.

---

## What Thumb-2 Would Improve

### 1. Jump Table Entries: 12 bytes to 4 bytes (3x smaller)

The single biggest win. The current ARMv6-M jump table entry is **12 bytes**
(`jit_armv6m.erl:420-460`):

```
ldr r3, pc+4           (2 bytes)
push {r1,r4,r5,r6,r7,lr}  (2 bytes)
add pc, pc, r3         (2 bytes)
nop                    (2 bytes)  -- alignment padding
<32-bit offset>        (4 bytes)  -- literal data
```

With Thumb-2 `b.w` (32-bit branch, +/-16 MB range), this becomes **4 bytes**:

```
b.w offset             (4 bytes)
```

For a module with N labels, this saves N * 8 bytes. For a 100-label module,
that is 800 bytes in the jump table alone.

### 2. Immediate Loading: Up to 5 strategies to 2 instructions

The current `mov_immediate` function (`jit_armv6m.erl:3302-3354`) uses up to
5 different strategies to load a 32-bit constant, frequently falling back to a
**literal pool** (data embedded in the code stream):

| Value Range | ARMv6-M (Thumb-1) | ARMv7-M (Thumb-2) |
|---|---|---|
| 0-255 | `movs Rd, #imm` (2 bytes) | `movs Rd, #imm` (2 bytes) |
| 256-510 | `movs Rd, #255` + `adds Rd, #N` (4 bytes) | `movw Rd, #imm` (4 bytes) |
| 511-65535 | Shift trick or literal pool (4-8+ bytes) | `movw Rd, #imm` (4 bytes) |
| 65536+ | Literal pool: `ldr Rd, [pc]` + 4-byte data | `movw` + `movt` (8 bytes) |

The literal pool approach adds complexity:
- Each pooled constant adds 4 bytes of data plus a 2-byte `ldr` instruction
- The pool must be **flushed every ~512 bytes** because PC-relative `ldr` has
  only an 8-bit offset field (max 1020 bytes)
- Flushing inserts a branch to skip over the pool data, adding more code
- This is managed by `maybe_flush_literal_pool`/`flush_literal_pool` -- ~55
  lines of complexity that Thumb-2 could eliminate entirely

With `movw`/`movt`, any 32-bit constant loads in exactly 8 bytes, inline,
with no pool management.

### 3. Far Branches: 10-18 bytes to 4 bytes

ARMv6-M `b` (branch) reaches only **+/-2 KB**. For branches beyond that range,
the backend emits a "far branch" sequence (`jit_armv6m.erl:960-1035`):

- **With a free register**: 10-12 bytes (ldr + add + bx + alignment + literal)
- **Without free registers**: 16-18 bytes (push + ldr + mov + pop + add + bx +
  alignment + literal)

Thumb-2 `b.w` reaches **+/-16 MB** in a single 4-byte instruction,
eliminating:
- The far branch mechanism entirely (~75 lines in `branch_to_label_code`
  and `patch_branch`)
- Complex alignment-dependent size calculations
- The "have a temp register" vs "no registers free" code paths

### 4. Conditional Branches: +/-256 bytes to +/-1 MB

ARMv6-M conditional branches (`bcc`) reach only **+/-256 bytes**. The backend
handles out-of-range conditionals by inverting the condition and branching over
a far branch, doubling the code for distant conditional jumps.

Thumb-2 `b<cc>.w` reaches **+/-1 MB**, sufficient for any realistic function,
eliminating this pattern entirely.

### 5. Arithmetic With All 16 Registers

In Thumb-1, arithmetic instructions (`adds`, `subs`, `ands`, `orrs`, `eors`,
`lsls`, `lsrs`, `cmp`) are restricted to **r0-r7** (low registers). The
backend uses only 6 available registers: `[r7, r6, r5, r4, r3, r1]`.

Thumb-2 lifts this restriction for most operations, allowing:
- r8-r11 as general-purpose scratch/spill registers
- Reduced register pressure and fewer spill/reload sequences
- Simpler register allocation

### 6. Memory Access With Wider Offsets

Thumb-1 `ldr`/`str` with immediate offset supports only **0-124 bytes** (5-bit
field * 4). Context struct fields approaching this limit require workarounds.

Thumb-2 `ldr.w`/`str.w` support **12-bit unsigned immediate** offsets
(0-4095 bytes), covering the entire context struct directly.

### 7. Additional Instructions

Thumb-2 adds instructions that optimize common BEAM patterns:

- **`cbz`/`cbnz`**: Compare-and-branch-on-zero in 2 bytes. Useful for null
  checks and tag tests that are pervasive in BEAM code.
- **`tbb`/`tbh`**: Hardware branch tables for dispatch.
- **`bfi`/`bfc`**: Bit field insert/clear for term tag manipulation.
- **`clz`**: Count leading zeros.
- **`sdiv`/`udiv`**: Hardware division (M3+), avoiding a primitive call.
- **IT blocks**: Conditional execution of up to 4 instructions without
  branching, eliminating short conditional branch sequences.

### 8. DWARF Accuracy

The current DWARF generation (`jit_dwarf.erl:368-431`) hardcodes ARM EABI
attributes for ARMv6S-M with `THUMB_ISA_use = 1` (Thumb-1 only). A variant
would generate correct attributes for the actual target.

---

## Quantitative Code Size Estimate

For a module with N labels and M branch sites:

| Component | ARMv6-M | ARMv7-M | Savings |
|---|---|---|---|
| Jump table | N * 12 bytes | N * 4 bytes | N * 8 bytes |
| Far branches | M * ~12 bytes avg | M * 4 bytes | M * ~8 bytes |
| Literal pool overhead | Variable | 0 | Eliminates pool |
| Immediate loads | ~6 bytes avg + pool | 4-8 bytes inline | ~2 bytes + pool |

For a typical 100-label module with 50 far branches and 30 literal pool
entries, estimated savings: ~1.4 KB per module.

---

## Implementation Approach: Variant vs. Separate Backend

Two options exist:

1. **A new architecture** (`JIT_ARCH_ARMV7M = 6`) -- separate modules
2. **A variant flag** on the existing `armv6m` architecture

A **variant** within the `armv6m` backend is the pragmatic choice because:
- Same calling convention, register layout, context offsets, 32-bit word size
- Most backend code (register tracking, term encoding, VM operation sequences)
  is identical -- only the instruction encoding differs
- The existing variant mechanism (`?JIT_VARIANT_FLOAT32`) already shows the
  pattern
- A separate architecture would require duplicating the 4122-line backend

The variant approach would need:
- A new `?JIT_VARIANT_THUMB2` flag in `jit.hrl`
- Extended `jit_armv6m_asm.erl` with Thumb-2 encodings (or a companion module)
- Conditional paths in `jit_armv6m.erl` for `mov_immediate`,
  `branch_to_label_code`, `jump_table`, and `flush_literal_pool`
- Updated `stm32_device.cmake` and `rp2/CMakeLists.txt` to detect Thumb-2
  capable cores and set the variant
- Updated DWARF attribute generation

---

## Conclusion

Adding Thumb-2 support as a variant for ARMv7-M and ARMv8-M would provide:

1. **~3x smaller jump tables** (the most impactful single change)
2. **Elimination of the literal pool** mechanism (simpler code, smaller output)
3. **Elimination of far branch sequences** (smaller, faster branches)
4. **Wider immediate and offset ranges** (fewer multi-instruction sequences)
5. **More available registers** for arithmetic
6. **New instructions** (cbz/cbnz, IT blocks, hardware divide) for tighter code

These benefits apply to **11 out of 12** STM32 families and to the RP2350
(Pico 2). The only devices remaining on pure Thumb-1 would be the STM32G0
(Cortex-M0+) and the RP2040 (original Pico).
