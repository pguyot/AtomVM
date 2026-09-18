<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# JIT backend capability survey — 2026-09-18

`jit.erl` asks each backend what it can do with `erlang:function_exported/3`
and falls back to a generic sequence when the answer is no.  There are **58
such capabilities** and **8 backend modules**, all of them shipped
(`AVM_PRECOMPILED_TARGETS`).  This is what each one has, what the gaps cost,
and what to do about the gates themselves.

Which platform runs which backend:

| backend | platforms |
|---|---|
| `x86_64`, `aarch64` | generic_unix (desktop, CI) |
| `arm32` | generic_unix on 32-bit ARM (Pi, GRiSP) |
| `riscv64` | generic_unix on RISC-V |
| `armv6m` (+thumb2) | rp2 (RP2040/RP2350), stm32 |
| `riscv32` | rp2 (RP2350 Hazard3), esp32 (C3/C5/C6/C61/H2/P4) |
| `xtensa` | esp32 (ESP32/S2/S3) |
| `wasm32` | emscripten |

## Coverage

Generated from the compiled beams (`erlang:function_exported/3` on each
backend), not from grepping export lists.

### Present everywhere (2)

`shift_right_arith/3`, `supports_div/1`.

### Missing on one or two backends

| capability | missing on |
|---|---|
| `add_deferred_stub/2`, `take_deferred_stubs/1`, `pending_flush_mask/2`, `set_live_masks/2`, `supports_loop_residency/0` | wasm32 |
| `jump_to_label_cond/3` | wasm32, xtensa |
| `heap_bump_alloc/2` | wasm32, xtensa, armv6m |

### The two spellings of overflow checking (4/8 each)

| capability | present on |
|---|---|
| `add_overflow/3`, `sub_overflow/3` | x86_64, aarch64, arm32, armv6m |
| `add_overflow_check/3` | riscv64, riscv32, wasm32, xtensa |
| `mul_overflow/3` | x86_64, aarch64, arm32 |
| `mul_overflow_check/3` | riscv64, riscv32 |

### The branch-relaxation cluster (4/8)

`set_branch_hints/2`, `take_overflows/1`, `rewind_stream/2` — present on
aarch64, riscv64, riscv32, armv6m; **missing on x86_64, arm32, wasm32,
xtensa**.  `labels/1`, `set_preset_labels/2`, `enable_eager_flush/1` are
riscv64 + riscv32 only (they serve the flash stream).

### The inline fast paths (3/8 or fewer)

| capability | present on | fallback when absent |
|---|---|---|
| `allocate_frame_fast/2` | x86_64, aarch64, arm32 | `PRIM_ALLOCATE` call per `allocate` |
| `call_ext_with_cp_direct/4`, `call_ext_last_direct/5`, `call_primitive_direct/3` | x86_64, aarch64, arm32 | round trip through the scheduler loop |
| `call_fun_with_cp_direct/3` | x86_64, aarch64 | same |
| `get_list_head_tail/4` | x86_64, aarch64, arm32 | two loads, one temp, first destination evicted |
| `read_heap_fragments/1` | x86_64, aarch64, arm32 | — |
| `supports_select_val_ranges/0` | x86_64, aarch64, arm32 | a compare chain instead of sub + unsigned bound test |
| `read_shrink_probe_mismatch/1` | x86_64, aarch64 | `PRIM_TEST_HEAP` call at every `test_heap` |
| `compare_stub_call/3`, `map_get_stub_call/3` | x86_64, aarch64 | C comparator / map lookup call |
| `jump_table_dispatch/1` | x86_64, aarch64 | binary search over `select_val` |
| `get_cp_base/1`, `return_cross_module/2` | x86_64, aarch64 | primitive call on return |
| `move_imported_gcbif_to_native_register/3` | x86_64, aarch64 | `PRIM_GET_IMPORTED_GCBIF` per call site |
| `move_imported_bif_to_native_register/2` | **x86_64 only** | `PRIM_GET_IMPORTED_BIF` per call site |
| `term_from_float_inline/2` | **x86_64 only** | `PRIM_TERM_FROM_FLOAT` per boxed float |
| `move_float_to_fp_reg/3`, `load_be_unsigned/3`, `store_be/4`, `supports_inline_tuple2_eq/0`, `shift_right_arith_reg/3`, `add_deferred_raise/5`, `take_deferred_raises/1`, `move_to_array_elements_pair/5` | x86_64, aarch64 | generic sequence |

### Genuinely ISA-shaped (1/8)

`or_shifted_arith/4`, `extract_bits/4` (`ubfx`), `and_to_native_register/3`,
`can_test_in_place/1`, `get_array_elements_pair/3`,
`move_array_elements_pair/5`, `move_to_vm_registers_pair/4` (aarch64
`ldp`/`stp`); `pairs_memory_adjacent_only/0` (x86_64); `constants_are_free/0`
and `supports_tail_cache/0` (wasm32); `return_to_cp_address/1` (arm32's
two-word cp); `addsub_smallint_fused/4` (aarch64, arm32 — needs a
conditional-compare, and x86_64 measured the split form as better).

## Should the gates go?

**No, and not for speed.**  The per-site `erlang:function_exported/3` is free
in practice: memoizing all 74 probe sites in the process dictionary and
recompiling `erl_parse.beam` for x86_64 made the compile **0.5–1% slower**
across three interleaved rounds (6.76s/6.79s, 6.77s/6.79s, 6.87s/6.94s).  The
export-table lookup costs less than a dictionary hit, so hoisting the probes
into the compile state would buy nothing.

What the gates *do* cost is comprehension and drift: a backend silently misses
a fast path and nobody notices until someone benchmarks that target.  Two
concrete fixes, neither of which removes a gate:

1. **A capability test that lists, per backend, which gates it answers no
   to.**  The matrix above took a script to produce; it should be a test
   whose expected value is checked in, so adding a backend or losing a
   capability shows up as a diff.  Keep it as a data file, not an assertion,
   since "no" is legitimate.
2. **`_Static_assert` the 32-bit *heap* offsets.**  `jit.c` asserts the
   Context and JITState offsets for both widths — except the heap group
   (`heap.root`, `heap.heap_ptr`, `heap.heap_end`, `shrink_probe_heap_end`,
   `cp_base`), which sits under `#if TERM_BYTES == 8`.  arm32 and riscv32
   nonetheless hard-code `heap.root` at `0x4` and `heap.heap_ptr` at `0xC` for
   their inline allocation paths, with nothing checking them — the shape of
   bug that silently drifted the wasm32 offsets during the generational-GC
   work.  The true 32-bit values, probed with a wasm32 compile, are `0x4`,
   `0xC`, `0x10`, `0xDC` and `0x14`; asserting them turns a runtime-only,
   layout-dependent bug into a compile error.

Two gates *can* be simplified:

- `shift_right_arith/3` and `supports_div/1` are exported by all 8 backends,
  so their `function_exported` probes are dead.  `supports_div/1` still has to
  be *called* (it takes the state and can answer no per target variant), but
  the probe around it can go.
- `add_overflow/3` vs `add_overflow_check/3` (and the `mul_` pair) is not a
  capability, it is two spellings of one operation: flag-setting ISAs against
  flagless ones, and every backend implements exactly one.  A single
  `add_checked/3` returning either a flag condition or a result register would
  delete four probes and one branch in `jit.erl` for every arithmetic site.

## What to extend, in order

Ranked by evidence and by how many shipped targets benefit.  Everything here
is portable: no entry needs an instruction the target lacks.

1. **`read_shrink_probe_mismatch/1` + lifting the `word_size() =:= 8` gate**
   (missing: arm32, riscv32, riscv64, xtensa, armv6m, wasm32).  Enabling this
   on x86_64 today was **+10.7% ESTONE and +3.1% on the benchmark app**
   ([BENCHMARK_X86_64_2026-09-18.md](BENCHMARK_X86_64_2026-09-18.md)), and the
   inline check is four ALU ops.  On the MCU targets a saved call is worth
   relatively more.  The 32-bit half needs the Context offsets the 64-bit
   backends have asserted — which is item 2 of the previous section.
2. **`heap_bump_alloc/2`** (missing: xtensa, armv6m, wasm32).  It turns
   `put_list`, `put_tuple2` and every `alloc_tuple` from a primitive call into
   a load/add/store; those two opcodes are among the most frequent in compiler
   output.
3. **The direct-call family** (`call_ext_with_cp_direct`,
   `call_ext_last_direct`, `call_primitive_direct`; missing on riscv32,
   riscv64, xtensa, armv6m, wasm32).  When arm32 got it (`c6ebd0a33`) the
   ESTONE `fcalls` micro went **402,887 us -> 221,087 us**, from 4.1x the
   GRiSP arm32 JIT to 2.25x.  That commit's own conclusion is the reason to
   extend it: the dispatch alone measured neutral, and all of the win came
   from resolving `imported_funcs[Index]` inline so the fast path touches no
   C at all.
4. **`allocate_frame_fast/2`** (missing: riscv32, riscv64, xtensa, armv6m,
   wasm32).  Same shape as 2: an `allocate` with room available becomes a
   compare, a subtract and a store.
5. **`get_list_head_tail/4`** (missing: riscv32, riscv64, xtensa, armv6m,
   wasm32).  Keeping head and tail in distinct registers is what removed the
   reload in every list walk on arm32; the paired load is a bonus, not the
   point, so a backend without `ldp` still gains.
6. **The branch-relaxation cluster on x86_64 and arm32**
   (`set_branch_hints/2` + `take_overflows/1` + `rewind_stream/2`).  aarch64,
   riscv32/64 and armv6m converge each forward branch to the size it needs
   over a sizing pass.  x86_64 instead guesses from a static table of
   condition kinds (`cond_skip_disp_width/1`): the small-integer tag test
   `{_, '&', _, '!=', _}` — the most common guard there is — always takes the
   rel32 form.  A wrong guess is caught by an always-on assertion (`?ASSERT`
   is `true = Expr` there, in every build), so this is a size question and
   not a correctness one: the convergence loop would let every site that
   fits take the short form, four bytes smaller.
7. **`supports_select_val_ranges/0`** (missing: riscv32/64, xtensa, armv6m,
   wasm32) — a subtract and one unsigned compare replacing at least twice as
   many compares, in `case` statements over contiguous integers.
8. **`term_from_float_inline/2` on aarch64** and
   **`move_imported_bif_to_native_register/2` on aarch64** — both exist on
   x86_64 only, and aarch64 has everything they need.  Small, isolated.
9. **`jump_table_dispatch/1`** — natural on wasm32 (`br_table`), doable on
   arm32 and riscv; today x86_64 and aarch64 only.
10. **`compare_stub_call/3` / `map_get_stub_call/3`** beyond x86_64 and
    aarch64.  These need per-module stub emission, so they are the most work
    of the list; the payoff is the same C call removed from every comparison
    and map read.

Not worth extending: `addsub_smallint_fused/4` to x86_64 (measured: the split
form is already 8 instructions with two never-taken branches, and the fused
form needs `seto` plus a temp), and the `ldp`/`stp` pair family anywhere
without paired memory instructions.

## State of the worklist

Done in the session that wrote this survey:

- **`div`/`rem` by a power-of-two literal** now strength-reduces on every
  backend, including without a hardware divide, so arm32 stopped calling the
  BIF for `X div 2` (`pow2probe:d2/1` is a tag test and eleven ALU
  instructions there, against two C calls before).
- **`read_shrink_probe_mismatch/1` on arm32, riscv32 and riscv64**, with the
  shared corridor check generalised from 64-bit to any word size — item 1 for
  three of the six backends that lacked it.  The RISC-V pair also needed
  `read_avail_heap_memory/1`, and both helpers live in `jit_riscv_impl.hrl`,
  so one implementation serves 32- and 64-bit.
- **The `(uint)>` condition and `supports_select_val_ranges/0` on riscv32 and
  riscv64** — item 7 for both, and the condition is what item 1 needed there.
  A `case` over runs 1..5 and 20..23 now costs a subtract and one `bltu` per
  run instead of a compare per value.
- **`term_from_float_inline/2` and `move_imported_bif_to_native_register/2`
  on aarch64** — item 8, both of them.
- **The two dead probes removed**, and the 32-bit heap offsets asserted.

Not done, and why: items 1–5 for xtensa, armv6m and wasm32 each need the whole
inline-heap family first (none of them exports `read_avail_heap_memory/1`, and
xtensa and armv6m have no heap-pointer access at all), and none of those
targets can be *measured* from a desktop — CI builds them and runs the suites under qemu-user, which
proves correctness but not that inlining pays on a Cortex-M0+ or an ESP32,
where code size is the binding constraint.  That measurement belongs on the
Pi, the Pico and an ESP32 board.  Item 6 (branch relaxation on x86_64) is
four bytes a site with no correctness argument behind it, so it is a size
experiment, not a bug fix.  Items 7, 9 and 10 are unstarted.

## Method

The matrix comes from `erlang:function_exported/3` against each compiled
backend, cross-checked against `jit.erl`'s probe sites; the timing experiment
used a copy of `libs/jit/src` with all 74 probes routed through a
process-dictionary memo.  Neither is in the tree: both are one-off scripts.
