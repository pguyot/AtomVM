<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# What BeamAsm emits that we don't — a codegen audit, 2026-09-14

Reading AtomVM's generated code next to BeamAsm's for the same source, on
AArch64, and checking what differs against the fusion rules BEAM actually
declares. Nothing here is implemented yet; this is the shopping list.

## Method

Both compilers can be made to annotate their output with the BEAM opcode each
run of instructions came from, which is what makes the comparison mechanical.

```shell
# BeamAsm: writes <module>.asm per loaded module, with "# opcode_name" comments
erl +JDdump true -pa . -noshell -eval 'codegen_probe:tup4({1,2,3,4}),halt().'

# AtomVM: ELF with DWARF, symbols named <module>:op_<opcode>@<offset>
JITDW=... tools/dev/jit_dwdump.sh aarch64 codegen_probe.beam
```

`tools/dev/jit_dwdump.sh` carries the recipe (including how to build the JIT
with `-DJIT_DWARF`), and works for any target from any host — the arm32 and
x86-64 numbers below were taken on the Mac. `tools/dev/codegen_probe.erl` is
the probe set. BEAM's own list of fused opcodes is
`erts/emulator/beam/jit/arm/ops.tab` in the OTP tree, which is a better
checklist than guessing.

## The size of it

Hot-path instructions actually executed, counted by hand from the two dumps
(slow paths and their inline bodies excluded on both sides):

| function | AtomVM | BeamAsm | ratio |
|---|---:|---:|---:|
| `tup4/1` — 4 tuple elements in, 4-tuple out | ~60 | ~20 | 3.0 |
| `lst/2` — cons loop body | ~28 | ~12 | 2.3 |

`tup4/1` side by side, after the type test, is the whole audit in miniature:

```
BeamAsm                             AtomVM
and  x0, x25, -8                    mov x7, x25 ; and x7, x7, #~3
ldp  x26, x27, [x0, 8]              ldr x8, [x7, #8]  ; mov x26, x8 ; str x8, [x21, #0x60]
ldp  x28, x25, [x0, 24]             mov x7, x25 ; and x7, x7, #~3
                                    ldr x8, [x7, #0x10] ; mov x27, x8 ; str x8, [x21, #0x68]
                                    ... twice more ...
3 instructions                      20 instructions
```

## Findings, ranked

### 1. Two-operand backend ops force a copy out of every register home

`MMod:and_(St, {free, Reg}, Mask)` emits `and Reg, Reg, Mask`. The `{free, _}`
convention means "you may destroy this", so a frontend that needs the source
afterwards — which is almost always, since the source is a live x register —
has to `move_to_native_register` first, and on AArch64 that copies out of the
x0-x3 home register. Hence `mov x7, x25 ; and x7, x7, #~3` where BeamAsm writes
`and x0, x25, -8`.

The underlying assembler already takes three operands
(`jit_aarch64_asm:and_(Rd, Rn, Rm)`); it is the backend interface that is
two-operand. Every derived value pays one instruction for this, and derived
values are everywhere: type tests, untagging, arithmetic, comparisons. There
are 131 register-to-register `mov`s in our output for the probe module against
19 in BeamAsm's.

**Fix**: three-operand forms (`and_/4`, `shift_right/4`, …), or a read-only
handle from `move_to_native_register` plus a separate destination.
**Applies to**: AArch64, arm32, RISC-V, Xtensa — every three-operand ISA.
x86-64 is genuinely two-operand and keeps the copy.

### 2. No current-tuple pointer

BEAM emits `load_tuple_ptr` once after a tuple type test and keeps the untagged
pointer live across the whole run of `get_tuple_element`s (`current_tuple` in
`ops.tab`, dropped when the tuple register is overwritten). We re-derive it
every time: `OP_GET_TUPLE_ELEMENT` in `jit.erl` does `move_to_native_register`
+ `and_(TERM_PRIMARY_CLEAR_MASK)` per element. In `tup4/1` that is 8 wasted
instructions out of 20.

**Fix**: frontend (`jit.erl`), one cache entry alongside the existing
`vm_types` tracking. **Applies to**: every backend.

### 3. No paired load/store for consecutive heap words

BEAM fuses two `get_tuple_element`s at consecutive positions into
`get_two_tuple_elements` (one `ldp`), builds tuples with `stp`, and builds
conses with `stp`. We emit one load or store per word.

The machinery already exists on both ends: AArch64's `get_list_head_tail` uses
`ldp`, and arm32 gained `ldrd`/`strd` yesterday. It just is not applied to
`get_tuple_element` pairs, `put_tuple2` or `put_list`. In `tup4/1`: 4 loads
would become 2, and `put_tuple2`'s 5 stores would become 3.

### 4. Write-through x-register stores that BEAM never emits

Every write to an x register also stores to `ctx->x[N]`, even when the value
dies before the next GC or exception point. `tup4/1` emits five such stores;
BeamAsm emits none, keeping x0-x3 in x25-x28 and materialising them only where
C can see them. The deferred-store machinery (`pending_*`, `set_live_masks`)
exists but is not eliding these.

**Fix**: worth understanding why the existing elision does not fire here before
building anything new.

### 5. Tuple header comparisons are decomposed

`OP_TEST_ARITY` emits `ldr h ; tst h, #0x3F ; b.ne ; lsr h, #6 ; cmp h, #N ;
b.ne`. Since `TERM_BOXED_TUPLE` is 0, a tuple header of arity N *is* `N bsl 6`,
so `ldr h ; cmp h, #(N bsl 6) ; b.ne` says the same thing — BeamAsm's
`cmp x8, 256` for a 4-tuple. **3 instructions per tuple type test**, frontend
fix, every backend.

### 6. Tag tests could be single bit tests

`is_nonempty_list` costs `and #3 ; cmp #1 ; b.eq ; b` against BeamAsm's one
`tbnz x25, 1`. The trick is that primary tag `0b00` (our `TERM_PRIMARY_CP`)
never appears in an x register, so one bit separates list from non-list and
boxed from non-boxed. BEAM relies on exactly the same invariant. Worth stating
the invariant explicitly in the code if we take this.

### 7. Inverted branch pairs and branch-to-branch chains

Throughout `cmp/2` there are sequences like `b.eq over ; b target` where
`b.ne target` would do, and `b X` where X is itself `b Y`. A branch-resolution
peephole at finalisation would remove both; the branch-hint/backtrack machinery
on AArch64 (`set_branch_hints`, `rewind_stream`) is adjacent but is not doing
this.

### 8. Slow paths are emitted inline

The `test_heap` GC call — about 26 instructions — sits between the heap check
and the continuation, so the hot path branches over it. BeamAsm branches to a
shared stub (`bl L31`) instead. This costs no executed instructions but a lot
of I-cache. The cold-arm outlining mechanism already in the tree is the right
home for it; it just does not cover `test_heap`/`allocate`.

### 9. No post-indexed heap stores

BeamAsm bumps the heap inside the store: `stp x9, x25, [x23], 16`. We emit
`mov ptr, hp ; add hp, hp, N ; str ; str`. Pairs with finding 3.

### 10. No arithmetic fusion

BEAM fuses `A*B+C` into `i_mul_add`, and routes a bare `*` through the same
instruction with a zero addend. We have `addsub_smallint_fused` but nothing for
multiply-accumulate. Lowest value of the ten, but `int_arith` is 1.76x off
GRiSP on arm32, so the family is worth a look.

## Not worth copying

- BeamAsm's `i_breakpoint_trampoline` on every function entry: we have no
  equivalent tracing requirement.
- Its reduction charge at function entry rather than at the call site: same
  cost, and ours interacts with the pinned counter we just added.
- Its `mov x2, 63 ; mov x3, 127` literal materialisation before the smallint
  check in `i_mul_add`: that is asmjit not folding immediates, not a design.

## Suggested order

1 and 2 are the two that pay everywhere and are contained. 5 and 6 are small,
local and mechanical. 3 needs 2 first (a paired load wants the base pointer
already in a register). 4 is the largest single count in `tup4/1` but needs a
diagnosis before a fix. 7 and 8 are finalisation-pass work, independent of the
rest. 9 pairs with 3. 10 last.
