<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Hottest losing path: term comparison in ordered-collection scans

## Profiling method & finding

- **BEAM side** (eprof, erlc compiling erl_parse + beam_ssa_opt + beam_ssa_type):
  flat profile, top function 1.87%. The recurring names are all
  comparison/map/set primitives: `sets:is_element/2` 1.67%,
  `sets:add_element/2` 1.59%, `beam_types:verified_normal_type/1` 1.86%,
  `maps:remove/2` 1.03%, `ordsets:union/2` 1.01%, plus `beam_ssa_type` /
  `beam_types` meet/glb comparisons.
- **AtomVM side** (macOS `sample`, erlc compiling stdlib): self-time is
  dominated by C primitives that BEAM never calls out to —
  `term_compare0` #1 (reached 2:1 from generated compare ops
  `jit_term_compare_pin` and from map lookups `node_find`),
  `termtree_get`/`node_find` (ordered-map B-tree) #2, `term_find_map_pos`,
  `nif_maps_merge`, `nif_maps_remove`.

Both profiles point to the same two operations: **term comparison** and
**map-key lookup**. AtomVM loses on them because BeamAsm inlines them into
native code while AtomVM (a) calls C primitives and (b) writes every
mutated x-register through to memory so its moving GC can read the roots.

Representative function (small, pure, confirmed hot): **`ordsets:is_element/2`** —
a linear ordered scan whose entire body is `is_lt` / `is_ge` term
comparisons. This is exactly the path my new compare-order stub targets.

---

## 1. BEAM bytecode (`erlc -S ordsets.erl`)

```erlang
{function, is_element, 2, 21}.
  {label,21}.
    {test,is_nonempty_list,{f,23},[{x,1}]}.
    {get_list,{x,1},{x,2},{x,1}}.           %% x2 = head, x1 = tail
    {test,is_lt,{f,22},[{x,2},{x,0}]}.      %% if head < E: keep scanning
    {call_only,2,{f,21}}.                   %% tail-recurse
  {label,22}.
    {test,is_ge,{f,24},[{x,0},{x,2}]}.      %% if E >= head (i.e. ==): found
    {move,{atom,true},{x,0}}.
    return.
  {label,23}.
    {test,is_nil,{f,20},[{x,1}]}.
  {label,24}.
    {move,{atom,false},{x,0}}.
    return.
```

The two `is_lt`/`is_ge` tests on arbitrary terms are the hot work.

---

## 2. BeamAsm native (`erl +JDdump true`, aarch64)

XREG homes: x25=x0(E), x26=x1(list), x27=x2(head); x22=reduction counter,
x20=native stack top.

```asm
is_element/2:
    str x30, [x20, -8]!            ; push CP
    ...
    subs w22, w22, 1               ; reduction-- (in a REGISTER)
    b.le L93
    tbnz x26, 1, @label_23-4       ; is_nonempty_list
# get_list_Sdd
    and x8, x26, -8
    ldp x27, x26, [x8]             ; x27=head, x26=tail  -- NO stores
# is_lt_fss  (head x27  vs  E x25)
    cmp x27, x25                   ; identity: head == E?
    b.eq L121
    and x8, x27, x25               ; both small ints? (AND both, test once)
    and x8, x8, 15
    cmp x8, 15
    b.ne L120
    cmp x27, x25                   ; both small: tagged signed compare
    b   L121
L120:
    mov x0, x27
    mov x1, x25
    bl  L104                       ; else: ONE call to the global comparator
L121:
    b.ge @label_22-5
# i_call_only_f  (tail recurse)
    ldr x30, [x20], 8
    b   is_element/2
```

---

## 3. AtomVM JIT native (precompiled, aarch64)

Pinned: x19=jit_state, x20=prim table, x21=ctx; x25=x0(E) x26=x1 homes.

```asm
7ec is_element/2:
    mov  x7, x25                   ; x7 = E
    mov  x8, x26                   ; x8 = list
7f4:
    and  x16, x8, #0x3             ; is_nonempty_list
    cmp  x16, #0x1
    b.ne 0x9fc
    and  x8, x8, #~3
    ldp  x7, x9, [x8]              ; x7=tail(idx0), x9=head(idx1)
    mov  x27, x9
    str  x9, [x21, #104]           ; <-- WRITE-THROUGH head to ctx->x[] (GC root)
    mov  x26, x7
    str  x7, [x21, #96]            ; <-- WRITE-THROUGH tail to ctx->x[] (GC root)
    mov  x7, x25                   ; reload E
# is_lt  (head x9 vs E x7)
    and  x8, x9, #0xf              ; head small int?
    cmp  x8, #0xf
    b.eq 0x898                     ;   yes -> test E (0x898), then tagged cmp (0x8d0)
    cmp  x9, x7                    ; identity head == E?
    b.eq 0x884
    mov  x16, x9                   ; else -> COMPARE STUB (resolves lists/tuples/
    mov  x17, x7                   ;         mixed immediates without a C call)
    bl   0x3554                    ; bl compare_stub_call
    ldr  x30, [x19, #48]           ; reload lr from jit_state->dispatcher_ret
    mov  x8, x17                   ; status
    cbnz x8, 0x874                 ; stub resolved? branch on its verdict
    ldr  x16, [x20, #88]           ; else load PRIM_TERM_COMPARE ...
    mov  x0, x9
    mov  x1, x7
    mov  x2, #0x0
    blr  x16                       ; ... and call C term_compare0
    ldr  x30, [x19, #48]
    cbz  w0, 0x337c                ; OOM check
    ...
# reduction check (hot loop-back)
8e8:
    ldr  w16, [x19, #16]           ; reduction-- (in MEMORY: jit_state->remaining_reductions)
    subs x16, x16, #0x1
    str  w16, [x19, #16]
    b.ne 0x7f4                     ; loop back (direct branch, tight)
    ... (schedule-out spill only when reductions exhausted)
```

Stub at 0x3554 = `emit_compare_stub_body` (prologue `stp x7,x8`/`stp x9,x10`/
`stp x11,x12`, then `cmp x16,x17; b.eq` identity, then primary-tag dispatch).

---

## Where AtomVM loses, ranked (per hot iteration)

1. **Write-through GC-root stores — the structural tax.** Every mutated
   x-register is stored to the `ctx->x[]` array in memory (`str x9,[x21,#104]`,
   `str x7,[x21,#96]` after each `get_list`) so the moving GC can scan
   registers as roots. BeamAsm keeps head/tail in x27/x26 with **no store**
   and uses stack maps — it flushes lazily only before a GC-safe point, not
   on every mutation. This is +2 stores here and recurs on every x-register
   write across the whole program. It is the same wall the deferred-store
   experiments hit; closing it needs GC root maps (a VM-architecture change),
   not codegen.

2. **Reduction counter in memory vs register.** AtomVM does
   load/`subs`/store on `jit_state->remaining_reductions` each iteration;
   BeamAsm decrements register x22. +1 load +1 store per call.

3. **Comparison itself is now at parity** — and this is the win from the
   work this session. Both sides inline identity + both-small-int tagged
   compare, then call a shared helper for everything else. AtomVM's compare
   stub even resolves *more* cases inline than BeamAsm's single
   `bl L104` (lists, tuples, mixed immediates decided without touching C),
   at the cost of a slightly longer call setup. Before the stub this was a
   naked C `term_compare0` call on every non-small-int comparison.

4. **Minor codegen slack.** AtomVM reloads E (x25→x7) each iteration and
   splits the two-operand small-int test across two branches
   (`and x8,x9,0xf` … then E at 0x898), whereas BeamAsm ANDs both operands
   and tests once. ~1–2 instructions.

Net: the comparison volume that shows up as `term_compare0` in the profile
is now mostly the *fallback* tail (atoms, which the stub leaves undecided)
plus the map-tree `node_find` path. The dominant remaining structural loss
on this hottest path is the write-through root model (#1), not the
comparison codegen — which the stub brought to parity with BeamAsm.
