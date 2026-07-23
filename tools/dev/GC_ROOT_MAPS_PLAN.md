<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# GC root maps / deferred x-register stores — engineering plan

Goal: eliminate the write-through store that today accompanies every VM
x0-x3 register write in aarch64 JIT code, by making generated code keep
x registers register-resident and spill them to `ctx->x` only at points
where something else may legitimately read them. This is the last
identified structural lever against BeamAsm on M1/Neoverse-class
hardware (the label-cache ceiling measured 0.001%; every micro-lever is
shipped as of w27/jit-pinned-regs 0df05c874).

## 1. Evidence and sizing

- Static mix of hot compiled modules (beam_types): ldr 24%, str 13%;
  1501 of the strs are x0-3 write-through stores (~2.2% of instructions),
  plus post-call home reloads (2 ldp per non-cache-safe call; x0-only
  variant shipped for OP_CALL_EXT).
- The measured per-op residual vs BeamAsm concentrates in exactly this:
  iteration 28 traced base.ex's hottest native code to sets:from_list's
  fold, where each add_element call pays per-call arg stores plus a
  4-home reload.
- Neoverse perf: our kernel batch executes 89.9B instructions at IPC
  2.68 (no stalls) — ~2.5x BEAM's instruction count. Instruction count is
  the only lever there.
- Naive deferred stores (v2) measured only ~1% (1099 vs 1111 ms on
  base.ex) *before* correctness fixes were complete, because GC-point
  density in compiler code is high. The plan below changes that
  arithmetic with slow-path-only spills (P2) and register-passed
  arguments (P4).
- 2026-07-23 disassembly corroboration (ordsets:is_element/2, the hottest
  loser path — see scratchpad/HOTPATH_REPORT.md). AtomVM emits, per
  get_list iteration, `str x9,[x21,#104]` + `str x7,[x21,#96]`
  (head+tail write-through to ctx->x[]); BeamAsm keeps head/tail in
  x27/x26 with NO store (stack-map GC) and its reduction counter in
  register x22 vs our ldr/subs/str on [x19,#16]. The comparison codegen
  itself is now at PARITY (both inline identity + both-small-int tagged
  compare + a fallback call; our compare stub resolves MORE inline than
  BeamAsm's single global call), and term_compare / find_map_pos are both
  prim_pure/no-GC with the map path already at minimal work (identity
  loops <=32, binary search + inline probes >32). This CONFIRMS the
  write-through store discipline (P2 slow-path spills + P4 register-passed
  args) is now the ONLY remaining structural lever — every bounded
  codegen and algorithmic lever is measured and taken.

## 2. Reference model (verified in OTP sources)

BeamAsm/aarch64 (`erts/emulator/beam/jit/arm/beam_asm.hpp`):
- XREG0..XREG5 live in callee-saved machine registers (x25 = XREG0, ...).
- A register cache tracks what is memory-backed vs dirty.
- `emit_enter_runtime<Update::eXRegs>` spills cached x registers to the
  x_reg_array before entering any C code that may GC or read x regs;
  leaving the runtime re-loads what is needed.
- Erlang-to-Erlang calls pass x0..xN in those machine registers; there
  is no memory crossing for arguments. Each function spills at its own
  runtime entries with its own liveness.
- The GC has NO register knowledge: it reads the x array MEMORY with a
  live count, after the spill. "Root map" == spill discipline + live
  count, nothing more.

AtomVM already has: x25-x28 homes for x0-3 (write-through v1), pinned
ctx/jit_state/table (x21/x19/x20), hp/e in x22/x23, callee-saved cache
pool, jit_liveness (per-label live-in masks, dead-move elision,
per-call-site plumbing exists since the x0-only reload change).

## 3. Post-mortem of failed attempt v2 (what the plan must solve)

From memory `elixirc-base-ex-perf-analysis` iterations 8-14:

1. GC reads `ctx->x[0..live-1]` from MEMORY at every GC point. GC points
   are not just call boundaries: inline allocate/test_heap slow paths,
   every GC-capable primitive, callee entries.
   -> Plan: spill {dirty ∩ live} at every GC point, but move the spill
   INTO the slow path where a fast/slow split exists (P2), so the hot
   path pays nothing.
2. Cross-process/C readers of another context's `ctx->x` (process_info
   class) can read stale memory while the owner runs.
   -> Plan: reader census (P0) + make cross-process introspection either
   tolerate staleness explicitly or synchronize at safe points.
3. Exception re-entry read homes that memory writers (jit_handle_error)
   had bypassed.
   -> Already solved on the branch: continuation_via_dispatcher reroutes
   catch continuations through the dispatcher, which re-seeds homes.
4. call_fun fast path clobbered arg homes on reload (fixed in ca0db43c4,
   fix preserved in the revert notes).
5. Dead home slots holding stale pre-GC pointers must never be treated
   as roots. Liveness masks bound the root set; flush stores NIL into
   dead-but-flushed slots or the mask excludes them.
6. Instrumentation: naive poison traps false-positive on dead slots.
   -> Use the shadow-array GC oracle (P1) instead.

## 4. Correctness invariants (to hold at the end state)

- I1: At entry to any C code that may collect, for every live VM x
  register i, `ctx->x[i]` holds the current value.
- I2: After return from any C code that may collect, generated code
  re-loads every live home whose term may have moved before reading it.
- I3: Any non-generated-code reader R of `ctx->x` either (a) runs only
  at points where I1-equivalent flushing has happened for the slots it
  reads, or (b) is documented stale-tolerant.
- I4: A dead x slot is never presented to the GC as a root.
- I5: The emulator (mixed mode) and 32-bit/other backends keep the
  write-through contract unchanged until explicitly ported; module
  boundaries between conventions must flush like a C boundary.

## 4.5 P0 GATE RESULT (2026-07-22) — PROGRAM STOPPED AT P1.5

Measured on one kernel-app erlc compile (counting build: emitted counters
at every x0-3 write-through store and every non-cache-safe C-call
crossing; C counters at the slow paths):

    x0-3 write-through stores   1,577,897,301
    non-cache-safe crossings      135,448,857   (post prim_no_gc)
    test_heap slow path                58,454   (fast path billions)
    allocate slow path                    232
    ensure_free entries            26,517,270

P4 net = 1.578B saved strs - ~135M x (dirty∩live ~1-2) spill strs
       ≈ 1.4B instructions ≈ 1.5-1.8% of the ~90B-instruction compile.
Below the 2% gate. The write-through traffic is NOT the structural gap:
it is ~1.75% of dynamic instructions. The 2.5x instruction-count gap vs
BeamAsm lives in C-primitive interiors (compare walks, map searches, GC
copies) — BEAM wins by compiling those calls away (typed ops, inlined
map access), not by storing less. DECISION: keep P1.5 (shipped,
dba0d221d); P2/P3/P4 are closed by the gate. The oracle (P1) and the
reader census remain documented here should the calculus change (e.g.,
a port where store bandwidth is the bottleneck).

## 5. Phases

Each phase is independently shippable and measured; later phases are
gated on the earlier ones' numbers. Kill criteria included.

### P0 — Reader census + dynamic sizing (1 session, no behavior change)
- Enumerate ALL readers of `ctx->x` outside generated code (grep + call
  graph): GC roots, NIF argv (`nif_ptr(ctx, argc, ctx->x)`), BIF args,
  apply/spawn arg marshalling (memory_estimate_usage <- do_spawn was a
  v2 crash site), emulator opcodes (mixed mode), signals/process_info
  (cross-process!), crash dumps, stacktrace building, term_to_binary of
  arguments in exits, distribution.
  Deliverable: table (reader, slots read, when, class a/b per I3).
- Instrument (temp counters): x0-3 write frequency, GC-point execution
  frequency by class (call_ext NIF, gc_bif, test_heap slow path taken vs
  not, allocate, local call), dirty∩live size distribution at each
  class. Compute the predicted store savings.
- GATE: predicted net instruction saving < 2% on kernel+stdlib batch =>
  stop at P2 (still worth it), skip P3/P4.

### P1 — Shadow-root GC oracle (1 session, debug-only infrastructure)
- Debug build flag: every home flush also writes a shadow array in
  jit_state; memory_gc asserts roots == shadow for x0-3 before
  collecting; optionally NIL-fills dead slots (liveness-aware, masks
  already exist in the pending machinery).
- This converts "hangs three hours into a sweep" into "assert at first
  divergence" — the single biggest cost of v2 was debugging latency.
- Also port the poison-store emission helpers from ca0db43c4 as a build
  option, and keep the per-module v1/v2 bisection harness recipe
  (scratchpad/bisect.py pattern) in tools/dev.

### P1.5 — SHIPPED 2026-07-22 (dba0d221d): precise GC-boundary primitive
classification. prim_no_gc/1 marks the allocate-within-reservation,
no-collect, no-x-write middle class (map puts, boxed allocators,
bitstring slice/copy family, module_load_literal, binary reuse):
homes and register cache survive those calls; hp/e stay synced.
beam_ssa_type -0.39% instructions. This is the audit the plan's P0
called for, done for the primitive tier; the audit script pattern is
in the session scratchpad (gcaudit.py, static call-closure over
src/libAtomVM with GC/x-write sinks).

### P2 — Slow-path-only spills for split GC points (1-2 sessions)
- test_heap / allocate / corridor checks: today the C call in the slow
  branch is a normal non-cache-safe call; the fast path continues
  without memory traffic ONLY because v1 write-through already paid for
  it at every write. Change: fast path unchanged and pays nothing; the
  slow branch gains a {dirty ∩ live} home spill before the C call and
  masked reload after.
- With v1 write-through still on, this phase is a no-op; it lands the
  EMISSION MACHINERY (spill blocks, masks at sites) behind the oracle,
  so P4 is a flag-flip of store policy rather than new codegen.
- Extend today's per-site mask plumbing (post_call_xregs) from
  {all|x0} to a 4-bit mask fed by jit_liveness per-op live-after; apply
  to gc_bif/bif result ops where the call is the emitter's final
  x-touching action (bounded, known-safe set).

### P3 — Register-resident arguments for native-to-native calls (1-2 sessions)
- Calls whose target is resolved native code (local OP_CALL family, the
  inline resolved call_ext fast path, call_fun fast path) already have
  args in homes; the callee reads args via homes. Drop the arg MEMORY
  stores at such sites; the callee's own first GC point spills per its
  liveness (after P4; under v1, this phase only removes redundant
  duplicate stores of never-memory-read args — measurable alone).
- NIF/BIF/emulated/apply targets keep full memory args (NIF ABI reads
  ctx->x): classify per call site at emission (target class is known for
  the inline fast paths; the C fallback keeps the old contract).
- This is the phase that attacks the measured sets:add_element pattern.

### P4 — Deferred stores, take three (3-5 sessions + hardening)
- Flip x0-3 writes to home-only (mov); every GC point (now = C call
  sites + slow paths from P2) spills {dirty ∩ live}, NILs dead-flushed
  slots or relies on masks per I4; masked reloads after.
- Order of validation (all infrastructure exists by now):
  1. oracle build over test-erlang, estdlib suite, jit eunit;
  2. erlc corpus compile with BYTE-IDENTITY check of emitted .beam
     files vs BEAM erlc (the ssh corruption lesson: existence != valid);
  3. elixirc full sweep including inspect.ex (the historical
     reproducer) under oracle;
  4. per-module bisection harness on any failure;
  5. hammered/deterministic 232-file validation, then benchmarks.
- Mixed-mode: entering the emulator or an emulated-pinned module is a
  C-boundary flush (I5). Cross-process readers per P0 census outcome.
- GATE: if oracle keeps finding new reader classes after 2 sessions of
  P4 debugging, stop, keep P2+P3, document.

### P5 — Ports and cleanup (optional, after aarch64 proves out)
- riscv (s-regs free since pinning), x86_64 (fewer spare regs; likely
  x0-1 only), arm32. Each is mechanical once the generic-layer masks and
  invariants are settled. The dispatcher_ret/lr scheme ports similarly.

## 6. Effort and expected return

SUPERSEDED BY THE P0 GATE MEASUREMENT (section 4.5). The estimates below
were written a-priori, BEFORE P0 was run. The gate measured the actual
dynamic write-through traffic at ~1.75% of the compile's instructions,
so the P4 return is ~1.5-1.8% (NOT the "2-6%" guessed here), below the
2% gate. P2/P3/P4 are CLOSED. Kept for provenance only:

- P0+P1: ~2 sessions, pure information + safety net.
- P2+P3: ~3 sessions, expected 1-3% on M4, more on Neoverse; low risk.
- P4: 3-5 sessions with real miscompile risk contained by the oracle;
  a-priori GUESS of 2-6% on erlc-class workloads — REFUTED by the P0
  gate (actual ~1.5-1.8%).
- Combined a-priori target: high single digits on the losing apps —
  NOT SUPPORTED by measurement. The write-through is ~1.75% and the
  compare/map "volume cuts" that this line counted on are, as of the
  2026-07-23 disassembly, already at minimal work (compare = BeamAsm
  parity, map = binary-search + inline probes). No measured lever, in
  or out of this plan, closes the 10-14% gap on stdlib/kernel/debugger.
  The 2.5x instruction gap is C-primitive interiors + call volume that
  BeamAsm compiles away via type specialization; inlining them in-JIT
  measured 0% (work conserved), HAMT measured net-worse twice.

## 7. Kill criteria and standing risks

- P0 sizing gate (< 2% predicted => P2-only).
- Any GC-root unsoundness that the oracle cannot pin within 2 sessions.
- Cross-process reader that cannot be classified stale-tolerant and
  cannot be synchronized without a lock on the hot path.
- The emulator interop surface growing new flush points faster than
  they are audited (watch: new mixed-mode features on the branch).

## References

- memories: elixirc-base-ex-perf-analysis (iterations 7-14, 25, 28:
  v1/v2 history, GC-wall proof, disasm evidence), erlc-otp-corpus-goal-w29
  (label-cache ceiling 0.001%, Neoverse IPC evidence, censuses),
  jit-call-spill-only-prologue-removes-it.
- code: v2 design + call_fun fix in commit ca0db43c4 (reverted by
  99a01e36a) on the elixirc-era branch; poison harness in the same diff.
- OTP: erts/emulator/beam/jit/arm/beam_asm.hpp (XREG cache,
  emit_enter_runtime spill contract).
