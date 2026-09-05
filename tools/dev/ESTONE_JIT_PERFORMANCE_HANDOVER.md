<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Estone and JIT performance ideas — static audit handover

Date: 2026-09-05.

## Scope and status

The user requested a static audit of OTP's estone benchmark, AtomVM's VM and JIT implementation, with unconventional performance ideas for **arm32, aarch64, x86-64, and riscv32**. The user explicitly requested that nothing be run. The audit used file searches and source/document reads only: **no builds, benchmarks, tests, disassembly generation, or project code execution**. This document is the only resulting change.

All proposed performance benefits below are **unmeasured hypotheses**. Implementation gaps described as observed were established from the working-tree sources, not from generated machine code. No baseline commit was recorded; source locations and feature availability must be rechecked if the tree changes.

The next agent should preserve the static-only constraint unless the user authorizes execution. The validation suggestions below are plans, not authorization to run them. This handover does not authorize implementing the ideas either.

Local paths on this machine:

- AtomVM: `/Users/paul/Projets/AtomVM/AtomVM`
- OTP: `/opt/src/otp` (the initially supplied `~/otp` path does not exist here)
- Benchmark project: `/Users/paul/Projets/AtomVM/atomvm_benchmark`
- OTP benchmark: `/opt/src/otp/erts/emulator/test/estone_SUITE.erl`
- Benchmark port: `../atomvm_benchmark/src/estone_test.erl` relative to the AtomVM repository root. There is also a root-level `estone_test.erl` and `estone_main.erl` in AtomVM. Do not assume these copies or historical benchmark images are identical.

## Current JIT model

AtomVM's JIT is a module compiler written in Erlang, not a profiling trace JIT. The shared frontend decodes BEAM, performs liveness analysis, consumes BEAM type information, and invokes architecture-specific emitters. Finalization resolves branches and deferred/shared stubs. AOT uses the same compilation machinery; native code is embedded in an `avmN` chunk and selected by the loader. The precompiler can remove the BEAM `Code` chunk, so native code is not necessarily stored alongside bytecode in an AOT image.

Existing optimizations include register caching, dead-move elimination, pending-store elision, typed arithmetic, specialized comparison paths, direct native dispatch, and shared cold paths. Avoid proposing these as entirely missing.

Pending-store elision emits stores first and subsequently replaces proven-unobservable stores with same-width NOPs. It is not general deferred GC-root materialization. C primitives are classified separately: pure primitives, allocating but noncollecting primitives, and context-changing primitives have different synchronization requirements.

Primary sources:

- [Shared compiler](../../libs/jit/src/jit.erl): `compile0`, `emit_pass`, `jit_liveness:analysis`, capability checks.
- [Liveness analysis](../../libs/jit/src/jit_liveness.erl).
- [Register tracking](../../libs/jit/src/jit_regs.erl).
- [Pending-store machinery](../../libs/jit/src/jit_backend_pending_impl.hrl).
- [Primitive classifications](../../libs/jit/src/jit_prim_pure.hrl).
- [Native loader](../../src/libAtomVM/module.c): native chunk selection around line 1571.
- [JIT documentation](../../doc/src/jit.md).

### Backend differences observed

| Backend | Relevant current implementation |
|---|---|
| aarch64 | Pinned heap and stack pointers, x0–x3 homes, loop residency, and an already-pinned reduction counter (`r24`). |
| x86-64 | Pinned heap and stack pointers; loop residency disabled; reduction counter decremented in memory; baseline variable shifts use RCX. |
| arm32 | Loop residency disabled; no exported `heap_bump_alloc`; `supports_div` and `supports_fp` return false. This is the ARM Linux backend, distinct from armv6m/Thumb backends. |
| riscv32 | Loop residency disabled; no inline heap operations; RV32IMC; division available and FP unavailable. Context uses `s1` for compressed addressing. |

Sources: [aarch64](../../libs/jit/src/jit_aarch64.erl), [x86-64](../../libs/jit/src/jit_x86_64.erl), [arm32](../../libs/jit/src/jit_arm32.erl), [riscv32](../../libs/jit/src/jit_riscv32.erl), [shared RISC-V implementation](../../libs/jit/src/jit_riscv_impl.hrl).

## Historical evidence and conclusions to avoid repeating

Read these before selecting work:

- [GC root maps plan](GC_ROOT_MAPS_PLAN.md): **section 4.5 supersedes the opening argument**. The measured gate found x0–x3 write-through stores were about 1.75% of dynamic instructions in the measured compiler workload. The broad deferred-store project was stopped. This does not establish the same economics on in-order 32-bit targets or allocation-free micros.
- [Hot-path report](HOTPATH_REPORT.md): useful historical disassembly, but its aarch64 memory-resident reduction counter is obsolete. Current code pins reductions in `r24`.
- [Fusion benchmark](FUSION_BENCHMARK.md): list-test/get-list fusion was reverted after an approximately 6% x86-64 regression, neutral aarch64 results, and no demonstrated MCU win. Record access and fixed-field binary decode fusions were retained. Do not revive list fusion merely because it removes a load.
- [Link relations benchmark](LINK_RELATIONS_BENCHMARK.md), dated 2026-08-30: the latest relevant estone result found in this audit was **0.770× BEAM** on a common subset, versus approximately 0.60× in older July notes. These are historical aarch64 measurements, not measurements of the current audit or evidence for other architectures.
- [Benchmark matrix results](BENCHMARK_MATRIX_RESULTS.md): retain workload, scheduler count, inclusion set, build configuration, and date when quoting results.
- [Code-size plan](../../jit-code-size-plan.md) and [opcode-fusion recommendation](../../jit-opcode-fusion-recommendation.md): several opportunities were already proposed or subsequently implemented. Recheck current code before treating an item as new.

The August link report already rejects generated-code link/unlink fast paths as mostly benefiting redundant operations in estone. It also proposes pooled relation storage and symmetric local relation installation. Those are existing proposals, not novel findings from this audit.

## Idea 1 — Eliminate reconstruction of a list just destructured

**Priority:** high-value larger experiment. **Targets:** all four, especially arm32/riscv32. **Status:** source-motivated hypothesis; surviving BEAM constructions have not been verified in generated artifacts.

OTP's `pat_loop2` and several other pattern loops repeatedly match a list prefix and rebuild that same prefix:

```erlang
loop(I, [X, Y | Tail]) ->
    loop(I - 1, [X, Y | Tail]).
```

When all fields are unchanged, reuse the original tagged list pointer. A small provenance analysis could record the relationship between a cons and its extracted head/tail, then recognize `put_list` reconstructing it. Extend through short cons chains.

This removes allocation, stores, heap-pointer updates, and future GC work. It is distinct from test/get-list fusion, which mostly rearranges existing loads.

Important exception: `pat_loop1` matches an unchecked first element and reconstructs it as `0`. Reusing the whole input requires proving that first element is zero. Suffix reuse can still be valid. Do not infer invariants merely from estone's particular input.

Implementation entry points:

- OTP `estone_SUITE.erl`: `pattern`, `pat_loop1` through `pat_loop5`, around lines 499–601.
- Shared `jit.erl`: `OP_GET_LIST` around line 1377 and `OP_PUT_LIST` around line 1432.
- Register/value provenance must survive only across operations that preserve its validity. A raw pre-GC pointer cannot be reused after collection without proper root handling.

First static check: inspect existing compiled BEAM artifacts, if readable without executing project code, to determine which reconstructions OTP's compiler already eliminates. A source pattern alone does not prove redundant native allocation remains.

Later validation: changed-head cases, aliases, partial suffix reuse, improper lists, GC boundaries, and branching predecessors. Measure allocated words and GC work in addition to time. Reject a general analysis if it adds hot-path work without eliminating meaningful construction volume.

## Idea 2 — Shift tagged small integers directly

**Priority:** first small code-generation experiment. **Targets:** aarch64/x86-64 now; use the formulation for future 32-bit paths. **Status:** concrete redundant sequence observed in runtime shift lowering.

AtomVM's integer tag is all ones in the low four bits:

```text
tag(n) = (n << 4) | 15
tag(n bsr k) = ASR(tag(n), min(k, word_bits - 1)) OR 15
```

The identity applies to a small integer and a nonnegative shift count. Current runtime literal `bsr` lowering instead performs an arithmetic right shift by `k + 4`, a left shift by four, and an OR of the tag. Shifting the tagged word directly removes the retagging left shift.

The variable-count path can likewise clamp the count, shift the tagged operand directly, and restore its low bits. Preserve operand checks and the negative-count fallback. Hardware shift-count masking is not Erlang's large-count saturation.

Sources:

- [Term layout](../../src/libAtomVM/term.h): `TERM_INTEGER_TAG`, `TERM_IMMED_TAG_MASK`, both `0xF`.
- Shared `jit.erl`: `op_gc_bif2_shift_reg_runtime2` around line 6558 and `op_gc_bif2_shift_lit_runtime` following it.
- OTP `erts/emulator/beam/jit/arm/instr_arith.cpp`: `emit_i_bsr`, around line 1507, already exploits this identity.
- OTP `erts/emulator/beam/jit/x86/instr_arith.cpp`: `emit_i_bsr`, around line 1585, uses the equivalent form.

Later validation: negative and positive small integers, zero, minimum/maximum small integer, count zero, counts around payload width and machine width, very large positive counts, negative counts, and noninteger operands. Verify guard failure and body exception behavior separately. Inspect emitted instruction counts; do not assume one fewer instruction guarantees a timing win.

## Idea 3 — Solve arithmetic guards backwards

**Priority:** second small frontend experiment. **Targets:** all four. **Status:** no matching consumer-aware simplification found in inspected lowering.

Estone's `pat_loop2` contains:

```erlang
Y bsl 1 == 0
Y bsl 2 == 0
Y bsl 2 == 4
```

If the shift result is dead except for the comparison, replace these guards with exact integer tests for `0`, `0`, and `1`, respectively. Exact tests reject floats and other nonintegers, as the original shift guard does.

General restricted rule: for constant nonnegative `k` and integer constant `C`, solve `(X bsl k) == C` at compile time. If `C` is not divisible by `2^k`, the guard is impossible; otherwise compare `X` exactly with the integer preimage. This eliminates the intermediate operation, overflow work, and potential bignum construction.

Start with guard-only patterns and a dead intermediate result. Preserve failure labels, aliasing, and live values. Do not apply the transformation indiscriminately to body expressions, where exceptions and intermediate results may matter. Avoid huge compile-time intermediate allocations when checking extreme constants.

Sources: OTP `pat_loop2`; shared `jit.erl` GC-BIF shift lowering and comparison lowering. Existing type-range specialization is not the same as solving the consumer's predicate.

Later validation: divisible/indivisible constants, negative constants, small and boxed integer operands, floats, nonnumbers, aliases, and alternate guard clauses. First establish that this pattern survives the OTP frontend.

## Idea 4 — Inline 32-bit allocation without requiring permanent HP pinning

**Priority:** concrete backend gap. **Targets:** arm32/riscv32. **Status:** capability absence observed.

Shared `OP_PUT_LIST` selects inline allocation only when `heap_bump_alloc/2` is exported. These two backends do not export it, so surviving cons constructions call `PRIM_PUT_LIST`.

A first implementation can load HP from `Context`, store two cons fields, advance/write HP, and produce the tagged result. The preceding BEAM heap reservation supplies the space guarantee. A permanently pinned heap register is optional, not a prerequisite.

Then batch adjacent cons constructions into one prefix allocation and one final HP update. OTP's x86 `ops.tab` around line 495 has `put_cons`/`store_cons` rules worth studying. This saves work even without interpreter dispatch overhead.

Sources: shared `jit.erl` around line 1432; arm32/riscv32 exports and register conventions; `prim_no_gc(PRIM_PUT_LIST)` in the primitive classification file.

Related porting opportunity: runtime shift fast paths are gated on `word_size() =:= 8` around `op_gc_bif2_shift_fallback` and `op_gc_bif2_shift_reg_fallback` (approximately line 6486). Add missing backend operations and derive limits from word size. **Do not just delete the gate:** constants such as 59 and 63 and overflow checks currently assume 64-bit words.

Later validation: reservations, destination/source aliasing, cons order, HP synchronization before C calls, overflow/GC paths, and code size. Compare temporary HP loading against pinning on actual targets. On RV32, include compressed-encoding consequences in the comparison.

## Idea 5 — Caller-saved register contracts for call-free loops

**Priority:** medium-sized experiment after reducing C-call density. **Targets:** x86-64/arm32 first; riscv32 also relevant. **Status:** speculative alternative to full cross-call residency.

The x86-64 backend's explanation for disabling loop residency says callee-saved registers are occupied. That constrains values surviving arbitrary C calls, but does not require caller-saved values to be discarded on a call-free hot backedge.

For selected loops:

1. Normal entry loads live values into a private register assignment.
2. Backedges reconcile assignments and jump after entry loads.
3. Calls and scheduler exits materialize state.
4. Resumption enters through the normal entry.

Initially keep write-through stores. This isolates removal of repeated loads/moves from the deferred-GC-root project and its historical correctness difficulties.

Allocation and shift fast paths may make previously ineligible loops call-free, so evaluate these ideas together after isolating their individual effects. Riscv32 can separately evaluate unused callee-saved registers, with dispatcher/ABI preservation updated as necessary. RV32 allocation should account for compressed-register encodings.

Sources: x86-64 `supports_loop_residency` around line 464; arm32 equivalent around line 450; shared RISC-V equivalent around line 64; aarch64 loop preload/reconciliation implementation near `maybe_emit_loop_preload`.

Later validation: multiple predecessors, register shuffles and cycles, cold helper calls, scheduling at the backedge, GC, and shared tail-cache blocks. Existing shared blocks may assume memory-backed entries; do not enter them with an incompatible private contract.

## Idea 6 — A restricted native ABI for tiny local leaves

**Priority:** larger architectural experiment. **Targets:** aarch64/x86-64 first. **Status:** speculative; no generated-code prototype.

Estone `int_arith` repeatedly invokes tiny non-tail helpers. AtomVM's generic return path decodes the continuation representation, checks the module, and reconstructs a destination even on ordinary same-module returns.

Instead of changing the whole continuation ABI, identify local leaves that cannot allocate, collect, trap, or call Erlang. Emit a second entry with register arguments and native call/return. Retain the normal entry for ordinary callers and mixed-mode compatibility.

Potential benefit: remove argument-memory crossings and continuation decoding together. Restrict the first version to functions whose effect summaries make the semantics manageable.

Sources:

- OTP `estone_SUITE.erl`: `int_arith`, `do_arith`, `do_arith2`, around line 939.
- Shared `jit.erl`: `OP_CALL` around line 574 and `OP_RETURN` around line 781.
- OTP `erts/emulator/beam/jit/x86/instr_call.cpp`: `emit_return_do`, `emit_i_call`; native-stack behavior is configuration-dependent.
- OTP aarch64 `instr_call.cpp`: corresponding native call/frame behavior.

Constraints: reductions and fairness, stack traces, error behavior, code/module lifetime, ABI clobbers, and native-stack lifetime. Do not retain an unbounded native stack across process scheduling. A leaf containing generic arithmetic fallback may not meet the no-trap/no-allocation contract; prove eligibility rather than inferring it from its source size.

Later validation: eligible and ineligible callers, boundary arithmetic, reductions, stack unwinding, and fallback behavior. Reject if eligibility is too rare or maintaining duplicate entries costs more than it removes.

## Idea 7 — Avoid message sizing through construction knowledge

**Priority:** runtime/JIT experiment. **Targets:** all four; allocation predictability matters on MCUs. **Status:** concrete remaining sizing traversal; proposed replacement speculative.

The August documentation describes a one-pass shallow copy. Current `mailbox_message_create_from_term` still calls `memory_estimate_shallow_usage`, allocates storage, then calls the recursive shallow copier. It avoids general traversal machinery, but still sizes before copying.

For message shapes proven by generated code, calculate storage requirements statically and copy directly into a size-class block. Begin with envelopes whose fields are known immediates. Unknown payloads retain the generic path.

A more aggressive extension constructs a send-only, nonescaping envelope directly in message storage, avoiding its intermediate process-heap allocation. This requires substantially stronger escape and ownership reasoning.

Sources: [mailbox.c](../../src/libAtomVM/mailbox.c) around line 265; [memory.c](../../src/libAtomVM/memory.c), `memory_estimate_shallow`, `memory_copy_shallow`, around lines 759–855.

Constraints: payload sizing, off-heap binary/resource references, allocator size accounting, escaping envelopes, and receiver ownership. Do not assume a fixed outer tuple implies a fixed total deep-copy size.

The August report measured improved focused ping-pong copying but worse estone message chains through scheduler interactions. Evaluate copy cost separately from end-to-end chain latency. Raising the shallow-depth limit alone does not remove sizing traversal.

## Idea 8 — Coalesce readiness notifications

**Priority:** focused scheduler experiment before a full run-queue redesign. **Targets:** SMP hosts and multicore embedded targets; queue-operation savings may also matter on single-core systems. **Status:** repeated queue work observed; synchronization design remains open.

Each mailbox post reaches `scheduler_signal_message` and `scheduler_make_ready`. The latter takes the global process-list lock and removes/reappends the context. The current code already suppresses waking another scheduler unless a runnable backlog exists; do not propose that suppression as missing.

Avoid repeated queue work while a receiver is already notified and runnable. Estone's binary echo posts ten messages consecutively, providing a natural motivating case.

Use a synchronized notification-pending transition and a receiver-side mailbox recheck before parking. An unsynchronized `Ready` test can lose a wakeup. Account for mailbox publication order, running contexts, kill/spawn states, multiple producers, and task/interrupt delivery paths.

Source: [scheduler.c](../../src/libAtomVM/scheduler.c), `scheduler_make_ready` around line 412 and `scheduler_signal_message` around line 483; mailbox enqueue/post code.

Later validation: adversarial enqueue-versus-park interleavings, multiple senders, signal-only delivery, termination, and fairness. Measure lock acquisitions, queue operations, wakeups, throughput, and tail latency independently.

## Estone interpretation and harness limitations

The official score sums `weight * weight * 31000000 div max(1, microseconds)` across micros. Large score changes in tiny micros need not imply proportional application improvement. Keep per-micro times and inclusion sets alongside the total.

Several names obscure the work actually performed:

- `int_arith`: considerable non-tail helper-call work as well as arithmetic.
- `pattern`: reconstruction/allocation and arithmetic guards as well as matching.
- `timer`: queues messages first and then executes successful receives with timeout syntax; it does not primarily measure timer expiry.
- `large_local_dataset_work`: `_Data` is unused after creation. The source does not establish that the purported passive dataset remains live during subsequent work; inspect compiled liveness before drawing GC conclusions.
- `bif_dispatch`: repeatedly demonitoring the same reference mostly measures the absent-monitor path after its first removal, alongside dictionary and debug-BIF dispatch.
- `links`: the historical report identifies much redundant/idempotent link work. A score-only optimization need not improve real relation churn.
- Message-chain micros include process spawning and scheduler transitions, not just copying.

The port's scored runner catches all exceptions and reports a skipped micro. That can hide correctness failures as unsupported functionality. Future execution, if authorized, should distinguish expected unsupported operations from unexpected failures and enforce the same included subset on both engines. The historical common subset omitted port I/O and ETS on both engines; do not assume a current source list or image has the same exclusions.

The original suite collects GC and reductions statistics; the port's score output is more limited. Preserve equivalent work, compiler options, scheduler count, and timing boundaries. Do not extrapolate Apple Silicon results to arm32 or RV32 merely by instruction count.

## Recommended sequence for the next agent

1. Recheck current source and existing artifacts against these assumptions without running code.
2. Tagged right-shift simplification: smallest directly supported code-generation opportunity.
3. Backward arithmetic-guard simplification: establish that the pattern survives BEAM compilation.
4. arm32/riscv32 inline cons allocation and word-size-correct runtime shift paths.
5. Reconstruction elimination and loop-local register contracts, assessing their interaction with reduced allocation/helper calls.
6. Restricted native leaf ABI as the larger architectural bet.
7. Pursue construction-aware messages and notification coalescing as separate runtime hypotheses with separate accounting.

If execution is later authorized, use meaningful semantic edge cases and focused mechanism measurements before whole-suite timing. Report code-size and RAM costs on 32-bit targets. Reuse the existing benchmark methodology and rejected-experiment history; do not claim speedups from static instruction savings alone.
