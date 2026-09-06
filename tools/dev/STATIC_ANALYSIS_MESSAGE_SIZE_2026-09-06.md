<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Static analysis, register types and sized sends — measured — 2026-09-06

Three hypotheses were put:

- **(a)** more JIT static analysis would pay, but a per-function symbolic IR
  might not fit an MCU doing a runtime compile;
- **(b)** register type information could yield message sizes;
- **(c)** a `send` variant taking a size, when the size is known statically or
  is a simple function of a received message's size.

Goal: beat BEAM on ESTONE while keeping the MCU memory constraint.

All three were measured. **(c) is capped at about 1.9% of the ESTONE score and
the mechanism is worth a third of that.** The census run to size it found an
unrelated and larger defect, which is fixed in `bc8249423`.

## Method

- Apple M4 Mac mini, macOS 26.6.2, AArch64, AC power; OTP 29; Release `-O3`,
  SMP and JIT on; benchmark modules AOT-precompiled for `aarch64`.
- Where a cost had to be attributed to a source change, it was priced
  **in-process against the real library** (`pathcost.c`: the shipped
  `memory.c.o` linked into a bench that calls both paths on the same term), not
  by comparing two builds. Cross-build comparison of ESTONE message micros is
  known to be worthless below ~20%, and this session reproduced that: in the
  A/B below, `medium messages` — which carries no reference and *cannot* be
  affected by the change under test — moved 15%.

## (a) More static analysis — the pass already exists, and its cost is known

The fear is specific and testable: does a whole-function analysis fit on an
MCU that runs the JIT at load time? It already does one.

- `jit.erl` is **already two-pass** for the flash path: `compile_sizing/9`
  converges branch-size hints on a counting stream, then `compile_emit/10`
  emits once. `code_server.erl` calls both on device.
- `jit_liveness:analysis/1` is **already a whole-function pre-analysis**,
  running for every backend inside `emit_pass`, producing per-label live-in
  masks, call targets and a dead-move set. Its header already states the MCU
  constraint and the poisoning rule that keeps it conservative.

Measured cost of that existing pass (result held live, after GC; words
converted at 4 bytes for a 32-bit target):

| corpus | modules | code bytes max/mean | result max/mean |
|---|---:|---:|---:|
| AtomVM `estdlib` | 66 | 11,568 / 2,423 | **8 KiB / 1 KiB** |
| AtomVM `eavmlib` | 11 | 1,890 / 621 | 1 KiB / 0 KiB |
| OTP `kernel` | 104 | 34,773 / 6,795 | 21 KiB / 4 KiB |
| OTP `stdlib` | 98 | 225,048 / 17,079 | 137 KiB / 9 KiB |

Throughput 17.7 MB/s of code chunk on the host.

So: **8 KiB for the largest module an MCU actually runtime-compiles is already
being spent today.** A general symbolic IR over a 225 KB module is indeed out
of reach — but nothing that pays needs one. The analyses below want a per-x-register
lattice threaded through the existing forward pass: 16–32 registers × a couple
of bits, ~32 bytes, no extra pass and nothing persistent. That is the same
shape as the record-type tracking already living in the backend's `jit_regs`
state (`set_vm_record_type/3`, `get_vm_record_type/2`), which is invalidated
automatically on register writes, clobbering calls and labels.

**The fear is correct about the general case and does not apply to the specific
analyses.** What it should be spent on is ranked at the end.

## (b) Register type information for message sizes — no path

Taken as *runtime* shadow state (a type/size word beside each x register,
maintained as the program runs), this is a per-process cost paid on every
register write to serve one consumer, `send`. Rejected on first principles.

Taken as *compile-time* register state, it is (a), and it reaches only what (c)
reaches.

There is a third form that looked promising and is not: **the VM already knows
a received message's exact size for free.** A `Message` records `heap_end`, so
`heap_end - storage - 1` is the received term's word count with no traversal.
But `p1/1`, the estone forwarder, does not resend the message it received:

```erlang
p1(To) ->
    receive
        {_From, {message, X}} -> To ! {self(), {message, X}}, p1(To);
```

`erlc -S` shows the compiler already reusing the matched inner tuple
(`get_tuple_element` into `{x,0}`, then `put_tuple2` storing `{x,0}` straight
into the new tuple — `test_heap` is 3, not 6). The sent term is therefore
*the received message with a different outer tuple*, and relating its size to
the fragment's requires knowing the payload's extent inside the fragment, which
is the traversal being avoided. The relation is exact only for a **pure
forward** — `receive M -> To ! M end` — where the whole `MailboxMessage` could
be re-posted with no copy at all. That shape is rare; wrapping the payload is
the common idiom.

## (c) A sized `send` — coverage measured, ceiling measured

### How often is the size statically known?

Backwards intra-block dataflow from each `send` over `put_tuple2`/`put_list`/
`move`, with `self()`, atom, integer and nil literals as provably-immediate
producers:

| corpus | send sites | const size | const immediate | unknown |
|---|---:|---:|---:|---:|
| OTP stdlib+kernel+compiler+sasl | 483 | 8.5% | 2.9% | **88.6%** |
| AtomVM `estdlib`+`eavmlib` | 50 | 12.0% | 2.0% | 86.0% |
| benchmark app (estone, pingpong) | 37 | 35.1% | 8.1% | 56.8% |

Benchmarks send fixed synthetic shapes; real code sends variables. The 88.6%
are unknown because the payload arrives as an argument or out of a data
structure — the `p1` case, which no static analysis can size.

### What is one sizing pass worth?

Priced in-process on the estone shapes, against the real library:

| | ns |
|---|---:|
| `memory_estimate_shallow_usage`, 3-word message | 3.0–4.3 |
| `malloc` + `free`, 64 B | 9.7 |
| `memory_copy_shallow_term_to_storage` | 2.5 |
| whole size+malloc+copy+free | ~15 |

`pingpong_speed_test` runs 200,000 messages in 21,709 us = **108 ns/message**,
so the sizing pass is **~3% of a message round trip** and the whole message
block lifecycle is ~14%. Note `malloc`+`free` is 2.5x the sizing pass and that
9.7 ns is a lower bound (the loop hits one size class repeatedly; the VM
separates the allocation from the free by a receive and a GC).

Against the ESTONE gap, all message components together are **+47k estones of a
~235k deficit, i.e. 1.9% of the score**, and they rank fourth. A sized `send`
addresses a fraction of a fraction. **Not built.**

## What the census found instead — shipped in `bc8249423`

Instrumenting `mailbox_message_create_from_term` over a full estone run:

```
shallow_calls=1768092 shallow_words=33804121 deep_calls=599022 deep_words=20610416
deep_hist  <=16:583422  <=64:0  <=256:0  <=1024:15600  >1024:0
bail depth=15600  improper_tail=0
bail boxedtag[REF]=351022  bail boxedtag[REFC_BINARY]=232400
```

**A quarter of all messages took the general term-tree path**, and almost none
of them for being large: 583,422 of 599,022 are 16 words or fewer and were
rejected on a *boxed tag*, not on depth or size. `memory_estimate_shallow`
accepted only immediates, tuples and lists.

A reference is the common case. Every `gen_server:call` sends
`{'$gen_call', {Pid, Ref}, Request}` and receives `{Ref, Reply}`, so **both
halves of every call in OTP-style code took the slow path.**

The fix sizes a boxed term from its header and copies its words, for exactly
those tags that hold no pointer into the sending heap and no mso-list linkage:
bignums, floats, heap binaries, external pids/ports/references, plain
references, and const (literal) refcounted binaries. A self-contained leaf
needs no depth budget, so the depth check moved to the two recursing branches.

Priced on identical terms in one binary:

| shape | words | single pass | general path | ratio |
|---|---:|---:|---:|---:|
| estone small | 9 | 18.0 ns | 32.9 ns | 1.82x |
| estone medium | 15 | 25.0 ns | 51.0 ns | 2.04x |
| message with a reference | 12 | 23.1 ns | 35.9 ns | 1.55x |
| `{'$gen_call', {Pid, Ref}, _}` | 10 | 17.6 ns | 29.0 ns | 1.65x |

End to end, `gen_server:call` over ten interleaved rounds, medians of the
per-call cost, ranges non-overlapping:

| | median | range |
|---|---:|---|
| before | 614 ns | 604–629 |
| after | 572 ns | 564–578 |

**6.8% per call round trip.** The estone deep-path count drops 599,022 →
248,000 (−59%).

ESTONE itself cannot resolve it: 31 interleaved rounds give
**1.0047x (bootstrap 95% CI 0.9908–1.0131)**, and in the same run the
reference-free `medium messages` component moved 15%. The application suite
shows no regression: aggregate **0.9971x (CI 0.9638–1.0081)**.

Regression test: `copy_terms19` round-trips every affected leaf, nested and
bare, with both processes garbage collecting while holding their copy. It was
verified to have teeth — marking `TERM_BOXED_SUB_BINARY` self-contained makes
it fail.

### Left on the table, sized

- **232,400 estone messages** still bail on a non-const refcounted binary. The
  shallow copy would have to append to the mso list and increment the
  refcount, as `memory_scan_and_copy` does. About 40% of the remaining
  opportunity, at roughly the per-message deltas above.
- **15,600** bail on depth: `msgp_huge` sends `very_big(15)`, 996 words at
  nesting depth 48, against `SHALLOW_MAX_DEPTH 8`. Raising the limit trades
  against C-stack recursion, which is what the limit is for.

## Where the ESTONE gap actually is

AtomVM/BEAM **0.907x**. Ranked by recoverable estones, not by ratio:

| component | ratio | recoverable | share of score |
|---|---:|---:|---:|
| Bif dispatch | 0.65x | +111k | 4.4% |
| pattern matching | 0.92x | +98k | 3.9% |
| Small Integer arithmetic | 0.61x | +96k | 3.8% |
| messages (small/medium/huge) | 0.39–0.46x | +47k | 1.9% |
| Links | 0.34x | +39k | 1.5% |

Messages rank fourth: **perfect** message handling is worth 1.9%.

The top three are all static-analysis targets, which is where (a) should be
spent. `bif_dispatch` is `erts_debug:flat_size/1` and `demonitor/1` through
`call_ext`, whose import is a compile-time constant; an earlier profile put
43.5% of that component in the generic `call_ext` path. Pattern matching is
`is_tuple`/`test_arity`/`select_val` chains that propagated types would thin.
Small-integer arithmetic is `gc_bif` with operands whose range is often known.

Two cautions on the target itself. The ESTONES score is dominated by
`pattern matching` at ~46% of the total, and that component completes in 0–1 ms
per run, so the headline number is partly timer quantisation — total measured
time is the better instrument. And the noise floor is ±1.3% on the ESTONE
total, ±3.4% per component, and far worse on the message micros, so nothing
below a few percent can be demonstrated on this benchmark at all.
