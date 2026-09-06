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

## Relocatable tuples, and where GC time actually goes — 2026-09-06

A follow-up proposal: cap tuple arity, spend a bit of the arity field on a
"relocatable" flag, and let such a tuple carry its whole subtree contiguously
with a total size, so message copy and GC can `memcpy` it instead of recursing.

### The bit is affordable

| | limit |
|---|---|
| BEAM tuple arity | **16,777,215** (2^24-1), confirmed on OTP 29: `make_tuple(16777216)` is `badarg` |
| AtomVM boxed header | `(size << 6) \| tag`, 6-bit tag |
| AtomVM arity field | 58 bits (64-bit), **26 bits (32-bit)** |

Spending one bit leaves 25 bits on a 32-bit target — 33,554,431, still twice
BEAM's own limit. Two caveats: boxed tag `0x1C` is **not** free (`term.h` marks
it reserved, `libs/jit/src/term.hrl` would misidentify it as a boxed number),
and `term_get_size_from_boxed_header` is shared by every boxed type, so a mask
lands on the GC scan and copy paths.

### Maintaining the status is cheap; carrying the size is the open part

A tuple whose elements are all immediates is closed and contiguous by
construction, so `put_tuple2` can set the flag for free, and a destructive
tuple update keeps it as long as the written value is an immediate. For that
case the total size *is* `arity + 1`, already in the header: the flag needs no
extra storage. `memory_copy_shallow` also lays a message out depth-first, so
every nested tuple inside a message fragment is already contiguous.

What is not free is a compound tuple: `{Self, {message, X}}` has total size
`3 + size(X)`, and there is nowhere in the header to put that number. Either
the tuple grows a word (a 2-tuple goes 3 -> 4 words, +33% on every pingpong
message), or the arity field splits into arity plus total-size with a
"doesn't fit, fall back" encoding.

### What it would be worth

**Message copy.** Already measured above: on the estone shapes the single pass
costs 18-25 ns of a **108 ns** message round trip, of which the sizing pass is
3-4 ns and the copy 2.5 ns. Replacing both with a header read and a `memcpy`
saves ~4 ns, about 4% of a message, on components worth 1.9% of the score. The
deep case is where it would pay -- `msgp_huge` is 996 words at depth 48 -- and
that is 4% of the weight.

**GC.** Measured by timing `memory_gc` (two `clock_gettime` calls at a
calibrated 17.4 ns each, subtracted):

| | collections | live words copied | mean live/collection | real GC share |
|---|---:|---:|---:|---:|
| estone | 1,442,919 | 44,504,136 | 31 | **~10%** |
| app suite | 420,797 | 6,865,598 | 16 | **~11%** |

That is ~1.6 ns per live word copied. But the live set per collection is tiny:
**72% of estone collections and 99.5% of the app suite's leave 8 words or
fewer**, and not one collection in either run was a full sweep.

### The finding that matters: 95% of collections are forced by a message fragment

`memory_ensure_free_with_roots` contains

```c
bool should_gc = free_space < size || (alloc_mode == MEMORY_FORCE_SHRINK)
    || c->heap.root->next != NULL;
```

and `mailbox_message_dispose` appends every received message as a fragment
(`memory_heap_append_fragment`). So a process that receives a message collects
on its next allocation, whatever the memory pressure. Attributing the trigger:

| | out of space | force shrink | **heap fragment present** |
|---|---:|---:|---:|
| estone | 45,942 (3.2%) | 0 | **1,369,393 (94.9%)** |
| app suite | 11,016 (2.6%) | 0 | **400,057 (95.1%)** |

Introduced by `6309b7283`, "Run GC when there is a memory fragment to copy
message data".

So AtomVM's GC cost is a *collection count* problem, not a scan volume problem:
1.4M collections copying 31 words each. A relocatable representation makes the
31-word scan cheaper and cannot reduce the 1.4M. Tolerating fragments -- a
threshold on fragment count or words before forcing the fold -- attacks the
term that actually carries the cost. **Not yet attempted; sized here only.**

### Sharing, for the record

`erts_debug:size/1` against `flat_size/1` measures what an "own your contents"
rule would cost if it were ever applied at construction:

| term | shared | flat | blow-up |
|---|---:|---:|---:|
| `{A, A}`, A a 100-list | 203 | 403 | 2.0x |
| `{A, A, A, A}` | 205 | 805 | 3.9x |
| 100 references to one tuple | 225 | 2,700 | **12.0x** |
| `lists.erl` abstract code | 156,994 | 156,994 | 1.0x |

The worse hazard is not the final size but incremental construction: building
`lists:foldl(fun(X, Acc) -> {X, Acc} end, [], L)` with ownership copies costs
sum(2i) = 1,001,000 words for a 1,000-deep nest against 3,000 today, 333x.
This is why the flag can only ever be *maintained* (immediate arguments, or a
copy that already produces a contiguous layout), never *established* by
copying at `put_tuple2`.

### How many live tuples are actually flat?

"A tuple of immediates is relocatable for free, and the collector can create
relocatable tuples" is the strongest form of the proposal, and it is
measurable: census every tuple the collector walks, and count the ones whose
elements are all immediates (relocatable with no stored size, since the extent
is `arity + 1`).

| | scan words | tuples | tuple words | **flat tuple words** | share of tuple words | share of scan |
|---|---:|---:|---:|---:|---:|---:|
| estone | 20,762,309 | 1,108,298 | 6,586,041 | **827** | **0.0%** | 0.0% |
| app suite | 1,409,941 | 89,440 | 557,719 | **283,089** | **50.8%** | 20.1% |

The two workloads disagree completely, and the reason estone reads zero is
instructive: its forwarders' live set at collection time is ~3 words, because
the payload is already garbage by the time the fragment forces the collection.
What survives to be scanned belongs to the list and dataset micros —
5,223,979 list pointers (98.4% with an immediate head) against 6,586,041 words
of tuple, and every one of those tuples is compound.

The *closedness* half of the intuition does hold: boxed terms that are neither
tuple nor cons (references, binaries, maps, funs) are only 8.5% of estone's
scanned words and 4.1% of the app suite's, so 91-96% of live data is
structurally closed. What is not free is the other half — contiguity plus a
stored total size for a compound tuple.

And a marked subtree cannot be a pure `memcpy` even so: anything pointing at a
nested tuple inside it still needs a forwarding pointer, or the next collection
duplicates that subtree. The collector would skip *element* words but still
visit *headers*, which for a tree of 2-tuples is one word in three.

Ceiling, generously: tuple words are 32% (estone) and 40% (app) of the scan,
skipping two thirds of them cuts the collector by ~25%, and the collector is
10-11% of runtime — about **2-3%**, against a mask on every boxed size read (on
the scan path itself), a word per compound tuple or an arity/size split, a
depth-first layout in a breadth-first collector, and eight backends'
`test_arity`/`get_tuple_element`.

**Ordering matters more than the verdict.** While 95% of collections are forced
by a message fragment, collections are frequent and tiny and the scan is not
the cost. Fix that first and collections become rarer and larger — at which
point scan volume does start to dominate and this proposal deserves
re-measuring rather than dismissing.

## Avoiding the fragment-forced collection — measured

The follow-up suggestion was to use static analysis to skip the collection when
the received message is dropped. Measuring it turned up something simpler.

**A collection already ignores a dead message.** The collector copies live data
only, so a dropped message contributes zero copied words; that is why the mean
live set at a fragment-forced collection is 31 words while the message that
forced it is 9 to 33. The cost is not copying the message, it is *running a
collection at all*, 1.44M times.

**Removing the rule outright is worse.** Building with the
`c->heap.root->next != NULL` clause deleted:

| | estone total | max RSS |
|---|---:|---:|
| baseline | 1.038 s | 31.4 MB |
| never fold on fragments | 1.148 s | 87.9 MB |

**Batching the fold does cut the work, a lot.** Folding only once the chain
reaches N fragments, counted rather than timed:

| threshold | collections | live words copied | mean |
|---:|---:|---:|---:|
| 1 (today) | 1,442,919 | 44,504,136 | 30.8 |
| 2 | 768,382 | 27,693,718 | 36.0 |
| 8 | 310,399 | 13,379,656 | 43.1 |
| 32 | **123,719** | **7,563,517** | 61.1 |

11.7x fewer collections and 5.9x fewer copied words at N=32, with resident
memory unchanged at 31 MB (unlike the unbounded ablation above).

**And yet it measured 19% slower.** 25 interleaved rounds, N=32 against
baseline: ESTONES 1.0017x (CI 0.9956-1.0053), total measured time 799.1ms ->
953.9ms (CI 0.8286-0.8574).

The reason is the point of the original design. Folding on the *first* fragment
keeps the chain length at one, which is what makes every chain walk O(1) --
and `memory_heap_memory_size` walks the chain on **every allocation** under
`FibonacciHeapGrowth`, not on every collection. Batching turns that into O(N)
per allocation to save O(1) per collection, and allocations vastly outnumber
collections. The threshold test itself had the same flaw.

**So the batching needs O(1) fragment accounting**: a count and a word total
maintained in `struct Heap` by `memory_heap_append_fragment` and
`memory_heap_alloc_new_fragment`, reset by a collection, with
`memory_heap_memory_size` reading the total instead of walking. Two extra
fields shift `struct Context`, whose offsets eight JIT backends hardcode --
`jit.c`'s `_Static_assert`s catch it immediately and name the required values,
so the update is mechanical but real.

**The prize, if the per-word model holds.** Fitting
`total = calls x F + words x W` across both benchmarks gives F ~ 0 and
W ~ 1.78 ns/word, i.e. collection cost tracks copied words. Cutting copied
words 5.9x would take the collector from ~10% of runtime to ~1.7%, about
**8% of total runtime** -- larger than anything else measured in this
document. **Not yet demonstrated:** it depends on the O(1) accounting landing
first, and on the per-word model holding once collections become rarer and
larger.

On the static-analysis form specifically: proving the message is dropped does
not avoid copying it, since the collector already does not copy dead data. It
could avoid the *collection*, but only with a proof that nothing live points
into the fragment, and checking the roots alone is unsound -- `put_tuple2` can
store a fragment-derived pointer into a heap-allocated term. A sound version
needs a write barrier or an analysis proving no fragment-derived value is
stored into the heap between `remove_message` and the fold point.
`jit_liveness` already computes per-label live-in masks, so such an analysis
has a home. But the counter-based batching gets most of the same benefit with
no analysis at all, and should be tried first.

## Confirmed: the fragment at `test_heap` is the *previous* message, and nothing live points at it

`p1/1` reserves its heap **before** it removes the message:

```
{loop_rec,{f,6},{x,0}}.          %% x0 points into the incoming Message block,
                                 %% still mailbox-owned, not yet a fragment
{get_tuple_element,{x,0},1,{x,0}}.
{test_heap,3,1}.                 %% <-- the collection happens here
remove_message.                  %% <-- the block becomes a fragment only now
{put_tuple2,{x,1},{list,[{x,1},{x,0}]}}.   %% payload stored BY POINTER
send.                            %% malloc + size + copy into a new block
{call_last,1,{f,2},1}.           %% live from here: To, an immediate
```

So the fragment chained at the collection is always the message from the
*previous* iteration, dead since that tail call. Instrumenting every collection
whose only trigger is `c->heap.root->next != NULL`, and testing whether any
live x register or stack slot points into a fragment:

| workload | fragment-only collections | live x reg in a fragment | stack slot in a fragment | **either** | live x reg in an un-adopted Message |
|---|---:|---:|---:|---:|---:|
| `p1` forwarding, 100k | 100,015 | 15 | 0 | **15 (0.01%)** | 99,996 |
| estone | 1,369,394 | 15,973 | 175,571 | **191,527 (14.0%)** | 1,002,952 |
| app suite | 400,057 | 2 | 0 | **2 (0.0005%)** | 0 |

**86% of estone's forced collections and essentially all of the application
suite's have nothing live pointing into a fragment at all.** In the forwarding
loop the live register points into the *un-adopted mailbox Message* (99,996 of
100,015) — the message being matched, which is not a fragment yet.

### Why the direct-root test is necessary but not sufficient

A live root can reach a fragment *through* a heap term, and in `p1` it
literally does: `put_tuple2` stores the fragment pointer into the outgoing
tuple. At the `test_heap` above that has not happened yet, but at any later
allocation site it would, and a roots-only test would wrongly conclude the
fragment is dead.

Postponing is always safe — the fragment stays chained, nothing is freed — but
that is the batching already measured above, and it regrows the chain.
*Freeing* needs a transitive argument, which is either the collection itself,
or one of:

- the JIT's liveness at the tail call, which already knows the live-in mask at
  label 2 is x0 alone holding `To`, never fragment-derived; or
- **not creating the fragment at all** — reusing the incoming block for the
  outgoing message, since the two differ in exactly one immediate word.

The second subsumes the first for every forwarding shape.

## The metric to optimise is time, not estones

One estone run, per component:

| component | time | estones | share of time | share of score |
|---|---:|---:|---:|---:|
| small messages | 412 ms | 7,510 | 41% | 0.3% |
| medium messages | 394 ms | 15,407 | 39% | 0.7% |
| huge messages | 44 ms | 11,271 | 4% | 0.5% |
| Generic server | 73 ms | 33,950 | 7% | 1.4% |
| **pattern matching** | **0 ms** | **1,124,818** | **0%** | **48%** |
| all the rest | 78 ms | 1,153,467 | 8% | 49% |

Message passing is **84% of the wall time and 1.5% of the score**, and the
score is half-decided by a component that finishes in zero measurable
milliseconds. AtomVM runs estone in ~1.01 s against BEAM's ~0.51 s. Ranking
work by recoverable *estones*, as the sections above do, points at BIF dispatch
and pattern matching; ranking it by *time* points at message passing and
nothing else.

## Forwarding by handing over the message block — prototyped, measured, not shipped

`p1/1` receives `{From, {message, X}}` and sends `{self(), {message, X}}`: the
same arity, the same payload, one word different, and that word an immediate.
Written by hand you would patch the incoming block and re-post it.

### The runtime mechanism works

Built and measured: `Context.forward_pending` (added at the end of the struct,
where nothing's offset is pinned), a `PRIM_REMOVE_MESSAGE_KEEP` that takes the
block out of the mailbox without folding it into the heap, and a
`PRIM_FORWARD_MESSAGE` that checks the outgoing term is the incoming root with
only immediate differences, patches those words and posts the block, falling
back to a normal send otherwise.

A chain of four forwarders, so four hops in five are forwards:

| message words | AtomVM before | AtomVM forwarding | BEAM | vs before | vs BEAM |
|---:|---:|---:|---:|---:|---:|
| 7 | 127 ns | **68 ns** | 214 ns | 1.87x | 3.1x |
| 601 | 1,188 ns | **572 ns** | 1,777 ns | 2.08x | 3.1x |
| 6,001 | 8,601 ns | **2,049 ns** | 13,990 ns | **4.20x** | **6.8x** |

The gain grows with payload because a forwarded hop stops being O(payload); the
residual scaling is the one hop in five that still builds a fresh message.
`test-erlang` and the C suites pass with it enabled.

### Why it is not shipped

The recogniser that decides where to emit it reasons about **shape only**: a
`remove_message`, then immediate-producing opcodes, then `send`, then an
overwrite of x0 and a tail call. That is not enough. Nothing in it establishes
that the value reaching the tail call is not *derived from the message*:

```erlang
p1(To) ->
    receive
        {_From, {message, X}} -> To ! {self(), {message, X}}, p1(X)
    end.
```

Here `X` points into the block after it has been handed to `To`, and the target
may collect and free it. OTP 29 happens to compile this to a
`get_tuple_element` rather than a `move`, which this recogniser rejects — but
that is an accident of one compiler version, not a property to rely on. A
shallow root scan at run time does not close it either, since a heap term
reachable from a live y register can hold the block pointer indirectly.

**What it needs:** provenance tracking — which registers hold values derived
from the message being received — carried from `loop_rec` to the send, with the
tail call's live arguments checked against it. That is the per-register
provenance discussed above, and this is the case that justifies it. It cannot
be done from the `remove_message` onwards alone, because a store into a y
register can happen between `loop_rec` and `remove_message`; it wants either a
pre-pass in the shape of `jit_liveness:analysis/1`, or provenance threaded
through `emit_pass` with a conservative poison on every unmodelled opcode.

The prototype is kept in `tools/dev/forward-message-prototype.patch` (it also
carries the live-register fragment-skip experiment).
