<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Large maps: CHAMP instead of the ordered tree — 2026-09-07

Replaces the persistent B-tree that backed maps above `TERM_MAP_HASH_THRESHOLD`
with a CHAMP trie, then adds a stored hash per entry and a scheduler-thread memo
that carries an iterated key's hash to the next lookup. Both steps are measured
against the same B-tree baseline, built from the same commit on the same
machine, in the same sitting.

## Why

`unicode_util.erl` is the one file of 280 where AtomVM's `erlc` loses to BEAM on
wall-clock, and its profile is 55.5% map-and-compare work: `node_find` 26.7%,
`term_compare0` 17.0%, `bt_insert` 7.8%. 57% of its tree lookups are on maps of
more than 1024 entries and 36% on maps of 4k–16k, so an ordered tree pays
log2(n) key comparisons per lookup — and its keys are compound terms, where the
comparison, not the descent, is the expensive part.

A hash trie pays one hash and one comparison whatever the size. The earlier
HAMT experiment replaced the B-tree wholesale and came out worse; CHAMP differs from it in the two ways that mattered there —
contiguous inline entries per node, and a canonical form — and a prototype
measured 0.41–0.70x of the B-tree's lookup time on the key shapes
`unicode_util` actually uses.

## Design

A node is an ordinary boxed tuple, so the collector, term copier, term hasher
and the JIT's inlined `map_size` need no new cases:

    {DataMap, NodeMap, K0, V0, .., Kd-1, Vd-1, N0, .., Nn-1}

- **16-way, 4 bits per level**, the fan-out ERTS gives its hashmaps. The two
  bitmaps are separate small integers because one 32-bit word would not fit a
  32-bit build's 27-bit small-integer range.
- **Canonical**: a sub-node that would hold one entry is inlined into its
  parent, so equal maps have equal shapes and equality is a parallel walk that
  skips pointer-identical sub-nodes.
- **Collision nodes** hold entries whose hashes are fully equal. Growing one is
  the only allocation a put cannot bound by a constant, so the root records
  whether the map contains one at all; every other map — every map in practice
  — reserves a constant instead of something proportional to its size.
- **Bulk build** counting-sorts the entries by slot, level by level,
  alternating between two index arrays. Above 128 entries the scratch is
  borrowed from the heap words the caller already reserved, which keeps the
  build allocation-free and therefore infallible: it runs from a JIT primitive
  with no way to report a failure.

### What changes for callers

A trie enumerates its entries in hash order, not key order. Erlang's map
ordering is observable in three places and each now sorts explicitly —
comparing two maps, encoding to the external term format, and printing — while
iteration (`maps:next` and the `keys`/`values`/`merge` NIFs) walks in trie
order.

`term_hash`'s map case had to become **order-independent**: the same entries can
sit in either representation depending on how the map was built, and a map is
itself a valid map key, so it now sums each entry's own hash rather than folding
them in sequence. Getting this wrong makes a lookup find a map key or not
depending on the history of the term.

## Correctness

- **OTP corpus, byte-identical output.** An AtomVM-hosted `erlc` built against
  each backend compiled `compiler`, `stdlib`, `kernel`, `sasl` and `crypto`:
  **279 of 279 sources produced byte-identical `.beam` files.**
- `test-erlang` (JIT and emulator builds), `test-heap`, `test-structs`,
  `test-term`, `test-bitstring`, `test-mailbox`, and the `estdlib`, `eavmlib`,
  `etest`, `alisp`, `exavmlib` and `jit` suites pass. The only estdlib failures
  are `test_serial_dist_socat` and `test_net_kernel`, which fail identically on
  the B-tree baseline (known-flaky, environment-dependent).
- The `test-heap` CHAMP unit test inserts 2000 scrambled keys with a collection
  between every insert, checks every lookup, the sorted materialization and the
  cursor, updates every third entry, removes every key down to the empty trie,
  and checks that a bulk build and the same entries inserted one at a time are
  structurally equal.

### Two bugs the corpus diff found

1. **Stack buffer overflow converting a large flat map.** `maps:from_keys/2` and
   the external term format both build flat maps of unbounded size, so a flat
   map far larger than the threshold can meet its first insert. The bulk build
   sized its index scratch for the threshold instead of for the map: 20 of 279
   sources crashed the VM (`sets:from_list/1` + `sets:add_element/2` is the
   shape that reaches it). Fixed by borrowing the scratch from the heap above
   128 entries. Regression test: `test_large_flat_map_conversion`.
2. **Representation-dependent map hashing** (above). Regression test:
   `test_large_map_as_key`.

## Measurement

Method: every round runs each engine once on the same work and rotates which
goes first, so thermal drift is shared rather than attributed to whichever
engine happens to run last. Ratios below are **A/B**, so greater than 1 means B
is faster. Baseline `A` is the B-tree at `0ade10faf`; every artifact (VM,
`atomvmlib`, AOT images, the AtomVM-hosted `erlc`) was rebuilt from each tree in
the same sitting. Reference BEAM is OTP 29 (`emu_flavor=jit`).

Drivers: [`bench_ab_atomvm.py`](bench_ab_atomvm.py) for ESTONE,
[`bench_erlc_ab.py`](bench_erlc_ab.py) for the `erlc` corpus (`compiler`,
`stdlib`, `kernel`, `sasl`, `crypto`; 279 sources both engines compile).

### Step 1 — CHAMP, no stored hash

| benchmark | result |
|---|---|
| ESTONE (common subset, score) | **0.9973x** (bootstrap 95% CI 0.9917–1.0057) |
| `erlc`, 279 files, ratio of per-file median sums | **0.9880x** (paired bootstrap 95% CI 0.9768–0.9990) |

ESTONE is neutral, which is what it should be: it barely uses maps above the
flat threshold. The `erlc` corpus is **1.2% slower**, faster on 149 of 279 files
and slower on the rest — and `unicode_util`, the file the whole exercise is
aimed at, is **1.046x faster** (2535.6 ms → 2423.5 ms, 0.871x → 0.911x of BEAM).

A caveat on the per-file numbers: AtomVM's per-file timings are quantized in
roughly 60 ms steps (149.8, 209.8, 269.8, 329.9 ms and so on), so a single
file's ratio can move a whole step for no reason. The aggregate over 279 files
is sound; individual rows below +/-1 step are not. The runs after this one
therefore also measure **batch** mode, which compiles a whole application in one
process and so has no per-process startup to quantize.

### Step 2 — hash an atom by its index, not its characters

The step-1 regression had one dominant cause, and it was not the trie. AtomVM's
`hash_atom` read the atom's **characters**; the ordered tree it replaced never
did, because `atom_table_cmp_using_atom_index` answers almost every atom pair
from an eight-byte sort key cached on the atom-table node. Moving lookups onto a
hash therefore made every atom-keyed map -- most of the Erlang compiler's --
pay a text walk it had never paid before.

An atom's identity is its table index: interned, unique, and never renumbered.
`term_hash` backs internal hash tables only (maps, ETS buckets,
`persistent_term`), none of which needs a value stable across runs, so hashing
the index is sound. Commit `0f0391542`.

| benchmark | CHAMP alone | + atom-index hash |
|---|---|---|
| ESTONE (score) | 0.9973x | **1.0008x** (CI 0.9982-1.0118) |
| `erlc`, 279 files, per-file | 0.9880x | **1.0026x** (CI 0.9910-1.0137) |
| `erlc`, batch (one process per app) | — | **1.0031x** |
| `unicode_util` | 1.046x | **1.055x** |

So the trie reaches **parity** with the tree across the corpus, and wins on the
file the exercise was aimed at: `unicode_util` goes from 0.873x of BEAM to
**0.921x**, and `erl_parse` from 1.128x to 1.157x.

Batch, per application (A = B-tree, B = CHAMP + atom-index hash):

| app | BEAM | A | B | B/A |
|---|---:|---:|---:|---:|
| compiler | 7.434s | 7.049s | 7.080s | 0.996x |
| stdlib | 14.862s | 14.830s | 14.668s | **1.011x** |
| kernel | 6.527s | 6.310s | 6.311s | 1.000x |
| sasl | 0.955s | 0.801s | 0.843s | 0.950x |
| crypto | 0.471s | 0.324s | 0.321s | 1.009x |
| **total** | **30.249s** | **29.313s** | **29.223s** | **1.0031x** |

Against BEAM the corpus is 1.032x (tree) and 1.035x (trie).

### Step 3 — store each entry's hash, and carry it from walk to lookup

The idea: put the hash beside the key in the node, so that a slot collision
moves a resident entry without rehashing it, a lookup that lands on the wrong
key rejects it by comparing hashes rather than terms, and — the reason for the
whole thing — iteration hands the hash out for free, where a one-entry
scheduler-thread memo can carry it to the following lookup in another map. That
"walk one map, look each key up in another" shape is what the compiler was
believed to spend its map time in.

Storing the hash costs a word per entry (entries go from two words to three) and
narrows the hash from 32 bits to 28 so it fits a small integer on a 32-bit
build, costing one level of trie depth.

**It does not pay.** Against step 2, on the same machine in the same sitting:

| benchmark | result |
|---|---|
| ESTONE (score) | **0.9950x** (CI 0.9887–0.9976) |
| `erlc`, 279 files, per-file | **0.9938x** (CI 0.9878–1.0001) |
| `erlc`, batch | **0.9842x** |
| `unicode_util` | **0.979x** |
| `erl_parse` | **0.970x** |

Every figure is negative, and the two files the exercise was aimed at are the
two that regress most.

#### Why: the memo almost never fires

Counting hits with an instrumented VM (`-DAVM_COUNT_MAP_MEMO`) compiling the two
files:

| file | map lookups | memo hits | hit rate |
|---|---:|---:|---:|
| `unicode_util` | 21,437,995 | 5,670 | **0.026%** |
| `erl_parse` | 3,174,242 | 8 | **0.000%** |

Iteration recorded 2.9M hashes while compiling `unicode_util`, and essentially
none of them was ever asked for again. So the premise is wrong for this
workload: AtomVM's 21M map lookups do not come from walking one map and looking
its keys up in another. They come from `get_map_elements` and `maps:get` on keys
that arrive from pattern matching and from the caller — keys the iteration
never saw. (The memo is also dropped on every collection and every process
switch, 188k times here, which would break even a genuine pair straddling one.)

What is left is the cost: a thread-local load and compare on all 21M lookups,
and a third of the map's memory again in stored hashes, which the collector then
has to copy.

#### Why the premise cannot hold: the loop iterates the wrong side

Only a hash-backed map runs the trie cursor, and only the cursor knows a stored
hash, so only iterating a *large* map can feed the memo. But a subset-style loop
iterates the **small** side and probes the large one. Counting which side
`maps:next` actually walks:

| file | `maps:next` on hash maps | on flat maps | mean flat size |
|---|---:|---:|---:|
| `unicode_util` | 2,891,597 | 2,120,470 (42.3%) | 2.6 entries |
| `erl_parse` | 29,811 | 2,363,013 (98.8%) | 4.8 entries |

For `erl_parse` that settles it on its own: 98.8% of iteration is over flat maps
of about five entries, which record nothing, which is why it takes 8 memo hits
in 3.2M lookups. For `unicode_util` the large maps *are* iterated, so a second
effect dominates there: a one-entry memo is overwritten by the next `maps:next`
before any lookup runs, which is exactly what a `maps:to_list`- or
`maps:keys`-shaped full walk does.

Making the shape pay would require storing hashes in **flat** maps too, so that
the small side's iteration also yields one — a word per entry in every small
map, far more than it could save.

#### Which half cost what

Rebuilding with the memo compiled out (`-DAVM_MAP_MEMO_OFF`) but the hash still
stored separates the two, batch mode, against step 2:

| variant | `erlc` batch |
|---|---|
| stored hash, no memo | **0.9920x** |
| stored hash + memo | **0.9842x** |

Both halves lose independently. The extra word costs about 0.8% on its own --
entries go from two words to three, roughly a third more map memory for the
collector to copy -- and the memo costs another 0.8% for a thread-local load and
compare on every one of 21M lookups, in exchange for a 0.026% hit rate.

**Step 3 is therefore not kept.** The code is parked in
[`champ-stored-hash-memo.patch`](champ-stored-hash-memo.patch), which also
carries the `AVM_COUNT_MAP_MEMO` instrumentation used above.

## What BEAM actually does

Worth recording, because it reframes the result. BEAM does not have a faster
hash and does not cache one per atom. Its **loader precomputes the hash of every
literal map key and bakes it into the instruction**
(`erts/emulator/beam/generators.tab`):

```c
gen.get_map_element(Fail, Src, Size, Rest) {
    key_term = beam_load_get_term(S, Key);
    if (is_value(key_term)) {                       /* the key is a literal */
        $BeamOpNameArity(op, i_get_map_element_hash, 5);
        op->a[3].val = (BeamInstr) hashmap_make_hash(key_term);
    } else {
        $BeamOpNameArity(op, i_get_map_element, 4); /* runtime key: hash at lookup */
    }
}
```

`gen.get_map_elements` does the same for every key of a multi-key fetch (three
words per key: source, destination, **hash**). The runtime entry point is
`get_map_element_hash(map, key, hx)`, which skips `hashmap_make_hash` entirely
and only asserts equality in a debug build, and BeamAsm uses the same path
(`emit_i_get_map_element_hash`).

So on `get_map_elements` with a literal key -- which is what record-style map
access compiles to, and the bulk of the compiler's map reads -- BEAM pays **no
hash at all**, while AtomVM pays one on each of `unicode_util`'s 21.4M lookups.
That is the remaining structural gap, and it is invisible from the tree, which
never hashed.

## Verdict

- **Keep** the CHAMP backend and the atom-index hash. Together they are at
  parity with the tree across the corpus (1.003x batch, 1.0026x per-file,
  1.0008x ESTONE) while winning on the files the exercise targeted
  (`unicode_util` 1.055x, `erl_parse` 1.025x), and they reserve far less heap
  per put: a constant ~450 words against the tree's `(height+1)*3` nodes of 47
  keys, which was 2664 words at 4k entries.
- **Do not keep** the stored hash or the walk-to-lookup memo. Measured, both
  lose, and the premise behind the memo does not hold for this workload.

Parity alone would not justify the churn. What does is that the trie is the
representation that can *use* a precomputed hash: with literal-key hashes baked
in at JIT-precompile time, the hash leaves the hot path and the trie's single
key comparison beats the tree's log2(n). The tree can never benefit from that
work, because it never hashes.

## Addendum: three things that looked worth doing and are not

Each was measured rather than argued, and none is kept.

### Precomputing literal-key hashes, as BEAM's loader does

Retracted. Censusing what is actually hashed while compiling settles it:

| hashed key shape | `unicode_util` (24.7M) | `erl_parse` (4.2M) |
|---|---:|---:|
| small integer | 59.8% | 55.6% |
| tuple | 39.9% | 43.1% |
| **atom** | **0.1%** | **1.2%** |

| hashed by operation | | |
|---|---:|---:|
| get / put / bulk build / collision rehash | 73.2 / 13.7 / 7.7 / 5.4% | 54.1 / 21.4 / 20.3 / 3.9% |

Almost nothing hashed here is a literal. The keys are SSA variable identities --
small integers and `{b_var, N}` tuples -- computed at runtime, which a loader
transform cannot reach. Literal atom keys are what record-style access uses, and
those maps stay under the flat threshold and never hash at all: AtomVM already
gets for free what BEAM's transform exists to recover. BEAM needs it because it
switches to hashmaps at 32 entries where AtomVM switches at 128.

### Replacing the byte-at-a-time integer hash

`hash_integer` folds a value one byte at a time, which looks obviously wrong,
and a murmur3 finalizer measured **2.15x** faster on small integers and **1.56x**
on `{b_var, N}` tuples standalone -- with far better distribution, the
chi-square of a 4-bit trie slice over 200k sequential keys falling from 2,464
(bits 16-19) and 150,349 (bits 20-23) to about 24, against an ideal of 15.

On the corpus it measured **0.9971x**: slightly slower. The magnitude census
says why:

| hashed integer | share |
|---|---:|
| zero | 16.6% |
| < 256 | 4.4% |
| 2 bytes | 76.5% |
| 3 bytes | 2.5% |

The loop's trip count is nearly constant in this workload, so its branch
predicts and costs almost nothing, and at two iterations the two versions run a
comparable number of ALU operations -- while for the 16.6% of keys that hash
zero the loop does no iterations at all against the finalizer's seven
operations. The standalone benchmark had mixed 1- and 2-byte values and was
measuring branch misprediction that does not occur here.

The distribution skew is real but does not bite at these sizes: 16^4 slots
already exceed the largest maps, so the levels that consume bits 16 and above
are never reached. It would bite on maps beyond 65k entries.

### Storing a precomputed hash per atom instead of the 8-byte sort key

No: they serve different operations. `sort_key` answers *ordering*, which a hash
cannot, and ordering (`term_compare0`, 11.4% of self time) is the single largest
cost in the profile, against atom hashing at 0.1% of hashes. A stored hash would
also be slower than what is there now -- hashing the atom index is a few ALU
operations on a value already in the term register, where a stored hash means a
pointer chase into the atom table. The footprint concern behind the question is
already handled: `ATOM_TABLE_SORT_KEY_CACHE` is compiled out on 32-bit targets
because the `uint64_t` doubled `sizeof(struct HNode)` and pushed esp32c3 out of
memory.

### Where the time actually goes

Self time sampling an `erlc` run over `unicode_util`:

| symbol | self |
|---|---:|
| `term_compare0` | 11.4% |
| `champ_put_rec` | 10.3% |
| `termmap_champ_get` | 8.5% |
| `hash_term_incr` | 8.3% |
| bulk build (`champ_size_rec` + `champ_build_rec` + `champ_partition`) | 5.1% |

Comparison, not hashing, is still the largest single cost, and map *insertion*
costs as much as lookup -- which is where the remaining headroom is.

## Follow-ups, in the order they look worth doing

1. **Stop sorting in `maps:keys/1` and `maps:values/1`.** Step 1 made
   `map_hash_array` sort so that `maps:merge/2`'s two-pointer merge kept working,
   but Erlang does not specify the order and BEAM returns hash order for
   hashmaps; the tests already `lists:sort` the result. Splitting the sorted and
   unsorted uses drops an O(n log n) `term_compare` pass from both.
2. **Raise `MAPS_MERGE_SMALL_MAX`.** Merging a small map into a large trie is
   O(m) puts with no scratch and no sort; the cutoff of 8 was tuned for the tree.
3. **Revisit `TERM_MAP_HASH_THRESHOLD` (128).** A hashed lookup should overtake
   a binary search well before 128 entries; BEAM switches at 32.

# Part two, 2026-09-08: insertion, and where a hash should be kept

Follow-up session. Three questions: revisit insertion, try caching atom hashes,
and work out how many bits are available for a hash and whether it makes sense
to keep one in a map or a tuple. Method is unchanged from Part one -- interleaved
A/B, every round running each engine on the same work and rotating who goes
first -- with one refinement: for a change aimed at map-heavy code, the same VM
binary is spliced into the *same* erlc payload for both engines, so the only
difference between A and B is the change itself, and a new driver
([`bench_erlc_file_ab.py`](bench_erlc_file_ab.py)) concentrates runs on
`unicode_util` and `erl_parse` where the corpus average buries the effect.

Ratios are A/B throughout, so greater than one means B is faster.

## What shipped

| commit | erlc corpus (batch) | `unicode_util` | `erl_parse` | ESTONE |
|---|---:|---:|---:|---:|
| Move a node's payload as words | 1.0044x | -- | -- | -- |
| + decide shallow equality at the call site | **1.0098x** | 1.0162x | 1.0087x | 0.9990x |
| Cache an atom's hash | **1.0036x** | 1.0037x | 1.0023x | -- |

Every application of the corpus moved the same way in all three. All 279 sources
still compile byte-identically at each step.

### Insertion: the copy, not the algorithm

`champ_put_rec` and `champ_remove_rec` rebuilt each node on the path a term at a
time through `term_put_tuple_element`, which re-derives the destination pointer
and -- since source and destination are both `term *` -- leaves the compiler
unable to widen the copy. The destination is a tuple allocated moments earlier
by a bump allocator and cannot overlap its source, so the runs move with
`memcpy`. A node near the root holds up to sixteen entries, so a put on a large
map was moving something like fifty words one at a time.

On its own this is 1.0044x on the batch and 1.0024x over the files, paired
bootstrap CI 0.9953-1.0097 -- consistent in direction across all five
applications but not resolvable against the noise floor.

### Comparison: the largest block in the profile

`term_compare` already resolves the shapes a map descent meets without its
general machinery, but reaching that code costs a cross-translation-unit call
plus an ordering prologue. Lifting the shallow test into `term.h` as
`term_exact_equals_shallow` -- shared, so `term.c` and the trie cannot drift --
lets the descent decide inline. Paired with the word move this reaches 1.0098x
on the batch and 1.0088x over the files, **CI 1.0021-1.0154**, faster on 168 of
279. That is the pair clearing the noise floor the word move alone did not.

### The emulator's put_map walked every key twice

Both `put_map` opcodes navigated every key before touching the map -- assoc to
count new keys, exact to check none were missing -- and then hashed and
descended again to do the work. On a hash-backed source neither walk is needed:
the result is hash-backed whatever the count says and the reservation does not
depend on it, and the trie's put already reports whether the key was there. This
is the emulator's path, so it does not show in the AOT benchmarks; it is what an
MCU build without the JIT runs.

## Caching an atom's hash: the recorded reasoning was wrong

Part one argued a stored atom hash would lose, because hashing the index is a few
operations on a value already in a register where a cache means a chase into the
atom table. Measured, it wins: **1.0036x** on the corpus batch, 1.0037x on
`unicode_util` (CI 1.0018-1.0057). The operations are few but form a dependent
chain of four multiplies, and the atom table is small and stays hot. Atoms are
0.1% of the keys a large map hashes but most of what its tuple keys are made of,
which is where it shows up.

It is free where it is taken: on 64-bit the `uint32_t` lands in padding
`struct HNode` already had, so the node stays 32 bytes (now asserted). On 32-bit
it would take the node from 12 to 16 bytes, which is the growth that compiles the
sort key out on small MCUs, so it is gated the same way and the mix runs inline
at the call site there instead.

**A cheaper mix is not a substitute.** Replacing the murmur finalizer with a
single multiply measured 1.0060x on `unicode_util` and **0.9853x** on
`erl_parse`: what it saves in arithmetic it gives back in collisions. The mix is
earning its keep, so the only way to make it cheaper is to stop repeating it.

## How many bits, and where a hash should live

Censusing what `term_hash` is asked to hash while compiling:

| | `unicode_util` | `erl_parse` |
|---|---:|---:|
| calls | 24.7M | 4.2M |
| small integer | 59.8% of calls, 22.5% of work | 55.4% / 22.0% |
| **tuple** | **39.9% of calls, 75.4% of work** | **43.2% / 76.3%** |
| atom | 0.1% | 1.3% |

Tuples are where the work is. And the *same tuple object* is hashed again and
again: a direct-mapped table keyed by the term's address hits **87.1%** of boxed
hashes on `unicode_util` and **71.3%** on `erl_parse`.

That reframes the question. The value worth caching is not in the map -- it is
the hash of the *probe* key, the term you arrive with. This is why Part one's
stored-per-entry hash lost: it caches keys already in the map and never the one
being looked up.

### The bits

A boxed header is `size << 6 | tag`. On 64-bit the size field has 58 bits and
real arities need at most 24, so **34 bits are spare** -- room for a whole hash.
On 32-bit the field is 26 bits and there is no room worth taking. But
`term_get_size_from_boxed_header` is the GC's hottest accessor, and the JIT
emits `shift_right(..., 6)` at four sites shared by eight backends; masking a
hash out of the arity touches all of it. Header bits are also the *only* place
immune to staleness by construction, since the value is copied with the object
and dies with it -- which matters, as the next section shows.

### A memo outside the term measures better, and is the open question

Keyed by address, thread-local, invalidated by an epoch:

| | `unicode_util` | `erl_parse` | corpus batch | ESTONE |
|---|---:|---:|---:|---:|
| hash memo, 2048 slots | **1.0304x** | **1.0119x** | **1.0091x** | 0.9937x |

CIs 1.0275-1.0336 and 1.0105-1.0134. That is the largest effect measured in
either part of this work, and it is **not committed**, for two reasons.

**It regresses ESTONE** (0.9937x, CI 0.9886-1.0015), with the message-heavy
components down 4-5%. The epoch has to be bumped wherever a term's address can
be reclaimed, and `memory_heap_block_free` is on the message-passing hot path
while ESTONE's maps are flat and never hashed -- all cost, no benefit.

**Soundness is not finished.** Bumping the epoch only in `memory_gc` looked
sufficient: the census put address reuse at 0.00% and 0.05% of hits. It is not
sufficient, and reading a non-zero stale rate as "essentially nonexistent" was
the mistake -- one mis-hashed key lands in the wrong trie slot and is never
found again. That build compiled **37 of 279 sources wrong**. Bumping on every
`memory_heap_block_free` as well restores 279/279 and, notably, is also *faster*
than the unsound version, which was paying for its own collisions.

What remains open is SMP. A thread-local epoch covers a thread's own frees;
another scheduler freeing memory whose addresses this one has memoed does not.
Two ways to close it, and the choice is a design decision:

- a global epoch, release-stored before any free and acquire-loaded per hash --
  simple, but every scheduler's memo is then invalidated by every other
  scheduler's collections;
- confine the memo to terms the running process owns: bump the epoch when a
  scheduler dispatches a different process (`scheduler_run` is the chokepoint),
  and keep `ets_multimap` and `persistent_term` -- which hash terms living in
  shared storage another thread can free -- on an uncached entry point.

The prototype is parked in [`hash-memo-prototype.patch`](hash-memo-prototype.patch).
Table sizing, should it be picked up (entries are 16 bytes, and this is
per-scheduler memory that an MCU build would have to gate):

| slots | size | `unicode_util` | `erl_parse` |
|---:|---:|---:|---:|
| 512 | 8K | 43.1% | 61.2% |
| 2048 | 32K | 70.9% | 66.6% |
| 8192 | 128K | 86.6% | 69.8% |
| 32768 | 512K | 92.4% | 71.7% |
