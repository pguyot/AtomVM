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

## Follow-ups, in the order they look worth doing

1. **Precompute literal-key hashes in the JIT**, as BEAM's loader does, for
   `get_map_elements` and `has_map_fields`. This is the big one and it is what
   the CHAMP work unlocks.
2. **Stop sorting in `maps:keys/1` and `maps:values/1`.** Step 1 made
   `map_hash_array` sort so that `maps:merge/2`'s two-pointer merge kept working,
   but Erlang does not specify the order and BEAM returns hash order for
   hashmaps; the tests already `lists:sort` the result. Splitting the sorted and
   unsorted uses drops an O(n log n) `term_compare` pass from both.
3. **Raise `MAPS_MERGE_SMALL_MAX`.** Merging a small map into a large trie is
   O(m) puts with no scratch and no sort; the cutoff of 8 was tuned for the tree.
4. **Revisit `TERM_MAP_HASH_THRESHOLD` (128).** A hashed lookup should overtake
   a binary search well before 128 entries; BEAM switches at 32.
