<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Every comparison a compile makes, and what it says about the representation

Measured 2026-09-12 on macOS aarch64, from `w30/jit-edge` at
`c2470d72d` (CHAMP maps, atom hash cache, the inline `=:=` prefix).
Instrumentation: `tools/dev/term-compare-census.patch`, analysis:
`tools/dev/analyze_term_census.py`.

Earlier work sized three representation ideas from a *count* of where
`term_compare0` resolves ([ERLC_TERM_REPRESENTATION_IDEAS_2026-09-06.md]).
This census records the comparisons themselves -- both operands, what decided
them, how far the walk got, and, for atoms, how much of each name had to be
read -- so the representation questions can be answered from the workload
instead of from a plausible story about it.

## Method

A build with `-DAVM_TERM_CENSUS` records every *logical* comparison: the
outermost one, with the walks it makes internally counted as part of it rather
than as separate events. Three doors are instrumented, which between them are
every comparison the VM performs in C:

* `term_compare()`, the out-of-line comparator;
* `term_exact_eq()`, the inline `=:=` prefix that the map descents use, which
  resolves identical terms, distinct immediates and flat tuples of immediates
  without a call;
* `term_map_probe_tup_cmp()`, the flat-map probe, which resolves covered
  candidate keys inline during a binary search.

`atom_table_cmp_using_atom_index` records separately, so an atom pair is
counted where it costs: a lookup and possibly a name comparison.

After a comparison resolves, the recorder re-walks the pair with recording
suppressed and classifies it: which of the two operands' types, which element
of a tuple or list decided it, how deep the walk went, how many element pairs
it touched, and -- for two atoms -- the common prefix of the names and the
position of the first differing bit. The classification is keyed with the
call site *and its caller* (`__builtin_return_address(0)` and `(1)`), because
every map NIF reaches the comparator through the same two helpers and the
inner frame alone cannot tell `maps:remove` from `maps:keys`.

The workload is one `erlc` compile of a single OTP-29 source, run twice: once
with the AOT (JIT) payload, which is the shipped configuration, and once with
the plain-bytecode payload, which is what a target without a JIT backend runs.
The two differ enormously and the difference is itself a result.

Reproduction:

    cmake -B build.census -DCMAKE_BUILD_TYPE=Release -DAVM_DISABLE_SMP=ON \
        -DCMAKE_C_FLAGS="-DAVM_TERM_CENSUS -g"
    ~/atomvm_erlc/build_erlc_variant.sh aot build.census /tmp/erlc-census
    AVM_TERM_CENSUS_OUT=/tmp/census.json /tmp/erlc-census -o /tmp/out ... file.erl
    tools/dev/analyze_term_census.py /tmp/census.json label /tmp/erlc-census

Two caveats on reading the numbers. A comparison the JIT resolves in generated
code (two small integers, `=:=` on immediates) never reaches C and is absent
from the AOT column -- that is why the emulated column is 5-10x larger, and
why the emulated column is the honest count of what the *program* compares.
And a "comparison" is not a unit of cost: the entry-point breakdown below
separates the ones that cost a call from the ones that cost three
instructions.

## What a compile compares

AOT payload, the shipped configuration:

| file | comparisons reaching C | through the out-of-line comparator | decidable from the two words alone | atom-table lookups | of those, ordering |
|---|---:|---:|---:|---:|---:|
| `stdlib/unicode_util` | 61,694,914 | 71.1% | 88.2% | 1,352,867 (2.19%) | 1,063,657 |
| `stdlib/erl_parse` | 34,937,670 | 91.5% | 75.7% | 4,531,761 (12.97%) | 4,220,673 |
| `compiler/beam_ssa_opt` | 6,101,955 | 89.9% | 72.8% | 825,429 (13.53%) | 721,058 |
| `stdlib/lists` | 2,916,578 | 89.9% | 64.3% | 548,145 (18.79%) | 506,440 |

Emulated payload, what a JIT-less target runs:

| file | comparisons | out-of-line | two-word decidable | atom-table lookups | of those, ordering |
|---|---:|---:|---:|---:|---:|
| `stdlib/unicode_util` | 477,096,050 | 96.1% | 98.0% | 168,449,416 (35.31%) | 2,016,356 |
| `stdlib/erl_parse` | 300,835,706 | 98.8% | 96.3% | 105,883,916 (35.20%) | 6,259,208 |
| `compiler/beam_ssa_opt` | 57,823,656 | 100.0% | 96.1% | 21,785,067 (37.68%) | 1,148,615 |
| `stdlib/lists` | 37,059,276 | 100.0% | 96.1% | 15,086,783 (40.71%) | 839,575 |

"Decidable from the two words alone" means the answer follows from the two
term words without dereferencing either: identical encodings, two distinct
small integers, two distinct atoms asked only whether they are the same atom,
or two different type tags. Three quarters to seven eighths of everything the
compiler compares is in that class.

## The first finding: the emulator asks the atom table a question it answered already

Look at the two atom columns. Emulated, 35-41% of *all* comparisons are
atom-table round trips -- 168M of them on unicode_util -- and only 2.0M of
those were ordering questions. The other 166M are `=:=` on two atoms.

`term_compare0` resolves two atoms through `atom_table_cmp_using_atom_index`
before it looks at `TermCompareEqualOnly`. For `A =:= B` with distinct atoms
the answer is already in the two words -- distinct indices are distinct atoms
-- but the comparator takes two five-load dependency chains into the atom
table, and, when the names share a prefix, a `memcmp` as well, to compute an
ordering the caller then throws away.

The JIT hides this: it answers `=:=` on immediates in generated code, so the
AOT column shows only 289K such lookups on unicode_util instead of 166M. Every
target without a JIT backend pays the full price, and so does any
`AVM_DISABLE_JIT` build.

## Where the comparisons come from

AOT, by the frame that called the comparator (and by its caller where that
frame is shared):

| `stdlib/erl_parse` | share |
|---|---:|
| `termmap_champ_fill_array_sorted` | 22.4% |
| ... from `nif_maps_remove` | 14.6% |
| ... from `maps_project` (`maps:keys`/`values`) | 7.9% |
| `jit_term_compare_pin` (comparison from generated code) | 20.5% |
| `jit_put_map_assoc_one_pin` (flat-map insert) | 19.0% |
| `nif_lists_member` | 8.5% |
| `bif_erlang_greater_than_or_equal_2` | 5.3% |
| `term_find_map_pos` | 3.3% |

| `stdlib/unicode_util` | share |
|---|---:|
| `jit_term_compare_pin` | 21.5% |
| `termmap_champ_fill_array_sorted` | 17.6% |
| ... from `maps_project` | 11.5% |
| ... from `nif_maps_merge` | 4.3% |
| `nif_maps_from_list` | 11.0% |
| `bif_erlang_greater_than_or_equal_2` | 6.5% |
| `bif_erlang_map_get_2` | 4.9% |

| `compiler/beam_ssa_opt` | share |
|---|---:|
| `jit_put_map_assoc_one_pin` | 25.4% |
| `termmap_champ_fill_array_sorted` | 21.5% |
| ... from `nif_maps_remove` | 17.1% |
| `jit_term_compare_pin` | 20.6% |

Sorting a whole hash map into key order is 15-22% of every compile's
comparisons, on every file measured. It has two sources. `maps_project` sorts
because `maps:keys/1` and `maps:values/1` return a list and AtomVM returns it
in key order. `nif_maps_remove` sorts because a hash map that drops to
`TERM_MAP_HASH_THRESHOLD` (128) entries converts back to a flat map, and a
flat map is a sorted array -- so a workload that keeps a map hovering at the
boundary pays an O(n log n) materialization for every single removal that
crosses it. On `beam_ssa_opt` that one path is 17% of all comparisons.

## Atoms: what a different representation would be worth

Of the ordering comparisons that do reach the atom table, here is how much of
the name has to be read. The column is the share resolved if the term itself
carried the first N bits of the zero-padded name, which is the "atom index
carrying value bits" idea:

| file | ordering lookups | 8 bits | 16 | 24 | 32 | 38 | 48 | 64 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| `unicode_util` | 1,063,657 | 79.0% | 90.9% | 91.9% | 96.1% | 97.0% | 99.2% | 99.4% |
| `erl_parse` | 4,220,673 | 24.4% | 28.3% | 28.8% | 29.4% | 35.3% | 35.9% | 36.3% |
| `beam_ssa_opt` | 721,058 | 69.8% | 83.3% | 85.3% | 86.2% | 90.4% | 92.5% | 94.1% |
| `lists` | 506,440 | 71.6% | 83.3% | 85.1% | 87.3% | 90.5% | 91.7% | 93.8% |

38 bits is what an atom term has free on 64-bit (the index occupies bits 6..25
and is capped at 20 bits by `uint32_t index : 20`). It would resolve 90-97% of
the ordering on three of the four files -- and 35% on `erl_parse`, where the
distribution of common prefixes is:

| common prefix, bytes | 0 | 4 | 9 | 10 | 11 | 12 |
|---|---:|---:|---:|---:|---:|---:|
| share of orderings | 24.4% | 6.0% | 7.9% | 30.9% | 13.6% | 7.1% |

`erl_parse` is a generated parser and its atoms are `yeccpars2_123_`,
`yeccgoto_expr`, and so on: 63.7% of its atom orderings share more than eight
bytes of name, so they fall out of the existing `sort_key` cache and out of
any prefix a term could carry, and land in `memcmp`. This is the case the
8-byte `sort_key` was supposed to cover and does not.

So a name prefix -- in the term or in the node -- is the wrong thing to cache,
because it is only as good as the workload's naming. **The representation that
is not workload-dependent is a rank**: a dense order index per atom, so that
ordering two atoms is comparing two integers, always, with no name access and
no fallback.

A rank cannot live in the atom term. Interning a new atom shifts the ranks of
every atom above it, and terms already on heaps and in literals would carry
stale ones. It can live in a flat `uint32_t` array indexed by atom index,
rebuilt when the table grows (atoms arrive in module-load batches; a compile
of `erl_parse` interns 2,243 of them). Then an ordering is two independent
loads and a compare, against today's two five-load dependency chains plus a
`memcmp` 64% of the time on `erl_parse`. It is also *smaller* than what it
replaces -- 4 bytes per atom in one array instead of 8 bytes per `HNode` --
which matters because `ATOM_TABLE_SORT_KEY_CACHE` is compiled out on 32-bit
precisely because the `uint64_t` doubled the node and cost esp32c3 its
handshakes. A rank array would give 32-bit targets the fast path they do not
have today.

Relabelling is sound: rebuilding preserves the relative order of existing
atoms, so no sorted container is ever invalidated.

## Boxed terms: no, there is no room worth taking for a hash

The bits exist -- a boxed header is `(size << 6) | tag` and no tuple needs 58
bits of arity on 64-bit. The question is whether a cached hash in that field
would pay, by letting `=:=` answer "different" without walking.

Every compound `=:=` that did not resolve on identity, by how many element
pairs it examined before deciding:

| pairs examined | 1 | 2-3 | 4-7 | 8-15 | 16+ |
|---|---:|---:|---:|---:|---:|
| `erl_parse`, differ | 46.3% | 18.1% | 8.5% | 4.3% | 0.2% |
| `erl_parse`, equal | 5.7% | 13.3% | 2.8% | 0.8% | 0.1% |
| `unicode_util`, differ | 35.6% | 24.7% | 16.3% | 0.4% | 0.0% |
| `unicode_util`, equal | 10.9% | 11.2% | 0.8% | 0.1% | 0.0% |

73% of the differing comparisons decide within four element pairs, 46% on the
first. A stored hash would replace one or two register compares with a hash
compare, and would have to be computed and maintained at every site that
builds a tuple -- `put_tuple`, `setelement`, the record update, the external
term decoder, literal loading, both message copy paths and the collector. The
walks are already too short to amortize it. The answer to "how many bits do we
have, and would it make sense to cache a hash in tuples or maps" is: enough
bits, and no.

(This agrees with the separate finding that caching the hash *of the probe key*
in a thread-local memo is worth 1.03x on `unicode_util` while storing a hash
per map entry lost. The value is in not re-hashing the same object, not in
having the hash to hand.)

## Ranked, from the census

1. **Stop sorting a whole map to remove one key.** `nif_maps_remove` is 14.6%
   of `erl_parse`'s comparisons and 17.1% of `beam_ssa_opt`'s, all of it the
   hash-to-flat conversion at the 128-entry boundary. Hysteresis between the
   two thresholds removes the oscillation; it needs a check that `term_hash`
   of a map is representation-independent, since a map below the threshold
   could then be either shape.
2. **Answer `=:=` on two atoms before consulting the atom table.** Free, and
   worth 35-41% of all comparisons on every JIT-less target.
3. **Atom ordering by rank, not by name prefix**, sized above: 12-19% of the
   comparisons on three of the four files, and the only one of the three
   representation ideas that survives measurement.
4. **The sorted materialization behind `maps:keys`/`values`.** 4-12% of the
   comparisons. CHAMP's iteration order is canonical for a key set
   (same keys, same trie, same order, regardless of insertion order), so
   returning trie order stays deterministic; it is still an observable change
   and wants the byte-identical corpus as its test.
5. An inline two-word prefix at the heavy *ordering* call sites, the way
   `term_exact_equals_shallow` did it for `=:=`: half of the out-of-line calls
   on `erl_parse` are two distinct small integers.

Not worth doing, with the measurement that says so: a hash in the boxed
header (above), and a name prefix in the atom term (the `erl_parse` row).

## The rank, re-measured on the finished baseline — 2026-09-13

Idea 3 above was built on `w30/atom-rank` (the code lives in that branch's
`tools/dev/atom-rank-prototype.patch`, not as tracked source): every atom
carries a 32-bit order label, atom ordering is a label compare and nothing
else, `sort_key` and the name comparison are deleted, new atoms are labelled
in the gaps their merged batch lands in, and only gap exhaustion re-strides.
It applies to the finished baseline with a three-way merge, and passes the
Erlang and JIT suites on AArch64 **and on 32-bit arm32**, which it had never
been run on before.

### It does not move the compiler

Eleven alternating rounds, two warmups, identical AOT payloads; ratio above
1.0 means the rank is faster.

| file | baseline | with ranks | ratio | 95% CI |
|---|---:|---:|---:|---:|
| `stdlib/unicode_util` | 2122.6 ms | 2117.3 ms | 1.0020 | 0.9993-1.0045 |
| `stdlib/erl_parse` | 1616.9 ms | 1624.3 ms | 0.9981 | 0.9947-1.0017 |
| `stdlib/lists` | 204.5 ms | 207.4 ms | **0.9787** | **0.9657-0.9898** |
| `compiler/beam_ssa_opt` | 428.9 ms | 431.9 ms | 0.9953 | 0.9900-0.9998 |

A control run of the same harness against **two builds of identical source**
(different worktrees, so different paths and code layout) gives `erl_parse`
0.9943 with a CI of 0.9885-0.9996 and `lists` 0.9935 (0.9833-1.0030). That is
this measurement's noise floor: **an A/B of two separately built binaries
resolves nothing below about 0.6%, CI or no CI.** Only `lists` is outside it,
and it is against the rank -- that file is a 200 ms compile where interning
dominates and there are almost no atom orderings to win back.

On arm32 (Pi 2, nine rounds) the compiler is likewise a non-event, with much
wider intervals because each compile is 10-90 s on that box: `erl_parse`
88,360 ms to 88,386 ms, `lists` 10,323 ms to 10,259 ms, `beam_ssa_opt`
20,191 ms to 19,981 ms. ESTONE there is 46,125 to 45,655 (0.990x), inside the
run-to-run spread that host already shows. Peak RSS over an `erl_parse`
compile is 211,124 KiB against 211,052 KiB -- the 8 bytes per atom are
invisible at that scale.

### It moves atom ordering by up to five times

`tools/dev/atom_order_bench.erl` sorts 4000 atoms twenty times, in two name
shapes: one where the first eight bytes tie (the shape the census found in
`erl_parse`, 63.7% of its atom orderings) and one where they differ in byte
one.

| | AArch64 baseline | AArch64 rank | arm32 baseline | arm32 rank |
|---|---:|---:|---:|---:|
| common prefix | 11,279 us | 5,147 us (**2.19x**) | 473,824 us | 95,537 us (**4.96x**) |
| distinct prefix | 6,323 us | 4,990 us (**1.27x**) | 359,552 us | 93,782 us (**3.83x**) |

The rank's own time barely moves between the two shapes -- 5.0 against 5.1 ms,
94 against 96 ms -- which is the property the census asked for: a rank is the
only one of the three representation ideas whose cost does not depend on what
the atoms are called. arm32 gains most because it has no `sort_key` at all:
the cache is compiled out below 64 bits, since the `uint64_t`'s alignment
doubles `struct HNode` there, so every ordering comparison on a 32-bit target
walks the names today.

### Verdict

Not merged, and the reason is about workload, not about the rank. The
comparison it makes cheap has been deliberately removed from the paths we
benchmark: `maps:from_list` sorts by hash, `maps:keys`/`values` no longer
materialize a sorted array, the flat-boundary sort in `maps:remove` is gone,
and `=:=` on two atoms never reaches the comparator. What is left on those
workloads is its insertion cost, which is what `lists` measures.

It is still the right structure for code that orders atoms in bulk --
`lists:sort/1` over atoms, `ordsets`/`orddict`/`gb_trees` keyed by atoms,
`ets` ordered sets -- and on 32-bit targets that is 4-5x, not a percent. The
cost on those targets is 8 bytes per atom of new RAM (the labels plus the
by-name index, with nothing given back, since `sort_key` is already compiled
out) plus retired label arrays that only a reclamation scheme frees. If that
trade is ever wanted, the thing to fix first is the per-batch insertion cost,
which is the only measured regression.
