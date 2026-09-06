# Where erlc still loses, and what the term-representation ideas are worth

Measured 2026-09-06 on macOS aarch64, `build.jit.rebase` (Release, JIT on),
OTP 29 corpus. The workload is one `erlc` compile of
`~/otp/lib/stdlib/src/erl_parse.erl` (951 KB, the largest stdlib source and
the one where AtomVM loses to BEAM on CPU: 1.81 s user vs BEAM's 1.62 s).

## Method

Two instrumented builds, both `-DAVM_COUNT_COMPARE` over
`build.census` (the patch is kept at
`tools/dev/census-term-compare-gcscan.patch`):

* counters in `term_compare0` for every fast path it can resolve on, plus a
  return-address histogram at the `term_exact_equals` call so each caller can
  be named;
* counters in `memory_scan_and_copy_impl` classifying every word the
  collector inspects.

`sample(1)` on the same compile gives the time distribution; self time is
computed by subtracting each frame's children from its call-graph count.

## The profile

Roughly, of one `erl_parse.erl` compile:

| cluster | share |
|---|---|
| map operations and term comparison | ~41% |
| garbage collection | ~17% |
| cross-module call dispatch (`jit_call_ext*`) | ~11% |

## The comparison census

48,538,376 calls to `term_compare0` for a single file. Where they resolve:

| resolved at | calls | share |
|---|---:|---:|
| tuple element (inline tuple path) | 24,314,200 | 50.1% |
| `term_exact_equals` (`Exact\|EqualOnly`) | 15,635,494 | 32.2% |
| two small integers | 2,961,212 | 6.1% |
| deep walk (temp stack) | 2,161,734 | 4.5% |
| identical terms (`t == other`) | 1,914,531 | 3.9% |
| two atoms, top level | 1,045,216 | 2.2% |
| two atoms, list element | 19,624 | 0.04% |

Of those, 7,663,279 (15.8%) end at an atom-vs-atom ordering decision.

The exact-equality half, by caller (return-address histogram):

| caller | calls |
|---|---:|
| `jit_term_compare_pin` (`=:=` from generated code) | 5,560,227 |
| **`nif_maps_remove`** | **5,387,676** |
| `nif_lists_member` | 2,980,531 |
| `term_find_map_pos` | 970,666 |
| `nif_maps_from_keys` | 302,448 |
| `bif_erlang_exactly_(not_)equal_to_2` | 316,346 |

`nif_maps_remove` was 11% of *all* comparisons in the compile. It compared
the key against every entry of a tree-backed map -- at least
`TERM_MAP_TREE_THRESHOLD` (128) of them -- although the entries it had just
materialized are in key order and the flat-map path immediately above already
binary-searches. Fixed (see the commit "Find the key to remove by binary
search, not by scanning the map"): erl_parse.erl 3.5% faster, the 280-file
corpus 1%, every .beam byte-identical.

## The GC census

One `erl_parse.erl` compile, `memory_scan_and_copy_impl`:

| | count |
|---|---:|
| scans | 2,397,354 |
| top-level words inspected | 22,780,686 |
| ... immediates | 3,650,432 |
| ... list pointers | 5,709,476 |
| ... boxed pointers | 5,391,534 |
| ... boxed headers | 8,029,244 |
| tuples scanned | 7,259,857 |
| tuple elements visited | 22,433,827 |
| ... that are immediates | 12,213,999 (54.4%) |
| tuples with *every* element immediate | 2,840,163 (39.1%) |

So the collector inspects ~45.2M terms per compile, and 12.2M of those
inspections are of tuple elements that turn out to be immediates.

## The three ideas, sized

### (a) Relocatable tuple: a bit in the arity word

The bit is available. A boxed header is `(size << 6) | tag`, and no real
tuple needs 58 bits of arity on 64-bit (or 26 on 32-bit).

The cheap subset -- one "no element of this tuple is a pointer" bit, letting
the scan do `ptr += arity + 1` -- is sized directly by the census above:
39.1% of scanned tuples qualify, average arity 3.09, so it removes about 8.8M
of the 45.2M inspections, ~19% of the scan work. Scan and shallow copy are
~8.7% of runtime, so **the ceiling is ~1.4%**.

The full version (whole subtree contiguous, memcpy'd as a unit) bounds at the
whole GC scan-and-copy cost, so **~2-3%** even if every collection became a
memcpy.

Against that: the invariant has to be maintained by every producer of a
tuple -- `put_tuple`, `setelement`, the destructive record update, the
external-term decoder, literal loading, both message copy paths and the
collector itself -- and any one site that forgets to clear the bit is a
silent heap corruption rather than a test failure. The arity bit is free; the
invariant is not. Worth doing only after the ~41% comparison cluster and the
~11% dispatch cluster, not before.

### (b) Atom index carrying value bits

This is already half-shipped and it was the right call. `struct HNode` caches
an 8-byte big-endian, zero-padded prefix of the atom name (`sort_key`), so
atom ordering is one integer compare with a full-name comparison only on a
tie; on 32-bit it is compiled out, because the `uint64_t`'s alignment doubles
`sizeof(struct HNode)` and that cost esp32c3 its TLS handshakes. The aarch64
JIT compare stub reads the field directly in generated code.

What is left is the addressing, not the compare. Both the C path and the JIT
stub reach `sort_key` through a five-load dependency chain: `ctx -> global ->
atom_table -> index_to_node -> node[idx] -> sort_key`. Putting the prefix in
the atom term itself (bits 26..63 are unused on 64-bit; the index occupies
bits 6..25 and is capped at 20 bits by `uint32_t index : 20`) would make
atom ordering a register compare with no memory access at all, at the cost of
one mask in `term_to_atom_index`.

Sizing: 7.66M atom-resolved comparisons per compile, at roughly 5 ns of
pointer chasing each, is ~38 ms of 1810 ms, so **~2%**, plus whatever the
JIT stub's inline copies of the same chain add. The intermediate -- a flat
`uint64_t *` array of sort keys indexed by atom index, cached in the Context
-- gets most of it (two independent loads instead of five dependent ones) for
a fraction of the blast radius, and keeps atom terms identical across builds.
Recommended shape if this is picked up.

### (c) Terms being pointer-sized

For `cp` specifically this is mostly already banked: `cp` is
`(module_index << 24) | offset` tagged `TERM_PRIMARY_CP` (0x0), and since
native code addresses are 4-byte aligned their low bits are already 0x0, so a
`cp` *could* hold a native return address directly. But the w27 inline
cross-module return already removed the jump-table hop from the hot return
path, and `jit_return` does not appear in this profile at all.

The version of (c) that is still worth something is the *call* side, not the
return side. `jit_call_ext_direct_pin` + `jit_call_ext0` are ~11% of the
compile, and that is pure dispatch: `module_resolve_function` is already down
to a single acquire load of `mod->imported_funcs[index]`, and the
`ModuleFunction -> ModuleNativeFunction` upgrade already caches the resolved
entry point. What remains is the C round trip itself -- through the primitive
table, spilling `ctx`/`jit_state`, switching on the function type, returning
an address for the generated code to branch to -- on every single
cross-module call.

A per-call-site inline cache (a RAM word holding the resolved native entry;
generated code loads, tests, and branches; a null goes out to C to fill it)
turns that into one load, one test and an indirect branch. It is the largest
single lever left in this workload, plausibly half of the 11%. It is also a
real project: eight backends, and cache invalidation on module reload and
purge.

## Ranking of what is left

1. Cross-module call inline cache -- ~11% of the workload is dispatch, and
   most of it is the C round trip, not the resolution. (c), call side.
2. The comparison cluster at ~41%. The HAMT experiment and the inline
   map-lookup experiment both refuted "the data structure" and "call
   overhead" as the cause; the census above says the remaining volume is
   genuine tuple-vs-tuple structural comparison, so the lever is comparing
   less, not comparing faster.
3. Atom sort keys via a flat array, ~2%. (b), addressing half.
4. All-immediate-tuple bit, ~1.4% ceiling, high invariant cost. (a).

## Addendum: GC time on unicode_util vs erl_parse

`unicode_util.erl` is the one file in the 280-file corpus where AtomVM is
slower than BEAM wall-clock (0.83x; 0.64x on CPU). The obvious suspect is the
collector, and it is wrong.

Measured directly with `-DAVM_TIME_GC` (a `CLOCK_MONOTONIC_RAW` pair around
the collection itself, not around the `memory_ensure_free_with_roots` check),
denominators taken from the *uninstrumented* build:

| | unicode_util | erl_parse |
|---|---:|---:|
| collections | 616 | 22,677 |
| all minor / major | 616 / 0 | 22,677 / 0 |
| forced by a chained fragment | 446 (72%) | 22,292 (98%) |
| total GC time | 95.7 ms | 196.0 ms |
| **share of CPU** | **3.9%** | **11.3%** |
| per collection | 155 us | 8.6 us |
| `memory_ensure_free_with_roots` calls | 16,580,171 | 11,204,751 |

The two files have opposite collection profiles: unicode_util runs 37x fewer
collections, each 18x more expensive, because the compiler state for a
table-driven module is one big long-lived heap that rarely fills. erl_parse
runs many small collections, 98% of them forced by the fragment rule rather
than by memory pressure (consistent with the earlier finding that 95% of
collections are fragment-forced).

Sampled cluster shares agree with the direct measurement (main-thread self
time only; `???` is JIT-generated code, which carries no symbols):

| cluster | unicode_util | erl_parse |
|---|---:|---:|
| JIT native code | 29.5% | 31.9% |
| map + comparison | **51.9%** | 32.5% |
| gc / memory | 6.2% | 20.0% |
| `call_ext` dispatch | 1.9% | 1.7% |

So the file where AtomVM actually loses to BEAM is the one where GC matters
*least* and the map cluster matters most: `node_find` alone is 24.7% of it,
`term_compare0` 15.9%, `bt_insert` 7.4%. unicode_util is the cleanest test
case available for the comparison-volume wall, and the right benchmark for
any work on ideas (a) or (b) -- the corpus average dilutes it to nothing.

## Addendum 2: what BEAM does with the same file

Same method applied to `/opt/local/bin/erlc` (OTP 29, BeamAsm) compiling
`unicode_util.erl`, sampling the `erts_sched_1` thread; plus `eprof` at the
Erlang level. Both VMs run the *same* compiler source, so the Erlang-level
call counts are identical on both and any time difference is the VM.

### BEAM's C-level profile (scheduler thread, self time)

| symbol | share |
|---|---:|
| `???` (BeamAsm generated code) | 44.9% |
| `erts_hashmap_get` | 9.7% |
| `kevent` (idle) | 7.7% |
| `erts_hashmap_insert_up` | 7.7% |
| `eq` | 7.0% |
| `make_internal_hash` | 6.1% |
| `erts_hashmap_insert_down` | 3.3% |
| `erts_internal_map_next_3` | 2.2% |
| `get_map_element` / `erts_maps_put` | 1.7% each |
| `erts_cmp_compound` | 1.7% |

Normalizing each VM to its own non-idle samples and multiplying by its CPU
time (BEAM 1.58 s, AtomVM 2.46 s):

| | BEAM | AtomVM | |
|---|---:|---:|---|
| generated native code | ~0.77 s | ~0.73 s | **parity** |
| map primitives | ~0.62 s | ~1.23 s | **2x** |
| garbage collection | ~0.02 s | 0.10 s | 6x |
| total CPU | 1.58 s | 2.46 s | |

The gap is not in the code the JIT emits. Our generated code is level with
BeamAsm on this workload; essentially all of the 0.88 s is the C map
primitives, and `erts_cmp_compound` -- BEAM's ordering comparison -- is 1.7%,
because a hash map almost never needs one.

The mechanism, measured on our side by instrumenting `node_find`:

* 51,997,888 `node_find` calls, **207,680,103 key probes** (~4 per lookup)
* resolved inline: 62.5% small-int, 20.0% 2-tuple probe, 6.9% identity
* fell back to `term_compare`: 22,033,780 (10.6%)

BEAM pays, per lookup, one `make_internal_hash` plus about one `eq`. We pay
about four *ordering* comparisons. An equality test may early-exit on any
differing word; an ordering test must find the *leftmost* difference. That is
the structural difference, and it is not a statement about which data
structure is faster in the abstract -- our per-probe code is already good
(~2.9 ns, 89.4% resolved by two or three inline instructions).

### Erlang level (eprof)

187,987,960 Erlang calls for the one file. eprof inflates the run 1.6 s ->
11.0 s, so only call counts and relative shares mean anything, and BEAM's
`map_get`/`is_map_key` are instructions rather than calls so they are
invisible here (they show up only in the C profile above).

| module | share | calls |
|---|---:|---:|
| `beam_ssa_dead` | 30.0% | 48,584,259 |
| `beam_types` | 13.7% | 27,613,069 |
| `maps` | 6.4% | 14,568,348 |
| `beam_ssa` | 5.6% | 10,886,758 |
| `lists` | 5.4% | 14,499,886 |
| `beam_ssa_ss` | 4.3% | 8,064,409 |
| `sets` | 4.3% | 8,794,121 |

`maps:remove/2` is called 2,111,312 times, which is why the binary-search fix
paid on erl_parse; on unicode_util most of those maps are flat (under
TERM_MAP_TREE_THRESHOLD) and already binary-searched, which is why it did not
pay here. `erlang:garbage_collect/0` is called 58 times explicitly by the
compiler -- against AtomVM's 616 collections for the same work.

### A negative result: extending the inline probe does not help

`struct TermMapProbe` only classifies a key when it is a 2-tuple whose
elements are *all* immediates. The census said that looked badly wrong: of
the 22.0M fallbacks, 18.6M are keys shaped `{Atom, Tuple, X}`, rejected on
the middle element, and in 90.6% of those the first two elements are
pointer-identical and element 2 decides.

Two versions were built and measured on unicode_util:

1. widen the arity to 2..4, still requiring every element immediate --
   covered 728 additional descents (element 1 is a tuple in 18,651,984 of
   18,653,652 cases), no change;
2. drop the element-type requirement entirely and classify on arity alone,
   skipping identical element pairs by identity and only requiring the
   leftmost *differing* pair to be covered -- **still no gain, ~2% slower.**

The reason is that `term_compare0`'s inline tuple path already does exactly
this work: skip identical elements, small-integer fast path, atom fast path,
recurse on a compound differing pair. Routing more keys through the probe
saves a call, not the comparison, and the wider probe costs more to
initialize on every map operation. Both versions were reverted.

This is the third experiment (after the full HAMT and the inlined map-lookup
descent) to confirm the same thing from a different angle: the cost is
comparison **volume**, and nothing that makes an individual comparison
cheaper moves it. The only lever left on this cluster is doing fewer
comparisons per lookup, which is a statement about the map's shape, not about
its comparator.
