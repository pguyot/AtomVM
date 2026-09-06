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
