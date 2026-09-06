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

1. ~~Cross-module call inline cache -- ~11% of the workload is dispatch.~~
   **WRONG, corrected below in Addendum 5: 11-13% is the INCLUSIVE cost of
   `jit_call_ext*` (the NIFs and BIFs reached through it doing real work).
   Dispatch self time is ~1.9%.**
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

## Addendum 3: BEAM at the function level, and why eprof gets it wrong

The compiler's Erlang functions average 1.6 s / 188.0M calls = **8.5 ns per
call**. Both per-call instrumenting profilers add roughly 50 ns of bookkeeping
on top of that, so their time column collapses to a call count:

| profiler | wall on unicode_util | distortion |
|---|---:|---|
| untraced | 1.60 s | -- |
| `eprof` | 10.95 s | 6.8x |
| `call_time` tracing (all modules) | 10.85 s | 6.8x |
| **statistical sampler** | **1.69 s** | **1.05x** |

`+JPperf` (which would let a native sampler symbolicate BeamAsm frames) is
not supported on macOS. The alternative that works anywhere is
`tools/dev/beam_erlang_sampler.erl`: a high-priority process that polls
`process_info(P, [status, current_stacktrace])`, keeps the innermost frame of
whichever process is `running`, and never touches the target's code.

Ranked by sampled time, with eprof's own ranking beside it:

| function | true % | eprof % | eprof rank | calls |
|---|---:|---:|---:|---:|
| `beam_ssa_dead:maps_is_subset_kv/2` | **5.16** | 1.64 | 8 | 1,874,375 |
| `maps:next/1` | **3.62** | 1.74 | 5 | 4,326,794 |
| `beam_ssa_dead:eval_is/4` | 2.68 | 3.51 | 1 | 3,873,490 |
| `beam_ssa:successors/1` | 1.93 | 0.52 | **50** | 1,363,863 |
| `beam_ssa_ss:meet_in_args_elems1/3` | 1.93 | 2.11 | 4 | 2,675,145 |
| `beam_ssa_dead:'-sub/2-lc$^0/1-0-'/2` | 1.93 | 2.50 | 2 | 4,224,050 |
| `beam_ssa:normalize/1` | 1.89 | 0.65 | **42** | 2,142,755 |
| `beam_ssa_dead:get_value/2` | 1.85 | 1.72 | 7 | 4,565,760 |
| `beam_ssa_dead:sub/2` | 1.69 | 1.07 | 17 | 1,895,436 |
| `beam_ssa_dead:map_intersect_kv_2/3` | 1.65 | 0.73 | **35** | 783,255 |
| `beam_ssa:rpo_1/4` | 1.46 | 1.17 | 13 | 1,397,094 |
| `beam_ssa_dead:will_succeed/2` | 1.42 | 0.62 | **43** | 1,895,719 |
| `beam_ssa:linearize_1/4` | 1.22 | 0.57 | **46** | 662,257 |
| `beam_ssa_dead:shortcut_3/5` | 1.02 | 2.44 | **3** | 1,970,257 |

eprof puts `shortcut_3/5` third and `successors/1` fiftieth; sampling
reverses them. Anything ranked by eprof on this codebase is ranked by call
count.

The real head of the profile is map *bulk* work, not map lookup:
`maps_is_subset_kv/2` + `maps:next/1` + `map_intersect_kv_2/3` + `maps:iterator/2`
is about 11% of BEAM's Erlang-level time, and all of it is iteration and
whole-map set algebra rather than single-key probes.

That is a different target from everything above. Our `nif_maps_next` walks a
tree map with a lazy in-order cursor and allocates on *every* step -- cursor
frames, a 1-tuple wrapper, a cons and a 3-tuple -- against BEAM's HAMT array
walk. 4.3M `maps:next/1` calls per file is a lot of allocation to hand the
collector, and it is a plausible part of why our GC costs 6x BEAM's here.
Worth measuring on our side before anything else on this cluster.

## Addendum 4: the hot loop in BEAM assembly, and where the curves cross

`beam_ssa_dead:maps_is_subset_kv/2` is 5.16% of BEAM's whole compile of
unicode_util, and it is nine lines:

```erlang
maps_is_subset_kv({K, V, Iterator}, BigMap) ->
    Next = maps:next(Iterator),
    case BigMap of
        #{K := V} -> maps_is_subset_kv(Next, BigMap);
        #{} -> false
    end;
maps_is_subset_kv(none, _BigMap) -> true.
```

`erlc -S` gives the whole loop body:

```
{test,is_tuple,{f,445},[{x,0}]}.
{test,test_arity,{f,442},[{x,0},3]}.
{allocate,2,2}.
{move,{x,1},{y,0}}.
{move,{x,0},{y,1}}.
{get_tuple_element,{x,0},2,{x,0}}.
{call_ext,1,{extfunc,maps,next,1}}.            % <- one iterator step
{test,is_map,{f,446},[{y,0}]}.
{get_tuple_element,{y,1},0,{x,1}}.
{get_map_elements,{f,444},{tr,{y,0},{t_map,any,any}},{list,[{x,1},{x,1}]}}.
{get_tuple_element,{y,1},1,{x,2}}.
{test,is_eq_exact,{f,444},[{x,1},{x,2}]}.
{move,{y,0},{x,1}}.
{call_last,2,{f,443},2}.
```

Per iteration: **one `maps:next/1` and one single-key `get_map_elements`**,
around ten cheap register ops. Everything else is noise. So the whole 5.16% is
two map primitives, and the same is true of `map_intersect_kv_2/3` and of
`maps:next/1` in its own right -- together about 11% of BEAM's Erlang time.

`tools/dev/bench_map_subset_kv.erl` runs exactly that loop, plus its two
components separately, on both VMs (AOT-precompiled for AtomVM):

| n | subset_kv BEAM | subset_kv AtomVM | `maps:next` BEAM | `maps:next` AtomVM | lookup BEAM | lookup AtomVM |
|---:|---:|---:|---:|---:|---:|---:|
| 16 | 47.4 | **26.6** | 3.8 | 11.5 | 43.4 | **15.2** |
| 64 | 29.7 | **24.8** | 7.4 | 9.8 | 24.6 | **14.4** |
| 256 | 33.6 | 33.6 | 7.0 | 13.1 | 26.9 | **20.8** |
| 1024 | **29.7** | 45.5 | 7.6 | 13.1 | 29.3 | **26.0** |
| 4096 | **32.7** | 60.0 | 6.0 | 13.0 | 31.2 | 52.9 |

(ns per entry / per key.)

Two clean facts:

1. **BEAM's lookup is flat and ours is not.** 24.6 -> 31.2 ns from n=64 to
   n=4096 for BEAM; 14.4 -> 52.9 ns for us. We are 1.7x *faster* at n=64,
   still faster at n=1024, and 1.7x slower at n=4096. The curves cross around
   n = 1500-2000. Hashing has a high fixed cost (it walks the whole key) and
   no size term; ordered comparison has a low fixed cost and a log(n) term.
   For the small maps that dominate ordinary Erlang -- and every MCU workload
   -- the ordered map is the better structure, and the measurement says so.

2. **`maps:next/1` is flat on both and we are 2x slower on it** (13.0 vs
   6.0 ns), independent of map size. That is not the data structure; it is
   `nif_maps_next` allocating on every step -- cursor frames, a 1-tuple
   wrapper, a cons and a 3-tuple -- where BEAM walks a HAMT array. 4.3M
   `maps:next/1` calls per compile is a lot of garbage, and it is consistent
   with our GC costing 6x BEAM's on this file.

The second one is a bug-shaped cost with no structural excuse, and it is the
next thing to fix on this cluster. The first is a genuine design trade with a
measurable crossover, which argues for a size-triggered hash index over the
existing ordered tree rather than replacing it: keep the structure that wins
below ~1500 entries, and stop paying log(n) above it. Note also that the
per-probe cost at n=4096 (4.4 ns over ~12 probes) is memory latency, not
comparison logic -- 89.4% of probes already resolve in two or three inline
instructions -- so the lever there is locality (a dense array of order
surrogates) rather than a cheaper comparator.

## Addendum 5: correcting the call_ext number, and recognizing system NIFs

The "~11% of the workload is cross-module dispatch" claim above was wrong: it
came from a call-graph parse that failed to subtract children, so parents kept
their callees' samples. Recomputed with self and inclusive time separated:

| | unicode_util self | incl | erl_parse self | incl |
|---|---:|---:|---:|---:|
| `jit_call_ext0` | 1.48% | 11.90% | 1.21% | 13.29% |
| `jit_call_ext_direct_pin` | 0.45% | 12.68% | 0.54% | 13.56% |

**Dispatch overhead is ~1.9%**, not 11%. The other ~11% is the NIFs and BIFs
called through it doing actual work. Removing the C round trip entirely caps
at about 2%.

### The mechanism is already in the tree

`jit.erl` carries an `import_resolver` (`#state.import_resolver`, built in
`jit_precompile:import_resolver/2` from the `ImpT` and `AtU8` chunks) that
maps an import index to `{Module, Function, Arity}` at compile time. It is
already used to specialize `OP_BIF2`, `OP_GC_BIF1` and `OP_GC_BIF2` on the
actual BIF.

`OP_BIF0/1/2` also already skip the generic path entirely:
`resolve_bif_func_ptr/3` asks the backend for
`move_imported_bif_to_native_register/2`, and x86_64 implements it as four
inline loads (`jit_state->module -> imported_funcs -> [bif] -> bif0_ptr`),
then calls the pointer directly. Other backends fall back to
`?PRIM_GET_IMPORTED_BIF`, one cheap C call.

`OP_CALL_EXT{,_LAST,_ONLY}` does none of this: it always goes through
`jit_call_ext0`, which re-resolves, does an acquire load of `func->type`,
switches on it, and returns a continuation address.

### What "recognize every system NIF" needs

`src/libAtomVM/nifs.gperf` is 282 entries of exactly the right shape, one per
line after the `%%`:

```
maps:next/1, &maps_next_nif
binary:at/2, &binary_at_nif
```

Nothing on the Erlang side reads it today. Generating a `jit_nifs` module
from it at build time keeps one source of truth and gives
`jit.erl` an `is_system_nif({M, F, A})` test to use with the resolver it
already has.

The call site then emits an inline guard rather than a cache: load
`imported_funcs[Index]`, load `type`, compare against `NIFFunctionType`,
and fall back to `?PRIM_CALL_EXT` if it does not match. That is strictly
better than the inline cache proposed earlier in this document, because it
keys on a field the VM already maintains -- so module reload and code purge
need no invalidation at all, they just change the type and the guard takes
the fallback.

Worth ~2% by itself. The larger prize is that knowing the callee statically
also allows dropping per-call work the generic path must do
conservatively (`ctx->nif_call_arity`, the `heap.root->next` fragment check,
the trap/error preamble) for NIFs known not to need it, and -- for
`maps:next/1` specifically, combined with the integer-path iterator below --
opens the door to emitting the common iterator step as native code with no C
call at all.
