<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Benchmark matrix results — 2026-07-05 (second update): stdlib WON

With loop-header register residency (6a4b2af03), JITState cp_base
(c90d9d2e7) and — decisively — the select_val all-immediate word-compare
(92e3565c1, which removed ~115M per-compile PRIM_TERM_COMPARE calls from
atom-dispatch select_vals in the compiler's own code):

- **erlc stdlib (84 files): BEAM 290.9 ms/file vs JIT SMP 284.4 ms/file —
  1.02x, AtomVM BEATS BEAM.** unicode_util 5.40→4.68 s, erl_parse
  3.45→3.04 s; every other file lifted by the compiler's own speedup.
- **erlc kernel (101 files): 1.59x** (205.6 vs 129.5 ms/file).
- **erlc overall (185 files): 1.22x** (244.4 vs 199.8 ms/file).
- **benchmark app compute: 1.02x** (BEAM 81.1 ms vs JIT SMP 79.8 ms);
  wall including startup 108 vs 341 ms (3.1x).
- **estone: ~1.6 M vs 2.73 M (0.59x).** Remaining deficits: pattern
  −565k (per-iteration instruction count vs BeamAsm), int_arith −168k
  (non-tail call/return overhead), bif_dispatch −123k, links −54k.

With the x-register liveness analysis (jit_liveness pass A) and deferred
write-back elision (pass B) on top of the 2026-07-04 batch:

- **benchmark app compute: BEAM 83.1 ms vs JIT SMP 79.8 ms — 1.04x,
  AtomVM BEATS BEAM.** Wall including startup 111 vs 343 ms (3.1x).
- **erlc stdlib+kernel (185 files): overall 1.08x (kernel 1.41x, stdlib
  0.89x).** The stdlib deficit remains unicode_util + erl_parse
  (ordered-map term_compare volume).
- **estone: ~1.58 M vs ~2.63 M (0.60x).**

# Benchmark matrix results — 2026-07-04 update (BEAM vs JIT SMP only)

After the 2026-07-04 optimization batch (sort/delete/seq NIFs, aarch64
inline allocate/test_heap/deallocate/put_list, select_val binary tree +
dense jump tables, runtime bsl/bsr fast paths, literal adds/subs,
direct-dispatch call_fun/call_ext, LTO on build.release, and the `B + B`
aliasing miscompile fix), measured with the interleaved drivers:

- **benchmark app compute: BEAM 84.7 ms vs JIT SMP 85.9 ms (0.99x — parity;
  runs oscillate 0.97–1.01x).** AtomVM now beats BEAM on pingpong (23 vs
  37 ms), prng, sudoku_solution (0.55 vs 0.84 ms) and list_test (13.3 vs
  14.1 ms); sudoku_puzzle is the only remaining loss (31.3 vs 17.4 ms).
  Median wall including startup: 117 ms vs 347 ms (3.0x in AtomVM's favor).
- **erlc (OTP-29 stdlib+kernel, 185 common files): overall 1.03x — AtomVM
  beats BEAM erlc on the corpus.** kernel 1.36x, stdlib 0.86x; 139 of 185
  files individually faster than BEAM. The stdlib deficit is concentrated
  in unicode_util (0.37x) and erl_parse (0.53x) — the ordered-map
  term_compare wall.
- **estone (ported to `atomvm_benchmark/src/estone.erl`, port_io and ets
  excluded on both VMs): BEAM ≈ 2.63 M vs AtomVM ≈ 1.48 M ESTONES (0.56x).**
  The formula (w²·31e6/µs) is dominated by sub-millisecond micros: pattern
  644 k vs 1 252 k, int_arith 73 k vs 253 k, bif_dispatch 181 k vs 335 k.
  AtomVM beats BEAM on the generic-server micro.

Previous full-matrix run below for reference.

# Benchmark matrix results — 2026-06-15

Host: macOS aarch64 (Apple Silicon). All AtomVM builds `-O2` (RelWithDebInfo).
BEAM = OTP 29.0. JIT target = aarch64 (AOT-precompiled). Method: see
`BENCHMARK_MATRIX.md`.

**Interleaved measurement.** These numbers use the interleaved drivers
`tools/dev/bench_interleave_app.py` and `tools/dev/bench_interleave_erlc.py`,
which round-robin the configs *within* each run (rotating the order) so every
config sees nearly the same thermal/load state — the config-vs-config **ratio**
is then robust to thermal drift, which on this Apple-Silicon host is large under
sustained load (BEAM's heavier startup throttles more than AtomVM's). Absolute
times therefore run warmer than an idle-machine measurement; the **vs-BEAM
ratios are the comparable metric**, not the absolute milliseconds.

Includes the literal-pool + B-tree map commits (`module: cache deserialized
literals in a shared per-module pool`, `term: store maps over 32 entries as a
persistent B-tree`, `termmap_tree: inline small-integer key comparison`), which
roughly doubled the AtomVM-JIT erlc ratio versus the previous (pre-pool) run.

## Benchmark app (interleaved, median of 15 runs, 2 warmup; microseconds)

| test                         |   BEAM |  emu noSMP | emu SMP | JIT noSMP | JIT SMP |
|------------------------------|-------:|-----------:|--------:|----------:|--------:|
| pingpong_speed_test          | 43,759 |   75,141 | 63,722 |   63,162 | 47,758 |
| prime_speed_test             |  4,958 |   79,966 | 48,408 |   11,517 |  4,566 |
| prng_test                    |    760 |    1,002 |  2,061 |      903 |    880 |
| pi_test                      |  7,977 |   49,677 | 47,058 |    9,447 | 12,909 |
| bigint_test                  |  3,108 |    3,569 |  6,633 |    2,247 |  2,249 |
| crypto_test                  |  2,958 |    2,442 |  2,206 |    2,208 |  2,055 |
| sudoku_solution_test         |  1,762 |    2,666 |  2,679 |      950 |    962 |
| sudoku_puzzle_test           | 20,217 |  203,180 |202,825 |   60,996 | 60,874 |
| list_test                    | 16,804 |  119,755 |120,122 |   49,545 | 49,575 |
| map_test                     |  2,722 |    4,159 |  4,375 |    2,384 |  2,390 |
| binary_test                  |    961 |    5,327 |  5,238 |      701 |    711 |
| pingpong_speed_test [sched=1]|182,828 |        — | 40,284 |        — | 32,835 |
| prime_speed_test [sched=1]   | 14,325 |        — | 82,497 |        — |  9,311 |

Aggregate over the 11 base tests (sum of medians):

| config    | compute (ms) | vs BEAM | median wall incl. startup (ms) |
|-----------|-------------:|--------:|-------------------------------:|
| BEAM      |        106.0 |   1.00× |                          475.8 |
| emu noSMP |        546.9 |   0.19× |                          553.5 |
| emu SMP   |        505.3 |   0.21× |                          632.7 |
| JIT noSMP |        204.1 |   0.52× |                          209.9 |
| JIT SMP   |        184.9 |   0.57× |                          233.7 |

- JIT ≈ 2.9× faster than the interpreter; SMP ≈ 1.1× on this app.
- On compute BEAM is ~1.8× faster than the best AtomVM (JIT SMP); but AtomVM's
  light VM startup means JIT wall time (~234 ms) beats BEAM (~476 ms).
- The literal-pool/B-tree commits don't move this runtime benchmark (it is not
  a literal/map-heavy *compile* workload) — included for cross-check; numbers
  match the previous run within thermal noise.

## erlc — OTP-29 stdlib/kernel/sasl/crypto (interleaved, median of 3 runs)

Whole-process wall time. 203 files compiled by every compiler; files any
compiler fails on (bitstring/feature gaps) are excluded from all totals.

| compiler  | sum (ms) | mean/file (ms) | vs BEAM |
|-----------|---------:|---------------:|--------:|
| BEAM      |  91,160  |        449.1   |   1.00× |
| emu noSMP | 290,764  |       1432.3   |   0.31× |
| emu SMP   | 304,493  |       1500.0   |   0.30× |
| JIT noSMP | 123,014  |        606.0   |   0.74× |
| JIT SMP   | 124,421  |        612.9   |   0.73× |

Per app (sum ms / mean-per-file ms; vs-BEAM ratio in parentheses):

| app    | files |        BEAM |   emu noSMP |    emu SMP |        JIT noSMP |          JIT SMP |
|--------|------:|------------:|------------:|-----------:|-----------------:|-----------------:|
| stdlib |    84 | 46,160/549.5|211,744/2520.8|223,583/2661.7| 73,091/870.1 (0.63×)| 73,886/879.6 (0.62×)|
| kernel |   100 | 37,764/377.6| 68,264/682.6| 69,959/699.6| 42,364/423.6 (0.89×)| 42,855/428.5 (0.88×)|
| sasl   |    17 |  6,165/362.6|  8,500/500.0|  8,645/508.5|  6,284/369.6 (0.98×)|  6,400/376.4 (0.96×)|
| crypto |     2 |  1,070/534.9|  2,257/1128.3| 2,306/1152.9|  1,276/637.8 (0.84×)|  1,281/640.6 (0.84×)|

- **AtomVM JIT now reaches 0.73–0.74× of BEAM erlc overall (was 0.39× pre-pool),
  and 0.96× on sasl / 0.88× on kernel** — near parity. The literal pool
  eliminated a forced-GC storm from per-access literal re-deserialization (the
  compiler's literal-dense modules paid one forced collection per literal
  access; compiling `erl_parse.erl` alone dropped from 4.9M GCs to ~24k).
- SMP makes essentially no difference for erlc (single-process compile; SMP is a
  hair slower from scheduler-thread startup).
- JIT ≈ 2.4× faster than the interpreter on this corpus.
- Caveat: the `atomvm_erlc` front-end returns each compile result via a process
  `exit/1`, so AtomVM prints a (harmless) crash report per file — a fixed
  per-invocation overhead added to every AtomVM config (not BEAM). It does not
  affect the cross-config comparison.

### Excluded files

~18 fail to compile on AtomVM on all configs (bitstring / feature gaps):
`stdlib/{beam_lib,dets_v9,epp,erl_lint,erl_tar,io_lib_format,json,man_docs,peer,re,shell_docs,shell_docs_markdown,unicode,zip}`,
`kernel/{code,erl_erts_errors,file_io_server,inet_dns}`.

`stdlib/unicode_util.erl` is a ≈9M-word GC-heavy compile: it now completes on
every config within the 120 s per-file timeout (it previously timed out on the
interpreter), so it is included in this run — accounting for the 203rd common
file (vs 202 in the pre-pool run).
