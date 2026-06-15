<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

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
