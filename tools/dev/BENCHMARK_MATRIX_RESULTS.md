<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Benchmark matrix results — 2026-06-15

Host: macOS aarch64 (Apple Silicon). All AtomVM builds `-O2` (RelWithDebInfo).
BEAM = OTP 29.0. JIT target = aarch64 (AOT-precompiled). Method: see
`BENCHMARK_MATRIX.md`.

## Benchmark app (median of 15 runs, 2 warmup; microseconds)

| test                         |   BEAM |  emu noSMP | emu SMP | JIT noSMP | JIT SMP |
|------------------------------|-------:|-----------:|--------:|----------:|--------:|
| pingpong_speed_test          | 35,462 |   64,454 | 32,796 |   55,921 | 25,686 |
| prime_speed_test             |  3,112 |   67,010 | 18,210 |    8,239 |  3,616 |
| prng_test                    |    350 |      925 |    888 |      363 |    366 |
| pi_test                      |  6,095 |   86,842 | 89,223 |    8,476 |  8,376 |
| bigint_test                  |  1,629 |    3,341 |  3,270 |    2,037 |  2,020 |
| crypto_test                  |  1,651 |    2,402 |  2,154 |    2,121 |  1,996 |
| sudoku_solution_test         |    855 |    2,477 |  2,413 |      874 |    874 |
| sudoku_puzzle_test           | 16,926 |  172,465 |167,659 |   52,232 | 52,463 |
| list_test                    | 13,427 |  107,332 |107,586 |   46,569 | 46,606 |
| map_test                     |  1,290 |    3,482 |  3,406 |    2,082 |  2,055 |
| binary_test                  |    467 |    4,869 |  4,922 |      651 |    670 |
| pingpong_speed_test [sched=1]|161,611 |        — | 33,114 |        — | 25,846 |
| prime_speed_test [sched=1]   |  9,000 |        — | 64,554 |        — |  8,378 |

Aggregate over the 11 base tests (sum of medians):

| config    | compute (ms) | vs BEAM | median wall incl. startup (ms) |
|-----------|-------------:|--------:|-------------------------------:|
| BEAM      |         81.3 |   1.00× |                          340.8 |
| emu noSMP |        515.6 |   0.16× |                          521.5 |
| emu SMP   |        432.5 |   0.19× |                          534.6 |
| JIT noSMP |        179.6 |   0.45× |                          182.9 |
| JIT SMP   |        144.7 |   0.56× |                          181.9 |

- JIT ≈ 2.9× faster than the interpreter; SMP ≈ 1.2× on this app.
- On compute BEAM is ~1.8× faster than the best AtomVM (JIT SMP); but AtomVM's
  light VM startup means JIT wall time (~182 ms) beats BEAM (~341 ms).

## erlc — OTP-29 stdlib/kernel/sasl/crypto (median of 3 runs, whole-process wall)

202 files compiled by every compiler; 19 excluded (not comparable, see below).

| compiler  | sum (ms) | mean/file (ms) | vs BEAM |
|-----------|---------:|---------------:|--------:|
| BEAM      |  46,634  |        230.86  |   1.00× |
| emu noSMP | 215,027  |       1064.49  |   0.22× |
| emu SMP   | 222,348  |       1100.73  |   0.21× |
| JIT noSMP | 118,313  |        585.71  |   0.39× |
| JIT SMP   | 119,636  |        592.25  |   0.39× |

Per app (sum ms / mean-per-file ms over common files):

| app    | files |  BEAM sum/mean | emu noSMP | JIT noSMP |
|--------|------:|---------------:|----------:|----------:|
| stdlib |    83 |  22,385 / 269.7|134,169/1616|69,576/838 |
| kernel |   100 |  20,435 / 204.4| 70,794/708 |42,401/424 |
| sasl   |    17 |   3,240 / 190.6|  7,187/423 | 4,522/266 |
| crypto |     2 |     574 / 287.1|  2,877/1438| 1,813/906 |

- BEAM ≈ 2.6× faster than AtomVM JIT on this corpus; JIT ≈ 1.8× faster than emu.
- SMP makes essentially no difference for erlc (single-process compile; SMP is a
  hair slower from scheduler-thread startup).
- Caveat: the `atomvm_erlc` front-end returns each compile result via a process
  `exit/1`, so AtomVM prints a (harmless) crash report per file — a fixed
  per-invocation overhead added to every AtomVM config (not BEAM). It does not
  affect the cross-config comparison.

### Excluded files (19)

18 fail to compile on AtomVM on all configs (bitstring / feature gaps):
`stdlib/{beam_lib,dets_v9,epp,erl_lint,erl_tar,io_lib_format,json,man_docs,peer,re,shell_docs,shell_docs_markdown,unicode,zip}`,
`kernel/{code,erl_erts_errors,file_io_server,inet_dns}`.

1 is config-dependent: `stdlib/unicode_util.erl` compiles under JIT but exceeds
the 120 s per-file timeout on the interpreter (≈9M-word GC-heavy compile).
