<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Hash-ordered maps:from_list — 2026-09-12

Continuation of [comparison frame splitting](COMPARE_SPLIT_2026-09-12.md).
Artifacts: `build.ab/from-list-hash-2026-09-12/`.

Large maps are built as CHAMP tries, but maps:from_list still sorted every
input key by full term order before construction. Trie construction does not
require that ordering. The implementation caches each input key's 32-bit hash and
sorts by hash, using exact term order only for equal hashes. Stable merge sort
keeps duplicate runs in original input order, so the last occurrence wins.
Adjacent different hashes also avoid term comparison during deduplication.

Surviving indices are compacted first. If a large input collapses to at most
128 unique entries, those surviving keys are sorted in term order for the
flat-map representation. Only selected pairs become GC roots. Allocation and
invalid-input failures free the scratch arrays. The final variant also reuses
these hashes in the CHAMP builder, as described below.

Nine alternating compiler rounds against the retained `shallow/` comparator
variant give unicode_util **1.0195**, paired bootstrap 95% CI **1.0169–1.0226**,
and erl_parse **1.0010**, CI **0.9977–1.0045**. All **280 compiler sources**
produce byte-identical output (`correctness.json`).

Expanded maps tests cover 127/128/129/512/2048 unique compound keys, duplicate
inputs collapsing to flat and hash maps, exact numeric keys, malformed long
inputs, and integer keys 46834102 and 7327637 which share internal hash
0x5c37eb9e. Both a large collision-bearing map and a long input collapsing to
just those two keys are checked against independent left-fold insertion.
The projection helper previously used lists:usort, which merges distinct
exact numeric keys; this failed on BEAM too and now uses exact map-key
uniqueness. The expanded tests pass on BEAM, Debug emulator, and AArch64 AOT.

The user suggested `w30/atom-rank` at de513305e. Its patch and measurements were
inspected. That work reports erl_parse 1.0051, unicode_util 0.9985, lists 0.9886,
and beam_ssa_opt 0.9882 against its original sort-key baseline, with interning
upkeep accounting for the short-file regressions. It has not been applied here;
the measured maps:from_list gain takes priority. The patch remains available
at that commit for a later combined trial.


## Reusing hashes in the measured builder

`reuse/` copies the surviving hashes beside the compact GC-root buffer and
passes them to a new `termmap_champ_measure_hashed` entry point. Only the term
portion is registered as roots; cached hashes remain valid after collection.
The existing entry point and callers retain their original behavior. The
hashes are copied into builder-owned scratch, so ownership and cleanup stay
explicit.

A nine-round A/B against `candidate/` gives another unicode_util **1.0074**
(CI **1.0048–1.0099**); erl_parse **0.9979** (CI **0.9943–1.0020**) is neutral.
All 280 compiler outputs match the baseline again. Release Erlang, Debug term,
Debug Erlang, and the expanded Debug/AOT maps suites pass; return statuses
are recorded in `test-results-reuse.json`.

The first candidate's BEAM comparison was noisy: unicode_util median BEAM
2179.1 ms, AtomVM 2197.4 ms, paired ratio 0.9875 (CI 0.9647–1.0044). erl_parse
ratio 1.2241 (CI 1.1508–1.3071). That initial run did not establish a unicode_util win. The final quiet
measurements below use the reuse variant.


## Final performance measurements

Host: macOS AArch64, Apple M4. Reference: OTP 29.0.3 from `/opt/local/bin`.
AtomVM: Release, JIT/AOT, SMP enabled, AArch64 images; no instrumentation or
benchmark-specific code paths. Native executable and AOT payload hashes are
in `final-artifacts.json`. The rebuilt production VM has the same native
`__text` as the measured `reuse/AtomVM` (453,676 bytes).

The full established corpus is **280 files** across compiler (59), stdlib
(98), kernel (104), sasl (17), and crypto (2). Three paired rounds per file
show **280/280 faster than BEAM** (`per-file.json`). The sum of per-file
medians is **80.345 s BEAM versus 32.113 s AtomVM**, ratio **2.502**. The
closest files are unicode_util 1.0266, beam_core_to_ssa 1.1903, erl_parse
1.2376, erl_lint 1.2469, and v3_core 1.3085. No files were removed.

Independent focused comparison, eleven rounds and two warmups
(`beam-files-reuse.json`):

| file | BEAM median ms | AtomVM median ms | paired speed ratio | 95% CI |
|---|---:|---:|---:|---:|
| unicode_util | 2175.3 | 2153.2 | **1.0078** | **1.0041–1.0115** |
| erl_parse | 1992.3 | 1619.9 | **1.2293** | **1.2256–1.2324** |

ESTONE, each run 31 paired rounds and three warmups, using the same raw OTP 29
workload and the validated bounded-range AOT images:

| run | BEAM median score | AtomVM median score | score ratio | 95% CI |
|---|---:|---:|---:|---:|
| first | 2,473,888 | 2,486,397 | 1.0051 | 0.99996–1.01024 |
| repeat | 2,465,754 | 2,487,881 | **1.0090** | **1.00245–1.01076** |
| pooled 62 rounds | 2,469,404.5 | 2,487,742.5 | **1.00743** | **1.00261–1.00993** |

The first run alone includes parity; the next repeat and pooled data favor
AtomVM within those runs. However, the final check below reverses that small
margin, so a reliable ESTONE win is not yet established. AtomVM also still
takes longer in total wall time. Earlier builds and noisy runs are retained
in the preceding reports rather than treated as current wins.
Raw samples: `beam-estone.json`, `beam-estone-repeat.json`; pooled summary:
`beam-estone-pooled.json`.

## Reproduction

The ready-to-run AOT compiler is
`build.ab/from-list-hash-2026-09-12/reuse/erlc`. Its payload includes the same
169 raw library and 100 compiler beam inputs previously validated, with only
the native VM replaced for the C changes here. No AOT ABI changed.

```sh
python3 tools/dev/bench_publication.py erlc \
  --atomvm-erlc build.ab/from-list-hash-2026-09-12/reuse/erlc \
  --beam-erl /opt/local/bin/erl --beam-erlc /opt/local/bin/erlc \
  --runs-per-file 3 --runs-batch 0 --output /tmp/erlc-final.json
python3 tools/dev/bench_publication.py estone \
  --build build.ab/select-ranges-2026-09-12/publication \
  --atomvm build.ab/from-list-hash-2026-09-12/reuse/AtomVM \
  --beam-erl /opt/local/bin/erl --runs 31 --warmup 3 \
  --output /tmp/estone-final.json
```

Redirect timing output and pause builds/tests while measuring. Each JSON records
its commands and environment. Margins on unicode_util and ESTONE are small,
so fresh paired measurements matter on another host or build configuration.


## Regression checks and final ESTONE audit

The final source and test changes pass Release Erlang, all six Debug C suites
(term, heap, structs, bitstring, enif, mailbox), Debug Erlang, all 3,170 JIT
tests, etest, alisp, eavmlib, and Elixir. Expanded maps tests also pass on BEAM
and in both AtomVM execution modes. Forty estdlib modules pass; the only
remaining failure is the previously established test_net_kernel hostname
resolution failure at test_autoconnect_to_beam/1:226 (`nxdomain`). Two initial
stdlib test launches had harness path errors: the serial peer requires both
the built VM on PATH and the build root as its working directory. The final
correctly launched run passes the serial test, including both BEAM peers.
See `release-lib-tests.json`, `debug-core-tests.json`, `estdlib-status.json`,
and `release-estdlib-final.log`. No applicable CLAUDE.md or AGENTS.md was found.

A final quiet 31-round ESTONE comparison after the tests (`beam-estone-final.json`)
gives BEAM **2,527,010**, AtomVM **2,497,275**, ratio **0.988**, CI **0.983–0.996**.
The earlier small win is therefore not robust across fresh runs. The full
performance goal remains open: all 280 compiler files have measured wins,
but ESTONE needs a larger improvement. The next experiment enables link-time
optimization in an isolated build while preserving these validated snapshots.

Final combined-build results: [CHAMP full-node lookup](CHAMP_DENSE_LOOKUP_2026-09-13.md), including all 280 per-file wins, the focused compiler confidence intervals, two independent ESTONE score wins, and validation.
