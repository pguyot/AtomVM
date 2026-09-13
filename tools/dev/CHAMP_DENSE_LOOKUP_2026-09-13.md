<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Full internal CHAMP node lookup — 2026-09-13

Continuation of [direct dictionary lookup](DICTIONARY_GET_2026-09-13.md).
The fresh compiler sample puts termmap_champ_get first among native leaf
samples. A CHAMP node with zero inline entries and all sixteen child bits set
stores children in hash-slot order. Its lookup can index that array directly,
avoiding two bitmap population counts. The root collision flag is masked out
of the data bitmap as before. Collision nodes retain their existing scan.

The lookup also consumes the low four hash bits after each descent, instead
of tracking an increasing shift and applying a variable right shift at every
level. Hash values, term representation, map layout, and AOT ABI are unchanged.
There is no workload-dependent condition.

Artifacts: `build.ab/champ-dense-2026-09-13/`. `base/` is the smaller dictionary
first-entry variant on top of the retained leaf NIF, map construction,
comparison, scalar hash, and integer-selection optimizations. `candidate/`
adds the full-node shortcut and hash consumption. AOT payloads are identical.

## Initial paired result

Eleven rounds, two warmups:

| File | Base median | Candidate median | Paired speedup | Bootstrap 95% CI |
| --- | ---: | ---: | ---: | ---: |
| unicode_util | 2165.4 ms | 2113.6 ms | **1.0241** | **1.0196–1.0287** |
| erl_parse | 1618.8 ms | 1617.5 ms | **1.0023** | **0.9993–1.0052** |

Raw samples: `files.json`. This establishes the compiler gain over the
preceding build. The final measurements below validate the combined build.

## Final result on the established corpus

The final AArch64 AOT build wins every one of the **280 individual files**
against the OTP 29 BEAM JIT in the three-round per-file sweep. The sum of
per-file medians is **80.361 s BEAM versus 31.998 s AtomVM**, **2.511x**.
The corpus contains compiler 59, stdlib 98, kernel 104, sasl 17, and crypto 2
files, with no unsupported source excluded. Startup is included for both
compilers, and execution order alternates. `per-file.json` contains raw
samples; `per-file.csv` lists all medians; `per-file-audit.json` checks the
280 wins and lists the closest files.

The closest per-file median ratios are unicode_util **1.0344**,
beam_core_to_ssa **1.1894**, erl_parse **1.2436**, and erl_lint **1.2486**.
A separate fifteen-round focused comparison, with two warmups, resolves the
tightest case more precisely:

| File | BEAM median | AtomVM median | Paired speedup | Bootstrap 95% CI |
| --- | ---: | ---: | ---: | ---: |
| unicode_util | 2152.7 ms | 2103.5 ms | **1.0236** | **1.0205–1.0268** |
| erl_parse | 1987.9 ms | 1617.3 ms | **1.2279** | **1.2248–1.2305** |

The focused ratios use paired aggregate times, so they need not equal the
ratio of the printed medians. Raw samples: `beam-files.json`.

## Final ESTONE score

Two independent 31-round alternating comparisons, each with three warmups,
separated by the full compiler sweep, both establish a score lead:

| Run | BEAM median ESTONES | AtomVM median ESTONES | Ratio | Bootstrap 95% CI |
| --- | ---: | ---: | ---: | ---: |
| First | 2,487,541 | **2,568,637** | **1.0326** | **1.0301–1.0385** |
| Repeat | 2,470,629 | **2,570,538** | **1.0404** | **1.0337–1.0467** |

Raw samples: `beam-estone.json`, `beam-estone-repeat.json`. This is a win on
ESTONE's weighted score, the requested metric. Total reported runtime remains
higher for AtomVM: 746.3 versus 516.2 ms in the first run and 731.6 versus
517.0 ms in the repeat. The score win should not be described as a total
ESTONE elapsed-time win.

Earlier narrow wins and reversals remain recorded in the preceding reports;
they were not used to declare success. Both final score confidence intervals
are above parity, as is the longer unicode_util comparison.

## Validation and identity

- All 280 compiler outputs are byte-identical to the preceding baseline:
  `correctness.json` and `correctness-baseline.json`.
- Full Release AArch64 JIT, Debug emulator, and assertions-enabled Release JIT
  Erlang suites pass. The six Debug C suites pass.
- All **3,170 JIT tests** pass, as do etest, alisp, eavmlib, and Elixir.
- The standard-library suite has **40 passing modules** and the unchanged
  baseline failure in test_net_kernel:test_autoconnect_to_beam/1, line 226,
  after nxdomain for the short hostname. This run used the build root as cwd
  and the built AtomVM on PATH; local and BEAM serial-peer tests pass.
- The dictionary/leaf fixtures were also checked against BEAM during this
  session. Expanded map tests cover large maps, exact numeric keys, duplicate
  collapse across the flat/hash threshold, and full-hash collisions.
- `test-status.json`, `estdlib-status.json`, and individual logs record the
  results. `benchmark-status.json` records successful benchmark drivers.
  AtomVM ESTONE itself returns status 1 for its tuple result; the driver
  accepts this only after parsing its complete result.

Host: Apple M4 AArch64, macOS 26.6.2, 24 GB, AC power. BEAM is OTP 29.0.3
(`/opt/local/bin/erl` and `erlc`), JIT enabled, ten schedulers. AtomVM uses
Release -O3, JIT and SMP enabled, JIT DWARF disabled, and its default scheduler
count. No single-scheduler override was used. Builds and tests were stopped
during timing. Full commands and host metadata are embedded in the result
JSON files. OTP source commit: `8ce4b017ba3bad0a09ec067b14cf0b12a227022c`.

The native __text of the tested `build.champ/src/AtomVM` is byte-identical to
`candidate/AtomVM`. A final cleanup reduced unrelated whitespace changes in
opcodesswitch.h to its three required leaf-NIF cases; the rebuilt production
executable retains the exact measured native text. Debug metadata can differ.
The standalone `candidate/erlc` contains the same native text plus the retained
compiler AOT payload. Native __text is **454,224 bytes**, SHA-256
`1ef7f9df79289a1bfae988cb41dfa5be600e8040d074eec6b612a4a4ca757069`.
`final-artifacts.json`, `final-inputs.json`, `final-sources.json`, and
`final-implementation.patch` identify executables, benchmark payloads,
configuration, and working-source changes. Sources and executables were
rechecked against these manifests after the final measurements.

## Retained changes and reproduction

The final build combines direct map keys/values projection, scalar hash
inlining, bounded integer select ranges with type guards, comparison fast/slow
splitting, hash-ordered maps:from_list with hash reuse, audited leaf NIF
dispatch, the smaller dictionary first-entry return, and this CHAMP lookup.
The atom-rank alternative was reviewed and left unapplied; its reported
unicode_util result did not improve the compiler. Thin LTO, memory-size
splitting, and other neutral or regressing prototypes remain parked.

```sh
python3 tools/dev/bench_erlc_file_ab.py \
  --a /opt/local/bin/erlc \
  --b build.ab/champ-dense-2026-09-13/candidate/erlc \
  --a-label BEAM --b-label AtomVM --runs 15 --warmup 2 --output focused.json

python3 tools/dev/bench_publication.py erlc \
  --atomvm-erlc build.ab/champ-dense-2026-09-13/candidate/erlc \
  --beam-erl /opt/local/bin/erl --beam-erlc /opt/local/bin/erlc \
  --runs-per-file 3 --runs-batch 0 --output per-file.json

python3 tools/dev/bench_publication.py estone \
  --build build.ab/select-ranges-2026-09-12/publication \
  --atomvm build.ab/champ-dense-2026-09-13/candidate/AtomVM \
  --beam-erl /opt/local/bin/erl --runs 31 --warmup 3 --output estone.json
```
