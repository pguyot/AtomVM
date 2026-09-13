<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# CHAMP map projection — 2026-09-11

Follow-up to [the CHAMP report](CHAMP_MAP_BACKEND_2026-09-07.md).
The goal remains beating BEAM on every compiler source and on ESTONE;
this change alone does not establish that result.

## Change

`maps:keys/1` and `maps:values/1` now reserve the result list and walk the
CHAMP trie directly. Previously they allocated a temporary key/value array
and heap-sorted it, then repeated the materialization and sort if reserving
the result triggered GC. Projection order is unspecified. Both projections
use the same traversal, preserving the pairing that `maps:to_list/1` needs.
Flat-map projections retain their existing traversal.

This removes sorting and scratch allocation from the hash-map path, with no
new representation, cache, or public API. The map is rooted across the heap
reservation, and the subsequent visitor cannot trigger GC.

Tests that assumed sorted projections now check completeness, unique keys,
value contents and the `to_list` round trip. New cases use compound keys and
values at sizes 128, 129, 512 and 2048 and repeat after collections.

## Controlled measurements

Baseline: `a84676ced`, rebuilt in `build.champ`, Release, JIT/SMP enabled,
JIT DWARF disabled, no instrumentation flags. Native AArch64 on Apple M4.
Both compiler executables contain the **same AOT payload**, extracted from
`build.ab/erlc-atomcache2`; only the native VM changes. ESTONE likewise uses
identical AOT workload and library images. Each round reverses engine order.

Artifacts and logs: `build.ab/projection-2026-09-11/`. `artifacts.json`
records SHA-256 hashes; `base/` and `candidate/` hold the measured engines.

Nine rounds, one warmup, `bench_erlc_file_ab.py`; ratios are baseline/candidate:

| source | baseline median | candidate median | ratio of sums | paired bootstrap 95% CI |
|---|---:|---:|---:|---:|
| `unicode_util` | 2329.9 ms | 2290.7 ms | 1.0154 | 1.0119–1.0192 |
| `erl_parse` | 1693.8 ms | 1662.4 ms | 1.0186 | 1.0159–1.0214 |

Log: `files.txt`. This first run predates the driver's JSON output option;
its individual samples were not retained. The driver now accepts `--output`
to save samples, engine paths and executable hashes.

ESTONE, 21 rounds and two warmups (`estone.json`, `estone.txt`):

- Baseline median: 2,325,623 ESTONES; candidate: 2,329,831.
- Score ratio: **1.0018**, bootstrap 95% CI **0.9996–1.0106**: neutral.
- Total measured time: 750.5 ms → 715.8 ms, ratio 1.0485.

Do not attribute the message-time improvement to map projection. These
components are sensitive to native code layout, as earlier reports establish,
and this change does not remove work from their message path.

## A merge-cutoff experiment not retained

Changing `MAPS_MERGE_SMALL_MAX` from 8 to 32 on top of projection gave:

| source | ratio | paired bootstrap 95% CI |
|---|---:|---:|
| `unicode_util` | 1.0045 | 1.0011–1.0075 |
| `erl_parse` | 1.0031 | 0.9977–1.0089 |

Seven interleaved rounds, one warmup; `merge32.json` and `merge32.txt`.
The small effect does not justify changing a cutoff globally without a
broader merge workload study. Source is restored to 8; the experimental
binary remains in `merge32/`.

## Compiler support and correctness

All **280** sources across compiler, stdlib, kernel, sasl and crypto produce
byte-identical output between baseline and candidate. `correctness.json`
covers the 279 sources accepted by the previous driver; the supplemental
`beam-asm-correctness.json` covers the remaining source.

The earlier A/B driver omitted `-DCOMPILER_VSN='"0"'`, so `beam_asm.erl`
failed on both engines and disappeared from its common corpus. The publication
driver already supplied this macro. Both A/B drivers now supply it too;
the actual subprocess argument is `-DCOMPILER_VSN="0"`, without shell quotes.
BEAM also successfully compiles this source with the macro. This is a tooling
correction, not a new compiler compatibility fix.

## Current comparison with BEAM

Nine interleaved rounds, one warmup (`beam-files.json`, `beam-files.txt`):

| source | BEAM median | AtomVM median | BEAM/AtomVM ratio | paired bootstrap 95% CI |
|---|---:|---:|---:|---:|
| `unicode_util` | 2229.5 ms | 2301.6 ms | **0.9689** | 0.9639–0.9741 |
| `erl_parse` | 2035.9 ms | 1673.7 ms | **1.2154** | 1.2113–1.2193 |

`erl_parse` wins in this session; `unicode_util` still needs about 3.2% less
wall time to reach parity. Neither ratio proves performance on other files.

The full one-round screening run (`corpus.json`) finds **279/280 wins**,
with `unicode_util` the sole loss. Per-file totals are 89.027 s BEAM,
39.312 s baseline and 39.306 s candidate. This is aggregate parity between
AtomVM builds, not evidence of a corpus-wide speedup. See the timing caveat
below before interpreting any individual row.

Current ESTONE comparison (`beam-estone.json`): 21 rounds, two warmups,
BEAM **2,451,193** median ESTONES, AtomVM **2,328,052**. AtomVM/BEAM is
**0.950**, paired bootstrap 95% CI **0.946–0.959**. The ESTONE goal also
remains open. Median reported time is **516.296 ms BEAM / 707.315 ms
AtomVM**. Host and toolchain metadata are included in that JSON.

## The per-file timing quantization was in the driver

The earlier reports observed roughly 60 ms steps in short compiler timings.
The local Python 3.9 implementation of `subprocess.Popen._wait(timeout)`
explains them: with both streams redirected to DEVNULL it checks `waitpid`
nonblockingly, then sleeps exponentially up to 50 ms between checks. The
timed interval ends on the next poll, not when the compiler exits.

`bench_erlc_ab.py` and `bench_publication.py` now capture a pipe and drain it
with `communicate`, which wakes on EOF while retaining timeout handling.
`bench_erlc_file_ab.py` already used a blocking wait without a timeout and was
unaffected, including the focused A/B and BEAM comparisons above.

Five alternating trials of `/bin/sleep` demonstrate the difference on this
host (`wait-quantization.json`); values include process startup:

| requested sleep | DEVNULL + timeout median | pipe + timeout median |
|---|---:|---:|
| 20 ms | 44.76 ms | 31.22 ms |
| 80 ms | 144.25 ms | 90.38 ms |
| 140 ms | 200.46 ms | 152.00 ms |

The corpus screening run started before this fix was applied, so its
`corpus.json` timings still include polling delay. Treat them as screening,
not evidence for small individual improvements, and do not compare old and
new driver measurements as if a VM optimization caused their difference.

## Validation

- Release AArch64 `test-erlang`, `jit`, `etest`, `alisp`, `eavmlib` and
  Elixir `Tests.avm` pass.
- Debug emulator build, with C assertions enabled: `test-heap`, `test-term`,
  `test-structs`, `test-bitstring`, `test-mailbox`, `test-enif` and
  `test-erlang` all pass. A standalone runner of the complete `test_maps`
  module also passes, including the new projection tests.
- Full Release `estdlib` with networking permitted: 40 modules pass,
  including `test_maps` and serial-distribution tests. `test_net_kernel`
  fails at `test_autoconnect_to_beam/1:226` after hostname `nxdomain`
  errors. The unchanged baseline reproduces the same failure at the same
  line (`base-test-net-kernel.log`). Initial sandbox-only networking failures
  are superseded by these unrestricted runs.
- Python syntax checks pass. Both corrected corpus drivers successfully
  compile `beam_asm.erl` with the candidate (`driver-smoke.json`).
- `git diff --check` passes. Restoring the experimental cutoff and rebuilding
  changes Mach-O metadata/signatures, but the native `__text` section is
  byte-identical to the measured candidate.

The requested root `CLAUDE.md` is absent from this checkout; test commands
were taken from `.github/workflows/build-and-test-macos.yaml` and the CMake
test definitions. Full logs are in the artifact directory above.

## Remaining work

The full goal is not complete: `unicode_util` loses by about 3.2%, and
ESTONE's median score is about 5% below BEAM. The corrected timing drivers
should be used for subsequent measurements. A larger merge cutoff alone is
not a substantial next step. Investigate eliminating the intermediate key
and value lists built by `maps:to_list/1`, or the remaining compound-key
hashing and insertion cost. Any native `to_list` path must retain the
iterator inputs accepted by the existing Erlang implementation.

Later experiments and validation are recorded in
[the follow-up report](OPTIMIZATION_EXPERIMENTS_2026-09-11.md), including the
parked native `to_list` and tagged division prototypes and scalar-hash results.
