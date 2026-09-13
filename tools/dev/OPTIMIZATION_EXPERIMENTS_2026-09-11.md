<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Follow-up experiments, 2026-09-11

Continuation of [the map projection measurements](MAP_PROJECTION_2026-09-11.md).
The retained starting point is the projection change on `a84676ced`.
The goal remains open: the last controlled BEAM comparison has `unicode_util`
about 3.2% slower and ESTONE about 5% below BEAM.

## Native maps:to_list/1 — parked

Patch: [native-map-to-list-prototype.patch](native-map-to-list-prototype.patch).
Artifacts: `build.ab/to-list-2026-09-11/`.

The native path allocates five heap words per entry instead of constructing
separate key and value lists first (nine words per entry). It also supports
the existing iterator inputs. Focused tests passed for ordered, partial,
materialized and empty iterators, malformed arguments, and maps at sizes
0, 3, 128, 129, 512 and 2048.

Nine alternating rounds (`files.json`) were neutral: baseline/candidate
1.0022, CI 0.9991–1.0053 for `unicode_util`; 0.9998, CI 0.9979–1.0014
for `erl_parse`. Separating the new entry projection from the existing hot
keys/values path (`split/`, `files-split.json`, seven rounds) gave
1.0086, CI 1.0046–1.0129 on `unicode_util`, but `erl_parse` had substantial
outliers and a ratio of 0.9483. This needs a repeat before claiming a win.
The implementation and its tests were removed from the working tree and
preserved in the patch.

Test caveat: literal iterator inputs to `maps:to_list/1` can be optimized as
impossible by the host OTP compiler, eliminating subsequent test code even
though the runtime supports those inputs. The patch uses `id/1` to hide the
literal shape, and the resulting test code was inspected and executed.

## Tagged power-of-two arithmetic — parked

Patch: [tagged-pow2-arithmetic-prototype.patch](tagged-pow2-arithmetic-prototype.patch).
Artifacts: `build.ab/divpow2-2026-09-11/`.

An optional AArch64 backend operation handles signed small-integer division
and remainder by positive powers of two on tagged values. It biases negative
values before shifting to retain truncation toward zero. A shorter division
by two uses ADD with LSR #63, ASR #1, ORR #15: three instructions. The shared
known-nonnegative path also drops an unnecessary untag/retag step.

Both variants passed an AOT arithmetic test across 26 operand values,
including small-integer boundaries and boxed integers, seven powers of two,
and invalid arguments. The first variant removed two hardware SDIV
instructions from ESTONE's generated code. Native VM and library AOT images
were identical between each pair; only the workload's generated code changed.

Neither variant improved the score measurably (21 alternating rounds, two
warmups per engine):

| variant | baseline median | candidate median | score ratio | bootstrap 95% CI |
|---|---:|---:|---:|---:|
| generic tagged sequence | 2,334,179 | 2,335,622 | 1.0006 | 0.9908–1.0054 |
| three-instruction division by two | 2,358,269 | 2,359,668 | 1.0006 | 0.9883–1.0119 |

Logs: `estone.json`, `estone.txt`, `estone-carry.json`, `estone-carry.txt`.
The carry variant's small-integer component moved only 1.003x. Full JIT and
assembler tests were not run for that last variant, so it remains a prototype.
All five arithmetic/JIT source and test files were restored to HEAD after
saving the patch.

## Build and toolchain notes

The default PATH now selects Homebrew OTP 28.5.0.3. The reference BEAM used in
the comparison is MacPorts OTP 29.0.3 at `/opt/local/bin/erl` and `erlc`.
Use these absolute paths, or `PATH=/opt/local/bin:$PATH`, for subsequent work.
The carry prototype's changed JIT modules and tests were compiled explicitly
with OTP 29. Earlier broad rebuilds used the default PATH. OTP 28's
`beam_disasm` cannot decode the benchmark's OTP 29 type chunk.

`build.champ` is Release, JIT/SMP enabled, DWARF disabled. Its precompiled
target list was narrowed to `aarch64` for iteration. Saved benchmark snapshots
are authoritative; the build directory's AOT outputs can contain experimental
JIT code until rebuilt after restoring source.

The Release build was subsequently rebuilt with the restored JIT source and
OTP 29 during scalar-hash validation; the arithmetic prototypes are no longer
in its generated test/library images.

## Scalar hashing — retained

Artifacts: `build.ab/tuplehash-2026-09-11/`. Every compiler snapshot uses the
same AOT payload as the projection report. `base/` is the retained projection
VM; `split/` changes only native hashing. `artifacts.json` records hashes.

The original recursive `hash_term_incr` needs a large native stack frame for
complex traversal. It entered that frame even when hashing an atom or integer
inside a tuple. The current change leaves those two cases in a small inline
dispatcher and puts the recursive traversal in a separate `NOINLINE` function.
This avoids traversal register saves for scalar elements. It adds no cache,
changes no hash arithmetic, and preserves the element-dependent seeds.
Native AArch64 `__text` grows by 1,368 bytes (449,356 to 450,724); no other
architecture's performance or code size was measured. The exact retained
hashing diff is also saved as `retained-hash.patch` in the artifact directory.

Initial screens: nine alternating rounds, one warmup. Ratios are
baseline/candidate; confidence intervals are paired bootstrap 95% intervals.

| variant | unicode_util ratio (CI) | erl_parse ratio (CI) |
|---|---:|---:|
| special atom/integer pair | 1.0025 (0.9994–1.0062) | 0.9979 (0.9934–1.0022) |
| pair plus direct integer path | 1.0062 (1.0021–1.0111) | 1.0011 (0.9983–1.0037) |
| plus expanded one/two-byte fold | 0.9990 (0.9959–1.0021) | 1.0041 (1.0011–1.0078) |
| scalar/complex split alone | 1.0129 (1.0104–1.0154) | 1.0027 (0.9966–1.0072) |
| split with tuple case first | 1.0609 (1.0025–1.1152) | 0.9369 (0.8839–0.9859) |

The last run has substantial timing outliers in both engines; its apparent
large effects do not establish a benefit. A separate nine-round repeat of the
simpler split (`files-repeat.json`) confirms smaller gains on both files:

| source | baseline median | split median | ratio | paired bootstrap 95% CI |
|---|---:|---:|---:|---:|
| unicode_util | 2268.0 ms | 2252.7 ms | 1.0066 | 1.0039–1.0095 |
| erl_parse | 1638.6 ms | 1633.0 ms | 1.0047 | 1.0023–1.0071 |

The earlier specialized paths, expanded fold and tuple-case reorder are not
in the working source. Their source and executable snapshots remain in the
artifact directory alongside raw JSON samples and text summaries.

Validation of the scalar split:

- A differential C harness links the unchanged HEAD hasher and the candidate
  into one process and compares **2,544,318 hashes**. It covers every integer
  from -65536 to 65536, byte and small-integer boundaries up through int64
  extremes, 10,000 deterministic pseudorandom int64s, three atom prefixes,
  boxed/immediate representations, lists, nested tuples and fallback shapes.
  All match. `check_hash.py`, `differential.c`, `reference.c`, and
  `differential-split.log` preserve reproduction and results.
- All **280 compiler sources** across five applications produce byte-identical
  output (`verify_corpus.py`, `correctness.json`, per-application logs).
- Debug, with C assertions: term, heap, structs, bitstring, enif, mailbox and
  Erlang tests pass. The standalone full maps test also passes. Its first
  launch omitted the etest library and failed with `undef`; rerunning with
  the required library resolves that harness error.
- Release: Erlang, JIT (**3,170 tests**), etest, alisp, eavmlib and Elixir tests
  pass. Forty estdlib modules pass; `test_net_kernel` again times out at
  `test_autoconnect_to_beam/1:226` after hostname `nxdomain`, matching the
  unchanged-baseline failure already documented in the projection report.
- The Release build's native `__text` is byte-identical to `split/AtomVM`
  (`tested-text.json`). Clang-format and `git diff --check` pass.

Fresh focused BEAM comparison, nine alternating rounds and one warmup
(`beam-files.json`):

| source | BEAM median | AtomVM median | BEAM/AtomVM | paired bootstrap 95% CI |
|---|---:|---:|---:|---:|
| unicode_util | 2201.4 ms | 2251.1 ms | 0.9790 | 0.9755–0.9819 |
| erl_parse | 2002.2 ms | 1639.3 ms | 1.2188 | 1.2135–1.2228 |

`unicode_util` still needs about 2.1% less wall time to reach parity. The
separate before/after experiment establishes the hashing improvement; the
BEAM comparison establishes the remaining gap under current host conditions.

ESTONE A/B, 21 alternating rounds and two warmups (`estone.json`): baseline
2,332,696 median ESTONES, split 2,330,238; ratio **0.9989**, bootstrap 95% CI
**0.9853–1.0020**. Median reported time is 714.5 ms versus 723.7 ms, ratio
0.9873, CI 0.9714–1.0056. This does not establish a score change, and the
interval still permits a small regression. The split is retained for its
replicated compiler gain, with no ESTONE gain claimed.

Fresh BEAM ESTONE comparison (`beam-estone.json`), 21 alternating rounds and
two warmups: BEAM **2,495,642**, AtomVM **2,338,088** median ESTONES. The score
ratio is **0.9369**, bootstrap 95% CI **0.934–0.951**. The ESTONE target remains
open; the scalar-hash change does not close it. Median reported times are
514.608 ms for BEAM and 728.236 ms for AtomVM.

The refreshed full per-file screen (`corpus.json`, corrected pipe-based timing,
one alternating-order round) finds **279/280 wins**, with `unicode_util` the
only loss. Totals are **80.590 s BEAM / 32.422 s AtomVM**, ratio 2.486.
The next closest file, `beam_core_to_ssa`, is 1.183x faster on AtomVM. This
one-round screen locates remaining losses; use the nine-round focused results
above for the size of the `unicode_util` gap. Do not compare these totals
against the older quantized driver and attribute the difference to hashing.

This pass made progress but did not complete the goal. Both `unicode_util`
and ESTONE still lose to BEAM. The next pass should preserve the measured
projection and scalar-hash changes and use the saved snapshots as baselines.
