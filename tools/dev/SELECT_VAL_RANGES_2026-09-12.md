<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Integer selection ranges — 2026-09-12

Continuation of [the previous experiments](OPTIMIZATION_EXPERIMENTS_2026-09-11.md).
That pass retained scalar hashing, confirmed 279/280 compiler wins, and left
`unicode_util` about 2.1% slower and ESTONE about 6.3% below BEAM. This pass is
in progress; the full goal is not yet established.

## Implementation being validated

`select_val` previously chose a jump table or search tree solely from the
integer values. It now coalesces consecutive values sharing a target into
closed ranges. When at most four ranges replace at least twice as many case
entries, it emits the range tests. Singleton ranges use exact word compares;
other ranges subtract the lower bound and test the unsigned difference.
Range widths are limited to the existing backend immediate corridor (4095).

Four-case selections also enter this dispatcher. If a proven nonnegative
small-integer input already implies a range's lower bound, that range needs
only an upper-bound comparison. This applies to ordinary BEAM type facts;
there are no module, function, or benchmark-specific conditions.

## A pre-existing correctness bug

The dense integer jump table previously omitted the small-integer tag check
for untyped input. An atom or local pid whose word fell inside the numeric
span could become a misaligned computed jump. The new `test_select_val_ranges`
crashes the baseline with **SIGBUS** (exit status -10 on this host) and passes
on the candidate. Dense tables and coalesced ranges now check the tag unless
the input type proves it is a small integer.

The test covers dense distinct destinations, positive ranges with holes,
negative ranges, a bounded four-case selector, floats, atoms, nil, tuples,
binaries, local pids, refs, funs, and large positive/negative integers. It is
registered in the regular Erlang test suite.

## Initial controlled ESTONE measurements

Artifacts: `build.ab/select-ranges-2026-09-12/`. Native VM and library AOT
images are identical between each measured pair. The same OTP 29 raw ESTONE
beam is precompiled separately with baseline and candidate JIT modules.
Every run uses 21 alternating rounds and two warmups.

| variant | baseline median | candidate median | score ratio | paired bootstrap 95% CI |
|---|---:|---:|---:|---:|
| coalesced ranges, N >= 6 | 2,353,394 | 2,490,464 | 1.0582 | 1.0400–1.0847 |
| plus four-case and proven lower bounds | 2,337,468 | 2,498,833 | 1.0690 | 1.0488–1.0808 |

The pattern-matching component improves 12.2% and 14.2%, respectively.
Reported total time moves 723.8 to 721.0 ms in the first run and 721.4 to
715.5 ms in the second; both time confidence intervals include parity.
Logs: `estone.json` / `.txt` and `estone-bounded.json` / `.txt`.
These comparisons establish an improvement over AtomVM's baseline, not yet
a win over BEAM.

`base/`, `candidate/`, and `bounded/` preserve JIT beams, generated workload
images, and source snapshots. The initial regression results are in
`test-results.json`, `test-base.log`, `test-candidate.log`, and
`test-bounded.log`.

## Rebuild and validation

`build_compilers.py` rebuilds both compiler engines from the same OTP compiler
inputs and raw AtomVM library beams, all copied into the artifact directory.
`input-hashes.json` records their identities. Outputs go to `base/` and
`bounded/`; the newly compiled library is named `atomvmlib-full.avm` to keep
the initial ESTONE library unchanged. The existing sibling build script was
read for packaging details; its output directory outside this workspace was
not modified.

Both rebuilds completed: 100 compiler beams and 169 library beams per engine.
The Release build also rebuilt all AArch64 AOT images with the current JIT.

- All **280 compiler sources** produce byte-identical output between the new
  baseline and bounded-range engines (`correctness.json` and per-app logs).
- Release Erlang tests, including the new regression, pass. The first run
  exposed a fixture dependency on `lists:foreach/2`, which the C test runner's
  minimal lists module does not implement; the fixture now uses local
  sequence, membership, and traversal helpers, and the full suite passes.
- All **3,170 JIT tests**, etest, alisp, eavmlib and Elixir tests pass.
- Forty estdlib modules pass. The existing `test_net_kernel` hostname failure
  repeats at `test_autoconnect_to_beam/1:226`, with `nxdomain`, matching the
  previously established baseline failure.
- The standalone regression passes on BEAM, the Debug emulator, and AArch64
  AOT. x86-64 precompilation succeeds, but x86-64 native execution has not
  been tested on this AArch64 host.

`base-full/` and `bounded-full/` use the separately regenerated library
images for subsequent complete-image ESTONE comparisons. `artifacts.json`
records image and executable hashes. Compiler timings and a fresh BEAM ESTONE
comparison completed for the bounded variant:

- Compiler A/B, nine rounds (`files.json`): `unicode_util` ratio 1.0022,
  CI 0.9990–1.0055; `erl_parse` 1.0001, CI 0.9982–1.0018. No compiler speed
  change is established.
- Fresh BEAM ESTONE, 21 rounds (`beam-estone.json`): BEAM **2,468,311**,
  AtomVM **2,478,127** median ESTONES, ratio **1.0040**, CI **0.997–1.009**.
  The median is slightly higher, but this is still parity within uncertainty.
- Fresh BEAM compiler, nine rounds (`beam-files.json`): `unicode_util`
  2223.1 ms versus 2287.6 ms, ratio **0.9740**, CI 0.9695–0.9789;
  `erl_parse` 2023.0 ms versus 1666.5 ms, ratio **1.2128**, CI 1.2108–1.2148.

Neither complete performance goal is yet established. A further `priority/`
variant tested singleton equalities before range checks, since the equalities
need no range temporary/subtraction. Ranges are disjoint, so their order does
not affect selection semantics. Its standalone regression passes; its
21-round A/B against `bounded/` gave 0.9845, CI 0.9365–1.0080, with substantial
noise. It does not establish a benefit and is not retained. The JIT source was
restored to the validated bounded-range implementation. Its snapshots and
`estone-priority.json` retain the experiment.


## Quiet repeat and parked shift-predicate experiment

A quiet 31-round BEAM comparison with three warmups (`beam-estone-repeat.json`)
puts BEAM at **2,550,227** and AtomVM at **2,499,716** median ESTONES:
ratio **0.980**, paired bootstrap 95% CI **0.970–0.988**. The validated build
still needs about 2% more score; the earlier near-parity run did not establish
a win. Benchmark output was redirected and other task activity paused while
timing to reduce interference.

A subsequent shift-predicate prototype removes a proven bounded small-integer
left shift when its result is used only by an equality or short selection.
It substitutes the constant's preimage and checks destination liveness on all
outgoing paths. No module or benchmark names participate in the transformation.
The prototype passes 32 liveness tests and a differential runtime fixture with
2,001 integers, negative and impossible constants, live results, invalid types,
and bigint fallbacks. ESTONE's native code shrinks by 40 bytes.

The quiet 31-round A/B gives **2,504,424** baseline versus **2,507,253** candidate,
ratio **1.0011**, CI **0.9964–1.0041**. This does not establish a benefit, so
source changes were restored and the prototype is parked in
`tools/dev/bsl-predicate-prototype.patch`. Sources, fixtures, generated code,
and measurements remain in `build.ab/shift-test-2026-09-12/`.

A fresh three-second compiler profile is in `profile/sample.txt` under the
selection-range artifact directory. Of 2,322 samples, term comparison accounts
for 232 leaf samples, memory copying 209, CHAMP lookup 200, complex hashing
101, and external-call dispatch 57. These are sampling observations, not
instrumented call counts.
