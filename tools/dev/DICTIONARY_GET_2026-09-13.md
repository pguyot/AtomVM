<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Direct dictionary lookup — 2026-09-13

Continuation of [leaf NIF dispatch](LEAF_NIF_2026-09-13.md). Artifacts are in
`build.ab/dictionary-get-2026-09-13/`. `base/` is the six-function leaf NIF
variant with the late fragment check. `candidate/` adds only the lookup change.
Both standalone compilers contain identical AOT payloads.

## Implementation

The get/1 BIF returns directly for an empty dictionary, a word-identical first
key, or an immediate key found during a word-equality scan. All other cases
use the unchanged dictionary_get implementation through a noinline helper,
including structural equality and allocation-failure handling. Dictionary
storage and update order are unchanged. Immediate-key word equality matches
the existing dictionary_find behavior. The AArch64 fast path has no stack
frame; the compound fallback is a tail branch.

The existing dictionary test now covers empty/missing lookups, integer versus
float keys, atoms, nil, pids, separately constructed compound keys, and lookups
after garbage collection. These cases exercise both the direct path and the
structural fallback.

## Paired candidate measurements

21 ESTONE rounds, two warmups, alternating execution order:

- Score: **2,493,035 → 2,507,838**, ratio **1.0059**, bootstrap 95% CI
  **1.0019–1.0086**.
- BIF dispatch: **260,709 → 286,208**, +9.8%.
- Reported runtime: **735.5 → 679.1 ms**, ratio **1.0830**, CI
  **1.0439–1.1070**. Message components are more variable than BIF dispatch.

Nine compiler rounds, one warmup:

- unicode_util: 2173.1 → 2181.3 ms, paired ratio **0.9932**, CI
  **0.9833–1.0000**.
- erl_parse: 1637.9 → 1651.2 ms, paired ratio **0.9906**, CI
  **0.9869–0.9939**.

The compiler penalty means a direct final comparison against BEAM is required;
the earlier compiler milestone must not be assumed to apply unchanged. Full
validation and final BEAM measurements are in progress. ESTONE scores from
separate experiments have shifted with machine conditions; paired ratios,
not differences between unpaired historical medians, establish the gains.

## Validation and direct BEAM comparison

The first candidate passes full Release, Debug emulator, and assertions-enabled
JIT Erlang tests; the six Debug C suites; all 3,170 JIT tests; etest, alisp,
eavmlib, and Elixir. The dictionary and leaf-NIF fixtures also pass on BEAM.
All 280 compiler outputs are byte-identical to the preceding leaf-NIF build.
The full estdlib run has 40 passing modules and the same baseline failure in
test_net_kernel:test_autoconnect_to_beam/1, line 226, following nxdomain for the
short hostname. Logs and structured statuses are in the artifact directory.
`final-artifacts.json` identifies this tested first candidate; it does not
identify later experiments.

Direct comparison with BEAM, fifteen compiler rounds and two warmups:

- unicode_util: BEAM 2171.5 ms, AtomVM 2168.1 ms; paired mean ratio
  **0.9989**, CI **0.9968–1.0010**. This is a tie, not a proven win.
- erl_parse: BEAM 1989.7 ms, AtomVM 1634.0 ms; **1.2165**, CI
  **1.2133–1.2194**.
- ESTONE, 31 rounds and three warmups: BEAM **2,465,171**, AtomVM
  **2,559,550**; **1.038**, CI **1.031–1.044**.

## Smaller first-entry variant

`first/` removes the duplicated immediate-key scan, retaining direct returns
only for empty dictionaries and a word-identical first key. All other lookups
use dictionary_get. Eleven A/B compiler rounds versus the leaf-only base give
unicode_util **0.9990** (CI **0.9968–1.0008**) and erl_parse **1.0008**
(CI **0.9982–1.0033**): neutral.

Nevertheless its fifteen-round direct BEAM comparison still loses unicode_util:
BEAM **2172.5 ms**, AtomVM **2187.1 ms**, ratio **0.9923**,
CI **0.9892–0.9953**. erl_parse remains ahead at **1.2234**,
CI **1.2171–1.2321**. ESTONE gives BEAM **2,399,960**, AtomVM **2,524,185**,
ratio **1.052**, but a much wider CI **1.000–1.114**. These results do not
establish the full goal; the compiler needs another gain. The smaller variant
is the base for the next CHAMP lookup experiment, not a completed final build.

A fresh ten-second statistical compiler sample is in `profile/sample.txt`.
Native leaf samples are led by termmap_champ_get (751), memmove (547), complex
hashing (408), term_compare0 (316), and the exact equality walker (276).
These are sampled CPU observations, not call counts. The compiler was run on
eight copies of unicode_util in one invocation; profiling is separate from
all timing measurements.

Final combined-build results: [CHAMP full-node lookup](CHAMP_DENSE_LOOKUP_2026-09-13.md), including all 280 per-file wins, the focused compiler confidence intervals, two independent ESTONE score wins, and validation.
