<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Comparison frame split — 2026-09-12

Continuation of [integer selection ranges](SELECT_VAL_RANGES_2026-09-12.md).
Artifacts: `build.ab/compare-split-2026-09-12/`.

The fresh unicode_util profile identifies term comparison as 232 of 2,322
leaf samples. `term_compare0` combines bounded recursive tuple/list fast paths
with the general temporary-stack walker. Clang also inlines the exact-equality
walker into it. On AArch64 the resulting frame is 560 bytes on every recursive
call, including calls that resolve without the general walker.

`candidate/` extracts the general walker into a noinline function, without
changing its body, reducing the fast function's frame to 432 bytes. The first
nine-round alternating comparison (`files.json`) gives unicode_util **1.0142**,
95% CI **1.0112–1.0171**, and erl_parse **1.0120**, CI **1.0109–1.0132**.
All **280 compiler sources** produce identical output to the baseline
(`correctness.json`). The Debug term and Erlang suites pass.

`both/` additionally prevents inlining of the existing exact-equality walker.
This reduces the fast function's frame to **80 bytes**. It improves unicode_util by **1.0076**, CI **1.0049–1.0104**, and erl_parse by
**1.0056**, CI **0.9994–1.0110**, against the original baseline. A direct comparison
with `candidate/` favors the first variant: both/candidate speed ratio **0.9920**,
CI **0.9896–0.9946**, for unicode_util, and **0.9962**, CI **0.9923–1.0001**, for
erl_parse (`files-direct.json`). The smaller frame alone is insufficient: moving
the shallow exact-equality checks across a call boundary adds work. The second
variant passes the Release Erlang suite, but was restored pending a more focused
split. The original first variant is retained for now. The baseline includes all retained optimizations
from the preceding report, and every variant uses identical AOT payloads;
only native term comparison changes.


## Final selection and validation

Splitting only the general ordering walker reproducibly regresses ESTONE
message runtime: first 21 rounds, 730.3 to 785.9 ms; repeat 31 rounds,
735.6 to 780.3 ms (time ratio 0.9427, CI 0.9286–0.9528). Scores are statistically
neutral, but the runtime regression prevents retaining that variant alone.

`shallow/` keeps the existing shallow exact-equality checks inline and splits
only their temporary-stack fallback into a second noinline function. Its direct
compiler comparison with `candidate/` is statistically neutral: unicode_util
0.9985 (CI 0.9951–1.0018), erl_parse 0.9962 (CI 0.9919–1.0003).
It avoids the message regression: 21 rounds give scores 2,486,646 versus
2,488,149 (ratio 1.0006, CI 0.9986–1.0040) and time 722.2 versus 718.0 ms
(ratio 1.0059, CI 0.9965–1.0166). This is the retained comparison variant.

All 280 compiler outputs match the baseline with `shallow/` too (the latest
`correctness.json` records this pair). Release Erlang, Debug Erlang, and Debug
term tests pass. Sources and the alternative patches remain in the artifacts.
The final combined compiler measurement is recorded in
[hash-ordered map construction](FROM_LIST_HASH_2026-09-12.md); no separate
baseline-versus-shallow compiler repeat was used to claim its contribution.

Before the final variant, `candidate/` versus BEAM gave unicode_util 0.9798
(CI 0.9769–0.9824) and erl_parse 1.2265 (CI 1.2214–1.2308). This establishes
neither a unicode_util win nor an ESTONE win; further work is required.
