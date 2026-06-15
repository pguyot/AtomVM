#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Interleaved benchmark-app driver: like bench_matrix_app.py but round-robins
the configs WITHIN each run so every config sees nearly the same thermal/load
state per run -- the config-vs-config ratio is then robust to thermal drift over
the run. Median over N runs; per-test microseconds + aggregate."""

import os
import sys
import statistics

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import bench_matrix_app as B  # noqa: E402

N = int(os.environ.get("RUNS", "15"))
WARMUP = int(os.environ.get("WARMUP", "2"))

CFGS = [("BEAM", B.BEAM_CMD)] + B.CONFIGS

for _, cmd in CFGS:
    for _ in range(WARMUP):
        B.run_once(cmd)

samples = {lab: {} for lab, _ in CFGS}
for _ in range(N):
    for lab, cmd in CFGS:
        for test, usec in B.run_once(cmd).items():
            samples[lab].setdefault(test, []).append(usec)

med = {lab: {t: statistics.median(v) for t, v in d.items()} for lab, d in samples.items()}

rows = []
for lab, _ in CFGS:
    for t in med[lab]:
        if t != B.TOTAL_KEY and t not in rows:
            rows.append(t)

labels = [lab for lab, _ in CFGS]
print(f"# interleaved, RUNS={N}, WARMUP={WARMUP} (median us)")
print(f"{'test':<32}" + "".join(l.rjust(11) for l in labels))
for t in rows + [B.TOTAL_KEY]:
    cells = "".join((f"{med[l][t]:>11.0f}" if t in med[l] else f"{'-':>11}") for l in labels)
    print(f"{t:<32}{cells}")

# aggregate: sum of the base tests every config runs (exclude [sched=1] rows + TOTAL)
base = [t for t in rows if "[sched" not in t]
print("\n# aggregate over base tests (sum of medians, ms; vs BEAM):")
beam_sum = sum(med["BEAM"][t] for t in base if t in med["BEAM"]) / 1000.0
for l in labels:
    s = sum(med[l][t] for t in base if t in med[l]) / 1000.0
    wall = med[l].get(B.TOTAL_KEY, 0) / 1000.0
    print(f"  {l:<11} compute={s:8.1f}ms  vs BEAM={beam_sum/s:5.2f}x  wall={wall:8.1f}ms")
