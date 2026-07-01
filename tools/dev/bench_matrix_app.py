#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

"""Run the AtomVM benchmark app across the full build matrix and against BEAM.

Configurations compared (all AtomVM builds at the same optimisation level):

    BEAM            official Erlang VM (erl), reference
    emu noSMP       AtomVM interpreter,   AVM_DISABLE_JIT=ON  AVM_DISABLE_SMP=ON
    emu SMP         AtomVM interpreter,   AVM_DISABLE_JIT=ON  AVM_DISABLE_SMP=OFF
    JIT noSMP       AtomVM AOT (native),  AVM_DISABLE_JIT=OFF AVM_DISABLE_SMP=ON
    JIT SMP         AtomVM AOT (native),  AVM_DISABLE_JIT=OFF AVM_DISABLE_SMP=OFF

For each configuration the benchmark is run RUNS times (after WARMUP discarded
runs). Per-test timings are parsed from the benchmark output (lines of the form
"<label>: <microseconds>"); the median across runs is reported per test, plus
the median total wall-clock time (VM startup included). The last column is the
speed-up of each config relative to BEAM on the total wall time.

The build-matrix paths below match tools/dev/BENCHMARK_MATRIX.md. Override the
roots with env vars if your layout differs.

Usage:
    tools/dev/bench_matrix_app.py [-n RUNS] [--warmup N] [--only LABELS]
"""

import argparse
import os
import re
import statistics
import subprocess
import sys
import time
from pathlib import Path

LINE_RE = re.compile(r"^(?P<label>\S.*?):\s*(?P<usec>\d+)\s*$")

ATOMVM_ROOT = Path(os.environ.get("ATOMVM_ROOT", Path(__file__).resolve().parent.parent.parent))
BENCHMARK_DIR = Path(os.environ.get("BENCHMARK_DIR", ATOMVM_ROOT.parent / "atomvm_benchmark"))
EBIN = BENCHMARK_DIR / "_build" / "default" / "lib" / "benchmark" / "ebin"
ERL = os.environ.get("ERL", "erl")

TOTAL_KEY = "TOTAL wall (incl. VM start)"

# Each AtomVM config: (label, AtomVM binary, benchmark avm, atomvmlib avm).
# BEAM is handled separately.
def atomvm_cmd(build, avm, lib):
    return [str(ATOMVM_ROOT / build / "src" / "AtomVM"),
            str(avm if Path(avm).is_absolute() else ATOMVM_ROOT / build / avm),
            str(ATOMVM_ROOT / build / "libs" / lib)]


TARGET = os.environ.get("TARGET", "aarch64")
AOT_AVM = f"benchmark-aot-{TARGET}/benchmark-{TARGET}.avm"
AOT_LIB = f"atomvmlib-{TARGET}.avm"
PLAIN_AVM = str(ATOMVM_ROOT / "build.emu" / "benchmark-plain.avm")

# Order: slowest-expected first so the table reads emu -> JIT -> reference.
CONFIGS = [
    ("emu noSMP", atomvm_cmd("build.emu.nosmp", PLAIN_AVM, "atomvmlib.avm")),
    ("emu SMP",   atomvm_cmd("build.emu",       PLAIN_AVM, "atomvmlib.avm")),
    ("JIT noSMP", atomvm_cmd("build.nosmp",     AOT_AVM,   AOT_LIB)),
    ("JIT SMP",   atomvm_cmd("build.release",   AOT_AVM,   AOT_LIB)),
]

# ONLY="emu SMP,JIT SMP" selects a subset (also honoured by the interleave
# driver, which imports CONFIGS from this module).
_ONLY = os.environ.get("ONLY")
if _ONLY:
    _want = [s.strip() for s in _ONLY.split(",")]
    CONFIGS = [c for c in CONFIGS if c[0] in _want]

BEAM_CMD = [ERL, "-pa", str(EBIN), "-noshell", "-s", "benchmark", "start",
            "-s", "init", "stop"]


def run_once(cmd):
    start = time.monotonic()
    proc = subprocess.run(cmd, capture_output=True, text=True)
    total = time.monotonic() - start
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise SystemExit(f"command failed ({proc.returncode}): {' '.join(cmd)}")
    timings = {}
    for line in proc.stdout.splitlines():
        m = LINE_RE.match(line)
        if m:
            timings[m.group("label")] = int(m.group("usec"))
    timings[TOTAL_KEY] = total * 1_000_000.0
    return timings


def bench_config(cmd, runs, warmup):
    for _ in range(warmup):
        run_once(cmd)
    samples = {}
    for _ in range(runs):
        for label, usec in run_once(cmd).items():
            samples.setdefault(label, []).append(usec)
    return {label: statistics.median(vals) for label, vals in samples.items()}


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-n", "--runs", type=int, default=15)
    ap.add_argument("--warmup", type=int, default=2)
    ap.add_argument("--only", default=None,
                    help="comma-separated config labels to run (default: all + BEAM)")
    args = ap.parse_args()

    if not EBIN.is_dir():
        raise SystemExit(f"benchmark ebin not found: {EBIN}\n"
                         f"  rebar3 compile in {BENCHMARK_DIR} first")

    wanted = set(args.only.split(",")) if args.only else None
    results = {}  # config label -> {test: median usec}
    col_order = []

    def want(label):
        return wanted is None or label in wanted

    if want("BEAM"):
        print("BEAM ...", file=sys.stderr)
        results["BEAM"] = bench_config(BEAM_CMD, args.runs, args.warmup)
        col_order.append("BEAM")
    for label, cmd in CONFIGS:
        if not want(label):
            continue
        print(f"{label} ...", file=sys.stderr)
        results[label] = bench_config(cmd, args.runs, args.warmup)
        col_order.append(label)

    # Row order: first-seen test labels across all configs, TOTAL last.
    rows = []
    for cfg in col_order:
        for label in results[cfg]:
            if label != TOTAL_KEY and label not in rows:
                rows.append(label)
    rows.append(TOTAL_KEY)

    label_w = max([len(r) for r in rows] + [4])
    colw = 13
    header = "test".ljust(label_w) + "".join(c.rjust(colw) for c in col_order)
    print()
    print(f"AtomVM benchmark matrix  (median us over {args.runs} runs, "
          f"{args.warmup} warmup)")
    print(header)
    print("-" * len(header))
    for label in rows:
        if label == TOTAL_KEY:
            print("-" * len(header))
        cells = []
        for cfg in col_order:
            v = results[cfg].get(label)
            cells.append(("" if v is None else f"{v:,.0f}").rjust(colw))
        print(label.ljust(label_w) + "".join(cells))

    # Headline aggregate: sum of per-test medians over the base tests that
    # EVERY config runs (exclude the SMP-only "[schedulers=1]" duplicate rows
    # and the wall-time row). This is the stable compute-time comparison; the
    # raw process wall time is reported alongside as it is startup-dominated on
    # so short a workload (and penalises SMP for spawning scheduler threads).
    base_tests = [r for r in rows
                  if r != TOTAL_KEY and "[schedulers=" not in r]

    def agg(cfg):
        return sum(results[cfg].get(t, 0.0) for t in base_tests)

    if "BEAM" in results:
        base = agg("BEAM")
        bw = results["BEAM"][TOTAL_KEY]
        print()
        print(f"Aggregate over {len(base_tests)} base tests "
              "(sum of medians; speed-up vs BEAM, higher = faster):")
        print(f"  {'config':<12}{'compute (ms)':>14}{'vs BEAM':>10}"
              f"{'   median wall (ms)':>20}")
        for cfg in col_order:
            c = agg(cfg)
            w = results[cfg][TOTAL_KEY]
            print(f"  {cfg:<12}{c/1000:>14,.1f}{base / c:>9.2f}x"
                  f"{w/1000:>17,.1f}")


if __name__ == "__main__":
    main()
