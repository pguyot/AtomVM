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

"""Run the AOT aarch64 benchmark AVM repeatedly and report timing statistics.

For each run, AtomVM is launched on the benchmark AVM. Per-test timings are
parsed from the benchmark output (lines of the form "<test>: <microseconds>"),
and the total wall-clock time of the AtomVM process (including VM startup) is
measured with a monotonic clock. After all runs, the average, minimum, maximum
and median are reported for every test and for the total time.

The benchmark AVM is built by tools/dev/build_benchmark_aot_aarch64.sh
(use --build to (re)build it here, or build it yourself first).

Usage:
    tools/dev/bench_benchmark_aot_aarch64.py [-n RUNS] [--build] [--warmup N]

Environment overrides mirror build_benchmark_aot_aarch64.sh:
    ATOMVM_ROOT, BUILD_DIR, TARGET
"""

import argparse
import os
import re
import statistics
import subprocess
import sys
import time
from pathlib import Path

# Matches "pingpong_speed_test: 884472" and
# "pingpong_speed_test [schedulers=1]: 146437". The label (test name plus any
# suffix) becomes the metric key; the trailing integer is microseconds.
LINE_RE = re.compile(r"^(?P<label>\S.*?):\s*(?P<usec>\d+)\s*$")

SCRIPT_DIR = Path(__file__).resolve().parent
ATOMVM_ROOT = Path(os.environ.get("ATOMVM_ROOT", SCRIPT_DIR.parent.parent)).resolve()
BUILD_DIR = Path(os.environ.get("BUILD_DIR", ATOMVM_ROOT / "build")).resolve()
TARGET = os.environ.get("TARGET", "aarch64")

ATOMVM = BUILD_DIR / "src" / "AtomVM"
AVM = BUILD_DIR / f"benchmark-aot-{TARGET}" / f"benchmark-{TARGET}.avm"
ATOMVMLIB = BUILD_DIR / "libs" / f"atomvmlib-{TARGET}.avm"
BUILD_SCRIPT = SCRIPT_DIR / "build_benchmark_aot_aarch64.sh"

TOTAL_KEY = "TOTAL (incl. VM start)"


def build_avm():
    print(f"==> building benchmark AVM via {BUILD_SCRIPT.name}", file=sys.stderr)
    subprocess.run([str(BUILD_SCRIPT), "--no-run"], check=True)


def run_once():
    """Run AtomVM once. Return (total_seconds, {label: microseconds})."""
    start = time.monotonic()
    proc = subprocess.run(
        [str(ATOMVM), str(AVM), str(ATOMVMLIB)],
        capture_output=True,
        text=True,
    )
    total = time.monotonic() - start
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise SystemExit(f"AtomVM exited with code {proc.returncode}")

    timings = {}
    for line in proc.stdout.splitlines():
        m = LINE_RE.match(line)
        if m:
            timings[m.group("label")] = int(m.group("usec"))
    return total, timings


def fmt_usec(value):
    """Format a microsecond value as microseconds with a thousands separator."""
    return f"{value:>14,.1f}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-n", "--runs", type=int, default=20, help="number of timed runs (default: 20)"
    )
    parser.add_argument(
        "--warmup",
        type=int,
        default=1,
        metavar="N",
        help="discarded warmup runs before timing (default: 1)",
    )
    parser.add_argument(
        "--build", action="store_true", help="(re)build the benchmark AVM first"
    )
    args = parser.parse_args()

    if args.build:
        build_avm()

    for f in (ATOMVM, AVM, ATOMVMLIB):
        if not f.exists():
            raise SystemExit(
                f"error: required artifact not found: {f}\n"
                f"       Run with --build, or run {BUILD_SCRIPT.name} first."
            )

    # collected[label] -> list of samples (microseconds; total is in usec too)
    collected = {}
    # Preserve first-seen order so the report follows benchmark output order,
    # with the total appended last.
    order = []

    def record(label, value):
        if label not in collected:
            collected[label] = []
            order.append(label)
        collected[label].append(value)

    for i in range(args.warmup):
        print(f"warmup {i + 1}/{args.warmup}...", file=sys.stderr)
        run_once()

    for i in range(args.runs):
        print(f"run {i + 1}/{args.runs}...", file=sys.stderr)
        total, timings = run_once()
        for label, usec in timings.items():
            record(label, usec)
        record(TOTAL_KEY, total * 1_000_000.0)

    # Report
    label_w = max(len(label) for label in order)
    header = (
        f"{'test'.ljust(label_w)}  {'avg (us)':>14}  {'min (us)':>14}  "
        f"{'max (us)':>14}  {'median (us)':>14}  {'n':>3}"
    )
    print()
    print(
        f"Benchmark AOT {TARGET}: {args.runs} runs "
        f"({args.warmup} warmup discarded)"
    )
    print(header)
    print("-" * len(header))
    for label in order:
        samples = collected[label]
        if label == TOTAL_KEY:
            print("-" * len(header))
        print(
            f"{label.ljust(label_w)}  "
            f"{fmt_usec(statistics.fmean(samples))}  "
            f"{fmt_usec(min(samples))}  "
            f"{fmt_usec(max(samples))}  "
            f"{fmt_usec(statistics.median(samples))}  "
            f"{len(samples):>3}"
        )


if __name__ == "__main__":
    main()
