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

"""Run the benchmark on BEAM (erl) repeatedly and report timing statistics.

This mirrors tools/dev/bench_benchmark_aot_aarch64.py, but launches the
benchmark on the official Erlang VM (erl, currently OTP 29) instead of AtomVM.
The benchmark beams are picked up with -pa from the rebar3 build directory; no
AVM is needed.

For each run, per-test timings are parsed from the benchmark output (lines of
the form "<test>: <microseconds>"), and the total wall-clock time of the erl
process (including VM startup) is measured with a monotonic clock. After all
runs, the average, minimum, maximum and median are reported for every test and
for the total time.

Usage:
    tools/dev/bench_benchmark_erl.py [-n RUNS] [--warmup N] [--compile]

Environment overrides:
    ATOMVM_ROOT      AtomVM source root (default: derived from this script)
    BENCHMARK_DIR    Benchmark app directory (default: $ATOMVM_ROOT/../atomvm_benchmark)
    ERL              erl executable (default: erl on PATH)
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
BENCHMARK_DIR = Path(
    os.environ.get("BENCHMARK_DIR", ATOMVM_ROOT.parent / "atomvm_benchmark")
).resolve()
ERL = os.environ.get("ERL", "erl")

EBIN = BENCHMARK_DIR / "_build" / "default" / "lib" / "benchmark" / "ebin"

TOTAL_KEY = "TOTAL (incl. VM start)"


def compile_benchmark():
    print(f"==> rebar3 compile ({BENCHMARK_DIR})", file=sys.stderr)
    subprocess.run(["rebar3", "compile"], cwd=str(BENCHMARK_DIR), check=True)


def run_once():
    """Run erl once. Return (total_seconds, {label: microseconds})."""
    cmd = [
        ERL,
        "-pa",
        str(EBIN),
        "-noshell",
        "-s",
        "benchmark",
        "start",
        "-s",
        "init",
        "stop",
    ]
    start = time.monotonic()
    proc = subprocess.run(cmd, capture_output=True, text=True)
    total = time.monotonic() - start
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise SystemExit(f"erl exited with code {proc.returncode}")

    timings = {}
    for line in proc.stdout.splitlines():
        m = LINE_RE.match(line)
        if m:
            timings[m.group("label")] = int(m.group("usec"))
    return total, timings


def fmt_usec(value):
    """Format a microsecond value as microseconds with a thousands separator."""
    return f"{value:>14,.1f}"


def erl_release():
    """Best-effort OTP release string for the report header."""
    try:
        out = subprocess.run(
            [
                ERL,
                "-noshell",
                "-eval",
                'io:format("~s", [erlang:system_info(otp_release)])',
                "-s",
                "init",
                "stop",
            ],
            capture_output=True,
            text=True,
            check=True,
        )
        return f"OTP {out.stdout.strip()}"
    except Exception:
        return "erl"


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
        "--compile",
        action="store_true",
        help="rebar3 compile the benchmark app first",
    )
    args = parser.parse_args()

    if args.compile:
        compile_benchmark()

    if not EBIN.is_dir():
        raise SystemExit(
            f"error: benchmark ebin not found: {EBIN}\n"
            f"       Run with --compile, or 'rebar3 compile' in {BENCHMARK_DIR} first."
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
        f"Benchmark on {erl_release()} (erl): {args.runs} runs "
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
