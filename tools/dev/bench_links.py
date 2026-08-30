#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Interleaved BEAM/baseline-AtomVM/current-AtomVM link relation benchmark.

Rotates the three commands within every round so all of them see the same
thermal and load state, and reports per-workload medians together with the
baseline-over-current speedup. The workload is tools/dev/link_bench.erl, which
must be compiled for BEAM (--beam-dir) and packed into an avm for AtomVM
(--avm).
"""

import argparse
import json
import statistics
import subprocess
import time
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent.parent
LINE = "BENCH "


def command(args, label):
    if label == "BEAM":
        return [
            args.erl, "-pa", args.beam_dir, "-noshell", "-s", "link_bench",
            "start", "-s", "init", "stop"
        ]
    binary = args.baseline_atomvm if label == "AtomVM-baseline" else args.atomvm
    return [binary, args.avm, args.atomvmlib]


def run_once(cmd):
    start = time.perf_counter()
    proc = subprocess.run([str(item) for item in cmd], capture_output=True, text=True)
    wall = time.perf_counter() - start
    if proc.returncode != 0:
        raise RuntimeError(
            f"command failed with status {proc.returncode}: {' '.join(map(str, cmd))}\n"
            f"{proc.stdout}\n{proc.stderr}"
        )
    timings = {}
    iterations = {}
    for line in proc.stdout.splitlines():
        if line.startswith(LINE):
            _, name, count, usec = line.split()
            timings[name] = int(usec)
            iterations[name] = int(count)
    if not timings:
        raise RuntimeError(f"no benchmark output from {' '.join(map(str, cmd))}")
    return {"wall_seconds": wall, "usec": timings, "iterations": iterations}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--erl", type=Path, default=Path("/opt/local/bin/erl"))
    parser.add_argument("--beam-dir", type=Path, required=True)
    parser.add_argument("--atomvm", type=Path, required=True)
    parser.add_argument("--baseline-atomvm", type=Path, required=True)
    parser.add_argument("--avm", type=Path, required=True)
    parser.add_argument("--atomvmlib", type=Path, required=True)
    parser.add_argument("--runs", type=int, default=31)
    parser.add_argument("--warmup", type=int, default=3)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    labels = ("BEAM", "AtomVM-baseline", "AtomVM-current")
    commands = {label: command(args, label) for label in labels}
    samples = {label: [] for label in labels}
    for warmup in range(args.warmup):
        for offset in range(len(labels)):
            run_once(commands[labels[(warmup + offset) % len(labels)]])
    for round_number in range(args.runs):
        for offset in range(len(labels)):
            label = labels[(round_number + offset) % len(labels)]
            samples[label].append(run_once(commands[label]))
        print(f"round {round_number + 1}/{args.runs}", flush=True)

    names = sorted(samples[labels[0]][0]["usec"])
    medians = {
        label: {
            name: statistics.median(sample["usec"][name] for sample in samples[label])
            for name in names
        }
        for label in labels
    }
    result = {
        "method": {"runs": args.runs, "warmup": args.warmup, "rotating_order": True},
        "commands": {label: [str(item) for item in cmd] for label, cmd in commands.items()},
        "samples": samples,
        "median_usec": medians,
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")
    for name in names:
        base = medians["AtomVM-baseline"][name]
        current = medians["AtomVM-current"][name]
        beam = medians["BEAM"][name]
        print(
            f"{name:<24} BEAM={beam:>9.0f}us baseline={base:>9.0f}us "
            f"current={current:>9.0f}us A/B={base/current:>6.2f}x "
            f"current/BEAM={beam/current:>6.2f}x"
        )


if __name__ == "__main__":
    main()
