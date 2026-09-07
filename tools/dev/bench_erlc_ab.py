#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Interleaved A/B of two AtomVM erlc builds over an OTP source corpus.

Same method as bench_publication.py's erlc mode, widened from two engines to
three so that a VM change can be read against both the other build and BEAM in
one sitting: every round compiles a file with each engine, rotating which goes
first, so thermal drift is shared rather than attributed to whichever engine
happens to run last.

    tools/dev/bench_erlc_ab.py --a build.ab/erlc-btree --b build.ab/erlc-champ \
        --a-label btree --b-label champ --runs-per-file 5 --output out.json
"""
import argparse
import glob
import json
import os
import random
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_OTP = ROOT.parent / "otp"
DEFAULT_BEAM_ERLC = Path("/opt/local/bin/erlc")
APP_MACROS = {"crypto": ["-DVSN=\"5.5.3\""]}


def base_includes(otp):
    includes = []
    for directory in sorted(glob.glob(str(otp / "lib/*/include"))):
        includes += ["-I", directory]
    includes += ["-I", str(otp / "erts/include"), "-I", str(otp / "lib")]
    for directory in sorted(glob.glob(str(otp / "lib/*/src"))):
        includes += ["-I", directory]
    return includes


def compile_once(executable, sources, includes, timeout):
    with tempfile.TemporaryDirectory() as output:
        command = [str(executable), "-o", output, *includes, *map(str, sources)]
        start = time.perf_counter()
        try:
            proc = subprocess.run(command, stdout=subprocess.DEVNULL,
                                  stderr=subprocess.DEVNULL, timeout=timeout)
            status = proc.returncode
        except subprocess.TimeoutExpired:
            return float("inf"), set(), "timeout"
        elapsed = time.perf_counter() - start
        produced = {Path(path).stem for path in glob.glob(os.path.join(output, "*.beam"))}
        return elapsed, produced, status


def discover(executable, sources, includes, timeout):
    """Which of these sources this compiler can handle at all."""
    _, produced, _ = compile_once(executable, sources, includes, timeout)
    if len(produced) == len(sources):
        return {source.stem for source in sources}
    supported = set()
    for source in sources:
        _, one, _ = compile_once(executable, [source], includes, timeout)
        if source.stem in one:
            supported.add(source.stem)
    return supported


def boot_ci(a, b, seed=0, iters=20000):
    if len(a) != len(b) or not a:
        return None
    rng = random.Random(seed)
    ratios = []
    for _ in range(iters):
        idx = [rng.randrange(len(a)) for _ in a]
        ratios.append(statistics.median([a[i] for i in idx]) / statistics.median([b[i] for i in idx]))
    ratios.sort()
    return [ratios[int(0.025 * (len(ratios) - 1))], ratios[int(0.975 * (len(ratios) - 1))]]


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--a", type=Path, required=True)
    parser.add_argument("--b", type=Path, required=True)
    parser.add_argument("--a-label", default="A")
    parser.add_argument("--b-label", default="B")
    parser.add_argument("--beam-erlc", type=Path, default=DEFAULT_BEAM_ERLC)
    parser.add_argument("--otp", type=Path, default=DEFAULT_OTP)
    parser.add_argument("--apps", nargs="+",
                        default=["compiler", "stdlib", "kernel", "sasl", "crypto"])
    parser.add_argument("--runs-per-file", type=int, default=5)
    parser.add_argument("--runs-batch", type=int, default=0,
                        help="compile each application in one process this many times; "
                             "immune to the per-process startup quantization the per-file "
                             "mode shows, so it is the better aggregate")
    parser.add_argument("--timeout", type=int, default=900)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    labels = ["BEAM", args.a_label, args.b_label]
    executables = {"BEAM": args.beam_erlc, args.a_label: args.a, args.b_label: args.b}
    for path in executables.values():
        if not Path(path).exists():
            raise SystemExit(f"missing executable: {path}")

    common_includes = base_includes(args.otp)
    corpus = []
    for app in args.apps:
        source_directory = args.otp / "lib" / app / "src"
        sources = sorted(source_directory.glob("*.erl"))
        if not sources:
            raise SystemExit(f"no Erlang sources for application {app}: {source_directory}")
        includes = (["-I", str(source_directory), "-I", str(source_directory.parent / "include")]
                    + common_includes + APP_MACROS.get(app, []))
        supported = {label: discover(executables[label], sources, includes, args.timeout)
                     for label in labels}
        common = set.intersection(*supported.values())
        print(f"support {app}: {len(common)}/{len(sources)} common", file=sys.stderr, flush=True)
        for source in sources:
            if source.stem in common:
                corpus.append((app, source, includes))

    batch = {}
    if args.runs_batch:
        by_app = {}
        for app, source, includes in corpus:
            by_app.setdefault(app, (includes, []))[1].append(source)
        for app_number, (app, (includes, sources)) in enumerate(by_app.items()):
            values = {label: [] for label in labels}
            expected = {s.stem for s in sources}
            for r in range(args.runs_batch):
                rotation = (r + app_number) % len(labels)
                for label in labels[rotation:] + labels[:rotation]:
                    elapsed, produced, status = compile_once(
                        executables[label], sources, includes, args.timeout)
                    if produced != expected:
                        raise RuntimeError(
                            f"batch failed: {app} {label} status={status} "
                            f"{len(produced)}/{len(expected)}")
                    values[label].append(elapsed)
                print(f"batch {app} round {r + 1}/{args.runs_batch}", file=sys.stderr, flush=True)
            batch[app] = {label: statistics.median(values[label]) for label in labels}
            batch[app]["samples"] = values
            print(f"  {app}: " + " ".join(
                f"{label}={batch[app][label]:.3f}s" for label in labels), file=sys.stderr, flush=True)

    per_file = []
    file_number = 0
    for app, source, includes in corpus:
        values = {label: [] for label in labels}
        for r in range(args.runs_per_file):
            rotation = (r + file_number) % len(labels)
            for label in labels[rotation:] + labels[:rotation]:
                elapsed, produced, status = compile_once(
                    executables[label], [source], includes, args.timeout)
                if source.stem not in produced:
                    raise RuntimeError(f"{label} failed on {source} (status {status})")
                values[label].append(elapsed)
        medians = {label: statistics.median(values[label]) for label in labels}
        per_file.append({"app": app, "file": source.stem, "median": medians,
                         "samples": values})
        file_number += 1
        print(f"[{file_number}/{len(corpus)}] {app}/{source.stem} "
              + " ".join(f"{label}={medians[label] * 1000:.0f}ms" for label in labels),
              file=sys.stderr, flush=True)

    a_times = [entry["median"][args.a_label] for entry in per_file]
    b_times = [entry["median"][args.b_label] for entry in per_file]
    beam_times = [entry["median"]["BEAM"] for entry in per_file]
    summary = {
        "files": len(per_file),
        "batch": {app: {label: v[label] for label in labels} for app, v in batch.items()},
        "batch_b_over_a": (
            sum(v[args.a_label] for v in batch.values()) / sum(v[args.b_label] for v in batch.values())
            if batch else None),
        "sum_seconds": {label: sum(entry["median"][label] for entry in per_file)
                        for label in labels},
        # Greater than 1 means B is faster than A.
        "b_over_a_speedup": sum(a_times) / sum(b_times),
        "b_over_a_ci": boot_ci(a_times, b_times),
        "a_vs_beam": sum(beam_times) / sum(a_times),
        "b_vs_beam": sum(beam_times) / sum(b_times),
    }
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps({"summary": summary, "per_file": per_file, "batch": batch}, indent=1))
    print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    main()
