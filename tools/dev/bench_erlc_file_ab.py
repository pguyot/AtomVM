#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Interleaved A/B of two AtomVM erlc builds on a few named OTP sources.

The corpus driver (bench_erlc_ab.py) answers "does this change the compiler
overall"; this one answers "does it change the files the change is aimed at",
which for a map or hashing change is a handful of map-heavy modules. Same
method -- every round compiles the file with each engine, rotating which goes
first -- but concentrated enough to resolve an effect the corpus average
buries.

    tools/dev/bench_erlc_file_ab.py --a build.ab/erlc-a --b build.ab/erlc-b \
        --files stdlib/unicode_util stdlib/erl_parse --runs 21
"""
import argparse
import glob
import hashlib
import json
import random
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_OTP = ROOT.parent / "otp"
APP_MACROS = {"compiler": ['-DCOMPILER_VSN="0"'], "crypto": ['-DVSN="5.5.3"']}


def includes_for(otp, app):
    source = otp / "lib" / app / "src"
    inc = ["-I", str(source), "-I", str(source.parent / "include")]
    for directory in sorted(glob.glob(str(otp / "lib/*/include"))):
        inc += ["-I", directory]
    inc += ["-I", str(otp / "erts/include"), "-I", str(otp / "lib")]
    for directory in sorted(glob.glob(str(otp / "lib/*/src"))):
        inc += ["-I", directory]
    return inc + APP_MACROS.get(app, [])


def compile_once(executable, source, inc):
    with tempfile.TemporaryDirectory() as output:
        start = time.perf_counter()
        proc = subprocess.run([str(executable), "-o", output, *inc, str(source)],
                              stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        elapsed = time.perf_counter() - start
        if proc.returncode != 0 or not glob.glob(f"{output}/*.beam"):
            raise RuntimeError(f"{executable} failed on {source}")
        return elapsed


def boot_ci(a, b, seed=0, iters=20000):
    rng = random.Random(seed)
    ratios = []
    for _ in range(iters):
        idx = [rng.randrange(len(a)) for _ in a]
        ratios.append(sum(a[i] for i in idx) / sum(b[i] for i in idx))
    ratios.sort()
    return ratios[int(0.025 * (len(ratios) - 1))], ratios[int(0.975 * (len(ratios) - 1))]


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--a", type=Path, required=True)
    parser.add_argument("--b", type=Path, required=True)
    parser.add_argument("--a-label", default="A")
    parser.add_argument("--b-label", default="B")
    parser.add_argument("--otp", type=Path, default=DEFAULT_OTP)
    parser.add_argument("--files", nargs="+", default=["stdlib/unicode_util", "stdlib/erl_parse"],
                        help="app/module pairs")
    parser.add_argument("--runs", type=int, default=21)
    parser.add_argument("--warmup", type=int, default=2)
    parser.add_argument("--output", type=Path, help="save raw samples and artifact hashes as JSON")
    args = parser.parse_args()

    engines = [(args.a_label, args.a), (args.b_label, args.b)]
    if args.a_label == args.b_label:
        parser.error("engine labels must differ")
    result = {
        "engines": {label: {"path": str(exe.resolve()),
                             "sha256": hashlib.sha256(exe.read_bytes()).hexdigest()}
                    for label, exe in engines},
        "otp": str(args.otp.resolve()), "runs": args.runs, "warmup": args.warmup,
        "files": {},
    }
    for spec in args.files:
        app, module = spec.split("/")
        source = args.otp / "lib" / app / "src" / f"{module}.erl"
        inc = includes_for(args.otp, app)
        for _ in range(args.warmup):
            for _, exe in engines:
                compile_once(exe, source, inc)
        samples = {label: [] for label, _ in engines}
        for r in range(args.runs):
            order = engines if r % 2 == 0 else engines[::-1]
            for label, exe in order:
                samples[label].append(compile_once(exe, source, inc))
            print(f"  {spec} round {r + 1}/{args.runs}", file=sys.stderr, flush=True)
        a = samples[args.a_label]
        b = samples[args.b_label]
        low, high = boot_ci(a, b)
        result["files"][spec] = {"samples_seconds": samples,
                                 "ratio_a_over_b": sum(a) / sum(b),
                                 "ci95": [low, high]}
        if args.output:
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_text(json.dumps(result, indent=2) + "\n")
        print(f"{spec}: {args.a_label}={statistics.median(a) * 1000:.1f}ms "
              f"{args.b_label}={statistics.median(b) * 1000:.1f}ms "
              f"A/B={sum(a) / sum(b):.4f}x (95% CI {low:.4f}-{high:.4f})", flush=True)


if __name__ == "__main__":
    main()
