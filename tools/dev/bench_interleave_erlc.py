#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Interleaved erlc matrix benchmark: for each source file, round-robins all
compilers WITHIN each run (rotating the order per run so no compiler always eats
the cold-cache first slot). Compiler-vs-compiler ratios are then robust to
thermal drift. Median per (compiler, file); a file counts only if every compiler
produced a .beam. Reports per-app and overall sum + mean-per-file + speed-up vs
BEAM.

Env: OTP (default ~/otp), MATRIX_DIR (default ~/atomvm_erlc/_build/matrix),
RUNS (default 5), TIMEOUT (default 120), APPS (default the four),
PER_FILE=1 to print a per-file line (medians + vs-BEAM ratio per config)."""

import os
import glob
import time
import statistics
import subprocess
import tempfile
from pathlib import Path

OTP = Path(os.environ.get("OTP", os.path.expanduser("~/otp")))
MATRIX = Path(os.environ.get("MATRIX_DIR", os.path.expanduser("~/atomvm_erlc/_build/matrix")))
RUNS = int(os.environ.get("RUNS", "5"))
TIMEOUT = int(os.environ.get("TIMEOUT", "120"))
APPS = os.environ.get("APPS", "stdlib kernel sasl crypto").split()
BEAM_ERLC = os.environ.get("BEAM_ERLC", "/opt/local/bin/erlc")
PER_FILE = os.environ.get("PER_FILE") == "1"

COMPILERS = [
    ("BEAM", BEAM_ERLC),
    ("emu noSMP", str(MATRIX / "erlc-emu-nosmp")),
    ("emu SMP", str(MATRIX / "erlc-emu-smp")),
    ("JIT noSMP", str(MATRIX / "erlc-jit-nosmp")),
    ("JIT SMP", str(MATRIX / "erlc-jit-smp")),
]
# Restrict to a subset of configs (comma-separated labels) via CONFIGS, e.g.
# CONFIGS="BEAM,JIT noSMP,JIT SMP" to skip the slow interpreter columns. BEAM is
# always kept as the reference even if omitted.
_configs = os.environ.get("CONFIGS")
if _configs:
    _keep = {s.strip() for s in _configs.split(",")} | {"BEAM"}
    COMPILERS = [c for c in COMPILERS if c[0] in _keep]
LABELS = [c[0] for c in COMPILERS]

BASE_INC = []
for d in sorted(glob.glob(str(OTP / "lib/*/include"))):
    BASE_INC += ["-I", d]
BASE_INC += ["-I", str(OTP / "erts/include"), "-I", str(OTP / "lib")]


def compile_once(exe, src, inc):
    with tempfile.TemporaryDirectory() as out:
        cmd = [exe, "-o", out, *inc, str(src)]
        start = time.perf_counter()
        try:
            subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=TIMEOUT)
        except subprocess.TimeoutExpired:
            return float("inf"), False
        dt = time.perf_counter() - start
        return dt, bool(glob.glob(os.path.join(out, "*.beam")))


def bench_file(src, inc):
    """Interleaved: run round r touches every compiler once, rotated by r."""
    times = {lab: [] for lab in LABELS}
    ok = {lab: True for lab in LABELS}
    n = len(COMPILERS)
    for r in range(RUNS):
        order = COMPILERS[r % n:] + COMPILERS[: r % n]
        for lab, exe in order:
            dt, good = compile_once(exe, src, inc)
            times[lab].append(dt)
            ok[lab] = ok[lab] and good
    return {lab: statistics.median(times[lab]) for lab in LABELS}, all(ok.values())


def main():
    for lab, exe in COMPILERS:
        if lab != "BEAM" and not Path(exe).exists():
            raise SystemExit(f"missing {lab}: {exe}")
    print(f"# interleaved erlc, RUNS={RUNS}, apps={','.join(APPS)}")
    totals = {lab: 0.0 for lab in LABELS}
    ncommon = 0
    per_app = {}
    for app in APPS:
        srcdir = OTP / "lib" / app / "src"
        inc = BASE_INC + ["-I", str(srcdir), "-I", str(srcdir.parent / "include")]
        asum = {lab: 0.0 for lab in LABELS}
        an = 0
        for src in sorted(srcdir.glob("*.erl")):
            res, good = bench_file(src, inc)
            if not good:
                continue
            an += 1
            ncommon += 1
            for lab in LABELS:
                asum[lab] += res[lab]
                totals[lab] += res[lab]
            if PER_FILE:
                cells = "  ".join(
                    f"{lab}={res[lab]*1000:7.1f}ms({res['BEAM']/res[lab]:4.2f}x)"
                    for lab in LABELS if lab != "BEAM"
                )
                print(f"  {src.name:<28} BEAM={res['BEAM']*1000:7.1f}ms  {cells}", flush=True)
        per_app[app] = (an, asum)
        b = asum["BEAM"]
        print(f"\n## {app}  ({an} files)")
        for lab in LABELS:
            s = asum[lab]
            print(f"  {lab:<11} sum={s*1000:9.0f}ms  mean/file={s/an*1000:7.1f}ms  vs BEAM={b/s:5.2f}x")

    print(f"\n## OVERALL  ({ncommon} files)")
    b = totals["BEAM"]
    for lab in LABELS:
        s = totals[lab]
        print(f"  {lab:<11} sum={s*1000:9.0f}ms  mean/file={s/ncommon*1000:7.1f}ms  vs BEAM={b/s:5.2f}x")


if __name__ == "__main__":
    main()
