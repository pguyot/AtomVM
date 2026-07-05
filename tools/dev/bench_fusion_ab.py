#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""Interleaved A/B micro-benchmark for the JIT opcode fusions (D/E/H).

Compares two JIT-enabled AtomVM build directories -- typically the same tree
built at two commits (e.g. a fusion commit vs its parent). For each build it
AOT-precompiles tools/dev/fusion_microbench.erl with THAT build's jit, packs it,
and runs it with THAT build's AtomVM + atomvmlib. The two configs are
round-robined within each run so both see the same thermal/load state, and the
per-op median ratio is reported.

The `checks:` line printed by the microbench must be identical between the two
builds; the harness aborts if it is not (a correctness regression).

Usage:
  BASELINE_BUILD=/path/to/build.baseline \\
  FUSION_BUILD=/path/to/build.fusion \\
  TARGET=aarch64 \\
  tools/dev/bench_fusion_ab.py [N_RUNS]

Env vars:
  BASELINE_BUILD  build dir of the "before" config (required)
  FUSION_BUILD    build dir of the "after" config  (required)
  TARGET          JIT target (default: aarch64)
  ERLC            erlc to compile the microbench (default: erlc)
"""

import os
import re
import statistics
import subprocess
import sys
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
SRC = HERE / "fusion_microbench.erl"
MODULE = "fusion_microbench"
TARGET = os.environ.get("TARGET", "aarch64")
ERLC = os.environ.get("ERLC", "erlc")
N = int(sys.argv[1]) if len(sys.argv) > 1 else 25
WARMUP = 2
LINE = re.compile(r"^([a-z_]+):\s*(\d+)\s*$")


def need(var):
    v = os.environ.get(var)
    if not v:
        sys.exit(f"error: {var} must be set (a JIT-enabled build directory)")
    return Path(v)


def build_avm(build, workdir):
    """AOT-precompile the microbench with `build`'s jit; return the .avm path."""
    jit_beams = build / "libs" / "jit" / "src" / "beams"
    packbeam = build / "tools" / "packbeam" / "packbeam"
    for f in (jit_beams / "jit_precompile.beam", packbeam):
        if not f.exists():
            sys.exit(f"error: {f} not found -- is {build} a JIT build?")
    # 1. compile the microbench to plain beam
    subprocess.run([ERLC, "+no_line_info", "-o", str(workdir), str(SRC)], check=True)
    # 2. jit_precompile it for TARGET
    aot = workdir / build.name
    aot.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        ["erl", "-pa", str(jit_beams), "-noshell", "-s", "jit_precompile",
         "-s", "init", "stop", "--", TARGET, str(aot) + "/", str(workdir / (MODULE + ".beam"))],
        check=True,
    )
    # 3. packbeam
    avm = workdir / f"{build.name}.avm"
    subprocess.run(
        [str(packbeam), "create", "--prune", "--start", MODULE, str(avm),
         str(aot / (MODULE + ".beam"))],
        check=True, stdout=subprocess.DEVNULL,
    )
    return avm


def run(build, avm):
    vm = build / "src" / "AtomVM"
    lib = build / "libs" / f"atomvmlib-{TARGET}.avm"
    out = subprocess.run([str(vm), str(avm), str(lib)], capture_output=True, text=True).stdout
    times, checks = {}, None
    for ln in out.splitlines():
        if ln.startswith("checks:"):
            checks = ln
        m = LINE.match(ln)
        if m:
            times[m.group(1)] = int(m.group(2))
    return times, checks


def main():
    baseline, fusion = need("BASELINE_BUILD"), need("FUSION_BUILD")
    with tempfile.TemporaryDirectory() as td:
        work = Path(td)
        cfgs = [
            ("baseline", baseline, build_avm(baseline, work)),
            ("fusion", fusion, build_avm(fusion, work)),
        ]
        # correctness guard
        checks = {}
        for lab, build, avm in cfgs:
            _, c = run(build, avm)
            checks[lab] = c
        if checks["baseline"] != checks["fusion"]:
            sys.exit(f"CORRECTNESS MISMATCH:\n  baseline {checks['baseline']}\n  fusion   {checks['fusion']}")
        print(f"# checks match: {checks['baseline']}")

        for _ in range(WARMUP):
            for _, build, avm in cfgs:
                run(build, avm)
        samples = {lab: {} for lab, _, _ in cfgs}
        for _ in range(N):
            for lab, build, avm in cfgs:
                t, _ = run(build, avm)
                for op, us in t.items():
                    samples[lab].setdefault(op, []).append(us)

    med = {lab: {op: statistics.median(v) for op, v in d.items()} for lab, d in samples.items()}
    print(f"# interleaved A/B, TARGET={TARGET}, N={N} (median us; speedup = baseline/fusion)")
    print(f"{'op':<16}{'baseline':>11}{'fusion':>11}{'speedup':>9}")
    for op in ("list_match", "record_access", "bin_decode"):
        if op in med["baseline"] and op in med["fusion"]:
            b, f = med["baseline"][op], med["fusion"][op]
            print(f"{op:<16}{b:>11.0f}{f:>11.0f}{b / f:>8.3f}x")


if __name__ == "__main__":
    main()
