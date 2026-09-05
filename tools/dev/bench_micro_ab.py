#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""A/B a microbenchmark module across two JIT snapshots.

Each snapshot directory holds AtomVM, atomvmlib-aarch64.avm and jitbeams/.
The module is precompiled once per snapshot, then runs interleaved.

    tools/dev/bench_micro_ab.py micro/shift_bench.erl \
        --a build.ab/base --b build.ab/cand --runs 11
"""
import argparse, re, statistics, subprocess, sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent.parent
LINE = re.compile(r"^(\w[\w ]*?):\s*(-?\d+)")


def sh(cmd, **kw):
    return subprocess.run([str(x) for x in cmd], check=True,
                          capture_output=True, text=True, **kw)


def build(src, engine, work):
    mod = src.stem
    out = work / f"aot-{engine.name}"
    if out.exists():
        subprocess.run(["rm", "-rf", str(out)], check=True)
    out.mkdir(parents=True)
    sh(["erl", "-pa", engine / "jitbeams", "-noshell", "-s", "jit_precompile",
        "-s", "init", "stop", "--", "aarch64", str(out) + "/", work / f"{mod}.beam"])
    avm = work / f"{mod}-{engine.name}.avm"
    sh([ROOT / "build.jit.rebase/tools/packbeam/packbeam", "create", "--prune",
        "--start", mod, avm, out / f"{mod}.beam"])
    return avm, (out / f"{mod}.beam").stat().st_size


def run(engine, avm):
    proc = subprocess.run([str(engine / "AtomVM"), str(avm),
                           str(engine / "atomvmlib-aarch64.avm")],
                          capture_output=True, text=True)
    res = {}
    for line in proc.stdout.splitlines():
        m = LINE.match(line)
        if m:
            res[m.group(1).strip()] = int(m.group(2))
    if not res:
        raise RuntimeError(f"no output ({proc.returncode}):\n{proc.stdout}\n{proc.stderr}")
    return res


def main():
    p = argparse.ArgumentParser()
    p.add_argument("source", type=Path)
    p.add_argument("--a", type=Path, required=True)
    p.add_argument("--b", type=Path, required=True)
    p.add_argument("--runs", type=int, default=11)
    p.add_argument("--warmup", type=int, default=2)
    args = p.parse_args()

    src = args.source.resolve()
    work = src.parent
    sh(["erlc", src.name], cwd=work)
    engines = {"A": args.a.resolve(), "B": args.b.resolve()}
    avms, sizes = {}, {}
    for label, engine in engines.items():
        avms[label], sizes[label] = build(src, engine, work)
    print(f"native beam size  A={sizes['A']}  B={sizes['B']}  "
          f"delta={sizes['B'] - sizes['A']:+d} bytes")

    samples = {"A": [], "B": []}
    for _ in range(args.warmup):
        for label in ("A", "B"):
            run(engines[label], avms[label])
    for n in range(args.runs):
        for label in (("A", "B") if n % 2 == 0 else ("B", "A")):
            samples[label].append(run(engines[label], avms[label]))
        print(f"round {n+1}/{args.runs}", file=sys.stderr, flush=True)

    names = list(samples["A"][0])
    print(f"\n{'case':<16}{'A us':>10}{'B us':>10}{'A/B':>9}")
    for name in names:
        a = statistics.median([s[name] for s in samples["A"]])
        b = statistics.median([s[name] for s in samples["B"]])
        print(f"{name:<16}{a:>10,.0f}{b:>10,.0f}{a / b:>9.3f}")


if __name__ == "__main__":
    main()
