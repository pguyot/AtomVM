#!/usr/bin/env python3
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
"""A/B driver: two AtomVM engine snapshots on estone or the benchmark app.

Each engine is a directory holding AtomVM, atomvmlib-aarch64.avm and the
workload avm.  Rounds alternate the order so drift is shared.

    tools/dev/bench_ab_atomvm.py estone --a build.ab/base --b build.ab/cand \
        --runs 21 --warmup 2
"""
import argparse, json, random, re, statistics, subprocess, sys, time
from pathlib import Path

ESTONE_TOTAL_RE = re.compile(r"^\*\*\*\* ESTONES = (\d+) \*\*\*\*$", re.MULTILINE)
ESTONE_TIME_RE = re.compile(r"^\*\*\*\* Total time ([0-9.eE+-]+) seconds \*\*\*\*$", re.MULTILINE)
ESTONE_LINE_RE = re.compile(r"^(.+?)\s+(\d+)ms\s+(\d+) estones\s+\d+%\s+\d+ loops$", re.MULTILINE)
APP_LINE_RE = re.compile(r"^(?P<label>\S.*?):\s*(?P<usec>\d+)\s*$")


def cmd_for(engine, workload):
    d = Path(engine)
    avm = "estone-aarch64.avm" if workload == "estone" else "benchmark-aarch64.avm"
    return [str(d / "AtomVM"), str(d / avm), str(d / "atomvmlib-aarch64.avm")]


def run_once(cmd, workload):
    start = time.perf_counter()
    proc = subprocess.run(cmd, capture_output=True, text=True)
    wall = time.perf_counter() - start
    out = proc.stdout
    if workload == "estone":
        total = ESTONE_TOTAL_RE.search(out)
        seconds = ESTONE_TIME_RE.search(out)
        if not total or not seconds:
            raise RuntimeError(f"bad estone output ({proc.returncode}):\n{out}\n{proc.stderr}")
        comps = {n.strip(): (int(ms), int(es)) for n, ms, es in ESTONE_LINE_RE.findall(out)}
        return {"wall": wall, "estones": int(total.group(1)),
                "seconds": float(seconds.group(1)),
                "component_ms": {k: v[0] for k, v in comps.items()},
                "component_estones": {k: v[1] for k, v in comps.items()}}
    if proc.returncode != 0:
        raise RuntimeError(f"failed ({proc.returncode}):\n{out}\n{proc.stderr}")
    timings = {}
    for line in out.splitlines():
        m = APP_LINE_RE.match(line)
        if m:
            timings[m.group("label")] = int(m.group("usec"))
    if not timings:
        raise RuntimeError(f"bad app output:\n{out}")
    return {"wall": wall, "tests_usec": timings}


def boot_ci(a, b, seed=0, iters=20000):
    if len(a) != len(b) or not a:
        return None
    rng = random.Random(seed)
    r = []
    for _ in range(iters):
        idx = [rng.randrange(len(a)) for _ in a]
        r.append(statistics.median([a[i] for i in idx]) / statistics.median([b[i] for i in idx]))
    r.sort()
    lo = r[int(0.025 * (len(r) - 1))]
    hi = r[int(0.975 * (len(r) - 1))]
    return [lo, hi]


def main():
    p = argparse.ArgumentParser()
    p.add_argument("workload", choices=["estone", "app"])
    p.add_argument("--a", required=True, help="baseline engine dir")
    p.add_argument("--b", required=True, help="candidate engine dir")
    p.add_argument("--runs", type=int, default=21)
    p.add_argument("--warmup", type=int, default=2)
    p.add_argument("--output", type=Path)
    args = p.parse_args()

    cmds = {"A": cmd_for(args.a, args.workload), "B": cmd_for(args.b, args.workload)}
    for label, c in cmds.items():
        for path in c:
            if not Path(path).exists():
                raise SystemExit(f"missing {label}: {path}")
    samples = {"A": [], "B": []}
    for w in range(args.warmup):
        for label in ("A", "B"):
            run_once(cmds[label], args.workload)
        print(f"warmup {w+1}/{args.warmup}", file=sys.stderr, flush=True)
    for n in range(args.runs):
        order = ("A", "B") if n % 2 == 0 else ("B", "A")
        for label in order:
            samples[label].append(run_once(cmds[label], args.workload))
        print(f"round {n+1}/{args.runs}", file=sys.stderr, flush=True)

    result = {"workload": args.workload, "a": args.a, "b": args.b,
              "runs": args.runs, "samples": samples}
    if args.workload == "estone":
        tot = {k: [s["estones"] for s in samples[k]] for k in samples}
        ma, mb = statistics.median(tot["A"]), statistics.median(tot["B"])
        ci = boot_ci(tot["B"], tot["A"])
        print(f"A (base) median ESTONES: {ma:,.0f}")
        print(f"B (cand) median ESTONES: {mb:,.0f}")
        print(f"B/A: {mb/ma:.4f}x  (bootstrap 95% CI {ci[0]:.4f}-{ci[1]:.4f})")
        sec = {k: [s["seconds"] for s in samples[k]] for k in samples}
        sa, sb = statistics.median(sec["A"]), statistics.median(sec["B"])
        sci = boot_ci(sec["A"], sec["B"])
        print(f"total measured time A={sa*1000:.1f}ms B={sb*1000:.1f}ms "
              f"A/B={sa/sb:.4f}x (bootstrap 95% CI {sci[0]:.4f}-{sci[1]:.4f})")
        names = sorted(set(samples["A"][0]["component_estones"]))
        print(f"\n{'component':<34}{'A est':>10}{'B est':>10}{'B/A':>8}")
        for name in names:
            ca = statistics.median([s["component_estones"][name] for s in samples["A"]])
            cb = statistics.median([s["component_estones"][name] for s in samples["B"]])
            print(f"{name:<34}{ca:>10,.0f}{cb:>10,.0f}{cb/ca:>8.3f}")
        result["summary"] = {"a_median": ma, "b_median": mb, "ratio": mb/ma, "ci": ci}
    else:
        tests = sorted(set(samples["A"][0]["tests_usec"]) & set(samples["B"][0]["tests_usec"]))
        tests = [t for t in tests if "[schedulers=1]" not in t]
        med = {k: {t: statistics.median([s["tests_usec"][t] for s in samples[k]]) for t in tests}
               for k in samples}
        print(f"\n{'test':<30}{'A us':>11}{'B us':>11}{'A/B':>8}")
        for t in tests:
            print(f"{t:<30}{med['A'][t]:>11,.0f}{med['B'][t]:>11,.0f}{med['A'][t]/med['B'][t]:>8.3f}")
        agg = {k: sum(med[k].values()) for k in med}
        totals = {k: [sum(s["tests_usec"][t] for t in tests) for s in samples[k]] for k in samples}
        ci = boot_ci(totals["A"], totals["B"])
        print(f"\naggregate A={agg['A']/1000:.1f}ms B={agg['B']/1000:.1f}ms "
              f"A/B={agg['A']/agg['B']:.4f}x (bootstrap 95% CI {ci[0]:.4f}-{ci[1]:.4f})")
        result["summary"] = {"test_median_usec": med, "aggregate_usec": agg,
                             "speedup_a_over_b": agg["A"] / agg["B"], "ci": ci}
    if args.output:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
