#!/usr/bin/env python3
"""Publication-oriented BEAM/AtomVM A/B benchmark driver.

Runs the same work on OTP's BEAM JIT and an aarch64-AOT AtomVM build.  The
execution order rotates within every round, all raw samples are retained as
JSON, and compiler output is checked rather than trusting the exit status.

Examples (defaults match the local repository layout):

    tools/dev/bench_publication.py erlc --runs-per-file 7 --runs-batch 15 \
        --output build.jit.rebase/publication/erlc.json
    tools/dev/bench_publication.py estone --runs 31 --warmup 3 \
        --output build.jit.rebase/publication/estone.json
    tools/dev/bench_publication.py app --runs 21 --warmup 3 \
        --output build.jit.rebase/publication/app.json
"""

import argparse
import glob
import json
import os
import platform
import random
import re
import statistics
import subprocess
import sys
import tempfile
import time
from datetime import datetime
from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent.parent
DEFAULT_BUILD = ROOT / "build.jit.rebase"
DEFAULT_BENCHMARK = ROOT.parent / "atomvm_benchmark"
DEFAULT_OTP = ROOT.parent / "otp"
DEFAULT_BEAM_ERL = Path("/opt/local/bin/erl")
DEFAULT_BEAM_ERLC = Path("/opt/local/bin/erlc")
LABELS = ("BEAM", "AtomVM")


def run_text(cmd, *, cwd=None, timeout=60):
    return subprocess.run(
        [str(x) for x in cmd], cwd=cwd, capture_output=True, text=True,
        timeout=timeout, check=True
    ).stdout.strip()


def metadata(args):
    def optional(cmd):
        try:
            return run_text(cmd)
        except (OSError, subprocess.SubprocessError):
            return None

    return {
        "timestamp": datetime.now().astimezone().isoformat(),
        "argv": sys.argv,
        "host": {
            "platform": platform.platform(),
            "machine": platform.machine(),
            "macos": platform.mac_ver()[0],
            "hardware": optional(["system_profiler", "SPHardwareDataType", "-detailLevel", "mini"]),
            "power": optional(["pmset", "-g", "batt"]),
        },
        "versions": {
            "atomvm_git": optional(["git", "-C", str(ROOT), "rev-parse", "HEAD"]),
            "atomvm_describe": optional(["git", "-C", str(ROOT), "describe", "--always", "--dirty"]),
            "otp_source_git": optional(["git", "-C", str(getattr(args, "otp", DEFAULT_OTP)), "rev-parse", "HEAD"]),
            "beam": optional([
                str(getattr(args, "beam_erl", DEFAULT_BEAM_ERL)), "-noshell", "-eval",
                'io:format("OTP=~s emu=~p schedulers=~p", '
                '[erlang:system_info(otp_release), erlang:system_info(emu_flavor), '
                'erlang:system_info(schedulers)]), halt().'
            ]),
        },
    }


def write_json(path, value):
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n")
    temporary.replace(path)


def median(values):
    return statistics.median(values)


def quantile(values, q):
    ordered = sorted(values)
    if len(ordered) == 1:
        return ordered[0]
    position = (len(ordered) - 1) * q
    lower = int(position)
    upper = min(lower + 1, len(ordered) - 1)
    fraction = position - lower
    return ordered[lower] * (1 - fraction) + ordered[upper] * fraction


def paired_ratio_ci(left, right, seed=0, iterations=20000):
    """Percentile bootstrap CI for median(left)/median(right), paired by round."""
    if len(left) != len(right) or not left:
        return None
    rng = random.Random(seed)
    ratios = []
    for _ in range(iterations):
        indexes = [rng.randrange(len(left)) for _ in left]
        ratios.append(
            median([left[i] for i in indexes]) /
            median([right[i] for i in indexes])
        )
    return [quantile(ratios, 0.025), quantile(ratios, 0.975)]


def alternating_order(round_number, unit_number=0):
    return LABELS if (round_number + unit_number) % 2 == 0 else tuple(reversed(LABELS))


def check_paths(paths):
    for path in paths:
        if not Path(path).exists():
            raise SystemExit(f"missing required artifact: {path}")


def add_common_paths(parser):
    parser.add_argument("--build", type=Path, default=DEFAULT_BUILD)
    parser.add_argument("--atomvm", type=Path,
                        help="AtomVM executable (defaults to BUILD/src/AtomVM)")
    parser.add_argument("--benchmark", type=Path, default=DEFAULT_BENCHMARK)
    parser.add_argument("--beam-erl", type=Path, default=DEFAULT_BEAM_ERL)
    parser.add_argument("--target", default="aarch64",
                        help="JIT target of the AOT artifacts (avm file names)")
    parser.add_argument("--beam-schedulers", type=int,
                        help="override BEAM scheduler count with +S")
    parser.add_argument("--atomvm-schedulers", type=int,
                        help="override AtomVM scheduler count with AVM_SCHEDULERS")
    parser.add_argument("--output", type=Path, required=True)


def estone_command(args, label):
    ebin = args.benchmark / "_build/default/lib/benchmark/ebin"
    if label == "BEAM":
        scheduler_args = ["+S", str(args.beam_schedulers)] if args.beam_schedulers else []
        return [args.beam_erl, *scheduler_args, "-pa", ebin, "-noshell", "-s", "estone", "start", "-s", "init", "stop"]
    command = [
        args.atomvm or args.build / "src/AtomVM",
        args.build / f"benchmark-aot-{args.target}/estone-{args.target}.avm",
        args.build / f"libs/atomvmlib-{args.target}.avm",
    ]
    return ([Path("/usr/bin/env"), f"AVM_SCHEDULERS={args.atomvm_schedulers}", *command]
            if args.atomvm_schedulers else command)


ESTONE_TOTAL_RE = re.compile(r"^\*\*\*\* ESTONES = (\d+) \*\*\*\*$", re.MULTILINE)
ESTONE_TIME_RE = re.compile(r"^\*\*\*\* Total time ([0-9.eE+-]+) seconds \*\*\*\*$", re.MULTILINE)
ESTONE_COMPONENT_RE = re.compile(r"^([^\n:]+): (\d+) estones$", re.MULTILINE)


def run_estone_once(cmd):
    start = time.perf_counter()
    proc = subprocess.run([str(x) for x in cmd], capture_output=True, text=True)
    wall = time.perf_counter() - start
    total_match = ESTONE_TOTAL_RE.search(proc.stdout)
    time_match = ESTONE_TIME_RE.search(proc.stdout)
    if not total_match or not time_match:
        raise RuntimeError(
            f"could not parse ESTONE output (status {proc.returncode}):\n"
            f"{proc.stdout}\n{proc.stderr}"
        )
    return {
        # AtomVM maps estone:start/0's non-integer result tuple to status 1;
        # successful, fully parsed benchmark output is the correctness check.
        "process_status": proc.returncode,
        "estones": int(total_match.group(1)),
        "reported_seconds": float(time_match.group(1)),
        "wall_seconds": wall,
        "components": {name: int(value) for name, value in ESTONE_COMPONENT_RE.findall(proc.stdout)},
    }


def command_estone(args):
    commands = {label: estone_command(args, label) for label in LABELS}
    check_paths([commands[label][0] for label in LABELS])
    samples = {label: [] for label in LABELS}
    for warmup in range(args.warmup):
        for label in alternating_order(warmup):
            run_estone_once(commands[label])
        print(f"estone warmup {warmup + 1}/{args.warmup}", file=sys.stderr, flush=True)
    for round_number in range(args.runs):
        for label in alternating_order(round_number):
            samples[label].append(run_estone_once(commands[label]))
        print(f"estone round {round_number + 1}/{args.runs}", file=sys.stderr, flush=True)

    totals = {label: [sample["estones"] for sample in samples[label]] for label in LABELS}
    ratio = median(totals["AtomVM"]) / median(totals["BEAM"])
    ci = paired_ratio_ci(totals["AtomVM"], totals["BEAM"])
    result = {
        "kind": "estone",
        "method": {
            "runs": args.runs,
            "warmup": args.warmup,
            "interleaved": True,
            "rotating_order": True,
            "metric": "median total ESTONES; higher is faster",
        },
        "metadata": metadata(args),
        "commands": {label: [str(x) for x in cmd] for label, cmd in commands.items()},
        "samples": samples,
        "summary": {
            label: {
                "median_estones": median(totals[label]),
                "min_estones": min(totals[label]),
                "max_estones": max(totals[label]),
                "median_wall_seconds": median([s["wall_seconds"] for s in samples[label]]),
            } for label in LABELS
        },
        "atomvm_vs_beam": ratio,
        "atomvm_vs_beam_bootstrap_95pct": ci,
    }
    write_json(args.output, result)
    print(f"BEAM median:   {median(totals['BEAM']):,.0f} ESTONES")
    print(f"AtomVM median: {median(totals['AtomVM']):,.0f} ESTONES")
    print(f"AtomVM/BEAM:   {ratio:.3f}x (paired bootstrap 95% CI {ci[0]:.3f}–{ci[1]:.3f})")


APP_LINE_RE = re.compile(r"^(?P<label>\S.*?):\s*(?P<usec>\d+)\s*$")


def app_command(args, label):
    ebin = args.benchmark / "_build/default/lib/benchmark/ebin"
    if label == "BEAM":
        scheduler_args = ["+S", str(args.beam_schedulers)] if args.beam_schedulers else []
        return [args.beam_erl, *scheduler_args, "-pa", ebin, "-noshell", "-s", "benchmark", "start", "-s", "init", "stop"]
    command = [
        args.atomvm or args.build / "src/AtomVM",
        args.build / f"benchmark-aot-{args.target}/benchmark-{args.target}.avm",
        args.build / f"libs/atomvmlib-{args.target}.avm",
    ]
    return ([Path("/usr/bin/env"), f"AVM_SCHEDULERS={args.atomvm_schedulers}", *command]
            if args.atomvm_schedulers else command)


def run_app_once(cmd):
    start = time.perf_counter()
    proc = subprocess.run([str(x) for x in cmd], capture_output=True, text=True)
    wall = time.perf_counter() - start
    if proc.returncode != 0:
        raise RuntimeError(f"failed ({proc.returncode}): {' '.join(map(str, cmd))}\n{proc.stderr}")
    timings = {}
    for line in proc.stdout.splitlines():
        match = APP_LINE_RE.match(line)
        if match:
            timings[match.group("label")] = int(match.group("usec"))
    if not timings:
        raise RuntimeError(f"could not parse benchmark output:\n{proc.stdout}")
    return {"wall_seconds": wall, "tests_usec": timings}


def command_app(args):
    commands = {label: app_command(args, label) for label in LABELS}
    check_paths([commands[label][0] for label in LABELS])
    samples = {label: [] for label in LABELS}
    for warmup in range(args.warmup):
        for label in alternating_order(warmup):
            run_app_once(commands[label])
        print(f"app warmup {warmup + 1}/{args.warmup}", file=sys.stderr, flush=True)
    for round_number in range(args.runs):
        for label in alternating_order(round_number):
            samples[label].append(run_app_once(commands[label]))
        print(f"app round {round_number + 1}/{args.runs}", file=sys.stderr, flush=True)

    tests = sorted(set.intersection(*[
        {name for sample in samples[label] for name in sample["tests_usec"] if "[schedulers=1]" not in name}
        for label in LABELS
    ]))
    medians = {
        label: {
            test: median([sample["tests_usec"][test] for sample in samples[label]])
            for test in tests
        } for label in LABELS
    }
    aggregate = {label: sum(medians[label].values()) for label in LABELS}
    result = {
        "kind": "benchmark_app",
        "method": {"runs": args.runs, "warmup": args.warmup, "interleaved": True, "rotating_order": True},
        "metadata": metadata(args),
        "commands": {label: [str(x) for x in cmd] for label, cmd in commands.items()},
        "samples": samples,
        "summary": {
            "test_median_usec": medians,
            "aggregate_usec": aggregate,
            "atomvm_vs_beam": aggregate["BEAM"] / aggregate["AtomVM"],
            "median_wall_seconds": {
                label: median([sample["wall_seconds"] for sample in samples[label]]) for label in LABELS
            },
        },
    }
    write_json(args.output, result)
    for test in tests:
        print(f"{test:<28} BEAM={medians['BEAM'][test]:>9,.0f}us  "
              f"AtomVM={medians['AtomVM'][test]:>9,.0f}us  "
              f"ratio={medians['BEAM'][test] / medians['AtomVM'][test]:.2f}x")
    print(f"aggregate: BEAM={aggregate['BEAM']/1000:.1f}ms "
          f"AtomVM={aggregate['AtomVM']/1000:.1f}ms "
          f"ratio={aggregate['BEAM']/aggregate['AtomVM']:.3f}x")


APP_MACROS = {"compiler": ['-DCOMPILER_VSN="0"'], "dialyzer": ['-DVSN="0"']}


def base_includes(otp):
    includes = []
    for directory in sorted(glob.glob(str(otp / "lib/*/include"))):
        includes += ["-I", directory]
    includes += ["-I", str(otp / "erts/include"), "-I", str(otp / "lib")]
    for directory in sorted(glob.glob(str(otp / "lib/*/src"))):
        includes += ["-I", directory]
    return includes


def compile_once(executable, sources, includes, timeout):
    with tempfile.TemporaryDirectory(prefix="atomvm-pub-") as output:
        command = [str(executable), "-o", output, *includes, *map(str, sources)]
        start = time.perf_counter()
        try:
            # Drain a pipe so communicate wakes on EOF; wait(timeout) with
            # DEVNULL polls in up to 50 ms steps on POSIX, quantizing timings.
            proc = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=timeout)
        except subprocess.TimeoutExpired:
            return float("inf"), set(), "timeout"
        elapsed = time.perf_counter() - start
        produced = {Path(path).stem for path in glob.glob(os.path.join(output, "*.beam"))}
        return elapsed, produced, proc.returncode


def discover(executable, sources, includes, timeout):
    _, produced, _ = compile_once(executable, sources, includes, timeout)
    if len(produced) == len(sources):
        return produced
    for source in sources:
        if source.stem not in produced:
            _, one, _ = compile_once(executable, [source], includes, timeout)
            produced |= one
    return produced


def summarize_erlc_mode(mode_samples, support, runs):
    per_app = {}
    all_file_rows = []
    round_totals = {label: [0.0] * runs for label in LABELS}
    for app, app_samples in mode_samples.items():
        if mode_samples[app]["unit"] == "batch":
            med = {label: median(app_samples["samples"][label]) for label in LABELS}
            per_app[app] = {
                "files": len(support[app]["common"]),
                "seconds": med,
                "beam_over_atomvm": med["BEAM"] / med["AtomVM"],
            }
            for label in LABELS:
                for r, value in enumerate(app_samples["samples"][label]):
                    round_totals[label][r] += value
        else:
            sums = {label: 0.0 for label in LABELS}
            for filename, file_samples in app_samples["files"].items():
                med = {label: median(file_samples[label]) for label in LABELS}
                all_file_rows.append((app, filename, med["BEAM"], med["AtomVM"]))
                for label in LABELS:
                    sums[label] += med[label]
                    for r, value in enumerate(file_samples[label]):
                        round_totals[label][r] += value
            per_app[app] = {
                "files": len(app_samples["files"]),
                "sum_of_per_file_medians_seconds": sums,
                "beam_over_atomvm": sums["BEAM"] / sums["AtomVM"],
            }

    if mode_samples and next(iter(mode_samples.values()))["unit"] == "batch":
        totals = {label: sum(row["seconds"][label] for row in per_app.values()) for label in LABELS}
    else:
        totals = {
            label: sum(
                row["sum_of_per_file_medians_seconds"][label] for row in per_app.values()
            ) for label in LABELS
        }
    ratios_by_round = [
        round_totals["BEAM"][r] / round_totals["AtomVM"][r] for r in range(runs)
    ]
    summary = {
        "per_app": per_app,
        "totals_seconds": totals,
        "beam_over_atomvm": totals["BEAM"] / totals["AtomVM"],
        "aggregate_ratio_by_round": ratios_by_round,
        "aggregate_ratio_round_min_max": [min(ratios_by_round), max(ratios_by_round)],
    }
    if all_file_rows:
        ratios = [beam / atomvm for _, _, beam, atomvm in all_file_rows]
        summary["files_atomvm_faster"] = sum(ratio > 1.0 for ratio in ratios)
        summary["files_total"] = len(ratios)
        summary["median_per_file_ratio"] = median(ratios)
        summary["slowest_atomvm_files"] = [
            {"app": app, "file": name, "beam_over_atomvm": beam / atomvm,
             "beam_seconds": beam, "atomvm_seconds": atomvm}
            for app, name, beam, atomvm in sorted(all_file_rows, key=lambda row: row[2] / row[3])[:20]
        ]
    return summary


def command_erlc(args):
    executables = {"BEAM": args.beam_erlc, "AtomVM": args.atomvm_erlc}
    check_paths(executables.values())
    common_includes = base_includes(args.otp)
    support = {}
    app_sources = {}
    app_includes = {}

    for app in args.apps:
        source_directory = args.otp / "lib" / app / "src"
        sources = sorted(source_directory.glob("*.erl"))
        if not sources:
            raise SystemExit(f"no Erlang sources for application {app}: {source_directory}")
        includes = (["-I", str(source_directory), "-I", str(source_directory.parent / "include")]
                    + common_includes + APP_MACROS.get(app, []))
        supported = {}
        for label in LABELS:
            supported[label] = discover(executables[label], sources, includes, args.timeout)
        common = sorted(supported["BEAM"] & supported["AtomVM"])
        support[app] = {
            "source_count": len(sources),
            "beam_count": len(supported["BEAM"]),
            "atomvm_count": len(supported["AtomVM"]),
            "common": common,
            "atomvm_failures": sorted(s.stem for s in sources if s.stem not in supported["AtomVM"]),
        }
        app_sources[app] = [source for source in sources if source.stem in common]
        app_includes[app] = includes
        print(f"support {app}: {len(common)}/{len(sources)} common", file=sys.stderr, flush=True)

    samples = {}
    if args.runs_batch:
        batch = {}
        for app_number, app in enumerate(args.apps):
            values = {label: [] for label in LABELS}
            expected = {source.stem for source in app_sources[app]}
            for r in range(args.runs_batch):
                for label in alternating_order(r, app_number):
                    elapsed, produced, status = compile_once(
                        executables[label], app_sources[app], app_includes[app], args.timeout
                    )
                    if produced != expected:
                        raise RuntimeError(
                            f"timed batch failed: app={app} label={label} status={status} "
                            f"produced={len(produced)}/{len(expected)}"
                        )
                    values[label].append(elapsed)
                print(f"batch {app} round {r + 1}/{args.runs_batch}", file=sys.stderr, flush=True)
            batch[app] = {"unit": "batch", "samples": values}
        samples["batch"] = batch

    if args.runs_per_file:
        per_file = {}
        global_file_number = 0
        for app in args.apps:
            files = {}
            for source in app_sources[app]:
                values = {label: [] for label in LABELS}
                for r in range(args.runs_per_file):
                    for label in alternating_order(r, global_file_number):
                        elapsed, produced, status = compile_once(
                            executables[label], [source], app_includes[app], args.timeout
                        )
                        if source.stem not in produced:
                            raise RuntimeError(
                                f"timed file failed: {app}/{source.name} label={label} status={status}"
                            )
                        values[label].append(elapsed)
                files[source.name] = values
                global_file_number += 1
                print(f"per-file {app}: {len(files)}/{len(app_sources[app])}", file=sys.stderr, flush=True)
            per_file[app] = {"unit": "file", "files": files}
        samples["per_file"] = per_file

    summaries = {}
    if "batch" in samples:
        summaries["batch"] = summarize_erlc_mode(samples["batch"], support, args.runs_batch)
    if "per_file" in samples:
        summaries["per_file"] = summarize_erlc_mode(samples["per_file"], support, args.runs_per_file)
    result = {
        "kind": "erlc",
        "method": {
            "apps": args.apps,
            "runs_per_file": args.runs_per_file,
            "runs_batch": args.runs_batch,
            "interleaved": True,
            "rotating_order": True,
            "warm_cache": "support discovery precedes timed runs",
            "timer": "perf_counter around compiler process; startup included",
            "fairness": "only sources producing a BEAM file on both compilers are timed",
        },
        "metadata": metadata(args),
        "executables": {label: str(path) for label, path in executables.items()},
        "support": support,
        "samples": samples,
        "summary": summaries,
    }
    write_json(args.output, result)
    for mode, summary in summaries.items():
        print(f"\n{mode}:")
        for app, row in summary["per_app"].items():
            seconds = row.get("seconds", row.get("sum_of_per_file_medians_seconds"))
            print(f"  {app:<10} {row['files']:>3} files  BEAM={seconds['BEAM']:7.3f}s  "
                  f"AtomVM={seconds['AtomVM']:7.3f}s  ratio={row['beam_over_atomvm']:.3f}x")
        totals = summary["totals_seconds"]
        print(f"  TOTAL          BEAM={totals['BEAM']:7.3f}s  AtomVM={totals['AtomVM']:7.3f}s  "
              f"ratio={summary['beam_over_atomvm']:.3f}x")
        print(f"  round-ratio range: {summary['aggregate_ratio_round_min_max'][0]:.3f}–"
              f"{summary['aggregate_ratio_round_min_max'][1]:.3f}x")


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command", required=True)

    estone = subparsers.add_parser("estone")
    add_common_paths(estone)
    estone.add_argument("--runs", type=int, default=31)
    estone.add_argument("--warmup", type=int, default=3)
    estone.set_defaults(function=command_estone)

    app = subparsers.add_parser("app")
    add_common_paths(app)
    app.add_argument("--runs", type=int, default=21)
    app.add_argument("--warmup", type=int, default=3)
    app.set_defaults(function=command_app)

    erlc = subparsers.add_parser("erlc")
    erlc.add_argument("--otp", type=Path, default=DEFAULT_OTP)
    erlc.add_argument("--beam-erl", type=Path, default=DEFAULT_BEAM_ERL)
    erlc.add_argument("--beam-erlc", type=Path, default=DEFAULT_BEAM_ERLC)
    erlc.add_argument("--atomvm-erlc", type=Path,
                      default=DEFAULT_BUILD / "publication/erlc-atomvm-aot")
    erlc.add_argument("--apps", nargs="+", default=["compiler", "stdlib", "kernel", "sasl", "crypto"])
    erlc.add_argument("--runs-per-file", type=int, default=7)
    erlc.add_argument("--runs-batch", type=int, default=15)
    erlc.add_argument("--timeout", type=int, default=900)
    erlc.add_argument("--output", type=Path, required=True)
    erlc.set_defaults(function=command_erlc)
    return parser.parse_args()


def main():
    args = parse_args()
    args.function(args)


if __name__ == "__main__":
    main()
