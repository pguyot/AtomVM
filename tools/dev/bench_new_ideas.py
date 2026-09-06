#!/usr/bin/env python3
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
"""Build and run new_ideas_probe with OTP 29 and a current AArch64 AOT build."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import statistics
import subprocess


def run(command):
    return subprocess.run([str(x) for x in command], check=True,
                          capture_output=True, text=True, timeout=180).stdout


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--build", type=Path, default=Path("build.jit.rebase"))
    parser.add_argument("--work", type=Path, required=True,
                        help="new directory for generated artifacts and raw samples")
    parser.add_argument("--erl", type=Path, default=Path("/opt/local/bin/erl"))
    parser.add_argument("--erlc", type=Path, default=Path("/opt/local/bin/erlc"))
    parser.add_argument("--runs", type=int, default=9)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    build, work = args.build.resolve(), args.work.resolve()
    work.mkdir(parents=True, exist_ok=False)
    aot = work / "aot"
    aot.mkdir()
    source = root / "tools/dev/new_ideas_probe.erl"
    run([args.erlc, "-o", work, source])
    run([args.erl, "+S", "1", "-pa", build / "libs/jit/src/beams", "-noshell",
         "-s", "jit_precompile", "-s", "init", "stop", "--", "aarch64",
         str(aot) + "/", work / "new_ideas_probe.beam"])
    avm = work / "probe.avm"
    run([build / "tools/packbeam/packbeam", "create", "--prune", "--start",
         "new_ideas_probe", avm, aot / "new_ideas_probe.beam"])
    commands = {
        "beam": [args.erl, "+S", "1", "-pa", work, "-noshell",
                 "-s", "new_ideas_probe", "start", "-s", "init", "stop"],
        "atomvm": [build / "src/AtomVM", avm, build / "libs/atomvmlib-aarch64.avm"],
    }
    samples = {engine: [] for engine in commands}
    expected = {f"receive_{n}" for n in (0, 100, 1000, 10000)} | {
        f"iterator_{n}_take_{take}" for n in (100, 1000, 10000) for take in (1, 8, n)}
    for round_no in range(-2, args.runs):
        order = ("beam", "atomvm") if round_no % 2 == 0 else ("atomvm", "beam")
        for engine in order:
            output = run(commands[engine])
            parsed = {name: int(value) for name, value in
                      re.findall(r"^(\w+): (\d+)$", output, re.MULTILINE)}
            if set(parsed) != expected:
                raise RuntimeError(f"incomplete {engine} output: {output}")
            if round_no >= 0:
                samples[engine].append(parsed)
        print(f"round {round_no + 1}/{args.runs}", flush=True)
    artifacts = [source, build / "src/AtomVM", build / "libs/atomvmlib-aarch64.avm",
                 build / "libs/jit/src/beams/jit.beam", avm]
    report = {
        "commands": {k: [str(x) for x in v] for k, v in commands.items()},
        "git_head": run(["git", "-C", root, "rev-parse", "HEAD"]).strip(),
        "otp": run([args.erl, "+S", "1", "-noshell", "-eval",
                    'io:format("~p", [{erlang:system_info(otp_release), '
                    'erlang:system_info(emu_flavor)}]), halt().']).strip(),
        "sha256": {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in artifacts},
        "samples_us": samples,
    }
    (work / "results.json").write_text(json.dumps(report, indent=2) + "\n")
    for name in samples["beam"][0]:
        medians = {k: statistics.median(s[name] for s in v) for k, v in samples.items()}
        print(f"{name:28s} BEAM {medians['beam']:9g} us  AtomVM {medians['atomvm']:9g} us")


if __name__ == "__main__":
    main()
