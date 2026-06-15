<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# AtomVM benchmark matrix (AOT/JIT vs emulated, SMP vs non-SMP, vs BEAM)

Reproducible methodology for comparing AtomVM across the execution-mode /
SMP build matrix and against the official Erlang VM (BEAM), on two workloads:

1. **benchmark app** — the `atomvm_benchmark` micro-benchmark suite (compute,
   messaging, bignum, crypto, sudoku, …).
2. **erlc** — the `atomvm_erlc` standalone compiler front-end, compiling real
   OTP-29 application source (`stdlib`, `kernel`, `sasl`, `crypto`).

All AtomVM builds use the **same optimisation level (`-O2`, CMake
`RelWithDebInfo`)** so the only variables are JIT-vs-interpreter and SMP-vs-not.
(`-O3` / CMake `Release` is also available — see *Optimisation level* below —
but a single level is used across the matrix for a fair comparison.)

## The matrix

| Config       | Execution            | JIT (`AVM_DISABLE_JIT`) | SMP (`AVM_DISABLE_SMP`) | Build dir          |
|--------------|----------------------|-------------------------|-------------------------|--------------------|
| BEAM         | OTP-29 reference     | —                       | —                       | (system `erl`)     |
| emu noSMP    | interpreter          | `ON`                    | `ON`                    | `build.emu.nosmp`  |
| emu SMP      | interpreter          | `ON`                    | `OFF`                   | `build.emu`        |
| JIT noSMP    | AOT native (aarch64) | `OFF`                   | `ON`                    | `build.nosmp`      |
| JIT SMP      | AOT native (aarch64) | `OFF`                   | `OFF`                   | `build.release`    |

- **emu** = the AtomVM bytecode interpreter. It runs *plain* packbeam AVMs.
- **JIT** = ahead-of-time compiled native code. On this host the native target
  is `aarch64`; `.beam`s are precompiled with `jit_precompile` and the
  JIT-enabled VM runs the embedded native code. JIT builds need OTP 28+.
- "non-SMP" is `-DAVM_DISABLE_SMP=ON`: a single scheduler, no scheduler
  threads.

### Non-SMP build note

The non-SMP generic_unix build requires the kqueue `EVFILT_USER` event that
`sys_signal` triggers to be *registered* whenever `sys_signal` is compiled —
which is "SMP **or** the task driver", not "SMP only". The condition in
`src/platforms/generic_unix/lib/sys.c` (`sys_init_platform`) is therefore:

```c
#if !defined(AVM_NO_SMP) || defined(AVM_TASK_DRIVER_ENABLED)
```

Without this a non-SMP task-driver build triggers an unregistered user event and
`kevent` fails. With it, all four AtomVM configs build cleanly and run the full
suites.

## 1. Build the four AtomVM configs

From the AtomVM source root (`erlc`/`erl` must be OTP 28+; on macOS-MacPorts
prefix with `PATH=/opt/local/bin:$PATH` so OTP-29 is used, not a Homebrew OTP):

```sh
# JIT SMP   (-O2)
cmake -G Ninja -DAVM_DISABLE_JIT=OFF -DAVM_DISABLE_SMP=OFF \
      -DCMAKE_BUILD_TYPE=RelWithDebInfo . -B build.release && cmake --build build.release
# JIT noSMP (-O2)
cmake -G Ninja -DAVM_DISABLE_JIT=OFF -DAVM_DISABLE_SMP=ON  \
      -DCMAKE_BUILD_TYPE=RelWithDebInfo . -B build.nosmp   && cmake --build build.nosmp
# emu SMP   (-O2)
cmake -G Ninja -DAVM_DISABLE_JIT=ON  -DAVM_DISABLE_SMP=OFF \
      -DCMAKE_BUILD_TYPE=RelWithDebInfo . -B build.emu     && cmake --build build.emu
# emu noSMP (-O2)
cmake -G Ninja -DAVM_DISABLE_JIT=ON  -DAVM_DISABLE_SMP=ON  \
      -DCMAKE_BUILD_TYPE=RelWithDebInfo . -B build.emu.nosmp && cmake --build build.emu.nosmp
```

JIT builds emit `libs/atomvmlib-<target>.avm` (e.g. `-aarch64`); emu builds emit
plain `libs/atomvmlib.avm`.

## 2. Build the workload artifacts

### benchmark app

```sh
# JIT configs: AOT-precompiled AVM (one per build dir)
BUILD_DIR=build.release TARGET=aarch64 tools/dev/build_benchmark_aot_aarch64.sh --no-run
BUILD_DIR=build.nosmp   TARGET=aarch64 tools/dev/build_benchmark_aot_aarch64.sh --no-run

# emu configs: one plain (non-precompiled) AVM, reused by both emu builds
EBIN=../atomvm_benchmark/_build/default/lib/benchmark/ebin
build.release/tools/packbeam/packbeam create --prune --start benchmark \
    build.emu/benchmark-plain.avm "$EBIN"/*.beam
```

### erlc

`atomvm_erlc/build_erlc_variant.sh <aot|emu> <build-dir> <out-exe>` packs the
front-end + OTP compiler/stdlib beams (AOT-precompiled or plain) and appends the
AVM to the AtomVM binary as a self-contained executable:

```sh
cd ../atomvm_erlc
./build_erlc_variant.sh aot ~/AtomVM/build.release   _build/matrix/erlc-jit-smp
./build_erlc_variant.sh aot ~/AtomVM/build.nosmp     _build/matrix/erlc-jit-nosmp
./build_erlc_variant.sh emu ~/AtomVM/build.emu       _build/matrix/erlc-emu-smp
./build_erlc_variant.sh emu ~/AtomVM/build.emu.nosmp _build/matrix/erlc-emu-nosmp
```

## 3. Run the benchmarks

```sh
# benchmark app: all configs + BEAM (median of 15 runs, 2 warmup)
PATH=/opt/local/bin:$PATH tools/dev/bench_matrix_app.py -n 15 --warmup 2

# erlc: all configs + BEAM, OTP-29 stdlib/kernel/sasl/crypto sources
cd ../atomvm_erlc
RUNS=3 APPS="stdlib kernel sasl crypto" PATH=/opt/local/bin:$PATH ./bench_matrix_erlc.py
```

## Measurement methodology

- **Timer**: each invocation is timed end-to-end (`time.monotonic` /
  `perf_counter`) — the whole OS process, **VM startup included**, i.e. what a
  user actually waits for.
- **Repetition**: every datum is the **median** over N runs; warmups are
  discarded (app driver). Each erlc file is compiled in a *fresh temp out dir*
  per run.
- **Like-for-like (erlc)**: a source file is only counted if **every** compiler
  (BEAM and all AtomVM configs) produced a `.beam`. AtomVM does not yet support
  some bitstring features, so files that fail on AtomVM are **listed as skipped
  and excluded from all totals** — every reported row/total is a fair
  comparison. erlc results report, per compiler, the **sum** over common files
  and the **mean per file**, plus the speed-up vs BEAM.
- **App aggregate**: the headline number is the **sum of per-test medians over
  the 11 base tests every config runs**. The SMP-only `[schedulers=1]` rows
  (the suite re-runs pingpong/prime pinned to one scheduler) and the raw
  process wall time are reported separately — wall time is startup-dominated on
  so short a workload and penalises SMP for spawning scheduler threads, so it is
  not the headline.

## Optimisation level

The matrix uses `-O2` (`RelWithDebInfo`). For `-O3`, configure the same four
combinations with `-DCMAKE_BUILD_TYPE=Release` (the repo also keeps `build.o3`
as a `-O3` JIT+SMP build). Keep one level across the whole matrix for a fair
comparison.

## Layout / prerequisites

- AtomVM source: `~/AtomVM`
- Benchmark app: `~/atomvm_benchmark` (`rebar3 compile` once)
- erlc front-end: `~/atomvm_erlc`
- OTP-29 source (erlc workload): `~/otp` (`OTP=` env overrides)
- OTP-29 toolchain on `PATH` (MacPorts `/opt/local/bin`); JIT needs OTP 28+.

Scripts: `tools/dev/bench_matrix_app.py`, `tools/dev/build_benchmark_aot_aarch64.sh`
(AtomVM); `bench_matrix_erlc.py`, `build_erlc_variant.sh` (atomvm_erlc). Env
vars (`ATOMVM_ROOT`, `BUILD_DIR`, `BENCHMARK_DIR`, `OTP`, `MATRIX_DIR`, `RUNS`,
`APPS`, …) override every hard-coded path.
