# AtomVM CI Workflow Analysis

Analysis date: 2026-03-05
Branch analyzed: `main`
Data source: GitHub Actions API (most recent completed runs)

---

## Executive Summary

Every push to `main` triggers **15 workflows** producing approximately **~200 parallel jobs** and consuming **~1,350 cumulative job-minutes** of compute time. The critical path (wall-clock time to all-green) is dominated by **"Build and Test"** at ~150 minutes, driven by 3 JIT+valgrind jobs taking ~60 minutes each.

---

## 1. Workflow Inventory and Timing

### Per-Workflow Job-Time (most recent run on main)

| Workflow | Jobs | Total Job-Time | Longest Job | Runner Type |
|----------|------|---------------|-------------|-------------|
| **Build and Test** | **49** | **632.6 min** | **60.9 min** (OTP 28 JIT arm64) | ubuntu-22.04/24.04/24.04-arm |
| **ESP32 Builds** | 11 | 202.4 min | 21.9 min | ubuntu-24.04 (IDF container) |
| **Build and Test on macOS** | 23 | 128.3 min | 10.8 min (OTP 28 JIT intel) | macos-14/15/15-intel/26 |
| **esp32-mkimage** | 26 | 122.9 min | 5.3 min | ubuntu-24.04 (IDF container) |
| **Pico Build** | 13 | 106.2 min | 12.6 min | ubuntu-24.04 |
| **Run tests with BEAM** | 12 | 56.9 min | 6.7 min | ubuntu-24.04, macos-15 |
| **STM32 Build** | 15 | 30.7 min | 2.5 min | ubuntu-24.04 |
| **build-and-test-on-freebsd** | 3 | 24.6 min | 8.6 min | ubuntu-24.04 (FreeBSD VM) |
| **Wasm Build** | 4 | 21.3 min | 8.6 min | ubuntu-24.04 |
| **CodeQL** | 1 | 6.4 min | 6.4 min | ubuntu-24.04 |
| **Build Docs** | 1 | 2.9 min | 2.9 min | ubuntu-24.04 |
| **Publish Docs** | 1 | 3.0 min | 3.0 min | ubuntu-24.04 |
| **Check formatting** | 3 | 1.5 min | 0.7 min | ubuntu-24.04 |
| **REUSE Compliance Check** | 1 | 0.2 min | 0.2 min | ubuntu-24.04 |
| **ESP32 Sim test** | 2 | 0.1 min | 0.1 min (skipped w/o token) | ubuntu-24.04 |
| **TOTAL** | **~165** | **~1,340 min** | | |

### Cost Multipliers by Runner Type

| Runner | Multiplier | Used By |
|--------|-----------|---------|
| ubuntu (x86_64) | 1x | Most workflows |
| ubuntu-arm | 2x | Build and Test (2 jobs) |
| macos (ARM) | 10x | macOS Build and Test, Run tests with BEAM |
| macos (Intel) | 10x | macOS Build and Test |

**Estimated weighted cost per push to main: ~2,700 "billable-equivalent" minutes**
(driven heavily by 23 macOS jobs at 10x multiplier = ~1,283 weighted minutes for macOS alone)

---

## 2. Critical Path Analysis

The wall-clock time before all checks pass on a push to main:

```
0        10        20        30        40        50        60 min
|---------|---------|---------|---------|---------|---------|
Check formatting    [=]                                           1.5 min
REUSE               [=]                                           0.2 min
CodeQL              [======]                                      6.4 min
Build Docs          [===]                                         2.9 min
STM32 Build         [===]                                         2.5 min
macOS Build         [==========]                                 10.8 min
FreeBSD Build       [=========]                                   8.6 min
Wasm Build          [=========]                                   8.6 min
Run tests w/ BEAM   [=======]                                     6.7 min
Pico Build          [=============]                              12.6 min
ESP32 Builds        [======================]                     21.9 min
ESP32 mkimage       [=====]                                       5.3 min
Build & Test (std)  [==============]                             13.4 min (longest non-JIT)
Build & Test (JIT)  [============================================================] 60.9 min <<<
```

**The critical path is 60.9 minutes**, entirely determined by the JIT+valgrind jobs in "Build and Test".

---

## 3. Bottleneck Deep Dive

### 3.1 Build and Test - JIT Valgrind Jobs (60 min each)

Step-level breakdown for the slowest job (OTP 28 JIT, aarch64):

| Step | Duration | % of Total |
|------|----------|-----------|
| Build: run make | 6.5 min | 10.7% |
| Test: test-erlang with valgrind | 14.1 min | 23.2% |
| Test: test_estdlib.avm with valgrind | 10.5 min | 17.2% |
| Test: test_jit.avm with valgrind | **20.3 min** | **33.3%** |
| Test: Tests.avm (Elixir) with valgrind | 2.0 min | 3.3% |
| Test: test_etest.avm with valgrind | 1.2 min | 2.0% |
| Test: test_eavmlib.avm with valgrind | 1.0 min | 1.6% |
| Other steps | 5.3 min | 8.7% |

**Key insight**: Valgrind testing accounts for ~48 min of the 60 min. The `test_jit.avm` suite alone takes 20 minutes under valgrind.

### 3.2 Build and Test - Matrix Explosion (49 jobs)

The matrix tests 6 base compilers x 3 OTP versions + 40 additional includes:
- 6 compiler variants with OTP 25, 26, 27 (18 jobs) -- ~7-9 min each
- 6 additional compiler-with-warnings jobs (~10-13 min each)
- 4 older OTP versions (21-24) (~9-10 min each)
- 2 cross-compilation (arm, riscv32) builds (~5-6 min each)
- 2 cross-compilation with QEMU test (arm, riscv32) (~17-28 min each)
- 3 JIT builds (x86_64 OTP 28, x86_64 OTP 29-rc1, aarch64 OTP 28) (~58-61 min each)
- 2 32-bit builds, 1 32-bit-float, master OTP, older cmake, etc.

### 3.3 ESP32 Builds (202 cumulative job-minutes)

Each ESP32 build job takes ~18-22 min, with the time split between:
- Building with idf.py: ~4 min
- CodeQL analysis: ~6 min
- Building tests with memory checks: ~4 min
- Building tests without memory checks: ~4 min
- QEMU test runs: ~0.5 min each

**CodeQL is embedded in every ESP32 build job** adding ~6 min to each of the 11 jobs (66 minutes total wasted).

### 3.4 esp32-mkimage (123 cumulative job-minutes, 26 jobs)

Builds 13 SoC variants x 2 flavors (erlang-only, +elixir). Each job takes ~4.5-5.3 min. All jobs use the same IDF version (v5.5.2) and compiler (clang-14).

### 3.5 macOS Build and Test (128 cumulative job-minutes, 23 jobs)

4 macOS versions x 5 OTP versions + 3 JIT variants. macOS runners are 10x cost. No build caching.

### 3.6 Pico Build (106 cumulative job-minutes, 13 jobs)

Two-stage workflow (build-atomvmlib -> pico matrix). CodeQL is embedded in pico build jobs adding ~4 min each.

---

## 4. Improvement Recommendations

### HIGH IMPACT

#### 4.1 Split Valgrind from Non-Valgrind Testing in JIT Builds
**Savings: ~45 min off critical path**

The JIT+valgrind jobs are the critical path at 60 min. Consider:
- Running valgrind tests only nightly or on a separate schedule for JIT builds
- Or splitting the JIT job into two: one fast job (build + test without valgrind, ~2 min test time) and one valgrind-only job that does not block merge

```yaml
# Example: scheduled valgrind for JIT
on:
  schedule:
    - cron: '0 2 * * *'  # nightly at 2 AM
```

#### 4.2 Add Build Caching to macOS Workflow
**Savings: ~30-50% of macOS job time (estimated ~50 weighted minutes)**

The macOS workflow has **no caching at all** — no ccache, no BEAM file cache, no brew cache. Given the 10x cost multiplier, this is the highest-ROI caching target.

- Add BEAM file caching (like build-and-test.yaml already does)
- Consider `ccache` for C/C++ compilation
- Cache Homebrew downloads

#### 4.3 Deduplicate CodeQL from Platform Builds
**Savings: ~90 cumulative job-minutes**

CodeQL analysis runs in:
1. A standalone `codeql-analysis.yaml` workflow (6.4 min)
2. Embedded in **every** ESP32 Build job (~6 min x 11 = 66 min)
3. Embedded in **every** Pico Build job (~4 min x 13 = 52 min)

Remove CodeQL from the ESP32 and Pico build matrices and rely on the standalone workflow. This saves ~118 cumulative minutes per push.

#### 4.4 Reduce the Build-and-Test Compiler Matrix
**Savings: ~80-130 cumulative job-minutes**

The current matrix tests 10+ compiler versions. Consider:
- **Drop gcc-7 and gcc-8**: These are 7+ years old and run in ubuntu:20.04 containers. If AtomVM's minimum supported compiler is documented, enforce it.
- **Consolidate OTP version testing**: The 6 base compilers each test 3 OTP versions (25, 26, 27). Since compiler compatibility is largely OTP-independent, test each compiler with one OTP version and test all OTP versions with one compiler.
- **Move older OTP (21-24) to nightly**: These are legacy; breakage is rare.

### MEDIUM IMPACT

#### 4.5 Deduplicate Build Docs / Publish Docs
**Savings: ~3 cumulative minutes + reduced workflow noise**

Both `build-docs.yaml` and `publish-docs.yaml` trigger on the same paths and do nearly identical work. On pushes to main, only `publish-docs` needs to run (it builds and publishes).

#### 4.6 Cache ESP-IDF Build Artifacts
**Savings: ~30-50% of ESP32 build time**

ESP32 builds take 18-22 min each with no caching. The IDF build system supports `ccache`. Adding ccache could save significant time on incremental builds.

```yaml
- uses: actions/cache@v4
  with:
    path: ~/.ccache
    key: esp-idf-${{ matrix.idf-version }}-${{ matrix.esp-idf-target }}-${{ github.sha }}
    restore-keys: |
      esp-idf-${{ matrix.idf-version }}-${{ matrix.esp-idf-target }}-
```

#### 4.7 Add Path Filters to Broad Workflows
**Savings: Avoid unnecessary runs on doc-only or metadata-only changes**

These workflows lack `paths` filters and use only `paths-ignore`:
- `build-and-test.yaml` (49 jobs!)
- `build-and-test-macos.yaml` (23 jobs!)
- `build-and-test-on-freebsd.yaml`
- `run-tests-with-beam.yaml`
- `codeql-analysis.yaml`
- `reuse-lint.yaml`

The `paths-ignore` approach means a change to any non-ignored file triggers the workflow. For example, adding a new license file or changing CI workflow YAML for an unrelated platform triggers all of these. Consider switching to explicit `paths` triggers or adding more exclusions.

#### 4.8 Reduce esp32-mkimage Matrix
**Savings: ~60 cumulative job-minutes**

26 jobs for 13 SoC variants x 2 flavors. Consider:
- Building elixir-enabled images only for mainline chips (esp32, esp32s3, esp32c3) rather than all 13 variants
- Or making elixir variant nightly-only

### LOWER IMPACT

#### 4.9 Consolidate QEMU Cross-Compilation Test Jobs
The riscv32 QEMU test job takes 28 min because it installs a custom toolchain and sysroot. Consider pre-building a Docker image with the toolchain to reduce setup time.

#### 4.10 Use `fail-fast: true` Strategically
All matrix workflows use `fail-fast: false`. For PR checks, `fail-fast: true` could save compute by cancelling remaining jobs when one fails. Keep `fail-fast: false` only for `main` branch pushes where full results are needed.

#### 4.11 Reduce FreeBSD Version Matrix
Testing 3 FreeBSD versions (13.5, 14.3, 15.0) in VMs is slow. Consider dropping FreeBSD 13.x (EOL) or making it nightly.

---

## 5. Summary of Estimated Savings

| Recommendation | Cumulative Minutes Saved | Critical Path Impact |
|---------------|-------------------------|---------------------|
| 4.1 Split JIT valgrind to nightly | ~170 min | **-45 min** (critical!) |
| 4.2 macOS build caching | ~50 weighted min | -2-3 min |
| 4.3 Deduplicate CodeQL | ~118 min | None |
| 4.4 Reduce compiler matrix | ~80-130 min | -2-3 min |
| 4.5 Dedup Build/Publish Docs | ~3 min | None |
| 4.6 Cache ESP-IDF builds | ~60-100 min | -5-10 min |
| 4.7 Add path filters | Variable (full run avoidance) | Full avoidance |
| 4.8 Reduce esp32-mkimage matrix | ~60 min | None |
| **Total** | **~540-630 min/push** | **~45 min** |

These changes could reduce cumulative job-time by **~40-47%** and cut the critical path from **61 minutes to ~15 minutes**.
