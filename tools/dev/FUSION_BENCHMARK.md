<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# JIT opcode-fusion micro-benchmark (D / E / H)

Isolates the three JIT fusions so their per-operation effect can be measured
directly, instead of being lost in the whole-app benchmark (where list-head
matching, record access and fixed-field binary decoding are a small fraction of
runtime).

| op              | fusion | opcodes                                   | commit      | status |
|-----------------|--------|-------------------------------------------|-------------|--------|
| `list_match`    | **D**  | `is_nonempty_list` + `get_list`           | `efc72f5e5` | reverted (`6a81daf7f`) |
| `record_access` | **E**  | `is_tagged_tuple` + `get_tuple_element`   | `5cf5b1d20` | kept |
| `bin_decode`    | **H**  | `bs_match` fixed 8/16/32-bit integer reads | `0ca81fb01` | kept |

To isolate D or E against the code without them, build at `efc72f5e5^`
(`0ca81fb01`, H only) as baseline. The fused commits still exist in history even
though D's effect is reverted on the branch tip, so the recipe below is unchanged.

## Why this exists

On x86_64 (out-of-order, fast L1) an interleaved A/B showed:

| op              | x86_64 speedup |
|-----------------|---------------:|
| `bin_decode` (H) | **~1.15x**    |
| `record_access` (E) | ~1.00x (was 0.95x before the resolve-first fix) |
| `list_match` (D) | ~0.94x        |

H is a clear win (it removes dead stores). D and E are neutral-to-negative on
x86_64 because the "redundant" reload they eliminate is a cheap L1 hit that gets
replaced by an equal-cost register operation. The open question was whether D
(and E) are net **wins on the in-order, cache-poorer MCU-class targets**
(aarch64 Apple Silicon is a partial proxy; real Cortex-M/Xtensa/RISC-V more so),
which is what this harness is for.

### aarch64 result (Apple Silicon, two interleaved N=41 runs)

| op                  | aarch64 speedup |
|---------------------|----------------:|
| `list_match` (D)    | ~1.000x (neutral) |
| `record_access` (E) | ~1.000x (neutral) |

Apple Silicon is a big out-of-order core with excellent caches, so this
confirms *no harm* on aarch64 but cannot confirm the MCU-class win that was D's
rationale. Given D showed a −6% regression on x86_64, neutral on aarch64, and
**no demonstrated win on any measured target**, it was reverted (`6a81daf7f`);
E was kept (neutral everywhere, shrinks MCU code). If a real Cortex-M / Xtensa /
RISC-V A/B later shows D winning, revert the revert — the fused commit is intact
in history.

## Isolating one fusion

Build the same tree at the fusion commit and at its parent, then A/B them. Each
build must be a JIT build for the target (`-DAVM_DISABLE_JIT=OFF`), which emits
`libs/atomvmlib-<target>.avm` and the `jit_precompile`/`packbeam` tools the
harness needs.

```sh
# Example: isolate D (list-head) on aarch64.
# baseline = the commit BEFORE D (H only); fusion = the D commit.
git worktree add ../avm-baseline 0ca81fb01     # parent of the D commit
git worktree add ../avm-fusion   efc72f5e5     # the D commit

for w in ../avm-baseline ../avm-fusion; do
  cmake -G Ninja -DAVM_DISABLE_JIT=OFF -DCMAKE_BUILD_TYPE=RelWithDebInfo "$w" -B "$w/build"
  cmake --build "$w/build"
done

BASELINE_BUILD=../avm-baseline/build \
FUSION_BUILD=../avm-fusion/build \
TARGET=aarch64 \
tools/dev/bench_fusion_ab.py 41
```

For **E** use `efc72f5e5` (parent) vs `5cf5b1d20`. For **all three vs
pre-fusion** use the commit before `0ca81fb01` vs `HEAD`. `list_match` is only
affected by D, `record_access` only by E, `bin_decode` only by H, so a single
"all three vs baseline" run also isolates each op.

## Reading the result

The harness round-robins the two configs within each run (robust to thermal
drift), aborts if the microbench's `checks:` line differs between builds (a
correctness regression), and prints the per-op median ratio
(`speedup = baseline / fusion`, so `>1.0` means the fusion is faster). Run it on
an otherwise-idle machine — the ops are small and sensitive to background load.
