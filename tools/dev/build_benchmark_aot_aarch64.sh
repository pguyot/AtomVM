#!/bin/sh
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Build an AOT (ahead-of-time, JIT-precompiled) AVM from the benchmark app for
# the aarch64 target, then run it with the local JIT-enabled AtomVM.
#
# Steps:
#   1. rebar3 compile the benchmark app
#   2. jit_precompile each .beam for aarch64
#   3. packbeam the precompiled beams into benchmark-aarch64.avm (start module)
#   4. run it with AtomVM together with atomvmlib-aarch64.avm
#
# Usage:
#   tools/dev/build_benchmark_aot_aarch64.sh [--no-run]
#
# Environment overrides:
#   ATOMVM_ROOT      AtomVM source root (default: derived from this script)
#   BUILD_DIR        CMake build directory (default: $ATOMVM_ROOT/build)
#   BENCHMARK_DIR    Benchmark app directory (default: $ATOMVM_ROOT/../atomvm_benchmark)
#   TARGET           JIT target/variant (default: aarch64)
#   START_MODULE     Start module packed into the avm (default: benchmark)

set -e

TARGET="${TARGET:-aarch64}"
START_MODULE="${START_MODULE:-benchmark}"

# Resolve the AtomVM source root from this script's location unless overridden.
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
ATOMVM_ROOT="${ATOMVM_ROOT:-$(CDPATH= cd -- "${SCRIPT_DIR}/../.." && pwd)}"
BUILD_DIR="${BUILD_DIR:-${ATOMVM_ROOT}/build}"
BENCHMARK_DIR="${BENCHMARK_DIR:-${ATOMVM_ROOT}/../atomvm_benchmark}"

JIT_BEAMS="${BUILD_DIR}/libs/jit/src/beams"
PACKBEAM="${BUILD_DIR}/tools/packbeam/packbeam"
ATOMVM="${BUILD_DIR}/src/AtomVM"
ATOMVMLIB="${BUILD_DIR}/libs/atomvmlib-${TARGET}.avm"

RUN=1
if [ "$1" = "--no-run" ]; then
    RUN=0
fi

# --- Sanity checks ----------------------------------------------------------
for f in "${JIT_BEAMS}/jit_precompile.beam" "${PACKBEAM}" "${ATOMVM}" "${ATOMVMLIB}"; do
    if [ ! -e "$f" ]; then
        echo "error: required artifact not found: $f" >&2
        echo "       Build AtomVM (with JIT, target ${TARGET}) first: cd ${BUILD_DIR} && cmake --build ." >&2
        exit 1
    fi
done
if [ ! -d "${BENCHMARK_DIR}" ]; then
    echo "error: benchmark app directory not found: ${BENCHMARK_DIR}" >&2
    exit 1
fi

# --- 1. Compile the benchmark app -------------------------------------------
echo "==> rebar3 compile (${BENCHMARK_DIR})"
( cd "${BENCHMARK_DIR}" && rebar3 compile )

EBIN="${BENCHMARK_DIR}/_build/default/lib/benchmark/ebin"
if [ ! -d "${EBIN}" ]; then
    echo "error: ebin directory not found after compile: ${EBIN}" >&2
    exit 1
fi

# --- 2. Precompile each .beam for the target --------------------------------
OUT_DIR="${BUILD_DIR}/benchmark-aot-${TARGET}"
AOT_DIR="${OUT_DIR}/${TARGET}"
rm -rf "${OUT_DIR}"
mkdir -p "${AOT_DIR}"

BEAMS=$(ls "${EBIN}"/*.beam)
echo "==> jit_precompile for ${TARGET}"
# jit_precompile.start/0 reads: <target> <out-dir> [dwarf] <beam>...
# The local build disables JIT DWARF (AVM_DISABLE_JIT_DWARF=ON), so no "dwarf" flag.
# shellcheck disable=SC2086
erl -pa "${JIT_BEAMS}" -noshell -s jit_precompile -s init stop -- \
    "${TARGET}" "${AOT_DIR}/" ${BEAMS}

# --- 3. Pack the precompiled beams into an avm ------------------------------
AVM="${OUT_DIR}/benchmark-${TARGET}.avm"
echo "==> packbeam create ${AVM} (start module: ${START_MODULE})"
# shellcheck disable=SC2086
"${PACKBEAM}" create --prune --start "${START_MODULE}" "${AVM}" "${AOT_DIR}"/*.beam

echo ""
echo "Created ${AVM}"
"${PACKBEAM}" list "${AVM}"

# --- 4. Run with the JIT-enabled AtomVM -------------------------------------
if [ "${RUN}" -eq 1 ]; then
    echo ""
    echo "==> ${ATOMVM} ${AVM} ${ATOMVMLIB}"
    echo ""
    "${ATOMVM}" "${AVM}" "${ATOMVMLIB}"
fi
