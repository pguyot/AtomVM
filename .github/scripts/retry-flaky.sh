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
# Re-run a command that is known to fail intermittently in CI. A deterministic
# failure still fails every attempt and so still fails the job; only transient
# flakes recover. Each retry emits a GitHub warning annotation so the flake rate
# stays visible in the run summary rather than being silently absorbed.
#
# TODO: each caller should reference a tracking issue for the flake it retries,
# and the retry should be removed once that issue is fixed.
#
# Usage: retry-flaky.sh <attempts> <delay-seconds> <label> -- <command> [args...]

set -eu

if [ "$#" -lt 5 ]; then
    echo "usage: $0 <attempts> <delay-seconds> <label> -- <command> [args...]" >&2
    exit 2
fi

attempts="$1"
delay="$2"
label="$3"
shift 3
if [ "$1" != "--" ]; then
    echo "$0: expected -- before the command" >&2
    exit 2
fi
shift

n=1
while true; do
    if "$@"; then
        exit 0
    fi
    if [ "$n" -ge "$attempts" ]; then
        echo "::error::${label} failed after ${n} attempts"
        exit 1
    fi
    echo "::warning::${label} failed (attempt ${n}/${attempts}) - retrying, suspected flake"
    n=$((n + 1))
    sleep "$delay"
done
