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

# Test the escript-style trailer-embedded AVM support: an AVM pack appended to
# the AtomVM executable followed by a 16-byte trailer (8-byte little-endian
# pack size + the 8-byte magic "ATOMVMv1") must boot like an embedded escript,
# passing command-line arguments through to the packed start module.
#
# Usage: test_escript_trailer.sh <path-to-AtomVM> <path-to-test.avm>
#
# The test avm's start module must print its plain arguments with
# io:format("args=~p~n", [init:get_plain_arguments()]) and return ok.

set -e

ATOMVM="$1"
TESTAVM="$2"

if [ ! -x "$ATOMVM" ] || [ ! -f "$TESTAVM" ]; then
    echo "usage: $0 <AtomVM> <test.avm>" >&2
    exit 2
fi

WORKDIR=$(mktemp -d)
trap 'rm -rf "$WORKDIR"' EXIT

EXE="$WORKDIR/trailer-exe"
cp "$ATOMVM" "$EXE"
cat "$TESTAVM" >> "$EXE"

# Append the 16-byte trailer: little-endian 64-bit size then magic "ATOMVMv1".
SIZE=$(wc -c < "$TESTAVM" | tr -d ' ')
python3 - "$EXE" "$SIZE" <<'EOF'
import struct, sys
with open(sys.argv[1], 'ab') as f:
    f.write(struct.pack('<Q', int(sys.argv[2])))
    f.write(b'ATOMVMv1')
EOF
chmod +x "$EXE"

OUTPUT=$("$EXE" hello trailer-world 2>&1)
echo "$OUTPUT"
case "$OUTPUT" in
    *'args=["hello","trailer-world"]'*) echo "PASS"; exit 0 ;;
    *) echo "FAIL: expected args=[\"hello\",\"trailer-world\"] in output" >&2; exit 1 ;;
esac
