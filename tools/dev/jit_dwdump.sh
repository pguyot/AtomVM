#!/bin/sh
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Disassemble AtomVM's JIT output for any target, from any host, with the BEAM
# opcode names carried through as symbols.
#
# WARNING: this is NOT the code the build emits. jit_dwarf does not export
# committed_offset/1, which is what a backend checks before it will emit a
# fused forward conditional branch, so every such branch appears here as the
# two-instruction "b.cond skip ; b target" form instead of one b.cond. Use it
# to read what an opcode lowers to, not to count instructions -- for counts,
# precompile without dwarf and disassemble the raw avmN chunk. Companion to `erl +JDdump true`,
# which writes BeamAsm's own assembly -- annotated with its opcode names the
# same way -- to <module>.asm, so the two can be read side by side.
#
#   JITDW=/path/to/dwarf-jit-beams tools/dev/jit_dwdump.sh aarch64 mod.beam [outdir]
#
# JITDW must hold the JIT compiler built with -DJIT_DWARF, which is what makes
# jit_precompile emit the ELF:
#
#   erlc -o $JITDW -I libs/jit/include -I libs/jit/src libs/jit/src/jit_dwarf_pt.erl
#   erlc -o $JITDW -pa $JITDW -DJIT_DWARF -DATOMVM_VERSION='"audit"' \
#        -I libs/jit/include -I libs/jit/src libs/jit/src/*.erl
#
set -e
TARGET=$1
BEAM=$2
OUT=${3:-$(mktemp -d)}
: "${JITDW:?set JITDW to a directory of jit beams built with -DJIT_DWARF}"
OBJDUMP=${OBJDUMP:-llvm-objdump}

mkdir -p "$OUT"
erl -pa "$JITDW" -noshell -run jit_precompile start -s init stop -- \
    "$TARGET" "$OUT" dwarf "$BEAM" >/dev/null
erl -noshell -eval '
  {ok,{_,[{_,C}]}} = beam_lib:chunks("'"$OUT/$(basename "$BEAM")"'", ["avmN"]),
  <<IS:32/big,_:IS/binary,Elf/binary>> = C,
  <<16#7f,"ELF",_/binary>> = Elf,
  ok = file:write_file("'"$OUT/mod.elf"'", Elf), halt().'

case $TARGET in
    x86_64) "$OBJDUMP" -d --no-show-raw-insn --x86-asm-syntax=intel "$OUT/mod.elf" ;;
    *)      "$OBJDUMP" -d --no-show-raw-insn "$OUT/mod.elf" ;;
esac
