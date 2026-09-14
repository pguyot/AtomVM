#!/bin/sh
#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#
# Native code size of the JIT compiler's own 27 modules, per backend.
#
# This is a sharper metric than compiling a corpus of other modules, and it
# measures something different: the compiler contains every backend, so it
# reports both the quality of the code a change generates AND the cost of the
# code the change itself adds. A codegen improvement that only helps one
# backend shows up as a net *increase* on the other seven, which a corpus
# measurement reports as "byte-identical" and hides completely.
#
#   tools/dev/jit_compiler_size.sh <dir-of-jit-beams> <label>
#
set -e
SP=${SP:-/tmp}
BEAMS=$1; LABEL=$2
OUT=$SP/jsz-$LABEL; rm -rf $OUT; mkdir -p $OUT
export PATH=/opt/local/bin:$PATH
for T in aarch64 arm32 armv6m riscv32 riscv64 x86_64 xtensa wasm32; do
  mkdir -p $OUT/$T
  erl -pa $BEAMS -noshell -run jit_precompile start -s init stop -- $T $OUT/$T $BEAMS/*.beam >/dev/null 2>&1 || true
done
erl -noshell -eval '
OUT = "'$OUT'",
ES = #{"aarch64"=>4,"arm32"=>8,"armv6m"=>12,"riscv32"=>8,"riscv64"=>8,
       "x86_64"=>5,"xtensa"=>20,"wasm32"=>4},
lists:foreach(fun(T) ->
  {ok, Fs} = file:list_dir(OUT ++ "/" ++ T),
  {N,B} = lists:foldl(fun(F,{C,Acc}) ->
     case beam_lib:all_chunks(OUT++"/"++T++"/"++F) of
       {ok,_,Cs} -> case proplists:get_value("avmN",Cs) of
            undefined -> {C,Acc};
            <<IS:32,Info:IS/binary,Code/binary>> ->
              <<L:32,_/binary>> = Info,
              {C+1, Acc + byte_size(Code) - L*maps:get(T,ES)} end;
       _ -> {C,Acc} end end, {0,0}, Fs),
  io:format("~s ~p ~p~n",[T,N,B])
end, ["aarch64","arm32","armv6m","riscv32","riscv64","x86_64","xtensa","wasm32"]), halt().'
