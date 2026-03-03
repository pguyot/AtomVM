%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

-module(jit_riscv64_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_riscv64).

% disassembly obtained with:
% riscv64-linux-gnu-objdump -b binary -m riscv:rv64 -D dump.bin

call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t00063f83          \tld\tt6,0(a2)\n"
            "   4:\t1101                \taddi\tsp,sp,-32\n"
            "   6:\te006                \tsd\tra,0(sp)\n"
            "   8:\te42a                \tsd\ta0,8(sp)\n"
            "   a:\te82e                \tsd\ta1,16(sp)\n"
            "   c:\tec32                \tsd\ta2,24(sp)\n"
            "   e:\t9f82                \tjalr\tt6\n"
            "  10:\t8faa                \tmv\tt6,a0\n"
            "  12:\t6082                \tld\tra,0(sp)\n"
            "  14:\t6522                \tld\ta0,8(sp)\n"
            "  16:\t65c2                \tld\ta1,16(sp)\n"
            "  18:\t6662                \tld\ta2,24(sp)\n"
            "  1a:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_1_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 1, [ctx, jit_state]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t4fa1                \tli\tt6,8\n"
            "   2:\t9fb2                \tadd\tt6,t6,a2\n"
            "   4:\t000fbf83          \tld\tt6,0(t6)\n"
            "   8:\t1101                \taddi\tsp,sp,-32\n"
            "   a:\te006                \tsd\tra,0(sp)\n"
            "   c:\te42a                \tsd\ta0,8(sp)\n"
            "   e:\te82e                \tsd\ta1,16(sp)\n"
            "  10:\tec32                \tsd\ta2,24(sp)\n"
            "  12:\t9f82                \tjalr\tt6\n"
            "  14:\t8faa                \tmv\tt6,a0\n"
            "  16:\t6082                \tld\tra,0(sp)\n"
            "  18:\t6522                \tld\ta0,8(sp)\n"
            "  1a:\t65c2                \tld\ta1,16(sp)\n"
            "  1c:\t6662                \tld\ta2,24(sp)\n"
            "  1e:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_2_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 2, [ctx, 42, 43, 44]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t4fc1                \tli\tt6,16\n"
            "   2:\t9fb2                \tadd\tt6,t6,a2\n"
            "   4:\t000fbf83          \tld\tt6,0(t6)\n"
            "   8:\t1101                \taddi\tsp,sp,-32\n"
            "   a:\te006                \tsd\tra,0(sp)\n"
            "   c:\te42a                \tsd\ta0,8(sp)\n"
            "   e:\te82e                \tsd\ta1,16(sp)\n"
            "  10:\tec32                \tsd\ta2,24(sp)\n"
            "  12:\t02a00593          \tli\ta1,42\n"
            "  16:\t02b00613          \tli\ta2,43\n"
            "  1a:\t02c00693          \tli\ta3,44\n"
            "  1e:\t9f82                \tjalr\tt6\n"
            "  20:\t8faa                \tmv\tt6,a0\n"
            "  22:\t6082                \tld\tra,0(sp)\n"
            "  24:\t6522                \tld\ta0,8(sp)\n"
            "  26:\t65c2                \tld\ta1,16(sp)\n"
            "  28:\t6662                \tld\ta2,24(sp)\n"
            "  2a:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, ?PRIM_ALLOCATE, [ctx, jit_state, 16, 32, 2]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t02800f93          \tli\tt6,40\n"
            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
            "   a:\t4641                \tli\ta2,16\n"
            "   c:\t02000693          \tli\ta3,32\n"
            "  10:\t4709                \tli\ta4,2\n"
            "  12:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_6_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get bin_ptr from x_reg 0 (similar to get_list_test pattern)
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, RegA} = ?BACKEND:and_(State1, {free, RegA}, ?TERM_PRIMARY_CLEAR_MASK),
    % Get another register for the last parameter to test {free, Reg} handling
    {State3, OtherReg} = ?BACKEND:move_to_native_register(State2, {x_reg, 1}),
    % Call PRIM_BITSTRING_EXTRACT_INTEGER with 6 arguments
    {State4, _ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_BITSTRING_EXTRACT_INTEGER, [
        ctx, jit_state, {free, RegA}, 64, 8, {free, OtherReg}
    ]),
    Stream = ?BACKEND:stream(State4),
    Dump =
        <<
            "   0:\t05853f83          \tld\tt6,88(a0)\n"
            "   4:\tffcfff93          \tandi\tt6,t6,-4\n"
            "   8:\t06053f03          \tld\tt5,96(a0)\n"
            "   c:\t02e00e93          \tli\tt4,46\n"
            "  10:\t0e8e                \tslli\tt4,t4,0x3\n"
            "  12:\t9eb2                \tadd\tt4,t4,a2\n"
            "  14:\t000ebe83          \tld\tt4,0(t4)\n"
            "  18:\t1101                \taddi\tsp,sp,-32\n"
            "  1a:\te006                \tsd\tra,0(sp)\n"
            "  1c:\te42a                \tsd\ta0,8(sp)\n"
            "  1e:\te82e                \tsd\ta1,16(sp)\n"
            "  20:\tec32                \tsd\ta2,24(sp)\n"
            "  22:\t867e                \tmv\ta2,t6\n"
            "  24:\t04000693          \tli\ta3,64\n"
            "  28:\t4721                \tli\ta4,8\n"
            "  2a:\t87fa                \tmv\ta5,t5\n"
            "  2c:\t9e82                \tjalr\tt4\n"
            "  2e:\t8eaa                \tmv\tt4,a0\n"
            "  30:\t6082                \tld\tra,0(sp)\n"
            "  32:\t6522                \tld\ta0,8(sp)\n"
            "  34:\t65c2                \tld\ta1,16(sp)\n"
            "  36:\t6662                \tld\ta2,24(sp)\n"
            "  38:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_extended_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:call_primitive(State0, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]),
    {State2, RegB} = ?BACKEND:call_primitive(State1, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 20]),
    {State3, RegC} = ?BACKEND:call_primitive(State2, ?PRIM_EXTENDED_REGISTER_PTR, [ctx, 19]),
    {State4, ResultReg} = ?BACKEND:call_primitive(State3, ?PRIM_PUT_LIST, [
        ctx, {free, {ptr, RegA}}, {free, {ptr, RegB}}
    ]),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {ptr, RegC}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg, {ptr, RegC}]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:\t09000f93          \tli\tt6,144\n"
        "   4:\t9fb2                \tadd\tt6,t6,a2\n"
        "   6:\t000fbf83          \tld\tt6,0(t6)\n"
        "   a:\t1101                \taddi\tsp,sp,-32\n"
        "   c:\te006                \tsd\tra,0(sp)\n"
        "   e:\te42a                \tsd\ta0,8(sp)\n"
        "  10:\te82e                \tsd\ta1,16(sp)\n"
        "  12:\tec32                \tsd\ta2,24(sp)\n"
        "  14:\t45cd                \tli\ta1,19\n"
        "  16:\t9f82                \tjalr\tt6\n"
        "  18:\t8faa                \tmv\tt6,a0\n"
        "  1a:\t6082                \tld\tra,0(sp)\n"
        "  1c:\t6522                \tld\ta0,8(sp)\n"
        "  1e:\t65c2                \tld\ta1,16(sp)\n"
        "  20:\t6662                \tld\ta2,24(sp)\n"
        "  22:\t02010113          \taddi\tsp,sp,32\n"
        "  26:\t09000f13          \tli\tt5,144\n"
        "  2a:\t9f32                \tadd\tt5,t5,a2\n"
        "  2c:\t000f3f03          \tld\tt5,0(t5)\n"
        "  30:\tfd010113          \taddi\tsp,sp,-48\n"
        "  34:\te006                \tsd\tra,0(sp)\n"
        "  36:\te42a                \tsd\ta0,8(sp)\n"
        "  38:\te82e                \tsd\ta1,16(sp)\n"
        "  3a:\tec32                \tsd\ta2,24(sp)\n"
        "  3c:\tf07e                \tsd\tt6,32(sp)\n"
        "  3e:\t45d1                \tli\ta1,20\n"
        "  40:\t9f02                \tjalr\tt5\n"
        "  42:\t8f2a                \tmv\tt5,a0\n"
        "  44:\t6082                \tld\tra,0(sp)\n"
        "  46:\t6522                \tld\ta0,8(sp)\n"
        "  48:\t65c2                \tld\ta1,16(sp)\n"
        "  4a:\t6662                \tld\ta2,24(sp)\n"
        "  4c:\t7f82                \tld\tt6,32(sp)\n"
        "  4e:\t03010113          \taddi\tsp,sp,48\n"
        "  52:\t09000e93          \tli\tt4,144\n"
        "  56:\t9eb2                \tadd\tt4,t4,a2\n"
        "  58:\t000ebe83          \tld\tt4,0(t4)\n"
        "  5c:\tfd010113          \taddi\tsp,sp,-48\n"
        "  60:\te006                \tsd\tra,0(sp)\n"
        "  62:\te42a                \tsd\ta0,8(sp)\n"
        "  64:\te82e                \tsd\ta1,16(sp)\n"
        "  66:\tec32                \tsd\ta2,24(sp)\n"
        "  68:\tf07e                \tsd\tt6,32(sp)\n"
        "  6a:\tf47a                \tsd\tt5,40(sp)\n"
        "  6c:\t45cd                \tli\ta1,19\n"
        "  6e:\t9e82                \tjalr\tt4\n"
        "  70:\t8eaa                \tmv\tt4,a0\n"
        "  72:\t6082                \tld\tra,0(sp)\n"
        "  74:\t6522                \tld\ta0,8(sp)\n"
        "  76:\t65c2                \tld\ta1,16(sp)\n"
        "  78:\t6662                \tld\ta2,24(sp)\n"
        "  7a:\t7f82                \tld\tt6,32(sp)\n"
        "  7c:\t7f22                \tld\tt5,40(sp)\n"
        "  7e:\t03010113          \taddi\tsp,sp,48\n"
        "  82:\t06800e13          \tli\tt3,104\n"
        "  86:\t9e32                \tadd\tt3,t3,a2\n"
        "  88:\t000e3e03          \tld\tt3,0(t3)\n"
        "  8c:\tfd010113          \taddi\tsp,sp,-48\n"
        "  90:\te006                \tsd\tra,0(sp)\n"
        "  92:\te42a                \tsd\ta0,8(sp)\n"
        "  94:\te82e                \tsd\ta1,16(sp)\n"
        "  96:\tec32                \tsd\ta2,24(sp)\n"
        "  98:\tf076                \tsd\tt4,32(sp)\n"
        "  9a:\t000fb583          \tld\ta1,0(t6)\n"
        "  9e:\t000f3603          \tld\ta2,0(t5)\n"
        "  a2:\t9e02                \tjalr\tt3\n"
        "  a4:\t8e2a                \tmv\tt3,a0\n"
        "  a6:\t6082                \tld\tra,0(sp)\n"
        "  a8:\t6522                \tld\ta0,8(sp)\n"
        "  aa:\t65c2                \tld\ta1,16(sp)\n"
        "  ac:\t6662                \tld\ta2,24(sp)\n"
        "  ae:\t7e82                \tld\tt4,32(sp)\n"
        "  b0:\t03010113          \taddi\tsp,sp,48\n"
        "  b4:\t01ceb023          \tsd\tt3,0(t4)"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_few_free_regs_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, 1),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, 2),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, 3),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, 4),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, 5),
    {State6, ResultReg} = ?BACKEND:call_primitive(State5, ?PRIM_BITSTRING_INSERT_INTEGER, [
        t5, t6, {free, t3}, t4, {free, t2}
    ]),
    State7 = ?BACKEND:free_native_registers(State6, [ResultReg, t5, t6, t4]),
    ?BACKEND:assert_all_native_free(State7),
    Stream = ?BACKEND:stream(State7),
    Dump = <<
        "   0:\t4f85                \tli\tt6,1\n"
        "   2:\t4f09                \tli\tt5,2\n"
        "   4:\t4e8d                \tli\tt4,3\n"
        "   6:\t4e11                \tli\tt3,4\n"
        "   8:\t4395                \tli\tt2,5\n"
        "   a:\t03900313          \tli\tt1,57\n"
        "   e:\t030e                \tslli\tt1,t1,0x3\n"
        "  10:\t9332                \tadd\tt1,t1,a2\n"
        "  12:\t00033303          \tld\tt1,0(t1)\n"
        "  16:\tfc010113          \taddi\tsp,sp,-64\n"
        "  1a:\te006                \tsd\tra,0(sp)\n"
        "  1c:\te42a                \tsd\ta0,8(sp)\n"
        "  1e:\te82e                \tsd\ta1,16(sp)\n"
        "  20:\tec32                \tsd\ta2,24(sp)\n"
        "  22:\tf07e                \tsd\tt6,32(sp)\n"
        "  24:\tf47a                \tsd\tt5,40(sp)\n"
        "  26:\tf876                \tsd\tt4,48(sp)\n"
        "  28:\t857a                \tmv\ta0,t5\n"
        "  2a:\t85fe                \tmv\ta1,t6\n"
        "  2c:\t8672                \tmv\ta2,t3\n"
        "  2e:\t86f6                \tmv\ta3,t4\n"
        "  30:\t871e                \tmv\ta4,t2\n"
        "  32:\t9302                \tjalr\tt1\n"
        "  34:\t832a                \tmv\tt1,a0\n"
        "  36:\t6082                \tld\tra,0(sp)\n"
        "  38:\t6522                \tld\ta0,8(sp)\n"
        "  3a:\t65c2                \tld\ta1,16(sp)\n"
        "  3c:\t6662                \tld\ta2,24(sp)\n"
        "  3e:\t7f82                \tld\tt6,32(sp)\n"
        "  40:\t7f22                \tld\tt5,40(sp)\n"
        "  42:\t7ec2                \tld\tt4,48(sp)\n"
        "  44:\t04010113          \taddi\tsp,sp,64"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_ext_only_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, -1]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:\t0105af83          \tlw\tt6,16(a1)\n"
        "   4:\t1ffd                \taddi\tt6,t6,-1\n"
        "   6:\t01f5a823          \tsw\tt6,16(a1)\n"
        "   a:\t000f9d63          \tbnez\tt6,0x24\n"
        "   e:\t00000f97          \tauipc\tt6,0x0\n"
        "  12:\t0fd9                \taddi\tt6,t6,22 # 0x24\n"
        "  14:\t0001                \tnop\n"
        "  16:\t01f5b423          \tsd\tt6,8(a1)\n"
        "  1a:\t4fc1                \tli\tt6,16\n"
        "  1c:\t9fb2                \tadd\tt6,t6,a2\n"
        "  1e:\t000fbf83          \tld\tt6,0(t6)\n"
        "  22:\t8f82                \tjr\tt6\n"
        "  24:\t02000f93          \tli\tt6,32\n"
        "  28:\t9fb2                \tadd\tt6,t6,a2\n"
        "  2a:\t000fbf83          \tld\tt6,0(t6)\n"
        "  2e:\t02e00613          \tli\ta2,46\n"
        "  32:\t4689                \tli\ta3,2\n"
        "  34:\t4709                \tli\ta4,2\n"
        "  36:\t57fd                \tli\ta5,-1\n"
        "  38:\t8f82                \tjr\tt6"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_last_5_args_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?CASE_CLAUSE_ATOM, {free, RegA}
    ]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:\t05853f83          \tld\tt6,88(a0)\n"
        "   4:\t09800f13          \tli\tt5,152\n"
        "   8:\t9f32                \tadd\tt5,t5,a2\n"
        "   a:\t000f3f03          \tld\tt5,0(t5)\n"
        "   e:\t4639                \tli\ta2,14\n"
        "  10:\t2cb00693          \tli\ta3,715\n"
        "  14:\t877e                \tmv\ta4,t6\n"
        "  16:\t8f02                \tjr\tt5"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_ext_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_CALL_EXT, [ctx, jit_state, offset, 2, 2, 10]),
    Stream = ?BACKEND:stream(State2),
    Dump = <<
        "   0:\t0105af83          \tlw\tt6,16(a1)\n"
        "   4:\t1ffd                \taddi\tt6,t6,-1\n"
        "   6:\t01f5a823          \tsw\tt6,16(a1)\n"
        "   a:\t000f9d63          \tbnez\tt6,0x24\n"
        "   e:\t00000f97          \tauipc\tt6,0x0\n"
        "  12:\t0fd9                \taddi\tt6,t6,22 # 0x24\n"
        "  14:\t0001                \tnop\n"
        "  16:\t01f5b423          \tsd\tt6,8(a1)\n"
        "  1a:\t4fc1                \tli\tt6,16\n"
        "  1c:\t9fb2                \tadd\tt6,t6,a2\n"
        "  1e:\t000fbf83          \tld\tt6,0(t6)\n"
        "  22:\t8f82                \tjr\tt6\n"
        "  24:\t02000f93          \tli\tt6,32\n"
        "  28:\t9fb2                \tadd\tt6,t6,a2\n"
        "  2a:\t000fbf83          \tld\tt6,0(t6)\n"
        "  2e:\t02e00613          \tli\ta2,46\n"
        "  32:\t4689                \tli\ta3,2\n"
        "  34:\t4709                \tli\ta4,2\n"
        "  36:\t47a9                \tli\ta5,10\n"
        "  38:\t8f82                \tjr\tt6"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_primitive_last_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:call_primitive_last(State0, 0, [ctx, jit_state, 42]),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t00063f83          \tld\tt6,0(a2)\n"
            "   4:\t02a00613          \tli\ta2,42\n"
            "   8:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

return_if_not_equal_to_ctx_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(t6, ResultReg),
                    State2 = ?BACKEND:return_if_not_equal_to_ctx(State1, {free, ResultReg}),
                    Stream = ?BACKEND:stream(State2),
                    Dump =
                        <<
                            "   0:\t0a800f93          \tli\tt6,168\n"
                            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
                            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
                            "   a:\t1101                \taddi\tsp,sp,-32\n"
                            "   c:\te006                \tsd\tra,0(sp)\n"
                            "   e:\te42a                \tsd\ta0,8(sp)\n"
                            "  10:\te82e                \tsd\ta1,16(sp)\n"
                            "  12:\tec32                \tsd\ta2,24(sp)\n"
                            "  14:\t9f82                \tjalr\tt6\n"
                            "  16:\t8faa                \tmv\tt6,a0\n"
                            "  18:\t6082                \tld\tra,0(sp)\n"
                            "  1a:\t6522                \tld\ta0,8(sp)\n"
                            "  1c:\t65c2                \tld\ta1,16(sp)\n"
                            "  1e:\t6662                \tld\ta2,24(sp)\n"
                            "  20:\t02010113          \taddi\tsp,sp,32\n"
                            "  24:\t00af8463          \tbeq\tt6,a0,0x2c\n"
                            "  28:\t857e                \tmv\ta0,t6\n"
                            "  2a:\t8082                \tret"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State1, ResultReg} = ?BACKEND:call_primitive(
                        State0, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
                            ctx, jit_state
                        ]
                    ),
                    ?assertEqual(t6, ResultReg),
                    {State2, OtherReg} = ?BACKEND:copy_to_native_register(State1, ResultReg),
                    ?assertEqual(t5, OtherReg),
                    State3 = ?BACKEND:return_if_not_equal_to_ctx(State2, {free, OtherReg}),
                    Stream = ?BACKEND:stream(State3),
                    Dump =
                        <<
                            "   0:\t0a800f93          \tli\tt6,168\n"
                            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
                            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
                            "   a:\t1101                \taddi\tsp,sp,-32\n"
                            "   c:\te006                \tsd\tra,0(sp)\n"
                            "   e:\te42a                \tsd\ta0,8(sp)\n"
                            "  10:\te82e                \tsd\ta1,16(sp)\n"
                            "  12:\tec32                \tsd\ta2,24(sp)\n"
                            "  14:\t9f82                \tjalr\tt6\n"
                            "  16:\t8faa                \tmv\tt6,a0\n"
                            "  18:\t6082                \tld\tra,0(sp)\n"
                            "  1a:\t6522                \tld\ta0,8(sp)\n"
                            "  1c:\t65c2                \tld\ta1,16(sp)\n"
                            "  1e:\t6662                \tld\ta2,24(sp)\n"
                            "  20:\t02010113          \taddi\tsp,sp,32\n"
                            "  24:\t8f7e                \tmv\tt5,t6\n"
                            "  26:\t00af0463          \tbeq\tt5,a0,0x2e\n"
                            "  2a:\t857a                \tmv\ta0,t5\n"
                            "  2c:\t8082                \tret"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end)
            ]
        end}.

move_to_cp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:move_to_cp(State0, {y_reg, 0}),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t05053f03          \tld\tt5,80(a0)\n"
            "   4:\t000f3f83          \tld\tt6,0(t5)\n"
            "   8:\t0ff53023          \tsd\tt6,224(a0)"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

increment_sp_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:increment_sp(State0, 7),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t05053f83          \tld\tt6,80(a0)\n"
            "   4:\t038f8f93          \taddi\tt6,t6,56\n"
            "   8:\t05f53823          \tsd\tt6,80(a0)"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

if_block_test_() ->
    {setup,
        fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, RegB} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State2, RegA, RegB}
        end,
        fun({State0, RegA, RegB}) ->
            [
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t000fd363          \tbgez\tt6,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t01efd363          \tbge\tt6,t5,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t02a00e93          \tli\tt4,42\n"
                        "   c:\t01dfd363          \tbge\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '<', 1024},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t40000e93          \tli\tt4,1024\n"
                        "   c:\t01dfd363          \tbge\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2\n"
                        "  12:\ta0fd                \tj\t0x100"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t000f9363          \tbnez\tt6,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t000f9363          \tbnez\tt6,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', -1},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t5efd                \tli\tt4,-1\n"
                        "   a:\t01df9363          \tbne\tt6,t4,0x10\n"
                        "   e:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t000f9363          \tbnez\tt6,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '==', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t000f9363          \tbnez\tt6,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03b00e93          \tli\tt4,59\n"
                        "   c:\t01df8363          \tbeq\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '!=', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03b00e93          \tli\tt4,59\n"
                        "   c:\t01df8363          \tbeq\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '!=', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t02a00e93          \tli\tt4,42\n"
                        "   c:\t01df8363          \tbeq\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    % Test large immediate (1995) that requires temporary register
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '!=', 1995},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 1)
                        end
                    ),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t7cb00e93          \tli\tt4,1995\n"
                        "   c:\t01df8363          \tbeq\tt6,t4,0x12\n"
                        "  10:\t0f05                \taddi\tt5,t5,1\n"
                        "  12:\ta0fd                \tj\t0x100"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '!=', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t02a00e93          \tli\tt4,42\n"
                        "   c:\t01df8363          \tbeq\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '==', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03b00e93          \tli\tt4,59\n"
                        "   c:\t01df9363          \tbne\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '==', ?TERM_NIL},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03b00e93          \tli\tt4,59\n"
                        "   c:\t01df9363          \tbne\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', RegA, '==', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t02a00e93          \tli\tt4,42\n"
                        "   c:\t01df9363          \tbne\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(int)', {free, RegA}, '==', 42},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t02a00e93          \tli\tt4,42\n"
                        "   c:\t01df9363          \tbne\tt6,t4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '==', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03ff9e93          \tslli\tt4,t6,0x3f\n"
                        "   c:\t000ec363          \tbltz\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '==', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03ff9e93          \tslli\tt4,t6,0x3f\n"
                        "   c:\t000ec363          \tbltz\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', RegA, '!=', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03ff9e93          \tslli\tt4,t6,0x3f\n"
                        "   c:\t000ed363          \tbgez\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {'(bool)', {free, RegA}, '!=', false},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03ff9e93          \tslli\tt4,t6,0x3f\n"
                        "   c:\t000ed363          \tbgez\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#7, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t007ffe93          \tandi\tt4,t6,7\n"
                        "   c:\t000e8363          \tbeqz\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#5, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t005ffe93          \tandi\tt4,t6,5\n"
                        "   c:\t000e8363          \tbeqz\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '&', 16#7, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t007ffe93          \tandi\tt4,t6,7\n"
                        "   c:\t000e8363          \tbeqz\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\tffffce93          \tnot\tt4,t6\n"
                        "   c:\t1ef2                \tslli\tt4,t4,0x3c\n"
                        "   e:\t000e8363          \tbeqz\tt4,0x14\n"
                        "  12:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\tffffcf93          \tnot\tt6,t6\n"
                        "   c:\t1ff2                \tslli\tt6,t6,0x3c\n"
                        "   e:\t000f8363          \tbeqz\tt6,0x14\n"
                        "  12:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t8efe                \tmv\tt4,t6\n"
                        "   a:\t03fefe93          \tandi\tt4,t4,63\n"
                        "   e:\t4e21                \tli\tt3,8\n"
                        "  10:\t01ce8363          \tbeq\tt4,t3,0x16\n"
                        "  14:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {{free, RegA}, '<', RegB},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t01efd363          \tbge\tt6,t5,0xe\n"
                        "   c:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {
                            {free, RegA},
                            '&',
                            ?TERM_BOXED_TAG_MASK,
                            '!=',
                            ?TERM_BOXED_POSITIVE_INTEGER
                        },
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t03ffff93          \tandi\tt6,t6,63\n"
                        "   c:\t4ea1                \tli\tt4,8\n"
                        "   e:\t01df8363          \tbeq\tt6,t4,0x14\n"
                        "  12:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                %% Test {RegA, '&', 16#3, '!=', 0} using ANDI instruction
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {RegA, '&', 16#3, '!=', 0},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t003ffe93          \tandi\tt4,t6,3\n"
                        "   c:\t000e8363          \tbeqz\tt4,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {100, '<', RegA},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t06400e93          \tli\tt4,100\n"
                        "   c:\t01fed363          \tbge\tt4,t6,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {100, '<', {free, RegA}},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t06400e93          \tli\tt4,100\n"
                        "   c:\t01fed363          \tbge\tt4,t6,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {1024, '<', RegA},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t40000e93          \tli\tt4,1024\n"
                        "   c:\t01fed363          \tbge\tt4,t6,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB, RegA]), lists:sort(?BACKEND:used_regs(State1)))
                end),
                ?_test(begin
                    State1 = ?BACKEND:if_block(
                        State0,
                        {1024, '<', {free, RegA}},
                        fun(BSt0) ->
                            ?BACKEND:add(BSt0, RegB, 2)
                        end
                    ),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t06053f03          \tld\tt5,96(a0)\n"
                        "   8:\t40000e93          \tli\tt4,1024\n"
                        "   c:\t01fed363          \tbge\tt4,t6,0x12\n"
                        "  10:\t0f09                \taddi\tt5,t5,2"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(lists:sort([RegB]), lists:sort(?BACKEND:used_regs(State1)))
                end)
            ]
        end}.

if_else_block_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg1} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg2} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    State3 = ?BACKEND:if_else_block(
        State2,
        {Reg1, '==', ?TERM_NIL},
        fun(BSt0) ->
            ?BACKEND:add(BSt0, Reg2, 2)
        end,
        fun(BSt0) ->
            ?BACKEND:add(BSt0, Reg2, 4)
        end
    ),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:\t05853f83          \tld\tt6,88(a0)\n"
            "   4:\t06053f03          \tld\tt5,96(a0)\n"
            "   8:\t03b00e93          \tli\tt4,59\n"
            "   c:\t01df9463          \tbne\tt6,t4,0x14\n"
            "  10:\t0f09                \taddi\tt5,t5,2\n"
            "  12:\ta011                \tj\t0x16\n"
            "  14:\t0f11                \taddi\tt5,t5,4"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

shift_right_test_() ->
    [
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, Reg} = ?BACKEND:shift_right(State1, {free, Reg}, 3),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:\t05853f83          \tld\tt6,88(a0)\n"
                    "   4:\t003fdf93          \tsrli\tt6,t6,0x3"
                >>,
            jit_tests_common:assert_stream(riscv64, Dump, Stream)
        end),
        ?_test(begin
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, OtherReg} = ?BACKEND:shift_right(State1, Reg, 3),
            ?assertNotEqual(OtherReg, Reg),
            Stream = ?BACKEND:stream(State2),
            Dump =
                <<
                    "   0:\t05853f83          \tld\tt6,88(a0)\n"
                    "   4:\t003fdf13          \tsrli\tt5,t6,0x3"
                >>,
            jit_tests_common:assert_stream(riscv64, Dump, Stream)
        end)
    ].

shift_left_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:shift_left(State1, Reg, 3),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:\t05853f83          \tld\tt6,88(a0)\n"
            "   4:\t0f8e                \tslli\tt6,t6,0x3"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_only_or_schedule_next_and_label_relocation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:\t00000697          \tauipc\ta3,0x0\n"
            "   4:\t04a68067          \tjr\t74(a3) # 0x4a\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t01068067          \tjr\t16(a3) # 0x18\n"
            "  10:\t00000697          \tauipc\ta3,0x0\n"
            "  14:\t03468067          \tjr\t52(a3) # 0x44\n"
            "  18:\t0105af83          \tlw\tt6,16(a1)\n"
            "  1c:\t1ffd                \taddi\tt6,t6,-1\n"
            "  1e:\t01f5a823          \tsw\tt6,16(a1)\n"
            "  22:\t000f8663          \tbeqz\tt6,0x2e\n"
            "  26:\ta839                \tj\t0x44\n"
            "  28:\t0001                \tnop\n"
            "  2a:\t00000013          \tnop\n"
            "  2e:\t00000f97          \tauipc\tt6,0x0\n"
            "  32:\t0fd9                \taddi\tt6,t6,22 # 0x44\n"
            "  34:\t0001                \tnop\n"
            "  36:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  3a:\t4fc1                \tli\tt6,16\n"
            "  3c:\t9fb2                \tadd\tt6,t6,a2\n"
            "  3e:\t000fbf83          \tld\tt6,0(t6)\n"
            "  42:\t8f82                \tjr\tt6\n"
            "  44:\t00063f83          \tld\tt6,0(a2)\n"
            "  48:\t8f82                \tjr\tt6\n"
            "  4a:\t4fa1                \tli\tt6,8\n"
            "  4c:\t9fb2                \tadd\tt6,t6,a2\n"
            "  4e:\t000fbf83          \tld\tt6,0(t6)\n"
            "  52:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_only_or_schedule_next_known_label_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:add_label(State2, 2, 16#36),
    State4 = ?BACKEND:call_only_or_schedule_next(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    Dump =
        <<
            "   0:\t00000697          \tauipc\ta3,0x0\n"
            "   4:\t04068067          \tjr\t64(a3) # 0x40\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t01068067          \tjr\t16(a3) # 0x18\n"
            "  10:\t00000697          \tauipc\ta3,0x0\n"
            "  14:\t02668067          \tjr\t38(a3) # 0x36\n"
            "  18:\t0105af83          \tlw\tt6,16(a1)\n"
            "  1c:\t1ffd                \taddi\tt6,t6,-1\n"
            "  1e:\t01f5a823          \tsw\tt6,16(a1)\n"
            "  22:\t000f9a63          \tbnez\tt6,0x36\n"
            "  26:\t00000f97          \tauipc\tt6,0x0\n"
            "  2a:\t0fc1                \taddi\tt6,t6,16 # 0x36\n"
            "  2c:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  30:\t4fc1                \tli\tt6,16\n"
            "  32:\t9fb2                \tadd\tt6,t6,a2\n"
            "  34:\t000fbf83          \tld\tt6,0(t6)\n"
            "  38:\t8f82                \tjr\tt6\n"
            "  3a:\t00063f83          \tld\tt6,0(a2)\n"
            "  3e:\t8f82                \tjr\tt6\n"
            "  40:\t4fa1                \tli\tt6,8\n"
            "  42:\t9fb2                \tadd\tt6,t6,a2\n"
            "  44:\t000fbf83          \tld\tt6,0(t6)\n"
            "  48:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test with large gap (256+ bytes) to force mov_immediate path
call_only_or_schedule_next_and_label_relocation_large_gap_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),
    % Add large padding by emitting many move_to_native_register operations
    % This creates a large gap between the jump table and the actual function bodies
    % Each operation emits ~2 bytes, so 128 operations = ~256 bytes
    StatePadded = lists:foldl(
        fun(_, S) ->
            ?BACKEND:move_to_native_register(S, {x_reg, 2}, a3)
        end,
        State1,
        lists:seq(1, 128)
    ),
    State2 = ?BACKEND:add_label(StatePadded, 1),
    State3 = ?BACKEND:call_only_or_schedule_next(State2, 2),
    State4 = ?BACKEND:add_label(State3, 2),
    State5 = ?BACKEND:call_primitive_last(State4, 0, [ctx, jit_state]),
    % OP_INT_CALL_END
    State6 = ?BACKEND:add_label(State5, 0),
    State7 = ?BACKEND:call_primitive_last(State6, 1, [ctx, jit_state]),
    State8 = ?BACKEND:update_branches(State7),
    Stream = ?BACKEND:stream(State8),
    % Extract the final section starting at 0x118 (after jump table 24 bytes + 128 loads 256 bytes)
    % RISC-V: Jump table is 3×8=24 bytes, loads are 2 bytes each (compressed)
    Dump = <<
        "   0:\t0105af83          \tlw\tt6,16(a1)\n"
        "   4:\t1ffd                \taddi\tt6,t6,-1\n"
        "   6:\t01f5a823          \tsw\tt6,16(a1)\n"
        "   a:\t000f8663          \tbeqz\tt6,0x16\n"
        "   e:\ta839                \tj\t0x2c\n"
        "  10:\t0001                \tnop\n"
        "  12:\t00000013          \tnop\n"
        "  16:\t00000f97          \tauipc\tt6,0x0\n"
        "  1a:\t0fd9                \taddi\tt6,t6,22 # 0x2c\n"
        "  1c:\t0001                \tnop\n"
        "  1e:\t01f5b423          \tsd\tt6,8(a1)\n"
        "  22:\t4fc1                \tli\tt6,16\n"
        "  24:\t9fb2                \tadd\tt6,t6,a2\n"
        "  26:\t000fbf83          \tld\tt6,0(t6)\n"
        "  2a:\t8f82                \tjr\tt6\n"
        "  2c:\t00063f83          \tld\tt6,0(a2)\n"
        "  30:\t8f82                \tjr\tt6\n"
        "  32:\t4fa1                \tli\tt6,8\n"
        "  34:\t9fb2                \tadd\tt6,t6,a2\n"
        "  36:\t000fbf83          \tld\tt6,0(t6)\n"
        "  3a:\t8f82                \tjr\tt6"
    >>,
    {_, RelevantBinary} = split_binary(Stream, 16#118),
    jit_tests_common:assert_stream(riscv64, Dump, RelevantBinary).

call_bif_with_large_literal_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, 8, [jit_state, 2]),
    {State2, ArgReg} = ?BACKEND:call_primitive(State1, 15, [ctx, 998238357]),
    {State3, ResultReg} = ?BACKEND:call_func_ptr(State2, {free, FuncPtr}, [
        ctx, 0, 1, {free, {x_reg, 0}}, {free, ArgReg}
    ]),
    State4 = ?BACKEND:if_block(State3, {ResultReg, '==', 0}, fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    State5 = ?BACKEND:move_to_vm_register(State4, ResultReg, {x_reg, 0}),
    State6 = ?BACKEND:free_native_registers(State5, [ResultReg]),
    ?BACKEND:assert_all_native_free(State6),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\t04000f93          \tli\tt6,64\n"
            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
            "   a:\t1101                \taddi\tsp,sp,-32\n"
            "   c:\te006                \tsd\tra,0(sp)\n"
            "   e:\te42a                \tsd\ta0,8(sp)\n"
            "  10:\te82e                \tsd\ta1,16(sp)\n"
            "  12:\tec32                \tsd\ta2,24(sp)\n"
            "  14:\t852e                \tmv\ta0,a1\n"
            "  16:\t4589                \tli\ta1,2\n"
            "  18:\t9f82                \tjalr\tt6\n"
            "  1a:\t8faa                \tmv\tt6,a0\n"
            "  1c:\t6082                \tld\tra,0(sp)\n"
            "  1e:\t6522                \tld\ta0,8(sp)\n"
            "  20:\t65c2                \tld\ta1,16(sp)\n"
            "  22:\t6662                \tld\ta2,24(sp)\n"
            "  24:\t02010113          \taddi\tsp,sp,32\n"
            "  28:\t07800f13          \tli\tt5,120\n"
            "  2c:\t9f32                \tadd\tt5,t5,a2\n"
            "  2e:\t000f3f03          \tld\tt5,0(t5)\n"
            "  32:\tfd010113          \taddi\tsp,sp,-48\n"
            "  36:\te006                \tsd\tra,0(sp)\n"
            "  38:\te42a                \tsd\ta0,8(sp)\n"
            "  3a:\te82e                \tsd\ta1,16(sp)\n"
            "  3c:\tec32                \tsd\ta2,24(sp)\n"
            "  3e:\tf07e                \tsd\tt6,32(sp)\n"
            "  40:\t3b7ff5b7          \tlui\ta1,0x3b7ff\n"
            "  44:\t89558593          \taddi\ta1,a1,-1899 # 0x3b7fe895\n"
            "  48:\t9f02                \tjalr\tt5\n"
            "  4a:\t8f2a                \tmv\tt5,a0\n"
            "  4c:\t6082                \tld\tra,0(sp)\n"
            "  4e:\t6522                \tld\ta0,8(sp)\n"
            "  50:\t65c2                \tld\ta1,16(sp)\n"
            "  52:\t6662                \tld\ta2,24(sp)\n"
            "  54:\t7f82                \tld\tt6,32(sp)\n"
            "  56:\t03010113          \taddi\tsp,sp,48\n"
            "  5a:\t1101                \taddi\tsp,sp,-32\n"
            "  5c:\te006                \tsd\tra,0(sp)\n"
            "  5e:\te42a                \tsd\ta0,8(sp)\n"
            "  60:\te82e                \tsd\ta1,16(sp)\n"
            "  62:\tec32                \tsd\ta2,24(sp)\n"
            "  64:\t4581                \tli\ta1,0\n"
            "  66:\t4605                \tli\ta2,1\n"
            "  68:\t6d34                \tld\ta3,88(a0)\n"
            "  6a:\t877a                \tmv\ta4,t5\n"
            "  6c:\t9f82                \tjalr\tt6\n"
            "  6e:\t8faa                \tmv\tt6,a0\n"
            "  70:\t6082                \tld\tra,0(sp)\n"
            "  72:\t6522                \tld\ta0,8(sp)\n"
            "  74:\t65c2                \tld\ta1,16(sp)\n"
            "  76:\t6662                \tld\ta2,24(sp)\n"
            "  78:\t02010113          \taddi\tsp,sp,32\n"
            "  7c:\t000f9a63          \tbnez\tt6,0x90\n"
            "  80:\t03000f93          \tli\tt6,48\n"
            "  84:\t9fb2                \tadd\tt6,t6,a2\n"
            "  86:\t000fbf83          \tld\tt6,0(t6)\n"
            "  8a:\t08a00613          \tli\ta2,138\n"
            "  8e:\t8f82                \tjr\tt6\n"
            "  90:\t05f53c23          \tsd\tt6,88(a0)"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

get_list_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, Reg} = ?BACKEND:and_(State1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
    State3 = ?BACKEND:move_array_element(State2, Reg, 1, {y_reg, 1}),
    State4 = ?BACKEND:move_array_element(State3, Reg, 0, {y_reg, 0}),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:\t05853f83          \tld\tt6,88(a0)\n"
            "   4:\tffcfff93          \tandi\tt6,t6,-4\n"
            "   8:\t008fbe83          \tld\tt4,8(t6)\n"
            "   c:\t05053f03          \tld\tt5,80(a0)\n"
            "  10:\t01df3423          \tsd\tt4,8(t5)\n"
            "  14:\t000fbe83          \tld\tt4,0(t6)\n"
            "  18:\t05053f03          \tld\tt5,80(a0)\n"
            "  1c:\t01df3023          \tsd\tt4,0(t5)"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

is_integer_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    Arg1 = {x_reg, 0},
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, Arg1),
    State3 = ?BACKEND:if_block(
        State2, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(MSt0) ->
            MSt1 = ?BACKEND:if_block(
                MSt0, {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            ),
            {MSt2, Reg} = ?BACKEND:and_(MSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
            MSt3 = ?BACKEND:move_array_element(MSt2, Reg, 0, Reg),
            ?BACKEND:if_block(
                MSt3,
                {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                fun(BSt0) ->
                    ?BACKEND:jump_to_label(BSt0, Label)
                end
            )
        end
    ),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\tffff                \t.insn\t2, 0xffff\n"
            "   2:\tffff                \t.insn\t2, 0xffff\n"
            "   4:\tffff                \t.insn\t2, 0xffff\n"
            "   6:\tffff                \t.insn\t2, 0xffff\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t0f868067          \tjr\t248(a3) # 0x100\n"
            "  10:\t05853f83          \tld\tt6,88(a0)\n"
            "  14:\tffffcf13          \tnot\tt5,t6\n"
            "  18:\t1f72                \tslli\tt5,t5,0x3c\n"
            "  1a:\t020f0963          \tbeqz\tt5,0x4c\n"
            "  1e:\t8f7e                \tmv\tt5,t6\n"
            "  20:\t003f7f13          \tandi\tt5,t5,3\n"
            "  24:\t4e89                \tli\tt4,2\n"
            "  26:\t01df0663          \tbeq\tt5,t4,0x32\n"
            "  2a:\ta8d9                \tj\t0x100\n"
            "  2c:\t0001                \tnop\n"
            "  2e:\t00000013          \tnop\n"
            "  32:\tffcfff93          \tandi\tt6,t6,-4\n"
            "  36:\t000fbf83          \tld\tt6,0(t6)\n"
            "  3a:\t03ffff93          \tandi\tt6,t6,63\n"
            "  3e:\t4f21                \tli\tt5,8\n"
            "  40:\t01ef8663          \tbeq\tt6,t5,0x4c\n"
            "  44:\ta875                \tj\t0x100\n"
            "  46:\t0001                \tnop\n"
            "  48:\t00000013          \tnop"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

cond_jump_to_label(Cond, Label, MMod, MSt0) ->
    MMod:if_block(MSt0, Cond, fun(BSt0) ->
        MMod:jump_to_label(BSt0, Label)
    end).

%% Keep the unoptimized version to test the and case.
is_number_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    Arg1 = {x_reg, 0},
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, Arg1),
    State3 = ?BACKEND:if_block(
        State2, {Reg, '&', ?TERM_IMMED_TAG_MASK, '!=', ?TERM_INTEGER_TAG}, fun(BSt0) ->
            BSt1 = cond_jump_to_label(
                {Reg, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, Label, ?BACKEND, BSt0
            ),
            {BSt2, Reg} = ?BACKEND:and_(BSt1, {free, Reg}, ?TERM_PRIMARY_CLEAR_MASK),
            BSt3 = ?BACKEND:move_array_element(BSt2, Reg, 0, Reg),
            cond_jump_to_label(
                {'and', [
                    {Reg, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_POSITIVE_INTEGER},
                    {{free, Reg}, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FLOAT}
                ]},
                Label,
                ?BACKEND,
                BSt3
            )
        end
    ),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\tffff                \t.insn\t2, 0xffff\n"
            "   2:\tffff                \t.insn\t2, 0xffff\n"
            "   4:\tffff                \t.insn\t2, 0xffff\n"
            "   6:\tffff                \t.insn\t2, 0xffff\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t0f868067          \tjr\t248(a3) # 0x100\n"
            "  10:\t05853f83          \tld\tt6,88(a0)\n"
            "  14:\tffffcf13          \tnot\tt5,t6\n"
            "  18:\t1f72                \tslli\tt5,t5,0x3c\n"
            "  1a:\t020f0f63          \tbeqz\tt5,0x58\n"
            "  1e:\t8f7e                \tmv\tt5,t6\n"
            "  20:\t003f7f13          \tandi\tt5,t5,3\n"
            "  24:\t4e89                \tli\tt4,2\n"
            "  26:\t01df0663          \tbeq\tt5,t4,0x32\n"
            "  2a:\ta8d9                \tj\t0x100\n"
            "  2c:\t0001                \tnop\n"
            "  2e:\t00000013          \tnop\n"
            "  32:\tffcfff93          \tandi\tt6,t6,-4\n"
            "  36:\t000fbf83          \tld\tt6,0(t6)\n"
            "  3a:\t8f7e                \tmv\tt5,t6\n"
            "  3c:\t03ff7f13          \tandi\tt5,t5,63\n"
            "  40:\t4ea1                \tli\tt4,8\n"
            "  42:\t01df0b63          \tbeq\tt5,t4,0x58\n"
            "  46:\t03ffff93          \tandi\tt6,t6,63\n"
            "  4a:\t4f61                \tli\tt5,24\n"
            "  4c:\t01ef8663          \tbeq\tt6,t5,0x58\n"
            "  50:\ta845                \tj\t0x100\n"
            "  52:\t0001                \tnop\n"
            "  54:\t00000013          \tnop"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

is_boolean_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(State2, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump = <<
        "   0:\tffff                \t.insn\t2, 0xffff\n"
        "   2:\tffff                \t.insn\t2, 0xffff\n"
        "   4:\tffff                \t.insn\t2, 0xffff\n"
        "   6:\tffff                \t.insn\t2, 0xffff\n"
        "   8:\t00000697          \tauipc\ta3,0x0\n"
        "   c:\t0f868067          \tjr\t248(a3) # 0x100\n"
        "  10:\t05853f83          \tld\tt6,88(a0)\n"
        "  14:\t04b00f13          \tli\tt5,75\n"
        "  18:\t01ef8963          \tbeq\tt6,t5,0x2a\n"
        "  1c:\t4f2d                \tli\tt5,11\n"
        "  1e:\t01ef8663          \tbeq\tt6,t5,0x2a\n"
        "  22:\ta8f9                \tj\t0x100\n"
        "  24:\t0001                \tnop\n"
        "  26:\t00000013          \tnop"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

is_boolean_far_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    Label = 1,
    State1 = ?BACKEND:jump_table(State0, 1),
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, {x_reg, 0}),
    State3 = ?BACKEND:if_block(State2, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State4 = ?BACKEND:free_native_registers(State3, [Reg]),
    ?BACKEND:assert_all_native_free(State4),
    State5 = ?BACKEND:add_label(State4, Label, 16#1000),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\tffff                \t.insn\t2, 0xffff\n"
            "   2:\tffff                \t.insn\t2, 0xffff\n"
            "   4:\tffff                \t.insn\t2, 0xffff\n"
            "   6:\tffff                \t.insn\t2, 0xffff\n"
            "   8:\t00001697          \tauipc\ta3,0x1\n"
            "   c:\tff868067          \tjr\t-8(a3) # 0x1000\n"
            "  10:\t05853f83          \tld\tt6,88(a0)\n"
            "  14:\t04b00f13          \tli\tt5,75\n"
            "  18:\t01ef8963          \tbeq\tt6,t5,0x2a\n"
            "  1c:\t4f2d                \tli\tt5,11\n"
            "  1e:\t01ef8663          \tbeq\tt6,t5,0x2a\n"
            "  22:\t7df0006f          \tj\t0x1000\n"
            "  26:\t00000013          \tnop"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

is_boolean_far_known_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 1),
    Label = 1,
    State2 = ?BACKEND:add_label(State1, Label, 16#1000),
    {State3, Reg} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    State4 = ?BACKEND:if_block(State3, {Reg, '!=', ?TRUE_ATOM}, fun(BSt0) ->
        ?BACKEND:if_block(BSt0, {Reg, '!=', ?FALSE_ATOM}, fun(BSt1) ->
            ?BACKEND:jump_to_label(BSt1, Label)
        end)
    end),
    State5 = ?BACKEND:free_native_registers(State4, [Reg]),
    ?BACKEND:assert_all_native_free(State5),
    State6 = ?BACKEND:update_branches(State5),
    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\tffff                \t.insn\t2, 0xffff\n"
            "   2:\tffff                \t.insn\t2, 0xffff\n"
            "   4:\tffff                \t.insn\t2, 0xffff\n"
            "   6:\tffff                \t.insn\t2, 0xffff\n"
            "   8:\t00001697          \tauipc\ta3,0x1\n"
            "   c:\tff868067          \tjr\t-8(a3) # 0x1000\n"
            "  10:\t05853f83          \tld\tt6,88(a0)\n"
            "  14:\t04b00f13          \tli\tt5,75\n"
            "  18:\t01ef8963          \tbeq\tt6,t5,0x2a\n"
            "  1c:\t4f2d                \tli\tt5,11\n"
            "  1e:\t01ef8663          \tbeq\tt6,t5,0x2a\n"
            "  22:\t00001f17          \tauipc\tt5,0x1\n"
            "  26:\tfdef0067          \tjr\t-34(t5) # 0x1000"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test OP_WAIT_TIMEOUT pattern that uses set_continuation_to_offset and continuation_entry_point
wait_timeout_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    Label = 42,
    {State1, OffsetRef0} = ?BACKEND:set_continuation_to_offset(State0),
    {State2, TimeoutReg} = ?BACKEND:move_to_native_register(State1, 5000),
    State3 = ?BACKEND:call_primitive_last(State2, ?PRIM_WAIT_TIMEOUT, [
        ctx, jit_state, {free, TimeoutReg}, Label
    ]),
    State4 = ?BACKEND:add_label(State3, OffsetRef0),
    State5 = ?BACKEND:continuation_entry_point(State4),
    {State6, ResultReg0} = ?BACKEND:call_primitive(State5, ?PRIM_PROCESS_SIGNAL_MESSAGES, [
        ctx, jit_state
    ]),
    State7 = ?BACKEND:return_if_not_equal_to_ctx(State6, {free, ResultReg0}),
    % ?WAITING_TIMEOUT_EXPIRED
    {State8, ResultReg1} = ?BACKEND:call_primitive(State7, ?PRIM_CONTEXT_GET_FLAGS, [ctx, 2]),
    State9 = ?BACKEND:if_block(State8, {{free, ResultReg1}, '==', 0}, fun(BlockSt) ->
        ?BACKEND:call_primitive_last(BlockSt, ?PRIM_WAIT_TIMEOUT_TRAP_HANDLER, [
            ctx, jit_state, Label
        ])
    end),
    State10 = ?BACKEND:update_branches(State9),

    Stream = ?BACKEND:stream(State10),
    Dump =
        <<
            "   0:\t00000f97          \tauipc\tt6,0x0\n"
            "   4:\t024f8f93          \taddi\tt6,t6,36 # 0x24\n"
            "   8:\t01f5b423          \tsd\tt6,8(a1)\n"
            "   c:\t6f85                \tlui\tt6,0x1\n"
            "   e:\t388f8f93          \taddi\tt6,t6,904 # 0x1388\n"
            "  12:\t0f000f13          \tli\tt5,240\n"
            "  16:\t9f32                \tadd\tt5,t5,a2\n"
            "  18:\t000f3f03          \tld\tt5,0(t5)\n"
            "  1c:\t867e                \tmv\ta2,t6\n"
            "  1e:\t02a00693          \tli\ta3,42\n"
            "  22:\t8f02                \tjr\tt5\n"
            "  24:\t0a800f93          \tli\tt6,168\n"
            "  28:\t9fb2                \tadd\tt6,t6,a2\n"
            "  2a:\t000fbf83          \tld\tt6,0(t6)\n"
            "  2e:\t1101                \taddi\tsp,sp,-32\n"
            "  30:\te006                \tsd\tra,0(sp)\n"
            "  32:\te42a                \tsd\ta0,8(sp)\n"
            "  34:\te82e                \tsd\ta1,16(sp)\n"
            "  36:\tec32                \tsd\ta2,24(sp)\n"
            "  38:\t9f82                \tjalr\tt6\n"
            "  3a:\t8faa                \tmv\tt6,a0\n"
            "  3c:\t6082                \tld\tra,0(sp)\n"
            "  3e:\t6522                \tld\ta0,8(sp)\n"
            "  40:\t65c2                \tld\ta1,16(sp)\n"
            "  42:\t6662                \tld\ta2,24(sp)\n"
            "  44:\t02010113          \taddi\tsp,sp,32\n"
            "  48:\t00af8463          \tbeq\tt6,a0,0x50\n"
            "  4c:\t857e                \tmv\ta0,t6\n"
            "  4e:\t8082                \tret\n"
            "  50:\t02100f93          \tli\tt6,33\n"
            "  54:\t0f8e                \tslli\tt6,t6,0x3\n"
            "  56:\t9fb2                \tadd\tt6,t6,a2\n"
            "  58:\t000fbf83          \tld\tt6,0(t6)\n"
            "  5c:\t1101                \taddi\tsp,sp,-32\n"
            "  5e:\te006                \tsd\tra,0(sp)\n"
            "  60:\te42a                \tsd\ta0,8(sp)\n"
            "  62:\te82e                \tsd\ta1,16(sp)\n"
            "  64:\tec32                \tsd\ta2,24(sp)\n"
            "  66:\t4589                \tli\ta1,2\n"
            "  68:\t9f82                \tjalr\tt6\n"
            "  6a:\t8faa                \tmv\tt6,a0\n"
            "  6c:\t6082                \tld\tra,0(sp)\n"
            "  6e:\t6522                \tld\ta0,8(sp)\n"
            "  70:\t65c2                \tld\ta1,16(sp)\n"
            "  72:\t6662                \tld\ta2,24(sp)\n"
            "  74:\t02010113          \taddi\tsp,sp,32\n"
            "  78:\t000f9a63          \tbnez\tt6,0x8c\n"
            "  7c:\t0f800f93          \tli\tt6,248\n"
            "  80:\t9fb2                \tadd\tt6,t6,a2\n"
            "  82:\t000fbf83          \tld\tt6,0(t6)\n"
            "  86:\t02a00613          \tli\ta2,42\n"
            "  8a:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test OP_WAIT pattern that uses set_continuation_to_label
wait_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:set_continuation_to_label(State2, Label),
    State4 = ?BACKEND:call_primitive_last(State3, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    State5 = ?BACKEND:add_label(State4, Label, 16#100),
    State6 = ?BACKEND:update_branches(State5),

    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\tffff                \t.insn\t2, 0xffff\n"
            "   2:\tffff                \t.insn\t2, 0xffff\n"
            "   4:\tffff                \t.insn\t2, 0xffff\n"
            "   6:\tffff                \t.insn\t2, 0xffff\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t02868067          \tjr\t40(a3) # 0x30\n"
            "  10:\t00000697          \tauipc\ta3,0x0\n"
            "  14:\t0f068067          \tjr\t240(a3) # 0x100\n"
            "  18:\tffff                \t.insn\t2, 0xffff\n"
            "  1a:\tffff                \t.insn\t2, 0xffff\n"
            "  1c:\tffff                \t.insn\t2, 0xffff\n"
            "  1e:\tffff                \t.insn\t2, 0xffff\n"
            "  20:\tffff                \t.insn\t2, 0xffff\n"
            "  22:\tffff                \t.insn\t2, 0xffff\n"
            "  24:\tffff                \t.insn\t2, 0xffff\n"
            "  26:\tffff                \t.insn\t2, 0xffff\n"
            "  28:\tffff                \t.insn\t2, 0xffff\n"
            "  2a:\tffff                \t.insn\t2, 0xffff\n"
            "  2c:\tffff                \t.insn\t2, 0xffff\n"
            "  2e:\tffff                \t.insn\t2, 0xffff\n"
            "  30:\t00000f97          \tauipc\tt6,0x0\n"
            "  34:\t0d0f8f93          \taddi\tt6,t6,208 # 0x100\n"
            "  38:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  3c:\t0e800f93          \tli\tt6,232\n"
            "  40:\t9fb2                \tadd\tt6,t6,a2\n"
            "  42:\t000fbf83          \tld\tt6,0(t6)\n"
            "  46:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test set_continuation_to_label with known label
wait_known_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    State1 = ?BACKEND:jump_table(State0, 5),
    State2 = ?BACKEND:add_label(State1, 1),
    Label = 2,
    State3 = ?BACKEND:add_label(State2, Label, 16#100),
    State4 = ?BACKEND:set_continuation_to_label(State3, Label),
    State5 = ?BACKEND:call_primitive_last(State4, ?PRIM_SCHEDULE_WAIT_CP, [ctx, jit_state]),
    State6 = ?BACKEND:update_branches(State5),

    Stream = ?BACKEND:stream(State6),
    Dump =
        <<
            "   0:\tffff                \t.insn\t2, 0xffff\n"
            "   2:\tffff                \t.insn\t2, 0xffff\n"
            "   4:\tffff                \t.insn\t2, 0xffff\n"
            "   6:\tffff                \t.insn\t2, 0xffff\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t02868067          \tjr\t40(a3) # 0x30\n"
            "  10:\t00000697          \tauipc\ta3,0x0\n"
            "  14:\t0f068067          \tjr\t240(a3) # 0x100\n"
            "  18:\tffff                \t.insn\t2, 0xffff\n"
            "  1a:\tffff                \t.insn\t2, 0xffff\n"
            "  1c:\tffff                \t.insn\t2, 0xffff\n"
            "  1e:\tffff                \t.insn\t2, 0xffff\n"
            "  20:\tffff                \t.insn\t2, 0xffff\n"
            "  22:\tffff                \t.insn\t2, 0xffff\n"
            "  24:\tffff                \t.insn\t2, 0xffff\n"
            "  26:\tffff                \t.insn\t2, 0xffff\n"
            "  28:\tffff                \t.insn\t2, 0xffff\n"
            "  2a:\tffff                \t.insn\t2, 0xffff\n"
            "  2c:\tffff                \t.insn\t2, 0xffff\n"
            "  2e:\tffff                \t.insn\t2, 0xffff\n"
            "  30:\t00000f97          \tauipc\tt6,0x0\n"
            "  34:\t0d0f8f93          \taddi\tt6,t6,208 # 0x100\n"
            "  38:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  3c:\t0e800f93          \tli\tt6,232\n"
            "  40:\t9fb2                \tadd\tt6,t6,a2\n"
            "  42:\t000fbf83          \tld\tt6,0(t6)\n"
            "  46:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test return_labels_and_lines/2 function
return_labels_and_lines_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 2),

    % Test return_labels_and_lines with some sample labels and lines
    State2 = ?BACKEND:add_label(State1, 2, 32),
    State3 = ?BACKEND:add_label(State2, 1, 16),

    % {Line, Offset} pairs
    SortedLines = [{10, 16}, {20, 32}],

    State4 = ?BACKEND:return_labels_and_lines(State3, SortedLines),
    Stream = ?BACKEND:stream(State4),

    % Should have jump table + generated code with label/line tables
    ?assert(byte_size(Stream) >= 32),

    % Expected: jump table (3 entries, 24 bytes) + auipc + addi + ret + padding + labels table + lines table
    Dump =
        <<
            "   0:  ffff                .insn   2, 0xffff\n"
            "   2:  ffff                .insn   2, 0xffff\n"
            "   4:  ffff                .insn   2, 0xffff\n"
            "   6:  ffff                .insn   2, 0xffff\n"
            "   8:  00000697            auipc   a3,0x0\n"
            "   c:  00868067            jr  8(a3) # 0x10\n"
            "  10:  00000697            auipc   a3,0x0\n"
            "  14:  01068067            jr  16(a3) # 0x20\n"
            "  18:  00000517            auipc   a0,0x0\n"
            "  1c:  0529                addi    a0,a0,10 # 0x22\n"
            "  1e:  8082                ret\n"
            "  20:  ffff                .insn   2, 0xffff\n"
            "  22:  0200                addi    s0,sp,256\n"
            "  24:  0100                addi    s0,sp,128\n"
            "  26:  0000                unimp\n"
            "  28:  1000                addi    s0,sp,32\n"
            "  2a:  0200                addi    s0,sp,256\n"
            "  2c:  0000                unimp\n"
            "  2e:  2000                fld fs0,0(s0)\n"
            "  30:  0200                addi    s0,sp,256\n"
            "  32:  0a00                addi    s0,sp,272\n"
            "  34:  0000                unimp\n"
            "  36:  1000                addi    s0,sp,32\n"
            "  38:  1400                addi    s0,sp,544\n"
            "  3a:  0000                unimp\n"
            "  3c:  2000                fld fs0,0(s0)"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test call_primitive with {free, {x_reg, X}}
gc_bif2_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_GET_IMPORTED_BIF, [jit_state, 42]),
    {State2, _ResultReg} = ?BACKEND:call_func_ptr(State1, {free, FuncPtr}, [
        ctx, 0, 3, {y_reg, 0}, {free, {x_reg, 0}}
    ]),

    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:\t04000f93          \tli\tt6,64\n"
            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
            "   a:\t1101                \taddi\tsp,sp,-32\n"
            "   c:\te006                \tsd\tra,0(sp)\n"
            "   e:\te42a                \tsd\ta0,8(sp)\n"
            "  10:\te82e                \tsd\ta1,16(sp)\n"
            "  12:\tec32                \tsd\ta2,24(sp)\n"
            "  14:\t852e                \tmv\ta0,a1\n"
            "  16:\t02a00593          \tli\ta1,42\n"
            "  1a:\t9f82                \tjalr\tt6\n"
            "  1c:\t8faa                \tmv\tt6,a0\n"
            "  1e:\t6082                \tld\tra,0(sp)\n"
            "  20:\t6522                \tld\ta0,8(sp)\n"
            "  22:\t65c2                \tld\ta1,16(sp)\n"
            "  24:\t6662                \tld\ta2,24(sp)\n"
            "  26:\t02010113          \taddi\tsp,sp,32\n"
            "  2a:\t1101                \taddi\tsp,sp,-32\n"
            "  2c:\te006                \tsd\tra,0(sp)\n"
            "  2e:\te42a                \tsd\ta0,8(sp)\n"
            "  30:\te82e                \tsd\ta1,16(sp)\n"
            "  32:\tec32                \tsd\ta2,24(sp)\n"
            "  34:\t4581                \tli\ta1,0\n"
            "  36:\t460d                \tli\ta2,3\n"
            "  38:\t05053f03          \tld\tt5,80(a0)\n"
            "  3c:\t000f3683          \tld\ta3,0(t5)\n"
            "  40:\t6d38                \tld\ta4,88(a0)\n"
            "  42:\t9f82                \tjalr\tt6\n"
            "  44:\t8faa                \tmv\tt6,a0\n"
            "  46:\t6082                \tld\tra,0(sp)\n"
            "  48:\t6522                \tld\ta0,8(sp)\n"
            "  4a:\t65c2                \tld\ta1,16(sp)\n"
            "  4c:\t6662                \tld\ta2,24(sp)\n"
            "  4e:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test case where parameter value is in a1
memory_ensure_free_with_roots_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, _FuncPtr} = ?BACKEND:call_primitive(State0, ?PRIM_MEMORY_ENSURE_FREE_WITH_ROOTS, [
        ctx, jit_state, {free, a1}, 4, 1
    ]),

    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t02c00f93          \tli\tt6,44\n"
            "   4:\t0f8e                \tslli\tt6,t6,0x3\n"
            "   6:\t9fb2                \tadd\tt6,t6,a2\n"
            "   8:\t000fbf83          \tld\tt6,0(t6)\n"
            "   c:\t1101                \taddi\tsp,sp,-32\n"
            "   e:\te006                \tsd\tra,0(sp)\n"
            "  10:\te42a                \tsd\ta0,8(sp)\n"
            "  12:\te82e                \tsd\ta1,16(sp)\n"
            "  14:\tec32                \tsd\ta2,24(sp)\n"
            "  16:\t8f2e                \tmv\tt5,a1\n"
            "  18:\t867a                \tmv\ta2,t5\n"
            "  1a:\t4691                \tli\ta3,4\n"
            "  1c:\t4705                \tli\ta4,1\n"
            "  1e:\t9f82                \tjalr\tt6\n"
            "  20:\t8faa                \tmv\tt6,a0\n"
            "  22:\t6082                \tld\tra,0(sp)\n"
            "  24:\t6522                \tld\ta0,8(sp)\n"
            "  26:\t65c2                \tld\ta1,16(sp)\n"
            "  28:\t6662                \tld\ta2,24(sp)\n"
            "  2a:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_ext_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    State2 = ?BACKEND:call_primitive_with_cp(State1, 4, [ctx, jit_state, 2, 5, -1]),
    ?BACKEND:assert_all_native_free(State2),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:\t0105af83          \tlw\tt6,16(a1)\n"
            "   4:\t1ffd                \taddi\tt6,t6,-1\n"
            "   6:\t01f5a823          \tsw\tt6,16(a1)\n"
            "   a:\t000f9d63          \tbnez\tt6,0x24\n"
            "   e:\t00000f97          \tauipc\tt6,0x0\n"
            "  12:\t0fd9                \taddi\tt6,t6,22 # 0x24\n"
            "  14:\t0001                \tnop\n"
            "  16:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  1a:\t4fc1                \tli\tt6,16\n"
            "  1c:\t9fb2                \tadd\tt6,t6,a2\n"
            "  1e:\t000fbf83          \tld\tt6,0(t6)\n"
            "  22:\t8f82                \tjr\tt6\n"
            "  24:\t0005bf03          \tld\tt5,0(a1)\n"
            "  28:\t000f2f03          \tlw\tt5,0(t5)\n"
            "  2c:\t0f62                \tslli\tt5,t5,0x18\n"
            "  2e:\t14000f93          \tli\tt6,320\n"
            "  32:\t00000013          \tnop\n"
            "  36:\t01ff6f33          \tor\tt5,t5,t6\n"
            "  3a:\t0fe53023          \tsd\tt5,224(a0)\n"
            "  3e:\t02000f93          \tli\tt6,32\n"
            "  42:\t9fb2                \tadd\tt6,t6,a2\n"
            "  44:\t000fbf83          \tld\tt6,0(t6)\n"
            "  48:\t4609                \tli\ta2,2\n"
            "  4a:\t4695                \tli\ta3,5\n"
            "  4c:\t577d                \tli\ta4,-1\n"
            "  4e:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

call_fun_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:decrement_reductions_and_maybe_schedule_next(State0),
    FuncReg = {x_reg, 0},
    ArgsCount = 0,
    {State2, Reg} = ?BACKEND:move_to_native_register(State1, FuncReg),
    {State3, RegCopy} = ?BACKEND:copy_to_native_register(State2, Reg),
    State4 = ?BACKEND:if_block(
        State3, {RegCopy, '&', ?TERM_PRIMARY_MASK, '!=', ?TERM_PRIMARY_BOXED}, fun(BSt0) ->
            ?BACKEND:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, RegCopy
            ])
        end
    ),
    {State5, RegCopy} = ?BACKEND:and_(State4, {free, RegCopy}, ?TERM_PRIMARY_CLEAR_MASK),
    State6 = ?BACKEND:move_array_element(State5, RegCopy, 0, RegCopy),
    State7 = ?BACKEND:if_block(
        State6, {RegCopy, '&', ?TERM_BOXED_TAG_MASK, '!=', ?TERM_BOXED_FUN}, fun(BSt0) ->
            ?BACKEND:call_primitive_last(BSt0, ?PRIM_RAISE_ERROR_TUPLE, [
                ctx, jit_state, offset, ?BADFUN_ATOM, RegCopy
            ])
        end
    ),
    State8 = ?BACKEND:free_native_registers(State7, [RegCopy]),
    State9 = ?BACKEND:call_primitive_with_cp(State8, ?PRIM_CALL_FUN, [
        ctx, jit_state, Reg, ArgsCount
    ]),
    ?BACKEND:assert_all_native_free(State9),
    Stream = ?BACKEND:stream(State9),
    Dump =
        <<
            "   0:\t0105af83          \tlw\tt6,16(a1)\n"
            "   4:\t1ffd                \taddi\tt6,t6,-1\n"
            "   6:\t01f5a823          \tsw\tt6,16(a1)\n"
            "   a:\t000f9d63          \tbnez\tt6,0x24\n"
            "   e:\t00000f97          \tauipc\tt6,0x0\n"
            "  12:\t0fd9                \taddi\tt6,t6,22 # 0x24\n"
            "  14:\t0001                \tnop\n"
            "  16:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  1a:\t4fc1                \tli\tt6,16\n"
            "  1c:\t9fb2                \tadd\tt6,t6,a2\n"
            "  1e:\t000fbf83          \tld\tt6,0(t6)\n"
            "  22:\t8f82                \tjr\tt6\n"
            "  24:\t05853f83          \tld\tt6,88(a0)\n"
            "  28:\t8f7e                \tmv\tt5,t6\n"
            "  2a:\t8efa                \tmv\tt4,t5\n"
            "  2c:\t003efe93          \tandi\tt4,t4,3\n"
            "  30:\t4e09                \tli\tt3,2\n"
            "  32:\t01ce8d63          \tbeq\tt4,t3,0x4c\n"
            "  36:\t09800f93          \tli\tt6,152\n"
            "  3a:\t9fb2                \tadd\tt6,t6,a2\n"
            "  3c:\t000fbf83          \tld\tt6,0(t6)\n"
            "  40:\t04000613          \tli\ta2,64\n"
            "  44:\t18b00693          \tli\ta3,395\n"
            "  48:\t877a                \tmv\ta4,t5\n"
            "  4a:\t8f82                \tjr\tt6\n"
            "  4c:\tffcf7f13          \tandi\tt5,t5,-4\n"
            "  50:\t000f3f03          \tld\tt5,0(t5)\n"
            "  54:\t8efa                \tmv\tt4,t5\n"
            "  56:\t03fefe93          \tandi\tt4,t4,63\n"
            "  5a:\t4e51                \tli\tt3,20\n"
            "  5c:\t01ce8d63          \tbeq\tt4,t3,0x76\n"
            "  60:\t09800f93          \tli\tt6,152\n"
            "  64:\t9fb2                \tadd\tt6,t6,a2\n"
            "  66:\t000fbf83          \tld\tt6,0(t6)\n"
            "  6a:\t06a00613          \tli\ta2,106\n"
            "  6e:\t18b00693          \tli\ta3,395\n"
            "  72:\t877a                \tmv\ta4,t5\n"
            "  74:\t8f82                \tjr\tt6\n"
            "  76:\t0005be83          \tld\tt4,0(a1)\n"
            "  7a:\t000eae83          \tlw\tt4,0(t4)\n"
            "  7e:\t0ee2                \tslli\tt4,t4,0x18\n"
            "  80:\t28800f13          \tli\tt5,648\n"
            "  84:\t00000013          \tnop\n"
            "  88:\t01eeeeb3          \tor\tt4,t4,t5\n"
            "  8c:\t0fd53023          \tsd\tt4,224(a0)\n"
            "  90:\t02000f13          \tli\tt5,32\n"
            "  94:\t0f0e                \tslli\tt5,t5,0x3\n"
            "  96:\t9f32                \tadd\tt5,t5,a2\n"
            "  98:\t000f3f03          \tld\tt5,0(t5)\n"
            "  9c:\t867e                \tmv\ta2,t6\n"
            "  9e:\t4681                \tli\ta3,0\n"
            "  a0:\t8f02                \tjr\tt5"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

move_to_vm_register_test0(State, Source, Dest, Dump) ->
    State1 = ?BACKEND:move_to_vm_register(State, Source, Dest),
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

move_to_vm_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, 0}, <<
                        "   0:\t4f81                \tli\tt6,0\n"
                        "   2:\t05f53c23          \tsd\tt6,88(a0)\n"
                        "   6:\ta8ed                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {x_reg, extra}, <<
                        "   0:\t4f81                \tli\tt6,0\n"
                        "   2:\t0df53c23          \tsd\tt6,216(a0)\n"
                        "   6:\ta8ed                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {ptr, t5}, <<
                        "   0:\t4f81                \tli\tt6,0\n"
                        "   2:\t01ff3023          \tsd\tt6,0(t5)\n"
                        "   6:\ta8ed                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 2}, <<
                        "   0:\t4f01                \tli\tt5,0\n"
                        "   2:\t05053f83          \tld\tt6,80(a0)\n"
                        "   6:\t01efb823          \tsd\tt5,16(t6)\n"
                        "   a:\ta8dd                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 0, {y_reg, 20}, <<
                        "   0:\t4f01                \tli\tt5,0\n"
                        "   2:\t05053f83          \tld\tt6,80(a0)\n"
                        "   6:\t0befb023          \tsd\tt5,160(t6)\n"
                        "   a:\ta8dd                \tj\t0x100"
                    >>)
                end),
                %% Test: Immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, 0}, <<
                        "   0:\t02a00f93          \tli\tt6,42\n"
                        "   4:\t05f53c23          \tsd\tt6,88(a0)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {x_reg, extra}, <<
                        "   0:\t02a00f93          \tli\tt6,42\n"
                        "   4:\t0df53c23          \tsd\tt6,216(a0)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 2}, <<
                        "   0:\t02a00f13          \tli\tt5,42\n"
                        "   4:\t05053f83          \tld\tt6,80(a0)\n"
                        "   8:\t01efb823          \tsd\tt5,16(t6)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 20}, <<
                        "   0:\t02a00f13          \tli\tt5,42\n"
                        "   4:\t05053f83          \tld\tt6,80(a0)\n"
                        "   8:\t0befb023          \tsd\tt5,160(t6)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: Immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 99, {ptr, a3}, <<
                        "   0:\t06300f93          \tli\tt6,99\n"
                        "   4:\t01f6b023          \tsd\tt6,0(a3)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                %% Test: x_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {x_reg, 2}, <<
                        "   0:\t06053f83          \tld\tt6,96(a0)\n"
                        "   4:\t07f53423          \tsd\tt6,104(a0)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                %% Test: x_reg to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 1}, {ptr, a1}, <<
                        "   0:\t06053f83          \tld\tt6,96(a0)\n"
                        "   4:\t01f5b023          \tsd\tt6,0(a1)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                %% Test: ptr to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {ptr, t3}, {x_reg, 3}, <<
                        "   0:\t000e3f83          \tld\tt6,0(t3)\n"
                        "   4:\t07f53823          \tsd\tt6,112(a0)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                %% Test: x_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 0}, {y_reg, 1}, <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t05053f03          \tld\tt5,80(a0)\n"
                        "   8:\t01ff3423          \tsd\tt6,8(t5)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: y_reg to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 0}, {x_reg, 3}, <<
                        "   0:\t05053f03          \tld\tt5,80(a0)\n"
                        "   4:\t000f3f83          \tld\tt6,0(t5)\n"
                        "   8:\t07f53823          \tsd\tt6,112(a0)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: y_reg to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 1}, {x_reg, 3}, <<
                        "   0:\t05053f03          \tld\tt5,80(a0)\n"
                        "   4:\t008f3f83          \tld\tt6,8(t5)\n"
                        "   8:\t07f53823          \tsd\tt6,112(a0)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: Native register to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, t4, {x_reg, 0}, <<
                        "   0:\t05d53c23          \tsd\tt4,88(a0)\n"
                        "   4:\ta8f5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, t5, {x_reg, extra}, <<
                        "   0:\t0de53c23          \tsd\tt5,216(a0)\n"
                        "   4:\ta8f5                \tj\t0x100"
                    >>)
                end),
                %% Test: Native register to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, t3, {ptr, a3}, <<
                        "   0:\t01c6b023          \tsd\tt3,0(a3)\n"
                        "   4:\ta8f5                \tj\t0x100"
                    >>)
                end),
                %% Test: Native register to y_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, a1, {y_reg, 0}, <<
                        "   0:\t05053f83          \tld\tt6,80(a0)\n"
                        "   4:\t00bfb023          \tsd\ta1,0(t6)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                %% Test: Large immediate to x_reg (uses lui + addi in RISC-V)
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, 0}, <<
                        "   0:\t12345fb7          \tlui\tt6,0x12345\n"
                        "   4:\t678f8f93          \taddi\tt6,t6,1656 # 0x12345678\n"
                        "   8:\t05f53c23          \tsd\tt6,88(a0)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {x_reg, extra}, <<
                        "   0:\t12345fb7          \tlui\tt6,0x12345\n"
                        "   4:\t678f8f93          \taddi\tt6,t6,1656 # 0x12345678\n"
                        "   8:\t0df53c23          \tsd\tt6,216(a0)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 2}, <<
                        "   0:\t12345fb7          \tlui\tt6,0x12345\n"
                        "   4:\t678f8f93          \taddi\tt6,t6,1656 # 0x12345678\n"
                        "   8:\t05053f03          \tld\tt5,80(a0)\n"
                        "   c:\t01ff3823          \tsd\tt6,16(t5)\n"
                        "  10:\ta8c5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {y_reg, 20}, <<
                        "   0:\t12345fb7          \tlui\tt6,0x12345\n"
                        "   4:\t678f8f93          \taddi\tt6,t6,1656 # 0x12345678\n"
                        "   8:\t05053f03          \tld\tt5,80(a0)\n"
                        "   c:\t0bff3023          \tsd\tt6,160(t5)\n"
                        "  10:\ta8c5                \tj\t0x100"
                    >>)
                end),
                %% Test: Large immediate to ptr
                ?_test(begin
                    move_to_vm_register_test0(State0, 16#12345678, {ptr, a3}, <<
                        "   0:\t12345fb7          \tlui\tt6,0x12345\n"
                        "   4:\t678f8f93          \taddi\tt6,t6,1656 # 0x12345678\n"
                        "   8:\t01f6b023          \tsd\tt6,0(a3)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: x_reg to y_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {x_reg, 15}, {y_reg, 31}, <<
                        "   0:\t0d053f83          \tld\tt6,208(a0)\n"
                        "   4:\t05053f03          \tld\tt5,80(a0)\n"
                        "   8:\t0fff3c23          \tsd\tt6,248(t5)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: y_reg to x_reg (high index)
                ?_test(begin
                    move_to_vm_register_test0(State0, {y_reg, 31}, {x_reg, 15}, <<
                        "   0:\t05053f03          \tld\tt5,80(a0)\n"
                        "   4:\t0f8f3f83          \tld\tt6,248(t5)\n"
                        "   8:\t0df53823          \tsd\tt6,208(a0)\n"
                        "   c:\ta8d5                \tj\t0x100"
                    >>)
                end),
                %% Test: Large y_reg index (32) that exceeds str immediate offset limit
                ?_test(begin
                    move_to_vm_register_test0(State0, 42, {y_reg, 32}, <<
                        "   0:\t02a00f13          \tli\tt5,42\n"
                        "   4:\t05053f83          \tld\tt6,80(a0)\n"
                        "   8:\t10000e93          \tli\tt4,256\n"
                        "   c:\t9efe                \tadd\tt4,t4,t6\n"
                        "   e:\t01eeb023          \tsd\tt5,0(t4)\n"
                        "  12:\ta0fd                \tj\t0x100"
                    >>)
                end),
                %% Test: Negative immediate to x_reg
                ?_test(begin
                    move_to_vm_register_test0(State0, -1, {x_reg, 0}, <<
                        "   0:\t5ffd                \tli\tt6,-1\n"
                        "   2:\t05f53c23          \tsd\tt6,88(a0)\n"
                        "   6:\ta8ed                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, -100, {x_reg, 0}, <<
                        "   0:\tf9c00f93          \tli\tt6,-100\n"
                        "   4:\t05f53c23          \tsd\tt6,88(a0)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end),
                ?_test(begin
                    move_to_vm_register_test0(State0, -1000, {x_reg, 0}, <<
                        "   0:\tc1800f93          \tli\tt6,-1000\n"
                        "   4:\t05f53c23          \tsd\tt6,88(a0)\n"
                        "   8:\ta8e5                \tj\t0x100"
                    >>)
                end)
            ]
        end}.

move_array_element_test0(State, Reg, Index, Dest, Dump) ->
    State1 = ?BACKEND:move_array_element(State, Reg, Index, Dest),
    Stream = ?BACKEND:stream(State1),
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

move_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 2, {x_reg, 0}, <<
                        "   0:\t0106bf83          \tld\tt6,16(a3)\n"
                        "   4:\t05f53c23          \tsd\tt6,88(a0)"
                    >>)
                end),
                %% move_array_element: reg[x] to ptr
                ?_test(begin
                    move_array_element_test0(State0, a3, 3, {ptr, t4}, <<
                        "   0:\t0186bf83          \tld\tt6,24(a3)\n"
                        "   4:\t01feb023          \tsd\tt6,0(t4)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 1, {y_reg, 2}, <<
                        "   0:\t0086bf03          \tld\tt5,8(a3)\n"
                        "   4:\t05053f83          \tld\tt6,80(a0)\n"
                        "   8:\t01efb823          \tsd\tt5,16(t6)"
                    >>)
                end),
                %% move_array_element: reg[x] to native reg (t4)
                ?_test(begin
                    move_array_element_test0(State0, a3, 1, t4, <<
                        "   0:\t0086be83          \tld\tt4,8(a3)"
                    >>)
                end),
                %% move_array_element: reg[x] to y_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 7, {y_reg, 31}, <<
                        "   0:\t0386bf03          \tld\tt5,56(a3)\n"
                        "   4:\t05053f83          \tld\tt6,80(a0)\n"
                        "   8:\t0fefbc23          \tsd\tt5,248(t6)"
                    >>)
                end),
                %% move_array_element: reg[x] to x_reg
                ?_test(begin
                    move_array_element_test0(State0, a3, 7, {x_reg, 15}, <<
                        "   0:\t0386bf83          \tld\tt6,56(a3)\n"
                        "   4:\t0df53823          \tsd\tt6,208(a0)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to x_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {x_reg, 2}, <<
                        "   0:\t0206bf83          \tld\tt6,32(a3)\n"
                        "   4:\t0f8e                \tslli\tt6,t6,0x3\n"
                        "   6:\t01f68fb3          \tadd\tt6,a3,t6\n"
                        "   a:\t000fbf83          \tld\tt6,0(t6)\n"
                        "   e:\t07f53423          \tsd\tt6,104(a0)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to pointer (large x reg)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {ptr, t4}, <<
                        "   0:\t0206bf83          \tld\tt6,32(a3)\n"
                        "   4:\t0f8e                \tslli\tt6,t6,0x3\n"
                        "   6:\t01f68fb3          \tadd\tt6,a3,t6\n"
                        "   a:\t000fbf83          \tld\tt6,0(t6)\n"
                        "   e:\t01feb023          \tsd\tt6,0(t4)"
                    >>)
                end),
                %% move_array_element: reg_x[reg_y] to y_reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, a3, 4),
                    move_array_element_test0(State1, a3, {free, Reg}, {y_reg, 31}, <<
                        "   0:\t0206bf83          \tld\tt6,32(a3)\n"
                        "   4:\t0f8e                \tslli\tt6,t6,0x3\n"
                        "   6:\t01f68fb3          \tadd\tt6,a3,t6\n"
                        "   a:\t000fbf83          \tld\tt6,0(t6)\n"
                        "   e:\t05053f03          \tld\tt5,80(a0)\n"
                        "  12:\t0fff3c23          \tsd\tt6,248(t5)"
                    >>)
                end),
                %% move_array_element with integer index and x_reg destination
                ?_test(begin
                    {State1, BaseReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    move_array_element_test0(State1, BaseReg, 2, {x_reg, 5}, <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t010fbf03          \tld\tt5,16(t6)\n"
                        "   8:\t09e53023          \tsd\tt5,128(a0)"
                    >>)
                end)
            ]
        end}.

get_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% get_array_element: reg[x] to new native reg
                ?_test(begin
                    {State1, Reg} = ?BACKEND:get_array_element(State0, t3, 4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t020e3f83          \tld\tt6,32(t3)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(t6, Reg)
                end)
            ]
        end}.

move_to_array_element_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_to_array_element/4: x_reg to reg[x]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, 2),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t01f6b823          \tsd\tt6,16(a3)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/4: x_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t8f72                \tmv\tt5,t3\n"
                        "   6:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   8:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "   c:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/4: ptr to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {ptr, t6}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t000fbf83          \tld\tt6,0(t6)\n"
                        "   4:\t8f72                \tmv\tt5,t3\n"
                        "   6:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   8:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "   c:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/4: y_reg to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {y_reg, 2}, a3, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05053f03          \tld\tt5,80(a0)\n"
                        "   4:\t010f3f83          \tld\tt6,16(t5)\n"
                        "   8:\t8f72                \tmv\tt5,t3\n"
                        "   a:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   c:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "  10:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    State1 = ?BACKEND:move_to_array_element(State0, {x_reg, 0}, a3, 2, 1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t01f6bc23          \tsd\tt6,24(a3)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/5: x_reg to reg[x+offset]
                ?_test(begin
                    A3Bit = 1 bsl 3,
                    T3Bit = 1 bsl 11,
                    UsedMask = A3Bit bor T3Bit,
                    AvailMask = element(7, State0) band (bnot UsedMask),
                    State1 = setelement(7, State0, AvailMask),
                    State2 = setelement(8, State1, UsedMask),
                    ?assertEqual(lists:sort([a3, t3]), lists:sort(?BACKEND:used_regs(State2))),
                    State3 = ?BACKEND:move_to_array_element(State2, {x_reg, 0}, a3, t3, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t001e0f13          \taddi\tt5,t3,1\n"
                        "   8:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   a:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "   e:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_array_element/5: imm to reg[x+offset]
                ?_test(begin
                    A3Bit = 1 bsl 3,
                    T3Bit = 1 bsl 11,
                    UsedMask = A3Bit bor T3Bit,
                    AvailMask = element(7, State0) band (bnot UsedMask),
                    State1 = setelement(7, State0, AvailMask),
                    State2 = setelement(8, State1, UsedMask),
                    ?assertEqual(lists:sort([a3, t3]), lists:sort(?BACKEND:used_regs(State2))),
                    State3 = ?BACKEND:move_to_array_element(State2, 42, a3, t3, 1),
                    Stream = ?BACKEND:stream(State3),
                    Dump = <<
                        "   0:\t02a00f93          \tli\tt6,42\n"
                        "   4:\t001e0f13          \taddi\tt5,t3,1\n"
                        "   8:\t0f0e                \tslli\tt5,t5,0x3\n"
                        "   a:\t01e68f33          \tadd\tt5,a3,t5\n"
                        "   e:\t01ff3023          \tsd\tt6,0(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end)
            ]
        end}.

move_to_native_register_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                %% move_to_native_register/2: imm
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, 42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  02a00f93            li  t6,42"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: negative value
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -42),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  fd600f93            li  t6,-42"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: -255 (boundary case)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -255),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  f0100f93            li  t6,-255"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: -256 (boundary case, fits in immediate for RISC-V)
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, -256),
                    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
                    Stream = ?BACKEND:stream(State2),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:  f0000f93            li  t6,-256\n"
                        "   4:  a8f5                    j   0x100"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: {ptr, reg}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {ptr, t5}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t5, Reg),
                    Dump = <<
                        "   0:\t000f3f03          \tld\tt5,0(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: {x_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {x_reg, 5}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:\t08053f83          \tld\tt6,128(a0)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/2: {y_reg, N}
                ?_test(begin
                    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 3}),
                    Stream = ?BACKEND:stream(State1),
                    ?assertEqual(t6, Reg),
                    Dump = <<
                        "   0:\t05053f03          \tld\tt5,80(a0)\n"
                        "   4:\t018f3f83          \tld\tt6,24(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: imm to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, 42, t5),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:  02a00f13            li  t5,42"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: reg to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, t6, t4),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:  8efe                    mv  t4,t6"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: {ptr, reg} to reg
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {ptr, t6}, t3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t000fbe03          \tld\tt3,0(t6)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: {x_reg, x} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {x_reg, 2}, a3),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t7534                \tld\ta3,104(a0)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% move_to_native_register/3: {y_reg, y} to reg[reg]
                ?_test(begin
                    State1 = ?BACKEND:move_to_native_register(State0, {y_reg, 2}, a1),
                    Stream = ?BACKEND:stream(State1),
                    Dump = <<
                        "   0:\t05053f83          \tld\tt6,80(a0)\n"
                        "   4:\t010fb583          \tld\ta1,16(t6)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                %% Test: ptr with offset to fp_reg (term_to_float)
                ?_test(begin
                    {State1, RegA} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
                    State2 = ?BACKEND:move_to_vm_register(
                        State1, {free, {ptr, RegA, 1}}, {fp_reg, 3}
                    ),
                    Stream = ?BACKEND:stream(State2),
                    Dump = <<
                        "   0:\t05853f83          \tld\tt6,88(a0)\n"
                        "   4:\t0e853f03          \tld\tt5,232(a0)\n"
                        "   8:\t004fae83          \tlw\tt4,4(t6)\n"
                        "   c:\t01df2c23          \tsw\tt4,24(t5)\n"
                        "  10:\t008fae83          \tlw\tt4,8(t6)\n"
                        "  14:\t01df2e23          \tsw\tt4,28(t5)"
                    >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end)
            ]
        end}.

add_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:add(State0, Reg, Imm),
    % Force emission of literal pool
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

add_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    add_test0(State0, a2, 2, <<
                        "   0:  0609                    addi    a2,a2,2\n"
                        "   2:  a8fd                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, a2, 256, <<
                        "   0:  10000f93            li  t6,256\n"
                        "   4:  967e                    add a2,a2,t6\n"
                        "   6:  a8ed                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    add_test0(State0, a2, a3, <<
                        "   0:  9636                    add a2,a2,a3\n"
                        "   2:  a8fd                    j   0x100"
                    >>)
                end)
            ]
        end}.

sub_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:sub(State0, Reg, Imm),
    % Force emission of literal pool
    State2 = ?BACKEND:jump_to_offset(State1, 16#100),
    Stream = ?BACKEND:stream(State2),
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

sub_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    sub_test0(State0, a2, 2, <<
                        "   0:  1679                    addi    a2,a2,-2\n"
                        "   2:  a8fd                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, a2, 256, <<
                        "      0:   10000f93            li  t6,256\n"
                        "      4:   41f60633            sub a2,a2,t6\n"
                        "      8:   a8e5                    j   0x100"
                    >>)
                end),
                ?_test(begin
                    sub_test0(State0, a2, a3, <<
                        "      0:   8e15                    sub a2,a2,a3\n"
                        "      2:   a8fd                    j   0x100"
                    >>)
                end)
            ]
        end}.

mul_test0(State0, Reg, Imm, Dump) ->
    State1 = ?BACKEND:mul(State0, Reg, Imm),
    Stream = ?BACKEND:stream(State1),
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

mul_test_() ->
    {setup,
        fun() ->
            ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0))
        end,
        fun(State0) ->
            [
                ?_test(begin
                    mul_test0(State0, a2, 2, <<
                        "      0:   0606                    slli    a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 3, <<
                        "      0:   00161f93            slli    t6,a2,0x1\n"
                        "      4:   00cf8633            add a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 4, <<
                        "      0:   060a                    slli    a2,a2,0x2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 5, <<
                        "      0:   00261f93            slli    t6,a2,0x2\n"
                        "      4:   00cf8633            add a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 6, <<
                        "      0:   00161f93            slli    t6,a2,0x1\n"
                        "      4:   00cf8633            add a2,t6,a2\n"
                        "      8:   0606                    slli    a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 7, <<
                        "      0:   00361f93            slli    t6,a2,0x3\n"
                        "      4:   40cf8633            sub a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 8, <<
                        "      0:   060e                    slli    a2,a2,0x3"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 9, <<
                        "      0:   00361f93            slli    t6,a2,0x3\n"
                        "      4:   00cf8633            add a2,t6,a2"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 10, <<
                        "      0:   00261f93            slli    t6,a2,0x2\n"
                        "      4:   00cf8633            add a2,t6,a2\n"
                        "      8:   0606                    slli    a2,a2,0x1"
                    >>)
                end),
                ?_test(begin
                    mul_test0(State0, a2, 11, <<
                        "      0:   4fad                    li  t6,11\n"
                        "      2:   03f60633            mul a2,a2,t6"
                    >>)
                end)
            ]
        end}.

%% Test set_args1 with y_reg pattern
set_args1_y_reg_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

    % Call primitive with y_reg argument to trigger {y_reg, X} pattern in set_args1
    % This mirrors: {MSt2, Value} = MMod:call_primitive(MSt1, ?PRIM_BITSTRING_GET_UTF8, [{free, Src}])
    % but with {y_reg, 5} instead of {free, Src}
    {State1, _ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_BITSTRING_GET_UTF8, [
        {y_reg, 5}
    ]),

    Stream = ?BACKEND:stream(State1),
    % Expected disassembly for loading from y_reg and calling primitive
    Dump = <<
        "   0:\t04300f93          \tli\tt6,67\n"
        "   4:\t0f8e                \tslli\tt6,t6,0x3\n"
        "   6:\t9fb2                \tadd\tt6,t6,a2\n"
        "   8:\t000fbf83          \tld\tt6,0(t6)\n"
        "   c:\t1101                \taddi\tsp,sp,-32\n"
        "   e:\te006                \tsd\tra,0(sp)\n"
        "  10:\te42a                \tsd\ta0,8(sp)\n"
        "  12:\te82e                \tsd\ta1,16(sp)\n"
        "  14:\tec32                \tsd\ta2,24(sp)\n"
        "  16:\t05053f03          \tld\tt5,80(a0)\n"
        "  1a:\t028f3503          \tld\ta0,40(t5)\n"
        "  1e:\t9f82                \tjalr\tt6\n"
        "  20:\t8faa                \tmv\tt6,a0\n"
        "  22:\t6082                \tld\tra,0(sp)\n"
        "  24:\t6522                \tld\ta0,8(sp)\n"
        "  26:\t65c2                \tld\ta1,16(sp)\n"
        "  28:\t6662                \tld\ta2,24(sp)\n"
        "  2a:\t02010113          \taddi\tsp,sp,32"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test large Y register read (Y=123, offset=492, exceeds immediate limit)
large_y_reg_read_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move from a large Y register (123 * 4 = 492 bytes, exceeds immediate limit)
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 123}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp register for large offset
    Dump = <<
        "   0:\t05053f03          \tld\tt5,80(a0)\n"
        "   4:\t3d800f93          \tli\tt6,984\n"
        "   8:\t9ffa                \tadd\tt6,t6,t5\n"
        "   a:\t000fbf83          \tld\tt6,0(t6)"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream),
    ?assertEqual(t6, Reg).

%% Test large Y register write with immediate value
large_y_reg_write_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Move immediate to a large Y register (123 * 4 = 492 bytes)
    State1 = ?BACKEND:move_to_vm_register(State0, 42, {y_reg, 123}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses helper with temp registers for large offset
    Dump = <<
        "   0:\t02a00f13          \tli\tt5,42\n"
        "   4:\t05053f83          \tld\tt6,80(a0)\n"
        "   8:\t3d800e93          \tli\tt4,984\n"
        "   c:\t9efe                \tadd\tt4,t4,t6\n"
        "   e:\t01eeb023          \tsd\tt5,0(t4)"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test large Y register read with limited registers (uses IP_REG fallback)
large_y_reg_read_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate most available registers to simulate near-exhaustion (leave 1 for the y_reg helper)
    {State1, _} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, _} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, _} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, _} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Leave one register available so the y_reg helper can work, but it will need IP_REG fallback
    {StateFinal, ResultReg} = ?BACKEND:move_to_native_register(State5, {y_reg, 35}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses t0+t1 fallback sequence when temps are exhausted
    Dump = <<
        "   0:\t05853f83          \tld\tt6,88(a0)\n"
        "   4:\t06053f03          \tld\tt5,96(a0)\n"
        "   8:\t06853e83          \tld\tt4,104(a0)\n"
        "   c:\t07053e03          \tld\tt3,112(a0)\n"
        "  10:\t07853383          \tld\tt2,120(a0)\n"
        "  14:\t05053283          \tld\tt0,80(a0)\n"
        "  18:\t11800313          \tli\tt1,280\n"
        "  1c:\t9316                \tadd\tt1,t1,t0\n"
        "  1e:\t00033303          \tld\tt1,0(t1)"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream),
    ?assertEqual(t1, ResultReg).

%% Test large Y register write with register exhaustion (uses t1/t0 fallback)
large_y_reg_write_register_exhaustion_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Get a source register first
    {State1, SrcReg} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    % Allocate most remaining registers to simulate exhaustion
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    % Try to write to large Y register when only one temp register is available
    StateFinal = ?BACKEND:move_to_vm_register(State5, SrcReg, {y_reg, 50}),
    Stream = ?BACKEND:stream(StateFinal),
    % Expected: uses t1/t0 fallback sequence
    Dump = <<
        "   0:\t05853f83          \tld\tt6,88(a0)\n"
        "   4:\t06053f03          \tld\tt5,96(a0)\n"
        "   8:\t06853e83          \tld\tt4,104(a0)\n"
        "   c:\t07053e03          \tld\tt3,112(a0)\n"
        "  10:\t07853383          \tld\tt2,120(a0)\n"
        "  14:\t05053303          \tld\tt1,80(a0)\n"
        "  18:\t19000293          \tli\tt0,400\n"
        "  1c:\t929a                \tadd\tt0,t0,t1\n"
        "  1e:\t01f2b023          \tsd\tt6,0(t0)"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test boundary case: Y=31 (124 bytes, exactly at limit, should use direct addressing)
y_reg_boundary_direct_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, Reg} = ?BACKEND:move_to_native_register(State0, {y_reg, 31}),
    Stream = ?BACKEND:stream(State1),
    % Expected: uses direct addressing since 31 * 4 = 124 < 2048
    Dump = <<
        "   0:\t05053f03          \tld\tt5,80(a0)\n"
        "   4:\t0f8f3f83          \tld\tt6,248(t5)"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream),
    ?assertEqual(t6, Reg).

%% Test debugger function
debugger_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:debugger(State0),
    Stream = ?BACKEND:stream(State1),
    Dump = <<
        "   0:\t9002                \tebreak"
    >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

and_register_exhaustion_negative_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test negative immediate (-4) which should use NOT+AND with t0 as temp
    {StateResult, t6} = ?BACKEND:and_(StateNoRegs, {free, t6}, -4),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:\t05853f83          \tld\tt6,88(a0)\n"
        "   4:\t06053f03          \tld\tt5,96(a0)\n"
        "   8:\t06853e83          \tld\tt4,104(a0)\n"
        "   c:\t07053e03          \tld\tt3,112(a0)\n"
        "  10:\t07853383          \tld\tt2,120(a0)\n"
        "  14:\t08053303          \tld\tt1,128(a0)\n"
        "  18:\tffcfff93          \tandi\tt6,t6,-4"
    >>,
    jit_tests_common:assert_stream(riscv64, ExpectedDump, Stream).

and_register_exhaustion_positive_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Allocate all available registers to simulate register exhaustion
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
    {StateNoRegs, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
    % Test positive immediate (0x3F) which should use AND with t0 as temp
    {StateResult, t6} = ?BACKEND:and_(StateNoRegs, {free, t6}, 16#3F),
    Stream = ?BACKEND:stream(StateResult),
    ExpectedDump = <<
        "   0:\t05853f83          \tld\tt6,88(a0)\n"
        "   4:\t06053f03          \tld\tt5,96(a0)\n"
        "   8:\t06853e83          \tld\tt4,104(a0)\n"
        "   c:\t07053e03          \tld\tt3,112(a0)\n"
        "  10:\t07853383          \tld\tt2,120(a0)\n"
        "  14:\t08053303          \tld\tt1,128(a0)\n"
        "  18:\t03ffff93          \tandi\tt6,t6,63"
    >>,
    jit_tests_common:assert_stream(riscv64, ExpectedDump, Stream).

jump_table_large_labels_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 512),
    Stream = ?BACKEND:stream(State1),
    % RISC-V: Each jump table entry is 8 bytes (AUIPC + JALR)
    ?assertEqual((512 + 1) * 8, byte_size(Stream)).

alloc_boxed_integer_fragment_small_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 42}
    ]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:\t07800f93          \tli\tt6,120\n"
            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
            "   a:\t1101                \taddi\tsp,sp,-32\n"
            "   c:\te006                \tsd\tra,0(sp)\n"
            "   e:\te42a                \tsd\ta0,8(sp)\n"
            "  10:\te82e                \tsd\ta1,16(sp)\n"
            "  12:\tec32                \tsd\ta2,24(sp)\n"
            "  14:\t02a00593          \tli\ta1,42\n"
            "  18:\t9f82                \tjalr\tt6\n"
            "  1a:\t8faa                \tmv\tt6,a0\n"
            "  1c:\t6082                \tld\tra,0(sp)\n"
            "  1e:\t6522                \tld\ta0,8(sp)\n"
            "  20:\t65c2                \tld\ta1,16(sp)\n"
            "  22:\t6662                \tld\ta2,24(sp)\n"
            "  24:\t02010113          \taddi\tsp,sp,32"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

alloc_boxed_integer_fragment_large_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, ?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT, [
        ctx, {avm_int64_t, 16#123456789ABCDEF0}
    ]),
    % Add a call primitive last to emit literal pool
    State2 = ?BACKEND:call_primitive_last(State1, ?PRIM_RAISE_ERROR_TUPLE, [
        ctx, jit_state, offset, ?BADMATCH_ATOM, {free, ResultReg}
    ]),
    ?assertEqual(t6, ResultReg),
    Stream = ?BACKEND:stream(State2),
    Dump =
        <<
            "   0:\t07800f93          \tli\tt6,120\n"
            "   4:\t9fb2                \tadd\tt6,t6,a2\n"
            "   6:\t000fbf83          \tld\tt6,0(t6)\n"
            "   a:\t1101                \taddi\tsp,sp,-32\n"
            "   c:\te006                \tsd\tra,0(sp)\n"
            "   e:\te42a                \tsd\ta0,8(sp)\n"
            "  10:\te82e                \tsd\ta1,16(sp)\n"
            "  12:\tec32                \tsd\ta2,24(sp)\n"
            "  14:\t123455b7          \tlui\ta1,0x12345\n"
            "  18:\t67958593          \taddi\ta1,a1,1657 # 0x12345679\n"
            "  1c:\t05b2                \tslli\ta1,a1,0xc\n"
            "  1e:\t9ac58593          \taddi\ta1,a1,-1620\n"
            "  22:\t05b2                \tslli\ta1,a1,0xc\n"
            "  24:\tcde58593          \taddi\ta1,a1,-802\n"
            "  28:\t05a2                \tslli\ta1,a1,0x8\n"
            "  2a:\t0f058593          \taddi\ta1,a1,240\n"
            "  2e:\t9f82                \tjalr\tt6\n"
            "  30:\t8faa                \tmv\tt6,a0\n"
            "  32:\t6082                \tld\tra,0(sp)\n"
            "  34:\t6522                \tld\ta0,8(sp)\n"
            "  36:\t65c2                \tld\ta1,16(sp)\n"
            "  38:\t6662                \tld\ta2,24(sp)\n"
            "  3a:\t02010113          \taddi\tsp,sp,32\n"
            "  3e:\t09800f13          \tli\tt5,152\n"
            "  42:\t9f32                \tadd\tt5,t5,a2\n"
            "  44:\t000f3f03          \tld\tt5,0(t5)\n"
            "  48:\t04800613          \tli\ta2,72\n"
            "  4c:\t28b00693          \tli\ta3,651\n"
            "  50:\t877e                \tmv\ta4,t6\n"
            "  52:\t8f02                \tjr\tt5"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test for stack alignment issue in call_func_ptr
%% RISC-V maintains 16-byte stack alignment (RISC-V calling convention)
call_func_ptr_stack_alignment_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
    {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
    {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
    {State5, _ResultReg} = ?BACKEND:call_func_ptr(State4, {free, t3}, [42]),
    Stream = ?BACKEND:stream(State5),
    Dump =
        <<
            "   0:\t05853f83          \tld\tt6,88(a0)\n"
            "   4:\t06053f03          \tld\tt5,96(a0)\n"
            "   8:\t06853e83          \tld\tt4,104(a0)\n"
            "   c:\t07053e03          \tld\tt3,112(a0)\n"
            "  10:\tfc010113          \taddi\tsp,sp,-64\n"
            "  14:\te006                \tsd\tra,0(sp)\n"
            "  16:\te42a                \tsd\ta0,8(sp)\n"
            "  18:\te82e                \tsd\ta1,16(sp)\n"
            "  1a:\tec32                \tsd\ta2,24(sp)\n"
            "  1c:\tf07e                \tsd\tt6,32(sp)\n"
            "  1e:\tf47a                \tsd\tt5,40(sp)\n"
            "  20:\tf876                \tsd\tt4,48(sp)\n"
            "  22:\t02a00513          \tli\ta0,42\n"
            "  26:\t9e02                \tjalr\tt3\n"
            "  28:\t8e2a                \tmv\tt3,a0\n"
            "  2a:\t6082                \tld\tra,0(sp)\n"
            "  2c:\t6522                \tld\ta0,8(sp)\n"
            "  2e:\t65c2                \tld\ta1,16(sp)\n"
            "  30:\t6662                \tld\ta2,24(sp)\n"
            "  32:\t7f82                \tld\tt6,32(sp)\n"
            "  34:\t7f22                \tld\tt5,40(sp)\n"
            "  36:\t7ec2                \tld\tt4,48(sp)\n"
            "  38:\t04010113          \taddi\tsp,sp,64"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% Test for register exhaustion issue in call_func_ptr with 5+ arguments
%% When all registers are used and we call a function with 5+ args,
%% set_args needs temporary registers but none are available
call_func_ptr_register_exhaustion_test_() ->
    {setup,
        fun() ->
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),

            % Allocate all available registers to simulate register pressure
            {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
            {State2, t5} = ?BACKEND:move_to_native_register(State1, {x_reg, 1}),
            {State3, t4} = ?BACKEND:move_to_native_register(State2, {x_reg, 2}),
            {State4, t3} = ?BACKEND:move_to_native_register(State3, {x_reg, 3}),
            {State5, t2} = ?BACKEND:move_to_native_register(State4, {x_reg, 4}),
            {State6, t1} = ?BACKEND:move_to_native_register(State5, {x_reg, 5}),
            State6
        end,
        fun(State6) ->
            [
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, 3, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:\t05853f83          \tld\tt6,88(a0)\n"
                            "   4:\t06053f03          \tld\tt5,96(a0)\n"
                            "   8:\t06853e83          \tld\tt4,104(a0)\n"
                            "   c:\t07053e03          \tld\tt3,112(a0)\n"
                            "  10:\t07853383          \tld\tt2,120(a0)\n"
                            "  14:\t08053303          \tld\tt1,128(a0)\n"
                            "  18:\tfc010113          \taddi\tsp,sp,-64\n"
                            "  1c:\te006                \tsd\tra,0(sp)\n"
                            "  1e:\te42a                \tsd\ta0,8(sp)\n"
                            "  20:\te82e                \tsd\ta1,16(sp)\n"
                            "  22:\tec32                \tsd\ta2,24(sp)\n"
                            "  24:\tf07e                \tsd\tt6,32(sp)\n"
                            "  26:\tf476                \tsd\tt4,40(sp)\n"
                            "  28:\tf872                \tsd\tt3,48(sp)\n"
                            "  2a:\tfc1a                \tsd\tt1,56(sp)\n"
                            "  2c:\t861e                \tmv\ta2,t2\n"
                            "  2e:\t468d                \tli\ta3,3\n"
                            "  30:\t4705                \tli\ta4,1\n"
                            "  32:\t9f02                \tjalr\tt5\n"
                            "  34:\t8f2a                \tmv\tt5,a0\n"
                            "  36:\t6082                \tld\tra,0(sp)\n"
                            "  38:\t6522                \tld\ta0,8(sp)\n"
                            "  3a:\t65c2                \tld\ta1,16(sp)\n"
                            "  3c:\t6662                \tld\ta2,24(sp)\n"
                            "  3e:\t7f82                \tld\tt6,32(sp)\n"
                            "  40:\t7ea2                \tld\tt4,40(sp)\n"
                            "  42:\t7e42                \tld\tt3,48(sp)\n"
                            "  44:\t7362                \tld\tt1,56(sp)\n"
                            "  46:\t04010113          \taddi\tsp,sp,64"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, 1, t1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:\t05853f83          \tld\tt6,88(a0)\n"
                            "   4:\t06053f03          \tld\tt5,96(a0)\n"
                            "   8:\t06853e83          \tld\tt4,104(a0)\n"
                            "   c:\t07053e03          \tld\tt3,112(a0)\n"
                            "  10:\t07853383          \tld\tt2,120(a0)\n"
                            "  14:\t08053303          \tld\tt1,128(a0)\n"
                            "  18:\tfc010113          \taddi\tsp,sp,-64\n"
                            "  1c:\te006                \tsd\tra,0(sp)\n"
                            "  1e:\te42a                \tsd\ta0,8(sp)\n"
                            "  20:\te82e                \tsd\ta1,16(sp)\n"
                            "  22:\tec32                \tsd\ta2,24(sp)\n"
                            "  24:\tf07e                \tsd\tt6,32(sp)\n"
                            "  26:\tf476                \tsd\tt4,40(sp)\n"
                            "  28:\tf872                \tsd\tt3,48(sp)\n"
                            "  2a:\tfc1a                \tsd\tt1,56(sp)\n"
                            "  2c:\t861e                \tmv\ta2,t2\n"
                            "  2e:\t4685                \tli\ta3,1\n"
                            "  30:\t871a                \tmv\ta4,t1\n"
                            "  32:\t9f02                \tjalr\tt5\n"
                            "  34:\t8f2a                \tmv\tt5,a0\n"
                            "  36:\t6082                \tld\tra,0(sp)\n"
                            "  38:\t6522                \tld\ta0,8(sp)\n"
                            "  3a:\t65c2                \tld\ta1,16(sp)\n"
                            "  3c:\t6662                \tld\ta2,24(sp)\n"
                            "  3e:\t7f82                \tld\tt6,32(sp)\n"
                            "  40:\t7ea2                \tld\tt4,40(sp)\n"
                            "  42:\t7e42                \tld\tt3,48(sp)\n"
                            "  44:\t7362                \tld\tt1,56(sp)\n"
                            "  46:\t04010113          \taddi\tsp,sp,64"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, t5},
                        [ctx, jit_state, {free, t2}, t1, 1]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:\t05853f83          \tld\tt6,88(a0)\n"
                            "   4:\t06053f03          \tld\tt5,96(a0)\n"
                            "   8:\t06853e83          \tld\tt4,104(a0)\n"
                            "   c:\t07053e03          \tld\tt3,112(a0)\n"
                            "  10:\t07853383          \tld\tt2,120(a0)\n"
                            "  14:\t08053303          \tld\tt1,128(a0)\n"
                            "  18:\tfc010113          \taddi\tsp,sp,-64\n"
                            "  1c:\te006                \tsd\tra,0(sp)\n"
                            "  1e:\te42a                \tsd\ta0,8(sp)\n"
                            "  20:\te82e                \tsd\ta1,16(sp)\n"
                            "  22:\tec32                \tsd\ta2,24(sp)\n"
                            "  24:\tf07e                \tsd\tt6,32(sp)\n"
                            "  26:\tf476                \tsd\tt4,40(sp)\n"
                            "  28:\tf872                \tsd\tt3,48(sp)\n"
                            "  2a:\tfc1a                \tsd\tt1,56(sp)\n"
                            "  2c:\t861e                \tmv\ta2,t2\n"
                            "  2e:\t869a                \tmv\ta3,t1\n"
                            "  30:\t4705                \tli\ta4,1\n"
                            "  32:\t9f02                \tjalr\tt5\n"
                            "  34:\t8f2a                \tmv\tt5,a0\n"
                            "  36:\t6082                \tld\tra,0(sp)\n"
                            "  38:\t6522                \tld\ta0,8(sp)\n"
                            "  3a:\t65c2                \tld\ta1,16(sp)\n"
                            "  3c:\t6662                \tld\ta2,24(sp)\n"
                            "  3e:\t7f82                \tld\tt6,32(sp)\n"
                            "  40:\t7ea2                \tld\tt4,40(sp)\n"
                            "  42:\t7e42                \tld\tt3,48(sp)\n"
                            "  44:\t7362                \tld\tt1,56(sp)\n"
                            "  46:\t04010113          \taddi\tsp,sp,64"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream),
                    ?assertEqual(t5, ResultReg)
                end),
                ?_test(begin
                    {State7, _ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {free, a1},
                        [t5, a3]
                    ),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:\t05853f83          \tld\tt6,88(a0)\n"
                            "   4:\t06053f03          \tld\tt5,96(a0)\n"
                            "   8:\t06853e83          \tld\tt4,104(a0)\n"
                            "   c:\t07053e03          \tld\tt3,112(a0)\n"
                            "  10:\t07853383          \tld\tt2,120(a0)\n"
                            "  14:\t08053303          \tld\tt1,128(a0)\n"
                            "  18:\tfb010113          \taddi\tsp,sp,-80\n"
                            "  1c:\te006                \tsd\tra,0(sp)\n"
                            "  1e:\te42a                \tsd\ta0,8(sp)\n"
                            "  20:\te82e                \tsd\ta1,16(sp)\n"
                            "  22:\tec32                \tsd\ta2,24(sp)\n"
                            "  24:\tf07e                \tsd\tt6,32(sp)\n"
                            "  26:\tf47a                \tsd\tt5,40(sp)\n"
                            "  28:\tf876                \tsd\tt4,48(sp)\n"
                            "  2a:\tfc72                \tsd\tt3,56(sp)\n"
                            "  2c:\te09e                \tsd\tt2,64(sp)\n"
                            "  2e:\te49a                \tsd\tt1,72(sp)\n"
                            "  30:\t8fae                \tmv\tt6,a1\n"
                            "  32:\t857a                \tmv\ta0,t5\n"
                            "  34:\t85b6                \tmv\ta1,a3\n"
                            "  36:\t9f82                \tjalr\tt6\n"
                            "  38:\te82a                \tsd\ta0,16(sp)\n"
                            "  3a:\t6082                \tld\tra,0(sp)\n"
                            "  3c:\t6522                \tld\ta0,8(sp)\n"
                            "  3e:\t65c2                \tld\ta1,16(sp)\n"
                            "  40:\t6662                \tld\ta2,24(sp)\n"
                            "  42:\t7f82                \tld\tt6,32(sp)\n"
                            "  44:\t7f22                \tld\tt5,40(sp)\n"
                            "  46:\t7ec2                \tld\tt4,48(sp)\n"
                            "  48:\t7e62                \tld\tt3,56(sp)\n"
                            "  4a:\t6386                \tld\tt2,64(sp)\n"
                            "  4c:\t6326                \tld\tt1,72(sp)\n"
                            "  4e:\t05010113          \taddi\tsp,sp,80"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end),
                ?_test(begin
                    {State7, ResultReg} = ?BACKEND:call_func_ptr(
                        State6,
                        {primitive, 2},
                        [{free, t5}, a3]
                    ),
                    ?assertEqual(ResultReg, t5),
                    Stream = ?BACKEND:stream(State7),
                    Dump =
                        <<
                            "   0:\t05853f83          \tld\tt6,88(a0)\n"
                            "   4:\t06053f03          \tld\tt5,96(a0)\n"
                            "   8:\t06853e83          \tld\tt4,104(a0)\n"
                            "   c:\t07053e03          \tld\tt3,112(a0)\n"
                            "  10:\t07853383          \tld\tt2,120(a0)\n"
                            "  14:\t08053303          \tld\tt1,128(a0)\n"
                            "  18:\tfb010113          \taddi\tsp,sp,-80\n"
                            "  1c:\te006                \tsd\tra,0(sp)\n"
                            "  1e:\te42a                \tsd\ta0,8(sp)\n"
                            "  20:\te82e                \tsd\ta1,16(sp)\n"
                            "  22:\tec32                \tsd\ta2,24(sp)\n"
                            "  24:\tf07e                \tsd\tt6,32(sp)\n"
                            "  26:\tf476                \tsd\tt4,40(sp)\n"
                            "  28:\tf872                \tsd\tt3,48(sp)\n"
                            "  2a:\tfc1e                \tsd\tt2,56(sp)\n"
                            "  2c:\te09a                \tsd\tt1,64(sp)\n"
                            "  2e:\t4fc1                \tli\tt6,16\n"
                            "  30:\t9fb2                \tadd\tt6,t6,a2\n"
                            "  32:\t000fbf83          \tld\tt6,0(t6)\n"
                            "  36:\t857a                \tmv\ta0,t5\n"
                            "  38:\t85b6                \tmv\ta1,a3\n"
                            "  3a:\t9f82                \tjalr\tt6\n"
                            "  3c:\t8f2a                \tmv\tt5,a0\n"
                            "  3e:\t6082                \tld\tra,0(sp)\n"
                            "  40:\t6522                \tld\ta0,8(sp)\n"
                            "  42:\t65c2                \tld\ta1,16(sp)\n"
                            "  44:\t6662                \tld\ta2,24(sp)\n"
                            "  46:\t7f82                \tld\tt6,32(sp)\n"
                            "  48:\t7ea2                \tld\tt4,40(sp)\n"
                            "  4a:\t7e42                \tld\tt3,48(sp)\n"
                            "  4c:\t73e2                \tld\tt2,56(sp)\n"
                            "  4e:\t6306                \tld\tt1,64(sp)\n"
                            "  50:\t05010113          \taddi\tsp,sp,80"
                        >>,
                    jit_tests_common:assert_stream(riscv64, Dump, Stream)
                end)
            ]
        end}.

%% Test jump_to_continuation optimization for intra-module returns
jump_to_continuation_test_() ->
    [
        ?_test(begin
            % Test 1: jump_to_continuation at offset 0
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            State1 = ?BACKEND:jump_to_continuation(State0, {free, a0}),
            Stream = ?BACKEND:stream(State1),
            % Expected: riscv64 PIC sequence
            Dump =
                <<
                    "   0:  00000f97            auipc   t6,0x0\n"
                    "   4:  9faa                add t6,t6,a0\n"
                    "   6:  8f82                jr  t6"
                >>,
            jit_tests_common:assert_stream(riscv64, Dump, Stream)
        end),
        ?_test(begin
            % Test 2: jump_to_continuation after jump table (non-zero relative address)
            State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
            % Generate a jump table for 3 labels (4 entries * 8 bytes = 32 bytes)
            State1 = ?BACKEND:jump_table(State0, 3),
            State2 = ?BACKEND:jump_to_continuation(State1, {free, a0}),
            Stream = ?BACKEND:stream(State2),
            % Expected: jump table (32 bytes) + jump_to_continuation
            % NetOffset = 0 - 32 = -32 (0xFFFFFFE0)
            Dump =
                <<
                    "   0:  ffffffff            .insn   4, 0xffffffff\n"
                    "   4:  ffffffff            .insn   4, 0xffffffff\n"
                    "   8:  ffffffff            .insn   4, 0xffffffff\n"
                    "   c:  ffffffff            .insn   4, 0xffffffff\n"
                    "  10:  ffffffff            .insn   4, 0xffffffff\n"
                    "  14:  ffffffff            .insn   4, 0xffffffff\n"
                    "  18:  ffffffff            .insn   4, 0xffffffff\n"
                    "  1c:  ffffffff            .insn   4, 0xffffffff\n"
                    "  20:  00000f97            auipc   t6,0x0\n"
                    "  24:  1f81                addi    t6,t6,-32 # 0x0\n"
                    "  26:  9faa                add t6,t6,a0\n"
                    "  28:  8f82                jr  t6"
                >>,
            jit_tests_common:assert_stream(riscv64, Dump, Stream)
        end)
    ].

%% Mimic part of add.beam
add_beam_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    State1 = ?BACKEND:jump_table(State0, 3),
    State2 = ?BACKEND:add_label(State1, 1),
    State3 = ?BACKEND:move_to_vm_register(State2, 16#9f, {x_reg, 1}),
    State4 = ?BACKEND:move_to_vm_register(State3, 16#8f, {x_reg, 0}),
    State5 = ?BACKEND:call_only_or_schedule_next(State4, 2),
    State6 = ?BACKEND:add_label(State5, 2),
    {State7, ResultReg} = ?BACKEND:call_primitive(State6, ?PRIM_ALLOCATE, [
        ctx, jit_state, 1, 0, 1
    ]),
    State8 = ?BACKEND:if_block(State7, {'(bool)', {free, ResultReg}, '==', false}, fun(BSt0) ->
        ?BACKEND:call_primitive_last(BSt0, ?PRIM_HANDLE_ERROR, [ctx, jit_state, offset])
    end),
    State9 = ?BACKEND:move_to_vm_register(State8, ?TERM_NIL, {y_reg, 0}),
    State10 = ?BACKEND:call_or_schedule_next(State9, 3),
    State11 = ?BACKEND:add_label(State10, 3),
    State12 = ?BACKEND:call_primitive_last(State11, ?PRIM_RETURN, [
        ctx, jit_state
    ]),
    % OP_INT_CALL_END
    State13 = ?BACKEND:add_label(State12, 0),
    State14 = ?BACKEND:call_primitive_last(State13, 1, [ctx, jit_state]),
    State15 = ?BACKEND:update_branches(State14),
    Stream = ?BACKEND:stream(State15),
    Dump =
        <<
            "   0:\t00000697          \tauipc\ta3,0x0\n"
            "   4:\t0fa68067          \tjr\t250(a3) # 0xfa\n"
            "   8:\t00000697          \tauipc\ta3,0x0\n"
            "   c:\t01868067          \tjr\t24(a3) # 0x20\n"
            "  10:\t00000697          \tauipc\ta3,0x0\n"
            "  14:\t04c68067          \tjr\t76(a3) # 0x5c\n"
            "  18:\t00000697          \tauipc\ta3,0x0\n"
            "  1c:\t0d868067          \tjr\t216(a3) # 0xf0\n"
            "  20:\t09f00f93          \tli\tt6,159\n"
            "  24:\t07f53023          \tsd\tt6,96(a0)\n"
            "  28:\t08f00f93          \tli\tt6,143\n"
            "  2c:\t05f53c23          \tsd\tt6,88(a0)\n"
            "  30:\t0105af83          \tlw\tt6,16(a1)\n"
            "  34:\t1ffd                \taddi\tt6,t6,-1\n"
            "  36:\t01f5a823          \tsw\tt6,16(a1)\n"
            "  3a:\t000f8663          \tbeqz\tt6,0x46\n"
            "  3e:\ta839                \tj\t0x5c\n"
            "  40:\t0001                \tnop\n"
            "  42:\t00000013          \tnop\n"
            "  46:\t00000f97          \tauipc\tt6,0x0\n"
            "  4a:\t0fd9                \taddi\tt6,t6,22 # 0x5c\n"
            "  4c:\t0001                \tnop\n"
            "  4e:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  52:\t4fc1                \tli\tt6,16\n"
            "  54:\t9fb2                \tadd\tt6,t6,a2\n"
            "  56:\t000fbf83          \tld\tt6,0(t6)\n"
            "  5a:\t8f82                \tjr\tt6\n"
            "  5c:\t02800f93          \tli\tt6,40\n"
            "  60:\t9fb2                \tadd\tt6,t6,a2\n"
            "  62:\t000fbf83          \tld\tt6,0(t6)\n"
            "  66:\t1101                \taddi\tsp,sp,-32\n"
            "  68:\te006                \tsd\tra,0(sp)\n"
            "  6a:\te42a                \tsd\ta0,8(sp)\n"
            "  6c:\te82e                \tsd\ta1,16(sp)\n"
            "  6e:\tec32                \tsd\ta2,24(sp)\n"
            "  70:\t4605                \tli\ta2,1\n"
            "  72:\t4681                \tli\ta3,0\n"
            "  74:\t4705                \tli\ta4,1\n"
            "  76:\t9f82                \tjalr\tt6\n"
            "  78:\t8faa                \tmv\tt6,a0\n"
            "  7a:\t6082                \tld\tra,0(sp)\n"
            "  7c:\t6522                \tld\ta0,8(sp)\n"
            "  7e:\t65c2                \tld\ta1,16(sp)\n"
            "  80:\t6662                \tld\ta2,24(sp)\n"
            "  82:\t02010113          \taddi\tsp,sp,32\n"
            "  86:\t03ff9f13          \tslli\tt5,t6,0x3f\n"
            "  8a:\t000f4a63          \tbltz\tt5,0x9e\n"
            "  8e:\t03000f93          \tli\tt6,48\n"
            "  92:\t9fb2                \tadd\tt6,t6,a2\n"
            "  94:\t000fbf83          \tld\tt6,0(t6)\n"
            "  98:\t09800613          \tli\ta2,152\n"
            "  9c:\t8f82                \tjr\tt6\n"
            "  9e:\t03b00f13          \tli\tt5,59\n"
            "  a2:\t05053f83          \tld\tt6,80(a0)\n"
            "  a6:\t01efb023          \tsd\tt5,0(t6)\n"
            "  aa:\t0005bf03          \tld\tt5,0(a1)\n"
            "  ae:\t000f2f03          \tlw\tt5,0(t5)\n"
            "  b2:\t0f62                \tslli\tt5,t5,0x18\n"
            "  b4:\t3c000f93          \tli\tt6,960\n"
            "  b8:\t00000013          \tnop\n"
            "  bc:\t01ff6f33          \tor\tt5,t5,t6\n"
            "  c0:\t0fe53023          \tsd\tt5,224(a0)\n"
            "  c4:\t0105af83          \tlw\tt6,16(a1)\n"
            "  c8:\t1ffd                \taddi\tt6,t6,-1\n"
            "  ca:\t01f5a823          \tsw\tt6,16(a1)\n"
            "  ce:\t000f8663          \tbeqz\tt6,0xda\n"
            "  d2:\ta839                \tj\t0xf0\n"
            "  d4:\t0001                \tnop\n"
            "  d6:\t00000013          \tnop\n"
            "  da:\t00000f97          \tauipc\tt6,0x0\n"
            "  de:\t0fd9                \taddi\tt6,t6,22 # 0xf0\n"
            "  e0:\t0001                \tnop\n"
            "  e2:\t01f5b423          \tsd\tt6,8(a1)\n"
            "  e6:\t4fc1                \tli\tt6,16\n"
            "  e8:\t9fb2                \tadd\tt6,t6,a2\n"
            "  ea:\t000fbf83          \tld\tt6,0(t6)\n"
            "  ee:\t8f82                \tjr\tt6\n"
            "  f0:\t4fa1                \tli\tt6,8\n"
            "  f2:\t9fb2                \tadd\tt6,t6,a2\n"
            "  f4:\t000fbf83          \tld\tt6,0(t6)\n"
            "  f8:\t8f82                \tjr\tt6\n"
            "  fa:\t4fa1                \tli\tt6,8\n"
            "  fc:\t9fb2                \tadd\tt6,t6,a2\n"
            "  fe:\t000fbf83          \tld\tt6,0(t6)\n"
            " 102:\t8f82                \tjr\tt6"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).

%% After freeing a register, cache is preserved so reload is elided
cached_load_after_free_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {State1, t6} = ?BACKEND:move_to_native_register(State0, {x_reg, 0}),
    State2 = ?BACKEND:free_native_registers(State1, [t6]),
    {State3, t6} = ?BACKEND:move_to_native_register(State2, {x_reg, 0}),
    Stream = ?BACKEND:stream(State3),
    Dump =
        <<
            "   0:\t05853f83          \tld\tt6,88(a0)"
        >>,
    jit_tests_common:assert_stream(riscv64, Dump, Stream).
