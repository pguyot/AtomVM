%
% This file is part of AtomVM.
%
% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

-module(jit_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").

-define(CODE_CHUNK_0,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 177, 0, 0, 0, 7, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 0,
        1, 32, 64, 50, 3, 19, 1, 48, 153, 0, 2, 18, 66, 0, 1, 64, 64, 18, 3, 78, 16, 0, 1, 80, 153,
        0, 2, 18, 66, 16, 1, 96, 64, 3, 19, 64, 18, 3, 78, 32, 16, 3>>
).

% Code chunk with typed register from test_term_to_int.erl
% Contains bs_get_binary2 opcode with typed register that uses term_to_int optimization
-define(CODE_CHUNK_1,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 182, 0, 0, 0, 4, 0, 0, 0, 1, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 45, 21, 19, 166, 53, 3, 32, 35, 117, 53, 87, 35, 16, 48, 87, 19, 32, 16, 0, 19, 182,
        53, 35, 23, 32, 50, 0, 64, 19, 3, 19, 1, 48, 153, 32, 72, 3, 3>>
).
-define(ATU8_CHUNK_1,
    <<255, 255, 255, 253, 8, 16, 116, 101, 115, 116, 95, 116, 101, 114, 109, 95, 116, 111, 95, 105,
        110, 116, 144, 101, 120, 116, 114, 97, 99, 116, 95, 105, 224, 101, 110, 115, 117, 114, 101,
        95, 101, 120, 97, 99, 116, 108, 121>>
).
-define(TYPE_CHUNK_1,
    <<0, 0, 0, 3, 0, 0, 0, 3, 15, 255, 0, 2, 0, 32>>
).

% Code chunk with typed register from test_call_simple.erl
% Contains call_fun2 opcode with typed register that uses verify_is_function optimization
-define(CODE_CHUNK_2,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 3, 0, 0, 0, 1, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 77, 21, 19, 12, 0, 32, 153, 32, 178, 50, 16, 87, 19, 16, 18, 0, 19, 3>>
).
-define(ATU8_CHUNK_2,
    <<255, 255, 255, 253, 8, 16, 116, 101, 115, 116, 95, 99, 97, 108, 108, 95, 115, 105, 109, 112,
        108, 101, 144, 116, 101, 115, 116, 95, 99, 97, 108, 108, 96, 117, 110, 115, 97, 102, 101>>
).
-define(TYPE_CHUNK_2,
    <<0, 0, 0, 3, 0, 0, 0, 2, 15, 255, 0, 16>>
).

compile_minimal_x86_64_test() ->
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_0,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_0,
        fun(_) -> undefined end,
        fun(_) -> undefined end,
        fun(_) -> any end,
        fun(_) -> undefined end,
        jit_x86_64,
        Stream2
    ),
    Stream4 = jit_x86_64:stream(Stream3),
    <<16:32, LabelsCount:32, ?JIT_FORMAT_VERSION:16, 1:16, ?JIT_ARCH_X86_64:16, ?JIT_VARIANT_PIC:16,
        0:32, Code/binary>> = Stream4,
    {JumpTable, _} = split_binary(Code, (LabelsCount + 1) * 5),
    ok = check_x86_64_jt(JumpTable),
    <<16#E9, LabelsLinesTable0:32/little, _/binary>> = JumpTable,
    {_, LabelsLinesCode0} = split_binary(Code, LabelsLinesTable0 + 5),
    {LabelsLinesCode, LabelsLinesTable} = split_binary(LabelsLinesCode0, 8),
    % 48 8d 05 01 00 00 00 	lea    0x1(%rip),%rax
    % c3                   	retq
    ?assertEqual(<<16#48, 16#8D, 16#05, 1:32/little, 16#C3>>, LabelsLinesCode),
    {ok, LinesTable} = check_labels_table(LabelsCount, LabelsLinesTable),
    ok = check_lines_table(LinesTable),
    ok.

check_x86_64_jt(<<>>) -> ok;
check_x86_64_jt(<<16#e9, _Offset:32/little, Tail/binary>>) -> check_x86_64_jt(Tail);
check_x86_64_jt(Bin) -> {unexpected, Bin}.

check_labels_table(LabelsCount, <<LabelsCount:16, Labels:(LabelsCount * 6)/binary, Rest/binary>>) ->
    ok = check_labels_table0(1, Labels),
    {ok, Rest}.

check_labels_table0(_, <<>>) -> ok;
check_labels_table0(N, <<N:16, _Offset:32, Rest/binary>>) -> check_labels_table0(N + 1, Rest).

check_lines_table(<<LinesCount:16, _Lines:(LinesCount * 6)/binary>>) -> ok.

term_to_int_verify_is_match_state_typed_optimization_x86_64_test() ->
    % Compile CODE_CHUNK_1 which contains a typed register for term_to_int optimization
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_1,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_1),
    LiteralResolver = fun(_) -> test_literal end,
    TypeResolver = jit_precompile:type_resolver(?TYPE_CHUNK_1),
    ImportResolver = fun(_) -> test_function end,

    % Compile with typed register support
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_1,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        jit_x86_64,
        Stream2
    ),
    CompiledCode = jit_x86_64:stream(Stream3),

    % Check the reading of x[1] is immediatly followed by a shift right.
    % 15c:	4c 8b 5f 38          	mov    0x38(%rdi),%r11
    % 160:	49 c1 eb 04          	shr    $0x4,%r11

    % As opposed to testing its type
    % 15c:	4c 8b 5f 38          	mov    0x38(%rdi),%r11
    % 160:	4d 89 da             	mov    %r11,%r10
    % 163:	41 80 e2 0f          	and    $0xf,%r10b
    % 167:	41 80 fa 0f          	cmp    $0xf,%r10b
    % 16b:	74 05                	je     0x172
    % 16d:	e9 ab 00 00 00       	jmpq   0x21d
    % 172:	49 c1 eb 04          	shr    $0x4,%r11
    ?assertMatch(
        {_, 8},
        binary:match(CompiledCode, <<16#4c, 16#8b, 16#5f, 16#38, 16#49, 16#c1, 16#eb, 16#04>>)
    ),

    % Check call to bs_start_match3 is followed by a skip of verify_is_boxed
    %  100:	48 8b 77 30          	mov    0x30(%rdi),%rsi
    %  104:	48 c7 c2 00 00 00 00 	mov    $0x0,%rdx
    %  10b:	ff d0                	callq  *%rax
    %  10d:	5a                   	pop    %rdx
    %  10e:	5e                   	pop    %rsi
    %  10f:	5f                   	pop    %rdi
    %  110:	48 89 47 40          	mov    %rax,0x40(%rdi)
    %  114:	48 8b 47 40          	mov    0x40(%rdi),%rax
    %  118:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax

    % As opposed to:
    %  100:	48 8b 77 30          	mov    0x30(%rdi),%rsi
    %  104:	48 c7 c2 00 00 00 00 	mov    $0x0,%rdx
    %  10b:	ff d0                	callq  *%rax
    %  10d:	5a                   	pop    %rdx
    %  10e:	5e                   	pop    %rsi
    %  10f:	5f                   	pop    %rdi
    %  110:	48 89 47 40          	mov    %rax,0x40(%rdi)
    %  114:	48 8b 47 40          	mov    0x40(%rdi),%rax
    %  118:	49 89 c3             	mov    %rax,%r11
    %  11b:	41 80 e3 03          	and    $0x3,%r11b
    %  11f:	41 80 fb 02          	cmp    $0x2,%r11b
    %  123:	74 13                	je     0x138
    %  125:	48 8b 02             	mov    (%rdx),%rax
    %  128:	48 c7 c2 28 01 00 00 	mov    $0x128,%rdx
    %  12f:	48 c7 c1 0b 01 00 00 	mov    $0x10b,%rcx
    %  136:	ff e0                	jmpq   *%rax
    %  138:	48 83 e0 fc          	and    $0xfffffffffffffffc,%rax
    ?assertMatch(
        {_, 28},
        binary:match(
            CompiledCode,
            <<16#48, 16#8b, 16#77, 16#30, 16#48, 16#c7, 16#c2, 16#00, 16#00, 16#00, 16#00, 16#ff,
                16#d0, 16#5a, 16#5e, 16#5f, 16#48, 16#89, 16#47, 16#40, 16#48, 16#8b, 16#47, 16#40,
                16#48, 16#83, 16#e0, 16#fc>>
        )
    ),

    ok.

verify_is_function_typed_optimization_x86_64_test() ->
    % Compile CODE_CHUNK_1 which contains a typed register for term_to_int optimization
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_2,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_2),
    LiteralResolver = fun(_) -> test_literal end,
    TypeResolver = jit_precompile:type_resolver(?TYPE_CHUNK_2),
    ImportResolver = fun(_) -> test_function end,

    % Compile with typed register support
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_2,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        jit_x86_64,
        Stream2
    ),
    CompiledCode = jit_x86_64:stream(Stream3),

    % Check that call to allocate is directly followed by the building the cp
    % for call
    % b6:	48 8b 42 10          	mov    0x10(%rdx),%rax
    % ba:	ff e0                	jmpq   *%rax
    % bc:	48 8b 47 38          	mov    0x38(%rdi),%rax
    % c0:	4c 8b 1e             	mov    (%rsi),%r11
    % c3:	45 8b 1b             	mov    (%r11),%r11d
    % c6:	49 c1 e3 18          	shl    $0x18,%r11
    % ...

    % As opposed to:
    % b6:	48 8b 42 10          	mov    0x10(%rdx),%rax
    % ba:	ff e0                	jmpq   *%rax
    % bc:	48 8b 47 38          	mov    0x38(%rdi),%rax
    % c0:	49 89 c3             	mov    %rax,%r11
    % c3:	4d 89 da             	mov    %r11,%r10
    % c6:	41 80 e2 03          	and    $0x3,%r10b
    % ca:	41 80 fa 02          	cmp    $0x2,%r10b
    % ce:	74 1a                	je     0xea
    % d0:	48 8b 82 98 00 00 00 	mov    0x98(%rdx),%rax
    % d7:	48 c7 c2 d7 00 00 00 	mov    $0xd7,%rdx
    % de:	48 c7 c1 8b 01 00 00 	mov    $0x18b,%rcx
    % e5:	4d 89 d8             	mov    %r11,%r8
    % e8:	ff e0                	jmpq   *%rax
    % ea:	49 83 e3 fc          	and    $0xfffffffffffffffc,%r11
    % ee:	4d 8b 1b             	mov    (%r11),%r11
    % f1:	4d 89 da             	mov    %r11,%r10
    % f4:	41 80 e2 3f          	and    $0x3f,%r10b
    % f8:	41 80 fa 14          	cmp    $0x14,%r10b
    % fc:	74 1a                	je     0x118
    % fe:	48 8b 82 98 00 00 00 	mov    0x98(%rdx),%rax
    % 105:	48 c7 c2 05 01 00 00 	mov    $0x105,%rdx
    % 10c:	48 c7 c1 8b 01 00 00 	mov    $0x18b,%rcx
    % 113:	4d 89 d8             	mov    %r11,%r8
    % 116:	ff e0                	jmpq   *%rax
    % 118:	4c 8b 1e             	mov    (%rsi),%r11
    % 11b:	45 8b 1b             	mov    (%r11),%r11d
    % 11e:	49 c1 e3 18          	shl    $0x18,%r11
    % ...

    ?assertMatch(
        {_, 20},
        binary:match(
            CompiledCode,
            <<16#48, 16#8b, 16#42, 16#10, 16#ff, 16#e0, 16#48, 16#8b, 16#47, 16#38, 16#4c, 16#8b,
                16#1e, 16#45, 16#8b, 16#1b, 16#49, 16#c1, 16#e3, 16#18>>
        )
    ),
    ok.

% Code chunk from raise_error.erl - tests tail cache optimization for error handlers
% Contains two functions f/1 and g/1 that both raise function_clause errors
% The second error handler should reuse cached code from the first
-define(CODE_CHUNK_RAISE_ERROR,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#b1, 16#00,
        16#00, 16#00, 16#05, 16#00, 16#00, 16#00, 16#02, 16#01, 16#10, 16#99, 16#10, 16#02, 16#12,
        16#22, 16#10, 16#01, 16#20, 16#b1, 16#05, 16#00, 16#10, 16#08, 16#20, 16#03, 16#17, 16#60,
        16#32, 16#10, 16#10, 16#02, 16#03, 16#09, 16#20, 16#13, 16#01, 16#30, 16#99, 16#20, 16#02,
        16#12, 16#42, 16#10, 16#01, 16#40, 16#b1, 16#05, 16#00, 16#10, 16#08, 16#10, 16#03, 16#17,
        16#60, 16#32, 16#10, 16#10, 16#02, 16#03, 16#09, 16#10, 16#13, 16#03>>
).
-define(ATU8_CHUNK_RAISE_ERROR,
    <<16#ff, 16#ff, 16#ff, 16#fc, 16#b0, 16#72, 16#61, 16#69, 16#73, 16#65, 16#5f, 16#65, 16#72,
        16#72, 16#6f, 16#72, 16#10, 16#66, 16#70, 16#69, 16#6e, 16#74, 16#65, 16#67, 16#65, 16#72,
        16#10, 16#67>>
).

tail_cache_raise_error_x86_64_test() ->
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_RAISE_ERROR,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_X86_64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_x86_64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_RAISE_ERROR),
    LiteralResolver = fun(_) -> undefined end,
    TypeResolver = fun(_) -> any end,
    ImportResolver = fun(_) -> undefined end,

    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_RAISE_ERROR,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        jit_x86_64,
        Stream2
    ),
    CompiledCode = jit_x86_64:stream(Stream3),

    % The test verifies that function g's error handler reuses the cached code from function f
    % Both functions should generate function_clause errors with the same structure
    %
    % Find the first occurrence of call .+5 (e8 00 00 00 00) which is the start of label 1
    {Label1Offset, _} = binary:match(CompiledCode, <<16#e8, 16#00, 16#00, 16#00, 16#00>>),

    % Extract label 1 code (18 bytes: call=5 + mov=7 + mov=3 + pop=1 + jmp=2)
    <<_Skip:Label1Offset/binary, Label1Code:18/binary, _/binary>> = CompiledCode,

    % Verify label 1 matches expected disassembly
    % The exact offsets in comments will vary, but the instruction bytes are what matter
    Label1Dump = <<
        "   0:    e8 00 00 00 00           callq  0x5\n"
        "   5:    48 c7 c1 cb 01 00 00     mov    $0x1cb,%rcx\n"
        "   c:    48 8b 02                 mov    (%rdx),%rax\n"
        "   f:    5a                       pop    %rdx\n"
        "  10:    ff e0                    jmpq   *%rax"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(Label1Dump), Label1Code),

    % Find another factorized error handler that reuses part of label 1
    % This should have: call .+5, mov with different error code, then jmp back to label 1's continuation
    {FactorizedOffset, _} = binary:match(
        binary:part(CompiledCode, Label1Offset + 18, byte_size(CompiledCode) - Label1Offset - 18),
        <<16#e8, 16#00, 16#00, 16#00, 16#00>>
    ),
    ActualFactorizedOffset = Label1Offset + 18 + FactorizedOffset,

    % Extract factorized code (14 bytes: call=5 + mov=7 + jmp=2)
    % The jmp should be a short jump (eb) back to the continuation of label 1
    <<_Skip2:ActualFactorizedOffset/binary, FactorizedCode:14/binary, _/binary>> = CompiledCode,

    % The factorized handler has a different error code but reuses label 1's tail
    FactorizedDump = <<
        "   0:    e8 00 00 00 00           callq  0x5\n"
        "   5:    48 c7 c1 0b 01 00 00     mov    $0x10b,%rcx\n"
        "   c:    eb ce                    jmp    0x-30"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(FactorizedDump), FactorizedCode),

    % Check that jump table entry for label 3 (function_clause for g) points to a call to the cached handler
    % The header is 20 bytes, then jump table entries are 5 bytes each
    % Entry for label 3 is at offset 20 + 3*5 = 35
    JT3EntryOffset = 20 + 3 * 5,
    <<_:JT3EntryOffset/binary, 16#e9, Entry3Offset:32/little-signed, _/binary>> = CompiledCode,
    % Jump entry location + jmp size + relative offset
    Entry3Target = JT3EntryOffset + 5 + Entry3Offset,

    % Extract the code at entry 3 (should start with a call instruction)
    <<_Skip3:Entry3Target/binary, Entry3Code:5/binary, _/binary>> = CompiledCode,

    % Entry 3 should be a call that jumps back to the cached handler at label 1
    % Call format: e8 <4-byte signed offset relative to next instruction>
    <<16#e8, CallOffset:32/little-signed>> = Entry3Code,
    CallTarget = Entry3Target + 5 + CallOffset,

    % The call should target the first mov instruction in label 1 (after the initial call .+5)
    % which is at Label1Offset + 5
    ?assertEqual(Label1Offset + 5, CallTarget),
    ok.

tail_cache_raise_error_aarch64_test() ->
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_RAISE_ERROR,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_AARCH64, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_aarch64:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_RAISE_ERROR),
    LiteralResolver = fun(_) -> undefined end,
    TypeResolver = fun(_) -> any end,
    ImportResolver = fun(_) -> undefined end,

    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_RAISE_ERROR,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        jit_aarch64,
        Stream2
    ),
    CompiledCode = jit_aarch64:stream(Stream3),

    % Find the first occurrence of bl .+4 (01 00 00 94) which is the start of label 1
    {Label1Offset, _} = binary:match(CompiledCode, <<16#01, 16#00, 16#00, 16#94>>),

    % Extract label 1 code (20 bytes: bl=4 + mov=4 + ldr=4 + mov=4 + br=4)
    <<_Skip:Label1Offset/binary, Label1Code:20/binary, _/binary>> = CompiledCode,

    % Verify label 1 matches expected disassembly
    % AArch64: bl .+4, mov x3 #0x1cb, ldr x7 [x2], mov x2 x30, br x7
    Label1Dump = <<
        "   0:	94000001 	bl	0x4\n"
        "   4:	d2803963 	mov	x3, #0x1cb\n"
        "   8:	f9400047 	ldr	x7, [x2]\n"
        "   c:	aa1e03e2 	mov	x2, x30\n"
        "  10:	d61f00e0 	br	x7"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(Label1Dump), Label1Code),

    % Find another factorized error handler that reuses part of label 1
    {FactorizedOffset, _} = binary:match(
        binary:part(CompiledCode, Label1Offset + 20, byte_size(CompiledCode) - Label1Offset - 20),
        <<16#01, 16#00, 16#00, 16#94>>
    ),
    ActualFactorizedOffset = Label1Offset + 20 + FactorizedOffset,

    % Extract factorized code (12 bytes: bl=4 + mov=4 + b=4)
    <<_Skip2:ActualFactorizedOffset/binary, FactorizedCode:12/binary, _/binary>> = CompiledCode,

    % The factorized handler has a different error code but branches back to label 1's continuation
    FactorizedDump = <<
        "   0:	94000001 	bl	0x4\n"
        "   4:	d2802163 	mov	x3, #0x10b\n"
        "   8:	17fffff4 	b	0xffffffffffffffd8"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(FactorizedDump), FactorizedCode),

    % Check that jump table entry for label 3 (function_clause for g) points to a call to the cached handler
    % The header is 20 bytes, then jump table entries are 4 bytes each
    % Entry for label 3 is at offset 20 + 3*4 = 32
    JT3EntryOffset = 20 + 3 * 4,
    <<_:JT3EntryOffset/binary, JT3Entry:4/binary, _/binary>> = CompiledCode,

    % Decode the branch instruction (b opcode = 0x14)
    % Instruction format: bits [31:26]=opcode (000101), bits [25:0]=signed offset
    <<JT3Instr:32/little>> = JT3Entry,
    Opcode = (JT3Instr bsr 26) band 16#3F,
    % Verify it's a B instruction
    ?assertEqual(16#05, Opcode),
    % Extract 26-bit signed offset
    Offset = JT3Instr band 16#03FFFFFF,
    % Sign-extend from 26 bits to full signed integer
    SignExtendedOffset =
        case Offset band 16#02000000 of
            % Positive
            0 -> Offset;
            % Negative (two's complement)
            _ -> Offset - 16#04000000
        end,
    Entry3Target = JT3EntryOffset + (SignExtendedOffset * 4),

    % Extract the code at entry 3 (should start with a bl instruction)
    <<_Skip3:Entry3Target/binary, Entry3Code:4/binary, _/binary>> = CompiledCode,

    % Entry 3 should be a bl that jumps back to the cached handler at label 1
    % BL instruction format: bits [31:26]=opcode (100101), bits [25:0]=signed offset
    <<BlInstr:32/little>> = Entry3Code,
    BlOpcode = (BlInstr bsr 26) band 16#3F,
    % Verify it's a BL instruction
    ?assertEqual(16#25, BlOpcode),
    % Extract 26-bit signed offset
    BlOffset = BlInstr band 16#03FFFFFF,
    % Sign-extend from 26 bits to full signed integer
    SignExtendedBlOffset =
        case BlOffset band 16#02000000 of
            % Positive
            0 -> BlOffset;
            % Negative (two's complement)
            _ -> BlOffset - 16#04000000
        end,
    BlTarget = Entry3Target + (SignExtendedBlOffset * 4),

    % The bl should target the first mov instruction in label 1 (after the initial bl .+4)
    % which is at Label1Offset + 4
    ?assertEqual(Label1Offset + 4, BlTarget),
    ok.

tail_cache_raise_error_riscv32_test() ->
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_RAISE_ERROR,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_RISCV32, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_riscv32:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_RAISE_ERROR),
    LiteralResolver = fun(_) -> undefined end,
    TypeResolver = fun(_) -> any end,
    ImportResolver = fun(_) -> undefined end,

    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_RAISE_ERROR,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        jit_riscv32,
        Stream2
    ),
    CompiledCode = jit_riscv32:stream(Stream3),

    % Find the first occurrence of jal 4 (11 20) which is the start of label 1
    {Label1Offset, _} = binary:match(CompiledCode, <<16#11, 16#20>>),

    % Extract label 1 code (14 bytes: mix of compressed and standard RISC-V instructions)
    % RISC-V32 uses: jal + li + lw + mv + jr
    <<_Skip:Label1Offset/binary, Label1Code:14/binary, _/binary>> = CompiledCode,

    % Verify label 1 matches expected disassembly
    % RISC-V32 uses compressed (16-bit) and standard (32-bit) instructions
    % jal 4 is encoded as 0x2011 (compressed c.jal instruction)
    Label1Dump = <<
        "   0:	2011                	jal	4\n"
        "   2:	1cb00693          	li	a3,459\n"
        "   6:	00062f83          	lw	t6,0(a2)\n"
        "   a:	8606                	mv	a2,ra\n"
        "   c:	8f82                	jr	t6"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(Label1Dump), Label1Code),

    % Find another factorized error handler that reuses part of label 1
    {FactorizedOffset, _} = binary:match(
        binary:part(CompiledCode, Label1Offset + 14, byte_size(CompiledCode) - Label1Offset - 14),
        <<16#11, 16#20>>
    ),
    ActualFactorizedOffset = Label1Offset + 14 + FactorizedOffset,

    % Extract factorized code (8 bytes: jal + li + jump back)
    <<_Skip2:ActualFactorizedOffset/binary, FactorizedCode:8/binary, _/binary>> = CompiledCode,

    % The factorized handler has a different error code (267) but jumps back to label 1's continuation
    FactorizedDump = <<
        "   0:	2011                	jal	4\n"
        "   2:	10b00693          	li	a3,267\n"
        "   6:	bfd9                	j	0xffffffffffffffdc"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(FactorizedDump), FactorizedCode),

    ok.

tail_cache_raise_error_armv6m_test() ->
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = ?CODE_CHUNK_RAISE_ERROR,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, ?JIT_ARCH_ARMV6M, ?JIT_VARIANT_PIC)
    ),
    Stream2 = jit_armv6m:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),

    AtomResolver = jit_precompile:atom_resolver(?ATU8_CHUNK_RAISE_ERROR),
    LiteralResolver = fun(_) -> undefined end,
    TypeResolver = fun(_) -> any end,
    ImportResolver = fun(_) -> undefined end,

    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_RAISE_ERROR,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        jit_armv6m,
        Stream2
    ),
    CompiledCode = jit_armv6m:stream(Stream3),

    % Find the first occurrence of mov lr, pc (FE 46) which is the start of label 1
    {Label1Offset, _} = binary:match(CompiledCode, <<16#FE, 16#46>>),

    % Extract label 1 code (24 bytes: 18 bytes instructions + 2 bytes alignment + 4 bytes literal)
    % ARMv6-M uses: mov lr,pc + ldr r3,[pc,#N] + ldr r7,[r2,#0] + manipulation + pop + literal
    <<_Skip:Label1Offset/binary, Label1Code:24/binary, _/binary>> = CompiledCode,

    % Verify label 1 matches expected disassembly
    % ARMv6-M Thumb instructions (16-bit each) with literal pool
    % Note: the literal pool value at 0x14 is 4 bytes but objdump shows it as separate halfwords
    Label1Dump = <<
        "   0:	46fe      	mov	lr, pc\n"
        "   2:	4b04      	ldr	r3, [pc, #16]\n"
        "   4:	6817      	ldr	r7, [r2, #0]\n"
        "   6:	03e2      	lsls	r2, r4, #15\n"
        "   8:	aa0e      	add	r2, sp, #56\n"
        "   a:	9e05      	ldr	r6, [sp, #20]\n"
        "   c:	9705      	str	r7, [sp, #20]\n"
        "   e:	46b6      	mov	lr, r6\n"
        "  10:	bdf2      	pop	{r1, r4, r5, r6, r7, pc}\n"
        "  12:	0000      	movs	r0, r0\n"
        "  14:	01cb 0000 	.word	0x000001cb"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(Label1Dump), Label1Code),

    % Find another factorized error handler that reuses part of label 1
    {FactorizedOffset, _} = binary:match(
        binary:part(CompiledCode, Label1Offset + 24, byte_size(CompiledCode) - Label1Offset - 24),
        <<16#FE, 16#46>>
    ),
    ActualFactorizedOffset = Label1Offset + 24 + FactorizedOffset,

    % Extract factorized code (10 bytes: 6 bytes instructions + 4 bytes literal)
    % mov lr,pc + ldr r3,[pc,#0] + branch back + literal
    <<_Skip2:ActualFactorizedOffset/binary, FactorizedCode:10/binary, _/binary>> = CompiledCode,

    % The factorized handler has a different error code but branches back to label 1's continuation
    % The literal value 0x010b (267) is right after the branch instruction
    FactorizedDump = <<
        "   0:	46fe      	mov	lr, pc\n"
        "   2:	4b00      	ldr	r3, [pc, #0]\n"
        "   4:	e7e9      	b.n	0xffffffda\n"
        "   6:	010b 0000 	.word	0x0000010b"
    >>,
    ?assertEqual(jit_tests_common:dump_to_bin(FactorizedDump), FactorizedCode),

    ok.
