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

% Code chunk from bool_min2.erl - tests tail-call cache optimization
% This module has multiple return opcodes which trigger the tail-call cache:
% - The first return creates a cached implementation
% - Subsequent returns use jump_to_offset to jump back to the cached code
-define(CODE_CHUNK_3,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B2, 16#00,
        16#00, 16#00, 16#09, 16#00, 16#00, 16#00, 16#03, 16#01, 16#10, 16#99, 16#10, 16#02, 16#12,
        16#22, 16#00, 16#01, 16#20, 16#0C, 16#10, 16#00, 16#AC, 16#17, 16#10, 16#04, 16#40, 16#32,
        16#23, 16#40, 16#32, 16#33, 16#40, 16#32, 16#13, 16#40, 16#42, 16#43, 16#40, 16#32, 16#03,
        16#99, 16#20, 16#04, 16#50, 16#45, 16#04, 16#10, 16#65, 16#40, 16#03, 16#04, 16#40, 16#42,
        16#23, 16#40, 16#42, 16#33, 16#40, 16#32, 16#13, 16#40, 16#42, 16#43, 16#40, 16#42, 16#03,
        16#99, 16#30, 16#04, 16#50, 16#45, 16#04, 16#10, 16#65, 16#99, 16#20, 16#7D, 16#05, 16#10,
        16#00, 16#57, 16#04, 16#10, 16#57, 16#03, 16#10, 16#03, 16#12, 16#10, 16#13, 16#01, 16#30,
        16#99, 16#40, 16#02, 16#12, 16#72, 16#50, 16#01, 16#40, 16#99, 16#50, 16#0B, 16#05, 16#10,
        16#03, 16#13, 16#03, 16#0B, 16#05, 16#10, 16#23, 16#33, 16#13, 16#0B, 16#05, 16#20, 16#57,
        16#03, 16#20, 16#57, 16#13, 16#20, 16#03, 16#0A, 16#05, 16#30, 16#43, 16#13, 16#0B, 16#05,
        16#20, 16#57, 16#03, 16#20, 16#57, 16#13, 16#20, 16#03, 16#13, 16#01, 16#50, 16#99, 16#60,
        16#02, 16#12, 16#B2, 16#10, 16#01, 16#60, 16#3B, 16#03, 16#55, 16#17, 16#40, 16#32, 16#85,
        16#42, 16#75, 16#01, 16#70, 16#40, 16#11, 16#03, 16#13, 16#01, 16#80, 16#40, 16#01, 16#03,
        16#13, 16#03>>
).
-define(ATU8_CHUNK_3,
    <<16#FF, 16#FF, 16#FF, 16#F5, 16#90, 16#62, 16#6F, 16#6F, 16#6C, 16#5F, 16#6D, 16#69, 16#6E,
        16#32, 16#50, 16#73, 16#74, 16#61, 16#72, 16#74, 16#50, 16#66, 16#61, 16#6C, 16#73, 16#65,
        16#40, 16#74, 16#72, 16#75, 16#65, 16#60, 16#65, 16#72, 16#6C, 16#61, 16#6E, 16#67, 16#10,
        16#2B, 16#10, 16#66, 16#30, 16#61, 16#6E, 16#64, 16#20, 16#6F, 16#72, 16#30, 16#6E, 16#6F,
        16#74, 16#B0, 16#6F, 16#6E, 16#65, 16#5F, 16#69, 16#66, 16#5F, 16#74, 16#72, 16#75, 16#65>>
).
-define(TYPE_CHUNK_3,
    <<16#00, 16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#03, 16#0F, 16#FF, 16#30, 16#20, 16#00,
        16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00,
        16#00, 16#01, 16#00, 16#01>>
).
-define(LINE_CHUNK_3,
    <<16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#07, 16#00,
        16#00, 16#00, 16#06, 16#00, 16#00, 16#00, 16#00, 16#41, 16#51, 16#61, 16#81, 16#91, 16#B1>>
).

% Code + atom chunks from test_eq_exact_atom.erl:
%   both(A, B) when is_atom(A), is_atom(B) ->
%       if A =:= B -> same; true -> different end.
% The is_eq_exact operands are typed registers annotated t_atom, exercising the
% immediate-typed exact-equality optimization (single native word compare).
-define(CODE_CHUNK_EQ_EXACT_ATOM,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 48, 21, 3, 48, 21, 19, 43, 53, 87, 3, 16, 87, 19, 16, 64, 50, 3, 19, 1, 48, 64, 66,
        3, 19, 1, 64, 153, 0, 2, 18, 82, 0, 1, 80, 64, 18, 3, 78, 16, 0, 1, 96, 153, 0, 2, 18, 82,
        16, 1, 112, 64, 3, 19, 64, 18, 3, 78, 32, 16, 3>>
).
-define(ATU8_CHUNK_EQ_EXACT_ATOM,
    <<0, 0, 0, 7, 18, 116, 101, 115, 116, 95, 101, 113, 95, 101, 120, 97, 99, 116, 95, 97, 116, 111,
        109, 4, 98, 111, 116, 104, 4, 115, 97, 109, 101, 9, 100, 105, 102, 102, 101, 114, 101, 110,
        116, 11, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 6, 101, 114, 108, 97, 110,
        103, 15, 103, 101, 116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
% Synthetic v4 Type chunk: two entries, both BEAM_TYPE_ATOM (0x0001) -> t_atom,
% so whichever type index the typed operands reference resolves to t_atom.
-define(TYPE_CHUNK_ALL_ATOM,
    <<4:32, 2:32, 16#00, 16#01, 16#00, 16#01>>
).

% Code + atom + import chunks from test_map_size.erl:
%   sz(M) when is_map(M) -> map_size(M).
% The map_size gc_bif operand is a typed register annotated t_map, exercising
% the inline map_size optimization.
-define(CODE_CHUNK_MAP_SIZE,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 181, 0, 0, 0, 7, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 16,
        1, 32, 156, 21, 3, 124, 5, 16, 0, 87, 3, 16, 3, 19, 1, 48, 153, 0, 2, 18, 82, 0, 1, 64, 64,
        18, 3, 78, 16, 16, 1, 80, 153, 0, 2, 18, 82, 16, 1, 96, 64, 3, 19, 64, 18, 3, 78, 32, 32,
        3>>
).
-define(ATU8_CHUNK_MAP_SIZE,
    <<255, 255, 255, 250, 208, 116, 101, 115, 116, 95, 109, 97, 112, 95, 115, 105, 122, 101, 32,
        115, 122, 96, 101, 114, 108, 97, 110, 103, 128, 109, 97, 112, 95, 115, 105, 122, 101, 176,
        109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103, 101, 116, 95, 109, 111, 100,
        117, 108, 101, 95, 105, 110, 102, 111>>
).
-define(IMPT_CHUNK_MAP_SIZE,
    <<0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 6, 0, 0, 0, 1, 0, 0, 0,
        3, 0, 0, 0, 6, 0, 0, 0, 2>>
).
% Real v4 Type chunk from that module: index 1 = BEAM_TYPE_MAP (0x0040) -> t_map.
-define(TYPE_CHUNK_MAP,
    <<0, 0, 0, 4, 0, 0, 0, 2, 16#1F, 16#FF, 16#00, 16#40>>
).

% Code + atom + import chunks from test_bif_eq.erl:
%   f(X, Y) when is_atom(X) -> X =:= Y.
%   g(X, Y) when is_atom(X) -> X =/= Y.
% Both comparisons are value-context bif2 calls whose first operand is a typed
% register annotated {t_atom, any}, exercising the immediate-typed inline.
-define(CODE_CHUNK_BIF_EQ,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 181, 0, 0, 0, 9, 0, 0, 0, 4, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 48, 21, 3, 11, 5, 0, 87, 3, 16, 19, 3, 19, 1, 48, 153, 32, 2, 18, 82, 32, 1, 64, 48,
        53, 3, 11, 5, 16, 87, 3, 16, 19, 3, 19, 1, 80, 153, 0, 2, 18, 114, 0, 1, 96, 64, 18, 3, 78,
        16, 32, 1, 112, 153, 0, 2, 18, 114, 16, 1, 128, 64, 3, 19, 64, 18, 3, 78, 32, 48, 3>>
).
-define(ATU8_CHUNK_BIF_EQ,
    <<255, 255, 255, 248, 176, 116, 101, 115, 116, 95, 98, 105, 102, 95, 101, 113, 16, 102, 96, 101,
        114, 108, 97, 110, 103, 48, 61, 58, 61, 16, 103, 48, 61, 47, 61, 176, 109, 111, 100, 117,
        108, 101, 95, 105, 110, 102, 111, 240, 103, 101, 116, 95, 109, 111, 100, 117, 108, 101, 95,
        105, 110, 102, 111>>
).
-define(IMPT_CHUNK_BIF_EQ,
    <<0, 0, 0, 4, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 6, 0, 0, 0, 2, 0, 0, 0,
        3, 0, 0, 0, 8, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 8, 0, 0, 0, 2>>
).
% Real v4 Type chunk from that module: index 1 = BEAM_TYPE_ATOM (0x0001) -> t_atom.
-define(TYPE_CHUNK_BIF_EQ,
    <<0, 0, 0, 4, 0, 0, 0, 2, 16#1F, 16#FF, 16#00, 16#01>>
).

-ifdef(JIT_DWARF).
compile_stream_setup(CodeChunk) ->
    compile_stream_setup_for_backend(jit_x86_64, CodeChunk).

compile_stream_setup_for_backend(Backend, CodeChunk) ->
    Arch = backend_to_arch(Backend),
    Stream0 = jit_dwarf:new(Backend, test_module, jit_stream_binary, 0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = CodeChunk,
    Stream1 = jit_dwarf:append(
        Stream0, jit:beam_chunk_header(LabelsCount, Arch, ?JIT_VARIANT_PIC)
    ),
    Stream2 = Backend:new(?JIT_VARIANT_PIC, jit_dwarf, Stream1),
    {LabelsCount, Stream2}.

compile_stream_finalize(Stream3) ->
    compile_stream_finalize_for_backend(jit_x86_64, Stream3).

compile_stream_finalize_for_backend(Backend, Stream3) ->
    DwarfStream = Backend:stream(Stream3),
    jit_dwarf:stream(DwarfStream).
-else.
compile_stream_setup(CodeChunk) ->
    compile_stream_setup_for_backend(jit_x86_64, CodeChunk).

compile_stream_setup_for_backend(Backend, CodeChunk) ->
    Arch = backend_to_arch(Backend),
    Stream0 = jit_stream_binary:new(0),
    <<16:32, 0:32, _OpcodeMax:32, LabelsCount:32, _FunctionsCount:32, _Opcodes/binary>> = CodeChunk,
    Stream1 = jit_stream_binary:append(
        Stream0, jit:beam_chunk_header(LabelsCount, Arch, ?JIT_VARIANT_PIC)
    ),
    Stream2 = Backend:new(?JIT_VARIANT_PIC, jit_stream_binary, Stream1),
    {LabelsCount, Stream2}.

compile_stream_finalize(Stream3) ->
    compile_stream_finalize_for_backend(jit_x86_64, Stream3).

compile_stream_finalize_for_backend(Backend, Stream3) ->
    Backend:stream(Stream3).
-endif.

compile_minimal_x86_64_test() ->
    {LabelsCount, Stream2} = compile_stream_setup(?CODE_CHUNK_0),
    {_LabelsCount, Stream3} = jit:compile(
        ?CODE_CHUNK_0,
        fun(_) -> undefined end,
        fun(_) -> undefined end,
        fun(_) -> any end,
        fun(_) -> undefined end,
        fun(_) -> false end,
        fun(_) -> undefined end,
        jit_x86_64,
        Stream2
    ),
    Stream4 = compile_stream_finalize(Stream3),
    <<16:32, LabelsCount:32, ?JIT_FORMAT_VERSION:16, 1:16, ?JIT_ARCH_X86_64:16, ?JIT_VARIANT_PIC:16,
        0:32, Code/binary>> = Stream4,
    {JumpTable, _} = split_binary(Code, (LabelsCount + 1) * 5),
    ok = check_x86_64_jt(JumpTable),
    <<16#E9, LabelsLinesTable0:32/little, _/binary>> = JumpTable,
    {_, LabelsLinesCode0} = split_binary(Code, LabelsLinesTable0 + 5),
    {LabelsLinesCode, LabelsLinesTable} = split_binary(LabelsLinesCode0, 8),
    % 48 8d 05 01 00 00 00 	lea    0x1(%rip),%rax
    % c3                   	retq
    <<16#48, 16#8D, 16#05, 1:32/little, 16#C3>> = LabelsLinesCode,
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

backend_to_arch(jit_x86_64) -> ?JIT_ARCH_X86_64;
backend_to_arch(jit_aarch64) -> ?JIT_ARCH_AARCH64;
backend_to_arch(jit_armv6m) -> ?JIT_ARCH_ARMV6M.

compile_stream_for_backend(Backend, CodeChunk, AtomChunk, TypeChunk) ->
    compile_stream_for_backend(Backend, CodeChunk, AtomChunk, TypeChunk, fun(_) ->
        {test, test, 2}
    end).

compile_stream_for_backend(Backend, CodeChunk, AtomChunk, TypeChunk, ImportResolver) ->
    {LabelsCount, Stream2} = compile_stream_setup_for_backend(Backend, CodeChunk),

    AtomResolver = jit_precompile:atom_resolver(AtomChunk),
    LiteralResolver = fun(_) -> test_literal end,
    TypeResolver = jit_precompile:type_resolver(TypeChunk),
    DebugResolver = fun(_) -> false end,

    RecordResolver = fun(_) -> undefined end,

    % Compile with typed register support
    {LabelsCount, Stream3} = jit:compile(
        CodeChunk,
        AtomResolver,
        LiteralResolver,
        TypeResolver,
        ImportResolver,
        DebugResolver,
        RecordResolver,
        Backend,
        Stream2
    ),
    compile_stream_finalize_for_backend(Backend, Stream3).

term_to_int_verify_is_match_state_typed_optimization_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?CODE_CHUNK_1, ?ATU8_CHUNK_1, ?TYPE_CHUNK_1
    ),

    % Check the reading of x[1] is immediatly followed by a shift right.
    % Untagging is an arithmetic shift (sar), so that a negative small integer
    % stays negative instead of becoming a large positive value. ctx is
    % pinned in r14.
    % 15c:	4d 8b 5e 60          	mov    0x60(%r14),%r11
    % 160:	49 c1 fb 04          	sar    $0x4,%r11

    % As opposed to testing its type
    % 15c:	4d 8b 5e 60          	mov    0x60(%r14),%r11
    % 160:	4d 89 da             	mov    %r11,%r10
    % 163:	41 80 e2 0f          	and    $0xf,%r10b
    % 167:	41 80 fa 0f          	cmp    $0xf,%r10b
    % 16b:	74 05                	je     0x172
    % 16d:	e9 ab 00 00 00       	jmpq   0x21d
    % 172:	49 c1 fb 04          	sar    $0x4,%r11
    ?assertMatch(
        {_, 8},
        binary:match(CompiledCode, <<16#4d, 16#8b, 16#5e, 16#60, 16#49, 16#c1, 16#fb, 16#04>>)
    ),

    % Check bs_start_match3 emits the match-state reuse fast path: load the
    % boxed header tag of the source and, when it already is a match state,
    % store the source itself to the destination, skipping the allocation.
    %   48 8b 47 58          	mov    0x58(%rdi),%rax
    %   48 83 e0 fc          	and    $0xfffffffffffffffc,%rax
    %   48 8b 00             	mov    (%rax),%rax
    %   83 e0 3f             	and    $0x3f,%eax
    %   83 f8 04             	cmp    $0x4,%eax
    %   75 0d                	jne    <alloc path>
    %   48 8b 47 58          	mov    0x58(%rdi),%rax
    %   48 89 47 68          	mov    %rax,0x68(%rdi)
    ?assertMatch(
        {_, 27},
        binary:match(
            CompiledCode,
            <<16#49, 16#8b, 16#46, 16#58, 16#48, 16#83, 16#e0, 16#fc, 16#48, 16#8b, 16#00, 16#83,
                16#e0, 16#3f, 16#83, 16#f8, 16#04, 16#75, 16#0d, 16#49, 16#8b, 16#46, 16#58, 16#49,
                16#89, 16#46, 16#68>>
        )
    ),

    % The allocation fallback is still emitted: call to term_alloc_bin_match_state
    % with the source as a gc-rooted argument, storing the result to the
    % destination.
    %   49 8b 7e 58          	mov    0x58(%r14),%rdi   (src = x[0])
    %   31 f6                	xor    %esi,%esi         (slots = 0)
    %   4d 89 66 18          	mov    %r12,0x18(%r14)   (write back hp)
    %   4d 89 7e 50          	mov    %r15,0x50(%r14)   (write back e)
    %   48 8b 83 68 01 00 00 	mov    0x168(%rbx),%rax  (term_alloc_bin_match_state)
    %   ff d0                	callq  *%rax
    %   41 5b                	pop    %r11
    %   4d 8b 66 18          	mov    0x18(%r14),%r12   (reload hp)
    %   4d 8b 7e 50          	mov    0x50(%r14),%r15   (reload e)
    %   49 89 46 68          	mov    %rax,0x68(%r14)   (result to x[2])
    ?assertMatch(
        {_, 37},
        binary:match(
            CompiledCode,
            <<16#49, 16#8b, 16#7e, 16#58, 16#31, 16#f6, 16#4d, 16#89, 16#66, 16#18, 16#4d, 16#89,
                16#7e, 16#50, 16#48, 16#8b, 16#83, 16#68, 16#01, 16#00, 16#00, 16#ff, 16#d0, 16#41,
                16#5b, 16#4d, 16#8b, 16#66, 16#18, 16#4d, 16#8b, 16#7e, 16#50, 16#49, 16#89, 16#46,
                16#68>>
        )
    ),

    ok.

verify_is_function_typed_optimization_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?CODE_CHUNK_2, ?ATU8_CHUNK_2, ?TYPE_CHUNK_2
    ),

    % Check that call to allocate is directly followed by the building the cp
    % for call
    % b6:	ff 62 10             	jmp    *0x10(%rdx)
    % b9:	48 8b 47 60          	mov    0x60(%rdi),%rax
    % bd:	4c 8b 1e             	mov    (%rsi),%r11
    % c0:	45 8b 1b             	mov    (%r11),%r11d
    % c3:	49 c1 e3 18          	shl    $0x18,%r11
    % ...

    % As opposed to:
    % b6:	48 8b 42 10          	mov    0x10(%rdx),%rax
    % ba:	ff e0                	jmpq   *%rax
    % bc:	48 8b 47 60          	mov    0x60(%rdi),%rax
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
        {_, 18},
        binary:match(
            CompiledCode,
            <<16#ff, 16#63, 16#10, 16#49, 16#8b, 16#46, 16#60, 16#4d, 16#8b, 16#5d, 16#00, 16#45,
                16#8b, 16#1b, 16#49, 16#c1, 16#e3, 16#18>>
        )
    ),
    ok.

%% is_eq_exact with an operand typed as an immediate (t_atom) compiles to a
%% single native word compare, skipping the runtime immediate-tag test and the
%% term_compare fallback of the untyped path. Verified by shrinkage: the typed
%% compile is strictly smaller than the untyped one. Runs across the backends
%% this module can target (see backend_to_arch/1) so the optimization is
%% exercised in more than one code generator. The is_ne_exact clause shares the
%% same helper (op_is_exact_eq_immediate).
is_eq_exact_atom_typed_optimization_test_() ->
    [
        {atom_to_list(Backend), fun() ->
            Untyped = compile_stream_for_backend(
                Backend, ?CODE_CHUNK_EQ_EXACT_ATOM, ?ATU8_CHUNK_EQ_EXACT_ATOM, <<>>
            ),
            Typed = compile_stream_for_backend(
                Backend, ?CODE_CHUNK_EQ_EXACT_ATOM, ?ATU8_CHUNK_EQ_EXACT_ATOM, ?TYPE_CHUNK_ALL_ATOM
            ),
            ?assert(byte_size(Typed) < byte_size(Untyped))
        end}
     || Backend <- [jit_x86_64, jit_aarch64, jit_armv6m]
    ].

%% map_size/1 on a value the Type chunk proves is a map compiles to an inline
%% size read (handling both the flat and tree map representations) instead of a
%% BIF call. Verified by shrinkage: the typed compile is strictly smaller than
%% the untyped one across the backends this module can target.
map_size_typed_optimization_test_() ->
    ImportResolver = jit_precompile:import_resolver(
        ?IMPT_CHUNK_MAP_SIZE, jit_precompile:atom_resolver(?ATU8_CHUNK_MAP_SIZE)
    ),
    [
        {atom_to_list(Backend), fun() ->
            Untyped = compile_stream_for_backend(
                Backend, ?CODE_CHUNK_MAP_SIZE, ?ATU8_CHUNK_MAP_SIZE, <<>>, ImportResolver
            ),
            Typed = compile_stream_for_backend(
                Backend, ?CODE_CHUNK_MAP_SIZE, ?ATU8_CHUNK_MAP_SIZE, ?TYPE_CHUNK_MAP, ImportResolver
            ),
            ?assert(byte_size(Typed) < byte_size(Untyped))
        end}
     || Backend <- [jit_x86_64, jit_aarch64, jit_armv6m]
    ].

%% Value-context '=:='/'=/=' (bif2) with an operand the Type chunk proves is an
%% atom compiles to an inline word compare selecting true/false instead of the
%% BIF call. Verified by shrinkage: the typed compile is strictly smaller than
%% the untyped one across the backends this module can target.
bif_eq_exact_atom_typed_optimization_test_() ->
    ImportResolver = jit_precompile:import_resolver(
        ?IMPT_CHUNK_BIF_EQ, jit_precompile:atom_resolver(?ATU8_CHUNK_BIF_EQ)
    ),
    [
        {atom_to_list(Backend), fun() ->
            Untyped = compile_stream_for_backend(
                Backend, ?CODE_CHUNK_BIF_EQ, ?ATU8_CHUNK_BIF_EQ, <<>>, ImportResolver
            ),
            Typed = compile_stream_for_backend(
                Backend, ?CODE_CHUNK_BIF_EQ, ?ATU8_CHUNK_BIF_EQ, ?TYPE_CHUNK_BIF_EQ, ImportResolver
            ),
            ?assert(byte_size(Typed) < byte_size(Untyped))
        end}
     || Backend <- [jit_x86_64, jit_aarch64, jit_armv6m]
    ].

tail_call_cache_armv6m_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_armv6m, ?CODE_CHUNK_3, ?ATU8_CHUNK_3, ?TYPE_CHUNK_3
    ),

    % PRIM_RETURN (primitive index 1) tail call pattern on armv6m:
    %   ldr  r7, [r2, #4]   ; load PRIM_RETURN function pointer
    %   ldr  r6, [sp, #20]  ; load saved LR
    %   str  r7, [sp, #20]  ; store function ptr as return address
    %   mov  lr, r6         ; restore LR
    %   pop  {r1,r4,r5,r6,r7,pc}  ; tail call via popped PC
    ReturnPattern =
        <<16#6857:16/little, 16#9e05:16/little, 16#9705:16/little, 16#46b6:16/little,
            16#bdf2:16/little>>,

    % The PRIM_RETURN pattern must appear exactly once in the compiled code.
    % The module has multiple OP_RETURN opcodes, so if caching works,
    % subsequent returns jump back to the first instance instead of
    % duplicating the tail call sequence.
    {Pos, Len} = binary:match(CompiledCode, ReturnPattern),
    % Verify there is no second occurrence after the first one
    ?assertEqual(
        nomatch,
        binary:match(CompiledCode, ReturnPattern, [
            {scope, {Pos + Len, byte_size(CompiledCode) - Pos - Len}}
        ])
    ),
    ok.

small_integer_bounds_test_() ->
    [
        ?_assertEqual({-(1 bsl 59), (1 bsl 59) - 1}, jit:small_integer_bounds(jit_x86_64)),
        ?_assertEqual({-(1 bsl 27), (1 bsl 27) - 1}, jit:small_integer_bounds(jit_armv6m))
    ].

%% BEAM types format version 3 (OTP < 29). Bound/unit flags live at bits
%% 12/13/14; type bits are 0..11.
type_resolver_v3_test_() ->
    IntLowerBoundEntry = <<((1 bsl 5) bor (1 bsl 12)):16, 2:64/signed>>,
    Chunk = <<3:32, 1:32, IntLowerBoundEntry/binary>>,
    R = jit_precompile:type_resolver(Chunk),
    [?_assertEqual({t_integer, {2, '+inf'}}, R(0))].

%% BEAM types format version 4 (OTP >= 29). v4 inserts BEAM_TYPE_RECORD at bit
%% 12, pushing the bound/unit flags up to bits 13/14/15; type bits are 0..12.
%% Mirrors src/libAtomVM/module.c which already handles both versions.
type_resolver_v4_test_() ->
    %% t_integer with lower bound 2, no upper bound.
    IntLowerBound = <<((1 bsl 5) bor (1 bsl 13)):16, 2:64/signed>>,
    %% t_integer with both bounds {2, 100}.
    IntBothBounds = <<((1 bsl 5) bor (1 bsl 13) bor (1 bsl 14)):16, 2:64/signed, 100:64/signed>>,
    %% t_bitstring with unit (HAS_UNIT at bit 15), unit byte stores unit-1.
    BitstringUnit = <<((1 bsl 1) bor (1 bsl 15)):16, 0:8>>,
    %% Plain atom, no extra bytes.
    AtomEntry = <<(1 bsl 0):16>>,
    Chunk =
        <<4:32, 4:32, IntLowerBound/binary, IntBothBounds/binary, BitstringUnit/binary,
            AtomEntry/binary>>,
    R = jit_precompile:type_resolver(Chunk),
    [
        ?_assertEqual({t_integer, {2, '+inf'}}, R(0)),
        ?_assertEqual({t_integer, {2, 100}}, R(1)),
        ?_assertEqual({t_bs_matchable, 1}, R(2)),
        ?_assertEqual(t_atom, R(3))
    ].

is_small_integer_range_test_() ->
    [
        % Both ranges within 32-bit small integer bounds
        ?_assert(jit:is_small_integer_range({0, 100}, {-50, 50}, jit_armv6m)),
        % Both ranges within 64-bit small integer bounds
        ?_assert(jit:is_small_integer_range({0, 100}, {-50, 50}, jit_x86_64)),
        % At the exact boundary for 32-bit
        ?_assert(
            jit:is_small_integer_range(
                {-(1 bsl 27), (1 bsl 27) - 1}, {0, 0}, jit_armv6m
            )
        ),
        % Exceeding boundary for 32-bit
        ?_assertNot(
            jit:is_small_integer_range(
                {-(1 bsl 27) - 1, 0}, {0, 0}, jit_armv6m
            )
        ),
        ?_assertNot(
            jit:is_small_integer_range(
                {0, (1 bsl 27)}, {0, 0}, jit_armv6m
            )
        ),
        % Second range exceeding boundary
        ?_assertNot(
            jit:is_small_integer_range(
                {0, 0}, {0, (1 bsl 27)}, jit_armv6m
            )
        ),
        % Unbounded range (atom bounds like '-inf'/'+inf')
        ?_assertNot(jit:is_small_integer_range({'-inf', 100}, {0, 50}, jit_x86_64)),
        ?_assertNot(jit:is_small_integer_range({0, '+inf'}, {0, 50}, jit_x86_64)),
        % At the exact boundary for 64-bit
        ?_assert(
            jit:is_small_integer_range(
                {-(1 bsl 59), (1 bsl 59) - 1}, {0, 0}, jit_x86_64
            )
        ),
        ?_assertNot(
            jit:is_small_integer_range(
                {-(1 bsl 59) - 1, 0}, {0, 0}, jit_x86_64
            )
        )
    ].

% Code chunk for byte_size inline test.
% Equivalent to: f(X) when is_binary(X) -> byte_size(X).
%
% Bytecodes:
%   label 1
%   line 0
%   func_info atom_1, atom_2, 1
%   label 2
%   is_binary label_1, x[0]
%   gc_bif1 label_0, 1, 0, typed_x[0]:t_bitstring(8), x[0]
%   return
%   int_call_end
-define(CODE_CHUNK_4,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 182, 0, 0, 0, 2, 0, 0, 0, 1, 1, 16, 153, 0, 2, 18, 34, 16,
        1, 32, 53, 21, 3, 124, 5, 16, 0, 16#57, 3, 0, 3, 19, 3>>
).
-define(ATU8_CHUNK_4,
    <<255, 255, 255, 254, 224, 116, 101, 115, 116, 95, 98, 121, 116, 101, 95, 115, 105, 122, 101,
        16, 102>>
).
% Type chunk for byte_size inline test.
% Version 3, 1 entry: t_bitstring with unit=8
-define(TYPE_CHUNK_4,
    <<0, 0, 0, 3, 0, 0, 0, 1, 16#40, 16#02, 7>>
).

byte_size_inline_binary_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_CHUNK_4,
        ?ATU8_CHUNK_4,
        ?TYPE_CHUNK_4,
        fun(_) -> {erlang, byte_size, 1} end
    ),

    % When is_binary guard precedes gc_bif1 byte_size, the JIT inlines
    % the operation. The inline code:
    %   1. Strips the primary tag (and $0xfc)
    %   2. Reads boxed_value[1] (the byte size)
    %   3. Encodes as tagged integer (shl $4, or $0xf)
    %
    % The shl $4 + or $0xf pattern is distinctive to the inline path.
    % In the non-inlined path, a function pointer call would appear instead.
    %
    % Inline sequence on rax:
    %   48 83 e0 fc    and    $0xfffffffffffffffc,%rax
    %   48 8b 40 08    mov    0x8(%rax),%rax
    %   48 c1 e0 04    shl    $0x4,%rax
    %   48 83 c8 0f    or     $0xf,%rax
    ?assertMatch(
        {_, 16},
        binary:match(
            CompiledCode,
            <<16#48, 16#83, 16#e0, 16#fc, 16#48, 16#8b, 16#40, 16#08, 16#48, 16#c1, 16#e0, 16#04,
                16#48, 16#83, 16#c8, 16#0f>>
        )
    ),
    ok.

%% The runtime-guarded div/rem inline path fires when both operands are proven
%% integers and the divisor is proven strictly positive (Min >= 1), so it is
%% never 0 and never -1 (ruling out divide-by-zero and the MIN div -1 overflow).
%% A positive lower bound with an unbounded upper bound ({2, '+inf'}) is the
%% common loop case the static is_small_integer_range path rejects.
can_inline_div_guarded_test_() ->
    [
        % Divisor proven >= 1, dividend any range: guarded inline allowed.
        ?_assert(jit:can_inline_div_guarded({'-inf', '+inf'}, {2, '+inf'}, jit_x86_64, undefined)),
        ?_assert(jit:can_inline_div_guarded({0, 100}, {1, '+inf'}, jit_x86_64, undefined)),
        % Divisor lower bound 0 (could be zero): not allowed.
        ?_assertNot(jit:can_inline_div_guarded({2, '+inf'}, {0, '+inf'}, jit_x86_64, undefined)),
        % Divisor could be negative (could be -1: overflow risk): not allowed.
        ?_assertNot(jit:can_inline_div_guarded({2, '+inf'}, {-1, '+inf'}, jit_x86_64, undefined)),
        ?_assertNot(
            jit:can_inline_div_guarded({2, '+inf'}, {'-inf', '+inf'}, jit_x86_64, undefined)
        )
    ].

%%-----------------------------------------------------------------------------
%% Tuple fusion tests
%%
%% Test that is_tuple + test_arity + get_tuple_element sequences are fused
%% into a single operation that loads the register, strips the tag, and loads
%% the header only once.
%%
%% The distinctive fused pattern on x86_64 is:
%%   48 83 e0 fc    and  $-4,%rax           (strip tag)
%%   4c 8b 18       mov  (%rax),%r11        (header into separate register)
%%
%% In the unfused code, move_array_element loads the header into the SAME
%% register as the pointer:
%%   48 83 e0 fc    and  $-4,%rax
%%   48 8b 00       mov  (%rax),%rax        (header overwrites pointer)
%%-----------------------------------------------------------------------------

% is_tuple + test_arity + single get_tuple_element
% f({A, _B}) -> A.
-define(FUSE_CODE_1,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#15, 16#03, 16#20, 16#42, 16#03, 16#00, 16#03,
        16#13, 16#03>>
).
-define(FUSE_ATU8_1,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#31, 16#10, 16#66>>
).

% is_tuple + test_arity + multiple get_tuple_elements
% f({A, B, C}) -> {C, B, A}. (just the destructuring part)
-define(FUSE_CODE_2,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#15, 16#03, 16#30, 16#42, 16#03, 16#00, 16#13,
        16#42, 16#03, 16#10, 16#23, 16#42, 16#03, 16#20, 16#03, 16#13, 16#03>>
).
-define(FUSE_ATU8_2,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#32, 16#10, 16#66>>
).

% is_tuple + test_arity only (no get_tuple_element)
-define(FUSE_CODE_3,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#15, 16#03, 16#20, 16#13, 16#03>>
).
-define(FUSE_ATU8_3,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#33, 16#10, 16#66>>
).

% is_tuple + test_arity with different fail labels + get_tuple_element
-define(FUSE_CODE_4,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#04, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#3A, 16#35, 16#03, 16#20, 16#42, 16#03, 16#00, 16#03,
        16#13, 16#01, 16#30, 16#40, 16#32, 16#03, 16#13, 16#03>>
).
-define(FUSE_ATU8_4,
    <<16#FF, 16#FF, 16#FF, 16#FD, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#34, 16#10, 16#66, 16#50, 16#66, 16#61, 16#6C, 16#73, 16#65>>
).

% is_tuple alone (no test_arity follows) - should NOT fuse
-define(FUSE_CODE_5,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B1, 16#00,
        16#00, 16#00, 16#03, 16#00, 16#00, 16#00, 16#01, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#39, 16#15, 16#03, 16#13, 16#03>>
).
-define(FUSE_ATU8_5,
    <<16#FF, 16#FF, 16#FF, 16#FE, 16#A0, 16#74, 16#65, 16#73, 16#74, 16#5F, 16#66, 16#75, 16#73,
        16#65, 16#35, 16#10, 16#66>>
).

fuse_tuple_single_get_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_1, ?FUSE_ATU8_1, <<>>
    ),
    % Fused: strip tag + load header into separate register
    %   48 83 e0 fc    and  $-4,%rax
    %   4c 8b 18       mov  (%rax),%r11
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    % Fused: element 0 loaded using kept untagged pointer
    %   4c 8b 58 08    mov  0x8(%rax),%r11
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#4c, 16#8b, 16#58, 16#08>>)
    ),
    ok.

fuse_tuple_multi_get_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_2, ?FUSE_ATU8_2, <<>>
    ),
    % Fused: strip + header into separate register
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    % All three elements loaded from the same untagged pointer:
    %   4c 8b 58 08    mov  0x8(%rax),%r11     (element 0 -> x[1])
    %   4c 89 5f 60    mov  %r11,0x60(%rdi)
    %   4c 8b 58 10    mov  0x10(%rax),%r11    (element 1 -> x[2])
    %   4c 89 5f 68    mov  %r11,0x68(%rdi)
    %   4c 8b 58 18    mov  0x18(%rax),%r11    (element 2 -> x[0])
    ?assertMatch(
        {_, _},
        binary:match(
            CompiledCode,
            <<16#4c, 16#8b, 16#58, 16#08, 16#4d, 16#89, 16#5e, 16#60, 16#4c, 16#8b, 16#58, 16#10,
                16#4d, 16#89, 16#5e, 16#68, 16#4c, 16#8b, 16#58, 16#18>>
        )
    ),
    ok.

fuse_tuple_arity_only_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_3, ?FUSE_ATU8_3, <<>>
    ),
    % Fused: strip + header into separate register (even without get_tuple_element)
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    ok.

fuse_tuple_diff_labels_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_4, ?FUSE_ATU8_4, <<>>
    ),
    % Fused: strip + header into separate register
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    ok.

no_fuse_tuple_alone_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_CODE_5, ?FUSE_ATU8_5, <<>>
    ),
    % Unfused: header loaded into SAME register (rax) since no need to keep ptr
    %   48 83 e0 fc    and  $-4,%rax
    %   48 8b 00       mov  (%rax),%rax
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#48, 16#8b, 16#00>>)
    ),
    % The fused pattern should NOT appear
    ?assertEqual(
        nomatch,
        binary:match(CompiledCode, <<16#48, 16#83, 16#e0, 16#fc, 16#4c, 16#8b, 16#18>>)
    ),
    ok.

fuse_tuple_armv6m_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_armv6m, ?FUSE_CODE_1, ?FUSE_ATU8_1, <<>>
    ),
    % Fused: header loaded into r6 while r7 keeps the untagged pointer
    %   43b7    bics  r7, r6       (strip tag, r7 = untagged ptr)
    %   683e    ldr   r6, [r7, #0] (header into r6)
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#b7, 16#43, 16#3e, 16#68>>)
    ),
    % Element loaded using kept r7
    %   687e    ldr   r6, [r7, #4]
    ?assertMatch(
        {_, _},
        binary:match(CompiledCode, <<16#7e, 16#68>>)
    ),
    ok.

%%-----------------------------------------------------------------------------
%% Tagged-tuple (record) fusion tests
%%
%% OTP lowers record matches to a bare is_tagged_tuple followed by
%% get_tuple_element reads. is_tagged_tuple already loads and tag-strips the
%% boxed pointer to validate the record; the following get_tuple_element ops
%% otherwise reload the source and re-strip it. Fusing keeps the stripped
%% pointer and reads the fields directly.
%%
%% The expected record tag atom is resolved FIRST (before the tuple pointer is
%% loaded) so nothing needs to be preserved across the resolver call. On x86_64
%% the fused form then keeps the stripped pointer in %r11 and reads the fields
%% directly from it:
%%   48 8b 42 18    mov  0x18(%rdx),%rax   (resolver primitive, resolved first)
%%   ...            call, then load+check the pointer into %r11
%%   4d 8b 53 08    mov  0x8(%r11),%r10    (tag atom into scratch, %r11 kept)
%%   49 8b 43 10    mov  0x10(%r11),%rax   (field 1, no reload)
%%
%% Unfused, each get_tuple_element reloads and re-strips the source:
%%   48 8b 47 58    mov  <src>,%rax
%%   48 83 e0 fc    and  $-4,%rax
%%-----------------------------------------------------------------------------

% is_tagged_tuple + single get_tuple_element
% f({tag, X}) -> X.
-define(FUSE_TT_SINGLE_CODE,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B5, 16#00,
        16#00, 16#00, 16#07, 16#00, 16#00, 16#00, 16#03, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#9F, 16#15, 16#03, 16#20, 16#32, 16#42, 16#03, 16#10, 16#03, 16#13, 16#01,
        16#30, 16#02, 16#12, 16#42, 16#00, 16#01, 16#40, 16#40, 16#12, 16#03, 16#4E, 16#10, 16#00,
        16#01, 16#50, 16#02, 16#12, 16#42, 16#10, 16#01, 16#60, 16#40, 16#03, 16#13, 16#40, 16#12,
        16#03, 16#4E, 16#20, 16#10, 16#03>>
).
-define(FUSE_TT_SINGLE_ATU8,
    <<16#FF, 16#FF, 16#FF, 16#FA, 16#A0, 16#65, 16#74, 16#74, 16#5F, 16#73, 16#69, 16#6E, 16#67,
        16#6C, 16#65, 16#10, 16#66, 16#30, 16#74, 16#61, 16#67, 16#B0, 16#6D, 16#6F, 16#64, 16#75,
        16#6C, 16#65, 16#5F, 16#69, 16#6E, 16#66, 16#6F, 16#60, 16#65, 16#72, 16#6C, 16#61, 16#6E,
        16#67, 16#F0, 16#67, 16#65, 16#74, 16#5F, 16#6D, 16#6F, 16#64, 16#75, 16#6C, 16#65, 16#5F,
        16#69, 16#6E, 16#66, 16#6F>>
).

% is_tagged_tuple + two get_tuple_element
% f({point, X, Y}) -> g(X, Y).
-define(FUSE_TT_MULTI_CODE,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B5, 16#00,
        16#00, 16#00, 16#09, 16#00, 16#00, 16#00, 16#04, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#9F, 16#15, 16#03, 16#30, 16#32, 16#42, 16#03, 16#10, 16#13, 16#42, 16#03,
        16#20, 16#03, 16#A9, 16#03, 16#13, 16#06, 16#20, 16#45, 16#01, 16#30, 16#02, 16#12, 16#42,
        16#20, 16#01, 16#40, 16#40, 16#52, 16#03, 16#13, 16#01, 16#50, 16#02, 16#12, 16#62, 16#00,
        16#01, 16#60, 16#40, 16#12, 16#03, 16#4E, 16#10, 16#00, 16#01, 16#70, 16#02, 16#12, 16#62,
        16#10, 16#01, 16#80, 16#40, 16#03, 16#13, 16#40, 16#12, 16#03, 16#4E, 16#20, 16#10, 16#03>>
).
-define(FUSE_TT_MULTI_ATU8,
    <<16#FF, 16#FF, 16#FF, 16#F8, 16#90, 16#65, 16#74, 16#74, 16#5F, 16#6D, 16#75, 16#6C, 16#74,
        16#69, 16#10, 16#66, 16#50, 16#70, 16#6F, 16#69, 16#6E, 16#74, 16#10, 16#67, 16#20, 16#6F,
        16#6B, 16#B0, 16#6D, 16#6F, 16#64, 16#75, 16#6C, 16#65, 16#5F, 16#69, 16#6E, 16#66, 16#6F,
        16#60, 16#65, 16#72, 16#6C, 16#61, 16#6E, 16#67, 16#F0, 16#67, 16#65, 16#74, 16#5F, 16#6D,
        16#6F, 16#64, 16#75, 16#6C, 16#65, 16#5F, 16#69, 16#6E, 16#66, 16#6F>>
).

%% x86_64 unfused get_tuple_element reloads and re-strips the source (adjacent).
-define(FUSE_TT_X86_64_RELOAD_STRIP, <<16#48, 16#8b, 16#47, 16#58, 16#48, 16#83, 16#e0, 16#fc>>).

fuse_tagged_tuple_single_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_TT_SINGLE_CODE, ?FUSE_TT_SINGLE_ATU8, <<>>
    ),
    % No get_tuple_element reload+strip: the field reads from the kept pointer.
    ?assertEqual(nomatch, binary:match(CompiledCode, ?FUSE_TT_X86_64_RELOAD_STRIP)),
    % Tag atom read into scratch (%r11 kept), field 1 read from the kept pointer
    %   4d 8b 53 08    mov  0x8(%r11),%r10
    %   49 8b 43 10    mov  0x10(%r11),%rax
    ?assertMatch({_, _}, binary:match(CompiledCode, <<16#4d, 16#8b, 16#53, 16#08>>)),
    ?assertMatch({_, _}, binary:match(CompiledCode, <<16#49, 16#8b, 16#43, 16#10>>)),
    ok.

fuse_tagged_tuple_multi_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_TT_MULTI_CODE, ?FUSE_TT_MULTI_ATU8, <<>>
    ),
    ?assertEqual(nomatch, binary:match(CompiledCode, ?FUSE_TT_X86_64_RELOAD_STRIP)),
    % Both fields read from the same kept pointer (word 2 and word 3)
    %   49 8b 43 10    mov  0x10(%r11),%rax   (element 1)
    %   49 8b 43 18    mov  0x18(%r11),%rax   (element 2)
    ?assertMatch({_, _}, binary:match(CompiledCode, <<16#49, 16#8b, 16#43, 16#10>>)),
    ?assertMatch({_, _}, binary:match(CompiledCode, <<16#49, 16#8b, 16#43, 16#18>>)),
    ok.

fuse_tagged_tuple_single_aarch64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_aarch64, ?FUSE_TT_SINGLE_CODE, ?FUSE_TT_SINGLE_ATU8, <<>>
    ),
    % Fused: tag atom read into scratch x9 keeps x8 (the pointer) live, then the
    % field is read from x8 directly.
    %   09 05 40 f9    ldr  x9, [x8, #8]     (tag atom into scratch)
    %   07 09 40 f9    ldr  x7, [x8, #16]    (field read from kept pointer)
    ?assertMatch({_, _}, binary:match(CompiledCode, <<16#09, 16#05, 16#40, 16#f9>>)),
    ?assertMatch({_, _}, binary:match(CompiledCode, <<16#07, 16#09, 16#40, 16#f9>>)),
    ok.

%%-----------------------------------------------------------------------------
%% Bit-syntax fixed-size fusion test (bs_match offset writeback)
%%
%% A fixed-field binary decoder (`<<A:8, B:8, C:16, D:32>>`) lowers to one
%% ensure command followed by a run of integer reads. The bit offset stays in a
%% register across the whole command sequence and every sub-command reads it
%% from there, so writing it back to match_state[2] after each command is a dead
%% store. The offset is now committed once, after the command sequence, instead
%% of after every command.
%%
%% On x86_64 the offset writeback is `mov %r9,0x10(%r11)` (store the offset to
%% word 2 of the stripped match-state pointer). Without the fusion it appears
%% once per command (5x: one ensure + four integer reads); with it, once.
%%-----------------------------------------------------------------------------

% f(<<A:8, B:8, C:16, D:32>>) -> {A, B, C, D}; f(_) -> error.
-define(FUSE_BS_CODE,
    <<16#00, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#B6, 16#00,
        16#00, 16#00, 16#08, 16#00, 16#00, 16#00, 16#03, 16#01, 16#10, 16#02, 16#12, 16#22, 16#10,
        16#01, 16#20, 16#A6, 16#35, 16#03, 16#10, 16#03, 16#B6, 16#35, 16#03, 16#17, 16#08, 16#1A,
        16#32, 16#08, 16#40, 16#42, 16#10, 16#02, 16#80, 16#10, 16#13, 16#42, 16#20, 16#02, 16#80,
        16#10, 16#23, 16#42, 16#30, 16#02, 16#08, 16#10, 16#10, 16#33, 16#42, 16#40, 16#02, 16#08,
        16#20, 16#10, 16#43, 16#10, 16#50, 16#50, 16#A4, 16#03, 16#17, 16#40, 16#13, 16#23, 16#33,
        16#43, 16#13, 16#01, 16#30, 16#40, 16#52, 16#03, 16#13, 16#01, 16#40, 16#02, 16#12, 16#62,
        16#00, 16#01, 16#50, 16#40, 16#12, 16#03, 16#4E, 16#10, 16#00, 16#01, 16#60, 16#02, 16#12,
        16#62, 16#10, 16#01, 16#70, 16#40, 16#03, 16#13, 16#40, 16#12, 16#03, 16#4E, 16#20, 16#10,
        16#03>>
).
-define(FUSE_BS_ATU8,
    <<16#FF, 16#FF, 16#FF, 16#F8, 16#30, 16#62, 16#73, 16#64, 16#30, 16#68, 16#64, 16#72, 16#E0,
        16#65, 16#6E, 16#73, 16#75, 16#72, 16#65, 16#5F, 16#65, 16#78, 16#61, 16#63, 16#74, 16#6C,
        16#79, 16#70, 16#69, 16#6E, 16#74, 16#65, 16#67, 16#65, 16#72, 16#50, 16#65, 16#72, 16#72,
        16#6F, 16#72, 16#B0, 16#6D, 16#6F, 16#64, 16#75, 16#6C, 16#65, 16#5F, 16#69, 16#6E, 16#66,
        16#6F, 16#60, 16#65, 16#72, 16#6C, 16#61, 16#6E, 16#67, 16#F0, 16#67, 16#65, 16#74, 16#5F,
        16#6D, 16#6F, 16#64, 16#75, 16#6C, 16#65, 16#5F, 16#69, 16#6E, 16#66, 16#6F>>
).
-define(FUSE_BS_TYPE,
    <<16#00, 16#00, 16#00, 16#04, 16#00, 16#00, 16#00, 16#01, 16#1F, 16#FF>>
).

count_matches(Bin, Pattern) ->
    count_matches(Bin, Pattern, 0, 0).
count_matches(Bin, Pattern, Start, Acc) ->
    case binary:match(Bin, Pattern, [{scope, {Start, byte_size(Bin) - Start}}]) of
        {Pos, Len} -> count_matches(Bin, Pattern, Pos + Len, Acc + 1);
        nomatch -> Acc
    end.

fuse_bs_match_offset_writeback_x86_64_test() ->
    CompiledCode = compile_stream_for_backend(
        jit_x86_64, ?FUSE_BS_CODE, ?FUSE_BS_ATU8, ?FUSE_BS_TYPE
    ),
    % The offset writeback `mov %r9,0x10(%r11)` is committed once (after the
    % whole command run) instead of after each of the 5 commands.
    ?assertEqual(1, count_matches(CompiledCode, <<16#4d, 16#89, 16#4b, 16#10>>)),
    ok.

%%-----------------------------------------------------------------------------
%% Typed integer optimization tests
%%
%% These tests verify that comparison and arithmetic operations on registers
%% whose types are known (via the Type chunk) take the fast inline path instead
%% of falling back to term_compare / BIF calls.
%%
%% Source patterns and expected OTP bytecodes:
%%
%% typed_is_lt_both:
%%   f(A, B) when is_list(A), is_list(B) -> N = length(A), M = length(B),
%%     if N < M -> less; true -> not_less end.
%%   => is_lt {tr,x0,{t_integer,{0,MaxSmi}}}, {tr,x1,{t_integer,{0,MaxSmi}}}
%%
%% typed_is_ge_typed_lit:
%%   f(A, B) when is_list(A), is_list(B), length(A) >= length(B) -> ge; f(_,_) -> lt.
%%   => is_ge {tr,x0,{t_integer,{0,MaxSmi}}}, {tr,x1,{t_integer,{0,MaxSmi}}}
%%
%% typed_is_ge_lit_typed:
%%   f(List) when is_list(List) -> if length(List) >= 10 -> large; true -> small end.
%%   => is_ge {tr,x0,{t_integer,{0,MaxSmi}}}, {integer,10}  (literal second arg)
%%
%% typed_is_eq_exact_both:
%%   f(A, B) when is_list(A), is_list(B), length(A) =:= length(B) -> equal; f(_,_) -> ne.
%%   => is_eq_exact {tr,x0,...}, {tr,x1,...}
%%
%% typed_is_eq_exact_typed_lit:
%%   f(A) when is_list(A), length(A) =:= 5 -> five; f(_) -> other.
%%   => is_eq_exact {tr,x0,...}, {integer,5}
%%
%% typed_is_not_eq_exact_both:
%%   f(A, B) when is_list(A), is_list(B), length(A) =/= length(B) -> not_equal; ...
%%   => is_ne_exact {tr,x0,...}, {tr,x1,...}
%%
%% typed_is_not_eq_exact_typed_lit:
%%   f(A) when is_list(A), length(A) =/= 5 -> not_five; f(_) -> other.
%%   => is_ne_exact {tr,x0,...}, {integer,5}
%%
%% typed_tuple_size:
%%   f(T) when is_tuple(T) -> tuple_size(T).
%%   => gc_bif tuple_size {tr,x0,t_tuple}  -> inline (skip is_tuple primitive check)
%%
%% typed_select_val_int:
%%   f(List) when is_list(List) -> N = length(List), case N of 0->zero; 1->one; ... end.
%%   => select_val {tr,x0,{t_integer,{0,MaxSmi}}}, ...  (typed source)
%%-----------------------------------------------------------------------------

% typed_is_lt_both: is_lt with two bounded typed integers
% f(A, B) when is_list(A), is_list(B) ->
%     N = length(A), M = length(B),
%     if N < M -> less; true -> not_less end.
-define(CODE_TYPED_IS_LT_BOTH,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 55, 21, 3, 55, 21, 19, 153, 32, 124, 5, 32, 0, 87, 3, 16, 3, 124, 5, 32, 0, 87, 19,
        16, 19, 39, 53, 87, 3, 32, 87, 19, 32, 64, 82, 3, 19, 1, 48, 64, 98, 3, 19, 1, 64, 153, 0,
        2, 18, 114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153, 0, 2, 18, 114, 16, 1, 112, 64, 3,
        19, 64, 18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_LT_BOTH,
    <<255, 255, 255, 248, 8, 16, 116, 121, 112, 101, 100, 95, 105, 115, 95, 108, 116, 95, 98, 111,
        116, 104, 16, 102, 96, 101, 114, 108, 97, 110, 103, 96, 108, 101, 110, 103, 116, 104, 64,
        108, 101, 115, 115, 128, 110, 111, 116, 95, 108, 101, 115, 115, 176, 109, 111, 100, 117,
        108, 101, 95, 105, 110, 102, 111, 240, 103, 101, 116, 95, 109, 111, 100, 117, 108, 101, 95,
        105, 110, 102, 111>>
).
% Type chunk: version=3, 3 entries: any, {t_integer,{0,2^58-1}}, {t_integer,{0,2^58-1}}
-define(TYPE_TYPED_IS_LT_BOTH,
    <<0, 0, 0, 3, 0, 0, 0, 3, 15, 255, 0, 132, 48, 32, 0, 0, 0, 0, 0, 0, 0, 0, 3, 255, 255, 255,
        255, 255, 255, 255>>
).

% typed_is_ge_typed_lit: is_ge with two bounded typed integers
% f(A, B) when is_list(A), is_list(B), length(A) >= length(B) -> ge; f(_,_) -> lt.
-define(CODE_TYPED_IS_GE_TYPED_LIT,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 55, 53, 3, 55, 53, 19, 124, 53, 32, 0, 87, 3, 16, 3, 124, 53, 32, 0, 87, 19, 16, 19,
        40, 53, 87, 3, 32, 87, 19, 32, 64, 82, 3, 19, 1, 48, 64, 98, 3, 19, 1, 64, 153, 0, 2, 18,
        114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153, 0, 2, 18, 114, 16, 1, 112, 64, 3, 19, 64,
        18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_GE_TYPED_LIT,
    <<255, 255, 255, 248, 8, 21, 116, 121, 112, 101, 100, 95, 105, 115, 95, 103, 101, 95, 116, 121,
        112, 101, 100, 95, 108, 105, 116, 16, 102, 96, 101, 114, 108, 97, 110, 103, 96, 108, 101,
        110, 103, 116, 104, 32, 103, 101, 32, 108, 116, 176, 109, 111, 100, 117, 108, 101, 95, 105,
        110, 102, 111, 240, 103, 101, 116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102,
        111>>
).
-define(TYPE_TYPED_IS_GE_TYPED_LIT, ?TYPE_TYPED_IS_LT_BOTH).

% typed_is_ge_lit_typed: is_ge with literal first arg, typed second arg
% f(List) when is_list(List) -> if length(List) >= 10 -> large; true -> small end.
-define(CODE_TYPED_IS_GE_LIT_TYPED,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 16,
        1, 32, 55, 21, 3, 124, 53, 16, 0, 87, 3, 16, 3, 40, 53, 87, 3, 32, 161, 64, 82, 3, 19, 1,
        48, 64, 98, 3, 19, 1, 64, 153, 0, 2, 18, 114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153,
        0, 2, 18, 114, 16, 1, 112, 64, 3, 19, 64, 18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_GE_LIT_TYPED,
    <<255, 255, 255, 248, 8, 21, 116, 121, 112, 101, 100, 95, 105, 115, 95, 103, 101, 95, 108, 105,
        116, 95, 116, 121, 112, 101, 100, 16, 102, 96, 101, 114, 108, 97, 110, 103, 96, 108, 101,
        110, 103, 116, 104, 80, 108, 97, 114, 103, 101, 80, 115, 109, 97, 108, 108, 176, 109, 111,
        100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103, 101, 116, 95, 109, 111, 100, 117, 108,
        101, 95, 105, 110, 102, 111>>
).
-define(TYPE_TYPED_IS_GE_LIT_TYPED, ?TYPE_TYPED_IS_LT_BOTH).

% typed_is_eq_exact_both: is_eq_exact with two bounded typed integers
% f(A, B) when is_list(A), is_list(B), length(A) =:= length(B) -> equal; f(_,_) -> not_equal.
-define(CODE_TYPED_IS_EQ_EXACT_BOTH,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 55, 53, 3, 55, 53, 19, 124, 53, 32, 0, 87, 3, 16, 3, 124, 53, 32, 0, 87, 19, 16, 19,
        43, 53, 87, 3, 32, 87, 19, 32, 64, 82, 3, 19, 1, 48, 64, 98, 3, 19, 1, 64, 153, 0, 2, 18,
        114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153, 0, 2, 18, 114, 16, 1, 112, 64, 3, 19, 64,
        18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_EQ_EXACT_BOTH,
    <<255, 255, 255, 248, 8, 22, 116, 121, 112, 101, 100, 95, 105, 115, 95, 101, 113, 95, 101, 120,
        97, 99, 116, 95, 98, 111, 116, 104, 16, 102, 96, 101, 114, 108, 97, 110, 103, 96, 108, 101,
        110, 103, 116, 104, 80, 101, 113, 117, 97, 108, 144, 110, 111, 116, 95, 101, 113, 117, 97,
        108, 176, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103, 101, 116, 95, 109,
        111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
-define(TYPE_TYPED_IS_EQ_EXACT_BOTH, ?TYPE_TYPED_IS_LT_BOTH).

% typed_is_eq_exact_typed_lit: is_eq_exact with bounded typed first arg and literal second arg
% f(A) when is_list(A), length(A) =:= 5 -> five; f(_) -> other.
-define(CODE_TYPED_IS_EQ_EXACT_TYPED_LIT,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 16,
        1, 32, 55, 53, 3, 124, 53, 16, 0, 87, 3, 16, 3, 43, 53, 87, 3, 32, 81, 64, 82, 3, 19, 1, 48,
        64, 98, 3, 19, 1, 64, 153, 0, 2, 18, 114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153, 0, 2,
        18, 114, 16, 1, 112, 64, 3, 19, 64, 18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_EQ_EXACT_TYPED_LIT,
    <<255, 255, 255, 248, 8, 27, 116, 121, 112, 101, 100, 95, 105, 115, 95, 101, 113, 95, 101, 120,
        97, 99, 116, 95, 116, 121, 112, 101, 100, 95, 108, 105, 116, 16, 102, 96, 101, 114, 108, 97,
        110, 103, 96, 108, 101, 110, 103, 116, 104, 64, 102, 105, 118, 101, 80, 111, 116, 104, 101,
        114, 176, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103, 101, 116, 95, 109,
        111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
-define(TYPE_TYPED_IS_EQ_EXACT_TYPED_LIT, ?TYPE_TYPED_IS_LT_BOTH).

% typed_is_not_eq_exact_both: is_ne_exact (OP 44) with two bounded typed integers
% f(A, B) when is_list(A), is_list(B), length(A) =/= length(B) -> not_equal; f(_,_) -> equal.
-define(CODE_TYPED_IS_NOT_EQ_EXACT_BOTH,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 32,
        1, 32, 55, 53, 3, 55, 53, 19, 124, 53, 32, 0, 87, 3, 16, 3, 124, 53, 32, 0, 87, 19, 16, 19,
        44, 53, 87, 3, 32, 87, 19, 32, 64, 82, 3, 19, 1, 48, 64, 98, 3, 19, 1, 64, 153, 0, 2, 18,
        114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153, 0, 2, 18, 114, 16, 1, 112, 64, 3, 19, 64,
        18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_NOT_EQ_EXACT_BOTH,
    <<255, 255, 255, 248, 8, 26, 116, 121, 112, 101, 100, 95, 105, 115, 95, 110, 111, 116, 95, 101,
        113, 95, 101, 120, 97, 99, 116, 95, 98, 111, 116, 104, 16, 102, 96, 101, 114, 108, 97, 110,
        103, 96, 108, 101, 110, 103, 116, 104, 144, 110, 111, 116, 95, 101, 113, 117, 97, 108, 80,
        101, 113, 117, 97, 108, 176, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103,
        101, 116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
-define(TYPE_TYPED_IS_NOT_EQ_EXACT_BOTH, ?TYPE_TYPED_IS_LT_BOTH).

% typed_is_not_eq_exact_typed_lit: is_ne_exact with bounded typed first arg and literal second arg
% f(A) when is_list(A), length(A) =/= 5 -> not_five; f(_) -> other.
-define(CODE_TYPED_IS_NOT_EQ_EXACT_TYPED_LIT,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 8, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 16,
        1, 32, 55, 53, 3, 124, 53, 16, 0, 87, 3, 16, 3, 44, 53, 87, 3, 32, 81, 64, 82, 3, 19, 1, 48,
        64, 98, 3, 19, 1, 64, 153, 0, 2, 18, 114, 0, 1, 80, 64, 18, 3, 78, 16, 16, 1, 96, 153, 0, 2,
        18, 114, 16, 1, 112, 64, 3, 19, 64, 18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_IS_NOT_EQ_EXACT_TYPED_LIT,
    <<255, 255, 255, 248, 8, 31, 116, 121, 112, 101, 100, 95, 105, 115, 95, 110, 111, 116, 95, 101,
        113, 95, 101, 120, 97, 99, 116, 95, 116, 121, 112, 101, 100, 95, 108, 105, 116, 16, 102, 96,
        101, 114, 108, 97, 110, 103, 96, 108, 101, 110, 103, 116, 104, 128, 110, 111, 116, 95, 102,
        105, 118, 101, 80, 111, 116, 104, 101, 114, 176, 109, 111, 100, 117, 108, 101, 95, 105, 110,
        102, 111, 240, 103, 101, 116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
-define(TYPE_TYPED_IS_NOT_EQ_EXACT_TYPED_LIT, ?TYPE_TYPED_IS_LT_BOTH).

% typed_tuple_size: gc_bif tuple_size on typed t_tuple arg (skips is_tuple primitive check)
% f(T) when is_tuple(T) -> tuple_size(T).
-define(CODE_TYPED_TUPLE_SIZE,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 7, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 16,
        1, 32, 57, 21, 3, 10, 5, 0, 87, 3, 16, 3, 19, 1, 48, 153, 0, 2, 18, 82, 0, 1, 64, 64, 18, 3,
        78, 16, 16, 1, 80, 153, 0, 2, 18, 82, 16, 1, 96, 64, 3, 19, 64, 18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_TUPLE_SIZE,
    <<255, 255, 255, 250, 8, 16, 116, 121, 112, 101, 100, 95, 116, 117, 112, 108, 101, 95, 115, 105,
        122, 101, 16, 102, 96, 101, 114, 108, 97, 110, 103, 160, 116, 117, 112, 108, 101, 95, 115,
        105, 122, 101, 176, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103, 101,
        116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
% Type chunk: version=3, 2 entries: any, t_tuple
-define(TYPE_TYPED_TUPLE_SIZE, <<0, 0, 0, 3, 0, 0, 0, 2, 15, 255, 8, 0>>).

% typed_select_val_int: select_val on typed integer (from length/1 result)
% f(List) when is_list(List) -> N = length(List),
%     case N of 0 -> zero; 1 -> one; 2 -> two; _ -> other end.
-define(CODE_TYPED_SELECT_VAL_INT,
    <<0, 0, 0, 16, 0, 0, 0, 0, 0, 0, 0, 178, 0, 0, 0, 11, 0, 0, 0, 3, 1, 16, 153, 16, 2, 18, 34, 16,
        1, 32, 55, 21, 3, 153, 32, 124, 5, 16, 0, 87, 3, 16, 3, 59, 87, 3, 32, 101, 23, 96, 1, 85,
        17, 69, 33, 53, 1, 48, 64, 82, 3, 19, 1, 64, 64, 98, 3, 19, 1, 80, 64, 114, 3, 19, 1, 96,
        64, 130, 3, 19, 1, 112, 153, 0, 2, 18, 146, 0, 1, 128, 64, 18, 3, 78, 16, 16, 1, 144, 153,
        0, 2, 18, 146, 16, 1, 160, 64, 3, 19, 64, 18, 3, 78, 32, 32, 3>>
).
-define(ATU8_TYPED_SELECT_VAL_INT,
    <<255, 255, 255, 246, 8, 20, 116, 121, 112, 101, 100, 95, 115, 101, 108, 101, 99, 116, 95, 118,
        97, 108, 95, 105, 110, 116, 16, 102, 96, 101, 114, 108, 97, 110, 103, 96, 108, 101, 110,
        103, 116, 104, 48, 116, 119, 111, 48, 111, 110, 101, 64, 122, 101, 114, 111, 80, 111, 116,
        104, 101, 114, 176, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111, 240, 103, 101,
        116, 95, 109, 111, 100, 117, 108, 101, 95, 105, 110, 102, 111>>
).
-define(TYPE_TYPED_SELECT_VAL_INT, ?TYPE_TYPED_IS_LT_BOTH).

% Import resolver for modules using erlang:length/1 as import index 0.
length_import_resolver(0) -> {erlang, length, 1};
length_import_resolver(1) -> {erlang, get_module_info, 1};
length_import_resolver(2) -> {erlang, get_module_info, 2}.

% Import resolver for modules using erlang:tuple_size/1 as import index 0.
tuple_size_import_resolver(0) -> {erlang, tuple_size, 1};
tuple_size_import_resolver(1) -> {erlang, get_module_info, 1};
tuple_size_import_resolver(2) -> {erlang, get_module_info, 2}.

typed_is_lt_both_x86_64_test() ->
    % is_lt with both-typed bounded integers should compile without error.
    % Fast path: direct register compare, no term_compare call.
    % Compared with empty type chunk, typed code is shorter (no PRIM_TERM_COMPARE call).
    TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_LT_BOTH,
        ?ATU8_TYPED_IS_LT_BOTH,
        ?TYPE_TYPED_IS_LT_BOTH,
        fun length_import_resolver/1
    ),
    UntypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_LT_BOTH,
        ?ATU8_TYPED_IS_LT_BOTH,
        <<>>,
        fun length_import_resolver/1
    ),
    ?assert(byte_size(TypedCode) < byte_size(UntypedCode)),
    ok.

typed_is_ge_both_x86_64_test() ->
    % is_ge with both typed bounded integers: fast inline path.
    TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_GE_TYPED_LIT,
        ?ATU8_TYPED_IS_GE_TYPED_LIT,
        ?TYPE_TYPED_IS_GE_TYPED_LIT,
        fun length_import_resolver/1
    ),
    UntypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_GE_TYPED_LIT,
        ?ATU8_TYPED_IS_GE_TYPED_LIT,
        <<>>,
        fun length_import_resolver/1
    ),
    ?assert(byte_size(TypedCode) < byte_size(UntypedCode)),
    ok.

typed_is_ge_lit_typed_x86_64_test() ->
    % is_ge with typed first arg and literal second arg: bignum-aware inline path.
    % Emits more code than term_compare call but avoids dynamic dispatch.
    % Just verify compilation succeeds without error.
    _TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_GE_LIT_TYPED,
        ?ATU8_TYPED_IS_GE_LIT_TYPED,
        ?TYPE_TYPED_IS_GE_LIT_TYPED,
        fun length_import_resolver/1
    ),
    ok.

typed_is_eq_exact_both_x86_64_test() ->
    % is_eq_exact with both bounded typed integers: fast inline path.
    TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_EQ_EXACT_BOTH,
        ?ATU8_TYPED_IS_EQ_EXACT_BOTH,
        ?TYPE_TYPED_IS_EQ_EXACT_BOTH,
        fun length_import_resolver/1
    ),
    UntypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_EQ_EXACT_BOTH,
        ?ATU8_TYPED_IS_EQ_EXACT_BOTH,
        <<>>,
        fun length_import_resolver/1
    ),
    ?assert(byte_size(TypedCode) < byte_size(UntypedCode)),
    ok.

typed_is_eq_exact_typed_lit_x86_64_test() ->
    % is_eq_exact with typed first arg and integer literal second: bignum-aware inline path.
    % Emits more code than term_compare call but avoids dynamic dispatch.
    % Just verify compilation succeeds without error.
    _TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_EQ_EXACT_TYPED_LIT,
        ?ATU8_TYPED_IS_EQ_EXACT_TYPED_LIT,
        ?TYPE_TYPED_IS_EQ_EXACT_TYPED_LIT,
        fun length_import_resolver/1
    ),
    ok.

%% Untyped (no type chunk) comparison default path: the operands' types are
%% unknown at compile time, so the JIT cannot take the typed bare-cmp path.
%% Instead it emits a *runtime* small-integer fast path (tag-check both
%% operands, native tagged compare) wrapping the term_compare fallback. This
%% guards that the default path is more than a bare term_compare call: it must
%% be larger than the typed inline path (which is a single cmp) yet still
%% compile correctly on every backend. Verified end-to-end correct by
%% test-erlang and the comparison edge-case checks; here we lock in that the
%% fast path is emitted for the untyped default path on each backend.
untyped_is_eq_exact_fastpath_test_() ->
    [
        {atom_to_list(Backend), fun() ->
            Typed = compile_stream_for_backend(
                Backend,
                ?CODE_TYPED_IS_EQ_EXACT_BOTH,
                ?ATU8_TYPED_IS_EQ_EXACT_BOTH,
                ?TYPE_TYPED_IS_EQ_EXACT_BOTH,
                fun length_import_resolver/1
            ),
            Untyped = compile_stream_for_backend(
                Backend,
                ?CODE_TYPED_IS_EQ_EXACT_BOTH,
                ?ATU8_TYPED_IS_EQ_EXACT_BOTH,
                <<>>,
                fun length_import_resolver/1
            ),
            % The runtime fast path (tag tests + native cmp + term_compare
            % fallback) is strictly larger than the typed bare-cmp path.
            ?assert(byte_size(Untyped) > byte_size(Typed))
        end}
     || Backend <- [jit_x86_64, jit_aarch64]
    ].

untyped_is_lt_fastpath_test_() ->
    [
        {atom_to_list(Backend), fun() ->
            Typed = compile_stream_for_backend(
                Backend,
                ?CODE_TYPED_IS_LT_BOTH,
                ?ATU8_TYPED_IS_LT_BOTH,
                ?TYPE_TYPED_IS_LT_BOTH,
                fun length_import_resolver/1
            ),
            Untyped = compile_stream_for_backend(
                Backend,
                ?CODE_TYPED_IS_LT_BOTH,
                ?ATU8_TYPED_IS_LT_BOTH,
                <<>>,
                fun length_import_resolver/1
            ),
            ?assert(byte_size(Untyped) > byte_size(Typed))
        end}
     || Backend <- [jit_x86_64, jit_aarch64]
    ].

typed_is_not_eq_exact_both_x86_64_test() ->
    % is_ne_exact (OP_IS_NOT_EQ_EXACT) with both bounded typed integers: fast inline path.
    TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_NOT_EQ_EXACT_BOTH,
        ?ATU8_TYPED_IS_NOT_EQ_EXACT_BOTH,
        ?TYPE_TYPED_IS_NOT_EQ_EXACT_BOTH,
        fun length_import_resolver/1
    ),
    UntypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_NOT_EQ_EXACT_BOTH,
        ?ATU8_TYPED_IS_NOT_EQ_EXACT_BOTH,
        <<>>,
        fun length_import_resolver/1
    ),
    ?assert(byte_size(TypedCode) < byte_size(UntypedCode)),
    ok.

typed_is_not_eq_exact_typed_lit_x86_64_test() ->
    % is_ne_exact with typed first arg and integer literal second: bignum-aware inline path.
    % Emits more code than term_compare call but avoids dynamic dispatch.
    % Just verify compilation succeeds without error.
    _TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_IS_NOT_EQ_EXACT_TYPED_LIT,
        ?ATU8_TYPED_IS_NOT_EQ_EXACT_TYPED_LIT,
        ?TYPE_TYPED_IS_NOT_EQ_EXACT_TYPED_LIT,
        fun length_import_resolver/1
    ),
    ok.

typed_tuple_size_x86_64_test() ->
    % OTP always emits bif tuple_size (OP_BIF1), not gc_bif1 (OP_GC_BIF1),
    % because tuple_size always returns a small integer. The is_known_tuple
    % optimization in op_gc_bif1 is therefore not reachable from OTP-compiled
    % code. This test verifies BIF1 tuple_size compiles correctly with a
    % typed t_tuple argument.
    _CompiledCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_TUPLE_SIZE,
        ?ATU8_TYPED_TUPLE_SIZE,
        ?TYPE_TYPED_TUPLE_SIZE,
        fun tuple_size_import_resolver/1
    ),
    ok.

typed_select_val_int_x86_64_test() ->
    % select_val on a typed integer source uses the inline chain path
    % (can_inline_select_val_src returns true for typed integer).
    TypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_SELECT_VAL_INT,
        ?ATU8_TYPED_SELECT_VAL_INT,
        ?TYPE_TYPED_SELECT_VAL_INT,
        fun length_import_resolver/1
    ),
    UntypedCode = compile_stream_for_backend(
        jit_x86_64,
        ?CODE_TYPED_SELECT_VAL_INT,
        ?ATU8_TYPED_SELECT_VAL_INT,
        <<>>,
        fun length_import_resolver/1
    ),
    ?assert(byte_size(TypedCode) =< byte_size(UntypedCode)),
    ok.

%%-----------------------------------------------------------------------------
%% Runtime small-integer fast path on unprovable ranges
%%
%% A gc_bif + whose typed argument has an UNBOUNDED t_integer range cannot be
%% inlined by the compile-time range check, but must still use the runtime
%% overflow-checked small-integer fast path — i.e. compile to exactly the same
%% code as the untyped case — rather than falling back to the BIF call.
%%
%% Chunk: label 1 ; gc_bif2 fail=0 live=2 bif=0 <arg1> <int 2> x[1] ;
%% int_call_end. <arg1> is x[0]: typed (16#57 16#03 16#10, type index 1) or
%% untyped (16#03).
%%-----------------------------------------------------------------------------
gc_bif_add_unbounded_range_runtime_fastpath_test_() ->
    [
        ?_test(gc_bif_add_unbounded_range_runtime_fastpath(Backend))
     || Backend <- [jit_x86_64, jit_aarch64]
    ].

%% Two-pass flash compile: the sizing pass (counting stream) converges the
%% fused-branch size hints, then a single emission pass with those hints runs
%% on a flash-like stream (held window, flush horizon, bit-clear-only below
%% it -- all enforced strictly by jit_stream_flash_mock). The result must be
%% byte-identical to the plain buffered-stream compile.
sizing_emit_flash_identity_test() ->
    Backend = jit_riscv32,
    Chunk =
        <<16:32, 0:32, 125:32, 1:32, 1:32, 1, 16#10, 125, 16#05, 16#20, 16#00, 16#03, 16#21, 16#13,
            3>>,
    Nil = fun(_) -> undefined end,
    AnyType = fun(_) -> any end,
    NoDebug = fun(_) -> false end,
    ImportResolver = fun(0) -> {erlang, '+', 2} end,
    Reference = jit_tests_common:compile_chunk(
        Backend, Chunk, Nil, Nil, AnyType, ImportResolver, NoDebug
    ),
    <<16:32, 0:32, _:32, LabelsCount:32, _:32, _/binary>> = Chunk,
    Header = jit:beam_chunk_header(
        LabelsCount, jit_tests_common:backend_to_arch(Backend), ?JIT_VARIANT_PIC
    ),
    %% Pass 1: sizing on the counting stream. It must start at the same base
    %% offset as the emission stream (label offsets are absolute).
    SizeStream0 = jit_stream_size:append(jit_stream_size:new(0), Header),
    SizeState = Backend:new(?JIT_VARIANT_PIC, jit_stream_size, SizeStream0),
    {LabelsCount, Plan} = jit:compile_sizing(
        Chunk, Nil, Nil, AnyType, ImportResolver, NoDebug, Nil, Backend, SizeState
    ),
    ?assertMatch(#{hints := _, labels := Labels} when is_map(Labels), Plan),
    %% Pass 2: single emission with the plan on the flash-like stream. The
    %% preset labels let it emit final jump-table entries (write-once) and
    %% flush eagerly, so the mock's bit-clear discipline below the advancing
    %% horizon is actually exercised.
    MockStream0 = jit_stream_flash_mock:append(jit_stream_flash_mock:new(0), Header),
    EmitState0 = Backend:new(?JIT_VARIANT_PIC, jit_stream_flash_mock, MockStream0),
    {LabelsCount, EmitState1} = jit:compile_emit(
        Chunk, Nil, Nil, AnyType, ImportResolver, NoDebug, Nil, Backend, EmitState0, Plan
    ),
    %% Eager flushing must have advanced the horizon during emission.
    ?assert(jit_stream_flash_mock:committed_offset(Backend:stream(EmitState1)) > 0),
    {Bytes, _Horizon} = jit_stream_flash_mock:flush(Backend:stream(EmitState1)),
    ?assertEqual(Reference, Bytes).

gc_bif_add_unbounded_range_runtime_fastpath(Backend) ->
    TypedChunk =
        <<16:32, 0:32, 125:32, 1:32, 1:32,
            %% label 1
            1, 16#10,
            %% gc_bif2 fail=0 live=2 bif=0, typed x[0] (type 1), int 2, x[1]
            125, 16#05, 16#20, 16#00, 16#57, 16#03, 16#10, 16#21, 16#13,
            %% int_call_end
            3>>,
    UntypedChunk =
        <<16:32, 0:32, 125:32, 1:32, 1:32, 1, 16#10, 125, 16#05, 16#20, 16#00, 16#03, 16#21, 16#13,
            3>>,
    ImportResolver = fun(0) -> {erlang, '+', 2} end,
    TypedCode = jit_tests_common:compile_chunk(
        Backend,
        TypedChunk,
        fun(_) -> undefined end,
        fun(_) -> undefined end,
        fun(1) -> {t_integer, {0, '+inf'}} end,
        ImportResolver,
        fun(_) -> false end
    ),
    UntypedCode = jit_tests_common:compile_chunk(
        Backend,
        UntypedChunk,
        fun(_) -> undefined end,
        fun(_) -> undefined end,
        fun(_) -> any end,
        ImportResolver,
        fun(_) -> false end
    ),
    ?assertEqual(UntypedCode, TypedCode).

is_function2_typed_register_arity_test_() ->
    %% OP_IS_FUNCTION2 with the arity in a typed integer register (e.g.
    %% `f(F, N) when is_integer(N), N >= 0, is_function(F, N)`) makes jit.erl
    %% emit an '(int)' '!=' condition with a *register* right-hand side
    %% (the fun arity loaded from the boxed fun vs the arity register).
    %% Every backend must accept that condition form; this is a regression
    %% test for jit_aarch64 crashing with function_clause on it (hit by
    %% beam_ssa_type.beam from the OTP compiler application).
    case erlang:system_info(machine) of
        "ATOM" ->
            %% Compiling the reproducer needs the host toolchain
            %% (compile:file, os:cmd); covered on BEAM only.
            [];
        "BEAM" ->
            is_function2_typed_register_arity_tests()
    end.

is_function2_typed_register_arity_tests() ->
    {setup,
        fun() ->
            Dir = string:trim(os:cmd("mktemp -d")),
            Source =
                "-module(isfun2_typed).\n"
                "-export([check/2]).\n"
                "check(F, N) when is_integer(N), N >= 0, is_function(F, N) -> yes;\n"
                "check(_, _) -> no.\n",
            SrcPath = filename:join(Dir, "isfun2_typed.erl"),
            ok = file:write_file(SrcPath, Source),
            {ok, isfun2_typed, BeamBin} = compile:file(SrcPath, [binary, return_errors]),
            BeamPath = filename:join(Dir, "isfun2_typed.beam"),
            ok = file:write_file(BeamPath, BeamBin),
            {Dir, BeamPath}
        end,
        fun({Dir, _}) ->
            os:cmd("rm -rf " ++ Dir)
        end,
        fun({Dir, BeamPath}) ->
            [
                {Target,
                    ?_test(begin
                        OutDir = filename:join(Dir, Target) ++ "/",
                        ok = filelib:ensure_path(OutDir),
                        ok = jit_precompile:compile(Target, OutDir, false, BeamPath)
                    end)}
             || Target <- [
                    "x86_64", "aarch64", "armv6m", "arm32", "riscv32", "riscv64", "xtensa"
                ]
            ]
        end}.

%% A v4 record type is encoded as BEAM_TYPE_TUPLE bor BEAM_TYPE_RECORD (bits
%% 11 and 12). Record matches -- the common is_tagged_tuple case -- must decode
%% to t_tuple so the boxed tag checks are elided; regression for treating the
%% record bit as `any'.
type_resolver_v4_record_is_tuple_test() ->
    %% v4 header: <<HasUnit:1, HasUpper:1, HasLower:1, TypeBits:13>>.
    Tuple = (1 bsl 11),
    Record = (1 bsl 11) bor (1 bsl 12),
    Chunk = <<4:32, 2:32, (0 bor Tuple):16, (0 bor Record):16>>,
    Resolver = jit_precompile:type_resolver(Chunk),
    ?assertEqual(t_tuple, Resolver(0)),
    ?assertEqual(t_tuple, Resolver(1)).
