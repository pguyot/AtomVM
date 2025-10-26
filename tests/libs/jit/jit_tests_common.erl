%% This file is part of AtomVM.
%%
%% Copyright 2025 Paul Guyot <pguyot@kallisys.net>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later

%% Shared helper functions for JIT assembler tests

-module(jit_tests_common).

-export([asm/3, dump_to_bin/1]).

-include_lib("eunit/include/eunit.hrl").

%% Architecture-specific assembler validation
-spec asm(atom(), binary(), string()) -> binary().
asm(Arch, Bin, Str) ->
    case erlang:system_info(machine) of
        "ATOM" ->
            Bin;
        "BEAM" ->
            case find_binutils(Arch) of
                false ->
                    Bin;
                {ok, AsCmd, ObjdumpCmd} ->
                    % Use unique temporary files to avoid conflicts
                    TempBase = "jit_test_" ++ integer_to_list(erlang:unique_integer([positive])),
                    AsmFile = TempBase ++ ".S",
                    ObjFile = TempBase ++ ".o",
                    try
                        ok = file:write_file(AsmFile, get_asm_header(Arch) ++ Str ++ "\n"),
                        Cmd = lists:flatten(
                            io_lib:format(
                                "~s ~s -c ~s -o ~s && ~s -j .text -D ~s",
                                [AsCmd, get_as_flags(Arch), AsmFile, ObjFile, ObjdumpCmd, ObjFile]
                            )
                        ),
                        Dump = os:cmd(Cmd),
                        DumpBin = list_to_binary(Dump),
                        DumpLines = binary:split(DumpBin, <<"\n">>, [global]),
                        AsmBin = asm_lines(DumpLines, <<>>, Arch),
                        if
                            AsmBin =:= Bin ->
                                ok;
                            true ->
                                io:format(
                                    "-------------------------------------------\n" ++
                                        "~s\n" ++
                                        "-------------------------------------------\n",
                                    [Dump]
                                )
                        end,
                        ?assertEqual(AsmBin, Bin),
                        Bin
                    after
                        % Clean up temporary files
                        file:delete(AsmFile),
                        file:delete(ObjFile)
                    end
            end
    end.

%% Helper function to find available binutils for a given architecture
-spec find_binutils(atom()) -> {ok, string(), string()} | false.
find_binutils(Arch) ->
    ArchStr = atom_to_list(Arch),
    BinutilsList = [
        {ArchStr ++ "-esp-elf-as", ArchStr ++ "-esp-elf-objdump"},
        {ArchStr ++ "-unknown-elf-as", ArchStr ++ "-unknown-elf-objdump"},
        {ArchStr ++ "-elf-as", ArchStr ++ "-elf-objdump"},
        {ArchStr ++ "-none-eabi-as", ArchStr ++ "-none-eabi-objdump"},
        {ArchStr ++ "-linux-gnu-as", ArchStr ++ "-linux-gnu-objdump"}
    ],
    find_binutils_from_list(BinutilsList).

%% Private functions

%% Generic helper function to find binutils from a list
-spec find_binutils_from_list([{string(), string()}]) -> {ok, string(), string()} | false.
find_binutils_from_list([]) ->
    false;
find_binutils_from_list([{AsCmd, ObjdumpCmd} | Rest]) ->
    case os:cmd("which " ++ AsCmd) of
        [] ->
            find_binutils_from_list(Rest);
        _ ->
            {ok, AsCmd, ObjdumpCmd}
    end.

%% Get architecture-specific assembly file header
-spec get_asm_header(atom()) -> string().
get_asm_header(arm) ->
    ".arch armv6-m\n.thumb\n.syntax unified\n";
get_asm_header(aarch64) ->
    ".text\n";
get_asm_header(x86_64) ->
    ".text\n";
get_asm_header(riscv32) ->
    ".text\n".

%% Get architecture-specific assembler flags
-spec get_as_flags(atom()) -> string().
get_as_flags(arm) ->
    "";
get_as_flags(aarch64) ->
    "";
get_as_flags(x86_64) ->
    "--64";
get_as_flags(riscv32) ->
    "-march=rv32imac".

%% Parse objdump output lines and extract binary data
-spec asm_lines([binary()], binary(), atom()) -> binary().
asm_lines([<<" ", Tail/binary>> | T], Acc, Arch) ->
    case binary:split(Tail, <<":\t">>) of
        [_Offset, HexAndRest] ->
            case binary:split(HexAndRest, <<"\t">>) of
                [HexStr, _] ->
                    AssembledBin = hex_to_bin(HexStr, <<>>, Arch),
                    asm_lines(T, <<Acc/binary, AssembledBin/binary>>, Arch);
                [HexStr] ->
                    % Handle continuation lines with no tab (just hex bytes)
                    AssembledBin = hex_to_bin(HexStr, <<>>, Arch),
                    asm_lines(T, <<Acc/binary, AssembledBin/binary>>, Arch)
            end;
        _ ->
            asm_lines(T, Acc, Arch)
    end;
asm_lines([_OtherLine | T], Acc, Arch) ->
    asm_lines(T, Acc, Arch);
asm_lines([], Acc, _Arch) ->
    Acc.

%% Convert hex string to binary with robust parsing (handles edge cases)
-spec hex_to_bin(binary(), binary(), atom()) -> binary().
hex_to_bin(<<>>, Acc, _Arch) ->
    Acc;
hex_to_bin(<<" ", Tail/binary>>, Acc, Arch) ->
    hex_to_bin(Tail, Acc, Arch);
hex_to_bin(HexStr, Acc, Arch) ->
    case binary:split(HexStr, <<" ">>) of
        [HexChunk, Rest] ->
            NumBits = byte_size(HexChunk) * 4,
            HexVal = binary_to_integer(HexChunk, 16),
            % All architectures use little-endian encoding
            hex_to_bin(Rest, <<Acc/binary, HexVal:NumBits/little>>, Arch);
        [HexChunk] when byte_size(HexChunk) > 0 ->
            NumBits = byte_size(HexChunk) * 4,
            HexVal = binary_to_integer(HexChunk, 16),
            % All architectures use little-endian encoding
            <<Acc/binary, HexVal:NumBits/little>>
    end.

%% Parse objdump output and extract the binary bytes
%% This function parses assembly dump output (from objdump) and extracts
%% just the hex bytes, ignoring addresses and instruction mnemonics.
-spec dump_to_bin(binary()) -> binary().
dump_to_bin(Dump) ->
    dump_to_bin0(Dump, addr, []).

-spec dump_to_bin0(binary(), atom(), [byte()]) -> binary().
% Skip whitespace at start or between lines
dump_to_bin0(<<N, Tail/binary>>, addr, Acc) when N == $\  orelse N == $\n orelse N == $\t ->
    dump_to_bin0(Tail, addr, Acc);
% Parse hex digits in address (offset)
dump_to_bin0(<<N, Tail/binary>>, addr, Acc) when
    (N >= $0 andalso N =< $9) orelse (N >= $a andalso N =< $f)
->
    dump_to_bin0(Tail, addr, Acc);
% Found colon after address, move to pre_hex state
dump_to_bin0(<<$:, Tail/binary>>, addr, Acc) ->
    dump_to_bin0(Tail, pre_hex, Acc);
% Skip whitespace between colon and hex bytes
dump_to_bin0(<<N, Tail/binary>>, pre_hex, Acc) when N == $\  orelse N == $\t ->
    dump_to_bin0(Tail, pre_hex, Acc);
% Start of hex bytes
dump_to_bin0(<<_Other, _Tail/binary>> = Bin, pre_hex, Acc) ->
    dump_to_bin0(Bin, hex, Acc);
% Parse 2 hex digits followed by single space (continue hex parsing)
dump_to_bin0(<<D1, D2, $\ , Tail/binary>>, hex, Acc) when
    ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f) orelse
        (D1 >= $A andalso D1 =< $F)) andalso
        ((D2 >= $0 andalso D2 =< $9) orelse (D2 >= $a andalso D2 =< $f) orelse
            (D2 >= $A andalso D2 =< $F))
->
    Byte = list_to_integer([D1, D2], 16),
    dump_to_bin0(Tail, hex, [Byte | Acc]);
% Parse 4 hex digits followed by space or tab (armv6m format: "46fe " or "46fe\t")
% This handles 16-bit Thumb instructions
dump_to_bin0(<<D1, D2, D3, D4, N, Tail/binary>>, hex, Acc) when
    ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f) orelse
        (D1 >= $A andalso D1 =< $F)) andalso
        ((D2 >= $0 andalso D2 =< $9) orelse (D2 >= $a andalso D2 =< $f) orelse
            (D2 >= $A andalso D2 =< $F)) andalso
        ((D3 >= $0 andalso D3 =< $9) orelse (D3 >= $a andalso D3 =< $f) orelse
            (D3 >= $A andalso D3 =< $F)) andalso
        ((D4 >= $0 andalso D4 =< $9) orelse (D4 >= $a andalso D4 =< $f) orelse
            (D4 >= $A andalso D4 =< $F)) andalso
        (N == $\  orelse N == $\t)
->
    % Parse as 2 bytes in reverse order (objdump shows big-endian, but actual bytes are little-endian)
    % Since Acc is reversed at the end, we add bytes in reverse order here
    B1 = list_to_integer([D3, D4], 16),
    B2 = list_to_integer([D1, D2], 16),
    dump_to_bin0(Tail, hex, [B2, B1 | Acc]);
% Parse 8 hex digits followed by space or tab (aarch64 format: "94000001 " or "94000001\t")
% This handles the case where hex bytes are grouped (e.g., "94000001" instead of "94 00 00 01")
% The bytes are in big-endian in the dump but represent little-endian instruction encoding
dump_to_bin0(<<D1, D2, D3, D4, D5, D6, D7, D8, N, Tail/binary>>, hex, Acc) when
    ((D1 >= $0 andalso D1 =< $9) orelse (D1 >= $a andalso D1 =< $f) orelse
        (D1 >= $A andalso D1 =< $F)) andalso
        ((D2 >= $0 andalso D2 =< $9) orelse (D2 >= $a andalso D2 =< $f) orelse
            (D2 >= $A andalso D2 =< $F)) andalso
        ((D3 >= $0 andalso D3 =< $9) orelse (D3 >= $a andalso D3 =< $f) orelse
            (D3 >= $A andalso D3 =< $F)) andalso
        ((D4 >= $0 andalso D4 =< $9) orelse (D4 >= $a andalso D4 =< $f) orelse
            (D4 >= $A andalso D4 =< $F)) andalso
        ((D5 >= $0 andalso D5 =< $9) orelse (D5 >= $a andalso D5 =< $f) orelse
            (D5 >= $A andalso D5 =< $F)) andalso
        ((D6 >= $0 andalso D6 =< $9) orelse (D6 >= $a andalso D6 =< $f) orelse
            (D6 >= $A andalso D6 =< $F)) andalso
        ((D7 >= $0 andalso D7 =< $9) orelse (D7 >= $a andalso D7 =< $f) orelse
            (D7 >= $A andalso D7 =< $F)) andalso
        ((D8 >= $0 andalso D8 =< $9) orelse (D8 >= $a andalso D8 =< $f) orelse
            (D8 >= $A andalso D8 =< $F)) andalso
        (N == $\  orelse N == $\t)
->
    % Parse as 4 bytes in reverse order (objdump shows big-endian, but actual bytes are little-endian)
    % Since Acc is reversed at the end, we add bytes in reverse order here
    B1 = list_to_integer([D7, D8], 16),
    B2 = list_to_integer([D5, D6], 16),
    B3 = list_to_integer([D3, D4], 16),
    B4 = list_to_integer([D1, D2], 16),
    dump_to_bin0(Tail, hex, [B4, B3, B2, B1 | Acc]);
% Skip extra whitespace in hex section (single space was already handled above)
dump_to_bin0(<<N, Tail/binary>>, hex, Acc) when N == $\  orelse N == $\t ->
    dump_to_bin0(Tail, hex, Acc);
% Newline ends this line, go back to addr state
dump_to_bin0(<<$\n, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
% Any other character in hex state means we hit the instruction - skip to end of line
dump_to_bin0(<<_Other, Tail/binary>>, hex, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
% In instruction state, skip everything until newline
dump_to_bin0(<<$\n, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, addr, Acc);
dump_to_bin0(<<_Other, Tail/binary>>, instr, Acc) ->
    dump_to_bin0(Tail, instr, Acc);
% End of input
dump_to_bin0(<<>>, _, Acc) ->
    list_to_binary(lists:reverse(Acc)).
