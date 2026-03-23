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

-module(jit_xtensa_asm).

-export([
    % RRR-type arithmetic instructions
    add/3,
    sub/3,
    and_/3,
    or_/3,
    xor_/3,
    addx2/3,
    addx4/3,
    addx8/3,
    % Shift instructions
    sll/3,
    srl/3,
    sra/3,
    slli/3,
    srli/3,
    srai/3,
    ssa8l/1,
    ssl/1,
    ssr/1,
    % Multiply/divide instructions
    mull/3,
    quou/3,
    remu/3,
    quos/3,
    rems/3,
    % Immediate arithmetic
    addi/3,
    addmi/3,
    % Move instructions
    movi/2,
    mov_n/2,
    % Load instructions
    l32i/2,
    l32i/3,
    l16ui/2,
    l16ui/3,
    l16si/2,
    l16si/3,
    l8ui/2,
    l8ui/3,
    % Store instructions
    s32i/2,
    s32i/3,
    s16i/2,
    s16i/3,
    s8i/2,
    s8i/3,
    % Branch instructions
    beq/3,
    bne/3,
    blt/3,
    bge/3,
    bltu/3,
    bgeu/3,
    beqz/2,
    bnez/2,
    bgez/2,
    bltz/2,
    beqi/3,
    bnei/3,
    blti/3,
    bgei/3,
    bltui/3,
    bgeui/3,
    bbci/3,
    bbsi/3,
    % Jump instructions
    j/1,
    jx/1,
    call0/1,
    callx0/1,
    call8/1,
    callx8/1,
    ret/0,
    ret_n/0,
    retw/0,
    retw_n/0,
    entry/2,
    % Narrow (density) instructions
    add_n/3,
    addi_n/3,
    l32i_n/2,
    l32i_n/3,
    s32i_n/2,
    s32i_n/3,
    movi_n/2,
    beqz_n/2,
    bnez_n/2,
    nop_n/0,
    break_n/2,
    % Upper immediate
    l32r/2,
    % Misc
    nop/0,
    break/2,
    memw/0,
    isync/0,
    dsync/0,
    % Pseudo-instructions
    li/2,
    mv/2,
    neg/2,
    not_/2
]).

-export_type([
    xtensa_register/0
]).

%% Xtensa Assembler for ESP32 (Xtensa LX6/LX7, call0 ABI)
%%
%% Xtensa Register Set (16 registers with call0 ABI):
%%   a0  - Return address
%%   a1  - Stack pointer
%%   a2  - Function argument 0 / Return value
%%   a3  - Function argument 1
%%   a4  - Function argument 2
%%   a5  - Function argument 3
%%   a6  - Function argument 4
%%   a7  - Function argument 5
%%   a8  - Temporary (caller-saved)
%%   a9  - Temporary (caller-saved)
%%   a10 - Temporary (caller-saved)
%%   a11 - Temporary (caller-saved)
%%   a12 - Callee-saved
%%   a13 - Callee-saved
%%   a14 - Callee-saved
%%   a15 - Callee-saved
%%
%% Instruction Encoding:
%%   24-bit (3 bytes) for standard instructions
%%   16-bit (2 bytes) for narrow/density instructions
%%   Byte ordering: little-endian (byte0 = bits[7:0])
%%
%% Instruction Formats (24-bit):
%%   RRR:  op2[23:20] | op1[19:16] | r[15:12] | s[11:8]  | t[7:4]  | op0[3:0]
%%   RRI8: imm8[23:16]| r[15:12]   | s[11:8]  | t[7:4]   | op0[3:0]
%%   RI16: imm16[23:8]             | t[7:4]   | op0[3:0]
%%   CALL: offset[23:6]           | n[5:4]   | op0[3:0]
%%   BRI8: imm8[23:16]| r[15:14]  | s[11:8]  | t[7:4]   | op0[3:0]
%%   BRI12:imm12[23:12]           | s[11:8]  | t[7:4]   | op0[3:0]
%%
%% See: Xtensa Instruction Set Architecture (ISA) Reference Manual

-type xtensa_register() ::
    a0
    | a1
    | a2
    | a3
    | a4
    | a5
    | a6
    | a7
    | a8
    | a9
    | a10
    | a11
    | a12
    | a13
    | a14
    | a15.

%% Register to number mapping
-spec reg_to_num(xtensa_register()) -> 0..15.
reg_to_num(a0) -> 0;
reg_to_num(a1) -> 1;
reg_to_num(a2) -> 2;
reg_to_num(a3) -> 3;
reg_to_num(a4) -> 4;
reg_to_num(a5) -> 5;
reg_to_num(a6) -> 6;
reg_to_num(a7) -> 7;
reg_to_num(a8) -> 8;
reg_to_num(a9) -> 9;
reg_to_num(a10) -> 10;
reg_to_num(a11) -> 11;
reg_to_num(a12) -> 12;
reg_to_num(a13) -> 13;
reg_to_num(a14) -> 14;
reg_to_num(a15) -> 15.

%%=============================================================================
%% 24-bit Instruction Format Encoders
%%=============================================================================

%% RRR format: op1[23:20] | op2[19:16] | r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_rrr(integer(), integer(), integer(), integer(), integer(), integer()) -> binary().
encode_rrr(Op0, T, S, R, Op1, Op2) ->
    Instr = (Op1 bsl 20) bor (Op2 bsl 16) bor (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% RRI8 format: imm8[23:16] | r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_rri8(integer(), integer(), integer(), integer(), integer()) -> binary().
encode_rri8(Op0, T, S, R, Imm8) ->
    Instr = ((Imm8 band 16#FF) bsl 16) bor (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% RI16 format: imm16[23:8] | t[7:4] | op0[3:0]
-spec encode_ri16(integer(), integer(), integer()) -> binary().
encode_ri16(Op0, T, Imm16) ->
    Instr = ((Imm16 band 16#FFFF) bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% CALL format: offset[23:6] | n[5:4] | op0[3:0]
-spec encode_call(integer(), integer(), integer()) -> binary().
encode_call(Op0, N, Offset18) ->
    Instr = ((Offset18 band 16#3FFFF) bsl 6) bor (N bsl 4) bor Op0,
    <<Instr:24/little>>.

%% BRI8 format for conditional branches: imm8[23:16] | r[15:14]|as[13:12] | s[11:8] | t[7:4] | op0[3:0]
%% Actually the branch format varies. For BEQ/BNE/BLT/BGE/BLTU/BGEU:
%% The encoding is: imm8[23:16] | r[15:12] | s[11:8] | t[7:4] | op0[3:0]
%% where op0=7 and r encodes the branch type
-spec encode_bri8(integer(), integer(), integer(), integer(), integer()) -> binary().
encode_bri8(Op0, T, S, R, Imm8) ->
    Instr = ((Imm8 band 16#FF) bsl 16) bor (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%% BRI12 format for BEQZ/BNEZ/BGEZ/BLTZ:
%% imm12[23:12] | s[11:8] | t[7:4] | op0[3:0]
%% where imm12 is split: m[13:12] encodes branch type, imm12[11:0] in [23:12]
-spec encode_bri12(integer(), integer(), integer(), integer()) -> binary().
encode_bri12(Op0, T, S, Imm12) ->
    Instr = ((Imm12 band 16#FFF) bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:24/little>>.

%%=============================================================================
%% 16-bit Narrow Instruction Format Encoders
%%=============================================================================

%% RRRN format: r[15:12] | s[11:8] | t[7:4] | op0[3:0]
-spec encode_rrrn(integer(), integer(), integer(), integer()) -> binary().
encode_rrrn(Op0, T, S, R) ->
    Instr = (R bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:16/little>>.

%% RI7 format (for narrow loads/stores): imm4[15:12] | s[11:8] | t[7:4] | op0[3:0]
%% where the offset encodes as imm4 in [15:12]
-spec encode_ri7n(integer(), integer(), integer(), integer()) -> binary().
encode_ri7n(Op0, T, S, Imm4) ->
    Instr = ((Imm4 band 16#F) bsl 12) bor (S bsl 8) bor (T bsl 4) bor Op0,
    <<Instr:16/little>>.

%% RI6 format (for narrow branch): imm6[15:12,5:4] | s[11:8] | t[7:4] | op0[3:0]
%% Actually BEQZ.N/BNEZ.N: imm6 split: [15:12] has imm6[5:2], [5:4] has imm6[1:0]
%% Full format: imm6[5:2][15:12] | s[11:8] | t[7:4] | imm6[1:0][5:4] | op0[3:0]
%% Wait, actually for narrow branches the encoding is different.
%% Let me use the general narrow branch encoder.

%%=============================================================================
%% RRR-type Arithmetic Instructions
%%=============================================================================

%% ADD: AR[r] = AR[s] + AR[t]
%% op0=0, op1=8, op2=0
-spec add(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
add(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#8, 0).

%% SUB: AR[r] = AR[s] - AR[t]
%% op0=0, op1=12, op2=0
-spec sub(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
sub(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#C, 0).

%% AND: AR[r] = AR[s] & AR[t]
%% op0=0, op1=1, op2=0
-spec and_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
and_(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#1, 0).

%% OR: AR[r] = AR[s] | AR[t]
%% op0=0, op1=2, op2=0
-spec or_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
or_(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#2, 0).

%% XOR: AR[r] = AR[s] ^ AR[t]
%% op0=0, op1=3, op2=0
-spec xor_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
xor_(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#3, 0).

%% ADDX2: AR[r] = (AR[s] << 1) + AR[t]
%% op0=0, op1=9, op2=0
-spec addx2(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
addx2(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#9, 0).

%% ADDX4: AR[r] = (AR[s] << 2) + AR[t]
%% op0=0, op1=10, op2=0
-spec addx4(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
addx4(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#A, 0).

%% ADDX8: AR[r] = (AR[s] << 3) + AR[t]
%% op0=0, op1=11, op2=0
-spec addx8(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
addx8(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#B, 0).

%%=============================================================================
%% Shift Instructions
%%=============================================================================

%% SLL: AR[r] = AR[s] << (32 - SAR)
%% Actually SLL uses the SAR register. Must set SAR with SSL first.
%% op0=0, bits[23:20]=0xA, bits[19:16]=1, t=0
-spec sll(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
sll(Ar, As, _At) ->
    encode_rrr(0, 0, reg_to_num(As), reg_to_num(Ar), 16#A, 16#1).

%% SRL: AR[r] = AR[t] >> SAR (logical)
%% Must set SAR with SSR first.
%% op0=0, bits[23:20]=0x9, bits[19:16]=1, s=0
-spec srl(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
srl(Ar, _As, At) ->
    encode_rrr(0, reg_to_num(At), 0, reg_to_num(Ar), 16#9, 16#1).

%% SRA: AR[r] = AR[t] >> SAR (arithmetic)
%% Must set SAR with SSR first.
%% op0=0, bits[23:20]=0xB, bits[19:16]=1, s=0
-spec sra(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
sra(Ar, _As, At) ->
    encode_rrr(0, reg_to_num(At), 0, reg_to_num(Ar), 16#B, 16#1).

%% SSL: Set SAR for left shift. SAR = 32 - AR[s][4:0]
%% op0=0, op1=4, op2=0, r=1, t=0
-spec ssl(xtensa_register()) -> binary().
ssl(As) ->
    encode_rrr(0, 0, reg_to_num(As), 1, 16#4, 0).

%% SSR: Set SAR for right shift. SAR = AR[s][4:0]
%% op0=0, op1=4, op2=0, r=0, t=0
-spec ssr(xtensa_register()) -> binary().
ssr(As) ->
    encode_rrr(0, 0, reg_to_num(As), 0, 16#4, 0).

%% SSA8L: Set SAR for byte-aligned shifts. SAR = AR[s][1:0] << 3
%% op0=0, op1=4, op2=0, r=2, t=0
-spec ssa8l(xtensa_register()) -> binary().
ssa8l(As) ->
    encode_rrr(0, 0, reg_to_num(As), 2, 16#4, 0).

%% SLLI: AR[r] = AR[s] << sa (1..31)
%% op0=0, RRR format with shift amount encoded as (32 - sa).
%% The encoded value split: sa_enc[4] at bits[23:20], op=1 at bits[19:16],
%% r at bits[15:12], s at bits[11:8], sa_enc[3:0] at bits[7:4].
-spec slli(xtensa_register(), xtensa_register(), 1..31) -> binary().
slli(Ar, As, Sa) when Sa >= 1, Sa =< 31 ->
    SaEnc = 32 - Sa,
    Sa4 = (SaEnc bsr 4) band 1,
    Sa30 = SaEnc band 16#F,
    Instr =
        (Sa4 bsl 20) bor (16#1 bsl 16) bor (reg_to_num(Ar) bsl 12) bor
            (reg_to_num(As) bsl 8) bor (Sa30 bsl 4) bor 0,
    <<Instr:24/little>>.

%% SRLI: AR[r] = AR[t] >> sa (logical, immediate)
%% op0=0, t=At, s=sa[3:0], r=Ar, bits[23:20]=4, bits[19:16]=1
-spec srli(xtensa_register(), xtensa_register(), 0..15) -> binary().
srli(Ar, At, Sa) when Sa >= 0, Sa =< 15 ->
    encode_rrr(0, reg_to_num(At), Sa band 16#F, reg_to_num(Ar), 16#4, 1).

%% SRAI: AR[r] = AR[t] >> sa (arithmetic, immediate)
%% op0=0, bits[23:20] = 2 + sa[4], bits[19:16] = 1, r=Ar, s=sa[3:0], t=At
-spec srai(xtensa_register(), xtensa_register(), 0..31) -> binary().
srai(Ar, At, Sa) when Sa >= 0, Sa =< 31 ->
    Sa4 = (Sa bsr 4) band 1,
    Sa30 = Sa band 16#F,
    Instr =
        ((2 + Sa4) bsl 20) bor (16#1 bsl 16) bor (reg_to_num(Ar) bsl 12) bor
            (Sa30 bsl 8) bor (reg_to_num(At) bsl 4) bor 0,
    <<Instr:24/little>>.

%%=============================================================================
%% Multiply/Divide Instructions
%%=============================================================================

%% MULL: AR[r] = AR[s] * AR[t] (low 32 bits)
%% op0=0, op1=8, op2=2
-spec mull(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
mull(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#8, 16#2).

%% QUOU: AR[r] = AR[s] / AR[t] (unsigned)
%% op0=0, op1=12, op2=2
-spec quou(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
quou(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#C, 16#2).

%% REMU: AR[r] = AR[s] % AR[t] (unsigned)
%% op0=0, op1=14, op2=2
-spec remu(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
remu(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#E, 16#2).

%% QUOS: AR[r] = AR[s] / AR[t] (signed)
%% op0=0, op1=13, op2=2
-spec quos(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
quos(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#D, 16#2).

%% REMS: AR[r] = AR[s] % AR[t] (signed)
%% op0=0, op1=15, op2=2
-spec rems(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
rems(Ar, As, At) ->
    encode_rrr(0, reg_to_num(At), reg_to_num(As), reg_to_num(Ar), 16#F, 16#2).

%%=============================================================================
%% Immediate Arithmetic
%%=============================================================================

%% ADDI: AR[t] = AR[s] + sign_extend(imm8)
%% op0=2, r=0xC
-spec addi(xtensa_register(), xtensa_register(), -128..127) -> binary().
addi(At, As, Imm8) when Imm8 >= -128, Imm8 =< 127 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#C, Imm8).

%% ADDMI: AR[t] = AR[s] + sign_extend(imm8 << 8)
%% op0=2, r=0xD
-spec addmi(xtensa_register(), xtensa_register(), integer()) -> binary().
addmi(At, As, Imm) ->
    Imm8 = (Imm bsr 8) band 16#FF,
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#D, Imm8).

%%=============================================================================
%% Move Instructions
%%=============================================================================

%% MOVI: AR[t] = sign_extend_12(imm)
%% op0=2, r=0xA, imm12 split: bits[11:8] in s field, bits[7:0] in imm8 field
-spec movi(xtensa_register(), -2048..2047) -> binary().
movi(At, Imm) when Imm >= -2048, Imm =< 2047 ->
    Imm12 = Imm band 16#FFF,
    Imm8 = Imm12 band 16#FF,
    Imm118 = (Imm12 bsr 8) band 16#F,
    encode_rri8(16#2, reg_to_num(At), Imm118, 16#A, Imm8).

%% MOV.N: AR[t] = AR[s] (narrow, 2 bytes)
-spec mov_n(xtensa_register(), xtensa_register()) -> binary().
mov_n(At, As) ->
    %% MOV.N encoding: op0=0xD (1101b), t=At, s=As, r=0
    encode_rrrn(16#D, reg_to_num(At), reg_to_num(As), 0).

%%=============================================================================
%% Load Instructions
%%=============================================================================

%% L32I: AR[t] = mem[AR[s] + imm8*4]
%% op0=2, r=2
-spec l32i(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
l32i(At, {As, Offset}) ->
    l32i(At, As, Offset).

-spec l32i(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
l32i(At, As, Offset) when Offset >= 0, Offset =< 1020, (Offset rem 4) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#2, Offset div 4).

%% L16UI: AR[t] = zero_extend(mem16[AR[s] + imm8*2])
%% op0=2, r=1
-spec l16ui(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
l16ui(At, {As, Offset}) ->
    l16ui(At, As, Offset).

-spec l16ui(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
l16ui(At, As, Offset) when Offset >= 0, Offset =< 510, (Offset rem 2) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#1, Offset div 2).

%% L16SI: AR[t] = sign_extend(mem16[AR[s] + imm8*2])
%% op0=2, r=9
-spec l16si(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
l16si(At, {As, Offset}) ->
    l16si(At, As, Offset).

-spec l16si(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
l16si(At, As, Offset) when Offset >= 0, Offset =< 510, (Offset rem 2) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#9, Offset div 2).

%% L8UI: AR[t] = zero_extend(mem8[AR[s] + imm8])
%% op0=2, r=0
-spec l8ui(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
l8ui(At, {As, Offset}) ->
    l8ui(At, As, Offset).

-spec l8ui(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
l8ui(At, As, Offset) when Offset >= 0, Offset =< 255 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#0, Offset).

%%=============================================================================
%% Store Instructions
%%=============================================================================

%% S32I: mem[AR[s] + imm8*4] = AR[t]
%% op0=2, r=6
-spec s32i(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
s32i(At, {As, Offset}) ->
    s32i(At, As, Offset).

-spec s32i(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
s32i(At, As, Offset) when Offset >= 0, Offset =< 1020, (Offset rem 4) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#6, Offset div 4).

%% S16I: mem16[AR[s] + imm8*2] = AR[t][15:0]
%% op0=2, r=5
-spec s16i(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
s16i(At, {As, Offset}) ->
    s16i(At, As, Offset).

-spec s16i(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
s16i(At, As, Offset) when Offset >= 0, Offset =< 510, (Offset rem 2) =:= 0 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#5, Offset div 2).

%% S8I: mem8[AR[s] + imm8] = AR[t][7:0]
%% op0=2, r=4
-spec s8i(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
s8i(At, {As, Offset}) ->
    s8i(At, As, Offset).

-spec s8i(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
s8i(At, As, Offset) when Offset >= 0, Offset =< 255 ->
    encode_rri8(16#2, reg_to_num(At), reg_to_num(As), 16#4, Offset).

%%=============================================================================
%% Branch Instructions
%%=============================================================================

%% BEQ: if AR[s] == AR[t] then PC += sign_extend(imm8)
%% op0=7, r=1
-spec beq(xtensa_register(), xtensa_register(), integer()) -> binary().
beq(As, At, Offset) ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#1, Offset).

%% BNE: if AR[s] != AR[t] then PC += sign_extend(imm8)
%% op0=7, r=9
-spec bne(xtensa_register(), xtensa_register(), integer()) -> binary().
bne(As, At, Offset) ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#9, Offset).

%% BLT: if AR[s] < AR[t] (signed) then PC += sign_extend(imm8)
%% op0=7, r=2
-spec blt(xtensa_register(), xtensa_register(), integer()) -> binary().
blt(As, At, Offset) ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#2, Offset).

%% BGE: if AR[s] >= AR[t] (signed) then PC += sign_extend(imm8)
%% op0=7, r=10 (0xA)
-spec bge(xtensa_register(), xtensa_register(), integer()) -> binary().
bge(As, At, Offset) ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#A, Offset).

%% BLTU: if AR[s] < AR[t] (unsigned) then PC += sign_extend(imm8)
%% op0=7, r=3
-spec bltu(xtensa_register(), xtensa_register(), integer()) -> binary().
bltu(As, At, Offset) ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#3, Offset).

%% BGEU: if AR[s] >= AR[t] (unsigned) then PC += sign_extend(imm8)
%% op0=7, r=11 (0xB)
-spec bgeu(xtensa_register(), xtensa_register(), integer()) -> binary().
bgeu(As, At, Offset) ->
    encode_bri8(16#7, reg_to_num(At), reg_to_num(As), 16#B, Offset).

%% BEQZ: if AR[s] == 0 then PC += sign_extend(imm12)
%% op0=6, t=1 (BZ type), m encodes BEQZ
%% Encoding: imm12[11:0] in bits[23:12], s[11:8], t=0001b[7:4], op0=0110b[3:0]
-spec beqz(xtensa_register(), integer()) -> binary().
beqz(As, Offset) ->
    %% BRI12 format: op0=6, t=1 (BEQZ)
    %% The 12-bit offset is: target = PC + 4 + offset
    %% imm12[11:0] placed at bits [23:12]
    encode_bri12(16#6, 16#1, reg_to_num(As), Offset band 16#FFF).

%% BNEZ: if AR[s] != 0 then PC += sign_extend(imm12)
%% op0=6, t=5 (BNZ type)
-spec bnez(xtensa_register(), integer()) -> binary().
bnez(As, Offset) ->
    encode_bri12(16#6, 16#5, reg_to_num(As), Offset band 16#FFF).

%% BGEZ: if AR[s] >= 0 then PC += sign_extend(imm12)
%% op0=6, t=13 (0xD)
-spec bgez(xtensa_register(), integer()) -> binary().
bgez(As, Offset) ->
    encode_bri12(16#6, 16#D, reg_to_num(As), Offset band 16#FFF).

%% BLTZ: if AR[s] < 0 then PC += sign_extend(imm12)
%% op0=6, t=9
-spec bltz(xtensa_register(), integer()) -> binary().
bltz(As, Offset) ->
    encode_bri12(16#6, 16#9, reg_to_num(As), Offset band 16#FFF).

%% BEQI: if AR[s] == b4const(r) then PC += sign_extend(imm8)
%% op0=6, r encodes constant via b4const table, t=2
%% The 'r' field indexes into b4const: see b4const_encode
-spec beqi(xtensa_register(), integer(), integer()) -> binary().
beqi(As, Imm, Offset) ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#2, reg_to_num(As), R, Offset).

%% BNEI: if AR[s] != b4const(r) then PC += sign_extend(imm8)
%% op0=6, t=6 (0x6)
-spec bnei(xtensa_register(), integer(), integer()) -> binary().
bnei(As, Imm, Offset) ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#6, reg_to_num(As), R, Offset).

%% BLTI: if AR[s] < b4const(r) then PC += sign_extend(imm8)
%% op0=6, t=0xA
-spec blti(xtensa_register(), integer(), integer()) -> binary().
blti(As, Imm, Offset) ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#A, reg_to_num(As), R, Offset).

%% BGEI: if AR[s] >= b4const(r) then PC += sign_extend(imm8)
%% op0=6, t=0xE
-spec bgei(xtensa_register(), integer(), integer()) -> binary().
bgei(As, Imm, Offset) ->
    R = b4const_encode(Imm),
    encode_bri8(16#6, 16#E, reg_to_num(As), R, Offset).

%% BLTUI: if (unsigned)AR[s] < b4constu(r) then PC += sign_extend(imm8)
%% op0=6, t=0xB
-spec bltui(xtensa_register(), integer(), integer()) -> binary().
bltui(As, Imm, Offset) ->
    R = b4constu_encode(Imm),
    encode_bri8(16#6, 16#B, reg_to_num(As), R, Offset).

%% BGEUI: if (unsigned)AR[s] >= b4constu(r) then PC += sign_extend(imm8)
%% op0=6, t=0xF
-spec bgeui(xtensa_register(), integer(), integer()) -> binary().
bgeui(As, Imm, Offset) ->
    R = b4constu_encode(Imm),
    encode_bri8(16#6, 16#F, reg_to_num(As), R, Offset).

%% BBCI: if AR[s] bit b is clear then PC += sign_extend(imm8)
%% op0=7, t=b[3:0], r=6 when b<16, r=7 when b>=16
-spec bbci(xtensa_register(), 0..31, integer()) -> binary().
bbci(As, Bit, Offset) when Bit >= 0, Bit =< 31 ->
    R =
        if
            Bit >= 16 -> 16#7;
            true -> 16#6
        end,
    encode_bri8(16#7, Bit band 16#F, reg_to_num(As), R, Offset).

%% BBSI: if AR[s] bit b is set then PC += sign_extend(imm8)
%% op0=7, t=b[3:0], r=E when b<16, r=F when b>=16
-spec bbsi(xtensa_register(), 0..31, integer()) -> binary().
bbsi(As, Bit, Offset) when Bit >= 0, Bit =< 31 ->
    R =
        if
            Bit >= 16 -> 16#F;
            true -> 16#E
        end,
    encode_bri8(16#7, Bit band 16#F, reg_to_num(As), R, Offset).

%%=============================================================================
%% B4CONST encoding tables
%%=============================================================================

%% B4CONST table maps r field to constants for BEQI/BNEI/BLTI/BGEI
b4const_encode(-1) -> 0;
b4const_encode(1) -> 1;
b4const_encode(2) -> 2;
b4const_encode(3) -> 3;
b4const_encode(4) -> 4;
b4const_encode(5) -> 5;
b4const_encode(6) -> 6;
b4const_encode(7) -> 7;
b4const_encode(8) -> 8;
b4const_encode(10) -> 9;
b4const_encode(12) -> 10;
b4const_encode(16) -> 11;
b4const_encode(32) -> 12;
b4const_encode(64) -> 13;
b4const_encode(128) -> 14;
b4const_encode(256) -> 15.

%% B4CONSTU table maps r field to unsigned constants for BLTUI/BGEUI
b4constu_encode(32768) -> 0;
b4constu_encode(65536) -> 1;
b4constu_encode(2) -> 2;
b4constu_encode(3) -> 3;
b4constu_encode(4) -> 4;
b4constu_encode(5) -> 5;
b4constu_encode(6) -> 6;
b4constu_encode(7) -> 7;
b4constu_encode(8) -> 8;
b4constu_encode(10) -> 9;
b4constu_encode(12) -> 10;
b4constu_encode(16) -> 11;
b4constu_encode(32) -> 12;
b4constu_encode(64) -> 13;
b4constu_encode(128) -> 14;
b4constu_encode(256) -> 15.

%%=============================================================================
%% Jump Instructions
%%=============================================================================

%% J: PC = PC + sign_extend(offset18) + 4
%% CALL format: op0=6, n=0
-spec j(integer()) -> binary().
j(Offset) ->
    encode_call(16#6, 0, Offset band 16#3FFFF).

%% JX: PC = AR[s]
%% RRR format: op0=0, op1=0, op2=0, r=0, t=0, s=register
%% Actually JX: op0=0, r=10 (0xA), s=register, t=0, op1=0, op2=0
%% Wait, JX encoding per ISA: op0=0, op1=0, op2=0, t=0, r=0, s=As
%% Plus the 'n' and 'm' fields for the SNM0 group.
%% JX is in the RST0 group: op0=0, op1=0, op2=0
%% Within RST0, r=0 selects SNM0, and within SNM0:
%%   m[5:4]=2, n[7:6]=0 for JX => t[7:4]=0b1000=8? No...
%% Actually the t field bits [7:4] encode: n[7:6] and m[5:4]
%% JX: n=2 (jump), m=0 => t = (2 bsl 2) bor 0 = 8? Let me reconsider.
%% t field is 4 bits: t[7:4]. The n field is t[3:2] and m is t[1:0]
%% No, within the instruction, t is just t[7:4]. For SNM0 subgroup:
%%   JX has specific r, t values.
%% Per Xtensa ISA: JX a_s:
%%   op0=0, t=0, s=As, r=0, op1=0, op2=0 with some special field
%%   Actually from xtensa manual: JX is encoded as:
%%   Type: CALLX, which is:
%%   op0=0, op1=0, op2=0, r=0, s=register, t=0 and n=2, m=0
%%   Where n and m are sub-fields within the instruction.
%%   The 24-bit encoding: (0 bsl 20)|(0 bsl 16)|(0 bsl 12)|(s bsl 8)|(0 bsl 6)|(2 bsl 4)|0
%%   Wait, n and m are encoded in the r/t fields.
%%   Actually: for CALLX type instructions in Xtensa:
%%   op0=0, op1=0, op2=0, r field encodes: r[3:2]=n, r[1:0]=m
%%   JX: n=2, m=2 => r = (2 bsl 2) bor 2 = 10? No.
%%   JX: n=0, m=2 => r = (0 bsl 2) bor 2 = 2? Hmm.
%%   Let me just use the known encoding directly.
%%   JX is: 0x000a0 | (s << 8) => byte0=0xa0, byte1=s<<0, byte2=0x00
%%   In 24-bit little-endian: byte0=LSB
%%   Actually from disassembly: JX a8 = 0x0080a0
%%   Let me compute: 0x0080a0 = (0 bsl 20)|(0 bsl 16)|(0 bsl 12)|(8 bsl 8)|(10 bsl 4)|0
%%   = (0xa bsl 4) | (8 bsl 8) = 0xa0 | 0x800 = 0x8a0... no.
%%   0x0080a0 as 24 bits: byte0 = 0xa0, byte1 = 0x80, byte2 = 0x00
%%   In value: 0xa0 | (0x80 << 8) | (0x00 << 16) = 0x80a0
%%   Fields: op0 = 0xa0 & 0xF = 0, t = (0xa0 >> 4) & 0xF = 0xa = 10
%%   s = (0x80) & 0xF = 0, r = (0x80 >> 4) & 0xF = 8
%%   op1 = 0, op2 = 0
%%   So JX a8: op0=0, t=10, s=0, r=8... that doesn't match a8=register 8
%%   Wait, register 8 is a8. So s should be 8.
%%   Let me reparse: 0x0080a0 as bytes (little-endian): byte0=0xa0, byte1=0x80, byte2=0x00
%%   24-bit value = byte0 | (byte1 << 8) | (byte2 << 16) = 0xa0 | 0x8000 | 0 = 0x80a0
%%   op0 = bits[3:0] = 0x0
%%   t = bits[7:4] = 0xa = 10
%%   s = bits[11:8] = 0x0
%%   r = bits[15:12] = 0x8 = 8
%%   op1 = bits[19:16] = 0x0
%%   op2 = bits[23:20] = 0x0
%%   Hmm, that gives r=8, s=0 for register a8. That's wrong.
%%   Actually looking again: maybe I have the byte order wrong.
%%   For Xtensa: 0x0080a0 in memory as: first byte=0xa0, second=0x80, third=0x00
%%   Instruction bits: bits[7:0]=0xa0, bits[15:8]=0x80, bits[23:16]=0x00
%%   op0 = bits[3:0] = 0
%%   t = bits[7:4] = 0xa
%%   s = bits[11:8] = 0
%%   r = bits[15:12] = 8
%%   Hmm wait. That means for JX a8: r=8, t=0xa, s=0.
%%   But register a8 has number 8. So maybe it's in the r field?
%%   Actually I think JX encoding might be:
%%   op0=0, op1=0, op2=0, r=0, s=register, t=0 BUT with a different
%%   interpretation. Let me just try the standard encoding:
%%   For JX as, the actual encoding uses:
%%   Essentially the SNM0 sub-opcode with specific bit patterns.
%%   Let me look at it differently. JX uses the CALLX-like format where:
%%   The instruction is: 0 | 0 | 0 | As | 0xa | 0  (reading fields MSB to LSB)
%%   = (0 << 20) | (0 << 16) | (0 << 12) | (As << 8) | (0xa << 4) | 0
%%   No wait that gives JX a8 = 0 | 0 | 0 | (8 << 8) | (0xa << 4) | 0 = 0x8a0
%%   That's 0x0008a0 as bytes: 0xa0, 0x08, 0x00
%%   But disassembly showed 0xa0, 0x80, 0x00 = 0x0080a0
%%   Hmm, that means s = (0x80 >> 0) & 0xF = 0, and something is at bits 11:8
%%   Actually wait: bits[11:8] of 0x80a0: (0x80a0 >> 8) & 0xF = 0x80 & 0xF = 0
%%   bits[15:12] of 0x80a0: (0x80a0 >> 12) & 0xF = 0x8
%%   Hmm. OK so maybe the register is in r, not s for JX?
%%   Let me just encode it as: encode_rrr with appropriate fields.
%%   If JX a8 = 0x0080a0, then:
%%   op0=0, t=0xa, s=0, r=8(a8), op1=0, op2=0
%%   This means JX encodes register in r field, t=0xa as subop code.
%%   Let me verify with JX a2: r=2, t=0xa, rest=0
%%   = (0xa << 4) | (2 << 12) = 0xa0 | 0x2000 = 0x20a0
%%   bytes: 0xa0, 0x20, 0x00
%%   So JX encoding: op0=0, t=0xa, s=0, r=register, op1=0, op2=0
-spec jx(xtensa_register()) -> binary().
jx(As) ->
    encode_rrr(0, 16#A, reg_to_num(As), 0, 0, 0).

%% CALL0: call relative, PC-relative with 18-bit offset * 4
%% op0=5, n=0
%% Target = ((PC + 4) & ~3) + (sign_extend(offset18) << 2)
-spec call0(integer()) -> binary().
call0(Offset) ->
    %% Offset is already the byte offset from (PC & ~3) + 4
    %% The encoded offset18 = Offset >> 2
    Offset18 = (Offset bsr 2) band 16#3FFFF,
    encode_call(16#5, 0, Offset18).

%% CALLX0: call register, a0 = PC + 3, PC = AR[s]
%% Similar to JX but with link: op0=0, t=0xC, s=0, r=register, op1=0, op2=0
-spec callx0(xtensa_register()) -> binary().
callx0(As) ->
    encode_rrr(0, 16#C, reg_to_num(As), 0, 0, 0).

%% RET: return from subroutine (call0 ABI). PC = a0
%% Same as JX a0 but with specific encoding
%% RET: op0=0, t=0x8, s=0, r=0, op1=0, op2=0
-spec ret() -> binary().
ret() ->
    encode_rrr(0, 16#8, 0, 0, 0, 0).

%% RET.N: narrow return (2 bytes)
%% Encoding: 0xf00d (little-endian: byte0=0x0d, byte1=0xf0)
-spec ret_n() -> binary().
ret_n() ->
    <<16#0D, 16#F0>>.

%% RETW: return from windowed subroutine. Rotates window back and PC = a0[29:0]
%% op0=0, t=0x9, s=0, r=0, op1=0, op2=0
-spec retw() -> binary().
retw() ->
    encode_rrr(0, 16#9, 0, 0, 0, 0).

%% RETW.N: narrow windowed return (2 bytes)
-spec retw_n() -> binary().
retw_n() ->
    <<16#1D, 16#F0>>.

%% CALL8: PC-relative windowed call with 8-register rotation
%% CALL format: op0=5, n=2
-spec call8(integer()) -> binary().
call8(Offset) ->
    Offset18 = (Offset bsr 2) band 16#3FFFF,
    encode_call(16#5, 2, Offset18).

%% CALLX8: indirect windowed call through register with 8-register rotation
%% op0=0, t=0xE, s=register, r=0, op1=0, op2=0
-spec callx8(xtensa_register()) -> binary().
callx8(As) ->
    encode_rrr(0, 16#E, reg_to_num(As), 0, 0, 0).

%% ENTRY: allocate stack frame and rotate register window
%% BRI12 format: op0=6, t=3, s=register, imm12=framesize/8
-spec entry(xtensa_register(), non_neg_integer()) -> binary().
entry(As, FrameSize) when (FrameSize rem 8) =:= 0, FrameSize >= 0, FrameSize =< 32760 ->
    Imm12 = FrameSize bsr 3,
    encode_bri12(16#6, 16#3, reg_to_num(As), Imm12 band 16#FFF).

%%=============================================================================
%% Narrow (Density) Instructions - 16-bit
%%=============================================================================

%% ADD.N: AR[r] = AR[s] + AR[t]
%% op0=0xA (1010b)
-spec add_n(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
add_n(Ar, As, At) ->
    encode_rrrn(16#A, reg_to_num(At), reg_to_num(As), reg_to_num(Ar)).

%% ADDI.N: AR[r] = AR[s] + imm
%% op0=0xB (1011b), imm encoded in r and t fields
%% The immediate is encoded as: if imm == -1, then r=0 t=0 special
%% For imm 0..15 certain encoding applies.
%% Actually ADDI.N: imm is in t field (0 means -1, 1..15 mean 1..15)
%% No, ADDI.N encoding: op0=0xB, r=Ar, s=As, t encodes immediate
%% t values: 0 = -1, 1..15 = 1..15
-spec addi_n(xtensa_register(), xtensa_register(), -1 | 1..15) -> binary().
addi_n(Ar, As, -1) ->
    encode_rrrn(16#B, 0, reg_to_num(As), reg_to_num(Ar));
addi_n(Ar, As, Imm) when Imm >= 1, Imm =< 15 ->
    encode_rrrn(16#B, Imm, reg_to_num(As), reg_to_num(Ar)).

%% L32I.N: AR[t] = mem[AR[s] + imm4*4]
%% op0=0x8 (1000b)
-spec l32i_n(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
l32i_n(At, {As, Offset}) ->
    l32i_n(At, As, Offset).

-spec l32i_n(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
l32i_n(At, As, Offset) when Offset >= 0, Offset =< 60, (Offset rem 4) =:= 0 ->
    encode_ri7n(16#8, reg_to_num(At), reg_to_num(As), Offset div 4).

%% S32I.N: mem[AR[s] + imm4*4] = AR[t]
%% op0=0x9 (1001b)
-spec s32i_n(xtensa_register(), {xtensa_register(), non_neg_integer()}) -> binary().
s32i_n(At, {As, Offset}) ->
    s32i_n(At, As, Offset).

-spec s32i_n(xtensa_register(), xtensa_register(), non_neg_integer()) -> binary().
s32i_n(At, As, Offset) when Offset >= 0, Offset =< 60, (Offset rem 4) =:= 0 ->
    encode_ri7n(16#9, reg_to_num(At), reg_to_num(As), Offset div 4).

%% MOVI.N: AR[s] = imm
%% op0=0xC in some docs, but actually the narrow MOVI uses a special encoding
%% MOVI.N format: imm7 split: imm[6] at bit 7, imm[5:0] at bits [15:12,11:8]
%% Actually, MOVI.N: s=As, imm7 with bits split
%% MOVI.N encoding: bits[3:0]=0xC, bits[7:4]=s, bits[11:8]=imm[3:0], bits[15:12]=imm[6:4]
%% Wait, that's not quite right. Let me use the correct encoding:
%% MOVI.N: 2-byte instruction
%% Byte layout: [imm6[5:4]:imm6[3:0] | s | 0xC] but this isn't standard RRRN
%% Actually MOVI.N uses the RI7 encoding:
%% op0 = 0xC (for some narrow instructions), with specific sub-encoding
%% MOVI.N: bits[3:0] = 0xC, bits[7:4] = s (register), imm7 split
%% Range: -32..95 (7-bit value where bit 6 is stored separately)
%% Encoding: bits[15:12] = imm[3:0], bits[11:8] = imm[6:4] | some flag
%% This is getting complex. Let me use a direct encoding:
-spec movi_n(xtensa_register(), -32..95) -> binary().
movi_n(As, Imm) when Imm >= -32, Imm =< 95 ->
    %% MOVI.N encoding:
    %% Format: imm[3:0][15:12] | s[11:8] | imm[6:4][7:4] | op0=0xC[3:0]
    Imm7 = Imm band 16#7F,
    Low4 = Imm7 band 16#F,
    Hi3 = (Imm7 bsr 4) band 16#7,
    SNum = reg_to_num(As),
    Instr = (Low4 bsl 12) bor (SNum bsl 8) bor (Hi3 bsl 4) bor 16#C,
    <<Instr:16/little>>.

%% BEQZ.N: if AR[s] == 0 then PC += imm6
%% Format: imm[3:0][15:12] | s[11:8] | (8|imm[5:4])[7:4] | op0=0xC[3:0]
-spec beqz_n(xtensa_register(), integer()) -> binary().
beqz_n(As, Offset) when Offset >= 0, Offset =< 63 ->
    Imm30 = Offset band 16#F,
    Imm54 = (Offset bsr 4) band 16#3,
    SNum = reg_to_num(As),
    Instr = (Imm30 bsl 12) bor (SNum bsl 8) bor ((16#8 bor Imm54) bsl 4) bor 16#C,
    <<Instr:16/little>>.

%% BNEZ.N: if AR[s] != 0 then PC += imm6
%% Format: imm[3:0][15:12] | s[11:8] | (0xC|imm[5:4])[7:4] | op0=0xC[3:0]
-spec bnez_n(xtensa_register(), integer()) -> binary().
bnez_n(As, Offset) when Offset >= 0, Offset =< 63 ->
    Imm30 = Offset band 16#F,
    Imm54 = (Offset bsr 4) band 16#3,
    SNum = reg_to_num(As),
    Instr = (Imm30 bsl 12) bor (SNum bsl 8) bor ((16#C bor Imm54) bsl 4) bor 16#C,
    <<Instr:16/little>>.

%% NOP.N: narrow no-operation (2 bytes)
%% Encoding: 0xf03d (little-endian: byte0=0x3d, byte1=0xf0)
-spec nop_n() -> binary().
nop_n() ->
    <<16#3D, 16#F0>>.

%% BREAK.N: narrow breakpoint
%% Encoding: op0=0xD with specific fields
-spec break_n(integer(), integer()) -> binary().
break_n(S, T) ->
    Instr = (16#F bsl 12) bor ((S band 16#F) bsl 8) bor ((T band 16#F) bsl 4) bor 16#D,
    <<Instr:16/little>>.

%%=============================================================================
%% L32R and Misc Instructions
%%=============================================================================

%% L32R: AR[t] = mem[((PC + 3) & ~3) + sign_extend(imm16) * 4 - ...]
%% op0=1
-spec l32r(xtensa_register(), integer()) -> binary().
l32r(At, Imm16) ->
    encode_ri16(16#1, reg_to_num(At), Imm16).

%% NOP: standard 3-byte no-operation
%% Encoding: OR a1, a1, a1 or specific NOP encoding
%% Actually Xtensa NOP is encoded as: 0x20f0 (3 bytes: 0xf0, 0x20, 0x00)
%% Wait, NOP is a pseudo-instruction.
%% The actual encoding of NOP (3-byte) is:
%% op0=0, op1=0, op2=2, r=0xF, s=0, t=0 = "OR a0, a0, a0" effectively
%% But the standard NOP encoding for Xtensa is:
%% Bytes: 0x20, 0xf0, 0x00 in memory -> value = 0x00f020
%% That gives op0=0, t=2, s=0, r=15, op1=0, op2=0
%% Actually that doesn't decode right either.
%% Let me just define NOP as: encode_rrr(0, 0, 0, 16#F, 0, 16#2)
%% Which gives: (2 << 20)|(0 << 16)|(15 << 12)|(0 << 8)|(0 << 4)|0 = 0x20F000
%% bytes: 0x00, 0xF0, 0x20... hmm.
%% Actually standard Xtensa NOP is a 3-byte instruction:
%% Per Xtensa ISA: NOP pseudo maps to "OR a1, a1, a1" in older cores
%% But there's a dedicated NOP opcode in newer cores.
%% The Xtensa NOP (with density option) is typically NOP.N (2 bytes).
%% For 3-byte NOP, use: op0=0, op1=0, op2=2, r=0, s=0, t=15 (WUR-like opcode that does nothing)
%% Actually, the 3-byte NOP encoding is commonly:
%% 0x0020f0 = (0 << 20)|(0 << 16)|(2 << 12)|(0 << 8)|(15 << 4)|0 -> bytes: f0, 20, 00
%% So: encode_rrr(0, 16#F, 0, 16#2, 0, 0)
-spec nop() -> binary().
nop() ->
    %% Equivalent of 'or a1, a1, a1' which is a standard 3-byte NOP
    or_(a1, a1, a1).

%% BREAK: breakpoint/debug trap
%% op0=0, op1=4, op2=0, r=4
-spec break(integer(), integer()) -> binary().
break(S, T) ->
    encode_rrr(0, T band 16#F, S band 16#F, 16#4, 0, 0).

%% MEMW: memory write barrier
%% op0=0, t=0xC, s=0, r=2, op1=0, op2=0
-spec memw() -> binary().
memw() ->
    encode_rrr(0, 16#C, 0, 2, 0, 0).

%% ISYNC: instruction sync
%% op0=0, t=0, s=0, r=2, op1=0, op2=0
-spec isync() -> binary().
isync() ->
    encode_rrr(0, 0, 0, 2, 0, 0).

%% DSYNC: data sync
%% op0=0, t=3, s=0, r=2, op1=0, op2=0
-spec dsync() -> binary().
dsync() ->
    encode_rrr(0, 16#3, 0, 2, 0, 0).

%%=============================================================================
%% Pseudo-instructions
%%=============================================================================

%% LI: Load 32-bit immediate value into register
%% Uses MOVI for small values, MOVI+ADDMI for medium, or shift sequences for large
-spec li(xtensa_register(), integer()) -> binary().
li(At, Imm) when Imm >= -2048, Imm =< 2047 ->
    %% Fits in MOVI's 12-bit signed immediate
    movi(At, Imm);
li(At, Imm) when Imm >= -32768, Imm =< 32767 ->
    %% Use MOVI + ADDMI for values in -32768..32767 range
    %% ADDMI adds imm8<<8 (sign-extended), so the high part must be a multiple of 256
    %% Split value into low part (fits in MOVI) and high part (multiple of 256)
    %% Strategy: find Low in -2048..2047 such that (Imm - Low) is a multiple of 256
    %% and (Imm - Low) >> 8 fits in -128..127
    Low0 = Imm band 16#FF,
    LowSigned0 =
        if
            Low0 >= 128 -> Low0 - 256;
            true -> Low0
        end,
    High0 = Imm - LowSigned0,
    HighByte0 = High0 bsr 8,
    {LowSigned, High} =
        if
            HighByte0 >= -128, HighByte0 =< 127 ->
                {LowSigned0, High0};
            HighByte0 > 127 ->
                {LowSigned0 + 256, High0 - 256};
            true ->
                {LowSigned0 - 256, High0 + 256}
        end,
    <<(movi(At, LowSigned))/binary, (addmi(At, At, High))/binary>>;
li(At, Imm) ->
    %% For arbitrary 32-bit values, build from top down using MOVI + SLLI + ADDI
    %% Strategy: decompose into groups that fit instruction immediate ranges
    Imm32 = Imm band 16#FFFFFFFF,
    %% Try to decompose as: upper bits loaded with MOVI, shifted left, then lower added
    %% We need to handle any 32-bit value. Use byte-by-byte construction.
    %% Approach: MOVI (top 12 bits sign-extended), SLLI 8, ADDI (next 8), SLLI 8, ADDI (low 8)
    %% But that's 5 instructions. We can do better in many cases.
    %%
    %% Optimal approach: MOVI loads up to 12 signed bits, SLLI can shift up to 31,
    %% and ADDI adds -128..127. So for most values we can use:
    %%   MOVI (high bits), SLLI N, ADDI/ORI (low bits)
    %% The challenge is fitting everything.
    %%
    %% General fallback: 4 instructions max for any 32-bit value
    %% MOVI upper 12 bits (signed), SLLI 12, ADDI middle 8, SLLI 8, etc.
    %%
    %% Simpler approach: split into high 16 and low 16.
    %% High16 loaded via MOVI+SLLI, low16 added via ADDMI+ADDI
    %%
    %% Most reliable: MOVI top portion, shift, add remaining bits
    %% Split: bits [31:20] via MOVI (12-bit signed), shift left 20,
    %%         then add bits [19:8] shifted, then add bits [7:0]
    %%
    %% Let's use a clean 3-step approach that handles all 32-bit values:
    %% Step 1: MOVI At, upper_portion (sign-extended 12-bit)
    %% Step 2: SLLI At, At, shift_amount
    %% Step 3: One or two ADDI/ADDMI to add the remaining bits
    li_large(At, Imm32).

%% li_large: load an arbitrary 32-bit value using MOVI + SLLI + ADDI sequences
%% Strategy: find the highest non-zero portion, load it with MOVI,
%% then shift and add remaining portions.
-spec li_large(xtensa_register(), non_neg_integer()) -> binary().
li_large(At, Imm32) ->
    %% Approach: decompose value into a signed 12-bit upper portion
    %% shifted left, plus signed 8-bit additions for the rest.
    %% We load bits [31:20] with MOVI, shift left 12,
    %% then add bits [19:12] via ADDMI, and bits [7:0] via ADDI.
    %%
    %% However, this doesn't always work because bit boundaries don't
    %% align perfectly with sign extension. Use a robust approach:
    %%
    %% Find the position of the highest set bit, then load the top
    %% portion with MOVI (12-bit signed = -2048..2047), shift it into
    %% position, then add remaining bits with ADDMI + ADDI.
    %%
    %% Most general: split into MOVI(high12) << 20 + middle8 << 8 + low8
    %% But ADDMI range is -32768..32512 (step 256) and ADDI range is -128..127.
    %%
    %% Simplest reliable approach for any 32-bit value:
    %% MOVI At, bits[31:24] (as signed byte, -128..127 via MOVI which supports -2048..2047)
    %% SLLI At, At, 8
    %% ADDI At, At, bits[23:16] (signed byte)
    %% SLLI At, At, 8
    %% ADDI At, At, bits[15:8] (signed byte)
    %% SLLI At, At, 8
    %% ADDI At, At, bits[7:0] (signed byte)
    %%
    %% That's 7 instructions worst case. We can optimize common cases.
    %%
    %% Better: MOVI supports -2048..2047 (12 bits signed).
    %% Load top 12 bits, shift 20, add remaining 20 bits in two steps.
    %% But ADDI only adds -128..127, so we'd need more instructions.
    %%
    %% Practical approach: MOVI loads top portion, SLLI shifts it,
    %% then ADDMI (adds imm*256) and ADDI (adds -128..127) handle rest.
    %% ADDMI can add -32768..32512 in steps of 256.
    %%
    %% Best decomposition for most values:
    %% 1. MOVI At, Imm32 >> 16 (top 16 bits, needs to fit in -2048..2047)
    %%    This only works if top 16 bits fit in 12-bit signed.
    %% 2. Otherwise, load top 8 bits, shift, add next chunks.
    %%
    %% Let's use: MOVI(top byte sign-extended) + SLLI 8 + ADDI(next byte) +
    %%            SLLI 8 + ADDI(next byte) + SLLI 8 + ADDI(last byte)
    %% But optimize: skip leading zero bytes, merge adjacent shifts.
    Byte3 = (Imm32 bsr 24) band 16#FF,
    Byte2 = (Imm32 bsr 16) band 16#FF,
    Byte1 = (Imm32 bsr 8) band 16#FF,
    Byte0 = Imm32 band 16#FF,
    %% Sign-extend bytes for ADDI (-128..127)
    SByte3 =
        if
            Byte3 >= 128 -> Byte3 - 256;
            true -> Byte3
        end,
    SByte2 =
        if
            Byte2 >= 128 -> Byte2 - 256;
            true -> Byte2
        end,
    SByte1 =
        if
            Byte1 >= 128 -> Byte1 - 256;
            true -> Byte1
        end,
    SByte0 =
        if
            Byte0 >= 128 -> Byte0 - 256;
            true -> Byte0
        end,
    %% Adjust for sign extension carry: when a byte is negative (>= 128),
    %% the sign extension effectively subtracts 256, so the next higher byte
    %% needs to be incremented by 1 to compensate.
    %% Work from low to high, propagating carries.
    {AB1, Carry0} =
        if
            SByte0 < 0 -> {SByte1 + 1, SByte0};
            true -> {SByte1, SByte0}
        end,
    AB1Adj = AB1 band 16#FF,
    SAB1 =
        if
            AB1Adj >= 128 -> AB1Adj - 256;
            true -> AB1Adj
        end,
    {AB2, Carry1} =
        if
            SAB1 < 0 -> {SByte2 + 1, SAB1};
            true -> {SByte2, SAB1}
        end,
    _ = Carry0,
    AB2Adj = AB2 band 16#FF,
    SAB2 =
        if
            AB2Adj >= 128 -> AB2Adj - 256;
            true -> AB2Adj
        end,
    {AB3, Carry2} =
        if
            SAB2 < 0 -> {SByte3 + 1, SAB2};
            true -> {SByte3, SAB2}
        end,
    _ = Carry1,
    AB3Adj = AB3 band 16#FF,
    SAB3 =
        if
            AB3Adj >= 128 -> AB3Adj - 256;
            true -> AB3Adj
        end,
    _ = Carry2,
    %% Now: value = SAB3 << 24 + SAB2 << 16 + Carry1 << 8 + Carry0
    %% Build: MOVI SAB3, SLLI 8, ADDI SAB2, SLLI 8, ADDI Carry1, SLLI 8, ADDI Carry0
    %% Optimize: skip zero bytes, merge shifts
    li_build(At, SAB3, SAB2, Carry1, Carry0).

-spec li_build(xtensa_register(), integer(), integer(), integer(), integer()) -> binary().
li_build(At, 0, 0, B1, B0) ->
    %% Only 2 bytes significant
    I0 = movi(At, B1),
    I1 = <<(slli(At, At, 8))/binary, (addi(At, At, B0))/binary>>,
    <<I0/binary, I1/binary>>;
li_build(At, 0, B2, B1, B0) ->
    %% 3 bytes significant
    I0 = movi(At, B2),
    I1 = <<(slli(At, At, 8))/binary, (addi(At, At, B1))/binary>>,
    I2 = <<(slli(At, At, 8))/binary, (addi(At, At, B0))/binary>>,
    <<I0/binary, I1/binary, I2/binary>>;
li_build(At, B3, B2, B1, B0) ->
    %% All 4 bytes significant
    %% Start with MOVI for the most significant non-zero byte,
    %% then SLLI + ADDI for each subsequent byte.
    %% All bytes are in -128..127 range (signed).
    %% B3 is the most significant byte (bits 31:24 adjusted).
    %% MOVI supports -2048..2047, so any single byte fits.
    I0 = movi(At, B3),
    I1 = <<(slli(At, At, 8))/binary, (addi(At, At, B2))/binary>>,
    I2 = <<(slli(At, At, 8))/binary, (addi(At, At, B1))/binary>>,
    I3 = <<(slli(At, At, 8))/binary, (addi(At, At, B0))/binary>>,
    <<I0/binary, I1/binary, I2/binary, I3/binary>>.

%% MV: move register (pseudo-instruction)
-spec mv(xtensa_register(), xtensa_register()) -> binary().
mv(Dst, Src) ->
    mov_n(Dst, Src).

%% NEG: negate register
-spec neg(xtensa_register(), xtensa_register()) -> binary().
neg(Ar, At) ->
    %% NEG is a real Xtensa instruction: AR[r] = 0 - AR[t]
    %% op0=0, t=At, s=0, r=Ar, bits[23:20]=6, bits[19:16]=0
    encode_rrr(0, reg_to_num(At), 0, reg_to_num(Ar), 16#6, 0).

%% NOT: bitwise NOT (complement)
-spec not_(xtensa_register(), xtensa_register()) -> binary().
not_(Ar, As) when Ar =:= As ->
    %% When Ar == As, we can't use MOVI Ar, -1 as it would overwrite the source.
    %% Use SUB-based approach: NOT(x) = -1 - x = -(x+1)
    %% NEG gives 0-x, then ADDI gives -x-1 = NOT(x)
    <<(neg(Ar, As))/binary, (addi(Ar, Ar, -1))/binary>>;
not_(Ar, As) ->
    %% MOVI Ar, -1; XOR Ar, Ar, As
    <<(movi(Ar, -1))/binary, (xor_(Ar, Ar, As))/binary>>.
