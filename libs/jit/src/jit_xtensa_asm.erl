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

-module(jit_xtensa_asm).

-export([
    add/3,
    addi/3,
    sub/3,
    mov/2,
    movi/2,
    l32i/3,
    s32i/3,
    ret/0,
    retw/0,
    callx0/1,
    jx/1,
    and_/3,
    or_/3,
    xor_/3,
    slli/3,
    srli/3,
    srai/3,
    mull/3,
    beqz/2,
    bnez/2,
    beq/3,
    bne/3,
    blt/3,
    bge/3,
    nop/0,
    reg_to_num/1
]).

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
    | a15
    | sp.

%%-----------------------------------------------------------------------------
%% Helper functions
%%-----------------------------------------------------------------------------

%% Convert register atoms to register numbers
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
reg_to_num(a15) -> 15;
%% Stack pointer (SP) is a1
reg_to_num(sp) -> 1.

%%-----------------------------------------------------------------------------
%% Xtensa instructions
%%-----------------------------------------------------------------------------

%% ADD.N - Add (narrow encoding, 16-bit)
%% Format: RRRR, bits [3:0]=0xA, [7:4]=t, [11:8]=s, [15:12]=r
-spec add(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
add(Ar, As, At) ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    Instr = 16#A bor (AtNum bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12),
    <<Instr:16/little>>.

%% ADDI - Add immediate
%% Optimizes to MOV.N when imm=0 and ar=as
%% Uses ADDI.N (16-bit) when imm in [-1, 15], otherwise ADDI (24-bit)
%% ADDI.N format: [15:12]=ar, [11:8]=as, [7:4]=imm4, [3:0]=0xB
%%   imm4: 0 means -1, 1-15 means 1-15
%% ADDI format: RRI8, bits [3:0]=0x2, [7:4]=at, [11:8]=as, [15:12]=0xC (r field!), [23:16]=imm8
-spec addi(xtensa_register(), xtensa_register(), -128..127) -> binary().
addi(At, As, 0) when At =:= As ->
    %% Optimize addi ar, ar, 0 to mov.n ar, ar
    mov(At, As);
addi(At, As, Imm) when is_integer(Imm), Imm >= -1, Imm =< 15 ->
    %% Use ADDI.N narrow encoding
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Imm4 =
        if
            Imm =:= -1 -> 0;
            true -> Imm
        end,
    Instr = 16#B bor (Imm4 bsl 4) bor (AsNum bsl 8) bor (AtNum bsl 12),
    <<Instr:16/little>>;
addi(At, As, Imm) when is_integer(Imm), Imm >= -128, Imm =< 127 ->
    %% Use ADDI wide encoding
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Imm8 = Imm band 16#FF,
    Instr = 16#2 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#C bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
addi(_, _, Imm) ->
    error({unencodable_immediate, Imm}).

%% SUB - Subtract
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=as, [15:12]=ar, [19:16]=0x0, [23:20]=0xC
-spec sub(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
sub(Ar, As, At) ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    Instr = 16#0 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12) bor (16#C bsl 20),
    <<Instr:24/little>>.

%% MOV.N - Move (narrow encoding, 16-bit)
%% Format: RRRR, bits [3:0]=0xD, [7:4]=t, [11:8]=s, [15:12]=0x0
-spec mov(xtensa_register(), xtensa_register()) -> binary().
mov(At, As) ->
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Instr = 16#D bor (AtNum bsl 4) bor (AsNum bsl 8),
    <<Instr:16/little>>.

%% MOVI - Move immediate
%% Uses MOVI.N (16-bit) for specific values in [-32,-16,-8,-4,-2,-1,0-15,16,32,48,64,80,95]
%% MOVI.N uses AI4CONST encoding with (r, s) pairs
%% MOVI format: RRI8, bits [3:0]=0xA, [7:4]=t, [11:8]=s, [15:12]=imm4, [23:16]=imm8
%%   s field should be 2 for MOVI
-spec movi(xtensa_register(), -2048..2047) -> binary().
movi(At, Imm) when Imm >= 0, Imm =< 15 ->
    %% MOVI.N: s=0, r=imm
    AtNum = reg_to_num(At),
    Instr = 16#C bor (AtNum bsl 8) bor (Imm bsl 12),
    <<Instr:16/little>>;
movi(At, 16) ->
    %% MOVI.N: s=1, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#1 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, 32) ->
    %% MOVI.N: s=2, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#2 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, 48) ->
    %% MOVI.N: s=3, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#3 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, 64) ->
    %% MOVI.N: s=4, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#4 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, 80) ->
    %% MOVI.N: s=5, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#5 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, 95) ->
    %% MOVI.N: s=5, r=15
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#5 bsl 4) bor (AtNum bsl 8) bor (16#F bsl 12),
    <<Instr:16/little>>;
movi(At, -32) ->
    %% MOVI.N: s=6, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#6 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, -16) ->
    %% MOVI.N: s=7, r=0
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#7 bsl 4) bor (AtNum bsl 8),
    <<Instr:16/little>>;
movi(At, -8) ->
    %% MOVI.N: s=7, r=8
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#7 bsl 4) bor (AtNum bsl 8) bor (16#8 bsl 12),
    <<Instr:16/little>>;
movi(At, -4) ->
    %% MOVI.N: s=7, r=12
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#7 bsl 4) bor (AtNum bsl 8) bor (16#C bsl 12),
    <<Instr:16/little>>;
movi(At, -2) ->
    %% MOVI.N: s=7, r=14
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#7 bsl 4) bor (AtNum bsl 8) bor (16#E bsl 12),
    <<Instr:16/little>>;
movi(At, -1) ->
    %% MOVI.N: s=7, r=15
    AtNum = reg_to_num(At),
    Instr = 16#C bor (16#7 bsl 4) bor (AtNum bsl 8) bor (16#F bsl 12),
    <<Instr:16/little>>;
movi(At, Imm) when is_integer(Imm), Imm >= -2048, Imm =< 2047 ->
    %% Use MOVI wide encoding
    AtNum = reg_to_num(At),
    Imm12 = Imm band 16#FFF,
    Imm8 = (Imm12 bsr 4) band 16#FF,
    Imm4 = Imm12 band 16#F,
    Instr = 16#A bor (AtNum bsl 4) bor (16#2 bsl 8) bor (Imm4 bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
movi(_, Imm) ->
    error({unencodable_immediate, Imm}).

%% L32I - Load 32-bit immediate offset
%% Uses L32I.N (16-bit) when offset in [0, 60] and multiple of 4, otherwise L32I (24-bit)
%% L32I.N format: [15:12]=imm4, [11:8]=as, [7:4]=at, [3:0]=0x8
%%   imm4 = offset/4
%% L32I format: RRI8, bits [3:0]=0x2, [7:4]=at, [11:8]=as, [15:12]=0x2 (r field!), [23:16]=imm8
-spec l32i(xtensa_register(), xtensa_register(), 0..1020) -> binary().
l32i(At, As, Offset) when
    is_integer(Offset), Offset >= 0, Offset =< 60, (Offset rem 4) =:= 0
->
    %% Use L32I.N narrow encoding
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Imm4 = Offset div 4,
    Instr = 16#8 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (Imm4 bsl 12),
    <<Instr:16/little>>;
l32i(At, As, Offset) when
    is_integer(Offset), Offset >= 0, Offset =< 1020, (Offset rem 4) =:= 0
->
    %% Use L32I wide encoding
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Imm8 = Offset div 4,
    Instr = 16#2 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#2 bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
l32i(_, _, Offset) ->
    error({unencodable_offset, Offset}).

%% S32I - Store 32-bit immediate offset
%% Uses S32I.N (16-bit) when offset in [0, 60] and multiple of 4, otherwise S32I (24-bit)
%% S32I.N format: [15:12]=imm4, [11:8]=as, [7:4]=at, [3:0]=0x9
%%   imm4 = offset/4
%% S32I format: RRI8, bits [3:0]=0x6, [7:4]=at, [11:8]=as, [15:12]=0x2 (r field!), [23:16]=imm8
-spec s32i(xtensa_register(), xtensa_register(), 0..1020) -> binary().
s32i(At, As, Offset) when
    is_integer(Offset), Offset >= 0, Offset =< 60, (Offset rem 4) =:= 0
->
    %% Use S32I.N narrow encoding
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Imm4 = Offset div 4,
    Instr = 16#9 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (Imm4 bsl 12),
    <<Instr:16/little>>;
s32i(At, As, Offset) when
    is_integer(Offset), Offset >= 0, Offset =< 1020, (Offset rem 4) =:= 0
->
    %% Use S32I wide encoding
    AtNum = reg_to_num(At),
    AsNum = reg_to_num(As),
    Imm8 = Offset div 4,
    Instr = 16#6 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#2 bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
s32i(_, _, Offset) ->
    error({unencodable_offset, Offset}).

%% RET.N - Return (narrow encoding, 16-bit)
%% Format: NNNN, bits [3:0]=0xF, [7:4]=0x0, [11:8]=0xD, [15:12]=0xF
-spec ret() -> binary().
ret() ->
    <<16#F00D:16/little>>.

%% RETW.N - Return windowed (narrow encoding, 16-bit)
%% Format: NNNN, bits [3:0]=0xF, [7:4]=0x0, [11:8]=0xD, [15:12]=0xF, [19:16]=0x1
-spec retw() -> binary().
retw() ->
    <<16#F01D:16/little>>.

%% CALLX0 - Call register
%% Format: CALLX, bits [3:0]=0x0, [7:4]=0xC, [11:8]=s, [15:12]=0x0, [19:16]=0x0, [23:20]=0x0
-spec callx0(xtensa_register()) -> binary().
callx0(As) ->
    AsNum = reg_to_num(As),
    Instr = (16#C bsl 4) bor (AsNum bsl 8),
    <<Instr:24/little>>.

%% JX - Jump register
%% Format: CALLX, bits [3:0]=0x0, [7:4]=0xA, [11:8]=s, [15:12]=0x0, [19:16]=0x0, [23:20]=0x0
-spec jx(xtensa_register()) -> binary().
jx(As) ->
    AsNum = reg_to_num(As),
    Instr = (16#A bsl 4) bor (AsNum bsl 8),
    <<Instr:24/little>>.

%% AND - Bitwise AND
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=as, [15:12]=ar, [19:16]=0x0, [23:20]=0x1
-spec and_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
and_(Ar, As, At) ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    Instr = 16#0 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12) bor (16#1 bsl 20),
    <<Instr:24/little>>.

%% OR - Bitwise OR
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=as, [15:12]=ar, [19:16]=0x0, [23:20]=0x2
-spec or_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
or_(Ar, As, At) ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    Instr = 16#0 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12) bor (16#2 bsl 20),
    <<Instr:24/little>>.

%% XOR - Bitwise XOR
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=as, [15:12]=ar, [19:16]=0x0, [23:20]=0x3
-spec xor_(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
xor_(Ar, As, At) ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    Instr = 16#0 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12) bor (16#3 bsl 20),
    <<Instr:24/little>>.

%% SLLI - Shift left logical immediate
%% Format: RRR, bits [3:0]=0x0, [7:4]=op_low, [11:8]=as, [15:12]=ar, [19:16]=op_high, [23:20]=0x1
%% For SLLI, op = 32 - sa (shift amount), split as op_low (bits 0-3) and op_high (bits 4-7)
-spec slli(xtensa_register(), xtensa_register(), 0..31) -> binary().
slli(Ar, As, Sa) when is_integer(Sa), Sa >= 0, Sa =< 31 ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    Op = 32 - Sa,
    OpLow = Op band 16#F,
    OpHigh = (Op bsr 4) band 16#F,
    Instr =
        16#0 bor (OpLow bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12) bor (OpHigh bsl 16) bor
            (16#1 bsl 20),
    <<Instr:24/little>>;
slli(_, _, Sa) ->
    error({unencodable_shift_amount, Sa}).

%% SRLI - Shift right logical immediate
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=sa, [15:12]=ar, [19:16]=0x1, [23:20]=0x4
-spec srli(xtensa_register(), xtensa_register(), 0..15) -> binary().
srli(Ar, At, Sa) when is_integer(Sa), Sa >= 0, Sa =< 15 ->
    ArNum = reg_to_num(Ar),
    AtNum = reg_to_num(At),
    Instr =
        16#0 bor (AtNum bsl 4) bor (Sa bsl 8) bor (ArNum bsl 12) bor (16#1 bsl 16) bor
            (16#4 bsl 20),
    <<Instr:24/little>>;
srli(_, _, Sa) ->
    error({unencodable_shift_amount, Sa}).

%% SRAI - Shift right arithmetic immediate
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=sa_low, [15:12]=ar, [19:16]=0x1, [23:20]=sa_high+2
%% sa is encoded as: sa_low = sa & 0xF, sa_high = sa >> 4, field [23:20] = 2 + sa_high
-spec srai(xtensa_register(), xtensa_register(), 0..31) -> binary().
srai(Ar, At, Sa) when is_integer(Sa), Sa >= 0, Sa =< 31 ->
    ArNum = reg_to_num(Ar),
    AtNum = reg_to_num(At),
    SaLow = Sa band 16#F,
    SaHigh = (Sa bsr 4) band 16#F,
    Instr =
        16#0 bor (AtNum bsl 4) bor (SaLow bsl 8) bor (ArNum bsl 12) bor (16#1 bsl 16) bor
            ((16#2 + SaHigh) bsl 20),
    <<Instr:24/little>>;
srai(_, _, Sa) ->
    error({unencodable_shift_amount, Sa}).

%% MULL - Multiply low (32x32 -> low 32)
%% Format: RRR, bits [3:0]=0x0, [7:4]=at, [11:8]=as, [15:12]=ar, [19:16]=0x2, [23:20]=0x8
-spec mull(xtensa_register(), xtensa_register(), xtensa_register()) -> binary().
mull(Ar, As, At) ->
    ArNum = reg_to_num(Ar),
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    Instr =
        16#0 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (ArNum bsl 12) bor (16#2 bsl 16) bor
            (16#8 bsl 20),
    <<Instr:24/little>>.

%% BEQZ - Branch if equal to zero
%% Uses BEQZ.N (16-bit) when offset is 4, otherwise BEQZ (24-bit)
%% BEQZ.N format for offset=4: [15:12]=0x0, [11:8]=as, [7:4]=0x8, [3:0]=0xC
%% BEQZ format: BRI12, bits [3:0]=0x6, [7:4]=0x1, [11:8]=as, [23:12]=imm12
%%   For PC-relative: imm12 = offset - 4
-spec beqz(xtensa_register(), -128..127) -> binary().
beqz(As, 4) ->
    %% Use BEQZ.N narrow encoding for offset=4
    AsNum = reg_to_num(As),
    Instr = 16#C bor (16#8 bsl 4) bor (AsNum bsl 8),
    <<Instr:16/little>>;
beqz(As, Offset) when is_integer(Offset), Offset >= -128, Offset =< 127 ->
    %% Use BEQZ wide encoding (BRI12)
    AsNum = reg_to_num(As),
    OffsetAdjusted = Offset - 4,
    Imm12 = OffsetAdjusted band 16#FFF,
    Instr = 16#6 bor (16#1 bsl 4) bor (AsNum bsl 8) bor (Imm12 bsl 12),
    <<Instr:24/little>>;
beqz(_, Offset) ->
    error({unencodable_offset, Offset}).

%% BNEZ - Branch if not equal to zero
%% Uses BNEZ.N (16-bit) when offset is 4, otherwise BNEZ (24-bit)
%% BNEZ.N format for offset=4: [15:12]=0x0, [11:8]=as, [7:4]=0xC, [3:0]=0xC
%% BNEZ format: BRI12, bits [3:0]=0x6, [7:4]=0x5, [11:8]=as, [23:12]=imm12
%%   For PC-relative: imm12 = offset - 4
-spec bnez(xtensa_register(), -128..127) -> binary().
bnez(As, 4) ->
    %% Use BNEZ.N narrow encoding for offset=4
    AsNum = reg_to_num(As),
    Instr = 16#C bor (16#C bsl 4) bor (AsNum bsl 8),
    <<Instr:16/little>>;
bnez(As, Offset) when is_integer(Offset), Offset >= -128, Offset =< 127 ->
    %% Use BNEZ wide encoding (BRI12)
    AsNum = reg_to_num(As),
    OffsetAdjusted = Offset - 4,
    Imm12 = OffsetAdjusted band 16#FFF,
    Instr = 16#6 bor (16#5 bsl 4) bor (AsNum bsl 8) bor (Imm12 bsl 12),
    <<Instr:24/little>>;
bnez(_, Offset) ->
    error({unencodable_offset, Offset}).

%% BEQ - Branch if equal
%% Format: BRI8, bits [3:0]=0x7, [7:4]=at, [11:8]=as, [15:12]=0x1, [23:16]=imm8
%% For PC-relative: imm8 = offset - 4
-spec beq(xtensa_register(), xtensa_register(), -128..127) -> binary().
beq(As, At, Offset) when is_integer(Offset), Offset >= -128, Offset =< 127 ->
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    OffsetAdjusted = Offset - 4,
    Imm8 = OffsetAdjusted band 16#FF,
    Instr = 16#7 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#1 bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
beq(_, _, Offset) ->
    error({unencodable_offset, Offset}).

%% BNE - Branch if not equal
%% Format: BRI8, bits [3:0]=0x7, [7:4]=at, [11:8]=as, [15:12]=0x9, [23:16]=imm8
%% For PC-relative: imm8 = offset - 4
-spec bne(xtensa_register(), xtensa_register(), -128..127) -> binary().
bne(As, At, Offset) when is_integer(Offset), Offset >= -128, Offset =< 127 ->
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    OffsetAdjusted = Offset - 4,
    Imm8 = OffsetAdjusted band 16#FF,
    Instr = 16#7 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#9 bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
bne(_, _, Offset) ->
    error({unencodable_offset, Offset}).

%% BLT - Branch if less than (signed)
%% Format: BRI8, bits [3:0]=0x7, [7:4]=at, [11:8]=as, [15:12]=0x2, [23:16]=imm8
%% For PC-relative: imm8 = offset - 4
-spec blt(xtensa_register(), xtensa_register(), -128..127) -> binary().
blt(As, At, Offset) when is_integer(Offset), Offset >= -128, Offset =< 127 ->
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    OffsetAdjusted = Offset - 4,
    Imm8 = OffsetAdjusted band 16#FF,
    Instr = 16#7 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#2 bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
blt(_, _, Offset) ->
    error({unencodable_offset, Offset}).

%% BGE - Branch if greater than or equal (signed)
%% Format: BRI8, bits [3:0]=0x7, [7:4]=at, [11:8]=as, [15:12]=0xA, [23:16]=imm8
%% For PC-relative: imm8 = offset - 4
-spec bge(xtensa_register(), xtensa_register(), -128..127) -> binary().
bge(As, At, Offset) when is_integer(Offset), Offset >= -128, Offset =< 127 ->
    AsNum = reg_to_num(As),
    AtNum = reg_to_num(At),
    OffsetAdjusted = Offset - 4,
    Imm8 = OffsetAdjusted band 16#FF,
    Instr = 16#7 bor (AtNum bsl 4) bor (AsNum bsl 8) bor (16#A bsl 12) bor (Imm8 bsl 16),
    <<Instr:24/little>>;
bge(_, _, Offset) ->
    error({unencodable_offset, Offset}).

%% NOP.N - No operation (narrow encoding, 16-bit)
%% Format: NNNN, bits [3:0]=0xF, [7:4]=0x3, [11:8]=0xD, [15:12]=0xF
-spec nop() -> binary().
nop() ->
    <<16#F03D:16/little>>.
