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

%% @doc Thumb-2 (ARMv7-M / ARMv8-M) instruction assembler.
%%
%% This module encodes Thumb-2 32-bit instructions that are not available
%% in the ARMv6-M (Thumb-1 only) instruction set. It is used as a companion
%% to jit_armv6m_asm when the thumb2 variant is enabled.
%%
%% Reference: ARM Architecture Reference Manual ARMv7-M (DDI 0403E)
%% Thumb-2 instructions are encoded as two 16-bit halfwords, stored
%% little-endian. The first halfword contains the high bits.

-module(jit_armv7m_asm).

-export([
    b_w/1,
    bcc_w/2,
    movw/2,
    movt/2
]).

-type arm_gpr_register() :: jit_armv6m_asm:arm_gpr_register().
-type cc() :: jit_armv6m_asm:cc().

%%-----------------------------------------------------------------------------
%% Thumb-2 32-bit branch (B.W)
%%
%% Encoding T4 (ARMv7-M):
%%   First halfword:  11110 S imm10[9:0]
%%   Second halfword: 10 J1 1 J2 imm11[10:0]
%%
%% Where:
%%   I1 = NOT(J1 XOR S)
%%   I2 = NOT(J2 XOR S)
%%   imm32 = SignExtend(S:I1:I2:imm10:imm11:0, 32)
%%
%% Range: -16777216 to +16777214 (±16 MB), 2-byte aligned
%%
%% The offset is relative to PC (instruction address + 4).
%%-----------------------------------------------------------------------------
-spec b_w(integer()) -> binary().
b_w(Offset) when
    is_integer(Offset),
    Offset >= -16777216,
    Offset =< 16777214,
    (Offset rem 2) =:= 0
->
    encode_branch32(Offset, 2#10, 2#1);
b_w(Offset) ->
    error({unencodable_branch_offset, Offset}).

%%-----------------------------------------------------------------------------
%% Thumb-2 32-bit conditional branch (B<cc>.W)
%%
%% Encoding T3 (ARMv7-M):
%%   First halfword:  11110 S cond[3:0] imm6[5:0]
%%   Second halfword: 10 J1 0 J2 imm11[10:0]
%%
%% Where:
%%   imm32 = SignExtend(S:J2:J1:imm6:imm11:0, 32)
%%
%% Range: -1048576 to +1048574 (±1 MB), 2-byte aligned
%%
%% Note: J1 and J2 are NOT inverted with S for conditional branches
%% (unlike unconditional B.W which uses I1 = NOT(J1 XOR S)).
%%-----------------------------------------------------------------------------
-spec bcc_w(cc(), integer()) -> binary().
bcc_w(Cond, Offset) when
    is_integer(Offset),
    Offset >= -1048576,
    Offset =< 1048574,
    (Offset rem 2) =:= 0
->
    CondNum = jit_armv6m_asm:cond_to_num(Cond),
    %% imm32 = SignExtend(S:J2:J1:imm6:imm11:0, 32)
    %% Encoding the 21-bit signed offset (bit 0 always 0, so we encode bits [20:1])
    Imm21 = (Offset bsr 1) band 16#1FFFFF,
    S = (Imm21 bsr 20) band 1,
    J2 = (Imm21 bsr 19) band 1,
    J1 = (Imm21 bsr 18) band 1,
    Imm6 = (Imm21 bsr 11) band 16#3F,
    Imm11 = Imm21 band 16#7FF,
    HW1 = (2#11110 bsl 11) bor (S bsl 10) bor (CondNum bsl 6) bor Imm6,
    HW2 = (2#10 bsl 14) bor (J1 bsl 13) bor (0 bsl 12) bor (J2 bsl 11) bor Imm11,
    <<HW1:16/little, HW2:16/little>>;
bcc_w(_Cond, Offset) ->
    error({unencodable_conditional_branch_offset, Offset}).

%%-----------------------------------------------------------------------------
%% Thumb-2 MOVW (Move Wide) - loads 16-bit immediate into lower half of register
%%
%% Encoding T3:
%%   First halfword:  11110 i 10 0 1 0 0 imm4[3:0]
%%   Second halfword: 0 imm3[2:0] Rd[3:0] imm8[7:0]
%%
%% Where: imm16 = imm4:i:imm3:imm8
%%
%% Sets lower 16 bits of Rd to imm16, zeroes upper 16 bits.
%%-----------------------------------------------------------------------------
-spec movw(arm_gpr_register(), non_neg_integer()) -> binary().
movw(Rd, Imm16) when is_integer(Imm16), Imm16 >= 0, Imm16 =< 65535 ->
    RdNum = jit_armv6m_asm:reg_to_num(Rd),
    Imm4 = (Imm16 bsr 12) band 16#F,
    I = (Imm16 bsr 11) band 1,
    Imm3 = (Imm16 bsr 8) band 7,
    Imm8 = Imm16 band 16#FF,
    HW1 = (2#11110 bsl 11) bor (I bsl 10) bor (2#100100 bsl 4) bor Imm4,
    HW2 = (0 bsl 15) bor (Imm3 bsl 12) bor (RdNum bsl 8) bor Imm8,
    <<HW1:16/little, HW2:16/little>>;
movw(_Rd, Imm) ->
    error({unencodable_immediate, Imm}).

%%-----------------------------------------------------------------------------
%% Thumb-2 MOVT (Move Top) - loads 16-bit immediate into upper half of register
%%
%% Encoding T1:
%%   First halfword:  11110 i 10 1 1 0 0 imm4[3:0]
%%   Second halfword: 0 imm3[2:0] Rd[3:0] imm8[7:0]
%%
%% Where: imm16 = imm4:i:imm3:imm8
%%
%% Sets upper 16 bits of Rd to imm16, preserves lower 16 bits.
%%-----------------------------------------------------------------------------
-spec movt(arm_gpr_register(), non_neg_integer()) -> binary().
movt(Rd, Imm16) when is_integer(Imm16), Imm16 >= 0, Imm16 =< 65535 ->
    RdNum = jit_armv6m_asm:reg_to_num(Rd),
    Imm4 = (Imm16 bsr 12) band 16#F,
    I = (Imm16 bsr 11) band 1,
    Imm3 = (Imm16 bsr 8) band 7,
    Imm8 = Imm16 band 16#FF,
    HW1 = (2#11110 bsl 11) bor (I bsl 10) bor (2#101100 bsl 4) bor Imm4,
    HW2 = (0 bsl 15) bor (Imm3 bsl 12) bor (RdNum bsl 8) bor Imm8,
    <<HW1:16/little, HW2:16/little>>;
movt(_Rd, Imm) ->
    error({unencodable_immediate, Imm}).

%%-----------------------------------------------------------------------------
%% Internal helpers
%%-----------------------------------------------------------------------------

%% Encode a 32-bit unconditional branch (B.W encoding T4)
%% LinkBit is 2#10 for B.W (bit 14 of second halfword: J1=1, link=0)
%% Actually this is the encoding pattern for the second halfword bits [15:12]
%%
%% First halfword:  11110 S imm10[9:0]
%% Second halfword: 10 J1 1 J2 imm11[10:0]
%%
%% I1 = NOT(J1 XOR S), I2 = NOT(J2 XOR S)
%% imm32 = SignExtend(S:I1:I2:imm10:imm11:0, 32)
encode_branch32(Offset, _SecondHW, _LinkBit) ->
    %% imm32 = SignExtend(S:I1:I2:imm10:imm11:0, 32)
    %% We need to reverse-engineer S, J1, J2, imm10, imm11 from the offset
    Imm25 = (Offset bsr 1) band 16#1FFFFFF,
    S = (Imm25 bsr 24) band 1,
    I1 = (Imm25 bsr 23) band 1,
    I2 = (Imm25 bsr 22) band 1,
    Imm10 = (Imm25 bsr 11) band 16#3FF,
    Imm11 = Imm25 band 16#7FF,
    %% J1 = NOT(I1 XOR S), J2 = NOT(I2 XOR S)
    J1 = (1 - (I1 bxor S)) band 1,
    J2 = (1 - (I2 bxor S)) band 1,
    HW1 = (2#11110 bsl 11) bor (S bsl 10) bor Imm10,
    HW2 = (2#10 bsl 14) bor (J1 bsl 13) bor (1 bsl 12) bor (J2 bsl 11) bor Imm11,
    <<HW1:16/little, HW2:16/little>>.
