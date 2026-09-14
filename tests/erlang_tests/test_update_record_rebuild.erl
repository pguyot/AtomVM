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

%% Exercises update_record's rebuild path -- the non-inplace one, which copies
%% the whole record and then overwrites the changed fields.
%%
%% The JIT moves that copy several words per instruction: ldp/stp on aarch64,
%% ldm/stm on arm32 and armv6m. The armv6m lowering streams the source pointer
%% with writeback and winds it back afterwards, so the case that matters most
%% is a record whose update is discarded because the value did not change (the
%% reuse hint still holding), since that is the only path that reads the source
%% pointer again after the copy. Sizes here are picked to leave every possible
%% tail length past a whole register group, and some shorter than one.
%%
%% test_update_record_inplace covers the other hint.
-module(test_update_record_rebuild).

-export([start/0]).

-record(r, {
    f01,
    f02,
    f03,
    f04,
    f05,
    f06,
    f07,
    f08,
    f09,
    f10,
    f11,
    f12,
    f13,
    f14,
    f15,
    f16,
    f17,
    f18,
    f19,
    f20
}).

%% The streamed copy moves a group of registers at a time and then a shorter
%% tail, so the record sizes here are picked to leave every possible tail
%% length, and to be shorter than a whole group.
-record(r19, {
    a01,
    a02,
    a03,
    a04,
    a05,
    a06,
    a07,
    a08,
    a09,
    a10,
    a11,
    a12,
    a13,
    a14,
    a15,
    a16,
    a17,
    a18,
    a19
}).
-record(r18, {
    b01,
    b02,
    b03,
    b04,
    b05,
    b06,
    b07,
    b08,
    b09,
    b10,
    b11,
    b12,
    b13,
    b14,
    b15,
    b16,
    b17,
    b18
}).
-record(r4, {c1, c2, c3, c4}).
-record(r2, {d1, d2}).

start() ->
    ok = same_value_keeps_the_original(),
    ok = new_value_rebuilds(),
    ok = every_field_round_trips(),
    ok = short_records(),
    0.

mk() ->
    #r{
        f01 = 1,
        f02 = 2,
        f03 = 3,
        f04 = 4,
        f05 = 5,
        f06 = 6,
        f07 = 7,
        f08 = 8,
        f09 = 9,
        f10 = 10,
        f11 = 11,
        f12 = 12,
        f13 = 13,
        f14 = 14,
        f15 = 15,
        f16 = 16,
        f17 = 17,
        f18 = 18,
        f19 = 19,
        f20 = 20
    }.

%% The reuse hint holds, so the result is the source term itself. If the
%% source pointer was left walked past the end, this returns garbage.
same_value_keeps_the_original() ->
    R = mk(),
    R2 = (opaque(R))#r{f07 = 7},
    true = (R2 =:= R) orelse throw({not_reused, R2}),
    20 = R2#r.f20,
    1 = R2#r.f01,
    ok.

%% The value differs, so the copy is kept and one field is overwritten. Every
%% other field must have survived the streamed copy intact.
new_value_rebuilds() ->
    R = mk(),
    R2 = (opaque(R))#r{f07 = 707},
    707 = R2#r.f07,
    7 = R#r.f07,
    ok = all_but(R2, 7, 707),
    ok.

every_field_round_trips() ->
    every_field_round_trips(1).

every_field_round_trips(N) when N > 20 ->
    ok;
every_field_round_trips(N) ->
    R2 = set_field(opaque(mk()), N),
    ok = all_but(R2, N, N + 1000),
    every_field_round_trips(N + 1).

%% Other arities: tails of every length, and runs shorter than a whole group.
%% The source goes through opaque/1 so the update cannot be constant-folded.
short_records() ->
    A = opaque(mk19()),
    A2 = A#r19{a09 = 99},
    {1, 99, 19} = {A2#r19.a01, A2#r19.a09, A2#r19.a19},
    {1, 9, 19} = {A#r19.a01, A#r19.a09, A#r19.a19},

    B = opaque(mk18()),
    B2 = B#r18{b08 = 88},
    {1, 88, 18} = {B2#r18.b01, B2#r18.b08, B2#r18.b18},

    C = opaque(mk4()),
    C2 = C#r4{c3 = 33},
    {1, 2, 33, 4} = {C2#r4.c1, C2#r4.c2, C2#r4.c3, C2#r4.c4},

    D = opaque(mk2()),
    D2 = D#r2{d2 = 22},
    {1, 22} = {D2#r2.d1, D2#r2.d2},

    %% The reuse hint holding on a short record exercises the rewind there too.
    C3 = C#r4{c3 = 3},
    true = (C3 =:= C) orelse throw({not_reused_short, C3}),
    ok.

mk19() -> #r19{a01 = 1, a09 = 9, a19 = 19}.
mk18() -> #r18{b01 = 1, b08 = 8, b18 = 18}.
mk4() -> #r4{c1 = 1, c2 = 2, c3 = 3, c4 = 4}.
mk2() -> #r2{d1 = 1, d2 = 2}.

all_but(R, Skip, Expected) ->
    all_but(R, Skip, Expected, 1).

all_but(_R, _Skip, _Expected, N) when N > 20 ->
    ok;
all_but(R, Skip, Expected, N) ->
    Got = get_field(R, N),
    Want =
        case N of
            Skip -> Expected;
            _ -> N
        end,
    Got =:= Want orelse throw({field, N, Got, Want}),
    all_but(R, Skip, Expected, N + 1).

get_field(R, N) -> element(N + 1, R).

set_field(R, 1) -> R#r{f01 = 1001};
set_field(R, 2) -> R#r{f02 = 1002};
set_field(R, 3) -> R#r{f03 = 1003};
set_field(R, 4) -> R#r{f04 = 1004};
set_field(R, 5) -> R#r{f05 = 1005};
set_field(R, 6) -> R#r{f06 = 1006};
set_field(R, 7) -> R#r{f07 = 1007};
set_field(R, 8) -> R#r{f08 = 1008};
set_field(R, 9) -> R#r{f09 = 1009};
set_field(R, 10) -> R#r{f10 = 1010};
set_field(R, 11) -> R#r{f11 = 1011};
set_field(R, 12) -> R#r{f12 = 1012};
set_field(R, 13) -> R#r{f13 = 1013};
set_field(R, 14) -> R#r{f14 = 1014};
set_field(R, 15) -> R#r{f15 = 1015};
set_field(R, 16) -> R#r{f16 = 1016};
set_field(R, 17) -> R#r{f17 = 1017};
set_field(R, 18) -> R#r{f18 = 1018};
set_field(R, 19) -> R#r{f19 = 1019};
set_field(R, 20) -> R#r{f20 = 1020}.

%% Keeps the compiler from proving anything about the record.
%%
%% An identity function is not enough: it gets inlined, the record is then a
%% known literal, and an update to a field that already holds that value folds
%% away entirely -- so the reuse-hint path, the only one that reads the source
%% pointer after the copy, never gets emitted. Round-tripping through the
%% process dictionary gives the compiler a term it knows nothing about.
opaque(X) ->
    put(opaque_record, X),
    get(opaque_record).
