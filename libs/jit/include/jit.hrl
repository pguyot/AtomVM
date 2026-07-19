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

-define(JIT_FORMAT_VERSION, 9).

% Before adding any new platform to the list below:
% Is it 64-bit big endian? if so, `put_digits` function in jit.erl must be updated to support
% big endian platforms.

-define(JIT_ARCH_X86_64, 1).
-define(JIT_ARCH_AARCH64, 2).
-define(JIT_ARCH_ARMV6M, 3).
-define(JIT_ARCH_RISCV32, 4).
-define(JIT_ARCH_RISCV64, 5).
-define(JIT_ARCH_ARM32, 6).
-define(JIT_ARCH_WASM32, 7).
-define(JIT_ARCH_XTENSA, 8).

-define(JIT_VARIANT_PIC, 1).
-define(JIT_VARIANT_FLOAT32, 2).
-define(JIT_VARIANT_THUMB2, 4).
%% Native code carries a relocation table applied by the loader: primitive calls
%% become a direct branch instead of an indirect load through the native-interface
%% table.
-define(JIT_VARIANT_RELOC, 8).
%% Generated code dispatches *_direct primitive results with the
%% sentinel-continuation contract (entry travels via jit_state->continuation,
%% result 1 = branch to it). Intrinsic to backends that emit it (x86_64).
-define(JIT_VARIANT_DIRECT_CALL, 16).

-define(MAX_REG, 16).
