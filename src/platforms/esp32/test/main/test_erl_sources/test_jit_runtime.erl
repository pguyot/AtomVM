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

% Runtime-JIT driver (AOT-precompiled). Boots the kernel supervisor so that
% code_server is running and registered, then makes a plain remote call into
% test_jit_runtime_guarded, which is packaged as plain BEAM bytecode (no AOT
% native code). In an AVM_NO_EMU build that call cannot be emulated: the JIT
% call site routes through jit_trap_and_load, which suspends this process and
% sends {load, test_jit_runtime_guarded, self()} to code_server; code_server
% JIT-compiles the module on-device (to flash via jit_stream_flash on esp32)
% and resumes us. This is the only esp32 test exercising the on-device
% runtime-JIT path (all other JIT tests run AOT-precompiled code).
-module(test_jit_runtime).

-export([start/0]).

start() ->
    % Start the kernel application supervisor, exactly as init:boot/1 does on
    % a normal boot. Its only child is code_server, a gen_server registered
    % under the name code_server, which jit_trap_and_load requires.
    {ok, _KernelPid} = kernel:start(boot, []),
    % The target module must not have native code yet, otherwise this test
    % would not exercise the runtime-JIT path.
    false = code_server:is_loaded(test_jit_runtime_guarded),
    % Plain remote call: traps to code_server, gets the module JIT-compiled
    % on-device, then runs its self-validating guard-heavy code.
    ok = test_jit_runtime_guarded:run(),
    % The trap-load cycle must have installed native code for the module.
    true = code_server:is_loaded(test_jit_runtime_guarded),
    % Second call goes straight through the (now native) module.
    ok = test_jit_runtime_guarded:run(),
    ok.
