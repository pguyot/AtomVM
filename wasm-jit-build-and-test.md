# WASM JIT Backend: Build and Test Guide

## Prerequisites (Ubuntu)

```bash
# System packages
sudo apt-get update
sudo apt-get install -y build-essential cmake ninja-build git curl zlib1g-dev

# Node.js (v18+ for WebAssembly support)
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs

# Erlang/OTP 28+ (required for JIT compiler)
# Using kerl or asdf, or build from source:
# https://www.erlang.org/downloads
# Verify:
erl -eval 'io:format("~s~n", [erlang:system_info(otp_release)]), halt().' -noshell
# Must print "28" or higher

# Emscripten SDK (for building WASM runtime)
cd ~
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk
./emsdk install latest
./emsdk activate latest
source ./emsdk_env.sh
# Add to shell profile:
# echo 'source ~/emsdk/emsdk_env.sh' >> ~/.bashrc
```

## Building

### Step 1: Host build (precompilation of test beams)

```bash
cd /path/to/AtomVM-wasm
mkdir -p build && cd build
cmake -DAVM_DISABLE_JIT=OFF -DAVM_JIT_TARGET_ARCH=wasm32 -G Ninja ..
ninja
```

This compiles the JIT compiler (Erlang), then precompiles all test BEAM files
to WASM32 native code. The precompiled beams are in
`build/tests/erlang_tests/wasm32/`.

### Step 2: Emscripten build (WASM runtime)

```bash
cd /path/to/AtomVM-wasm/src/platforms/emscripten
mkdir -p build && cd build
emcmake cmake -DAVM_DISABLE_JIT=OFF ..
emmake make -j
```

This produces `src/AtomVM.js` and `src/AtomVM.wasm` - the WASM runtime
that can execute precompiled BEAM files.

## Running a single test

```bash
cd /path/to/AtomVM-wasm/src/platforms/emscripten/build
node ./src/AtomVM.js ../../../../build/tests/erlang_tests/wasm32/add.beam
```

Expected output:
```
Downloading (null) failed, HTTP failure status code: 0.
Failed load module: init.beam
Return value: 17
```

The "Downloading" and "Failed load module: init.beam" messages are expected
when running standalone test beams without the full AtomVM standard library.
The important line is `Return value: 17` (= 8 + 9).

## Current test status

`add.beam` executes correctly and returns the expected value.

`test_bs.beam` and other complex tests previously crashed at runtime with
`null function or function signature mismatch`. This was caused by two
`call_indirect` type signature mismatches:

1. **Void-returning primitives**: C functions like `jit_trim_live_regs`,
   `mailbox_remove_message`, `timeout`, etc. return `void`, but the WASM
   type section only had `(i32 x N) -> i32` types. The WASM runtime traps
   when `call_indirect` type doesn't structurally match the target function.
   **Fix**: Added void-returning types (types 8-15) to the WASM type section
   and `primitive_returns_void/1` to select the correct type index.

2. **i64 parameter mismatch**: `alloc_boxed_integer_fragment` takes
   `avm_int64_t` which Emscripten compiles as native WASM `i64`, but the
   JIT was splitting it into two `i32` values.
   **Fix**: Added type 16 `(i32, i64) -> i32` and changed `emit_push_args`
   to use `i64_const` for `avm_int64_t` arguments.

## Architecture overview

1. **Precompilation** (host, Erlang): `jit_precompile.erl` reads BEAM files,
   compiles bytecode to WASM using `jit_wasm32.erl`, and embeds the WASM
   module in an `avmN` chunk in the output BEAM file.

2. **Runtime loading** (Emscripten, C/JS): `module.c` finds the `avmN` chunk,
   `jit_stream_wasm.c` extracts the WASM module binary and compiles it via
   `WebAssembly.Module()` / `WebAssembly.Instance()`. Exported functions are
   registered in Emscripten's indirect function table via `addFunction()`.

3. **Execution** (Emscripten, WASM): The C dispatch loop in `opcodesswitch.h`
   calls JIT-compiled label functions through function pointers. Each label
   function receives `(ctx, jit_state, native_interface)` and calls C
   primitives via `call_indirect` through the native interface table.
