<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# Creating a New JIT Backend for AtomVM

This guide walks through every step required to add a new CPU architecture
backend to the AtomVM JIT compiler. It covers the Erlang-side assembler and
backend modules, the C-side runtime integration, the test infrastructure
(including binutils-based validation), and the CI pipeline.

Throughout this guide, the placeholder `myarch` is used for the new
architecture. Replace it with the actual name (e.g. `riscv64`, `mips32`,
`loongarch64`).

> **Prerequisite — Cross binutils**: To run assembler tests on a development
> machine, you need the cross-architecture GNU binutils (assembler and
> objdump). Install them before writing any tests:
>
> ```bash
> # Debian/Ubuntu — example for RISC-V 64-bit:
> sudo apt-get install binutils-riscv64-linux-gnu
>
> # Verify:
> riscv64-linux-gnu-as --version
> riscv64-linux-gnu-objdump --version
> ```
>
> The `jit_tests_common:asm/3` function searches for binutils using the
> prefixes listed in `toolchain_prefixes/1`. If no assembler is found,
> the asm-validation tests silently pass without actually checking —
> so always verify that binutils are installed to get real validation.

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Step 1 — Register the Architecture](#2-step-1--register-the-architecture)
3. [Step 2 — The Assembler Module (`jit_myarch_asm.erl`)](#3-step-2--the-assembler-module-jit_myarch_asmerl)
4. [Step 3 — The Backend Module (`jit_myarch.erl`)](#4-step-3--the-backend-module-jit_myarcherl)
5. [Step 4 — C-Side Runtime Integration](#5-step-4--c-side-runtime-integration)
6. [Step 5 — CMake Build System](#6-step-5--cmake-build-system)
7. [Step 6 — Tests](#7-step-6--tests)
8. [Step 7 — CI Pipeline](#8-step-7--ci-pipeline)
9. [Best Practices](#9-best-practices)
10. [Verifying Test Coverage](#10-verifying-test-coverage)
11. [Porting Between Word Sizes (32-bit ↔ 64-bit)](#11-porting-between-word-sizes-32-bit--64-bit)
12. [Checklist](#12-checklist)

---

## 1. Architecture Overview

The AtomVM JIT is an ahead-of-time and just-in-time compiler that translates
BEAM bytecode into native machine code. The compilation pipeline is:

```
BEAM bytecode
    │
    ▼
jit_precompile.erl   ← orchestrates compilation, parses BEAM chunks
    │
    ▼
jit.erl              ← architecture-independent opcode translation
    │  (calls into the backend module via MMod:function(…) pattern)
    ▼
jit_myarch.erl       ← backend: maps VM operations to native code sequences
    │  (calls into the assembler module)
    ▼
jit_myarch_asm.erl   ← assembler: encodes individual machine instructions
    │
    ▼
Stream module        ← jit_stream_binary | jit_stream_mmap | jit_stream_flash
    │
    ▼
Native code blob     ← stored in "avmN" BEAM chunk or mapped into memory
```

Key design points:

- **`jit.erl`** is the shared frontend. It decodes BEAM opcodes and calls the
  backend module (referred to as `MMod`) for each operation. All backends share
  the same frontend — you never modify `jit.erl` for a new backend.

- **`jit_myarch.erl`** (the backend) translates high-level operations
  (`call_primitive`, `move_to_vm_register`, `jump_to_label`, etc.) into
  sequences of native instructions by calling the assembler module.

- **`jit_myarch_asm.erl`** (the assembler) encodes individual machine
  instructions into binaries. Each function takes register arguments (as atoms
  like `a0`, `t1`, `rax`) and returns an `iodata()` or `binary()`.

- **Stream modules** are pluggable storage backends. `jit_stream_binary` stores
  code as an Erlang binary (for desktop/precompilation), `jit_stream_mmap` uses
  memory-mapped executable pages (for runtime JIT on Unix), and
  `jit_stream_flash` is for microcontrollers.

- **`jit_regs.erl`** tracks register contents and term types across code
  generation to enable optimizations (avoiding redundant loads and type checks).

- **`jit_dwarf.erl`** optionally wraps the stream module to collect metadata
  for generating DWARF debug information and ELF files.

---

## 2. Step 1 — Register the Architecture

Several files need a new architecture constant.

### 2.1 `libs/jit/include/jit.hrl`

Add a new `JIT_ARCH_*` constant:

```erlang
-define(JIT_ARCH_MYARCH, 5).   % next available number
```

> **Important**: Read the comment in `jit.hrl` about big-endian support. If
> your target is 64-bit big-endian, the `put_digits` function in `jit.erl` must
> be updated.

### 2.2 `src/libAtomVM/jit.h`

Add the C-side constant and target detection:

```c
#define JIT_ARCH_MYARCH 5

// In the #ifndef AVM_NO_JIT block, add:
#ifdef __myarch__    // use the correct compiler predefined macro
#define JIT_ARCH_TARGET JIT_ARCH_MYARCH
#define JIT_JUMPTABLE_ENTRY_SIZE <N>   // size of one jump table entry in bytes
#endif
```

The `JIT_JUMPTABLE_ENTRY_SIZE` depends on how many bytes your jump table entry
uses. For example: x86_64 uses 5 (a near jmp), aarch64 uses 4 (a branch),
armv6m uses 12 (load + push + pc-relative add), and riscv32 uses 8
(auipc + jalr).

### 2.3 `libs/jit/src/jit_precompile.erl`

Add the architecture to the `parse_target` result and the `Arch` mapping:

```erlang
% In the Arch case expression:
"myarch" -> ?JIT_ARCH_MYARCH;
```

The backend module name is derived automatically: `list_to_atom("jit_" ++ BaseTarget)`.

### 2.4 `src/libAtomVM/nifs.c`

Add the NIF that returns the backend module atom at runtime:

```c
// In nif_jit_backend_module():
#elif JIT_ARCH_TARGET == JIT_ARCH_MYARCH
    return JIT_MYARCH_ATOM;
```

You will also need to add `JIT_MYARCH_ATOM` to `defaultatoms.def` and
`defaultatoms.h`.

### 2.5 `CMakeLists.txt`

Add auto-detection of the architecture and add it to `AVM_PRECOMPILED_TARGETS`:

```cmake
# In the JIT target architecture detection block:
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^myarch.*$")
    set(AVM_JIT_TARGET_ARCH "myarch")

# Update AVM_PRECOMPILED_TARGETS:
set(AVM_PRECOMPILED_TARGETS "x86_64;aarch64;armv6m;armv6m+float32;riscv32;myarch" ...)
```

---

## 3. Step 2 — The Assembler Module (`jit_myarch_asm.erl`)

Create `libs/jit/src/jit_myarch_asm.erl`.

### 3.1 Module structure

```erlang
-module(jit_myarch_asm).

-export([
    % List every instruction you implement
    add/3,
    sub/3,
    ...
    nop/0,
    ret/0
]).

-export_type([
    myarch_register/0
]).
```

### 3.2 Register type

Define a type for all CPU registers your backend uses:

```erlang
-type myarch_register() ::
    r0 | r1 | r2 | ... | sp | lr | pc.
```

### 3.3 Register encoding

Write a helper that converts register atoms to numeric encodings:

```erlang
-spec reg_to_num(myarch_register()) -> 0..31.
reg_to_num(r0) -> 0;
reg_to_num(r1) -> 1;
...
```

### 3.4 Instruction encoding

For each instruction format in your ISA, write an encoding function. For
example, for R-type RISC-V instructions:

```erlang
%% R-type instruction format:
%% funct7 (7) | rs2 (5) | rs1 (5) | funct3 (3) | rd (5) | opcode (7)
-spec encode_r_type(0..127, myarch_register(), 0..7,
                    myarch_register(), myarch_register(), 0..127) -> binary().
encode_r_type(Opcode, Rd, Funct3, Rs1, Rs2, Funct7) ->
    RdNum = reg_to_num(Rd),
    Rs1Num = reg_to_num(Rs1),
    Rs2Num = reg_to_num(Rs2),
    Instr = (Funct7 bsl 25) bor (Rs2Num bsl 20) bor (Rs1Num bsl 15)
            bor (Funct3 bsl 12) bor (RdNum bsl 7) bor Opcode,
    <<Instr:32/little>>.
```

Then each public instruction function is a thin wrapper:

```erlang
%% ADD rd, rs1, rs2
-spec add(myarch_register(), myarch_register(), myarch_register()) -> binary().
add(Rd, Rs1, Rs2) ->
    encode_r_type(16#33, Rd, 16#0, Rs1, Rs2, 16#00).
```

### 3.5 Best practices for the assembler

- **Each function returns `binary()`** — a raw encoded instruction.
- **Use little-endian encoding** (`<<Instr:32/little>>`) for all little-endian
  architectures. All current AtomVM JIT targets are little-endian.
- **Document the ISA reference** at the top of the module (link to the
  architecture manual).
- **Document the register set** (ABI names, calling convention, which
  registers are callee-saved, which are temporaries).
- **Consider compressed/short instructions** — if your ISA has variable-length
  encodings (like RISC-V C extension or Thumb), implement the short forms and
  have the public functions auto-select the shortest encoding when possible.
  This makes generated code smaller and faster.
- **Test every instruction** — see the test section below.
- **Never use custom functions in Erlang guards** — Erlang only allows BIFs
  and a small set of operators in guard expressions. If you have a helper like
  `is_compressed_reg(R)` that checks whether a register is in the compact
  encoding range, you **cannot** use it in a `when` clause. Use a `case`
  expression instead:

  ```erlang
  %% WRONG — will not compile:
  srli(Rd, Rs1, Shamt) when Rd =:= Rs1, is_compressed_reg(Rd) ->
      c_srli(Rd, Shamt);

  %% CORRECT:
  srli(Rd, Rs1, Shamt) when Rd =:= Rs1, Shamt >= 0, Shamt =< 63 ->
      case is_compressed_reg(Rd) of
          true  -> c_srli(Rd, Shamt);
          false -> encode_i_type(16#13, Rd, 16#5, Rs1, Shamt)
      end;
  ```

---

## 4. Step 3 — The Backend Module (`jit_myarch.erl`)

Create `libs/jit/src/jit_myarch.erl`.

### 4.1 Required exports

The backend must export the following functions, which are called by `jit.erl`
(the `MMod` module):

```erlang
-export([
    word_size/0,
    new/3,
    stream/1,
    offset/1,
    flush/1,
    debugger/1,
    used_regs/1,
    available_regs/1,
    free_native_registers/2,
    assert_all_native_free/1,
    jump_table/2,
    update_branches/1,
    call_primitive/3,
    call_primitive_last/3,
    call_primitive_with_cp/3,
    return_if_not_equal_to_ctx/2,
    jump_to_label/2,
    jump_to_continuation/2,
    jump_to_offset/2,
    cond_jump_to_label/3,
    if_block/3,
    if_else_block/4,
    shift_right/3,
    shift_left/3,
    move_to_vm_register/3,
    move_to_native_register/2,
    move_to_native_register/3,
    move_to_cp/2,
    move_array_element/4,
    move_to_array_element/4,
    move_to_array_element/5,
    set_bs/2,
    copy_to_native_register/2,
    get_array_element/3,
    increment_sp/2,
    set_continuation_to_label/2,
    set_continuation_to_offset/1,
    continuation_entry_point/1,
    get_module_index/1,
    and_/3,
    or_/3,
    add/3,
    sub/3,
    mul/3,
    decrement_reductions_and_maybe_schedule_next/1,
    call_or_schedule_next/2,
    call_only_or_schedule_next/2,
    call_func_ptr/3,
    return_labels_and_lines/2,
    add_label/2,
    add_label/3,
    set_type_tracking/3,
    get_type_tracking/2,
    get_regs_tracking/1,
    xor_/3,
    shift_right_arith/3,
    div_reg/3,
    rem_reg/3
]).
```

If DWARF support is desired (recommended), also export:

```erlang
-ifdef(JIT_DWARF).
-export([
    dwarf_opcode/2,
    dwarf_label/2,
    dwarf_function/3,
    dwarf_line/2,
    dwarf_ctx_register/0
]).
-endif.
```

### 4.2 Includes

```erlang
-include_lib("jit.hrl").
-include("primitives.hrl").
-include("term.hrl").

-ifdef(JIT_DWARF).
-include("jit_dwarf.hrl").
-endif.

-include("jit_backend_dwarf_impl.hrl").
```

### 4.3 State record

```erlang
-record(state, {
    stream_module :: module(),
    stream :: stream(),
    offset :: non_neg_integer(),
    branches :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}],
    jump_table_start :: non_neg_integer(),
    available_regs :: non_neg_integer(),   % bitmask
    used_regs :: non_neg_integer(),        % bitmask
    labels :: [{integer() | reference(), integer()}],
    variant :: non_neg_integer(),
    regs :: jit_regs:regs()                % register contents/type tracking
}).
```

### 4.4 Key design decisions

#### ABI and register allocation

Define how registers map to the JIT calling convention:

```erlang
%% Context pointer — always in first argument register
-define(CTX_REG, a0).        % or r0, rdi, etc.

%% JIT state — second argument register
-define(JITSTATE_REG, a1).   % or r1, rsi, etc.

%% Native interface pointer — third argument register
-define(NATIVE_INTERFACE_REG, a2).

%% Available scratch registers for JIT code generation
-define(AVAILABLE_REGS, [t6, t5, t4, t3, t2, t1, t0]).

%% Registers used for function call parameters
-define(PARAMETER_REGS, [a0, a1, a2, a3, a4, a5, a6, a7]).
```

All JIT entry points follow this signature (defined in `jit.h`):

```c
Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state,
                                   const ModuleNativeInterface *p);
```

#### Context struct offsets

You must determine the field offsets of the `Context` struct for your
architecture (32-bit vs 64-bit, pointer sizes). These are used as immediate
offsets in load/store instructions:

```erlang
% For a 32-bit architecture:
-define(Y_REGS, {?CTX_REG, 16#28}).
-define(X_REG(N), {?CTX_REG, 16#2C + (N * 4)}).
-define(CP, {?CTX_REG, 16#70}).
-define(FP_REGS, {?CTX_REG, 16#74}).
-define(BS, {?CTX_REG, 16#78}).
-define(BS_OFFSET, {?CTX_REG, 16#7C}).

% For a 64-bit architecture:
-define(Y_REGS, {?CTX_REG, 16#50}).
-define(X_REG(N), {?CTX_REG, 16#58 + (N * 8)}).
-define(CP, {?CTX_REG, 16#E0}).
-define(FP_REGS, {?CTX_REG, 16#E8}).
-define(BS, {?CTX_REG, 16#F0}).
-define(BS_OFFSET, {?CTX_REG, 16#F8}).
```

These offsets **must** match the actual C struct layout. They are verified at
compile time by `_Static_assert` statements in `src/libAtomVM/jit.c`.

#### Jump table design

Each label in the BEAM module gets a jump table entry. The design depends on
the instruction set:

- **x86_64**: 5-byte near `jmp rel32`
- **aarch64**: 4-byte `b offset` (26-bit signed offset × 4)
- **armv6m**: 12-byte sequence (load offset, push callee-saved, add pc)
- **riscv32**: 8-byte `auipc + jalr` pair

The jump table is emitted first, then labels are patched during `add_label/2`
and unresolved forward branches are fixed in `update_branches/1`.

### 4.5 Core function implementations

#### `new/3`

```erlang
new(Variant, StreamModule, Stream) ->
    #state{
        stream_module = StreamModule,
        stream = Stream,
        branches = [],
        jump_table_start = 0,
        offset = StreamModule:offset(Stream),
        available_regs = ?AVAILABLE_REGS_MASK,
        used_regs = 0,
        labels = [],
        variant = Variant,
        regs = jit_regs:new()
    }.
```

#### `word_size/0`

```erlang
word_size() -> 4.   % for 32-bit, or 8 for 64-bit
```

#### `call_primitive/3`

This is one of the most important functions. It emits code to call a native
C function through the `ModuleNativeInterface` function pointer table:

1. Load the function pointer from the interface table
2. Save callee-saved registers (ctx, jit_state, interface pointer) to stack
3. Set up arguments according to the platform ABI
4. Call the function (indirect call through register)
5. Move the return value to a scratch register
6. Restore callee-saved registers from stack

#### `cond_jump_to_label/3`

Emits a conditional branch. Since the label offset may not be known yet
(forward reference), record the branch in the `branches` list for later
patching:

```erlang
cond_jump_to_label(State, Condition, Label) ->
    % Emit conditional branch with placeholder offset
    % Record {Offset, Label, BranchType} in State#state.branches
    ...
```

### 4.6 Branch patching

The two-pass approach:

1. **First pass** (`jit.erl:first_pass`): Generates code, records labels and
   branch locations
2. **Second pass** (`update_branches/1`): Patches forward branches and jump
   table entries with correct offsets

The backend must implement `update_branches/1` to iterate over recorded
branches and use `StreamModule:replace/3` to patch the instruction bytes.

---

## 5. Step 4 — C-Side Runtime Integration

### 5.1 `src/libAtomVM/jit.c` — Offset assertions

Add `_Static_assert` blocks for your architecture to verify that the Erlang-side
offset constants match the actual C struct layout:

```c
#elif JIT_ARCH_TARGET == JIT_ARCH_MYARCH
_Static_assert(offsetof(Context, e) == 0x28,
    "ctx->e offset in jit/src/jit_myarch.erl");
_Static_assert(offsetof(Context, x) == 0x2C,
    "ctx->x offset in jit/src/jit_myarch.erl");
_Static_assert(offsetof(Context, cp) == 0x70,
    "ctx->cp offset in jit/src/jit_myarch.erl");
// ... all fields used by the backend
_Static_assert(offsetof(JITState, module) == 0x0,
    "jit_state->module offset");
_Static_assert(offsetof(JITState, continuation) == 0x4,
    "jit_state->continuation offset");
_Static_assert(offsetof(JITState, remaining_reductions) == 0x8,
    "jit_state->remaining_reductions offset");
```

These assertions are critical — if offsets are wrong, generated code will
corrupt memory.

### 5.2 `src/libAtomVM/jit.h` — Entry point

The platform must provide `jit_stream_entry_point()` which converts a JIT
stream term into a callable function pointer. For `mmap`-based platforms this
is already provided. For flash-based platforms (microcontrollers), this reads
from the flash stream.

### 5.3 `ModuleNativeInterface`

The function pointer table in `jit.h` is shared across all architectures. Each
slot corresponds to a `?PRIM_*` constant from `primitives.hrl`. The backend
calls these through indirect function calls:

```
% Load function pointer at index N from the interface table
% The interface pointer is in NATIVE_INTERFACE_REG (e.g. a2)
load RESULT_REG, [NATIVE_INTERFACE_REG + N * word_size]
call RESULT_REG
```

---

## 6. Step 5 — CMake Build System

### 6.1 `libs/jit/src/CMakeLists.txt`

Add the new modules to the `ERLANG_MODULES` list:

```cmake
set(ERLANG_MODULES
    jit
    jit_dwarf
    jit_precompile
    jit_regs
    jit_stream_binary
    jit_stream_flash
    jit_stream_mmap
    jit_aarch64
    jit_aarch64_asm
    jit_armv6m
    jit_armv6m_asm
    jit_riscv32
    jit_riscv32_asm
    jit_x86_64
    jit_x86_64_asm
    jit_myarch          # NEW
    jit_myarch_asm      # NEW
)
```

### 6.2 `tests/libs/jit/CMakeLists.txt`

Add the new test modules:

```cmake
set(ERLANG_MODULES
    tests
    jit_tests
    jit_tests_common
    jit_dwarf_tests
    jit_regs_tests
    jit_aarch64_tests
    jit_aarch64_asm_tests
    jit_armv6m_tests
    jit_armv6m_asm_tests
    jit_riscv32_tests
    jit_riscv32_asm_tests
    jit_x86_64_tests
    jit_x86_64_asm_tests
    jit_myarch_tests          # NEW
    jit_myarch_asm_tests      # NEW
)
```

### 6.3 `CMakeLists.txt` (root)

Update `AVM_PRECOMPILED_TARGETS` and add architecture detection as shown in
Step 1.

---

## 7. Step 6 — Tests

Testing is the most important part of adding a new backend. The AtomVM JIT
uses a layered testing strategy:

### 7.1 Assembler tests (`jit_myarch_asm_tests.erl`)

Create `tests/libs/jit/jit_myarch_asm_tests.erl`.

**Goal**: Test every single instruction your assembler can emit, verifying that
the binary output matches what a reference assembler (binutils) produces.

#### Test structure

```erlang
-module(jit_myarch_asm_tests).

-include_lib("eunit/include/eunit.hrl").

-define(_assertAsmEqual(Bin, Str, Value),
    ?_assertEqual(jit_tests_common:asm(myarch, Bin, Str), Value)
).
```

The `_assertAsmEqual` macro:
1. Calls `jit_myarch_asm:instruction(args)` to get the generated binary
2. Passes the assembly string `Str` to a real assembler (binutils `as`)
3. Disassembles the output with `objdump`
4. Parses the hex bytes from the disassembly
5. Asserts that both binaries are identical

#### Example tests

```erlang
add_test_() ->
    [
        ?_assertAsmEqual(
            <<16#00628533:32/little>>,
            "add a0, t0, t1",
            jit_myarch_asm:add(a0, t0, t1)
        ),
        % Test compressed encoding if available
        ?_assertAsmEqual(
            <<16#95aa:16/little>>,
            "add a1, a1, a0",
            jit_myarch_asm:add(a1, a1, a0)
        )
    ].

% Test every instruction variant:
sub_test_() -> ...
and_test_() -> ...
or_test_() -> ...
lw_test_() -> ...    % load
sw_test_() -> ...    % store
beq_test_() -> ...   % branch
jal_test_() -> ...   % jump
lui_test_() -> ...   % upper immediate
% ... one test generator per instruction
```

#### What to test

- **Every instruction** exported by the assembler module
- **Edge cases**: zero register, maximum immediates, negative offsets,
  boundary values for immediate fields
- **Multiple encodings**: if an instruction has both a 32-bit and compressed
  form, test that the assembler selects the correct one based on operands
- **All register combinations** that exercise different encoding paths
  (e.g., registers in the "compact" range for RISC-V C extension)

#### Common assembler test pitfalls

When writing `_assertAsmEqual` tests, the assembly string passed to the
reference assembler must follow strict conventions:

- **Branches use PC-relative addresses**: Write `"beq t0, t1, .+8"` (relative
  to current PC), not `"beq t0, t1, 8"` (which GNU `as` interprets as an
  absolute address, producing a different offset).

- **Jump-and-link targets are relative**: Write `"jal ra, .+1024"` or
  `"j ."` (for `jal(zero, 0)`), not `"jal ra, 1024"` or `"j 0"`.

- **Preventing unwanted compression**: GNU `as` may automatically compress
  instructions. For example, `nop` becomes `c.nop` (2 bytes instead of 4).
  To test the full-width encoding, prefix with `.option norvc`:

  ```erlang
  ?_assertAsmEqual(<<16#00000013:32/little>>,
      ".option norvc\nnop", jit_myarch_asm:nop())
  ```

- **Shift amounts on RV64**: On RV64, `slli`/`srli`/`srai` use a 6-bit
  shift amount (0–63) and compress when `Rd == Rs1` and the register is
  non-zero. On RV32, the shift amount is only 5 bits. Test both the
  compressed and non-compressed paths, especially for `shamt >= 32`.

### 7.2 Installing cross binutils

Before writing tests, install the cross-architecture GNU binutils for your
target. The assembler tests use `as` to assemble reference instructions and
`objdump` to disassemble both expected and actual code. Without them, the
validation step is silently skipped — tests pass but nothing is actually
checked.

```bash
# Debian/Ubuntu examples:
sudo apt-get install binutils-riscv64-linux-gnu    # RISC-V 64-bit
sudo apt-get install binutils-aarch64-linux-gnu    # AArch64
sudo apt-get install binutils-arm-linux-gnueabi    # ARM 32-bit
sudo apt-get install binutils-riscv64-unknown-elf  # Bare-metal RISC-V

# Verify both tools are available:
riscv64-linux-gnu-as --version
riscv64-linux-gnu-objdump --version
```

Some architectures can share toolchains — for example, `riscv64-linux-gnu-as`
with `-march=rv32imac` can assemble 32-bit RISC-V code. The
`toolchain_prefixes/1` function in `jit_tests_common.erl` handles fallback
lookups. For riscv32, it also tries riscv64 prefixes.

### 7.3 Adding binutils support to `jit_tests_common.erl`

Update the shared test helpers to recognize your architecture.

#### `toolchain_prefixes/1`

```erlang
toolchain_prefixes(myarch) ->
    ["myarch-esp-elf", "myarch-unknown-elf", "myarch-elf",
     "myarch-none-eabi", "myarch-linux-gnu"];
```

For some architectures, you may need to fall back to a different prefix. For
example, `riscv32` also tries `riscv64-*` prefixes because `riscv64-*-objdump`
can disassemble 32-bit code with the right flags.

#### `get_asm_header/1`

Return the assembly directives needed at the top of the `.S` file:

```erlang
get_asm_header(myarch) ->
    ".text\n";
    % For ARM Thumb mode you would need:
    % ".arch armv6-m\n.thumb\n.syntax unified\n"
```

#### `get_as_flags/1`

Return extra flags for the assembler:

```erlang
get_as_flags(myarch) ->
    "";   % e.g. "--64" for x86_64, "-march=rv32imac" for riscv32
```

#### `get_objdump_flags/1`

Return flags for objdump disassembly of raw binaries:

```erlang
get_objdump_flags(myarch) ->
    "-m myarch";   % e.g. "-m i386:x86-64", "-m riscv:rv32", "-marm"
```

### 7.4 Backend tests (`jit_myarch_tests.erl`)

Create `tests/libs/jit/jit_myarch_tests.erl`.

**Goal**: Test the backend module's code generation for high-level operations
(calling primitives, moving values, conditional branches, etc.) by comparing
against expected disassembly output.

#### Test structure

```erlang
-module(jit_myarch_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(BACKEND, jit_myarch).
```

#### Example backend test

```erlang
call_primitive_0_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary,
                          jit_stream_binary:new(0)),
    {State1, ResultReg} = ?BACKEND:call_primitive(State0, 0,
                                                   [ctx, jit_state]),
    ?assertEqual(expected_result_reg, ResultReg),
    Stream = ?BACKEND:stream(State1),
    Dump =
        <<
            "   0:  00062f83            lw  t6,0(a2)\n"
            "   4:  1141                addi    sp,sp,-16\n"
            % ... expected objdump output ...
        >>,
    jit_tests_common:assert_stream(myarch, Dump, Stream).
```

#### How to obtain the expected dump

**Manual approach** — for individual tests:

1. Write the test calling the backend function
2. Print the generated stream: `io:format("~w~n", [Stream])`
3. Write the raw bytes to a file: `file:write_file("/tmp/dump.bin", Stream)`
4. Run `myarch-objdump -b binary <flags> -D /tmp/dump.bin` to get the disassembly
5. Copy the disassembly output into the test as the `Dump` binary

**Dump generator script** — for porting tests from another backend (e.g.
riscv32 → riscv64), write an Erlang module that runs each test scenario
with the new backend and captures the actual disassembly:

```erlang
-module(gen_myarch_dumps).
-export([main/0]).
-include("jit/include/jit.hrl").
-include("jit/src/term.hrl").
-include("jit/src/default_atoms.hrl").
-include("jit/src/primitives.hrl").

-define(B, jit_myarch).

main() ->
    Tests = [{"call_primitive_0", fun call_primitive_0/0}, ...],
    lists:foreach(fun({Name, Fun}) ->
        io:format("=== ~s ===~n", [Name]),
        Stream = Fun(),
        ok = file:write_file("/tmp/dump.bin", Stream),
        io:format("~s~n", [os:cmd(
            "myarch-objdump -b binary -m myarch -D -z /tmp/dump.bin")])
    end, Tests),
    halt().

call_primitive_0() ->
    S0 = ?B:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    {S1, _} = ?B:call_primitive(S0, 0, [ctx, jit_state]),
    ?B:stream(S1).
```

Compile and run with:

```bash
erlc -DTEST -I libs/jit/include -I libs/jit/src -I libs -I tests \
    -pa /tmp/jit_test -o /tmp/jit_test gen_myarch_dumps.erl
erl -noshell -pa /tmp/jit_test -eval 'gen_myarch_dumps:main().'
```

This produces the correct objdump output for every test case at once,
which you then copy into the test file. This is especially useful when
porting a large test suite (50+ tests) from an existing backend.

**Debugging failures**: When a test fails, `diff_disasm/3` from
`jit_tests_common` automatically runs objdump on both expected and actual,
then prints a unified diff. This makes it easy to see exactly which
instructions differ.

#### Compiling and running tests without CMake

For rapid iteration during development, you can compile and run JIT tests
directly with `erlc` and `erl`:

```bash
# Create output directory
mkdir -p /tmp/jit_test

# Compile source modules (order matters — compile dependencies first)
for mod in jit_stream_binary jit_regs jit_riscv64_asm jit_riscv64; do
    erlc -I libs/jit/include -I libs/jit/src -I libs \
        -pa /tmp/jit_test -o /tmp/jit_test libs/jit/src/${mod}.erl
done

# Compile test modules (need -DTEST for eunit includes)
for mod in jit_tests_common jit_riscv64_asm_tests jit_riscv64_tests; do
    erlc -DTEST -I libs/jit/include -I libs/jit/src -I libs -I tests \
        -pa /tmp/jit_test -o /tmp/jit_test tests/libs/jit/${mod}.erl
done

# Run tests
erl -noshell -pa /tmp/jit_test \
    -eval 'eunit:test(jit_riscv64_asm_tests, [verbose]), halt().'
erl -noshell -pa /tmp/jit_test \
    -eval 'eunit:test(jit_riscv64_tests, [verbose]), halt().'
```

Note: test modules that use `-ifdef(TEST)` to guard eunit includes **must**
be compiled with `-DTEST`, or the include will be skipped and macros like
`?assertEqual` will be undefined.

#### What to test

The riscv32 backend has ~48 test functions across ~3500 lines. A new backend
should aim for comparable coverage. The following is the comprehensive list
of operations to test, grouped by category:

**Primitive calls** — the most important tests, exercising the calling
convention, stack save/restore, and argument passing:

- `call_primitive/3` with 0, 1, 2, 5, and 6+ arguments
- `call_primitive_last/3` (tail call — no save/restore) with varying args
- `call_primitive_with_cp/3` (sets continuation pointer before call)
- `call_primitive` with extended registers (register exhaustion scenarios)
- `call_primitive` with few free registers (all scratch regs occupied)

**External calls and scheduling**:

- `call_ext_only` — `decrement_reductions` + `call_primitive_last(PRIM_CALL_EXT, ...)`
- `call_ext_last` — same but with deallocation
- `call_ext` — with `set_continuation_to_offset` and `return_if_not_equal_to_ctx`
- `call_fun` — function call via `PRIM_CALL_FUN`
- `call_func_ptr` — indirect calls through BIF pointers (test stack alignment)
- `call_only_or_schedule_next` — forward label, known label, and large gap
- `call_or_schedule_next` — similar with return address

**Data movement**:

- `move_to_vm_register/3` — immediate to x_reg, x_reg to x_reg, x_reg to y_reg, y_reg to x_reg, etc.
- `move_to_native_register/2` — from x_reg, y_reg, immediate, large immediate
- `move_to_cp/2` — load from y_reg 0 on stack and store to CP field
- `move_array_element/4` — copy between array elements
- `move_to_array_element/4,5` — store to tuple/array positions
- `get_array_element/3` — load from tuple/array positions
- `set_args1_y_reg` — y_reg to x_reg move
- Large y_reg reads and writes (offset > small immediate range)
- Large y_reg with register exhaustion (all scratch regs occupied)
- `y_reg_boundary_direct` — boundary between direct and indirect offset
- `cached_load_after_free` — verify register cache invalidation

**Conditional branches**:

- `if_block/3` — all condition types: `<`, `==`, `!=`, `'(int)'` variants
- `if_block` with `{free, Reg}` (register freed after condition check)
- `if_block` with immediate comparisons: 0, -1, ?TERM_NIL, large values
- `if_else_block/4` — with true and false branches
- `return_if_not_equal_to_ctx/2` — two variants (direct and copied register)
- `is_integer`, `is_number`, `is_boolean` — nested `if_block` patterns
- `is_boolean_far` — long branch offsets
- `jump_table/2` — with label patching and large label counts

**Wait/timeout** (messaging primitives):

- `wait_timeout` — full receive loop with timeout
- `wait` — receive with schedule_wait_cp
- `wait_known` — receive with known label

**Arithmetic and bitwise**:

- `add/3`, `sub/3`, `mul/3` — register and immediate operands
- `and_/3`, `or_/3`, `xor_/3` — bitwise operations
- `shift_left/3`, `shift_right/3`, `shift_right_arith/3`
- Register exhaustion variants for `and_` (negative and positive masks)

**Miscellaneous**:

- `word_size` — verify correct word size (4 for 32-bit, 8 for 64-bit)
- `increment_sp/2` — stack pointer manipulation
- `return_labels_and_lines/2` — label and line metadata
- `gc_bif2` — garbage-collecting BIF with error handling
- `memory_ensure_free_with_roots` — heap allocation primitive
- `alloc_boxed_integer_fragment` — small and large integer allocation
- `debugger/1` — breakpoint instruction
- `add_beam` — complex multi-operation test (type check + arithmetic)

### 7.5 Register the test modules

In `tests/libs/jit/tests.erl`, add the new modules:

```erlang
start() ->
    etest:test([
        jit_tests,
        jit_dwarf_tests,
        % ... existing ...
        jit_myarch_tests,        % NEW
        jit_myarch_asm_tests     % NEW
    ]).
```

### 7.6 Running tests locally

#### With BEAM (for assembler + backend tests with binutils validation)

```bash
mkdir -p build && cd build
cmake -G Ninja ..
ninja
erl -pa tests/libs/jit/beams/ libs/jit/src/beams/ libs/etest/src/beams \
    -noshell -s tests -s init stop
```

This runs the tests using the BEAM VM. The `jit_tests_common:asm/3` function
will find binutils on the system and use them to validate instruction
encoding.

#### With AtomVM (for full JIT execution)

```bash
cmake -G Ninja -DAVM_DISABLE_JIT=OFF \
    -DAVM_JIT_TARGET_ARCH=myarch ..
ninja
./src/AtomVM tests/libs/jit/test_jit.avm
```

#### With valgrind (for memory error detection)

```bash
valgrind --error-exitcode=1 ./src/AtomVM tests/libs/jit/test_jit.avm
```

---

## 8. Step 7 — CI Pipeline

### 8.1 `.github/workflows/build-and-test.yaml`

Add matrix entries for the new architecture. Each architecture typically needs
at minimum:

1. **JIT build** — basic JIT compilation and testing
2. **JIT + DWARF build** — JIT with debug info generation

#### Native architecture (runs on matching hardware)

If GitHub Actions runners exist for your architecture:

```yaml
# JIT build (myarch)
- os: "ubuntu-24.04"          # or appropriate runner
  cc: "cc"
  cxx: "c++"
  cflags: ""
  otp: "28"
  elixir_version: "1.17"
  rebar3_version: "3.24.0"
  cmake_opts_other: "-DAVM_DISABLE_JIT=OFF"
  jit_target_arch: "myarch"

# JIT + DWARF build (myarch)
- os: "ubuntu-24.04"
  cc: "cc"
  cxx: "c++"
  cflags: ""
  otp: "28"
  elixir_version: "1.17"
  rebar3_version: "3.24.0"
  cmake_opts_other: "-DAVM_DISABLE_JIT=OFF -DAVM_DISABLE_JIT_DWARF=OFF"
  jit_target_arch: "myarch"
```

#### Cross-compiled architecture (needs QEMU)

For architectures without native runners, use cross-compilation + QEMU
emulation. There are two patterns depending on whether the cross-toolchain
is available as a standard Debian package:

**Standard Debian cross-toolchain** (e.g., riscv64, s390x, armhf):

If `crossbuild-essential-myarch` is available in Ubuntu, the generic
"Setup cross compilation architecture" step in the workflow handles everything
automatically. You only need to add matrix entries:

```yaml
# myarch plain build (cross-compiled, no JIT)
- os: "ubuntu-24.04"
  cc: "myarch-linux-gnu-gcc"
  cxx: "myarch-linux-gnu-g++"
  cflags: "-O2"
  otp: "28"
  elixir_version: "1.17"
  rebar3_version: "3.24.0"
  cmake_opts_other: "-DAVM_WARNINGS_ARE_ERRORS=ON -DCMAKE_TOOLCHAIN_FILE=${RUNNER_TEMP}/myarch_toolchain.cmake"
  compiler_pkgs: "crossbuild-essential-myarch libc6-dbg:myarch zlib1g-dev:myarch libmbedtls-dev:myarch qemu-user qemu-user-binfmt binfmt-support"
  arch: "myarch"
  library-arch: myarch-linux-gnu

# myarch JIT build
- os: "ubuntu-24.04"
  cc: "myarch-linux-gnu-gcc"
  cxx: "myarch-linux-gnu-g++"
  cflags: "-O2"
  otp: "28"
  elixir_version: "1.17"
  rebar3_version: "3.24.0"
  cmake_opts_other: "-DAVM_DISABLE_JIT=OFF -DAVM_JIT_TARGET_ARCH=myarch -DCMAKE_TOOLCHAIN_FILE=${RUNNER_TEMP}/myarch_toolchain.cmake"
  compiler_pkgs: "crossbuild-essential-myarch libc6-dbg:myarch zlib1g-dev:myarch libmbedtls-dev:myarch qemu-user qemu-user-binfmt binfmt-support"
  arch: "myarch"
  library-arch: myarch-linux-gnu
  jit_target_arch: "myarch"

# JIT + DWARF build (myarch)
- os: "ubuntu-24.04"
  cc: "myarch-linux-gnu-gcc"
  cxx: "myarch-linux-gnu-g++"
  cflags: "-O2"
  otp: "28"
  elixir_version: "1.17"
  rebar3_version: "3.24.0"
  cmake_opts_other: "-DAVM_DISABLE_JIT=OFF -DAVM_DISABLE_JIT_DWARF=OFF -DAVM_JIT_TARGET_ARCH=myarch -DCMAKE_TOOLCHAIN_FILE=${RUNNER_TEMP}/myarch_toolchain.cmake"
  compiler_pkgs: "crossbuild-essential-myarch libc6-dbg:myarch zlib1g-dev:myarch libmbedtls-dev:myarch qemu-user qemu-user-binfmt binfmt-support"
  arch: "myarch"
  library-arch: myarch-linux-gnu
  jit_target_arch: "myarch"
```

Each architecture needs **three** matrix entries: a plain cross-compiled build
(to catch compilation errors without JIT), a JIT build, and a JIT+DWARF build.

The generic setup step creates a CMake toolchain file, adds the ports.ubuntu.com
apt sources for the target architecture, and installs cross-compiled libraries.
The toolchain file is saved at `${RUNNER_TEMP}/${arch}_toolchain.cmake`.

**Custom cross-toolchain** (e.g., riscv32 with ILP32 ABI):

If no standard Debian package exists, you need a dedicated setup step. See the
existing riscv32 setup in `build-and-test.yaml` which downloads a custom
toolchain from a GitHub release, installs custom ABI libraries, sets up QEMU
binfmt, and creates a comprehensive CMake toolchain file with architecture
flags.

#### Cache key

The cache key already includes `matrix.jit_target_arch`, so your new
architecture will automatically get its own cache partition:

```yaml
key: ${{ matrix.otp }}-${{ hashFiles(...) }}-${{ matrix.jit_target_arch || 'nojit' }}-...
```

### 8.2 `.github/workflows/run-tests-with-beam.yaml`

The BEAM-based test workflow already runs `tests/libs/jit/` tests for all
OTP >= 23. No changes needed — your new test modules are automatically
included when they are added to the CMakeLists.txt and `tests.erl`.

### 8.3 Test steps

Ensure the following test steps exist (they already do for the existing
infrastructure — your tests will be included automatically):

1. **`test_jit.avm` with valgrind** — catches memory errors in generated code
2. **`test_jit.avm` without valgrind** — faster execution for cross-compiled
   targets (where valgrind may not be available)
3. **`test-jit_stream_flash`** — C-level tests for flash-based code storage
4. **BEAM-based tests** — runs all test modules using the BEAM VM with real
   binutils for instruction validation

---

## 9. Best Practices

### 9.1 Start with the assembler

Write and fully test `jit_myarch_asm.erl` before starting on the backend.
This gives you a solid foundation and catches encoding bugs early. Use binutils
as the ground truth.

### 9.2 Use binutils as the oracle

The `jit_tests_common:asm/3` function is your most powerful testing tool.
It assembles a text string with a real assembler and compares the result
byte-for-byte with your Erlang-generated binary. If they match, your encoding
is correct.

When a test fails, the `diff_disasm/3` function automatically:
1. Writes both expected and actual bytes to temporary files
2. Runs objdump on both
3. Strips comments/annotations
4. Shows a `diff -u` of the disassembly

This makes debugging encoding errors straightforward — you can see exactly
which instruction differs and how.

### 9.3 Use objdump dumps in backend tests

Backend tests use `jit_tests_common:assert_stream/3` with inline objdump
output. This serves dual purpose:
- It verifies the generated code is correct
- The disassembly text in the test acts as documentation of what the backend
  generates for each operation

To create a dump for a new test:
1. Generate the code with your backend
2. Write it to a file: `file:write_file("dump.bin", Stream)`
3. Disassemble: `myarch-objdump -b binary <flags> -D dump.bin`
4. Copy the relevant lines into your test

### 9.4 Complete coverage of the assembler module

Every exported function in the assembler module should have at least one test.
For instructions with variable encoding (compressed vs full-width), test both
paths. For instructions with immediate fields, test:
- Zero values
- Maximum positive values
- Maximum negative values (for signed fields)
- Boundary values that switch between encoding formats

### 9.5 Test calling convention edge cases

The most common bugs in JIT backends involve:
- **Wrong stack alignment**: Some ABIs require 16-byte aligned stacks at call
  sites. On RV64 with LP64, the stack must be 16-byte aligned at function
  calls. When saving 4 registers × 8 bytes = 32 bytes, this is naturally
  aligned, but saving 5 registers (40 bytes) would need padding to 48.
- **Wrong stack frame size**: When porting from 32-bit to 64-bit, the stack
  frame size doubles for pointer-sized saves: `addi sp,sp,-16` (4 regs × 4B)
  becomes `addi sp,sp,-32` (4 regs × 8B). Similarly the compressed stack
  load/store instructions change: `c.lwsp`/`c.swsp` → `c.ldsp`/`c.sdsp`.
- **Clobbered registers**: Forgetting to save a register that the ABI says
  is caller-saved (or vice versa)
- **Wrong immediate encoding**: Sign extension, field width, shifted immediates
- **Branch range**: Forward branches to labels not yet defined, very long
  branches that exceed the immediate field range
- **Primitive table overflow**: On 64-bit architectures, the primitive function
  pointer table uses 8-byte entries, which means high-numbered primitives
  (index ≥ 256 on RV64) exceed the 12-bit signed immediate range. The
  backend must detect this and emit an indirect load sequence.

### 9.6 Implement compressed instructions

If your ISA supports variable-length instructions (RISC-V C extension,
ARM Thumb, x86 short forms), implement the compressed variants. The assembler
should automatically select the shortest encoding when the operands fit. This
is not just an optimization — it affects code size significantly on embedded
platforms.

> **Warning — RV32C vs RV64C**: The compressed extension is **not identical**
> between RV32 and RV64. Notably, `c.jal` (opcode 01, funct3=001) exists on
> RV32C but **does not exist on RV64C** — that opcode is `c.addiw` instead.
> Similarly, `c.lw`/`c.sw` work on both but `c.ld`/`c.sd` are RV64C-only
> (replacing RV32C's `c.flw`/`c.fsw`). When porting compressed instructions,
> carefully check the ISA spec for each word size.

### 9.7 Static assertions are safety nets

The `_Static_assert` statements in `jit.c` are critical. They catch ABI
mismatches at compile time. When porting to a new architecture, the struct
layout may differ due to alignment rules. Always verify offsets experimentally
and encode them as assertions.

### 9.8 Register tracking with `jit_regs`

Use `jit_regs:set_contents/3` and `jit_regs:get_contents/2` to track what
each register contains. Use `jit_regs:find_reg_with_contents/2` before loading
a value — if a register already contains the needed value, skip the load.
Invalidate the tracking state at:
- Labels (any code path could reach a label)
- Function calls (caller-saved registers are clobbered)
- Branches (the target has unknown register state)

### 9.9 Development workflow

1. **Assembler first**: Implement and test individual instructions
2. **Simple backend functions**: Start with `new/3`, `stream/1`, `word_size/0`,
   `jump_table/2`
3. **Call primitives**: Implement `call_primitive/3` — this is the most complex
   function and exercises the calling convention
4. **Data movement**: `move_to_native_register/2`, `move_to_vm_register/3`
5. **Control flow**: `jump_to_label/2`, `cond_jump_to_label/3`
6. **Arithmetic/logic**: `add/3`, `sub/3`, `and_/3`, etc.
7. **Full integration**: Run the complete JIT test suite on real hardware or
   under QEMU
8. **DWARF support**: Add debug info generation last — include the
   `jit_backend_dwarf_impl.hrl` header and add a `backend_to_machine_type/1`
   clause for your backend in `jit_dwarf.erl`, plus define the
   `DWARF_*_REG_MYARCH` macro in `jit_dwarf.hrl`

---

## 10. Verifying Test Coverage

After writing tests, verify that your test suite actually covers all exported
functions and all encoding paths.

### 10.1 Assembler coverage

Check that every exported function in the assembler module has at least one
test by comparing the export list against the test function names:

```bash
# List all exported functions from the assembler module:
grep -oP '^\s+\K\w+/\d+' libs/jit/src/jit_myarch_asm.erl | head -100

# Count exports vs tests:
echo "Exports:"
grep -c '^\s\+[a-z_]*/[0-9]' libs/jit/src/jit_myarch_asm.erl
echo "Test functions:"
grep -c '_test' tests/libs/jit/jit_myarch_asm_tests.erl
```

Run the tests and check for 100% pass rate:

```bash
erl -noshell -pa /tmp/jit_test \
    -eval 'eunit:test(jit_myarch_asm_tests, [verbose]), halt().'
```

For the riscv64 backend, the assembler has 95 exported functions covered by
155 tests (multiple encodings per instruction — compressed, full-width,
different register classes, edge-case immediates).

### 10.2 Backend coverage

Compare your backend test suite against the reference (e.g., riscv32 or
aarch64):

```bash
# Count test functions in the reference backend:
grep -c '_test\b' tests/libs/jit/jit_riscv32_tests.erl

# Count test functions in your backend:
grep -c '_test\b' tests/libs/jit/jit_myarch_tests.erl
```

The riscv32 backend has ~48 test functions. A new backend derived from an
existing one should have the same count — each test exercises a specific
code generation pattern that should be validated independently.

### 10.3 Identifying untested operations

Systematically check which backend exports lack test coverage:

```erlang
%% In the Erlang shell:
Exports = jit_myarch:module_info(exports),
TestFuns = jit_myarch_tests:module_info(exports),
TestNames = [atom_to_list(F) || {F, _} <- TestFuns],
[{F, A} || {F, A} <- Exports,
    not lists:any(fun(T) ->
        string:find(T, atom_to_list(F)) =/= nomatch
    end, TestNames)].
```

---

## 11. Porting Between Word Sizes (32-bit ↔ 64-bit)

When creating a 64-bit backend from an existing 32-bit backend (or vice
versa), there are systematic differences that affect every layer.

### 11.1 Assembler differences

If both word sizes share the same base ISA (e.g., RV32I and RV64I), the
64-bit assembler can **delegate most instructions** to the 32-bit module
and only override what differs:

```erlang
%% jit_riscv64_asm.erl — delegate unchanged instructions:
add(Rd, Rs1, Rs2) -> jit_riscv32_asm:add(Rd, Rs1, Rs2).
sub(Rd, Rs1, Rs2) -> jit_riscv32_asm:sub(Rd, Rs1, Rs2).

%% Override word-sized operations:
ld(Rd, Rs1, Offset) -> ...    % was lw on RV32
sd(Rs2, Rs1, Offset) -> ...   % was sw on RV32
```

**Key instruction differences (RV32 → RV64 example)**:

| Operation | RV32 | RV64 |
|---|---|---|
| Word-size load | `lw` | `ld` |
| Word-size store | `sw` | `sd` |
| Compressed word load | `c.lw` | `c.ld` |
| Compressed word store | `c.sw` | `c.sd` |
| Compressed stack load | `c.lwsp` | `c.ldsp` |
| Compressed stack store | `c.swsp` | `c.sdsp` |
| Shift amount bits | 5 (0–31) | 6 (0–63) |
| `c.jal` (compact jump-and-link) | exists (RV32C) | **does not exist** (opcode is `c.addiw`) |
| `addiw`, `slliw`, etc. | not available | new W-suffix instructions |

### 11.2 Backend differences

The backend module has more pervasive changes:

**Context struct offsets**: Every field offset changes because pointers and
terms are wider. For example, `x_reg(0)` might be at offset `0x2C` on
32-bit but `0x58` on 64-bit. All `?X_REG(N)`, `?Y_REGS`, `?CP`, etc.
macros must be recalculated:

```erlang
%% 32-bit: term size = 4, pointer size = 4
-define(X_REG(N), {?CTX_REG, 16#2C + (N * 4)}).
-define(CP, {?CTX_REG, 16#70}).

%% 64-bit: term size = 8, pointer size = 8
-define(X_REG(N), {?CTX_REG, 16#58 + (N * 8)}).
-define(CP, {?CTX_REG, 16#E0}).
```

**Primitive table indexing**: The `ModuleNativeInterface` is a table of
function pointers. On 64-bit, each pointer is 8 bytes, so primitive index
`N` is at offset `N * 8`. When the offset exceeds the immediate field range
(e.g., 12-bit signed for RISC-V ld/sd, range ±2048), the backend must use
an indirect sequence:

```erlang
%% RV32: offset 4 fits in 12-bit immediate for lw
%%   lw t6, 4(a2)
%%
%% RV64: offset 8 fits for ld — but at primitive index 256+,
%% offset 2048+ does NOT fit. Need:
%%   li t6, <offset>
%%   add t6, t6, a2
%%   ld t6, 0(t6)
```

The riscv64 backend hits this at primitive index ≥ 256 (offset ≥ 2048).
The riscv32 backend doesn't hit it until index ≥ 512 because each pointer
is only 4 bytes. The threshold also changes for JIT state field offsets
(since JIT state contains both 32-bit integers and pointers — the latter
being 8 bytes on 64-bit).

**Stack frame sizes**: On 64-bit, each saved register occupies 8 bytes
instead of 4. A frame saving `{ra, a0, a1, a2}` is 32 bytes on RV64
vs 16 bytes on RV32. Use `sd`/`ld` instead of `sw`/`lw` for all
pointer/term-sized stack operations (though `remaining_reductions` stays
32-bit and uses `lw`/`sw`).

**Term encoding**: `?TERM_NIL`, atom tags, and integer tags use the same
bit patterns on both 32-bit and 64-bit, but immediate integer range is
wider on 64-bit. The `?TERM_IMMED2_TAG_SIZE` is 6 on both. The macros
in `term.hrl` and `default_atoms.hrl` are word-size-independent.

### 11.3 Test differences

**Dump strings change completely**: Even if the test logic (setup code) is
identical, the expected disassembly differs because:
- `lw`/`sw` → `ld`/`sd` for term-sized operations
- `c.lwsp`/`c.swsp` → `c.ldsp`/`c.sdsp` for stack operations
- Stack frame sizes change (e.g., `addi sp,sp,-16` → `addi sp,sp,-32`)
- Field offsets differ throughout
- Primitive table offsets may require `li+add+ld` sequences instead of
  direct `ld reg,offset(a2)`

The recommended approach for porting: write a dump generator script (see
Section 7.4) that runs the same test logic with the new backend and captures
the actual disassembly. Then copy the dumps into the test file.

---

## 12. Checklist

### Architecture registration

- [ ] `libs/jit/include/jit.hrl` — `?JIT_ARCH_MYARCH` constant
- [ ] `src/libAtomVM/jit.h` — `JIT_ARCH_MYARCH`, `JIT_ARCH_TARGET`, `JIT_JUMPTABLE_ENTRY_SIZE`
- [ ] `libs/jit/src/jit_precompile.erl` — target name → arch constant mapping
- [ ] `src/libAtomVM/nifs.c` — `nif_jit_backend_module()` case
- [ ] `src/libAtomVM/defaultatoms.def` + `defaultatoms.h` — `JIT_MYARCH_ATOM`
- [ ] `CMakeLists.txt` — auto-detection and `AVM_PRECOMPILED_TARGETS`

### Assembler module

- [ ] `libs/jit/src/jit_myarch_asm.erl` — all instructions implemented
- [ ] Register type defined and exported
- [ ] ISA reference documented in module header
- [ ] All instruction encoding formats implemented
- [ ] Compressed/short instructions (if applicable)

### Backend module

- [ ] `libs/jit/src/jit_myarch.erl` — all required exports implemented
- [ ] Correct struct offsets (`Context`, `JITState`) defined
- [ ] Calling convention documented
- [ ] Jump table implementation
- [ ] Branch patching (`update_branches/1`)
- [ ] Register allocation bitmask helpers
- [ ] DWARF integration (via `jit_backend_dwarf_impl.hrl`)
- [ ] `libs/jit/src/jit_dwarf.erl` — `backend_to_machine_type/1` clause added
- [ ] `libs/jit/src/jit_dwarf.hrl` — `DWARF_*_REG_MYARCH` context register macro defined

### C-side integration

- [ ] `src/libAtomVM/jit.c` — `_Static_assert` for all struct offsets
- [ ] `src/libAtomVM/jit.h` — compiler macro detection for the architecture

### Build system

- [ ] `libs/jit/src/CMakeLists.txt` — new modules added
- [ ] `tests/libs/jit/CMakeLists.txt` — new test modules added

### Tests

- [ ] `tests/libs/jit/jit_myarch_asm_tests.erl` — every instruction tested
- [ ] `tests/libs/jit/jit_myarch_tests.erl` — backend operations tested
- [ ] `tests/libs/jit/jit_tests_common.erl` — binutils support for the arch
- [ ] `tests/libs/jit/tests.erl` — new modules registered
- [ ] All tests pass with BEAM + binutils
- [ ] All tests pass with AtomVM on target hardware (or QEMU)
- [ ] Valgrind clean (no memory errors in generated code)

### CI pipeline

- [ ] `.github/workflows/build-and-test.yaml` — JIT matrix entry
- [ ] `.github/workflows/build-and-test.yaml` — JIT + DWARF matrix entry
- [ ] Cross-compilation setup (if needed): toolchain file, QEMU, sysroot
- [ ] Cache key includes the new `jit_target_arch` (automatic)

---

## Appendix A — File Reference

| File | Purpose |
|------|---------|
| `libs/jit/include/jit.hrl` | Architecture constants, format version, variant flags |
| `libs/jit/src/jit.erl` | Shared frontend — BEAM opcode → backend calls |
| `libs/jit/src/jit_precompile.erl` | BEAM file parser, compilation orchestrator |
| `libs/jit/src/jit_regs.erl` | Register contents and type tracking |
| `libs/jit/src/jit_dwarf.erl` | DWARF debug info stream wrapper |
| `libs/jit/src/jit_backend_dwarf_impl.hrl` | DWARF macro implementations for backends |
| `libs/jit/src/jit_stream_binary.erl` | Binary stream (for precompilation) |
| `libs/jit/src/jit_stream_mmap.erl` | Memory-mapped stream (for runtime JIT) |
| `libs/jit/src/jit_stream_flash.erl` | Flash memory stream (for MCUs) |
| `libs/jit/src/primitives.hrl` | Native function indices (`?PRIM_*`) |
| `libs/jit/src/term.hrl` | Term encoding constants (tags, masks) |
| `libs/jit/src/default_atoms.hrl` | Built-in atom indices |
| `src/libAtomVM/jit.h` | C-side JIT types, `ModuleNativeInterface`, constants |
| `src/libAtomVM/jit.c` | C-side runtime (primitives, offset assertions, ELF) |
| `src/libAtomVM/nifs.c` | NIFs for `backend_module/0`, `variant/0` |
| `tests/libs/jit/jit_tests_common.erl` | Shared test helpers (binutils, dump parsing) |
| `.github/workflows/build-and-test.yaml` | Main CI workflow with JIT matrix |
| `.github/workflows/run-tests-with-beam.yaml` | BEAM-based test workflow |

## Appendix B — Existing Backends Reference

| Architecture | Word Size | Jump Table Entry | Compressed? | CI Runner |
|---|---|---|---|---|
| x86_64 | 8 bytes | 5 bytes (jmp rel32) | N/A (CISC) | Native x86_64 |
| aarch64 | 8 bytes | 4 bytes (b offset) | No | Native arm64 |
| armv6m | 4 bytes | 12 bytes (ldr+push+add) | Thumb-only | Cross + QEMU |
| riscv32 | 4 bytes | 8 bytes (auipc+jalr) | RV32C | Cross + QEMU |
| riscv64 | 8 bytes | 8 bytes (auipc+jalr) | RV64C | Cross + QEMU |
