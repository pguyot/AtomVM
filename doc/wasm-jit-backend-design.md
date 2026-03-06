<!--
 Copyright 2026 Paul Guyot <pguyot@kallisys.net>

 SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
-->

# WASM JIT Backend — Architectural Analysis

This document analyzes the architectural changes required to add a WebAssembly
(WASM) JIT backend to AtomVM. The WASM target is fundamentally different from
all existing backends (x86_64, aarch64, armv6m, riscv32, riscv64) because WASM
is a structured, typed, stack-based virtual machine rather than a flat-memory
register machine with arbitrary jumps.

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Key Architectural Incompatibilities](#2-key-architectural-incompatibilities)
3. [Required Changes to `jit.h`](#3-required-changes-to-jith)
4. [Required Changes to `jit.c`](#4-required-changes-to-jitc)
5. [Required Changes to `module.c` / `module.h`](#5-required-changes-to-modulec--moduleh)
6. [Required Changes to `opcodesswitch.h`](#6-required-changes-to-opcodesSwitchh)
7. [Erlang-Side Backend Design](#7-erlang-side-backend-design)
8. [Two Possible Strategies](#8-two-possible-strategies)
9. [Recommended Approach: Per-label Function Table](#9-recommended-approach-per-label-function-table)
10. [Detailed `#ifdef` Plan](#10-detailed-ifdef-plan)
11. [Open Questions](#11-open-questions)

---

## 1. Executive Summary

The current JIT architecture relies on three properties that WASM does not
support:

1. **Computed jumps / arbitrary address arithmetic**: The jump table mechanism
   computes entry points as `native_code_base + label * entry_size` and casts
   the result to a function pointer (`ModuleNativeEntryPoint`). WASM has no raw
   pointers to code and no way to jump to a computed byte offset within a
   function.

2. **Flat executable memory**: Native backends emit a single contiguous code
   blob (jump table + instruction stream). Code addresses are raw pointers that
   can be stored, compared, and called. In WASM, functions are referenced by
   indices into a typed function table, not by memory addresses.

3. **Direct struct field access via hardcoded offsets**: All backends load/store
   `Context` and `JITState` fields using immediate offsets from pointer
   registers (e.g., `ld t0, 0x58(a0)` to read `ctx->x`). WASM's linear memory
   can support this pattern but requires the host to place C structs in linear
   memory and pass base pointers, which is the standard approach with
   Emscripten.

The core change is that **the jump table must become a function index table**
rather than a byte-offset table, and `ModuleNativeEntryPoint` needs a WASM-
compatible representation.

---

## 2. Key Architectural Incompatibilities

### 2.1 The Jump Table

Current design (`module.c:783-787`):

```c
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label) {
    assert(module->native_code);
    return (ModuleNativeEntryPoint)(
        ((const uint8_t *) module->native_code) + JIT_JUMPTABLE_ENTRY_SIZE * exported_label);
}
```

This does pointer arithmetic on executable code. Each jump table entry is a
fixed-size native instruction (5 bytes on x86_64, 4 bytes on aarch64, etc.)
that branches to the actual code for that label.

**WASM problem**: WASM has no concept of "jump to byte offset N in a code
blob." Functions in WASM are discrete units identified by index. You cannot
synthesize a function pointer from arithmetic on another function's address.

### 2.2 The Entry Point Signature

Current signature (`jit.h:71`):

```c
typedef Context *(*ModuleNativeEntryPoint)(
    Context *ctx, JITState *jit_state, const ModuleNativeInterface *p);
```

This is a C function pointer. In WASM/Emscripten, function pointers work via
`WebAssembly.Table` — they are indices into a typed indirect function table.
The signature itself is fine for WASM (it uses `i32` or `i64` params depending
on wasm32/wasm64), but:

- You cannot **manufacture** a function pointer from arithmetic. Each function
  that should be callable must be explicitly registered in the function table.
- The `continuation` field in `JITState` stores a `ModuleNativeEntryPoint`
  that is computed from byte offsets in `jit_return()`. This must change.

### 2.3 The Continuation Mechanism

In `jit.c:314-338`, `jit_return()` computes the continuation:

```c
const void *native_pc = ((const uint8_t *) mod->native_code)
    + ((ctx->cp & 0xFFFFFF) >> 2);
jit_state->continuation = (ModuleNativeEntryPoint) native_pc;
```

This manufactures a callable function pointer from arithmetic — impossible in
WASM.

### 2.4 Intra-Module Branches

Within a module, the backends emit direct branches (`jmp`, `b`, `beq`, etc.)
to other labels. WASM has structured control flow (`block`, `loop`, `br`,
`br_table`) — there is no `goto`. Branches can only target enclosing blocks,
not arbitrary instruction addresses.

---

## 3. Required Changes to `jit.h`

### 3.1 Architecture Registration

```c
#define JIT_ARCH_WASM32 6

#ifndef AVM_NO_JIT
// ... existing arch detection ...

#ifdef __EMSCRIPTEN__
#define JIT_ARCH_TARGET JIT_ARCH_WASM32
// Jump table entry is a function table index (4 bytes for wasm32)
#define JIT_JUMPTABLE_ENTRY_SIZE 4
#endif
```

### 3.2 Entry Point Type Divergence

The fundamental question is whether `ModuleNativeEntryPoint` remains a
function pointer or becomes an index. Two options:

**Option A — Keep as function pointer (Emscripten approach)**:
Emscripten supports C function pointers via `WebAssembly.Table`. Each
WASM-compiled label function gets a slot in the table. The function pointer
is actually a table index cast to a pointer. This works if each label is
compiled to a separate WASM function and added to the table.

```c
// No change to ModuleNativeEntryPoint typedef
// But module_get_native_entry_point must change
```

**Option B — Abstract entry point type**:

```c
#ifdef __EMSCRIPTEN__
typedef uint32_t ModuleNativeEntryPoint;  // table index
#else
typedef Context *(*ModuleNativeEntryPointFn)(Context *, JITState *, const ModuleNativeInterface *);
typedef ModuleNativeEntryPointFn ModuleNativeEntryPoint;
#endif
```

Option A is strongly preferred because it minimizes `#ifdef` proliferation.
Emscripten's function pointer mechanism handles the indirection automatically.

### 3.3 JITState Changes

The `continuation` union already supports both a function pointer and a raw
PC. For WASM, the function pointer path works if we generate one WASM function
per label:

```c
struct JITState {
    Module *module;
    union {
        ModuleNativeEntryPoint continuation;
        const void *continuation_pc;       // only for emulated fallback
#ifdef __EMSCRIPTEN__
        uint32_t continuation_index;       // optional: explicit table index
#endif
    };
    int remaining_reductions;
};
```

### 3.4 Jump Table Size

For WASM, the "jump table" would be an array of function table indices (4 bytes
each for wasm32), not executable instructions. So `JIT_JUMPTABLE_ENTRY_SIZE`
would be 4, representing the size of one `uint32_t` index. However, the
interpretation changes: instead of the jump table containing *executable code*
at each entry, it contains *data* (function indices).

This requires `module_get_native_entry_point` to work differently: instead of
casting an address within the jump table to a callable, it reads an index from
the table and looks up the function.

```c
// New ifdef in module.c
#ifdef __EMSCRIPTEN__
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label) {
    assert(module->native_code);
    // The "native_code" pointer points to an array of function table indices
    const uint32_t *table = (const uint32_t *) module->native_code;
    uint32_t func_index = table[exported_label];
    // Convert table index back to function pointer via Emscripten's mechanism
    return (ModuleNativeEntryPoint) func_index;
}
#else
// ... existing implementation ...
#endif
```

---

## 4. Required Changes to `jit.c`

### 4.1 Static Assert Block

A new `#elif` block for the WASM target:

```c
#elif JIT_ARCH_TARGET == JIT_ARCH_WASM32
// Emscripten wasm32 uses 32-bit pointers
_Static_assert(offsetof(Context, e) == 0x28, "ctx->e is 0x28 in jit/src/jit_wasm32.erl");
_Static_assert(offsetof(Context, x) == 0x2C, "ctx->x is 0x2C in jit/src/jit_wasm32.erl");
// ... etc (same as armv6m / riscv32 offsets since wasm32 has 32-bit pointers)

_Static_assert(offsetof(JITState, module) == 0x0, "...");
_Static_assert(offsetof(JITState, continuation) == 0x4, "...");
_Static_assert(offsetof(JITState, remaining_reductions) == 0x8, "...");
_Static_assert(sizeof(size_t) == 4, "size_t is expected to be 32 bits");
```

### 4.2 jit_return() Changes

The `jit_return()` function computes `continuation` from byte offset
arithmetic. For WASM, this must resolve to a valid function table entry
instead:

```c
static Context *jit_return(Context *ctx, JITState *jit_state) {
    int module_index = ctx->cp >> 24;
    Module *mod = globalcontext_get_module_by_index(ctx->global, module_index);

#ifndef AVM_NO_EMU
    if (mod->native_code == NULL) {
        const uint8_t *code = mod->code->code;
        const uint8_t *pc = code + ((ctx->cp & 0xFFFFFF) >> 2);
        jit_state->continuation = pc;
    } else {
#endif

#ifdef __EMSCRIPTEN__
        // For WASM: the cp encodes a label number, not a byte offset
        // Resolve it through the function index table
        int label = (ctx->cp & 0xFFFFFF) >> 2;
        jit_state->continuation = module_get_native_entry_point(mod, label);
#else
        // Existing: raw pointer arithmetic
        const void *native_pc = ((const uint8_t *) mod->native_code)
            + ((ctx->cp & 0xFFFFFF) >> 2);
        jit_state->continuation = (ModuleNativeEntryPoint) native_pc;
#endif

#ifndef AVM_NO_EMU
    }
#endif
    jit_state->module = mod;
    return ctx;
}
```

**Important**: This changes how `ctx->cp` is encoded for WASM. Currently,
the lower 24 bits store a byte offset (divided by 4). For WASM, they would
store a label number instead, since byte offsets into code are meaningless.
The `set_continuation_to_label` backend function must encode the label number
directly.

### 4.3 ModuleNativeInterface — No Changes Needed

The `ModuleNativeInterface` function pointer table works fine with WASM.
Emscripten handles C function pointers through its indirect function table.
The JIT code would call primitives the same way — loading a function pointer
from the struct and calling it indirectly.

### 4.4 DWARF Support

DWARF debug info is not applicable to WASM. The WASM backend should disable
DWARF:

```c
#ifdef __EMSCRIPTEN__
// DWARF not supported for WASM target
#define AVM_NO_JIT_DWARF
#endif
```

---

## 5. Required Changes to `module.c` / `module.h`

### 5.1 `module_get_native_entry_point`

This function must be `#ifdef`'d to handle the WASM case where the
native_code blob is a table of function indices rather than executable
instructions:

```c
#ifndef AVM_NO_JIT
#ifdef __EMSCRIPTEN__
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label) {
    assert(module->native_code);
    // For WASM, native_code points to an array of function pointers
    // registered in WebAssembly.Table
    const ModuleNativeEntryPoint *func_table = (const ModuleNativeEntryPoint *) module->native_code;
    return func_table[exported_label];
}
#else
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label) {
    assert(module->native_code);
    return (ModuleNativeEntryPoint)(
        ((const uint8_t *) module->native_code) + JIT_JUMPTABLE_ENTRY_SIZE * exported_label);
}
#endif
#endif
```

### 5.2 `module_set_native_code`

For WASM, setting native code would install the function table rather than
an executable blob. The signature can stay the same but the semantics change:
the `entry_point` pointer points to an array of `ModuleNativeEntryPoint`
values (function table indices).

---

## 6. Required Changes to `opcodesswitch.h`

The execution loop at lines 2063-2115 calls JIT code and loops on
`jit_state.continuation`. This loop works for WASM as-is, because:

- `native_pc(ctx, &jit_state, &module_native_interface)` — Emscripten handles
  indirect function calls through `WebAssembly.Table`.
- `jit_state.continuation` is set by primitives and checked in the loop.

No `#ifdef` changes needed in the execution loop itself, provided that
`ModuleNativeEntryPoint` remains a C function pointer type.

---

## 7. Erlang-Side Backend Design

### 7.1 Code Generation Strategy

Unlike native backends that emit raw machine instructions, the WASM backend
would emit WASM bytecode. Two sub-strategies:

**Strategy A — Emit WASM binary format directly**:
Generate `.wasm` module binary from the Erlang backend. The assembler module
(`jit_wasm32_asm.erl`) would encode WASM instructions directly. The resulting
binary is loaded at runtime via `WebAssembly.compile()` /
`WebAssembly.instantiate()` from JavaScript or Emscripten's dynamic linking.

**Strategy B — Emit C-like IR and compile via Emscripten**:
This is far too slow for JIT and is not practical.

Strategy A is the only viable option for runtime JIT.

### 7.2 One Function Per Label

Since WASM has no arbitrary jumps, each BEAM label should compile to a
separate WASM function. Intra-module control flow that crosses labels
(e.g., a conditional branch from label 5 to label 12) must be restructured:

- **Forward branches within the same BEAM opcode sequence**: Use WASM
  structured control flow (`block`/`br`).
- **Branches to other labels**: Return to the dispatch loop with the target
  label as the continuation, and let the C-side loop call the target function.
  This is the same pattern already used for inter-module calls.

This is a significant departure from native backends where all labels exist
in a single code blob and branches are direct jumps.

### 7.3 Backend Module Structure

```erlang
-module(jit_wasm32).

%% Same exports as other backends
-export([
    word_size/0,          %% Returns 4
    new/3,
    stream/1,
    offset/1,
    flush/1,
    jump_table/2,
    update_branches/1,
    call_primitive/3,
    %% ... all other required exports ...
]).
```

Key differences from native backends:

- `jump_table/2`: Instead of emitting jump instructions, emit a data section
  containing function table indices (to be filled during `update_branches/1`).
- `add_label/2`: Start a new WASM function for each label.
- `cond_jump_to_label/3`: Instead of emitting a conditional branch instruction,
  emit code that sets the continuation and returns to the dispatch loop.
  Alternatively, if the target label is "close" (within the same structured
  block), emit a `br` to an enclosing block.
- `call_primitive/3`: Emit a WASM `call_indirect` instruction to call through
  the `ModuleNativeInterface` table.

### 7.4 Assembler Module

```erlang
-module(jit_wasm32_asm).

%% WASM instruction encoding
-export([
    i32_const/1,
    i32_load/2,
    i32_store/2,
    i32_add/0,
    i32_sub/0,
    i32_and/0,
    i32_or/0,
    i32_shr_u/0,
    i32_shl/0,
    call_indirect/2,
    local_get/1,
    local_set/1,
    block/1,
    loop/1,
    br/1,
    br_if/1,
    br_table/2,
    end_block/0,
    return/0,
    %% ...
]).
```

### 7.5 Stream Module

A new stream module `jit_stream_wasm` would be needed. Instead of writing
to mmap'd executable memory, it would:

1. Accumulate WASM bytecode for each function
2. Package all functions into a WASM module binary
3. Use Emscripten's dynamic instantiation API to load the module
4. Extract function table indices for each label

---

## 8. Two Possible Strategies

### Strategy 1: One WASM Function Per Label (Trampoline Model)

Each BEAM label compiles to a separate WASM function with the standard
`ModuleNativeEntryPoint` signature. All cross-label transitions go through
the C dispatch loop.

**Pros**:
- Minimal changes to `jit.c` and `opcodesswitch.h`
- Clean separation, easy to debug
- The C dispatch loop already handles the continuation pattern

**Cons**:
- Every label transition costs a WASM→C→WASM roundtrip
- Poor performance for tight loops that span multiple labels
- Each function call goes through `WebAssembly.Table` indirection

### Strategy 2: One WASM Function Per Module with `br_table`

Compile the entire module into a single WASM function that uses a
`br_table` (computed branch) to dispatch to the right label within a
large `block`/`loop` structure. This is analogous to a switch statement.

**Pros**:
- Intra-module branches stay within WASM, avoiding C roundtrips
- Better optimization opportunities for the WASM engine
- More similar to the native backend model (single code blob)

**Cons**:
- Requires restructuring control flow into WASM's structured format
  (the "relooper" or "stackifier" problem)
- Forward and backward branches to arbitrary labels require careful
  nesting of `block` and `loop` constructs
- Generated code may be large and hard to debug
- Complex to implement in the Erlang backend

### Recommendation

**Strategy 2 (single function with `br_table`)** is strongly recommended for
performance despite its complexity. The dispatch pattern would be:

```wasm
(func $module_entry (param $ctx i32) (param $jit_state i32) (param $iface i32)
                     (result i32)
  (local $label i32)
  ;; initial label from entry point
  (local.set $label (i32.const <initial_label>))
  (block $exit
    (loop $dispatch
      (block $label_0
        (block $label_1
          (block $label_2
            ;; ... nested blocks for each label ...
            (br_table $label_0 $label_1 $label_2 ... $exit
              (local.get $label))
          ) ;; end label_2
          ;; Code for label 2
          ;; To branch to label 0: (local.set $label (i32.const 0)) (br $dispatch)
          ;; To return to C: just (return)
        ) ;; end label_1
        ;; Code for label 1
        ...
      ) ;; end label_0
      ;; Code for label 0
      ...
    ) ;; end dispatch loop
  ) ;; end exit
)
```

However, the module would need **multiple entry points**. Since WASM functions
have a single entry, the function must accept the target label as a parameter
(or encode it in `JITState`). This requires a **different entry point
signature** for WASM.

---

## 9. Recommended Approach: Per-label Function Table

After analyzing both strategies, a **hybrid approach** is most practical for
an initial implementation:

1. **Each label gets its own WASM function** (Strategy 1 for simplicity)
2. **Consecutive opcodes within a label** use WASM structured control flow
3. **All label functions are bundled into a single WASM module** for
   efficient instantiation
4. The "jump table" is an array of function table indices

This can be evolved to Strategy 2 later as an optimization.

### Entry Point Signature

The existing `ModuleNativeEntryPoint` signature works for WASM via Emscripten's
`WebAssembly.Table` mechanism. No signature change is needed.

However, if Strategy 2 is pursued, the signature must change to include the
target label:

```c
#ifdef __EMSCRIPTEN__
typedef Context *(*ModuleNativeEntryPoint)(
    Context *ctx, JITState *jit_state, const ModuleNativeInterface *p,
    uint32_t label);
#else
typedef Context *(*ModuleNativeEntryPoint)(
    Context *ctx, JITState *jit_state, const ModuleNativeInterface *p);
#endif
```

**This is the most significant `#ifdef` decision.** Adding a label parameter
to the entry point allows a single WASM function to serve all labels, avoiding
the overhead of hundreds of table entries per module. But it changes the
calling convention everywhere.

### Recommendation

For the initial implementation, **keep the existing signature and use
Strategy 1** (one function per label). The overhead of table dispatch is
acceptable and the implementation is far simpler. Strategy 2 can be
explored later if performance requires it.

---

## 10. Detailed `#ifdef` Plan

### `jit.h` — Required `#ifdef` additions

```c
// After existing arch definitions:
#define JIT_ARCH_WASM32 6

#ifndef AVM_NO_JIT
// ... existing arch blocks ...

#ifdef __EMSCRIPTEN__
#define JIT_ARCH_TARGET JIT_ARCH_WASM32
#define JIT_JUMPTABLE_ENTRY_SIZE 4  // sizeof(uint32_t) function table index
// WASM jump table entries are data (function indices), not executable code
#define JIT_JUMPTABLE_IS_DATA 1
#endif

#ifndef JIT_ARCH_TARGET
#error Unknown JIT target
#endif
#endif
```

The `JIT_JUMPTABLE_IS_DATA` flag distinguishes the WASM case where the jump
table contains data to be read (function indices) rather than code to be
executed (jump instructions).

### `module.c` — Required `#ifdef` additions

```c
#ifndef AVM_NO_JIT
ModuleNativeEntryPoint module_get_native_entry_point(Module *module, int exported_label) {
    assert(module->native_code);
#ifdef JIT_JUMPTABLE_IS_DATA
    // WASM: jump table is an array of function pointers
    const ModuleNativeEntryPoint *func_table = (const ModuleNativeEntryPoint *) module->native_code;
    return func_table[exported_label];
#else
    // Native: jump table is executable code at fixed-size entries
    return (ModuleNativeEntryPoint)(
        ((const uint8_t *) module->native_code) + JIT_JUMPTABLE_ENTRY_SIZE * exported_label);
#endif
}
#endif
```

### `jit.c` — Required `#ifdef` additions

1. **New offset assertion block** for `JIT_ARCH_WASM32`
2. **`jit_return()` change**: Use `module_get_native_entry_point()` instead of
   direct pointer arithmetic when `JIT_JUMPTABLE_IS_DATA` is defined

```c
static Context *jit_return(Context *ctx, JITState *jit_state) {
    int module_index = ctx->cp >> 24;
    Module *mod = globalcontext_get_module_by_index(ctx->global, module_index);

#ifndef AVM_NO_EMU
    if (mod->native_code == NULL) {
        const uint8_t *code = mod->code->code;
        const uint8_t *pc = code + ((ctx->cp & 0xFFFFFF) >> 2);
        jit_state->continuation = pc;
    } else {
#endif

#ifdef JIT_JUMPTABLE_IS_DATA
        // cp encodes the label index for data-based jump tables
        int label = (ctx->cp & 0xFFFFFF) >> 2;
        jit_state->continuation = module_get_native_entry_point(mod, label);
#else
        const void *native_pc = ((const uint8_t *) mod->native_code)
            + ((ctx->cp & 0xFFFFFF) >> 2);
        jit_state->continuation = (ModuleNativeEntryPoint) native_pc;
#endif

#ifndef AVM_NO_EMU
    }
#endif
    jit_state->module = mod;
    return ctx;
}
```

The same pattern applies in `jit_handle_error()` where
`module_get_native_entry_point()` is already used (line 373), so that path
needs no change.

### `opcodesswitch.h` — No `#ifdef` changes needed

The execution loop works as-is because it calls through the
`ModuleNativeEntryPoint` function pointer, which Emscripten handles via
`WebAssembly.Table`.

### `jit.erl` — No changes needed

The frontend is architecture-independent. The backend module (`jit_wasm32.erl`)
handles all WASM-specific code generation.

---

## 11. Open Questions

1. **Dynamic WASM module instantiation**: How does the runtime load a
   dynamically-generated WASM module? Emscripten's `dlopen()` emulation
   or direct use of `WebAssembly.instantiate()` via JavaScript?

2. **`ctx->cp` encoding for WASM**: The current encoding packs a byte offset
   in the lower 24 bits. For WASM, this should encode a label number instead
   (since byte offsets are meaningless). The `set_continuation_to_label`
   backend function in `jit_wasm32.erl` would encode `label * 4` (to maintain
   the `>> 2` shift convention), or we introduce a WASM-specific encoding.

3. **Memory model**: WASM32 uses 32-bit linear memory. All C pointers
   (Context*, JITState*, etc.) are 32-bit offsets into linear memory. This
   is handled transparently by Emscripten, but the Erlang backend must
   use the correct (32-bit) struct offsets.

4. **Function table limits**: Each label in every loaded module needs a
   function table slot. For a typical Erlang application with hundreds of
   modules and thousands of labels, the table could grow large. WASM engines
   handle this, but there may be practical limits.

5. **Performance of `call_indirect`**: Every call through
   `ModuleNativeInterface` uses WASM `call_indirect`. This has overhead
   (type checking, bounds checking) compared to native indirect calls.
   Profiling will determine if this is a bottleneck.

6. **Pre-compilation vs runtime JIT**: For WASM, pre-compiling BEAM to
   WASM modules and bundling them in `.avm` files may be more practical
   than runtime JIT compilation, since `WebAssembly.compile()` is itself
   relatively expensive. The `jit_stream_binary` + `jit_precompile.erl`
   path already supports this pattern.

7. **Thread safety**: If AtomVM uses `SharedArrayBuffer` for SMP on WASM,
   the WASM function table is shared. Module loading must be synchronized.
