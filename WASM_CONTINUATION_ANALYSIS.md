# WASM Continuation Analysis - Function Pointer Storage and Jumping

## Overview

By compiling C code to WASM and analyzing the output, we can understand how to implement continuation operations in the WASM JIT backend.

## Key Findings

### 1. Storing Function Pointers is Simple - Just i32.store!

**Critical Discovery:** Function pointers in WASM32 are just 32-bit integers (indices into the function table). Storing a continuation is a simple memory store operation.

### Function 1: test_set_continuation_direct

**C Code:**
```c
void test_set_continuation_direct(JITState *jit_state, continuation_func func_ptr) {
    jit_state->continuation = func_ptr;
}
```

**WASM Disassembly:**
```wat
(func $1 (param $0 i32) (param $1 i32)
  (i32.store offset=4
   (local.get $0)
   (local.get $1)
  )
)
```

**Analysis:**
- `jit_state->continuation` is at offset 4 (second field, after `module` at offset 0)
- Function pointer is just passed as `i32` parameter
- Simple `i32.store` with offset=4
- **For JIT:** If we have label index in a local, just store it to `jit_state` at offset 4

**Implementation Pattern:**
```erlang
set_continuation_to_label(State, LabelIndex) ->
    Code = <<
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:i32_const(LabelIndex))/binary,  % Label as function index
        (jit_wasm_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>.
```

### Function 2: test_set_continuation_from_array

**C Code:**
```c
void test_set_continuation_from_array(JITState *jit_state, continuation_func *func_array, int index) {
    jit_state->continuation = func_array[index];
}
```

**WASM Disassembly:**
```wat
(func $2 (param $0 i32) (param $1 i32) (param $2 i32)
  (i32.store offset=4
   (local.get $0)
   (i32.load
    (i32.add
     (local.get $1)
     (i32.shl
      (local.get $2)
      (i32.const 2)
     )
    )
   )
  )
)
```

**Analysis:**
- Array access: `base + (index << 2)` for 32-bit pointers
- Load function pointer from array
- Store to jit_state->continuation at offset 4
- **For JIT:** If labels are in an array, we can load by computed index

### Function 3: test_jump_to_continuation ⭐ MOST IMPORTANT

**C Code:**
```c
Context *test_jump_to_continuation(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface) {
    continuation_func cont = jit_state->continuation;
    return cont(ctx, jit_state, native_interface);
}
```

**WASM Disassembly:**
```wat
(func $3 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (call_indirect (type $i32_i32_i32_=>_i32)
   (local.get $0)
   (local.get $1)
   (local.get $2)
   (i32.load offset=4
    (local.get $1)
   )
  )
)
```

**Analysis:**
- Load continuation from `jit_state->continuation` (offset 4)
- Push arguments: ctx, jit_state, native_interface
- Call indirectly with loaded function pointer
- **This is IDENTICAL to how we call primitives!**

**Implementation Pattern:**
```erlang
jump_to_continuation(State) ->
    Code = <<
        % Arguments: ctx, jit_state, native_interface
        (jit_wasm_asm:local_get(0))/binary,  % ctx
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:local_get(2))/binary,  % native_interface
        % Load continuation from jit_state->continuation
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:i32_load(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
        % Call indirectly
        (jit_wasm_asm:call_indirect(0))/binary
    >>.
```

### Function 4: test_set_continuation_to_offset

**C Code:**
```c
void test_set_continuation_to_offset(JITState *jit_state, continuation_func *func_table, uint32_t offset_bytes) {
    continuation_func *ptr = (continuation_func *)((uint8_t *)func_table + offset_bytes);
    jit_state->continuation = *ptr;
}
```

**WASM Disassembly:**
```wat
(func $4 (param $0 i32) (param $1 i32) (param $2 i32)
  (i32.store offset=4
   (local.get $0)
   (i32.load
    (i32.add
     (local.get $1)
     (local.get $2)
    )
   )
  )
)
```

**Analysis:**
- Byte offset calculation: `base + offset_bytes`
- Load function pointer from computed address
- Store to continuation
- **For JIT:** We can use byte offsets directly with `i32.add`

**Implementation Pattern:**
```erlang
set_continuation_to_offset(State, OffsetBytes) ->
    Code = <<
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        % Load from label_table + offset
        (jit_wasm_asm:local_get(2))/binary,  % native_interface (has label table)
        (jit_wasm_asm:i32_const(OffsetBytes))/binary,
        (jit_wasm_asm:i32_add())/binary,
        (jit_wasm_asm:i32_load(2, 0))/binary,  % Load function pointer
        % Store to jit_state->continuation
        (jit_wasm_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>.
```

### Function 5: test_conditional_jump_to_continuation

**C Code:**
```c
Context *test_conditional_jump_to_continuation(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface, int condition) {
    if (condition) {
        continuation_func cont = jit_state->continuation;
        return cont(ctx, jit_state, native_interface);
    } else {
        return ctx;
    }
}
```

**WASM Disassembly:**
```wat
(func $5 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (if (result i32)
   (local.get $3)
   (call_indirect (type $i32_i32_i32_=>_i32)
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (i32.load offset=4
     (local.get $1)
    )
   )
   (local.get $0)
  )
)
```

**Analysis:**
- Uses `if/else` with result type `i32`
- If true: load continuation and call_indirect
- If false: return ctx unchanged
- **For JIT:** We can use structured if/else for conditional jumps

### Function 6: test_set_continuation_from_label_table

**C Code:**
```c
void test_set_continuation_from_label_table(JITState *jit_state, LabelTable *labels, int label_index) {
    continuation_func func;
    switch (label_index) {
        case 0: func = labels->label_0; break;
        case 1: func = labels->label_1; break;
        case 2: func = labels->label_2; break;
        default: func = labels->label_0; break;
    }
    jit_state->continuation = func;
}
```

**WASM Disassembly:**
```wat
(func $6 (param $0 i32) (param $1 i32) (param $2 i32)
  (block $label$1
   (block $label$2
    (block $label$3
     (br_table $label$3 $label$2 $label$1
      (i32.sub
       (local.get $2)
       (i32.const 1)
      )
     )
    )
    (local.set $1
     (i32.add
      (local.get $1)
      (i32.const 4)
     )
    )
    (br $label$1)
   )
   (local.set $1
    (i32.add
     (local.get $1)
     (i32.const 8)
    )
   )
  )
  (i32.store offset=4
   (local.get $0)
   (i32.load
    (local.get $1)
   )
  )
)
```

**Analysis:**
- Uses `br_table` for switch statement
- Adjusts pointer offset based on case (add 4 for each field)
- Loads from computed address
- **For JIT:** Complex label selection can use br_table

## Implementation Summary

### Key Offsets

```erlang
-define(JITSTATE_MODULE_OFFSET, 0).
-define(JITSTATE_CONTINUATION_OFFSET, 4).
-define(JITSTATE_REDUCTIONCOUNT_OFFSET, 8).
```

### Operation Implementations

#### 1. set_continuation_to_label/2

**Simple case: Label is a known function index**

```erlang
set_continuation_to_label(State, Label) ->
    % Get function index for this label
    FuncIndex = get_label_func_index(State, Label),
    Code = <<
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:i32_const(FuncIndex))/binary,  % Label as func index
        (jit_wasm_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>.
```

#### 2. set_continuation_to_offset/1

**When continuation is at a byte offset**

```erlang
set_continuation_to_offset(State, Offset) ->
    Code = <<
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        % Load from module label table + offset
        (jit_wasm_asm:local_get(2))/binary,  % native_interface (or label table)
        (jit_wasm_asm:i32_const(Offset))/binary,
        (jit_wasm_asm:i32_add())/binary,
        (jit_wasm_asm:i32_load(2, 0))/binary,  % Load function pointer
        % Store to jit_state->continuation
        (jit_wasm_asm:i32_store(2, ?JITSTATE_CONTINUATION_OFFSET))/binary
    >>.
```

#### 3. jump_to_continuation/1

**Call the stored continuation**

```erlang
jump_to_continuation(State) ->
    Code = <<
        % Standard arguments
        (jit_wasm_asm:local_get(0))/binary,  % ctx
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:local_get(2))/binary,  % native_interface
        % Load continuation
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:i32_load(2, ?JITSTATE_CONTINUATION_OFFSET))/binary,
        % Call it!
        (jit_wasm_asm:call_indirect(0))/binary
    >>.
```

#### 4. jump_to_offset/2

**Jump to a label at a specific offset (combination of set + jump)**

```erlang
jump_to_offset(State, Offset) ->
    Code = <<
        % Standard arguments
        (jit_wasm_asm:local_get(0))/binary,  % ctx
        (jit_wasm_asm:local_get(1))/binary,  % jit_state
        (jit_wasm_asm:local_get(2))/binary,  % native_interface
        % Load function pointer from offset
        (jit_wasm_asm:local_get(2))/binary,  % native_interface (label table)
        (jit_wasm_asm:i32_const(Offset))/binary,
        (jit_wasm_asm:i32_add())/binary,
        (jit_wasm_asm:i32_load(2, 0))/binary,  % Load function pointer
        % Call it directly
        (jit_wasm_asm:call_indirect(0))/binary
    >>.
```

## Critical Insights

### 1. Function Pointers are Just Indices

In WASM, function pointers are 32-bit indices into the function table. They can be:
- Stored in memory (like any i32)
- Passed as parameters
- Loaded from arrays
- Used with `call_indirect`

### 2. Continuations Use Same Mechanism as Primitives

Both continuations and primitive calls use `call_indirect` with the same signature:
```wat
(type $func_type (func (param i32 i32 i32) (result i32)))
```

### 3. No Special Instructions Needed

We already have everything we need:
- `i32.load` / `i32.store` for accessing continuation field
- `call_indirect` for calling stored function pointers
- `i32.add` for offset calculations

### 4. Labels Must Be in Function Table

For continuations to work, each label in a module must:
1. Be a separate function in the WASM module
2. Have an entry in the function table
3. Have a known index that can be stored as i32

## Runtime Requirements

### 1. Module Structure

Each Erlang module compiled to WASM needs:

```wat
(module
  ;; Type for all JIT functions
  (type $jit_func (func (param i32 i32 i32) (result i32)))

  ;; Import function table
  (import "env" "__indirect_function_table" (table 256 funcref))

  ;; Each label becomes a function
  (func $label_0 (param $ctx i32) (param $jit_state i32) (param $native i32) (result i32)
    ;; Label 0 code
  )

  (func $label_1 (param $ctx i32) (param $jit_state i32) (param $native i32) (result i32)
    ;; Label 1 code
  )

  ;; Export all labels
  (export "label_0" (func $label_0))
  (export "label_1" (func $label_1))
)
```

### 2. Label Index Resolution

The JIT compiler needs to know function indices for labels. Options:

**Option A: Sequential allocation**
- Label 0 → function index N
- Label 1 → function index N+1
- etc.

**Option B: Label table in native_interface**
- Store array of function indices
- Load by label ID

### 3. C-Side Setup

The C side must:
1. Load WASM module
2. Get function indices for each label
3. Make them available (either as known offsets or in a table)
4. Ensure function table is populated

## Testing Strategy

### Unit Tests

Test each operation:
1. `set_continuation_to_label` - verify i32.store to offset 4
2. `set_continuation_to_offset` - verify load + store sequence
3. `jump_to_continuation` - verify call_indirect from offset 4
4. `jump_to_offset` - verify combined load + call_indirect

### Integration Tests

Test continuation flow:
1. Set continuation to label 1
2. Jump to continuation
3. Verify label 1 code executes

## Next Steps

1. ✅ Analyze C→WASM for continuation operations (DONE)
2. ⏭️ Implement `set_continuation_to_label/2` in `jit_wasm.erl`
3. ⏭️ Implement `set_continuation_to_offset/1`
4. ⏭️ Implement `jump_to_continuation/1`
5. ⏭️ Implement `jump_to_offset/2`
6. ⏭️ Add tests for all four operations
7. ⏭️ Update documentation

## Conclusion

**Continuations in WASM are straightforward!**

Key points:
- Function pointers are i32 values
- Store with `i32.store offset=4` to `jit_state->continuation`
- Load with `i32.load offset=4` from `jit_state`
- Call with `call_indirect` (same as primitives)
- No new instructions needed!

The pattern exactly matches primitive calls, just with different source (jit_state->continuation instead of native_interface->functions[]).

This discovery means we can implement all continuation operations with our existing assembler instructions! 🎉
