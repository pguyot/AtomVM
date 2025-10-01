# WASM Function Call Analysis - Real Implementation

## Key Discovery: WASM Uses call_indirect, NOT Direct Calls

After compiling real C code to WASM and analyzing the disassembly, the actual calling convention is now clear.

## What We Learned

### Original Assumption (WRONG)
We thought we could import all primitives and use direct `call` instructions:
```wat
(import "atomvm" "primitive_0" (func $prim0 ...))
(call $prim0)  ; Direct call
```

### Actual Reality (CORRECT)
WASM uses `call_indirect` with a function table:
```wat
(import "env" "__indirect_function_table" (table $timport$0 0 funcref))
(call_indirect (type $i32_i32_i32_=>_i32)
  (local.get $0)    ; arg 1: ctx
  (local.get $1)    ; arg 2: jit_state
  (local.get $2)    ; arg 3: native_interface
  (i32.load         ; Load function pointer from array
    (local.get $2)  ; native_interface->functions[index]
  )
)
```

## Disassembly Analysis

### Function 1: test_call_primitive_0
```wat
(func $1 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (call_indirect (type $i32_i32_i32_=>_i32)
   (local.get $0)    ; ctx
   (local.get $1)    ; jit_state
   (local.get $2)    ; native_interface
   (i32.load         ; Load from native_interface->functions[0]
    (local.get $2)   ; Address = native_interface + 0
   )
  )
)
```

**Key Points:**
- Uses `call_indirect` with type signature
- Loads function pointer from `native_interface->functions[0]`
- Offset is 0 (first element of array)
- Function table index comes from loaded value

### Function 2: test_call_primitive_5
```wat
(func $2 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (call_indirect (type $i32_i32_i32_=>_i32)
   (local.get $0)
   (local.get $1)
   (local.get $2)
   (i32.load offset=20    ; Load from native_interface->functions[5]
    (local.get $2)        ; Offset = 5 * 4 = 20 bytes
   )
  )
)
```

**Key Points:**
- Same pattern but offset=20 (5 * sizeof(pointer) = 5 * 4 = 20)
- Shows how array indexing works in WASM

### Function 3: test_call_primitive_dynamic
```wat
(func $3 (param $0 i32) (param $1 i32) (param $2 i32) (param $3 i32) (result i32)
  (call_indirect (type $i32_i32_i32_=>_i32)
   (local.get $0)
   (local.get $1)
   (local.get $2)
   (i32.load
    (i32.add
     (local.get $2)           ; native_interface
     (i32.shl
      (local.get $3)          ; index
      (i32.const 2)           ; << 2 (multiply by 4)
     )
    )
   )
  )
)
```

**Key Points:**
- Dynamic indexing: `base + (index << 2)`
- Shows computed offset for runtime index

### Function 4: test_decrement_reductions
```wat
(func $4 (param $0 i32) (param $1 i32) (param $2 i32) (result i32)
  (local $3 i32)
  ; Store decremented value
  (i32.store offset=8
   (local.get $1)
   (local.tee $3
    (i32.sub
     (i32.load offset=8
      (local.get $1)
     )
     (i32.const 1)
    )
   )
  )
  ; Conditional call
  (if (result i32)
   (local.get $3)
   (local.get $0)      ; If non-zero, return ctx
   (call_indirect (type $i32_i32_i32_=>_i32)
    (local.get $0)
    (local.get $1)
    (local.get $2)
    (i32.load offset=40    ; Load from native_interface->functions[10]
     (local.get $2)        ; Offset = 10 * 4 = 40
    )
   )
  )
)
```

**Key Points:**
- Shows reduction counting pattern
- Conditional indirect call
- Scheduler at index 10 → offset 40

## Critical Requirements

### 1. Function Table Must Be Initialized by C Side

The WASM module imports a function table:
```wat
(import "env" "__indirect_function_table" (table $timport$0 0 funcref))
```

The C side must:
1. Create function table with all primitive pointers
2. Pass it to WASM module at instantiation

### 2. Type Signature Required

WASM needs type declaration:
```wat
(type $i32_i32_i32_=>_i32 (func (param i32 i32 i32) (result i32)))
```

All primitives must match this signature.

## Correct Implementation Strategy

### Step 1: C-Side Setup

```c
// In Emscripten platform code
#include <emscripten.h>

// All primitives have this signature
typedef Context* (*primitive_func)(Context*, JITState*, ModuleNativeInterface*);

// Create function table
EM_JS(void, setup_function_table, (ModuleNativeInterface* ni), {
  // Get function pointers from native interface
  const table = wasmTable; // Emscripten's function table

  // Populate table with primitive functions
  // Each primitive is already in the table at its index
  // (Emscripten handles this automatically)
});
```

### Step 2: Erlang Code Generation

```erlang
-spec call_primitive(state(), non_neg_integer(), [arg()]) -> state().
call_primitive(
    #state{stream_module = StreamModule, stream = Stream0} = State,
    PrimIndex,
    Args
) ->
    % 1. Push arguments on stack
    ArgCode = prepare_args(Args),

    % 2. Load function pointer from native_interface->functions[PrimIndex]
    % native_interface is in local 2
    LoadFunc = <<
        (jit_wasm_asm:local_get(2))/binary,  % native_interface
        (jit_wasm_asm:i32_load(2, PrimIndex * 4))/binary  % Load functions[PrimIndex]
    >>,

    % 3. call_indirect with type 0 (i32,i32,i32)->i32
    CallCode = jit_wasm_asm:call_indirect(0),

    Code = <<ArgCode/binary, LoadFunc/binary, CallCode/binary>>,
    Stream1 = StreamModule:append(Stream0, Code),
    State#state{stream = Stream1}.

prepare_args(Args) ->
    % Always pass ctx (local 0), jit_state (local 1), native_interface (local 2)
    <<
        (jit_wasm_asm:local_get(0))/binary,
        (jit_wasm_asm:local_get(1))/binary,
        (jit_wasm_asm:local_get(2))/binary
    >>.
```

### Step 3: Add call_indirect to Assembler

```erlang
%% In jit_wasm_asm.erl

%% @doc call_indirect typeidx - Indirect function call
-spec call_indirect(non_neg_integer()) -> binary().
call_indirect(TypeIdx) ->
    <<16#11, (encode_uleb128(TypeIdx))/binary, 16#00>>.
```

### Step 4: Module Setup

The WASM module needs:
```wat
(module
  (type $func_type (func (param i32 i32 i32) (result i32)))
  (import "env" "__indirect_function_table" (table 128 funcref))
  (import "env" "memory" (memory 1))

  (func $jit_func (param $ctx i32) (param $jit_state i32) (param $native i32) (result i32)
    ;; Load ctx, jit_state, native
    (local.get $ctx)
    (local.get $jit_state)
    (local.get $native)

    ;; Load function pointer from native->functions[5]
    (i32.load offset=20
      (local.get $native)
    )

    ;; Call indirectly
    (call_indirect (type $func_type))
  )
)
```

## Why This Changes Everything

### Old (Wrong) Approach
- Import 71 functions individually
- Use direct `call` instructions
- Complex import setup

### New (Correct) Approach
- Import ONE function table
- Use `call_indirect` with loaded pointers
- Native interface already has function pointers!
- Much simpler!

## Immediate Action Items

1. **Add `call_indirect` to assembler** ✓ (code above)

2. **Update `call_primitive` implementation:**
   ```erlang
   call_primitive(State, Index, _Args) ->
       Code = <<
           (jit_wasm_asm:local_get(0))/binary,  % ctx
           (jit_wasm_asm:local_get(1))/binary,  % jit_state
           (jit_wasm_asm:local_get(2))/binary,  % native_interface
           (jit_wasm_asm:local_get(2))/binary,  % native_interface (for load)
           (jit_wasm_asm:i32_load(2, Index * 4))/binary,  % Load func ptr
           (jit_wasm_asm:call_indirect(0))/binary  % Call it
       >>,
       ...
   ```

3. **Update module generation** to declare type 0:
   ```erlang
   generate_type_section() ->
       % Type 0: (i32, i32, i32) -> i32
       TypeCount = encode_uleb128(1),
       Type0 = <<16#60, 16#03, 16#7F, 16#7F, 16#7F, 16#01, 16#7F>>,
       <<16#01, TypeCount/binary, Type0/binary>>.
   ```

4. **C-side needs minimal changes:**
   - Emscripten already provides function table
   - Native interface already has function pointers
   - Just ensure they're in WASM table (Emscripten handles this)

## Performance Implications

**call_indirect is slightly slower than direct call** (~10-20% overhead) but:
- It's the ONLY way to call through pointers in WASM
- Still much faster than interpreter
- Matches how C compilers handle it
- No alternative approach exists

## Conclusion

The actual WASM calling convention is simpler than our original plan:
- No need to import 71 functions
- Just use `call_indirect` with function table
- Native interface structure maps directly to WASM memory
- Implementation is straightforward

This discovery drastically simplifies the implementation!