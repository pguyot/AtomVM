# WASM JIT Backend - Next Steps

## Current Status ✅

The WASM JIT backend is **structurally complete** with all core operations implemented:

- ✅ 127 tests (100% passing)
- ✅ All assembler instructions (84 tests)
- ✅ Core backend operations (43 tests)
- ✅ Function calls via `call_indirect`
- ✅ Continuations (all 4 operations)
- ✅ Control flow, arithmetic, memory access

## What's Next: Runtime Integration

To make the WASM backend actually execute code, we need to integrate it with the AtomVM runtime. This requires work on three fronts:

### Option A: Module Generation (Erlang-side, easier first step)

**Goal:** Generate complete, valid WASM modules from JIT bytecode

**Tasks:**
1. Create WASM module wrapper
   - Magic number & version (8 bytes)
   - Type section (function signatures)
   - Import section (primitives, memory, table)
   - Function section (declare JIT function)
   - Export section (export JIT function)
   - Code section (actual generated code)

2. Implement `flush/1` properly
   - Currently just delegates to stream
   - Should wrap generated code in complete module
   - Return valid .wasm binary

3. Test with wasm-validate
   - Ensure generated modules are valid
   - Can load with Node.js WebAssembly API

**Why start here:**
- Pure Erlang work (no C required)
- Can validate output independently
- Provides concrete artifacts to work with
- Enables offline testing

**Estimated effort:** 1-2 weeks

### Option B: Simple End-to-End Test (Recommended next)

**Goal:** Get ONE simple function working end-to-end

**Tasks:**
1. Write minimal Erlang function
   ```erlang
   -module(test_simple).
   -export([add/2]).
   add(A, B) -> A + B.
   ```

2. Manually compile to WASM
   - Use jit compiler to generate bytecode
   - Wrap in complete module (from Option A)
   - Save as test_simple.wasm

3. Load in Node.js
   ```javascript
   const fs = require('fs');
   const wasmBytes = fs.readFileSync('test_simple.wasm');
   const wasmModule = new WebAssembly.Module(wasmBytes);
   // ... setup imports ...
   const instance = new WebAssembly.Instance(wasmModule, imports);
   ```

4. Call and verify
   - Call add(5, 3)
   - Expect result = 8

**Why this approach:**
- Validates entire pipeline
- Identifies integration issues early
- Provides working example
- Builds confidence

**Estimated effort:** 2-3 weeks

### Option C: Full Platform Integration (Later)

**Goal:** Integrate WASM JIT into Emscripten platform

**Tasks:**
1. C-side changes
   - Modify `src/platforms/emscripten/src/main.c`
   - Add WASM module loading
   - Set up function table
   - Configure imports

2. Build system
   - CMake configuration
   - Emscripten linker flags
   - Export primitive functions

3. Testing infrastructure
   - Run erlang_tests with WASM JIT
   - Compare with interpreter
   - Ensure no regressions

**Why later:**
- More complex (C + build system)
- Depends on Options A & B working
- Harder to debug

**Estimated effort:** 3-4 weeks

## Recommended Roadmap

### Phase 1: Module Generation (Week 1-2)
**Focus:** Make `flush/1` generate valid WASM modules

**Deliverables:**
- Complete WASM module generation
- Valid .wasm files
- Pass wasm-validate

**Success criteria:**
```bash
# Generate module
erl -pa libs/jit/src/beams -eval 'jit:compile_module(test_simple, wasm)' -s init stop

# Validate
wasm-validate test_simple.wasm  # Should pass
```

### Phase 2: Standalone Testing (Week 3-4)
**Focus:** Load and execute generated modules

**Deliverables:**
- Node.js test harness
- Primitive function stubs
- Working add(5,3) example

**Success criteria:**
```bash
node test_wasm_jit.js
# Expected: add(5, 3) = 8 ✓
```

### Phase 3: Platform Integration (Week 5-8)
**Focus:** Integrate with AtomVM runtime

**Deliverables:**
- Modified Emscripten platform
- JIT-enabled builds
- Passing test suite

**Success criteria:**
```bash
cd build
cmake .. -DAVM_EMSCRIPTEN_JIT=ON
cmake --build .
./tests/test-erlang  # All tests pass with WASM JIT
```

## Detailed Next Action: Implement Module Generation

Let's start with module generation since it's pure Erlang and validates our bytecode.

### Step 1: Create jit_wasm_module.erl

A new module to handle WASM module generation:

```erlang
-module(jit_wasm_module).
-export([generate/1]).

generate(FunctionBodies) ->
    Magic = <<16#00, 16#61, 16#73, 16#6D>>,  % \0asm
    Version = <<16#01, 16#00, 16#00, 16#00>>,  % version 1

    TypeSection = generate_type_section(),
    ImportSection = generate_import_section(),
    FunctionSection = generate_function_section(length(FunctionBodies)),
    ExportSection = generate_export_section(),
    CodeSection = generate_code_section(FunctionBodies),

    <<Magic/binary, Version/binary,
      TypeSection/binary, ImportSection/binary,
      FunctionSection/binary, ExportSection/binary,
      CodeSection/binary>>.
```

### Step 2: Update jit_wasm.erl flush/1

```erlang
flush(#state{stream = Stream} = State) ->
    % Get raw bytecode
    Bytecode = jit_stream_binary:contents(Stream),

    % Wrap in complete WASM module
    Module = jit_wasm_module:generate([Bytecode]),

    State#state{stream = Module}.
```

### Step 3: Add validation test

```erlang
module_generation_test() ->
    State0 = ?BACKEND:new(?JIT_VARIANT_PIC, jit_stream_binary, jit_stream_binary:new(0)),
    % Simple function: return ctx
    State1 = State0,  % Empty function
    FinalState = ?BACKEND:flush(State1),
    Module = ?BACKEND:stream(FinalState),

    % Should start with WASM magic
    <<16#00, 16#61, 16#73, 16#6D, _/binary>> = Module,

    % Should validate
    TempFile = "/tmp/test.wasm",
    file:write_file(TempFile, Module),
    ?assertMatch({0, _}, os:cmd("wasm-validate " ++ TempFile)),
    file:delete(TempFile).
```

## Questions to Consider

1. **Module structure:** One function per module, or multiple functions?
   - Recommendation: Multiple functions (one per label)
   - Each label becomes a WASM function
   - Exported by name

2. **Import strategy:** Import all 71 primitives individually, or use function table?
   - We discovered: Use function table (already implemented in backend)
   - Simpler than 71 individual imports
   - Matches our call_indirect implementation

3. **Memory management:** Shared memory or separate?
   - Must be shared with C runtime
   - Import memory from environment

4. **Label resolution:** How do we handle labels as separate functions?
   - Each label → separate WASM function
   - Function indices stored in continuation
   - Requires tracking label → function index mapping

## Open Questions

1. How does jit.erl currently call backend flush?
2. What format does it expect back?
3. Should we generate one .wasm per Erlang module or one big module?
4. How do we test without C integration?

## Success Metrics

**Phase 1 (Module Generation):**
- [ ] Generate valid WASM module
- [ ] Pass wasm-validate
- [ ] Load in Node.js (even if imports missing)

**Phase 2 (Standalone Testing):**
- [ ] Execute simple function (add)
- [ ] Correct results
- [ ] All primitives stubbed

**Phase 3 (Integration):**
- [ ] 50% of erlang_tests pass
- [ ] 2x+ performance vs interpreter
- [ ] No regressions

## Resources

- WASM Spec: https://webassembly.github.io/spec/core/binary/modules.html
- Emscripten WASM: https://emscripten.org/docs/compiling/WebAssembly.html
- Node.js WASM API: https://nodejs.org/api/wasm.html

## Decision: What to do next?

**Recommended:** Start with **Phase 1: Module Generation**

1. Create `jit_wasm_module.erl`
2. Implement section generators
3. Update `flush/1`
4. Add validation test
5. Iterate until wasm-validate passes

This gives us:
- Concrete progress (valid .wasm files)
- Independent validation (wasm-validate)
- Foundation for testing
- No C dependencies yet

**Estimated time:** 1-2 weeks
**Risk:** Low (pure Erlang, well-defined spec)
**Value:** High (enables next phases)

Ready to proceed?
