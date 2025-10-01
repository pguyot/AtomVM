# WASM JIT Backend - Next Steps (REVISED)

## Current Status ✅

The WASM JIT backend is **structurally complete** with all core operations implemented:

- ✅ 127 tests (100% passing)
- ✅ All assembler instructions (84 tests)
- ✅ Core backend operations (43 tests)
- ✅ Function calls via `call_indirect`
- ✅ Continuations (all 4 operations)
- ✅ Control flow, arithmetic, memory access

## What's Next: Practical Integration Testing

**Key Insight:** We don't need a separate `jit_wasm_module.erl`. The existing `jit.erl` and `jit_precompile.erl` infrastructure already handles compilation. We just need to:

1. Add WASM as a recognized target in `jit_precompile.erl`
2. Try to precompile tests
3. See what breaks
4. Fix issues iteratively
5. Load generated WASM and test execution

This is the **test-driven, empirical approach** we've been using successfully!

### Step 1: Add WASM to jit_precompile.erl (Easy)

**Goal:** Make WASM a recognized compilation target

**Tasks:**
1. Add WASM architecture constant to `jit.hrl`
   ```erlang
   -define(JIT_ARCH_WASM32, 3).
   ```

2. Update `jit_precompile.erl` to recognize "wasm" or "wasm32"
   ```erlang
   Arch = case BaseTarget of
       "x86_64" -> ?JIT_ARCH_X86_64;
       "aarch64" -> ?JIT_ARCH_AARCH64;
       "armv6m" -> ?JIT_ARCH_ARMV6M;
       "wasm" -> ?JIT_ARCH_WASM32;  % ADD THIS
       "wasm32" -> ?JIT_ARCH_WASM32  % ADD THIS
   end,
   ```

3. Test precompilation
   ```bash
   cd build
   erl -pa libs/jit/src/beams -noshell \
       -eval 'jit_precompile:compile("wasm", ".", "tests/erlang_tests/add.beam")' \
       -s init stop
   ```

**Expected outcome:** It will try to compile and likely fail with specific errors. This tells us what's missing!

**Estimated effort:** 1 hour

### Step 2: Fix Issues Iteratively (Test-Driven)

**Goal:** Get precompilation working for simple tests

**Approach:**
1. Try to precompile the simplest test
   ```bash
   cd build
   # Try tests/erlang_tests/test_negate.beam (very simple)
   erl -pa libs/jit/src/beams -noshell \
       -eval 'jit_precompile:compile("wasm", ".", "../tests/erlang_tests/test_negate.beam")' \
       -s init stop 2>&1 | tee compile.log
   ```

2. Read the error message
   - What operation failed?
   - What's missing?
   - Is it a placeholder that needs implementation?

3. Fix the specific issue
   - If it's a missing operation: implement it
   - If it's a bug in existing code: fix it
   - If it's WASM-specific logic needed: add it

4. Repeat until precompilation succeeds

5. Examine the output
   ```bash
   # Check what's in the avmN chunk
   erl -noshell -eval 'beam_lib:chunks("test_negate.beam", ["avmN"])' -s init stop
   ```

**Expected iterations:**
- Issue 1: Missing WASM arch constant → Add to jit.hrl
- Issue 2: Some operation not implemented → Implement or fix
- Issue 3: flush() needs to return proper format → Fix
- Issue 4-10: Various smaller issues → Fix one by one

**Estimated effort:** 1-2 weeks (depends on issues found)

### Step 3: Precompile All Tests (Validation)

**Goal:** Ensure we can precompile entire test suite

**Tasks:**
1. Create script to precompile all tests
   ```bash
   #!/bin/bash
   cd build
   for beam in ../tests/erlang_tests/*.beam; do
       echo "Compiling $beam..."
       erl -pa libs/jit/src/beams -noshell \
           -eval "jit_precompile:compile(\"wasm\", \".\", \"$beam\")" \
           -s init stop || echo "FAILED: $beam"
   done
   ```

2. Count successes vs failures
   - Target: 80%+ success rate

3. Fix remaining issues
   - Focus on common patterns
   - Defer edge cases if needed

**Success criteria:**
- Can precompile 50+ simple tests
- avmN chunks contain WASM bytecode
- No crashes during compilation

**Estimated effort:** 1 week

### Step 4: Extract and Validate WASM (Verification)

**Goal:** Verify generated WASM is valid

**Tasks:**
1. Extract WASM from avmN chunk
   ```erlang
   extract_wasm(BeamFile) ->
       {ok, {_Module, [{avmN, Chunk}]}} = beam_lib:chunks(BeamFile, [avmN]),
       % Parse chunk header (skip header, get native code)
       <<_Header:32/binary, WasmCode/binary>> = Chunk,
       WasmCode.
   ```

2. Save as .wasm file
   ```bash
   erl -noshell -eval '
       Wasm = extract_wasm("test_negate.beam"),
       file:write_file("test_negate.wasm", Wasm)
   ' -s init stop
   ```

3. Validate with wasm-validate
   ```bash
   wasm-validate test_negate.wasm
   ```

4. Disassemble and inspect
   ```bash
   wasm-dis test_negate.wasm > test_negate.wat
   less test_negate.wat
   ```

**Expected issues:**
- WASM might be incomplete (missing module structure)
- May need to wrap bare code in module sections
- This tells us what flush() needs to return

**Estimated effort:** 2-3 days

### Step 5: Runtime Integration (Later)

**Goal:** Actually execute WASM code

**Tasks:**
1. C-side loader
2. Emscripten integration
3. Test execution

**This comes AFTER** we have working precompilation!

**Estimated effort:** 3-4 weeks

## Immediate Next Action

**Start with Step 1:** Add WASM as a recognized target

1. Add `?JIT_ARCH_WASM32` constant to `libs/jit/src/jit.hrl`
2. Update `jit_precompile.erl` to handle "wasm" target
3. Try to precompile a simple test: `test_negate.beam`
4. See what error we get
5. Fix it
6. Repeat!

This is the **empirical, test-driven approach** that's worked for us:
- Compile C → WASM → analyze disassembly → implement
- Write test → see it fail → implement → see it pass

Same approach, but now at the integration level!

## Summary

**DON'T create `jit_wasm_module.erl`** - the infrastructure already exists!

**DO:**
1. ✅ Add WASM target recognition
2. ✅ Try precompilation
3. ✅ Fix errors iteratively
4. ✅ Validate generated WASM
5. ✅ Test execution (later)

**Next commit:** Add WASM32 arch constant and target recognition to jit_precompile.erl
