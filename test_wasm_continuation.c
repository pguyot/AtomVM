// Test continuation operations for WASM JIT
// Compile with: emcc -O2 test_wasm_continuation.c -o test_wasm_continuation.wasm -s SIDE_MODULE=1 -s EXPORT_ALL=1
// Disassemble with: wasm-dis test_wasm_continuation.wasm

#include <stdint.h>

// Forward declarations matching AtomVM types
typedef struct Context Context;
typedef struct JITState JITState;
typedef struct ModuleNativeInterface ModuleNativeInterface;

// Function pointer type for continuations
typedef Context *(*continuation_func)(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface);

// JITState structure with continuation field
struct JITState {
    void *module;              // offset 0
    continuation_func continuation;  // offset 4 (32-bit pointer)
    int reduction_count;       // offset 8
};

// Test 1: Store a function pointer to jit_state->continuation
void test_set_continuation_direct(JITState *jit_state, continuation_func func_ptr) {
    jit_state->continuation = func_ptr;
}

// Test 2: Load a function pointer from an array and store to continuation
void test_set_continuation_from_array(JITState *jit_state, continuation_func *func_array, int index) {
    jit_state->continuation = func_array[index];
}

// Test 3: Jump to continuation (call the stored function pointer)
Context *test_jump_to_continuation(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface) {
    continuation_func cont = jit_state->continuation;
    return cont(ctx, jit_state, native_interface);
}

// Test 4: Set continuation to a specific address/offset in a table
void test_set_continuation_to_offset(JITState *jit_state, continuation_func *func_table, uint32_t offset_bytes) {
    // Calculate pointer from base + offset
    // offset_bytes should be byte offset, we need to convert to index
    continuation_func *ptr = (continuation_func *)((uint8_t *)func_table + offset_bytes);
    jit_state->continuation = *ptr;
}

// Test 5: Conditional jump to continuation
Context *test_conditional_jump_to_continuation(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface, int condition) {
    if (condition) {
        continuation_func cont = jit_state->continuation;
        return cont(ctx, jit_state, native_interface);
    } else {
        return ctx;  // No jump, return original context
    }
}

// Test 6: Store function pointer from another struct field
typedef struct {
    continuation_func label_0;
    continuation_func label_1;
    continuation_func label_2;
} LabelTable;

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
