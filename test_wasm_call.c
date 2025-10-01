/*
 * Test code to understand WASM function calling convention
 * for calling functions through ModuleNativeInterface
 */

#include <stdint.h>

// Simplified types matching AtomVM
typedef void* Context;
typedef void* term;

typedef struct JITState {
    void *module;
    int32_t continuation;
    int32_t reduction_count;
} JITState;

// Function pointer type matching AtomVM primitives
typedef Context *(*native_handler)(Context *ctx, JITState *jit_state, void *native_interface);

typedef struct ModuleNativeInterface {
    native_handler functions[128];  // Array of function pointers
    int count;
} ModuleNativeInterface;

// Test function: call primitive at index 0
Context *test_call_primitive_0(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface) {
    // Get function pointer from array
    native_handler func = native_interface->functions[0];

    // Call it
    return func(ctx, jit_state, native_interface);
}

// Test function: call primitive at index 5
Context *test_call_primitive_5(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface) {
    native_handler func = native_interface->functions[5];
    return func(ctx, jit_state, native_interface);
}

// Test function: call primitive at dynamic index
Context *test_call_primitive_dynamic(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface, int index) {
    native_handler func = native_interface->functions[index];
    return func(ctx, jit_state, native_interface);
}

// Test function: increment reduction count and call if zero
Context *test_decrement_reductions(Context *ctx, JITState *jit_state, ModuleNativeInterface *native_interface) {
    jit_state->reduction_count--;

    if (jit_state->reduction_count == 0) {
        // Call scheduler (let's say it's at index 10)
        native_handler scheduler = native_interface->functions[10];
        return scheduler(ctx, jit_state, native_interface);
    }

    return ctx;
}
