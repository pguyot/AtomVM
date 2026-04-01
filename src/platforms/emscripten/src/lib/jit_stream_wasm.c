/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 Paul Guyot <pguyot@kallisys.net>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

/**
 * @file jit_stream_wasm.c
 * @brief WASM JIT stream implementation for Emscripten platform.
 *
 * This module provides:
 * - jit:stream_module/0 NIF that returns jit_stream_binary
 * - jit_stream_entry_point() that compiles a WASM module from a JIT binary
 *   stream and returns a table of function pointers
 *
 * Stream format (produced by jit_wasm32.erl):
 *   Bytes 0..3:   num_entries = labels_count + 1 (uint32_t little-endian)
 *   Bytes 4..4+num_entries*4-1: Reserved jump table area (uint32_t each)
 *   Bytes 4+num_entries*4..wasm_offset-1: (unused padding)
 *   Bytes wasm_offset..: Complete WASM module binary
 *
 * The WASM module exports functions named "f0", "f1", ... "fN" corresponding
 * to each BEAM label. Each function has the ModuleNativeEntryPoint signature:
 *   (i32 ctx, i32 jit_state, i32 native_interface) -> i32
 *
 * jit_stream_entry_point:
 *   1. Extracts the binary data from the stream term
 *   2. Reads num_entries from the header
 *   3. Extracts the WASM module binary
 *   4. Compiles and instantiates the WASM module via JavaScript WebAssembly API
 *   5. Registers each exported function in Emscripten's indirect function table
 *   6. Allocates a function pointer array (the jump table)
 *   7. Fills it with the registered function pointers
 *   8. Returns the array base cast as ModuleNativeEntryPoint
 */

#ifndef AVM_NO_JIT

#include <stdlib.h>
#include <string.h>

#include <emscripten.h>

#include "context.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "jit.h"
#include "module.h"
#include "nifs.h"
#include "sys.h"
#include "term.h"

// NIF: jit:stream_module/0
// Returns the atom 'jit_stream_binary' since WASM JIT uses the binary stream
// module (pure Erlang, no C NIFs needed for stream operations).
static term nif_jit_stream_module(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);

    return globalcontext_make_atom(ctx->global, ATOM_STR("\x11", "jit_stream_binary"));
}

static const struct Nif jit_stream_module_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_module
};

/**
 * @brief Compile a WASM module from binary data and return function pointers.
 *
 * This JavaScript function is called from jit_stream_entry_point.
 * It receives a pointer to the WASM module binary data and its size,
 * plus the number of function entries needed.
 *
 * It compiles the WASM module, instantiates it with access to Emscripten's
 * linear memory and indirect function table, then registers each exported
 * function (named "f0", "f1", ...) in Emscripten's function table.
 *
 * @param wasm_data Pointer to the WASM module binary in linear memory
 * @param wasm_size Size of the WASM module binary
 * @param func_table Pointer to output array of function pointer slots
 * @param num_entries Number of entries in the function pointer array
 * @return 1 on success, 0 on failure
 */
EM_JS(int, jit_compile_wasm_module, (const uint8_t *wasm_data, int wasm_size, void **func_table, int num_entries), {
    try {
        // Extract WASM binary from Emscripten's linear memory
        var bytes = new Uint8Array(HEAPU8.buffer, wasm_data, wasm_size);

        // Compile the WASM module synchronously
        var wasmModule = new WebAssembly.Module(bytes);

        // Prepare imports: the JIT module needs access to Emscripten's memory
        // and indirect function table for call_indirect
        var imports = {
            env : {
                memory : wasmMemory,
                __indirect_function_table : wasmTable
            }
        };

        // Instantiate the module
        var instance = new WebAssembly.Instance(wasmModule, imports);

        // Register each exported function in Emscripten's indirect function table
        for (var i = 0; i < num_entries; i++) {
            var funcName = "f" + i;
            var func = instance.exports[funcName];
            if (func) {
                // addFunction registers the function and returns its table index
                var funcPtr = addFunction(func, 'iiii'); // (i32, i32, i32) -> i32
                // Store the function pointer in the output table
                HEAP32[(func_table >> 2) + i] = funcPtr;
            } else {
                // Label not implemented, store NULL
                HEAP32[(func_table >> 2) + i] = 0;
            }
        }

        return 1;
    } catch (e) {
        err("JIT WASM compilation failed: " + e.message);
        return 0;
    }
});

/**
 * @brief Parse the JIT stream header and compile the embedded WASM module.
 *
 * @details Shared by jit_stream_entry_point (runtime JIT) and
 * sys_map_native_code (precompiled native code).
 *
 * Stream format (produced by jit_wasm32:return_labels_and_lines):
 *   Bytes 0..3:   num_entries (uint32_t little-endian)
 *   Bytes 4..7:   wasm_offset (uint32_t little-endian)
 *   Bytes 8..8+num_entries*4-1: jump table area (filled by this function)
 *   Bytes wasm_offset..: Complete WASM module binary
 *
 * @param data pointer to the stream data
 * @param data_size size of the stream data
 * @return function pointer table cast as ModuleNativeEntryPoint, or NULL on failure
 */
static ModuleNativeEntryPoint compile_wasm_stream(const uint8_t *data, size_t data_size)
{
    if (data_size < 8) {
        return NULL;
    }

    // Read the header: num_entries (first 4 bytes, little-endian)
    uint32_t num_entries = data[0] | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);

    // Read the WASM module offset (next 4 bytes, little-endian)
    uint32_t wasm_offset = data[4] | (data[5] << 8) | (data[6] << 16) | (data[7] << 24);

    if (wasm_offset >= data_size) {
        return NULL;
    }

    const uint8_t *wasm_data = data + wasm_offset;
    size_t wasm_size = data_size - wasm_offset;

    // Verify WASM magic number
    if (wasm_size < 8 || wasm_data[0] != 0x00 || wasm_data[1] != 0x61
        || wasm_data[2] != 0x73 || wasm_data[3] != 0x6D) {
        return NULL;
    }

    // Allocate the function pointer table
    ModuleNativeEntryPoint *func_table = calloc(num_entries, sizeof(ModuleNativeEntryPoint));
    if (func_table == NULL) {
        return NULL;
    }

    // Compile the WASM module and fill the function pointer table
    int result = jit_compile_wasm_module(wasm_data, wasm_size, (void **) func_table, num_entries);
    if (!result) {
        free(func_table);
        return NULL;
    }

    // Return the table base cast as a function pointer.
    // module_get_native_entry_point will dereference to get actual function pointers
    // when JIT_JUMPTABLE_IS_DATA is defined.
    return (ModuleNativeEntryPoint) func_table;
}

ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    UNUSED(ctx);

    // The stream is a binary term (from jit_stream_binary)
    if (!term_is_binary(jit_stream)) {
        return NULL;
    }

    const uint8_t *data = (const uint8_t *) term_binary_data(jit_stream);
    size_t data_size = term_binary_size(jit_stream);

    return compile_wasm_stream(data, data_size);
}

ModuleNativeEntryPoint sys_map_native_code(const uint8_t *native_code, size_t size, size_t offset)
{
    return compile_wasm_stream(native_code + offset, size - offset);
}

bool sys_get_cache_native_code(GlobalContext *global, Module *mod, uint16_t *version, ModuleNativeEntryPoint *entry_point, uint32_t *labels)
{
    UNUSED(global);
    UNUSED(mod);
    UNUSED(version);
    UNUSED(entry_point);
    UNUSED(labels);
    return false;
}

void sys_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    UNUSED(global);
    UNUSED(mod);
    UNUSED(version);
    UNUSED(entry_point);
    UNUSED(labels);
}

const struct Nif *jit_stream_wasm_get_nif(const char *nifname)
{
    if (strcmp("jit:stream_module/0", nifname) == 0) {
        return &jit_stream_module_nif;
    }
    return NULL;
}

#endif /* AVM_NO_JIT */
