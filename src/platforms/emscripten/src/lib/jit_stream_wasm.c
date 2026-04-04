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
 * Each thread lazily compiles its own WebAssembly.Instance of the JIT module
 * because Emscripten's indirect function table (wasmTable) is per-thread in
 * pthreads mode. Functions registered via addFunction on the main thread are
 * not visible to worker threads.
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
 * @brief Get a per-thread function pointer for a JIT label.
 *
 * Uses a thread-local JavaScript cache (Module._jitCache) to lazily compile
 * the WASM module on each thread that needs it. The cache maps the WASM binary
 * pointer (in shared memory) to an array of addFunction indices.
 *
 * @param wasm_binary Pointer to the WASM module binary in shared memory
 * @param wasm_size Size of the WASM module binary
 * @param num_entries Number of function entries (labels)
 * @param label The label index to look up
 * @return Function pointer (addFunction index) for the current thread, or 0
 */
EM_JS(int, jit_get_thread_func_ptr, (const uint8_t *wasm_binary, int wasm_size, int num_entries, int label), {
    // Module._jitCache is thread-local: each Emscripten worker has its own
    // Module object, so this Map is not shared between threads.
    if (!Module._jitCache) {
        Module._jitCache = new Map();
    }

    var cache = Module._jitCache.get(wasm_binary);
    if (!cache) {
        try {
            var bytes = new Uint8Array(HEAPU8.buffer, wasm_binary, wasm_size);
            var wasmModule = new WebAssembly.Module(bytes);
            var imports = {
                env : {
                    memory : wasmMemory,
                    __indirect_function_table : wasmTable
                }
            };
            var instance = new WebAssembly.Instance(wasmModule, imports);

            cache = new Int32Array(num_entries);
            for (var i = 0; i < num_entries; i++) {
                var func = instance.exports["f" + i];
                if (func) {
                    cache[i] = addFunction(func, 'iiii');
                }
                // else cache[i] remains 0 (no function for this label)
            }
            Module._jitCache.set(wasm_binary, cache);
        } catch (e) {
            err("JIT per-thread WASM compilation failed: " + e.message);
            return 0;
        }
    }

    if (label < 0 || label >= num_entries) {
        return 0;
    }
    return cache[label];
});

/**
 * @brief Header stored at func_table[-1] for WASM JIT modules.
 *
 * Contains pointers to the lines/cont_map metadata and the WASM binary,
 * enabling per-thread lazy compilation.
 */
struct JITWasmHeader
{
    uint8_t *lines_metadata; // lines + cont_label_map data (or NULL)
    uint8_t *wasm_binary; // copy of WASM module binary
    uint32_t wasm_binary_size; // size of WASM binary
    uint32_t num_entries; // number of function entries
};

/**
 * @brief Get the lines metadata pointer from a WASM JIT module's native_code.
 *
 * Used by module.c for line info and cont_label_map lookups.
 */
const uint8_t *jit_wasm_get_lines_metadata(const void *native_code)
{
    const ModuleNativeEntryPoint *func_table = (const ModuleNativeEntryPoint *) native_code;
    const struct JITWasmHeader *header = (const struct JITWasmHeader *) (uintptr_t) func_table[-1];
    if (IS_NULL_PTR(header)) {
        return NULL;
    }
    return header->lines_metadata;
}

/**
 * @brief Get a per-thread function pointer for a WASM JIT module label.
 *
 * Called from module_get_native_entry_point (module.c) for WASM targets.
 */
ModuleNativeEntryPoint jit_wasm_get_entry_point(const void *native_code, int label)
{
    const ModuleNativeEntryPoint *func_table = (const ModuleNativeEntryPoint *) native_code;
    const struct JITWasmHeader *header = (const struct JITWasmHeader *) (uintptr_t) func_table[-1];
    if (IS_NULL_PTR(header) || IS_NULL_PTR(header->wasm_binary)) {
        return NULL;
    }
    int fp = jit_get_thread_func_ptr(header->wasm_binary, header->wasm_binary_size, header->num_entries, label);
    return (ModuleNativeEntryPoint) (uintptr_t) fp;
}

/**
 * @brief Parse the JIT stream header and prepare the JIT module.
 *
 * Stores the WASM binary in shared memory so each thread can lazily compile
 * its own WebAssembly.Instance.
 *
 * Stream format (produced by jit_wasm32:return_labels_and_lines):
 *   Bytes 0..3:   num_entries (uint32_t LE)
 *   Bytes 4..7:   wasm_offset (uint32_t LE)
 *   Bytes 8..11:  lines_offset (uint32_t LE)
 *   Bytes wasm_offset..lines_offset-1: Complete WASM module binary
 *   Bytes lines_offset..: Lines + cont_label_map metadata
 *
 * Layout of the allocated block returned as native_code:
 *   block[0]      = pointer to JITWasmHeader
 *   block[1..N]   = func_table (unused in per-thread mode, kept for layout compat)
 *
 * module->native_code points to &block[1] (func_table).
 * The JITWasmHeader pointer is at func_table[-1].
 */
static ModuleNativeEntryPoint compile_wasm_stream(const uint8_t *data, size_t data_size)
{
    if (data_size < 12) {
        return NULL;
    }

    // Read the header (12 bytes): num_entries, wasm_offset, lines_offset
    uint32_t num_entries = data[0] | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);
    if (UNLIKELY(num_entries > 0x00FFFFFF)) {
        return NULL;
    }
    uint32_t wasm_offset = data[4] | (data[5] << 8) | (data[6] << 16) | (data[7] << 24);
    uint32_t lines_offset = data[8] | (data[9] << 8) | (data[10] << 16) | (data[11] << 24);

    if (wasm_offset >= data_size) {
        return NULL;
    }

    // Clamp lines_offset to data_size to prevent OOB reads
    if (lines_offset > data_size) {
        lines_offset = data_size;
    }

    const uint8_t *wasm_data = data + wasm_offset;
    size_t wasm_size = (lines_offset > wasm_offset ? lines_offset : data_size) - wasm_offset;

    // Verify WASM magic number
    if (wasm_size < 8 || wasm_data[0] != 0x00 || wasm_data[1] != 0x61
        || wasm_data[2] != 0x73 || wasm_data[3] != 0x6D) {
        return NULL;
    }

    // Allocate the header
    struct JITWasmHeader *header = malloc(sizeof(struct JITWasmHeader));
    if (header == NULL) {
        return NULL;
    }

    // Copy WASM binary to shared memory (source may be on Erlang heap / GC'd)
    header->wasm_binary = malloc(wasm_size);
    if (header->wasm_binary == NULL) {
        free(header);
        return NULL;
    }
    memcpy(header->wasm_binary, wasm_data, wasm_size);
    header->wasm_binary_size = wasm_size;
    header->num_entries = num_entries;

    // Copy lines data
    header->lines_metadata = NULL;
    if (lines_offset > 0 && lines_offset < data_size) {
        size_t lines_data_size = data_size - lines_offset;
        header->lines_metadata = malloc(lines_data_size);
        if (header->lines_metadata != NULL) {
            memcpy(header->lines_metadata, data + lines_offset, lines_data_size);
        }
    }

    // Allocate: 1 extra slot for header pointer + num_entries func_table slots
    ModuleNativeEntryPoint *block = calloc(num_entries + 1, sizeof(ModuleNativeEntryPoint));
    if (block == NULL) {
        free(header->wasm_binary);
        free(header->lines_metadata);
        free(header);
        return NULL;
    }

    block[0] = (ModuleNativeEntryPoint) (uintptr_t) header;

    // block[0] stores the header pointer so we can retrieve it from native_code
    ModuleNativeEntryPoint *func_table = &block[1];

    // Return func_table as native_code.
    // Per-thread function pointers are obtained via jit_wasm_get_entry_point.
    return (ModuleNativeEntryPoint) func_table;
}

ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    UNUSED(ctx);

    if (!term_is_binary(jit_stream)) {
        return NULL;
    }

    const uint8_t *data = (const uint8_t *) term_binary_data(jit_stream);
    size_t data_size = term_binary_size(jit_stream);

    return compile_wasm_stream(data, data_size);
}

ModuleNativeEntryPoint sys_map_native_code(const uint8_t *native_code, size_t size, size_t offset)
{
    if (offset >= size) {
        return NULL;
    }
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
