/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 by Paul Guyot <pguyot@kallisys.net>
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

#ifndef AVM_NO_JIT

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "jit.h"
#include "nifs.h"
#include "platform_defaultatoms.h"
#include "term.h"

#include <emscripten.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static ErlNifResourceType *jit_stream_wasm_resource_type;
static void jit_stream_wasm_dtor(ErlNifEnv *caller_env, void *obj);

const ErlNifResourceTypeInit jit_stream_wasm_resource_type_init = {
    .members = 1,
    .dtor = jit_stream_wasm_dtor
};

struct JITStreamWASM
{
    uint8_t *stream_data;    // Accumulated WASM code
    size_t stream_offset;    // Current write position
    size_t stream_size;      // Total allocated size
    void *wasm_instance;     // Compiled WASM instance (NULL until finalized)
};

static term nif_jit_stream_wasm_new(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    VALIDATE_VALUE(argv[0], term_is_integer);

    size_t size = term_to_int(argv[0]);

    // Allocate buffer for WASM binary
    uint8_t *buffer = (uint8_t *) malloc(size);
    if (IS_NULL_PTR(buffer)) {
        fprintf(stderr, "Could not allocate buffer for WASM JIT\n");
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    // Create resource object
    struct JITStreamWASM *js = enif_alloc_resource(jit_stream_wasm_resource_type, sizeof(struct JITStreamWASM));
    if (IS_NULL_PTR(js)) {
        free(buffer);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    js->stream_data = buffer;
    js->stream_offset = 0;
    js->stream_size = size;
    js->wasm_instance = NULL;

    term obj = enif_make_resource(erl_nif_env_from_context(ctx), js);
    enif_release_resource(js);
    return obj;
}

static term nif_jit_stream_wasm_offset(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_wasm_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamWASM *js_obj = (struct JITStreamWASM *) js_obj_ptr;

    return term_from_int(js_obj->stream_offset);
}

static term nif_jit_stream_wasm_append(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_binary);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_wasm_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamWASM *js_obj = (struct JITStreamWASM *) js_obj_ptr;

    size_t binary_size = term_binary_size(argv[1]);
    const uint8_t *binary_data = (const uint8_t *) term_binary_data(argv[1]);

    if (UNLIKELY(js_obj->stream_offset + binary_size > js_obj->stream_size)) {
        fprintf(stderr, "WASM JIT buffer overflow: offset=%zu size=%zu capacity=%zu\n",
                js_obj->stream_offset, binary_size, js_obj->stream_size);
        RAISE_ERROR(BADARG_ATOM);
    }

    memcpy(js_obj->stream_data + js_obj->stream_offset, binary_data, binary_size);
    js_obj->stream_offset += binary_size;

    return argv[0];
}

static term nif_jit_stream_wasm_replace(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_binary);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_wasm_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamWASM *js_obj = (struct JITStreamWASM *) js_obj_ptr;

    size_t binary_size = term_binary_size(argv[2]);
    const uint8_t *binary_data = (const uint8_t *) term_binary_data(argv[2]);
    int offset = term_to_int(argv[1]);

    if (UNLIKELY(offset < 0 || (size_t)(binary_size + offset) > js_obj->stream_offset)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    memcpy(js_obj->stream_data + offset, binary_data, binary_size);

    return argv[0];
}

static term nif_jit_stream_wasm_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[1], term_is_integer);
    VALIDATE_VALUE(argv[2], term_is_integer);
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), argv[0], jit_stream_wasm_resource_type, &js_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct JITStreamWASM *js_obj = (struct JITStreamWASM *) js_obj_ptr;

    int offset = term_to_int(argv[1]);
    int len = term_to_int(argv[2]);

    if (UNLIKELY(memory_ensure_free_opt(ctx, TERM_BINARY_HEAP_SIZE(len), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    if (UNLIKELY(len <= 0 || offset < 0 || (size_t) (offset + len) > js_obj->stream_offset)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return term_from_literal_binary(js_obj->stream_data + offset, len, &ctx->heap, ctx->global);
}

static term nif_jit_stream_module(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    UNUSED(ctx);

    return JIT_STREAM_WASM_ATOM;
}

static const struct Nif jit_stream_module_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_module
};
static const struct Nif jit_stream_wasm_new_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_wasm_new
};
static const struct Nif jit_stream_wasm_offset_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_wasm_offset
};
static const struct Nif jit_stream_wasm_append_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_wasm_append
};
static const struct Nif jit_stream_wasm_replace_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_wasm_replace
};
static const struct Nif jit_stream_wasm_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_jit_stream_wasm_read
};

// Compile WASM and get entry point
// Note: For WASM, this is more complex than native platforms
// We need to instantiate the WASM module and get function references
ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    void *js_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), jit_stream, jit_stream_wasm_resource_type, &js_obj_ptr))) {
        fprintf(stderr, "jit_stream_entry_point: invalid resource\n");
        return NULL;
    }
    struct JITStreamWASM *js_obj = (struct JITStreamWASM *) js_obj_ptr;

    if (IS_NULL_PTR(js_obj->stream_data)) {
        fprintf(stderr, "jit_stream_entry_point: no WASM data\n");
        return NULL;
    }

    // TODO: Compile WASM module using WebAssembly.instantiate
    // For now, return NULL to indicate WASM JIT is not yet fully implemented
    fprintf(stderr, "WASM JIT: Compiled %zu bytes of WASM code (execution not yet implemented)\n",
            js_obj->stream_offset);

    // Placeholder: In the future, we'll:
    // 1. Call EM_ASM to instantiate the WASM module
    // 2. Get function table references
    // 3. Return entry point function pointer
    return NULL;
}

static void jit_stream_wasm_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct JITStreamWASM *js_obj = (struct JITStreamWASM *) obj;
    if (js_obj->stream_data) {
        free(js_obj->stream_data);
        js_obj->stream_data = NULL;
    }
    if (js_obj->wasm_instance) {
        // TODO: Free WASM instance
        js_obj->wasm_instance = NULL;
    }
}

//
// Entrypoints
//

const struct Nif *jit_stream_wasm_get_nif(const char *nifname)
{
    if (strcmp("jit:stream_module/0", nifname) == 0) {
        return &jit_stream_module_nif;
    }
    if (strncmp("jit_stream_wasm:", nifname, 16) == 0) {
        const char *rest = nifname + 16;
        if (strcmp("new/1", rest) == 0) {
            return &jit_stream_wasm_new_nif;
        }
        if (strcmp("offset/1", rest) == 0) {
            return &jit_stream_wasm_offset_nif;
        }
        if (strcmp("append/2", rest) == 0) {
            return &jit_stream_wasm_append_nif;
        }
        if (strcmp("replace/3", rest) == 0) {
            return &jit_stream_wasm_replace_nif;
        }
        if (strcmp("read/3", rest) == 0) {
            return &jit_stream_wasm_read_nif;
        }
    }
    return NULL;
}

void jit_stream_wasm_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    jit_stream_wasm_resource_type = enif_init_resource_type(&env, "jit_stream_wasm", &jit_stream_wasm_resource_type_init, ERL_NIF_RT_CREATE, NULL);
}

#endif
