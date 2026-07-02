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
 * @file native_call.h
 *
 * @brief Resolution and invocation of BIFs/NIFs by MFA, and the per-context
 * apply/3 target resolution cache, shared by the interpreter and the JIT
 * runtime.
 */

#ifndef _NATIVE_CALL_H_
#define _NATIVE_CALL_H_

#include "atom_table.h"
#include "bif.h"
#include "context.h"
#include "defaultatoms.h"
#include "exportedfunction.h"
#include "globalcontext.h"
#include "module.h"
#include "nifs.h"
#include "term_typedef.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Resolve a BIF/GCBIF/NIF for an MFA.
 *
 * @details This is expensive: it formats the MFA as a string and does
 * string-keyed hash lookups. Hot paths cache the result (the registries are
 * static, so a resolution never changes); see apply_resolve_cached.
 */
static inline const struct ExportedFunction *native_call_resolve(GlobalContext *glb,
    atom_index_t module_name, atom_index_t function_name, int arity)
{
    char mfa[MAX_MFA_NAME_LEN];
    atom_table_write_mfa(glb->atom_table, mfa, sizeof(mfa), module_name, function_name, arity);
    const struct ExportedFunction *exported_bif = bif_registry_get_handler(mfa);
    if (exported_bif) {
        return exported_bif;
    }
    const struct Nif *nif = nifs_get(mfa);
    if (nif) {
        return &nif->base;
    }
    return NULL;
}

/**
 * @brief Whether native_call_invoke supports this resolution and arity.
 */
static inline bool native_call_is_invokable(const struct ExportedFunction *native, unsigned int arity)
{
    switch (native->type) {
        case GCBIFFunctionType:
            return arity >= 1 && arity <= 3;
        case BIFFunctionType:
            return arity <= 2;
        default:
            return true;
    }
}

/**
 * @brief Call a resolved BIF/GCBIF/NIF with arguments from x registers.
 *
 * @details The caller is responsible for processing a trap return value.
 */
static inline term native_call_invoke(Context *ctx, const struct ExportedFunction *native,
    unsigned int arity)
{
    if (native->type == GCBIFFunctionType) {
        const struct GCBif *gcbif = EXPORTED_FUNCTION_TO_GCBIF(native);
        switch (arity) {
            case 1:
                return gcbif->gcbif1_ptr(ctx, 0, 0, ctx->x[0]);
            case 2:
                return gcbif->gcbif2_ptr(ctx, 0, 0, ctx->x[0], ctx->x[1]);
            default:
                return gcbif->gcbif3_ptr(ctx, 0, 0, ctx->x[0], ctx->x[1], ctx->x[2]);
        }
    } else if (native->type == NIFFunctionType) {
        const struct Nif *nif = EXPORTED_FUNCTION_TO_NIF(native);
        ctx->nif_call_arity = arity;
        term return_value = nif->nif_ptr(ctx, arity, ctx->x);
        ctx->nif_call_arity = 0;
        return return_value;
    } else {
        const struct Bif *bif = EXPORTED_FUNCTION_TO_BIF(native);
        switch (arity) {
            case 0:
                return bif->bif0_ptr(ctx);
            case 1:
                return bif->bif1_ptr(ctx, 0, ctx->x[0]);
            default:
                return bif->bif2_ptr(ctx, 0, ctx->x[0], ctx->x[1]);
        }
    }
}

/**
 * @brief Resolve and call a BIF/GCBIF/NIF, without caching.
 *
 * @return \c true if the MFA resolved to a native function that was called.
 */
static inline bool maybe_call_native(Context *ctx, atom_index_t module_name,
    atom_index_t function_name, unsigned int arity, term *return_value)
{
    const struct ExportedFunction *native = native_call_resolve(ctx->global, module_name, function_name, arity);
    if (native && native_call_is_invokable(native, arity)) {
        *return_value = native_call_invoke(ctx, native, arity);
        return true;
    }
    return false;
}

// Per-scheduler caches for send-to-registered-name and apply/3 target
// resolution. Both cache global truths (name -> pid, MFA -> target), so
// sharing one cache between the processes a scheduler executes is
// semantically free: entries are validated against the registered processes
// version (bumped on register/unregister) and against loaded_modules_count
// (any module load or upgrade); native resolutions come from static
// registries and stay valid forever. The struct lives on the scheduler
// loop's stack: the interpreter uses it directly and JIT primitives reach
// it through jit_state->caches, so processes carry no cache state at all.
struct SchedulerCaches
{
    // registered-name send cache, valid while regname_version matches the
    // global registered_processes_version
    uint32_t regname_version;
    int regname_atom_index;
    term regname_pid;
    // apply/3 target resolution cache, keyed by (module, function, arity)
    uint64_t apply_key;
    uint8_t apply_kind; // enum ApplyResolution; ApplyResolutionFailed = empty
    int apply_modules_count;
    const struct ExportedFunction *apply_fn;
    Module *apply_module;
    int apply_label;
};

/**
 * @brief Look up a registered process, using the scheduler's name cache.
 *
 * @details Sends to a registered name are common (e.g. gen_server calls)
 * and the uncached lookup takes a global read lock.
 *
 * @param glb the global context.
 * @param caches the executing scheduler's caches.
 * @param atom_index the registered name.
 * @return the pid or port term, or UNDEFINED_ATOM if the name is not
 * registered.
 */
static inline term get_registered_process_cached(GlobalContext *glb, struct SchedulerCaches *caches, int atom_index)
{
    uint32_t regname_version = glb->registered_processes_version;
    if (caches->regname_version == regname_version
        && caches->regname_atom_index == atom_index) {
        return caches->regname_pid;
    }
    term result = globalcontext_get_registered_process(glb, atom_index);
    if (result != UNDEFINED_ATOM) {
        caches->regname_version = regname_version;
        caches->regname_atom_index = atom_index;
        caches->regname_pid = result;
    }
    return result;
}

enum ApplyResolution
{
    ApplyResolutionFailed = 0,
    ApplyResolvedNative,
    ApplyResolvedModule,
};

#define APPLY_CACHE_KEY_MAX_ATOM_INDEX (((uint64_t) 1) << 28)
#define APPLY_CACHE_KEY(module_name, function_name, arity) \
    ((((uint64_t) (module_name)) << 36) | (((uint64_t) (function_name)) << 8) | (uint64_t) (arity))

/**
 * @brief Resolve an apply/3 target, using and filling the scheduler's cache.
 *
 * @details On ApplyResolvedNative, \c *native is set and is invokable with
 * this arity; on ApplyResolvedModule, \c *target_module and \c *target_label
 * are set. ApplyResolutionFailed means the target does not exist (undef).
 */
static inline enum ApplyResolution apply_resolve_cached(Context *ctx, struct SchedulerCaches *caches, atom_index_t module_name,
    atom_index_t function_name, unsigned int arity, const struct ExportedFunction **native,
    Module **target_module, int *target_label)
{
    bool cacheable = module_name < APPLY_CACHE_KEY_MAX_ATOM_INDEX
        && function_name < APPLY_CACHE_KEY_MAX_ATOM_INDEX && arity < 256;
    uint64_t key = 0;
    if (LIKELY(cacheable)) {
        key = APPLY_CACHE_KEY(module_name, function_name, arity);
        if (caches->apply_key == key) {
            if (caches->apply_kind == ApplyResolvedNative) {
                *native = (const struct ExportedFunction *) caches->apply_fn;
                return ApplyResolvedNative;
            }
            if (caches->apply_kind == ApplyResolvedModule
                && caches->apply_modules_count == ctx->global->loaded_modules_count) {
                *target_module = caches->apply_module;
                *target_label = caches->apply_label;
                return ApplyResolvedModule;
            }
        }
    }

    const struct ExportedFunction *resolved
        = native_call_resolve(ctx->global, module_name, function_name, arity);
    if (resolved && native_call_is_invokable(resolved, arity)) {
        if (cacheable) {
            caches->apply_key = key;
            caches->apply_kind = ApplyResolvedNative;
            caches->apply_fn = resolved;
        }
        *native = resolved;
        return ApplyResolvedNative;
    }

    Module *found_module = globalcontext_get_module(ctx->global, module_name);
    if (IS_NULL_PTR(found_module)) {
        return ApplyResolutionFailed;
    }
    int found_label = module_search_exported_function(found_module, function_name, arity);
    if (found_label == 0) {
        return ApplyResolutionFailed;
    }
    if (cacheable) {
        caches->apply_key = key;
        caches->apply_kind = ApplyResolvedModule;
        caches->apply_modules_count = ctx->global->loaded_modules_count;
        caches->apply_module = found_module;
        caches->apply_label = found_label;
    }
    *target_module = found_module;
    *target_label = found_label;
    return ApplyResolvedModule;
}

#ifdef __cplusplus
}
#endif

#endif
