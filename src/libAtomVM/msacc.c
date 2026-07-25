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

#include "msacc.h"

#include <stdlib.h>

#include "context.h"
#include "defaultatoms.h"
#include "exportedfunction.h"
#include "globalcontext.h"
#include "module.h"
#include "synclist.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

#define INITIAL_HOTSPOT_CAPACITY 32

// Set once per scheduler OS thread by msacc_thread_init; lets memory.c and
// scheduler.c reach "this thread's" accounting without a new parameter on
// memory_ensure_free_with_roots/scheduler_run0 and every one of their
// callers.
static _Thread_local struct MsaccInfo *t_msacc_current = NULL;

struct MsaccInfo *msacc_thread_init(GlobalContext *global)
{
    struct MsaccInfo *info = malloc(sizeof(struct MsaccInfo));
    if (IS_NULL_PTR(info)) {
        AVM_ABORT();
    }
    info->state = MsaccStateScheduler;
    info->last_transition_ns = sys_monotonic_time_u64();
    for (int i = 0; i < MsaccStateCount; i++) {
        info->bucket_ns[i] = 0;
    }
    info->hotspots = NULL;
    info->hotspot_count = 0;
    info->hotspot_capacity = 0;

    synclist_append(&global->msacc_info_list, &info->list_head);
    t_msacc_current = info;

    return info;
}

void msacc_transition_current(GlobalContext *global, enum MsaccState new_state)
{
    if (!global->msacc_enabled || t_msacc_current == NULL) {
        return;
    }
    msacc_transition(global, t_msacc_current, new_state);
}

void msacc_transition(GlobalContext *global, struct MsaccInfo *info, enum MsaccState new_state)
{
    if (!global->msacc_enabled) {
        return;
    }
    uint64_t now = sys_monotonic_time_u64();
    info->bucket_ns[info->state] += now - info->last_transition_ns;
    info->state = new_state;
    info->last_transition_ns = now;
}

static void record_hotspot(struct MsaccInfo *info, Module *module, const void *position, bool is_native, uint32_t line)
{
    for (int i = 0; i < info->hotspot_count; i++) {
        struct MsaccHotspot *h = &info->hotspots[i];
        if (h->module == module && h->position == position && h->is_native == is_native && h->line == line) {
            h->count++;
            return;
        }
    }
    if (info->hotspot_count == info->hotspot_capacity) {
        int new_capacity = info->hotspot_capacity == 0 ? INITIAL_HOTSPOT_CAPACITY : info->hotspot_capacity * 2;
        struct MsaccHotspot *new_hotspots
            = realloc(info->hotspots, new_capacity * sizeof(struct MsaccHotspot));
        if (IS_NULL_PTR(new_hotspots)) {
            // Out of memory for a diagnostic feature: drop the sample rather
            // than aborting the VM.
            return;
        }
        info->hotspots = new_hotspots;
        info->hotspot_capacity = new_capacity;
    }
    struct MsaccHotspot *h = &info->hotspots[info->hotspot_count++];
    h->module = module;
    h->position = position;
    h->is_native = is_native;
    h->line = line;
    h->count = 1;
}

void msacc_sample_and_transition(GlobalContext *global, struct MsaccInfo *info, Context *ctx)
{
    if (!global->msacc_enabled) {
        return;
    }
    Module *module = ctx->saved_module;
#if !defined(AVM_NO_JIT) && !defined(AVM_NO_EMU)
    bool is_native = module->native_code != NULL;
    const void *position = is_native ? (const void *) ctx->saved_function_ptr : ctx->saved_ip;
#elif defined(AVM_NO_EMU)
    bool is_native = true;
    const void *position = (const void *) ctx->saved_function_ptr;
#else
    bool is_native = false;
    const void *position = ctx->saved_ip;
#endif
    record_hotspot(info, module, position, is_native, ctx->current_line);
    msacc_transition(global, info, MsaccStateScheduler);
}

void msacc_sample_position_and_transition(GlobalContext *global, struct MsaccInfo *info,
    Module *module, const void *position, uint32_t line)
{
    if (!global->msacc_enabled) {
        return;
    }
    // Reached only when the JIT native-call reduction check fires, so the
    // executing module is necessarily native.
    record_hotspot(info, module, position, true, line);
    msacc_transition(global, info, MsaccStateScheduler);
}

// Labels are allocated (and their code emitted) in increasing address order
// within a module (module_get_function_from_label relies on the same
// property), so for INTERPRETED code a position is resolved to the
// highest-addressed label at or before it: module->labels[label] is the
// real bytecode address control jumps to.
//
// NOT usable for native (JIT-compiled) code: module_get_native_entry_point
// returns the address of a jump-table trampoline SLOT (fixed-size, laid out
// sequentially by label number for O(1) indexing), not the address of the
// label's actual compiled code -- the trampoline's own branch instruction
// carries that, which nothing exposes for a reverse address->label lookup.
// A sampled native PC (from deep inside the real code, past the whole
// table) therefore always compares >= every trampoline slot, so this would
// always "resolve" to the last label -- worse than reporting nothing.
// Native-code hotspots fall back to module-level attribution (see
// make_hotspot_list); resolving them to a function would need the JIT
// compiler to publish real per-label code ranges, which it does not today.
static int resolve_label(Module *module, const void *position, bool is_native)
{
    if (is_native) {
        return -1;
    }
    // code->labels is a big-endian field straight from the .beam Code chunk
    // (see module.c's own ENDIAN_SWAP_32(mod->code->labels) uses); reading
    // it raw on a little-endian host reverses the bytes into a huge count,
    // running this loop far past module->labels[]'s real bound.
    uint32_t labels_count = ENDIAN_SWAP_32(module->code->labels);
    int best_label = -1;
    uintptr_t best_addr = 0;
    uintptr_t target = (uintptr_t) position;
    for (uint32_t label = 1; label < labels_count; label++) {
        uintptr_t label_addr = (uintptr_t) module->labels[label];
        if (label_addr != 0 && label_addr <= target && label_addr > best_addr) {
            best_addr = label_addr;
            best_label = (int) label;
        }
    }
    return best_label;
}

// Every make_*_list / build step below assumes the caller already reserved
// the exact total heap the whole report needs in one memory_ensure_free
// call (see msacc_stop_and_report): none of them may trigger a GC while a
// partially-built list is live and unrooted.

#define STATE_ENTRY_SIZE (TUPLE_SIZE(2) + CONS_SIZE)
#define HOTSPOT_ENTRY_SIZE (TUPLE_SIZE(4) + TUPLE_SIZE(2) + CONS_SIZE)
#define SCHEDULER_ENTRY_SIZE (TUPLE_SIZE(3) + CONS_SIZE)

static term make_state_list(Context *ctx, struct MsaccInfo *info)
{
    term result = term_nil();
    for (int i = MsaccStateCount - 1; i >= 0; i--) {
        term state_name;
        switch ((enum MsaccState) i) {
            case MsaccStateEmulator:
                state_name = globalcontext_make_atom(ctx->global, ATOM_STR("\x8", "emulator"));
                break;
            case MsaccStateGC:
                state_name = globalcontext_make_atom(ctx->global, ATOM_STR("\x2", "gc"));
                break;
            case MsaccStateScheduler:
                state_name = globalcontext_make_atom(ctx->global, ATOM_STR("\x9", "scheduler"));
                break;
            case MsaccStateSleep:
            default:
                state_name = globalcontext_make_atom(ctx->global, ATOM_STR("\x5", "sleep"));
                break;
        }
        term pair = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(pair, 0, state_name);
        term_put_tuple_element(pair, 1, term_from_int((avm_int_t) info->bucket_ns[i]));
        result = term_list_prepend(pair, result, &ctx->heap);
    }
    return result;
}

// Native (JIT-compiled) samples with no line info (line == 0: not built with
// JIT_LINE_PROFILING, see jit_aarch64.erl:track_line/2) cannot be resolved
// past module granularity (see resolve_label); merge every such sample for
// the same module into one entry instead of emitting {Module, undefined,
// -1, 0} once per distinct raw position, which would otherwise force the
// caller to do this exact summation by hand to get a real per-module total.
// Samples that DO carry a line (JIT_LINE_PROFILING built in and it fired)
// are kept separate: no function name, but a real per-line breakdown, which
// is more useful than a module-wide sum. mutates hotspots in place (already
// private per-scheduler, consumed only by this report).
static void merge_native_samples_by_module(struct MsaccInfo *info)
{
    int write = 0;
    for (int read = 0; read < info->hotspot_count; read++) {
        struct MsaccHotspot *h = &info->hotspots[read];
        if (!h->is_native || h->line != 0) {
            info->hotspots[write++] = *h;
            continue;
        }
        bool merged = false;
        for (int j = 0; j < write; j++) {
            struct MsaccHotspot *prior = &info->hotspots[j];
            if (prior->is_native && prior->line == 0 && prior->module == h->module) {
                prior->count += h->count;
                merged = true;
                break;
            }
        }
        if (!merged) {
            info->hotspots[write++] = *h;
        }
    }
    info->hotspot_count = write;
}

static term make_hotspot_list(Context *ctx, struct MsaccInfo *info)
{
    merge_native_samples_by_module(info);
    term result = term_nil();
    for (int i = 0; i < info->hotspot_count; i++) {
        struct MsaccHotspot *h = &info->hotspots[i];
        int label = resolve_label(h->module, h->position, h->is_native);
        atom_index_t function_name_index;
        int arity = -1;
        bool resolved = label >= 0 && module_get_function_from_label(h->module, label, &function_name_index, &arity);

        // h->line is the raw OP_LINE/OP_EXECUTABLE_LINE operand: an index
        // into the module's Line chunk, not a source line number itself
        // (see module_resolve_line_ref). Resolve it here, once per report,
        // rather than at every track_line/OP_LINE site.
        uint32_t resolved_line = 0;
        if (h->line != 0) {
            module_resolve_line_ref(h->module, (uint16_t) h->line, &resolved_line);
        }

        term module_name_term = module_get_name(h->module);
        term function_name_term = resolved ? term_from_atom_index(function_name_index) : UNDEFINED_ATOM;
        term arity_term = resolved ? term_from_int(arity) : term_from_int(-1);
        term line_term = term_from_int((avm_int_t) resolved_line);

        term mfa = term_alloc_tuple(4, &ctx->heap);
        term_put_tuple_element(mfa, 0, module_name_term);
        term_put_tuple_element(mfa, 1, function_name_term);
        term_put_tuple_element(mfa, 2, arity_term);
        term_put_tuple_element(mfa, 3, line_term);

        term pair = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(pair, 0, mfa);
        term_put_tuple_element(pair, 1, term_from_int((avm_int_t) h->count));

        result = term_list_prepend(pair, result, &ctx->heap);
    }
    return result;
}

term msacc_start(GlobalContext *global)
{
    struct ListHead *item;
    struct ListHead *list = synclist_wrlock(&global->msacc_info_list);
    LIST_FOR_EACH (item, list) {
        struct MsaccInfo *info = GET_LIST_ENTRY(item, struct MsaccInfo, list_head);
        for (int i = 0; i < MsaccStateCount; i++) {
            info->bucket_ns[i] = 0;
        }
        info->hotspot_count = 0;
        info->last_transition_ns = sys_monotonic_time_u64();
    }
    synclist_unlock(&global->msacc_info_list);

    global->msacc_enabled = true;

    return OK_ATOM;
}

term msacc_stop_and_report(Context *ctx)
{
    GlobalContext *global = ctx->global;
    global->msacc_enabled = false;

    // Held across both passes: sizing and building must see the same
    // hotspot_count per scheduler, and no scheduler thread may be added
    // (msacc_thread_init also takes this lock) while we compute the total.
    struct ListHead *item;
    struct ListHead *list = synclist_wrlock(&global->msacc_info_list);

    size_t total_size = 0;
    LIST_FOR_EACH (item, list) {
        struct MsaccInfo *info = GET_LIST_ENTRY(item, struct MsaccInfo, list_head);
        total_size += MsaccStateCount * STATE_ENTRY_SIZE;
        total_size += (size_t) info->hotspot_count * HOTSPOT_ENTRY_SIZE;
        total_size += SCHEDULER_ENTRY_SIZE;
    }

    // Reserve the whole report's heap in one shot: every allocation below
    // is then guaranteed not to trigger a GC, so none of the
    // partially-built lists/tuples need to be passed as roots.
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, total_size, 0, NULL, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        synclist_unlock(&global->msacc_info_list);
        return term_nil();
    }

    term result = term_nil();
    int scheduler_index = 0;
    LIST_FOR_EACH (item, list) {
        struct MsaccInfo *info = GET_LIST_ENTRY(item, struct MsaccInfo, list_head);

        term states = make_state_list(ctx, info);
        term hotspots = make_hotspot_list(ctx, info);

        term entry = term_alloc_tuple(3, &ctx->heap);
        term_put_tuple_element(entry, 0, term_from_int(scheduler_index));
        term_put_tuple_element(entry, 1, states);
        term_put_tuple_element(entry, 2, hotspots);

        result = term_list_prepend(entry, result, &ctx->heap);
        scheduler_index++;
    }
    synclist_unlock(&global->msacc_info_list);

    return result;
}

static term nif_atomvm_profile_start(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return msacc_start(ctx->global);
}

const struct Nif atomvm_profile_start_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_profile_start
};

static term nif_atomvm_profile_stop(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    UNUSED(argv);
    return msacc_stop_and_report(ctx);
}

const struct Nif atomvm_profile_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_atomvm_profile_stop
};
