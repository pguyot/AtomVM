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
 * @file msacc.h
 * @brief Microstate accounting and reduction-sampled hotness profiling.
 *
 * @details Mirrors BEAM's `erlang:statistics(microstate_accounting)`: each
 * scheduler thread's time is bucketed into a small set of mutually exclusive
 * states (running BEAM code, garbage collecting, scheduler bookkeeping,
 * blocked in the I/O poller), using the existing sys_monotonic_time_u64
 * clock. Overhead when disabled is a single branch per transition.
 *
 * A second, coarser instrument piggybacks on the same transitions: instead
 * of a real-time sampling timer (generic_unix has none; embedded targets
 * would need one per platform), every point where a process stops running
 * (reduction budget exhausted, yield, wait) is a natural "tick" -- the
 * process's saved position (module + bytecode offset, or module + native
 * code address under the JIT) is recorded, deferring the expensive
 * position -> {module, function, arity} resolution to report time, when it
 * happens once per distinct position rather than once per sample.
 */

#ifndef _MSACC_H_
#define _MSACC_H_

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct GlobalContext;

// Mirrors BEAM's erlang:statistics(microstate_accounting) bucket set.
// Declared unconditionally (unlike everything else in this header): callers
// of msacc_transition_current (memory.c, scheduler.c) pass these values at
// their call sites regardless of whether AVM_ENABLE_MSACC is on, so the enum
// must exist either way -- only the accounting behind it is compiled out.
enum MsaccState
{
    MsaccStateEmulator = 0,
    MsaccStateGC,
    MsaccStateScheduler,
    MsaccStateSleep,
    MsaccStateCount // must be last: also the bucket array size
};

#ifdef AVM_ENABLE_MSACC

#include "list.h"
#include "term.h"

struct Nif;
struct Context;
struct Module;

// One distinct code position a process was found at when it stopped
// running, plus how many times sampling landed there. line is the source
// line ctx->current_line held at sample time: always meaningful for
// interpreted code, meaningful for native code only when compiled with
// JIT_LINE_PROFILING (see jit_aarch64.erl:track_line/2), 0 otherwise.
struct MsaccHotspot
{
    struct Module *module;
    const void *position;
    bool is_native;
    uint32_t line;
    uint32_t count;
};

// Per-scheduler-thread accounting. Allocated once when the scheduler thread
// starts (msacc_thread_init) and never freed until VM shutdown: a report
// may run concurrently with live scheduler threads, so reads during an
// active profiling window are approximate (like BEAM's msacc), never a use
// after free.
struct MsaccInfo
{
    struct ListHead list_head;
    enum MsaccState state;
    uint64_t last_transition_ns;
    uint64_t bucket_ns[MsaccStateCount];
    struct MsaccHotspot *hotspots;
    int hotspot_count;
    int hotspot_capacity;
};

/**
 * @brief Register this scheduler thread's accounting block.
 * @details Called once from scheduler_entry_point. The returned pointer is
 * valid for the process lifetime and is what every other msacc_* call in
 * this thread should use.
 */
struct MsaccInfo *msacc_thread_init(struct GlobalContext *global);

/**
 * @brief Transition this scheduler thread to a new accounting state.
 * @details A no-op (single atomic load + branch) when profiling is not
 * enabled globally.
 */
void msacc_transition(struct GlobalContext *global, struct MsaccInfo *info, enum MsaccState new_state);

/**
 * @brief Same as msacc_transition, but for callers (memory.c, scheduler.c)
 * that do not have this thread's MsaccInfo handy: threading it through
 * memory_ensure_free_with_roots' many call sites (bif.c, nifs.c, every
 * interpreter op and JIT primitive) just for a diagnostic feature is not
 * worth it, so this reads it from thread-local storage instead (set once by
 * msacc_thread_init). A no-op if this thread never called msacc_thread_init
 * (e.g. the main thread doing setup before any scheduler starts) or
 * profiling is not enabled.
 */
void msacc_transition_current(struct GlobalContext *global, enum MsaccState new_state);

/**
 * @brief Record the position a process was at when it stopped running, and
 * transition this scheduler thread to MsaccStateScheduler.
 * @details Called from the SCHEDULE_NEXT/SCHEDULE_WAIT/SCHEDULE_WAIT_ANY
 * macros, with ctx still referring to the process that is yielding (its
 * saved_module/saved_ip or saved_function_ptr already updated by the
 * caller). A no-op when profiling is not enabled globally.
 */
void msacc_sample_and_transition(struct GlobalContext *global, struct MsaccInfo *info, struct Context *ctx);

/**
 * @brief Same as msacc_sample_and_transition, but for the JIT native-call
 * reduction-exhaustion check in opcodesswitch.h, which does not go through
 * SCHEDULE_NEXT: ctx->saved_module/saved_function_ptr are not necessarily
 * current there (they are the interpreter's own resumption bookkeeping),
 * but the caller already has the right position in hand as jit_state's
 * module/continuation_pc. line should be ctx->current_line: unlike
 * saved_module/saved_function_ptr, JIT-compiled code (when built with
 * JIT_LINE_PROFILING) keeps it current continuously, not just at yield
 * points, so it is safe to read here even though saved_module is not.
 */
void msacc_sample_position_and_transition(struct GlobalContext *global, struct MsaccInfo *info,
    struct Module *module, const void *position, uint32_t line);

/**
 * @brief atomvm:profile_start/0: enable microstate accounting and hotness
 * sampling globally, resetting all counters.
 */
term msacc_start(struct GlobalContext *global);

/**
 * @brief atomvm:profile_stop/0: disable microstate accounting and hotness
 * sampling, returning a term describing the collected stats (built on ctx's
 * heap).
 */
term msacc_stop_and_report(struct Context *ctx);

extern const struct Nif atomvm_profile_start_nif;
extern const struct Nif atomvm_profile_stop_nif;

#else

#include "utils.h"

// Profiling not built in (see AVM_ENABLE_MSACC in CMakeLists.txt): every
// msacc_transition_current call site (memory.c, scheduler.c) becomes a no-op
// with no footprint in Context/GlobalContext.
static inline void msacc_transition_current(struct GlobalContext *global, enum MsaccState new_state)
{
    UNUSED(global);
    UNUSED(new_state);
}

#endif

#ifdef __cplusplus
}
#endif

#endif
