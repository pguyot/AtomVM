/*
 * This file is part of AtomVM.
 *
 * Copyright 2025 Paul Guyot <pguyot@kallisys.net>
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

#ifndef _JIT_H_
#define _JIT_H_

#include "bitstring.h"
#include "exportedfunction.h"
#include "term.h"
#include "term_typedef.h"

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

struct Context;

#ifndef TYPEDEF_CONTEXT
#define TYPEDEF_CONTEXT
typedef struct Context Context;
#endif

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

#ifndef TYPEDEF_JITSTATE
#define TYPEDEF_JITSTATE
typedef struct JITState JITState;
#endif

#ifndef TYPEDEF_MODULENATIVEINTERFACE
#define TYPEDEF_MODULENATIVEINTERFACE
typedef struct ModuleNativeInterface ModuleNativeInterface;
#endif

struct Module;

#ifndef TYPEDEF_MODULE
#define TYPEDEF_MODULE
typedef struct Module Module;
#endif

// Numeric architecture identifiers. These must be defined before the
// JIT_ARCH_TARGET assignments below so that `JIT_ARCH_TARGET == JIT_ARCH_*`
// comparisons (including those used in struct JITState) resolve correctly
// rather than against undefined identifiers.
#define JIT_ARCH_X86_64 1
#define JIT_ARCH_AARCH64 2
#define JIT_ARCH_ARMV6M 3
#define JIT_ARCH_RISCV32 4
#define JIT_ARCH_RISCV64 5
#define JIT_ARCH_ARM32 6
#define JIT_ARCH_WASM32 7
#define JIT_ARCH_XTENSA 8

#ifndef AVM_NO_JIT

#ifdef __x86_64__
#define JIT_ARCH_TARGET JIT_ARCH_X86_64
#define JIT_JUMPTABLE_ENTRY_SIZE 5
#define JIT_JUMPTABLE_OFFSET 0
// Pinned-register convention (see the aarch64 block below for the general
// contract): jit_state in r13, the primitives table in rbx, ctx in r14,
// ctx->heap.heap_ptr in r12 and ctx->e in r15 — all callee-saved under the
// SysV ABI. Base-register assignment follows ModRM cost (rbx/r14/r15 are
// plain-ModRM bases; r13 forces a disp8 at offset 0; r12 would need a SIB
// byte but hp is never used as a base).
#define JIT_PINNED_JIT_STATE 1
#define JIT_PINNED_JIT_STATE_REG "r13"
#define JIT_PINNED_CTX 1
#define JIT_PINNED_CTX_REG "r14"
#endif

#if defined(__arm64__) || defined(__aarch64__)
#define JIT_ARCH_TARGET JIT_ARCH_AARCH64
#define JIT_JUMPTABLE_ENTRY_SIZE 4
#define JIT_JUMPTABLE_OFFSET 0
// Pinned-register convention: generated code keeps jit_state in x19, the
// primitives table in x20 and ctx in x21 (callee-saved; seeded once per
// C->native crossing by the dispatch loop). Primitives take neither a
// jit_state nor a ctx parameter: the table-facing entry shims in jit.c read
// the pinned registers. A backend opts in by defining JIT_PINNED_JIT_STATE
// and/or JIT_PINNED_CTX and naming its callee-saved registers here; the
// JSP_/CTXP_ parameter macros and the shim block adapt. See JS_READ/CTX_READ.
#define JIT_PINNED_JIT_STATE 1
#define JIT_PINNED_JIT_STATE_REG "x19"
#define JIT_PINNED_CTX 1
#define JIT_PINNED_CTX_REG "x21"
#endif

#if defined(__arm__) && defined(AVM_JIT_ARM32)
#define JIT_ARCH_TARGET JIT_ARCH_ARM32
#define JIT_JUMPTABLE_ENTRY_SIZE 8
#define JIT_JUMPTABLE_OFFSET 0
// Pinned-register convention (see the aarch64 block for the general
// contract): ctx in r7, jit_state in r10, the primitives table in r9 and
// ctx->e in r8. r11 is deliberately avoided: it is the ARM frame pointer,
// and a register variable bound to it in the jit.c entry shims is undefined
// behaviour. The dispatch loop also takes ownership of saving r4-r6
// (declared as clobbers at the boundary), so generated code has no entry
// prologue frame. There are no inline heap operations on arm32, so hp is
// not pinned and only e follows the write-back/reload protocol.
#define JIT_PINNED_JIT_STATE 1
#define JIT_PINNED_JIT_STATE_REG "r10"
#define JIT_PINNED_CTX 1
#define JIT_PINNED_CTX_REG "r7"
#elif defined(__arm__)
#define JIT_ARCH_TARGET JIT_ARCH_ARMV6M
#ifdef AVM_JIT_THUMB2
#define JIT_JUMPTABLE_ENTRY_SIZE 6
#else
#define JIT_JUMPTABLE_ENTRY_SIZE 12
#endif
#define JIT_JUMPTABLE_OFFSET 0
#endif

#if defined(__riscv) && (__riscv_xlen == 32)
#define JIT_ARCH_TARGET JIT_ARCH_RISCV32
#define JIT_JUMPTABLE_ENTRY_SIZE 8
#define JIT_JUMPTABLE_OFFSET 0
#endif

#if defined(__riscv) && (__riscv_xlen == 64)
#define JIT_ARCH_TARGET JIT_ARCH_RISCV64
#define JIT_JUMPTABLE_ENTRY_SIZE 8
#define JIT_JUMPTABLE_OFFSET 0
#endif

#if defined(__riscv)
// Pinned-register convention (see the aarch64 block for the general
// contract): ctx in s1, jit_state in s2, the primitives table in s3 and
// ctx->e in s4 — all callee-saved. ctx takes s1, the only RVC-addressable
// callee-saved base besides the frame pointer, so compressed argument
// loads survive. There are no inline heap operations on RISC-V, so hp is
// not pinned and only e follows the write-back/reload protocol.
#define JIT_PINNED_JIT_STATE 1
#define JIT_PINNED_JIT_STATE_REG "s2"
#define JIT_PINNED_CTX 1
#define JIT_PINNED_CTX_REG "s1"
#endif

#ifdef __wasm__
#define JIT_ARCH_TARGET JIT_ARCH_WASM32
#define JIT_JUMPTABLE_ENTRY_SIZE 4
#define JIT_JUMPTABLE_IS_DATA
#define JIT_JUMPTABLE_OFFSET 0
#endif

#ifdef __XTENSA__
#define JIT_ARCH_TARGET JIT_ARCH_XTENSA
#define JIT_JUMPTABLE_ENTRY_SIZE 20
#define JIT_JUMPTABLE_OFFSET 4
#endif

#ifndef JIT_ARCH_TARGET
#error Unknown JIT target
#endif
#endif

// Interface to native code:
// Entry point returns the current (or new) context
// jit_state->remaining_reductions is updated.
// If returned context is different from passed context, scheduler resumes in
// schedule_in.
typedef Context *(*ModuleNativeEntryPoint)(Context *ctx, JITState *jit_state, const ModuleNativeInterface *p);

// Type for storing a native continuation reference.
// On WASM (JIT_JUMPTABLE_IS_DATA), this is a label encoding (label + 1).
// On other architectures, this is the actual function pointer.
#ifdef JIT_JUMPTABLE_IS_DATA
typedef uintptr_t NativeContinuation;
#else
typedef ModuleNativeEntryPoint NativeContinuation;
#endif

struct SchedulerCaches;

struct JITState
{
    Module *module;
    union
    {
        NativeContinuation continuation;
        const void *continuation_pc;
    };
    int remaining_reductions;
    // FP register bank. Seeded by the scheduler loop from its own lazily
    // allocated bank: fr registers are dead at every schedule-out point (the
    // compiler boxes floats across calls and receives), so all processes
    // executed by a scheduler share one bank. NULL until the first float
    // instruction this scheduler executes; a VM with no float code never
    // allocates a bank.
    avm_float_t *fr;
#if JIT_ARCH_TARGET == JIT_ARCH_XTENSA
    const void *code_base;
#endif
    // The executing scheduler's caches (registered-name sends, apply/3
    // resolution), living on the scheduler loop's stack. Only used by C
    // primitives, never by generated code, so its offset is not part of the
    // native code ABI.
    struct SchedulerCaches *caches;
    // module->module_index << 24, kept in sync with the module field (see
    // jit_state_set_module in jit.c and the scheduler-loop initialization).
    // Generated 64-bit code reads it to build and check cp values without
    // the module->index dereference chain; its offset is part of the native
    // code ABI (JITSTATE_CPBASE in jit_aarch64.erl).
    uintptr_t cp_base;
};

// Remember to keep this struct in sync with libs/jit/src/primitives.hrl
// Primitives must have at most 6 parameters, this is what several backends expect

// jit_state / ctx parameters of primitives. Under JIT_PINNED_JIT_STATE (resp.
// JIT_PINNED_CTX) the parameter disappears: generated code does not pass it
// (it is pinned in a callee-saved register) and the table-facing entry shims
// in jit.c read the register instead. JSP_/CTXP_ sit where the parameter
// followed by a comma would be, JSP_ONLY/CTXP_ONLY where it is the sole
// parameter, and CTX_JS_PARAMS covers the exact (ctx, jit_state) shape for
// every combination of the two flags.
#ifdef JIT_PINNED_JIT_STATE
#define JSP_
#define JSP_ONLY void
#else
#define JSP_ JITState *jit_state,
#define JSP_ONLY JITState *jit_state
#endif

#ifdef JIT_PINNED_CTX
#define CTXP_
#define CTXP_ONLY void
#else
#define CTXP_ Context *ctx,
#define CTXP_ONLY Context *ctx
#endif

#if defined(JIT_PINNED_CTX) && defined(JIT_PINNED_JIT_STATE)
#define CTX_JS_PARAMS void
#elif defined(JIT_PINNED_CTX)
#define CTX_JS_PARAMS JITState *jit_state
#elif defined(JIT_PINNED_JIT_STATE)
#define CTX_JS_PARAMS Context *ctx
#else
#define CTX_JS_PARAMS Context *ctx, JITState *jit_state
#endif

struct ModuleNativeInterface
{
    // Helpers
    Context *(*raise_error)(CTXP_ JSP_ int offset, term error_term);
    Context *(*do_return)(CTX_JS_PARAMS);
    Context *(*schedule_next_cp)(CTX_JS_PARAMS);
    term (*module_get_atom_term_by_id)(JSP_ int atom_index);
    Context *(*call_ext)(CTXP_ JSP_ int offset, int arity, int index, int n_words);
    bool (*allocate)(CTXP_ JSP_ uint32_t stack_need, uint32_t heap_need, uint32_t live);
    Context *(*handle_error)(CTXP_ JSP_ int offset);
    void (*jit_trim_live_regs)(CTXP_ uint32_t live);
    BifImpl0 (*get_imported_bif)(JSP_ uint32_t bif);
    bool (*deallocate)(CTXP_ JSP_ uint32_t n_words);
    Context *(*terminate_context)(CTX_JS_PARAMS);
    TermCompareResult (*term_compare)(CTXP_ JSP_ term t, term other, TermCompareOpts opts);
    bool (*test_heap)(CTXP_ JSP_ uint32_t heap_need, uint32_t live);
    term (*put_list)(CTXP_ term head, term tail);
    term (*module_load_literal)(CTXP_ JSP_ int index);
    term (*alloc_boxed_integer_fragment)(CTXP_ avm_int64_t value);
    term (*term_alloc_tuple)(CTXP_ uint32_t size);
    bool (*send)(CTX_JS_PARAMS);
    term *(*extended_register_pointer)(CTXP_ unsigned int index);
    Context *(*raise_error_tuple)(CTXP_ JSP_ int offset, term error_atom, term arg1);
    term (*term_alloc_fun)(CTXP_ JSP_ uint32_t fun_index, uint32_t numfree);
    Context *(*process_signal_messages)(CTX_JS_PARAMS);
    term (*mailbox_peek)(CTXP_ONLY);
    void (*mailbox_remove_message)(CTXP_ONLY);
    void (*timeout)(CTXP_ONLY);
    void (*mailbox_next)(CTXP_ONLY);
    void (*cancel_timeout)(CTXP_ONLY);
    void (*clear_timeout_flag)(CTXP_ONLY);
    Context *(*raise)(CTXP_ JSP_ term stacktrace, term exc_value);
    Context *(*schedule_wait_cp)(CTX_JS_PARAMS);
    Context *(*wait_timeout)(CTXP_ JSP_ term timeout, int label);
    Context *(*wait_timeout_trap_handler)(CTXP_ JSP_ int label);
    Context *(*call_fun)(CTXP_ JSP_ int offset, term fun, unsigned int args_count);
    int (*context_get_flags)(CTXP_ int mask);
    void (*ensure_fpregs)(JSP_ONLY);
    term (*term_from_float)(CTXP_ JSP_ int fpreg);
    bool (*term_is_number)(term t);
    void (*term_conv_to_float)(JSP_ term t, int fpreg);
    bool (*fadd)(JSP_ int fpreg_1, int fpreg_2, int fpreg_3);
    bool (*fsub)(JSP_ int fpreg_1, int fpreg_2, int fpreg_3);
    bool (*fmul)(JSP_ int fpreg_1, int fpreg_2, int fpreg_3);
    bool (*fdiv)(JSP_ int fpreg_1, int fpreg_2, int fpreg_3);
    void (*fnegate)(JSP_ int fpreg_1, int fpreg_2);
    bool (*catch_end)(CTX_JS_PARAMS);
    bool (*memory_ensure_free_with_roots)(CTXP_ JSP_ int sz, int live, int flags);
    term (*term_alloc_bin_match_state)(CTXP_ term src, int slots);
    term (*bitstring_extract_integer)(CTXP_ JSP_ term *bin_ptr, size_t offset, int n, int bs_flags);
    size_t (*term_sub_binary_heap_size)(term *bin_ptr, size_t size);
    term (*term_maybe_create_sub_binary)(CTXP_ term bin, size_t offset, size_t len);
    int (*term_find_map_pos)(CTXP_ term map, term key);
    int (*bitstring_utf8_size)(avm_int_t c);
    int (*bitstring_utf16_size)(avm_int_t c);
    term (*term_create_empty_binary)(CTXP_ size_t len);
    int (*decode_flags_list)(CTXP_ JSP_ term l);
    int (*bitstring_insert_utf8)(term bin, size_t offset, avm_int_t c);
    int (*bitstring_insert_utf16)(term bin, size_t offset, avm_int_t c, enum BitstringFlags flags);
    bool (*bitstring_insert_utf32)(term bin, size_t offset, avm_int_t c, enum BitstringFlags flags);
    bool (*bitstring_insert_integer)(term bin, size_t offset, term value, size_t n, enum BitstringFlags flags);
    void (*bitstring_copy_module_str)(CTXP_ JSP_ term bin, size_t offset, int str_id, size_t len);
    int (*bitstring_copy_binary)(CTXP_ JSP_ term t, size_t offset, term src, term size);
    Context *(*apply)(CTXP_ JSP_ int offset, term module, term function, unsigned int arity);
    void *(*malloc)(CTXP_ JSP_ size_t sz);
    void (*free)(void *ptr);
    term (*put_map_assoc)(CTXP_ JSP_ term src, size_t new_entries, size_t num_elements, term *kv);
    term (*bitstring_extract_float)(CTXP_ JSP_ term *match_state_ptr, int n, int bs_flags, int live);
    int (*module_get_fun_arity)(Module *fun_module, uint32_t fun_index);
    bool (*bitstring_match_module_str)(CTXP_ JSP_ term bin, size_t offset, int str_id, size_t len);
    term (*bitstring_get_utf8)(term src);
    term (*bitstring_get_utf16)(term src, int flags_value);
    term (*bitstring_get_utf32)(term src, int flags_value);
    term (*term_copy_map)(CTXP_ term src);
    term (*stacktrace_build)(CTXP_ONLY);
    term (*term_reuse_binary)(CTXP_ term src, size_t len);
    term (*alloc_big_integer_fragment)(CTXP_ size_t digits_len, term_integer_sign_t sign);
    bool (*bitstring_insert_float)(term bin, size_t offset, term value, size_t n, enum BitstringFlags flags);
    Context *(*raw_raise)(CTX_JS_PARAMS);
    Context *(*raise_error_mfa)(
        CTXP_ JSP_ int offset, int function_atom_index, int arity);
    void (*try_case)(CTXP_ONLY);
    uint32_t (*record_def_arity)(CTXP_ JSP_ term id);
    uint32_t (*record_field_pos)(term src, term field_name);
    term (*put_record)(CTXP_ JSP_ term id, term src, uint32_t num_pairs, term *kv);
    uint32_t (*is_record_of)(term src, term mod_atom, term name_atom);
    uint32_t (*is_record_accessible)(CTXP_ JSP_ term src, term scope);
    term (*get_record_field)(CTXP_ uint32_t fail_label, term src, term id, term field);
    term (*put_record_resolved)(CTXP_ JSP_ uint32_t record_index, term src, uint32_t num_pairs, term *kv);
    BifImpl0 (*get_imported_gcbif)(CTXP_ JSP_ uint32_t live, uint32_t bif);
    void (*set_tuple_element)(CTXP_ term tuple, uint32_t position, term value);
    size_t (*put_map_heap_need)(CTXP_ term src, size_t new_entries, size_t num_elements);
    term (*map_get_value)(CTXP_ term map, int pos);
    term (*term_get_map_assoc)(CTXP_ term map, term key);
    int (*term_get_map_assoc_miss)(CTXP_ term map, term key);
    // OP_CALL_FUN direct dispatch: returns the fun's native entry point with
    // bit 0 set (branch to it), or a Context * with bit 0 clear (return to
    // the scheduler loop with it).
    uintptr_t (*call_fun_direct)(CTXP_ JSP_ int offset, term fun, unsigned int args_count);
    uintptr_t (*call_ext_direct)(CTXP_ JSP_ int offset, int arity, int index, int n_words);
    uintptr_t (*return_direct)(CTX_JS_PARAMS);
    size_t (*bitstring_get_tail_heap_size)(term *bs_bin_ptr, size_t bs_offset);
    term (*bitstring_create_tail)(CTXP_ term bs_bin, size_t bs_offset);
    term (*bs_create_bin_wrap)(CTXP_ term byte_binary, size_t total_bits);
    size_t (*bitstring_slice_heap_size)(term *bs_bin_ptr, size_t offset, size_t len_bits);
    term (*bitstring_slice)(CTXP_ term bs_bin, size_t offset, size_t len_bits);
    bool (*bitstring_is_multiple_of)(size_t bits, size_t unit);
    size_t (*put_map_one_heap_need)(CTXP_ term src);
    term (*put_map_assoc_one)(CTXP_ JSP_ term src, term key, term value);
    size_t (*put_map_exact_one_heap_need)(CTXP_ term src);
    term (*put_map_exact_one)(CTXP_ JSP_ term src, int pos, term key, term value);
    term (*term_reuse_or_clone_binary)(CTXP_ term src, size_t total_bytes);
};

extern const ModuleNativeInterface module_native_interface;

enum TrapAndLoadResult
{
    TRAP_AND_LOAD_OK,
    TRAP_AND_LOAD_CODE_SERVER_NOT_FOUND
};

// n_words parameter values for call_ext
// n_words >= 0 means CALL_EXT_LAST (deallocate n_words from stack)
#define CALL_EXT_NO_DEALLOC -1
#define CALL_EXT_NO_DEALLOC_MFA -2

#define JIT_FORMAT_VERSION 9

#define JIT_VARIANT_PIC 1
#define JIT_VARIANT_FLOAT32 2
#define JIT_VARIANT_THUMB2 4
// Native code carries a relocation table applied by the loader: primitive calls
// become a direct branch instead of an indirect load through the native-interface
// table. Distinct instruction bytes + a relocation table, so it is its own
// variant: a runtime built without it never matches (and never loads) such code.
#define JIT_VARIANT_RELOC 8
// Generated code dispatches *_direct primitive results with the sentinel-
// continuation contract: the callee entry travels via jit_state->continuation
// and the primitive returns the bare sentinel 1 (see JIT_DIRECT_TAGGED).
// Intrinsic to backends whose jumptable entries are not 4-aligned (x86_64);
// code and runtime must agree, so it is its own variant bit.
#define JIT_VARIANT_DIRECT_CALL 16

#ifdef JIT_JUMPTABLE_IS_DATA
/**
 * @brief Get per-thread function pointer for a WASM JIT label.
 *
 * In Emscripten pthreads mode, each thread has its own wasmTable. This function
 * lazily compiles the WASM module for the calling thread and returns a function
 * pointer valid in that thread's table.
 */
ModuleNativeEntryPoint jit_wasm_get_entry_point(const void *native_code, int label);

/**
 * @brief Get the lines/cont_label_map metadata from a WASM JIT module.
 */
const uint8_t *jit_wasm_get_lines_metadata(const void *native_code);
#endif

/**
 * @brief Return the entry point from a given jit stream
 *
 * @details Platform implementing JIT must provide this function which
 * is called by code_server:set_native_code/2
 * @param ctx the current context (code_server)
 * @param jit_stream the jit stream term
 * @returns the pointer to the first function
 */
ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream);

/**
 * @brief Trap a process and load module, process shall be resumed at given label.
 *
 * @param ctx the process to trap
 * @param mod the module to load
 * @param label the label to resume the process to
 */
enum TrapAndLoadResult jit_trap_and_load(Context *ctx, Module *mod, uint32_t label);

#ifndef AVM_NO_JIT_DWARF
/**
 * @brief Register JIT-compiled code with debug info with GDB/LLDB
 *
 * @details This function registers native code and associated DWARF debug
 * information with the debugger using the GDB JIT interface. This allows
 * debuggers to show function names and source line information for JIT code.
 *
 * @param mod The module containing the JIT code
 * @param native_code Pointer to the native machine code
 * @param native_size Size of the native code in bytes
 * @param entry_point The actual mapped entry point address
 */
void jit_debug_register_code(Module *mod, const void *native_code, size_t native_size, ModuleNativeEntryPoint entry_point);

/**
 * @brief Unregister JIT-compiled code from debugger
 *
 * @details This function unregisters previously registered JIT code from
 * the debugger. Should be called when a module is unloaded.
 *
 * @param ctx The context
 * @param mod The module being unloaded
 */
void jit_debug_unregister_code(Context *ctx, Module *mod);

#endif

#ifdef __cplusplus
}
#endif

#endif
