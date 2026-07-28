%
% This file is part of AtomVM.
%
% Copyright 2026 Paul Guyot <pguyot@kallisys.net>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%

%% Primitives that neither read nor write ctx->heap.heap_ptr / ctx->e nor
%% trigger a GC, and do not return a Context *: hp/e stay authoritative in
%% their pinned registers across the call, so call sites skip the
%% write-back/reload pair. Context*-returning primitives must NEVER be
%% listed: their call sites can exit to the scheduler right after the call,
%% and the write-back before the call is what persists the heap state.
%% When in doubt, leave a primitive out — the only cost is 4 instructions.
prim_pure(?PRIM_MODULE_GET_ATOM_TERM_BY_ID) -> true;
prim_pure(?PRIM_TRIM_LIVE_REGS) -> true;
prim_pure(?PRIM_GET_IMPORTED_BIF) -> true;
%% term_compare only sets error fields on OOM (set_error with offset 0), it
%% never touches the heap.
prim_pure(?PRIM_TERM_COMPARE) -> true;
prim_pure(?PRIM_EXTENDED_REGISTER_PTR) -> true;
prim_pure(?PRIM_MAILBOX_PEEK) -> true;
prim_pure(?PRIM_MAILBOX_REMOVE_MESSAGE) -> true;
prim_pure(?PRIM_TIMEOUT) -> true;
prim_pure(?PRIM_MAILBOX_NEXT) -> true;
prim_pure(?PRIM_CANCEL_TIMEOUT) -> true;
prim_pure(?PRIM_CLEAR_TIMEOUT_FLAG) -> true;
prim_pure(?PRIM_CONTEXT_GET_FLAGS) -> true;
prim_pure(?PRIM_ENSURE_FPREGS) -> true;
prim_pure(?PRIM_TERM_IS_NUMBER) -> true;
prim_pure(?PRIM_TERM_CONV_TO_FLOAT) -> true;
prim_pure(?PRIM_FADD) -> true;
prim_pure(?PRIM_FSUB) -> true;
prim_pure(?PRIM_FMUL) -> true;
prim_pure(?PRIM_FDIV) -> true;
prim_pure(?PRIM_FNEGATE) -> true;
prim_pure(?PRIM_TERM_SUB_BINARY_HEAP_SIZE) -> true;
prim_pure(?PRIM_TERM_FIND_MAP_POS) -> true;
prim_pure(?PRIM_BITSTRING_UTF8_SIZE) -> true;
prim_pure(?PRIM_BITSTRING_UTF16_SIZE) -> true;
prim_pure(?PRIM_MODULE_GET_FUN_ARITY) -> true;
prim_pure(?PRIM_BITSTRING_MATCH_MODULE_STR) -> true;
prim_pure(?PRIM_BITSTRING_GET_UTF8) -> true;
prim_pure(?PRIM_BITSTRING_GET_UTF16) -> true;
prim_pure(?PRIM_BITSTRING_GET_UTF32) -> true;
prim_pure(?PRIM_RECORD_FIELD_POS) -> true;
prim_pure(?PRIM_IS_RECORD_OF) -> true;
prim_pure(?PRIM_SET_TUPLE_ELEMENT) -> true;
prim_pure(?PRIM_PUT_MAP_HEAP_NEED) -> true;
%% Like PUT_MAP_HEAP_NEED: these only read the map's size to size the
%% reservation. They allocate nothing, cannot collect, and return a size_t.
prim_pure(?PRIM_PUT_MAP_ONE_HEAP_NEED) -> true;
prim_pure(?PRIM_PUT_MAP_EXACT_ONE_HEAP_NEED) -> true;
prim_pure(?PRIM_MAP_GET_VALUE) -> true;
prim_pure(?PRIM_TERM_GET_MAP_ASSOC) -> true;
prim_pure(?PRIM_TERM_GET_MAP_ASSOC_MISS) -> true;
prim_pure(?PRIM_BITSTRING_GET_TAIL_HEAP_SIZE) -> true;
prim_pure(?PRIM_BITSTRING_SLICE_HEAP_SIZE) -> true;
prim_pure(_) -> false.

%% Primitives that allocate at most within the caller's heap reservation
%% (or a fresh fragment) and can neither collect (no memory_ensure_free on
%% any resuming path) nor write VM x registers: terms never move and
%% ctx->x is untouched, so the register cache — including cached VM
%% register contents and untagged {ptr, _} derivatives — stays valid and
%% no home reload is needed. They are NOT pure: hp/e must still be
%% written back and reloaded around the call. Error paths may exist but
%% never resume at the call site (they leave through handle_error and the
%% dispatcher, which re-seeds homes; the following label invalidates the
%% cache). Verified against the C bodies (static call-closure audit +
%% hand check of the hot ones); when in doubt, leave a primitive out.
-compile({nowarn_unused_function, [{prim_no_gc, 1}]}).

prim_no_gc(?PRIM_PUT_LIST) -> true;
prim_no_gc(?PRIM_MODULE_LOAD_LITERAL) -> true;
prim_no_gc(?PRIM_ALLOC_BOXED_INTEGER_FRAGMENT) -> true;
prim_no_gc(?PRIM_ALLOC_BIG_INTEGER_FRAGMENT) -> true;
prim_no_gc(?PRIM_TERM_ALLOC_TUPLE) -> true;
prim_no_gc(?PRIM_TERM_ALLOC_FUN) -> true;
prim_no_gc(?PRIM_TERM_FROM_FLOAT) -> true;
prim_no_gc(?PRIM_TERM_CREATE_EMPTY_BINARY) -> true;
prim_no_gc(?PRIM_TERM_ALLOC_BIN_MATCH_STATE) -> true;
prim_no_gc(?PRIM_TERM_MAYBE_CREATE_SUB_BINARY) -> true;
prim_no_gc(?PRIM_BITSTRING_SLICE) -> true;
prim_no_gc(?PRIM_BITSTRING_CREATE_TAIL) -> true;
prim_no_gc(?PRIM_BS_CREATE_BIN_WRAP) -> true;
prim_no_gc(?PRIM_BITSTRING_COPY_BINARY) -> true;
prim_no_gc(?PRIM_BITSTRING_COPY_MODULE_STR) -> true;
prim_no_gc(?PRIM_BITSTRING_EXTRACT_INTEGER) -> true;
prim_no_gc(?PRIM_TERM_REUSE_BINARY) -> true;
prim_no_gc(?PRIM_TERM_REUSE_OR_CLONE_BINARY) -> true;
prim_no_gc(?PRIM_PUT_MAP_ASSOC) -> true;
prim_no_gc(?PRIM_PUT_MAP_ASSOC_ONE) -> true;
prim_no_gc(?PRIM_PUT_MAP_EXACT_ONE) -> true;
prim_no_gc(_) -> false.

%% Primitives that may return a *different* Context than the one passed in,
%% because they can switch to another process or terminate this one. After
%% such a call the incoming ctx may already be freed, so the hp/e reload that
%% normally follows a call must not run before the result has been tested:
%% the call sites below (return_if_not_equal_to_ctx/2 and direct_dispatch/3)
%% emit it on the paths that stay with the same, still-live context. Only
%% primitives whose call sites do that may be listed -- a tail-called
%% (call_primitive_last) primitive never reloads at all and needs no entry.
-compile({nowarn_unused_function, [{prim_returns_context, 1}]}).

prim_returns_context(?PRIM_PROCESS_SIGNAL_MESSAGES) -> true;
prim_returns_context(?PRIM_CALL_EXT_DIRECT) -> true;
prim_returns_context(?PRIM_CALL_FUN_DIRECT) -> true;
prim_returns_context(?PRIM_RETURN_DIRECT) -> true;
prim_returns_context(_) -> false.
