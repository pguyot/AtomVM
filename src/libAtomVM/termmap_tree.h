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
 * @file termmap_tree.h
 * @brief Persistent weight-balanced tree used as the backing store for large
 * maps (more than TERM_MAP_TREE_THRESHOLD entries).
 *
 * @details Large flat maps cost O(n) per single-key insert/update (a whole new
 * values array is copied), so building an n-entry map one key at a time is
 * O(n^2) -- the dominant cost when AtomVM runs the Erlang compiler. This tree
 * gives O(log n) put/get with structural sharing, matching BEAM's switch to a
 * HAMT above 32 entries, while keeping AtomVM's "keys are sorted in
 * term_compare(TermCompareExact) order" invariant (an in-order walk yields the
 * keys ascending, exactly like a flat map).
 *
 * Nodes are ordinary boxed 5-tuples {Size, Key, Value, Left, Right}; an empty
 * subtree is NIL. Because nodes are plain tuples, the existing garbage
 * collector, term copier and term hasher traverse them with no special cases.
 */

#ifndef _TERMMAP_TREE_H_
#define _TERMMAP_TREE_H_

#include "memory.h"
#include "term.h"
#include "term_typedef.h"

#ifdef __cplusplus
extern "C" {
#endif

struct GlobalContext;

/** @brief The empty tree. */
static inline term termtree_empty(void)
{
    return term_nil();
}

/** @brief Number of key/value pairs in the (sub)tree rooted at \p node. */
size_t termtree_size(term node);

/**
 * @brief Structural equality of two tree roots, exploiting shared subtrees.
 *
 * Walks both trees in parallel, short-circuiting pointer-identical (shared)
 * subtrees, so comparing a map to a path-copied update of itself is O(height).
 * @return 1 if the two trees hold the same key/value set, 0 if they provably
 * differ, or -1 if their shapes diverge (an insert split) so the caller must
 * fall back to a full sorted comparison.
 */
int termtree_struct_equal(term a, term b, struct GlobalContext *global);

/**
 * @brief Equality of two trees holding the same number of entries, whatever
 * their shapes.
 *
 * termtree_struct_equal gives up when an insert split has made the two shapes
 * diverge; this walks both in ascending key order instead, skipping subtrees
 * that are pointer-identical, and so still answers a map versus a path-copied
 * update of itself in O(height) without materialising either side.
 * @return 1 if equal, 0 if they differ, -1 if the walk could not be performed
 * (tree deeper than the cursor bound) and the caller must fall back.
 */
int termtree_equal(term a, term b, struct GlobalContext *global);

/**
 * @brief Look up \p key.
 * @return the associated value, or term_invalid_term() if absent.
 */
term termtree_get(term node, term key, struct GlobalContext *global);

/**
 * @brief Insert or update \p key -> \p value, returning the new root.
 *
 * The caller must have reserved at least termtree_put_heap_size(size) free
 * words on \p heap; no garbage collection happens here. \p node and the new
 * value are shared where possible (path copying).
 */
term termtree_put(Heap *heap, term node, term key, term value, struct GlobalContext *global);

/**
 * @brief Worst-case number of heap words a single termtree_put may allocate
 * into a tree currently holding \p size entries.
 */
size_t termtree_put_heap_size(size_t size);

/**
 * @brief Build a balanced tree from \p n key/value pairs whose keys are already
 * sorted ascending (term_compare order). O(n); allocates n * TERMTREE_NODE_SIZE
 * words, which the caller must have reserved.
 *
 * @param keys array of \p n keys, ascending and unique
 * @param values array of \p n values (parallel to \p keys)
 */
term termtree_from_sorted(Heap *heap, const term *keys, const term *values, size_t n);

/** @brief Worst-case heap words termtree_from_sorted may allocate for \p n entries. */
size_t termtree_from_sorted_heap_size(size_t n);

/**
 * @brief In-order position (0-based rank) of \p key, or -1 if absent.
 *
 * Lets tree-backed maps reuse the flat-map "find position, then index" calling
 * convention (term_find_map_pos): the returned rank feeds termtree_select_*.
 */
int termtree_rank(term node, term key, struct GlobalContext *global);

/**
 * @brief Prepend the tree's entries, in ascending key order, to \p acc as a
 * flat list [K0,V0,K1,V1,...|acc]. One O(n) in-order walk, so iterating a tree
 * map (maps:next/fold/keys/values) is O(n) overall instead of O(n) position
 * selects. The caller must have reserved 2*size list cells (4*size words).
 */
term termtree_to_kv_list(term node, term acc, Heap *heap);

/**
 * @brief Fill \p out (length 2*size) with the entries in ascending key order as
 * [K0,V0,K1,V1,...], in one O(n) in-order walk. Lets callers that would
 * otherwise select each entry by position (e.g. term_compare of two maps) read
 * a tree map sequentially.
 */
void termtree_fill_array(term node, term *out);

/** @brief Key at in-order position \p index (0-based). */
term termtree_select_key(term node, size_t index);

/** @brief Value at in-order position \p index (0-based). */
term termtree_select_value(term node, size_t index);

#ifdef __cplusplus
}
#endif

#endif
