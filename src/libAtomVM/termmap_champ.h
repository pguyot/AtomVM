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
 * @file termmap_champ.h
 * @brief Persistent CHAMP trie used as the backing store for large maps (more
 * than TERM_MAP_HASH_THRESHOLD entries).
 *
 * @details CHAMP (Compressed Hash-Array Mapped Prefix-tree) indexes an entry by
 * successive 4-bit slices of term_hash(key), the same 16-way fan-out ERTS uses
 * for its hashmaps, so a lookup costs one hash plus one key comparison instead
 * of the log2(n) comparisons an ordered tree needs. That trade is what makes it
 * worth having: the Erlang compiler's hot maps run to thousands of entries and
 * their keys are compound terms, whose comparison is the expensive part.
 *
 * A node is an ordinary boxed tuple, exactly as the tree's nodes were, so the
 * garbage collector, term copier and term hasher traverse it with no special
 * cases:
 *
 *   {DataMap, NodeMap, K0, V0, .., Kd-1, Vd-1, N0, .., Nn-1}
 *
 * DataMap and NodeMap are 16-bit slot bitmaps held as separate small integers
 * (a single 32-bit word would not fit a 32-bit build's 27-bit small-int range).
 * A slot holds either an inline entry or a sub-node, never both. Keeping them
 * apart, rather than interleaving as a HAMT does, is what makes a node's
 * entries contiguous, which is CHAMP's advantage for iteration and lookup
 * locality.
 *
 * Once all 32 hash bits are consumed, entries whose hashes are fully equal
 * share a collision node, marked by DataMap = -1 and holding only entries.
 * Growing one is the only unbounded allocation a put can make, so the root
 * records whether the map contains one at all (CHAMP_ROOT_COLLISION_BIT); a
 * map without collisions -- every map in practice -- reserves a small constant.
 *
 * The representation is canonical: a sub-node that would hold a single entry is
 * inlined into its parent instead, so two maps with the same entries always
 * have the same shape. Entry order is therefore stable and reproducible, but it
 * is hash order, not term order; callers that need Erlang's term ordering (map
 * comparison, external term format, printing) sort explicitly.
 */

#ifndef _TERMMAP_CHAMP_H_
#define _TERMMAP_CHAMP_H_

#include "memory.h"
#include "term.h"
#include "term_typedef.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct GlobalContext;

/** @brief The empty map. */
static inline term termmap_champ_empty(void)
{
    return term_nil();
}

/** @brief Number of key/value pairs below \p node (walks it; for tests). */
size_t termmap_champ_count(term node);

/**
 * @brief Look up \p key.
 * @return the associated value, or term_invalid_term() if absent.
 */
term termmap_champ_get(term node, term key, struct GlobalContext *global);

/**
 * @brief Insert or update \p key -> \p value, returning the new root.
 *
 * Every untouched sub-node is shared; only the root-to-entry path is copied.
 * The caller must have reserved termmap_champ_put_heap_size(node, size) free
 * words on \p heap, measured against the same \p node (a garbage collection
 * between the two moves the node but does not change the count).
 *
 * @param added set to true when the key was not already present
 */
term termmap_champ_put(Heap *heap, term node, term key, term value, struct GlobalContext *global, bool *added);

/**
 * @brief Heap words a single termmap_champ_put into \p node may allocate.
 *
 * Constant unless the map holds a collision node, which only equal 32-bit
 * hashes create and whose growth is bounded by the map's size.
 */
size_t termmap_champ_put_heap_size(term node, size_t size);

/**
 * @brief Remove \p key, sharing every untouched sub-node and restoring the
 * canonical form.
 *
 * @param found set to true when the key was present; when false the original
 *        node is returned unchanged and nothing is allocated
 * @return the new root, or NIL when the map is now empty
 */
term termmap_champ_remove(Heap *heap, term node, term key, struct GlobalContext *global, bool *found);

/**
 * @brief Heap words a put may allocate into a trie the caller cannot name yet
 * (one it is about to build), which is the with-collisions bound.
 */
size_t termmap_champ_put_heap_size_max(size_t size);

/** @brief Heap words to reserve before one termmap_champ_remove on \p node. */
size_t termmap_champ_remove_heap_size(term node, size_t size);

/**
 * @brief Worst-case heap words a trie built from \p n entries can occupy.
 *
 * Only usable for the small conversions (a flat map outgrowing
 * TERM_MAP_FLAT_GROW_MAX); larger bulk builds should measure exactly with
 * termmap_champ_measure, whose answer is roughly a sixth of this bound.
 */
size_t termmap_champ_from_array_heap_size(size_t n);

/**
 * @brief Build a trie from \p n key/value pairs, whose keys must be distinct.
 * The caller must have reserved termmap_champ_from_array_heap_size(n) words.
 */
term termmap_champ_from_array(Heap *heap, const term *keys, const term *values, size_t n, struct GlobalContext *global);

/**
 * @brief A measured bulk build: the hashes and the exact word count of a trie,
 * computed before the heap is reserved.
 *
 * Hashes do not depend on where a term sits, so a garbage collection between
 * termmap_champ_measure and termmap_champ_build is fine as long as the caller
 * re-reads the (possibly moved) key and value arrays.
 */
struct ChampBuilder
{
    uint32_t *hashes;
    int *idx;
    int *scratch;
    size_t words;
    int n;
    bool collided;
};

/**
 * @brief Hash \p n keys and compute the exact heap size their trie needs.
 * @return false on allocation failure (nothing to free in that case).
 */
bool termmap_champ_measure(struct ChampBuilder *builder, const term *keys, size_t n, struct GlobalContext *global);

/**
 * @brief Compute the trie size from precomputed term_hash values for n keys.
 * Copies the hashes; the caller retains ownership of the input array.
 * @return false on allocation failure (nothing to free in that case).
 */
bool termmap_champ_measure_hashed(struct ChampBuilder *builder, const uint32_t *hashes, size_t n);

/**
 * @brief Build the trie measured by termmap_champ_measure. Consumes the
 * builder's scratch; call termmap_champ_builder_free afterwards either way.
 */
term termmap_champ_build(struct ChampBuilder *builder, const term *keys, const term *values, Heap *heap);

/** @brief Release a builder's scratch memory. */
void termmap_champ_builder_free(struct ChampBuilder *builder);

/**
 * @brief Call \p fn on every entry, in one depth-first walk and without
 * allocating, for callers that only need to see each entry once (term_hash).
 */
void termmap_champ_foreach(term node, void (*fn)(term key, term value, void *arg), void *arg);

/**
 * @brief Fill \p out (length 2*size) with the entries as [K0,V0,K1,V1,...] in
 * one depth-first walk. The order is hash order: stable and canonical for a
 * given set of entries, but not term order.
 */
void termmap_champ_fill_array(term node, term *out);

/**
 * @brief Fill \p out (length 2*size) with the entries sorted ascending by key
 * in term_compare(TermCompareExact) order, for the callers whose result is
 * ordered: map comparison, the external term format and the printer.
 * @return false if a key comparison ran out of memory.
 */
bool termmap_champ_fill_array_sorted(term node, term *out, size_t n, struct GlobalContext *global);

/**
 * @brief Prepend the entries to \p acc as a flat list [K0,V0,K1,V1,...|acc].
 * The caller must have reserved 2*size list cells (4*size words).
 */
term termmap_champ_to_kv_list(term node, term acc, Heap *heap);

/**
 * @brief Equality of two tries holding the same number of entries.
 *
 * Walks one side and looks each key up in the other, short-circuiting
 * pointer-identical sub-nodes, so a map versus a path-copied update of itself
 * is answered without materialising either.
 * @return 1 if equal, 0 if they differ, -1 if a comparison ran out of memory.
 */
int termmap_champ_equal(term a, term b, struct GlobalContext *global, bool exact);

/**
 * @brief Heap words to reserve before one termmap_champ_cursor_next step, and,
 * when \p first is true, before building the initial cursor as well.
 */
size_t termmap_champ_cursor_reserve(term root, bool first);

/**
 * @brief Build a cursor positioned at the first entry. The cursor is an
 * ordinary term -- a list of {Node, Index} frames -- so the garbage collector,
 * term copier and hasher need no special case for it.
 */
term termmap_champ_cursor_first(term root, Heap *heap);

/**
 * @brief Produce the entry at \p cursor and the cursor following it.
 * @returns false when the traversal is exhausted, leaving the outputs untouched.
 */
bool termmap_champ_cursor_next(term cursor, term *key, term *value, term *next_cursor, Heap *heap);

/**
 * @brief Key at walk position \p index (0-based). O(index): only the legacy
 * positional iteration paths use it.
 */
term termmap_champ_select_key(term node, size_t index);

/** @brief Value at walk position \p index (0-based). O(index). */
term termmap_champ_select_value(term node, size_t index);

/**
 * @brief Walk position of \p key, or -1 if absent. O(n); callers that only need
 * presence should use termmap_champ_get.
 */
int termmap_champ_rank(term node, term key, struct GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
