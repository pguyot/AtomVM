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

#include "termmap_champ.h"

#include <stdlib.h>
#include <string.h>

#include "memory.h"
#include "term.h"
#include "term_hash.h"
#include "utils.h"

// 4 bits per level over a 32-bit hash: eight levels of 16 slots, the same
// fan-out ERTS gives its hashmaps. Wider nodes would shorten the trie by a
// level at 4k entries but make every path copy rebuild a bigger tuple, and a
// 32-slot bitmap would not fit a 32-bit build's small-integer range.
#define CHAMP_BITS 4
#define CHAMP_WIDTH (1 << CHAMP_BITS)
#define CHAMP_MASK (CHAMP_WIDTH - 1)
#define CHAMP_HASH_BITS 32
#define CHAMP_MAX_LEVELS (CHAMP_HASH_BITS / CHAMP_BITS)

#define CHAMP_BITMAP_MASK ((uint32_t) ((((uint32_t) 1) << CHAMP_WIDTH) - 1))
// Set on the root's DataMap when the map holds a collision node anywhere, so
// that a put on any other map -- every map in practice -- reserves a constant.
#define CHAMP_ROOT_COLLISION_BIT (((uint32_t) 1) << CHAMP_WIDTH)

#define CHAMP_DATAMAP_IDX 0
#define CHAMP_NODEMAP_IDX 1
#define CHAMP_FIRST_ENTRY 2

// Header word, two bitmaps, every slot inline, every slot a child.
#define CHAMP_MAX_NODE_WORDS (1 + 2 + 2 * CHAMP_WIDTH + CHAMP_WIDTH)
// A slot collision pushes both entries down a chain of single-child nodes.
#define CHAMP_MERGE_CHAIN_WORDS (CHAMP_MAX_LEVELS * (1 + 2 + 1) + (1 + 2 + 4))
// Path copy plus that chain: what a put allocates when no collision node grows.
#define CHAMP_PUT_PATH_WORDS (CHAMP_MAX_LEVELS * CHAMP_MAX_NODE_WORDS + CHAMP_MERGE_CHAIN_WORDS)

// A cursor frame is a {Node, Index} tuple in a list; one per level.
#define CHAMP_CURSOR_FRAME_WORDS ((2 + 1) + 2)

// Bulk building from a plain array keeps its index scratch on the stack up to
// this many entries. Bigger arrays -- a large flat map, which maps:from_keys
// and the external term format can both produce, meeting its first insert --
// borrow the scratch from the heap the caller already reserved instead, so the
// build stays allocation-free and cannot fail.
#define CHAMP_STACK_SCRATCH_MAX 128
// Heap words of index scratch a build of n entries borrows: the hashes and the
// two index arrays the counting sort alternates between.
#define CHAMP_HEAP_SCRATCH_WORDS(n) (3 * (n))

static inline uint32_t champ_slot(uint32_t hash, int shift)
{
    return (hash >> shift) & CHAMP_MASK;
}

static inline int champ_index(uint32_t bitmap, uint32_t bit)
{
    return __builtin_popcount(bitmap & (bit - 1));
}

// Element i of a node is body[i]; the boxed header sits one word before it.
static inline const term *champ_body(term node)
{
    return term_to_const_term_ptr(node) + 1;
}

// A node under construction is written through this: the payload of a freshly
// allocated node never overlaps the node it is copied from, so whole runs move
// as words instead of one term_put_tuple_element at a time.
static inline term *champ_body_mut(term node)
{
    return term_to_term_ptr(node) + 1;
}

static inline void champ_move(term *dst, const term *src, size_t words)
{
    memcpy(dst, src, words * sizeof(term));
}

static inline bool champ_is_collision(term node)
{
    return term_to_int(champ_body(node)[CHAMP_DATAMAP_IDX]) < 0;
}

static inline uint32_t champ_datamap(term node)
{
    return ((uint32_t) term_to_int(champ_body(node)[CHAMP_DATAMAP_IDX])) & CHAMP_BITMAP_MASK;
}

static inline uint32_t champ_nodemap(term node)
{
    return (uint32_t) term_to_int(champ_body(node)[CHAMP_NODEMAP_IDX]);
}

static inline size_t champ_entries(term node)
{
    if (UNLIKELY(champ_is_collision(node))) {
        return (size_t) ((term_get_tuple_arity(node) - CHAMP_FIRST_ENTRY) / 2);
    }
    return (size_t) __builtin_popcount(champ_datamap(node));
}

static inline size_t champ_nodes(term node)
{
    if (UNLIKELY(champ_is_collision(node))) {
        return 0;
    }
    return (size_t) __builtin_popcount(champ_nodemap(node));
}

static inline term champ_entry_key(term node, size_t i)
{
    return champ_body(node)[CHAMP_FIRST_ENTRY + 2 * i];
}

static inline term champ_entry_value(term node, size_t i)
{
    return champ_body(node)[CHAMP_FIRST_ENTRY + 2 * i + 1];
}

static inline term champ_sub(term node, size_t entries, size_t i)
{
    return champ_body(node)[CHAMP_FIRST_ENTRY + 2 * entries + i];
}

// The descent asks "same key?" at every level, so the shallow cases -- an
// identical term, two distinct immediates, a same-header tuple of immediates --
// are decided here rather than through a call into term.c.
static inline bool champ_key_eq(term a, term b, GlobalContext *global)
{
    return term_exact_eq(a, b, global);
}

static term champ_node_new(Heap *heap, uint32_t datamap, uint32_t nodemap, size_t entries, size_t nodes)
{
    term node = term_alloc_tuple((uint32_t) (CHAMP_FIRST_ENTRY + 2 * entries + nodes), heap);
    term_put_tuple_element(node, CHAMP_DATAMAP_IDX, term_from_int((avm_int_t) datamap));
    term_put_tuple_element(node, CHAMP_NODEMAP_IDX, term_from_int((avm_int_t) nodemap));
    return node;
}

static term champ_collision_new(Heap *heap, size_t entries)
{
    term node = term_alloc_tuple((uint32_t) (CHAMP_FIRST_ENTRY + 2 * entries), heap);
    term_put_tuple_element(node, CHAMP_DATAMAP_IDX, term_from_int(-1));
    term_put_tuple_element(node, CHAMP_NODEMAP_IDX, term_from_int(0));
    return node;
}

/** @brief Whether the map rooted here holds a collision node anywhere. */
static inline bool champ_root_has_collision(term root)
{
    if (term_is_nil(root)) {
        return false;
    }
    avm_int_t raw = term_to_int(champ_body(root)[CHAMP_DATAMAP_IDX]);
    return raw < 0 || (((uint32_t) raw) & CHAMP_ROOT_COLLISION_BIT) != 0;
}

static inline void champ_root_set_collision(term root)
{
    if (term_is_nil(root) || champ_is_collision(root)) {
        return;
    }
    term *body = term_to_term_ptr(root) + 1;
    uint32_t raw = (uint32_t) term_to_int(body[CHAMP_DATAMAP_IDX]);
    body[CHAMP_DATAMAP_IDX] = term_from_int((avm_int_t) (raw | CHAMP_ROOT_COLLISION_BIT));
}

// ---- lookup -----------------------------------------------------------------

term termmap_champ_get(term node, term key, GlobalContext *global)
{
    if (UNLIKELY(term_is_nil(node))) {
        return term_invalid_term();
    }
    uint32_t hash = term_hash(key, global);
    for (;;) {
        const term *body = champ_body(node);
        avm_int_t raw = term_to_int(body[CHAMP_DATAMAP_IDX]);
        if (UNLIKELY(raw < 0)) {
            // Collision node: linear scan of the fully-equal-hash entries.
            int arity = term_get_tuple_arity(node);
            int count = (arity - CHAMP_FIRST_ENTRY) / 2;
            for (int i = 0; i < count; i++) {
                if (champ_key_eq(body[CHAMP_FIRST_ENTRY + 2 * i], key, global)) {
                    return body[CHAMP_FIRST_ENTRY + 2 * i + 1];
                }
            }
            return term_invalid_term();
        }
        uint32_t datamap = ((uint32_t) raw) & CHAMP_BITMAP_MASK;
        uint32_t nodemap = (uint32_t) term_to_int(body[CHAMP_NODEMAP_IDX]);
        uint32_t slot = hash & CHAMP_MASK;
        // Full internal nodes have no inline entries and store child pointers
        // in slot order, so their descent needs no population counts.
        if (datamap == 0 && nodemap == CHAMP_BITMAP_MASK) {
            node = body[CHAMP_FIRST_ENTRY + slot];
            hash >>= CHAMP_BITS;
            continue;
        }
        uint32_t bit = ((uint32_t) 1) << slot;
        if (datamap & bit) {
            int i = champ_index(datamap, bit);
            if (champ_key_eq(body[CHAMP_FIRST_ENTRY + 2 * i], key, global)) {
                return body[CHAMP_FIRST_ENTRY + 2 * i + 1];
            }
            return term_invalid_term();
        }
        if (!(nodemap & bit)) {
            return term_invalid_term();
        }
        int entries = __builtin_popcount(datamap);
        int i = champ_index(nodemap, bit);
        node = body[CHAMP_FIRST_ENTRY + 2 * entries + i];
        hash >>= CHAMP_BITS;
    }
}

// ---- insert -----------------------------------------------------------------

// Two entries whose hashes agree down to `shift`: build the subtree that
// separates them, descending until their slots differ, or, once the hash runs
// out, a collision node holding both.
static term champ_merge_two(Heap *heap, term k1, term v1, uint32_t h1, term k2, term v2,
    uint32_t h2, int shift, bool *collided)
{
    if (shift >= CHAMP_HASH_BITS) {
        *collided = true;
        term node = champ_collision_new(heap, 2);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY, k1);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 1, v1);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2, k2);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 3, v2);
        return node;
    }
    uint32_t s1 = champ_slot(h1, shift);
    uint32_t s2 = champ_slot(h2, shift);
    if (s1 != s2) {
        uint32_t datamap = (((uint32_t) 1) << s1) | (((uint32_t) 1) << s2);
        term node = champ_node_new(heap, datamap, 0, 2, 0);
        bool first_is_1 = s1 < s2;
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY, first_is_1 ? k1 : k2);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 1, first_is_1 ? v1 : v2);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2, first_is_1 ? k2 : k1);
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 3, first_is_1 ? v2 : v1);
        return node;
    }
    term child = champ_merge_two(heap, k1, v1, h1, k2, v2, h2, shift + CHAMP_BITS, collided);
    term node = champ_node_new(heap, 0, ((uint32_t) 1) << s1, 0, 1);
    term_put_tuple_element(node, CHAMP_FIRST_ENTRY, child);
    return node;
}

static term champ_put_rec(Heap *heap, term node, term key, term value, uint32_t hash, int shift,
    GlobalContext *global, bool *added, bool *collided)
{
    const term *src = champ_body(node);
    avm_int_t raw = term_to_int(src[CHAMP_DATAMAP_IDX]);

    if (UNLIKELY(raw < 0)) {
        size_t count = (size_t) ((term_get_tuple_arity(node) - CHAMP_FIRST_ENTRY) / 2);
        *collided = true;
        for (size_t i = 0; i < count; i++) {
            if (champ_key_eq(src[CHAMP_FIRST_ENTRY + 2 * i], key, global)) {
                term out = champ_collision_new(heap, count);
                term *dst = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
                champ_move(dst, src + CHAMP_FIRST_ENTRY, 2 * count);
                dst[2 * i + 1] = value;
                *added = false;
                return out;
            }
        }
        term out = champ_collision_new(heap, count + 1);
        term *dst = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
        champ_move(dst, src + CHAMP_FIRST_ENTRY, 2 * count);
        dst[2 * count] = key;
        dst[2 * count + 1] = value;
        *added = true;
        return out;
    }

    uint32_t datamap = ((uint32_t) raw) & CHAMP_BITMAP_MASK;
    uint32_t nodemap = (uint32_t) term_to_int(src[CHAMP_NODEMAP_IDX]);
    size_t entries = (size_t) __builtin_popcount(datamap);
    size_t nodes = (size_t) __builtin_popcount(nodemap);
    uint32_t bit = ((uint32_t) 1) << champ_slot(hash, shift);
    const term *se = src + CHAMP_FIRST_ENTRY;
    const term *sn = se + 2 * entries;

    if (datamap & bit) {
        size_t i = (size_t) champ_index(datamap, bit);
        term k = se[2 * i];
        if (champ_key_eq(k, key, global)) {
            term out = champ_node_new(heap, datamap, nodemap, entries, nodes);
            term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
            champ_move(de, se, 2 * entries + nodes);
            de[2 * i + 1] = value;
            *added = false;
            return out;
        }
        // Slot collision between distinct keys: the resident entry moves down
        // into a new sub-node together with the incoming one.
        term sub = champ_merge_two(heap, k, se[2 * i + 1], term_hash(k, global), key, value, hash,
            shift + CHAMP_BITS, collided);
        uint32_t new_nodemap = nodemap | bit;
        size_t ni = (size_t) champ_index(new_nodemap, bit);
        term out = champ_node_new(heap, datamap & ~bit, new_nodemap, entries - 1, nodes + 1);
        term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
        term *dn = de + 2 * (entries - 1);
        champ_move(de, se, 2 * i);
        champ_move(de + 2 * i, se + 2 * (i + 1), 2 * (entries - 1 - i));
        champ_move(dn, sn, ni);
        dn[ni] = sub;
        champ_move(dn + ni + 1, sn + ni, nodes - ni);
        *added = true;
        return out;
    }

    if (nodemap & bit) {
        size_t i = (size_t) champ_index(nodemap, bit);
        term sub = champ_put_rec(
            heap, sn[i], key, value, hash, shift + CHAMP_BITS, global, added, collided);
        term out = champ_node_new(heap, datamap, nodemap, entries, nodes);
        term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
        champ_move(de, se, 2 * entries + nodes);
        de[2 * entries + i] = sub;
        return out;
    }

    // Free slot: the entry goes inline, keeping slot order.
    size_t i = (size_t) champ_index(datamap | bit, bit);
    term out = champ_node_new(heap, datamap | bit, nodemap, entries + 1, nodes);
    term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
    champ_move(de, se, 2 * i);
    de[2 * i] = key;
    de[2 * i + 1] = value;
    champ_move(de + 2 * (i + 1), se + 2 * i, 2 * (entries - i));
    champ_move(de + 2 * (entries + 1), sn, nodes);
    *added = true;
    return out;
}

term termmap_champ_put(Heap *heap, term node, term key, term value, GlobalContext *global, bool *added)
{
    uint32_t hash = term_hash(key, global);
    bool collided = false;
    term root;
    if (term_is_nil(node)) {
        uint32_t bit = ((uint32_t) 1) << champ_slot(hash, 0);
        root = champ_node_new(heap, bit, 0, 1, 0);
        term_put_tuple_element(root, CHAMP_FIRST_ENTRY, key);
        term_put_tuple_element(root, CHAMP_FIRST_ENTRY + 1, value);
        *added = true;
        return root;
    }
    bool had_collision = champ_root_has_collision(node);
    root = champ_put_rec(heap, node, key, value, hash, 0, global, added, &collided);
    if (had_collision || collided) {
        champ_root_set_collision(root);
    }
    return root;
}

size_t termmap_champ_put_heap_size(term node, size_t size)
{
    if (UNLIKELY(champ_root_has_collision(node))) {
        // Growing a collision node rewrites all its entries, and only equal
        // 32-bit hashes put entries in one, so the map's size bounds it.
        return CHAMP_PUT_PATH_WORDS + CHAMP_FIRST_ENTRY + 1 + 2 * (size + 1);
    }
    return CHAMP_PUT_PATH_WORDS;
}

size_t termmap_champ_put_heap_size_max(size_t size)
{
    return CHAMP_PUT_PATH_WORDS + CHAMP_FIRST_ENTRY + 1 + 2 * (size + 1);
}

// ---- remove -----------------------------------------------------------------

// A removal that leaves a sub-node holding exactly one entry must pull that
// entry back into the parent: without it two equal maps could have different
// shapes, and the canonical form is what keeps structural equality cheap.
// *collapsed_key is set when the returned node is that lone entry.
static term champ_remove_rec(Heap *heap, term node, term key, uint32_t hash, int shift,
    GlobalContext *global, bool *found, term *collapsed_key, term *collapsed_value)
{
    *collapsed_key = term_invalid_term();
    const term *src = champ_body(node);
    avm_int_t raw = term_to_int(src[CHAMP_DATAMAP_IDX]);

    if (UNLIKELY(raw < 0)) {
        size_t count = (size_t) ((term_get_tuple_arity(node) - CHAMP_FIRST_ENTRY) / 2);
        for (size_t i = 0; i < count; i++) {
            if (champ_key_eq(src[CHAMP_FIRST_ENTRY + 2 * i], key, global)) {
                *found = true;
                if (count == 2) {
                    size_t other = 1 - i;
                    *collapsed_key = src[CHAMP_FIRST_ENTRY + 2 * other];
                    *collapsed_value = src[CHAMP_FIRST_ENTRY + 2 * other + 1];
                    return term_nil();
                }
                term out = champ_collision_new(heap, count - 1);
                term *dst = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
                champ_move(dst, src + CHAMP_FIRST_ENTRY, 2 * i);
                champ_move(dst + 2 * i, src + CHAMP_FIRST_ENTRY + 2 * (i + 1),
                    2 * (count - 1 - i));
                return out;
            }
        }
        *found = false;
        return node;
    }

    uint32_t datamap = ((uint32_t) raw) & CHAMP_BITMAP_MASK;
    uint32_t nodemap = (uint32_t) term_to_int(src[CHAMP_NODEMAP_IDX]);
    size_t entries = (size_t) __builtin_popcount(datamap);
    size_t nodes = (size_t) __builtin_popcount(nodemap);
    uint32_t bit = ((uint32_t) 1) << champ_slot(hash, shift);
    const term *se = src + CHAMP_FIRST_ENTRY;
    const term *sn = se + 2 * entries;

    if (datamap & bit) {
        size_t i = (size_t) champ_index(datamap, bit);
        if (!champ_key_eq(se[2 * i], key, global)) {
            *found = false;
            return node;
        }
        *found = true;
        if (entries == 2 && nodes == 0 && shift > 0) {
            // This node becomes a single entry: hand it to the parent.
            size_t other = 1 - i;
            *collapsed_key = se[2 * other];
            *collapsed_value = se[2 * other + 1];
            return term_nil();
        }
        term out = champ_node_new(heap, datamap & ~bit, nodemap, entries - 1, nodes);
        term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
        champ_move(de, se, 2 * i);
        champ_move(de + 2 * i, se + 2 * (i + 1), 2 * (entries - 1 - i));
        champ_move(de + 2 * (entries - 1), sn, nodes);
        return out;
    }

    if (!(nodemap & bit)) {
        *found = false;
        return node;
    }

    size_t i = (size_t) champ_index(nodemap, bit);
    term ck;
    term cv;
    term sub
        = champ_remove_rec(heap, sn[i], key, hash, shift + CHAMP_BITS, global, found, &ck, &cv);
    if (!*found) {
        return node;
    }
    if (!term_is_invalid_term(ck)) {
        // The sub-node collapsed: its lone entry moves inline here, and if that
        // leaves this node holding a single entry it collapses in turn.
        if (entries == 0 && nodes == 1 && shift > 0) {
            *collapsed_key = ck;
            *collapsed_value = cv;
            return term_nil();
        }
        size_t di = (size_t) champ_index(datamap | bit, bit);
        term out = champ_node_new(heap, datamap | bit, nodemap & ~bit, entries + 1, nodes - 1);
        term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
        term *dn = de + 2 * (entries + 1);
        champ_move(de, se, 2 * di);
        de[2 * di] = ck;
        de[2 * di + 1] = cv;
        champ_move(de + 2 * (di + 1), se + 2 * di, 2 * (entries - di));
        champ_move(dn, sn, i);
        champ_move(dn + i, sn + i + 1, nodes - 1 - i);
        return out;
    }
    term out = champ_node_new(heap, datamap, nodemap, entries, nodes);
    term *de = champ_body_mut(out) + CHAMP_FIRST_ENTRY;
    champ_move(de, se, 2 * entries + nodes);
    de[2 * entries + i] = sub;
    return out;
}

term termmap_champ_remove(Heap *heap, term node, term key, GlobalContext *global, bool *found)
{
    *found = false;
    if (term_is_nil(node)) {
        return node;
    }
    bool had_collision = champ_root_has_collision(node);
    uint32_t hash = term_hash(key, global);
    term ck;
    term cv;
    term root = champ_remove_rec(heap, node, key, hash, 0, global, found, &ck, &cv);
    if (!*found) {
        return node;
    }
    if (!term_is_invalid_term(ck)) {
        // The whole trie collapsed to one entry.
        uint32_t bit = ((uint32_t) 1) << champ_slot(term_hash(ck, global), 0);
        root = champ_node_new(heap, bit, 0, 1, 0);
        term_put_tuple_element(root, CHAMP_FIRST_ENTRY, ck);
        term_put_tuple_element(root, CHAMP_FIRST_ENTRY + 1, cv);
        return root;
    }
    if (term_is_nil(root)) {
        return root;
    }
    if (!champ_is_collision(root) && champ_entries(root) == 0 && champ_nodes(root) == 0) {
        return term_nil();
    }
    // The flag is sticky: a delete never proves the last collision node gone.
    if (had_collision) {
        champ_root_set_collision(root);
    }
    return root;
}

size_t termmap_champ_remove_heap_size(term node, size_t size)
{
    if (UNLIKELY(champ_root_has_collision(node))) {
        return CHAMP_PUT_PATH_WORDS + CHAMP_FIRST_ENTRY + 1 + 2 * (size + 1);
    }
    return CHAMP_PUT_PATH_WORDS;
}

// ---- walks ------------------------------------------------------------------

size_t termmap_champ_count(term node)
{
    if (term_is_nil(node)) {
        return 0;
    }
    size_t entries = champ_entries(node);
    size_t nodes = champ_nodes(node);
    size_t total = entries;
    for (size_t i = 0; i < nodes; i++) {
        total += termmap_champ_count(champ_sub(node, entries, i));
    }
    return total;
}

// Depth-first, a node's own entries in slot order before its sub-nodes: a
// deterministic order, identical for any two maps holding the same entries
// because the representation is canonical.
static term *champ_fill_rec(term node, term *out)
{
    size_t entries = champ_entries(node);
    size_t nodes = champ_nodes(node);
    for (size_t i = 0; i < entries; i++) {
        *out++ = champ_entry_key(node, i);
        *out++ = champ_entry_value(node, i);
    }
    for (size_t i = 0; i < nodes; i++) {
        out = champ_fill_rec(champ_sub(node, entries, i), out);
    }
    return out;
}

void termmap_champ_fill_array(term node, term *out)
{
    if (term_is_nil(node)) {
        return;
    }
    champ_fill_rec(node, out);
}

// Erlang orders maps by their keys, and the external term format and the
// printer follow suit, so the few callers that expose an order sort the walk's
// output. Heapsort keeps that allocation-free; the pairs move two words at a
// time.
static inline void champ_swap_pair(term *arr, size_t i, size_t j)
{
    term k = arr[2 * i];
    term v = arr[2 * i + 1];
    arr[2 * i] = arr[2 * j];
    arr[2 * i + 1] = arr[2 * j + 1];
    arr[2 * j] = k;
    arr[2 * j + 1] = v;
}

static void champ_sift_down(term *arr, size_t root, size_t n, GlobalContext *global, bool *oom)
{
    for (;;) {
        size_t child = 2 * root + 1;
        if (child >= n) {
            return;
        }
        if (child + 1 < n) {
            TermCompareResult r
                = term_compare(arr[2 * child], arr[2 * (child + 1)], TermCompareExact, global);
            if (UNLIKELY(r == TermCompareMemoryAllocFail)) {
                *oom = true;
                return;
            }
            if (r == TermLessThan) {
                child++;
            }
        }
        TermCompareResult r
            = term_compare(arr[2 * root], arr[2 * child], TermCompareExact, global);
        if (UNLIKELY(r == TermCompareMemoryAllocFail)) {
            *oom = true;
            return;
        }
        if (r != TermLessThan) {
            return;
        }
        champ_swap_pair(arr, root, child);
        root = child;
    }
}

bool termmap_champ_fill_array_sorted(term node, term *out, size_t n, GlobalContext *global)
{
    termmap_champ_fill_array(node, out);
    if (n < 2) {
        return true;
    }
    bool oom = false;
    for (size_t i = n / 2; i-- > 0;) {
        champ_sift_down(out, i, n, global, &oom);
        if (UNLIKELY(oom)) {
            return false;
        }
    }
    for (size_t end = n - 1; end > 0; end--) {
        champ_swap_pair(out, 0, end);
        champ_sift_down(out, 0, end, global, &oom);
        if (UNLIKELY(oom)) {
            return false;
        }
    }
    return true;
}

static void champ_foreach_rec(term node, void (*fn)(term, term, void *), void *arg)
{
    size_t entries = champ_entries(node);
    size_t nodes = champ_nodes(node);
    for (size_t i = 0; i < entries; i++) {
        fn(champ_entry_key(node, i), champ_entry_value(node, i), arg);
    }
    for (size_t i = 0; i < nodes; i++) {
        champ_foreach_rec(champ_sub(node, entries, i), fn, arg);
    }
}

void termmap_champ_foreach(term node, void (*fn)(term key, term value, void *arg), void *arg)
{
    if (term_is_nil(node)) {
        return;
    }
    champ_foreach_rec(node, fn, arg);
}

// The list is built back to front, so the walk runs in reverse.
static term champ_kv_list_rec(term node, term acc, Heap *heap)
{
    size_t entries = champ_entries(node);
    size_t nodes = champ_nodes(node);
    for (size_t i = nodes; i-- > 0;) {
        acc = champ_kv_list_rec(champ_sub(node, entries, i), acc, heap);
    }
    for (size_t i = entries; i-- > 0;) {
        acc = term_list_prepend(champ_entry_value(node, i), acc, heap);
        acc = term_list_prepend(champ_entry_key(node, i), acc, heap);
    }
    return acc;
}

term termmap_champ_to_kv_list(term node, term acc, Heap *heap)
{
    if (term_is_nil(node)) {
        return acc;
    }
    return champ_kv_list_rec(node, acc, heap);
}

static size_t champ_select_rec(term node, size_t index, term *key, term *value)
{
    size_t entries = champ_entries(node);
    if (index < entries) {
        *key = champ_entry_key(node, index);
        *value = champ_entry_value(node, index);
        return 0;
    }
    index -= entries;
    size_t nodes = champ_nodes(node);
    for (size_t i = 0; i < nodes; i++) {
        term sub = champ_sub(node, entries, i);
        size_t sub_count = termmap_champ_count(sub);
        if (index < sub_count) {
            return champ_select_rec(sub, index, key, value);
        }
        index -= sub_count;
    }
    return index + 1;
}

term termmap_champ_select_key(term node, size_t index)
{
    term key = term_invalid_term();
    term value;
    if (term_is_nil(node)) {
        return key;
    }
    champ_select_rec(node, index, &key, &value);
    return key;
}

term termmap_champ_select_value(term node, size_t index)
{
    term key;
    term value = term_invalid_term();
    if (term_is_nil(node)) {
        return value;
    }
    champ_select_rec(node, index, &key, &value);
    return value;
}

static int champ_rank_rec(term node, term key, GlobalContext *global, size_t *seen)
{
    size_t entries = champ_entries(node);
    size_t nodes = champ_nodes(node);
    for (size_t i = 0; i < entries; i++) {
        if (champ_key_eq(champ_entry_key(node, i), key, global)) {
            return (int) (*seen + i);
        }
    }
    *seen += entries;
    for (size_t i = 0; i < nodes; i++) {
        int r = champ_rank_rec(champ_sub(node, entries, i), key, global, seen);
        if (r >= 0) {
            return r;
        }
    }
    return -1;
}

int termmap_champ_rank(term node, term key, GlobalContext *global)
{
    if (term_is_nil(node)) {
        return -1;
    }
    // Answer absence with one hashed descent; only a hit pays the walk.
    if (term_is_invalid_term(termmap_champ_get(node, key, global))) {
        return -1;
    }
    size_t seen = 0;
    return champ_rank_rec(node, key, global, &seen);
}

// ---- equality ---------------------------------------------------------------

// Two maps with the same entries have the same shape, so equality is a parallel
// walk that stops at the first differing bitmap and skips shared sub-nodes.
static int champ_equal_rec(term a, term b, GlobalContext *global)
{
    if (a == b) {
        return 1;
    }
    bool a_coll = champ_is_collision(a);
    if (a_coll != champ_is_collision(b)) {
        return 0;
    }
    size_t entries = champ_entries(a);
    if (entries != champ_entries(b)) {
        return 0;
    }
    if (!a_coll && (champ_datamap(a) != champ_datamap(b) || champ_nodemap(a) != champ_nodemap(b))) {
        return 0;
    }
    for (size_t i = 0; i < entries; i++) {
        if (a_coll) {
            // Collision entries are in insertion order, not slot order.
            size_t j = 0;
            size_t count = entries;
            for (; j < count; j++) {
                if (champ_key_eq(champ_entry_key(a, i), champ_entry_key(b, j), global)) {
                    break;
                }
            }
            if (j == count) {
                return 0;
            }
            TermCompareResult r = term_compare(champ_entry_value(a, i), champ_entry_value(b, j),
                TermCompareExact | TermCompareEqualOnly, global);
            if (r == TermCompareMemoryAllocFail) {
                return -1;
            }
            if (r != TermEquals) {
                return 0;
            }
            continue;
        }
        term ka = champ_entry_key(a, i);
        term kb = champ_entry_key(b, i);
        if (ka != kb) {
            TermCompareResult r
                = term_compare(ka, kb, TermCompareExact | TermCompareEqualOnly, global);
            if (r == TermCompareMemoryAllocFail) {
                return -1;
            }
            if (r != TermEquals) {
                return 0;
            }
        }
        term va = champ_entry_value(a, i);
        term vb = champ_entry_value(b, i);
        if (va != vb) {
            TermCompareResult r
                = term_compare(va, vb, TermCompareExact | TermCompareEqualOnly, global);
            if (r == TermCompareMemoryAllocFail) {
                return -1;
            }
            if (r != TermEquals) {
                return 0;
            }
        }
    }
    size_t nodes = champ_nodes(a);
    for (size_t i = 0; i < nodes; i++) {
        int r = champ_equal_rec(champ_sub(a, entries, i), champ_sub(b, entries, i), global);
        if (r != 1) {
            return r;
        }
    }
    return 1;
}

int termmap_champ_equal(term a, term b, GlobalContext *global, bool exact)
{
    if (a == b) {
        return 1;
    }
    if (!exact) {
        // 1 and 1.0 are == but hash apart, so the shapes carry no information.
        return -1;
    }
    if (term_is_nil(a) || term_is_nil(b)) {
        return term_is_nil(a) && term_is_nil(b);
    }
    return champ_equal_rec(a, b, global);
}

// ---- cursor -----------------------------------------------------------------
//
// A cursor is a list of {Node, Index} frames, innermost first. In every frame
// but the head, Index >= entries(Node) and names the sub-node being descended;
// in the head frame Index < entries(Node) and names the current entry.

static term champ_frame(term node, size_t index, term rest, Heap *heap)
{
    term frame = term_alloc_tuple(2, heap);
    term_put_tuple_element(frame, 0, node);
    term_put_tuple_element(frame, 1, term_from_int((avm_int_t) index));
    return term_list_prepend(frame, rest, heap);
}

// Push frames from `node` down to the first node holding an entry.
static term champ_enter(term node, term stack, Heap *heap)
{
    for (;;) {
        size_t entries = champ_entries(node);
        if (entries > 0) {
            return champ_frame(node, 0, stack, heap);
        }
        // Canonical form: a node with no entries has at least one sub-node.
        stack = champ_frame(node, 0, stack, heap);
        node = champ_sub(node, 0, 0);
    }
}

term termmap_champ_cursor_first(term root, Heap *heap)
{
    if (term_is_nil(root)) {
        return term_nil();
    }
    return champ_enter(root, term_nil(), heap);
}

size_t termmap_champ_cursor_reserve(term root, bool first)
{
    UNUSED(root);
    // One step pops back up and descends again, at most a full path each way.
    size_t path = (CHAMP_MAX_LEVELS + 2) * CHAMP_CURSOR_FRAME_WORDS;
    return first ? 2 * path : path;
}

bool termmap_champ_cursor_next(term cursor, term *key, term *value, term *next_cursor, Heap *heap)
{
    if (term_is_nil(cursor)) {
        return false;
    }
    term frame = term_get_list_head(cursor);
    term node = term_get_tuple_element(frame, 0);
    size_t index = (size_t) term_to_int(term_get_tuple_element(frame, 1));
    *key = champ_entry_key(node, index);
    *value = champ_entry_value(node, index);

    term rest = term_get_list_tail(cursor);
    size_t entries = champ_entries(node);
    size_t next = index + 1;
    if (next < entries) {
        *next_cursor = champ_frame(node, next, rest, heap);
        return true;
    }
    // Entries exhausted: descend into this node's sub-nodes, and when those run
    // out, climb to the parent's next sub-node.
    size_t child = 0;
    for (;;) {
        size_t nodes = champ_nodes(node);
        if (child < nodes) {
            term parent_stack = champ_frame(node, entries + child, rest, heap);
            *next_cursor = champ_enter(champ_sub(node, entries, child), parent_stack, heap);
            return true;
        }
        if (term_is_nil(rest)) {
            *next_cursor = term_nil();
            return true;
        }
        term up = term_get_list_head(rest);
        node = term_get_tuple_element(up, 0);
        size_t up_index = (size_t) term_to_int(term_get_tuple_element(up, 1));
        rest = term_get_list_tail(rest);
        entries = champ_entries(node);
        child = up_index - entries + 1;
    }
}

// ---- bulk build -------------------------------------------------------------
//
// Counting-sort the entries by the slot their hash selects, level by level.
// Two index arrays are used alternately: a level reads one and writes the
// other over the same range, and the ranges its children get are disjoint
// subranges of that, so no level can overwrite what a caller still needs.

struct ChampCtx
{
    const term *keys;
    const term *values;
    const uint32_t *hashes;
};

static void champ_partition(
    const struct ChampCtx *c, const int *src, int n, int shift, int *dst, int *starts)
{
    int counts[CHAMP_WIDTH + 1];
    memset(counts, 0, sizeof(counts));
    for (int i = 0; i < n; i++) {
        counts[champ_slot(c->hashes[src[i]], shift) + 1]++;
    }
    for (int s = 0; s < CHAMP_WIDTH; s++) {
        counts[s + 1] += counts[s];
    }
    memcpy(starts, counts, sizeof(counts));
    int off[CHAMP_WIDTH];
    memcpy(off, counts, sizeof(off));
    for (int i = 0; i < n; i++) {
        dst[off[champ_slot(c->hashes[src[i]], shift)]++] = src[i];
    }
}

static size_t champ_size_rec(
    const struct ChampCtx *c, int *src, int *dst, int n, int shift, bool *collided)
{
    if (shift >= CHAMP_HASH_BITS) {
        *collided = true;
        return (size_t) (1 + CHAMP_FIRST_ENTRY + 2 * n);
    }
    int starts[CHAMP_WIDTH + 1];
    champ_partition(c, src, n, shift, dst, starts);
    size_t words = 0;
    int entries = 0;
    int nodes = 0;
    for (int s = 0; s < CHAMP_WIDTH; s++) {
        int cnt = starts[s + 1] - starts[s];
        if (cnt == 1) {
            entries++;
        } else if (cnt > 1) {
            nodes++;
            words += champ_size_rec(
                c, dst + starts[s], src + starts[s], cnt, shift + CHAMP_BITS, collided);
        }
    }
    return words + (size_t) (1 + CHAMP_FIRST_ENTRY + 2 * entries + nodes);
}

static term champ_build_rec(
    const struct ChampCtx *c, int *src, int *dst, int n, int shift, Heap *heap, bool *collided)
{
    if (shift >= CHAMP_HASH_BITS) {
        *collided = true;
        term node = champ_collision_new(heap, (size_t) n);
        for (int i = 0; i < n; i++) {
            term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2 * i, c->keys[src[i]]);
            term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2 * i + 1, c->values[src[i]]);
        }
        return node;
    }
    int starts[CHAMP_WIDTH + 1];
    champ_partition(c, src, n, shift, dst, starts);

    uint32_t datamap = 0;
    uint32_t nodemap = 0;
    int entries = 0;
    int nodes = 0;
    for (int s = 0; s < CHAMP_WIDTH; s++) {
        int cnt = starts[s + 1] - starts[s];
        if (cnt == 1) {
            datamap |= ((uint32_t) 1) << s;
            entries++;
        } else if (cnt > 1) {
            nodemap |= ((uint32_t) 1) << s;
            nodes++;
        }
    }
    // Build the children first: they cannot move a node that does not exist yet.
    term children[CHAMP_WIDTH];
    int ni = 0;
    for (int s = 0; s < CHAMP_WIDTH; s++) {
        int cnt = starts[s + 1] - starts[s];
        if (cnt > 1) {
            children[ni++] = champ_build_rec(
                c, dst + starts[s], src + starts[s], cnt, shift + CHAMP_BITS, heap, collided);
        }
    }
    term node = champ_node_new(heap, datamap, nodemap, (size_t) entries, (size_t) nodes);
    int di = 0;
    for (int s = 0; s < CHAMP_WIDTH; s++) {
        if (starts[s + 1] - starts[s] == 1) {
            int e = dst[starts[s]];
            term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2 * di, c->keys[e]);
            term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2 * di + 1, c->values[e]);
            di++;
        }
    }
    for (int i = 0; i < nodes; i++) {
        term_put_tuple_element(node, CHAMP_FIRST_ENTRY + 2 * entries + i, children[i]);
    }
    return node;
}

size_t termmap_champ_from_array_heap_size(size_t n)
{
    if (n == 0) {
        return 0;
    }
    // Every entry costs two words. A node exists only where two entries share a
    // hash prefix, so each of the CHAMP_MAX_LEVELS levels holds at most n/2 of
    // them (and one more level's worth covers the collision nodes), each
    // costing its header, its two bitmaps and its slot in its parent.
    size_t nodes = 1 + (CHAMP_MAX_LEVELS + 1) * (n / 2);
    size_t scratch = n > CHAMP_STACK_SCRATCH_MAX ? CHAMP_HEAP_SCRATCH_WORDS(n) : 0;
    return 2 * n + 4 * nodes + scratch;
}

term termmap_champ_from_array(
    Heap *heap, const term *keys, const term *values, size_t n, GlobalContext *global)
{
    if (n == 0) {
        return term_nil();
    }
    uint32_t stack_hashes[CHAMP_STACK_SCRATCH_MAX];
    int stack_a[CHAMP_STACK_SCRATCH_MAX];
    int stack_b[CHAMP_STACK_SCRATCH_MAX];
    uint32_t *hashes = stack_hashes;
    int *a = stack_a;
    int *b = stack_b;
    term *borrowed = NULL;
    if (n > CHAMP_STACK_SCRATCH_MAX) {
        // Borrow the words termmap_champ_from_array_heap_size set aside for
        // this. They become garbage the next collection reclaims, which is what
        // lets a build of any size stay allocation-free, and so infallible --
        // this runs from a JIT primitive that has no way to report a failure.
        borrowed = memory_heap_alloc(heap, CHAMP_HEAP_SCRATCH_WORDS(n));
        hashes = (uint32_t *) (void *) borrowed;
        a = (int *) (void *) (borrowed + n);
        b = (int *) (void *) (borrowed + 2 * n);
    }
    for (size_t i = 0; i < n; i++) {
        hashes[i] = term_hash(keys[i], global);
        a[i] = (int) i;
    }
    struct ChampCtx c = { .keys = keys, .values = values, .hashes = hashes };
    bool collided = false;
    term root = champ_build_rec(&c, a, b, (int) n, 0, heap, &collided);
    if (collided) {
        champ_root_set_collision(root);
    }
    if (borrowed != NULL) {
        // Leave valid terms behind: the words stay part of the heap until the
        // next collection, and nothing should ever read an index as a term.
        for (size_t i = 0; i < CHAMP_HEAP_SCRATCH_WORDS(n); i++) {
            borrowed[i] = term_nil();
        }
    }
    return root;
}

bool termmap_champ_measure(
    struct ChampBuilder *builder, const term *keys, size_t n, GlobalContext *global)
{
    builder->hashes = NULL;
    builder->idx = NULL;
    builder->scratch = NULL;
    builder->words = 0;
    builder->n = (int) n;
    if (n == 0) {
        return true;
    }
    builder->hashes = malloc(sizeof(uint32_t) * n);
    builder->idx = malloc(sizeof(int) * n);
    builder->scratch = malloc(sizeof(int) * n);
    if (IS_NULL_PTR(builder->hashes) || IS_NULL_PTR(builder->idx)
        || IS_NULL_PTR(builder->scratch)) {
        termmap_champ_builder_free(builder);
        return false;
    }
    for (size_t i = 0; i < n; i++) {
        builder->hashes[i] = term_hash(keys[i], global);
        builder->idx[i] = (int) i;
    }
    struct ChampCtx c = { .keys = keys, .values = NULL, .hashes = builder->hashes };
    builder->collided = false;
    builder->words
        = champ_size_rec(&c, builder->idx, builder->scratch, (int) n, 0, &builder->collided);
    // champ_size_rec consumed idx into scratch; restore it for the build pass.
    for (size_t i = 0; i < n; i++) {
        builder->idx[i] = (int) i;
    }
    return true;
}

term termmap_champ_build(
    struct ChampBuilder *builder, const term *keys, const term *values, Heap *heap)
{
    if (builder->n == 0) {
        return term_nil();
    }
    struct ChampCtx c = { .keys = keys, .values = values, .hashes = builder->hashes };
    bool collided = false;
    term root = champ_build_rec(&c, builder->idx, builder->scratch, builder->n, 0, heap, &collided);
    if (collided) {
        champ_root_set_collision(root);
    }
    return root;
}

void termmap_champ_builder_free(struct ChampBuilder *builder)
{
    free(builder->hashes);
    free(builder->idx);
    free(builder->scratch);
    builder->hashes = NULL;
    builder->idx = NULL;
    builder->scratch = NULL;
}
