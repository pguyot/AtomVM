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

#include "termmap_tree.h"

#include <stdlib.h>

#include "term.h"
#include "utils.h"

// Maximum heap words a single node occupies: a {Size, KV, Children} wrapper
// (4), a KV tuple of 2*BT_MAX_KEYS elements (2*BT_MAX_KEYS + 1), and a children
// tuple of BT_MAX_KEYS + 1 elements (BT_MAX_KEYS + 2).
#define BT_MAX_NODE_WORDS (4 + (2 * BT_MAX_KEYS + 1) + (BT_MAX_KEYS + 2))

// A persistent (path-copying) B-tree, used as the backing store of large maps.
//
// A binary tree would be simpler but, for the read/iterate-heavy workloads that
// build large maps (the Erlang compiler), it loses badly: 6 words/entry (3x a
// flat map) and an O(log2 n)-deep pointer chase per access. A B-tree keeps the
// node count -- and so the live memory (~2 words/entry) and the height -- small,
// so reads stay cache-friendly and in-order iteration by position (select 0,1,
// ..) costs ~O(height) per element, i.e. effectively O(n) overall.
//
// Each node is a 3-tuple {Size, KV, Children}:
//   Size     small int: number of entries in the whole subtree (for select/rank)
//   KV       tuple {K0,V0,K1,V1,...} of the node's own m entries, keys ascending
//   Children NIL for a leaf, else a tuple {C0,..,Cm} of m+1 child subtrees
// The empty tree is NIL. In-order traversal of a node is
//   C0, (K0,V0), C1, (K1,V1), .., C(m-1), (K(m-1),V(m-1)), Cm
// which yields keys ascending -- preserving the sorted-map invariant.
//
// Standard B-tree of minimum degree BT_T: every node holds BT_T-1..2*BT_T-1
// keys (the root may hold fewer). Nodes are plain tuples, so the garbage
// collector, term copier and hasher need no special cases.

#define BT_T 8
#define BT_MAX_KEYS (2 * BT_T - 1) // 15
#define BT_SPLIT_KEYS (2 * BT_T) // a node transiently reaching this is split

#define NODE_SIZE_IDX 0
#define NODE_KV_IDX 1
#define NODE_CHILDREN_IDX 2

static inline term node_kv(term n) { return term_get_tuple_element(n, NODE_KV_IDX); }
static inline term node_children(term n) { return term_get_tuple_element(n, NODE_CHILDREN_IDX); }
static inline bool node_is_leaf(term n) { return term_is_nil(node_children(n)); }
static inline size_t node_nkeys(term n) { return (size_t) term_get_tuple_arity(node_kv(n)) / 2; }
static inline term node_key(term n, size_t i) { return term_get_tuple_element(node_kv(n), 2 * i); }
static inline term node_value(term n, size_t i) { return term_get_tuple_element(node_kv(n), (2 * i) + 1); }
static inline term node_child(term n, size_t i) { return term_get_tuple_element(node_children(n), i); }

size_t termtree_size(term node)
{
    if (term_is_nil(node)) {
        return 0;
    }
    return (size_t) term_to_int(term_get_tuple_element(node, NODE_SIZE_IDX));
}

// Build a node from C arrays. keys/values have nkeys entries; children is NULL
// for a leaf or has nkeys+1 entries. The subtree size is computed from the
// children (or nkeys for a leaf).
static term make_node(Heap *heap, const term *keys, const term *values, size_t nkeys, const term *children)
{
    term kv = term_alloc_tuple(2 * nkeys, heap);
    for (size_t i = 0; i < nkeys; i++) {
        term_put_tuple_element(kv, 2 * i, keys[i]);
        term_put_tuple_element(kv, (2 * i) + 1, values[i]);
    }
    size_t total = nkeys;
    term children_tuple;
    if (children == NULL) {
        children_tuple = term_nil();
    } else {
        children_tuple = term_alloc_tuple(nkeys + 1, heap);
        for (size_t i = 0; i <= nkeys; i++) {
            term_put_tuple_element(children_tuple, i, children[i]);
            total += termtree_size(children[i]);
        }
    }
    term node = term_alloc_tuple(3, heap);
    term_put_tuple_element(node, NODE_SIZE_IDX, term_from_int((avm_int_t) total));
    term_put_tuple_element(node, NODE_KV_IDX, kv);
    term_put_tuple_element(node, NODE_CHILDREN_IDX, children_tuple);
    return node;
}

// Find the position of key in a node's sorted keys. Returns true and sets *pos
// to the matching index if present; otherwise returns false and sets *pos to
// the child index / insertion point.
static bool node_find(term node, term key, GlobalContext *global, size_t *pos)
{
    size_t lo = 0;
    size_t hi = node_nkeys(node);
    while (lo < hi) {
        size_t mid = lo + (hi - lo) / 2;
        term k = node_key(node, mid);
        if (k == key) {
            *pos = mid;
            return true;
        }
        // Small-integer fast path: ~a quarter of the Erlang compiler's large-map
        // key comparisons are bare small integers (the rest are mostly small
        // tuples, already handled inline by term_compare's tuple fast path).
        // term_is_integer is small-int-only, so two such keys -- known unequal
        // (k == key was just ruled out) -- compare numerically without the
        // term_compare call and its preamble.
        if (term_is_integer(key) && term_is_integer(k)) {
            if (term_to_int(key) < term_to_int(k)) {
                hi = mid;
            } else {
                lo = mid + 1;
            }
            continue;
        }
        TermCompareResult cmp = term_compare(key, k, TermCompareExact, global);
        if (cmp == TermLessThan) {
            hi = mid;
        } else if (cmp == TermGreaterThan) {
            lo = mid + 1;
        } else {
            *pos = mid;
            return true;
        }
    }
    *pos = lo;
    return false;
}

term termtree_get(term node, term key, GlobalContext *global)
{
    while (!term_is_nil(node)) {
        size_t pos;
        if (node_find(node, key, global, &pos)) {
            return node_value(node, pos);
        }
        if (node_is_leaf(node)) {
            return term_invalid_term();
        }
        node = node_child(node, pos);
    }
    return term_invalid_term();
}

// Result of inserting into a subtree: either no split (did_split false, the new
// subtree is the function's return value) or a split into two siblings around a
// median entry that the parent must absorb.
struct BTInsert
{
    bool did_split;
    term median_key;
    term median_value;
    term right;
};

static term bt_insert(Heap *heap, term node, term key, term value, GlobalContext *global, struct BTInsert *split);

// Insert into a leaf, splitting if it overflows.
static term leaf_insert(Heap *heap, term node, size_t at, term key, term value, struct BTInsert *split)
{
    size_t m = node_nkeys(node);
    term keys[BT_SPLIT_KEYS];
    term values[BT_SPLIT_KEYS];
    for (size_t i = 0; i < at; i++) {
        keys[i] = node_key(node, i);
        values[i] = node_value(node, i);
    }
    keys[at] = key;
    values[at] = value;
    for (size_t i = at; i < m; i++) {
        keys[i + 1] = node_key(node, i);
        values[i + 1] = node_value(node, i);
    }
    size_t total = m + 1;
    if (total <= BT_MAX_KEYS) {
        split->did_split = false;
        return make_node(heap, keys, values, total, NULL);
    }
    // Split the BT_SPLIT_KEYS entries: left [0,mid), median [mid], right (mid, total).
    size_t mid = total / 2;
    split->did_split = true;
    split->median_key = keys[mid];
    split->median_value = values[mid];
    split->right = make_node(heap, keys + mid + 1, values + mid + 1, total - mid - 1, NULL);
    return make_node(heap, keys, values, mid, NULL);
}

// Insert into an internal node after a child split, splitting this node if it
// overflows. child_left replaces children[at]; the median+child_right are
// inserted at position `at`.
static term internal_insert(Heap *heap, term node, size_t at, term child_left,
    term median_key, term median_value, term child_right, struct BTInsert *split)
{
    size_t m = node_nkeys(node);
    term keys[BT_SPLIT_KEYS];
    term values[BT_SPLIT_KEYS];
    term children[BT_SPLIT_KEYS + 1];
    for (size_t i = 0; i < at; i++) {
        keys[i] = node_key(node, i);
        values[i] = node_value(node, i);
        children[i] = node_child(node, i);
    }
    keys[at] = median_key;
    values[at] = median_value;
    children[at] = child_left;
    children[at + 1] = child_right;
    for (size_t i = at; i < m; i++) {
        keys[i + 1] = node_key(node, i);
        values[i + 1] = node_value(node, i);
        children[i + 2] = node_child(node, i + 1);
    }
    size_t total = m + 1;
    if (total <= BT_MAX_KEYS) {
        split->did_split = false;
        return make_node(heap, keys, values, total, children);
    }
    size_t mid = total / 2;
    split->did_split = true;
    split->median_key = keys[mid];
    split->median_value = values[mid];
    split->right = make_node(heap, keys + mid + 1, values + mid + 1, total - mid - 1, children + mid + 1);
    return make_node(heap, keys, values, mid, children);
}

static term bt_insert(Heap *heap, term node, term key, term value, GlobalContext *global, struct BTInsert *split)
{
    if (term_is_nil(node)) {
        // Empty tree: a one-entry leaf.
        split->did_split = false;
        return make_node(heap, &key, &value, 1, NULL);
    }
    size_t pos;
    bool found = node_find(node, key, global, &pos);
    if (found) {
        // Replace the value in place (copy this node only).
        split->did_split = false;
        size_t m = node_nkeys(node);
        term keys[BT_MAX_KEYS];
        term values[BT_MAX_KEYS];
        term children[BT_MAX_KEYS + 1];
        bool leaf = node_is_leaf(node);
        for (size_t i = 0; i < m; i++) {
            keys[i] = node_key(node, i);
            values[i] = (i == pos) ? value : node_value(node, i);
            if (!leaf) {
                children[i] = node_child(node, i);
            }
        }
        if (!leaf) {
            children[m] = node_child(node, m);
        }
        return make_node(heap, keys, values, m, leaf ? NULL : children);
    }
    if (node_is_leaf(node)) {
        return leaf_insert(heap, node, pos, key, value, split);
    }
    // Descend into child pos.
    struct BTInsert child_split;
    term new_child = bt_insert(heap, node_child(node, pos), key, value, global, &child_split);
    if (!child_split.did_split) {
        // Rebuild this node with children[pos] replaced.
        split->did_split = false;
        size_t m = node_nkeys(node);
        term keys[BT_MAX_KEYS];
        term values[BT_MAX_KEYS];
        term children[BT_MAX_KEYS + 1];
        for (size_t i = 0; i < m; i++) {
            keys[i] = node_key(node, i);
            values[i] = node_value(node, i);
        }
        for (size_t i = 0; i <= m; i++) {
            children[i] = (i == pos) ? new_child : node_child(node, i);
        }
        return make_node(heap, keys, values, m, children);
    }
    return internal_insert(heap, node, pos, new_child,
        child_split.median_key, child_split.median_value, child_split.right, split);
}

term termtree_put(Heap *heap, term node, term key, term value, GlobalContext *global)
{
    struct BTInsert split;
    term root = bt_insert(heap, node, key, value, global, &split);
    if (split.did_split) {
        // Grow taller: a new root with the median and the two halves.
        term children[2] = { root, split.right };
        return make_node(heap, &split.median_key, &split.median_value, 1, children);
    }
    return root;
}

size_t termtree_put_heap_size(size_t size)
{
    // An insert path-copies the root-to-leaf path, and a split may duplicate a
    // node and add one per level (plus a new root). Each node is at most
    // BT_MAX_NODE_WORDS; bound the height generously.
    size_t height = 2;
    size_t n = size + 1;
    while (n > 1) {
        n /= BT_T; // every level multiplies the capacity by at least BT_T
        height++;
    }
    return (height + 1) * 3 * BT_MAX_NODE_WORDS;
}

// Bulk-load a balanced B-tree from n sorted, unique entries in O(n).
static term build_level(Heap *heap, const term *keys, const term *values, size_t n);
// Build the internal level above nkeys separators with nkeys+1 children.
static term build_parent(Heap *heap, const term *keys, const term *values, size_t nkeys, const term *children);

term termtree_from_sorted(Heap *heap, const term *keys, const term *values, size_t n)
{
    if (n == 0) {
        return term_nil();
    }
    return build_level(heap, keys, values, n);
}

size_t termtree_from_sorted_heap_size(size_t n)
{
    // Leaves hold ~BT_MAX_KEYS entries; counting internal levels, the node
    // count is well under n. Bound at a few words per entry.
    return n * 4 + 4 * BT_MAX_NODE_WORDS;
}

static term build_level(Heap *heap, const term *keys, const term *values, size_t n)
{
    if (n <= BT_MAX_KEYS) {
        return make_node(heap, keys, values, n, NULL);
    }
    // Distribute n entries over g leaves with g-1 promoted separators, as evenly
    // as possible, so every separator has a non-empty leaf on each side.
    size_t g = (n + 1 + BT_MAX_KEYS) / (BT_MAX_KEYS + 1); // ceil((n+1)/(MAX+1))
    size_t node_entries = n - (g - 1);
    size_t base = node_entries / g;
    size_t extra = node_entries % g;

    term *up_keys = malloc((g - 1) * sizeof(term));
    term *up_values = malloc((g - 1) * sizeof(term));
    term *children = malloc(g * sizeof(term));
    if (IS_NULL_PTR(up_keys) || IS_NULL_PTR(up_values) || IS_NULL_PTR(children)) {
        free(up_keys);
        free(up_values);
        free(children);
        return term_nil();
    }
    size_t idx = 0;
    for (size_t j = 0; j < g; j++) {
        size_t cnt = base + (j < extra ? 1 : 0);
        children[j] = make_node(heap, keys + idx, values + idx, cnt, NULL);
        idx += cnt;
        if (j < g - 1) {
            up_keys[j] = keys[idx];
            up_values[j] = values[idx];
            idx += 1;
        }
    }
    term result = build_parent(heap, up_keys, up_values, g - 1, children);
    free(up_keys);
    free(up_values);
    free(children);
    return result;
}

static term build_parent(Heap *heap, const term *keys, const term *values, size_t nkeys, const term *children)
{
    if (nkeys <= BT_MAX_KEYS) {
        return make_node(heap, keys, values, nkeys, children);
    }
    // Distribute nkeys separators (nkeys+1 children) over g internal nodes with
    // g-1 promoted separators, evenly.
    size_t g = (nkeys + 1 + BT_MAX_KEYS) / (BT_MAX_KEYS + 1);
    size_t node_keys = nkeys - (g - 1);
    size_t base = node_keys / g;
    size_t extra = node_keys % g;

    term *up_keys = malloc((g - 1) * sizeof(term));
    term *up_values = malloc((g - 1) * sizeof(term));
    term *up_children = malloc(g * sizeof(term));
    if (IS_NULL_PTR(up_keys) || IS_NULL_PTR(up_values) || IS_NULL_PTR(up_children)) {
        free(up_keys);
        free(up_values);
        free(up_children);
        return term_nil();
    }
    size_t kidx = 0;
    size_t cidx = 0;
    for (size_t j = 0; j < g; j++) {
        size_t kc = base + (j < extra ? 1 : 0);
        // A node with kc keys consumes kc+1 consecutive children.
        up_children[j] = make_node(heap, keys + kidx, values + kidx, kc, children + cidx);
        kidx += kc;
        cidx += kc + 1;
        if (j < g - 1) {
            up_keys[j] = keys[kidx];
            up_values[j] = values[kidx];
            kidx += 1;
        }
    }
    term result = build_parent(heap, up_keys, up_values, g - 1, up_children);
    free(up_keys);
    free(up_values);
    free(up_children);
    return result;
}

int termtree_rank(term node, term key, GlobalContext *global)
{
    int acc = 0;
    while (!term_is_nil(node)) {
        size_t pos;
        bool found = node_find(node, key, global, &pos);
        bool leaf = node_is_leaf(node);
        if (!leaf) {
            for (size_t i = 0; i < pos; i++) {
                acc += (int) termtree_size(node_child(node, i));
            }
        }
        acc += (int) pos; // keys before pos in this node
        if (found) {
            if (!leaf) {
                acc += (int) termtree_size(node_child(node, pos));
            }
            return acc;
        }
        if (leaf) {
            return -1;
        }
        node = node_child(node, pos);
    }
    return -1;
}

// Walk to the in-order entry at position `index`, returning its node and the
// key slot within that node.
static bool select_node(term node, size_t index, term *out_node, size_t *out_slot)
{
    while (!term_is_nil(node)) {
        if (node_is_leaf(node)) {
            *out_node = node;
            *out_slot = index;
            return true;
        }
        size_t m = node_nkeys(node);
        size_t i = 0;
        for (;;) {
            size_t cs = termtree_size(node_child(node, i));
            if (index < cs) {
                node = node_child(node, i);
                break;
            }
            index -= cs;
            if (i < m) {
                if (index == 0) {
                    *out_node = node;
                    *out_slot = i;
                    return true;
                }
                index -= 1;
                i++;
            } else {
                // past the last child: shouldn't happen for a valid index
                return false;
            }
        }
    }
    return false;
}

term termtree_to_kv_list(term node, term acc, Heap *heap)
{
    if (term_is_nil(node)) {
        return acc;
    }
    size_t m = node_nkeys(node);
    bool leaf = node_is_leaf(node);
    // Reverse in-order so each prepend builds ascending order: rightmost child,
    // then (key,value) and left child of each slot, descending.
    if (!leaf) {
        acc = termtree_to_kv_list(node_child(node, m), acc, heap);
    }
    for (size_t i = m; i-- > 0;) {
        acc = term_list_prepend(node_value(node, i), acc, heap);
        acc = term_list_prepend(node_key(node, i), acc, heap);
        if (!leaf) {
            acc = termtree_to_kv_list(node_child(node, i), acc, heap);
        }
    }
    return acc;
}

static size_t fill_array(term node, term *out, size_t pos)
{
    if (term_is_nil(node)) {
        return pos;
    }
    size_t m = node_nkeys(node);
    bool leaf = node_is_leaf(node);
    for (size_t i = 0; i < m; i++) {
        if (!leaf) {
            pos = fill_array(node_child(node, i), out, pos);
        }
        out[pos++] = node_key(node, i);
        out[pos++] = node_value(node, i);
    }
    if (!leaf) {
        pos = fill_array(node_child(node, m), out, pos);
    }
    return pos;
}

void termtree_fill_array(term node, term *out)
{
    fill_array(node, out, 0);
}

term termtree_select_key(term node, size_t index)
{
    term n;
    size_t slot;
    if (select_node(node, index, &n, &slot)) {
        return node_key(n, slot);
    }
    return term_invalid_term();
}

term termtree_select_value(term node, size_t index)
{
    term n;
    size_t slot;
    if (select_node(node, index, &n, &slot)) {
        return node_value(n, slot);
    }
    return term_invalid_term();
}
