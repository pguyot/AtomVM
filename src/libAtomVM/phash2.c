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
 * @file phash2.c
 *
 * @brief erlang:phash2 — bit-exact port of ERTS make_hash2.
 *
 * The hash values MUST match Erlang/OTP exactly: phash2 is documented as
 * portable across ERTS instances and Elixir embeds phash2 values in compiled
 * modules (e.g. Module.Types.Descr BDD keys), so a divergent hash silently
 * produces beams that misbehave when loaded on BEAM. The reference is
 * erts/emulator/beam/erl_term_hashing.c (make_hash2_helper) and atom.c
 * (atom_hash); both are stable formats.
 */

#include "phash2.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "atom_table.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "module.h"
#include "term.h"
#include "utils.h"

/* Bob Jenkins' 96-bit mix, as used by ERTS. */
#define MIX(a, b, c)              \
    do {                          \
        a -= b;                   \
        a -= c;                   \
        a ^= (c >> 13);           \
        b -= c;                   \
        b -= a;                   \
        b ^= (a << 8);            \
        c -= a;                   \
        c -= b;                   \
        c ^= (b >> 13);           \
        a -= b;                   \
        a -= c;                   \
        a ^= (c >> 12);           \
        b -= c;                   \
        b -= a;                   \
        b ^= (a << 16);           \
        c -= a;                   \
        c -= b;                   \
        c ^= (b >> 5);            \
        a -= b;                   \
        a -= c;                   \
        a ^= (c >> 3);            \
        b -= c;                   \
        b -= a;                   \
        b ^= (a << 10);           \
        c -= a;                   \
        c -= b;                   \
        c ^= (b >> 15);           \
    } while (0)

#define HCONST 0x9e3779b9UL /* the golden ratio; an arbitrary value */

/* (HCONST * {2, ..., 23}) mod 2^32 */
#define HCONST_2 0x3c6ef372UL
#define HCONST_3 0xdaa66d2bUL
#define HCONST_4 0x78dde6e4UL
#define HCONST_5 0x1715609dUL
#define HCONST_6 0xb54cda56UL
#define HCONST_7 0x5384540fUL
#define HCONST_9 0x8ff34781UL
#define HCONST_10 0x2e2ac13aUL
#define HCONST_11 0xcc623af3UL
#define HCONST_12 0x6a99b4acUL
#define HCONST_13 0x08d12e65UL
#define HCONST_14 0xa708a81eUL
#define HCONST_15 0x454021d7UL
#define HCONST_16 0xe3779b90UL
#define HCONST_19 0xbe1e08bbUL

#define BLOCK_HASH_BYTES_PER_ITER 12

#define UINT32_HASH_2(Expr1, Expr2, AConst) \
    do {                                    \
        uint32_t a, b;                      \
        a = AConst + (uint32_t) (Expr1);    \
        b = AConst + (uint32_t) (Expr2);    \
        MIX(a, b, hash);                    \
    } while (0)

#define UINT32_HASH(Expr, AConst) UINT32_HASH_2(Expr, 0, AConst)

#define SINT32_HASH(Expr, AConst)                                     \
    do {                                                              \
        int32_t y = (int32_t) (Expr);                                 \
        if (y < 0) {                                                  \
            UINT32_HASH(-y, AConst);                                  \
            /* Negative numbers are unnecessarily mixed twice. */     \
        }                                                             \
        UINT32_HASH(y, AConst);                                       \
    } while (0)

#define IS_SSMALL28(x) (((uint64_t) (((x) >> (28 - 1)) + 1)) < 2)

#define NOT_SSMALL28_HASH(SMALL)              \
    do {                                      \
        uint64_t t;                           \
        uint32_t x, y;                        \
        uint32_t con;                         \
        if (SMALL < 0) {                      \
            con = HCONST_10;                  \
            t = (uint64_t) (-(SMALL));        \
        } else {                              \
            con = HCONST_11;                  \
            t = (uint64_t) (SMALL);           \
        }                                     \
        x = t & 0xffffffff;                   \
        y = t >> 32;                          \
        UINT32_HASH_2(x, y, con);             \
    } while (0)

/* ERTS 64-bit small range: signed 60-bit immediates. Any integer value in
 * this range hashes through the small paths whatever its AtomVM
 * representation (immediate or boxed int64) is. */
#define ERTS_SMALL_MIN (-(((int64_t) 1) << 59))
#define ERTS_SMALL_MAX (((((int64_t) 1) << 59)) - 1)

/* Work-stack markers. They live in the TERM_UNUSED immediate space
 * (low 6 bits 0x2B) so they can never collide with a valid term. */
#define HASH_MAP_TAIL ((term) ((1 << 6) | TERM_UNUSED))
#define HASH_MAP_PAIR ((term) ((2 << 6) | TERM_UNUSED))

struct HashStack
{
    term *data;
    size_t size;
    size_t capacity;
    term initial[64];
};

static inline void hash_stack_init(struct HashStack *s)
{
    s->data = s->initial;
    s->size = 0;
    s->capacity = 64;
}

static inline void hash_stack_destroy(struct HashStack *s)
{
    if (s->data != s->initial) {
        free(s->data);
    }
}

static void hash_stack_grow(struct HashStack *s)
{
    size_t new_capacity = s->capacity * 2;
    if (s->data == s->initial) {
        term *new_data = malloc(new_capacity * sizeof(term));
        if (IS_NULL_PTR(new_data)) {
            AVM_ABORT();
        }
        memcpy(new_data, s->initial, s->size * sizeof(term));
        s->data = new_data;
    } else {
        term *new_data = realloc(s->data, new_capacity * sizeof(term));
        if (IS_NULL_PTR(new_data)) {
            AVM_ABORT();
        }
        s->data = new_data;
    }
    s->capacity = new_capacity;
}

static inline void hash_stack_push(struct HashStack *s, term t)
{
    if (UNLIKELY(s->size == s->capacity)) {
        hash_stack_grow(s);
    }
    s->data[s->size++] = t;
}

static inline term hash_stack_pop(struct HashStack *s)
{
    return s->data[--s->size];
}

static inline bool hash_stack_is_empty(const struct HashStack *s)
{
    return s->size == 0;
}

struct BlockHashCtx
{
    uint32_t a;
    uint32_t b;
    uint32_t c;
};

static inline void block_hash_setup(uint32_t initval, struct BlockHashCtx *ctx)
{
    ctx->a = ctx->b = HCONST;
    ctx->c = initval; /* the previous hash value */
}

static inline void block_hash_buffer(const uint8_t *buf, size_t buf_length, struct BlockHashCtx *ctx)
{
    size_t len = buf_length;
    const uint8_t *k = buf;
    while (len >= BLOCK_HASH_BYTES_PER_ITER) {
        ctx->a += (k[0] + ((uint32_t) k[1] << 8) + ((uint32_t) k[2] << 16) + ((uint32_t) k[3] << 24));
        ctx->b += (k[4] + ((uint32_t) k[5] << 8) + ((uint32_t) k[6] << 16) + ((uint32_t) k[7] << 24));
        ctx->c += (k[8] + ((uint32_t) k[9] << 8) + ((uint32_t) k[10] << 16) + ((uint32_t) k[11] << 24));
        MIX(ctx->a, ctx->b, ctx->c);
        k += BLOCK_HASH_BYTES_PER_ITER;
        len -= BLOCK_HASH_BYTES_PER_ITER;
    }
}

static inline uint32_t block_hash_final_bytes(const uint8_t *buf, size_t buf_length, size_t full_length, struct BlockHashCtx *ctx)
{
    size_t len = buf_length;
    const uint8_t *k = buf;
    ctx->c += full_length;
    switch (len) { /* all the case statements fall through */
        case 11:
            ctx->c += ((uint32_t) k[10] << 24); /* fall through */
        case 10:
            ctx->c += ((uint32_t) k[9] << 16); /* fall through */
        case 9:
            ctx->c += ((uint32_t) k[8] << 8); /* fall through */
        /* the first byte of c is reserved for the length */
        case 8:
            ctx->b += ((uint32_t) k[7] << 24); /* fall through */
        case 7:
            ctx->b += ((uint32_t) k[6] << 16); /* fall through */
        case 6:
            ctx->b += ((uint32_t) k[5] << 8); /* fall through */
        case 5:
            ctx->b += k[4]; /* fall through */
        case 4:
            ctx->a += ((uint32_t) k[3] << 24); /* fall through */
        case 3:
            ctx->a += ((uint32_t) k[2] << 16); /* fall through */
        case 2:
            ctx->a += ((uint32_t) k[1] << 8); /* fall through */
        case 1:
            ctx->a += k[0];
            /* case 0: nothing left to add */
    }
    MIX(ctx->a, ctx->b, ctx->c);
    return ctx->c;
}

static uint32_t block_hash(const uint8_t *block, size_t block_length, uint32_t initval)
{
    struct BlockHashCtx ctx;
    size_t no_bytes_not_in_loop = block_length % BLOCK_HASH_BYTES_PER_ITER;
    size_t no_bytes_to_process_in_loop = block_length - no_bytes_not_in_loop;
    const uint8_t *final_bytes = block + no_bytes_to_process_in_loop;
    block_hash_setup(initval, &ctx);
    block_hash_buffer(block, no_bytes_to_process_in_loop, &ctx);
    return block_hash_final_bytes(final_bytes, no_bytes_not_in_loop, block_length, &ctx);
}

/* ERTS atom hash (atom.c:atom_hash): hashpjw over the name bytes with a
 * latin1 clutch that recombines two-byte UTF-8 sequences whose lead byte is
 * C2/C3 into a single latin1 character. */
static uint32_t phash2_atom_hash_compute(term t, GlobalContext *glb)
{
    atom_index_t index = term_to_atom_index(t);
    size_t len;
    const uint8_t *p = atom_table_get_atom_string(glb->atom_table, index, &len);
    uint32_t h = 0;
    uint32_t g;
    while (len--) {
        uint8_t v = *p++;
        /* latin1 clutch for r16 */
        if (len && (v & 0xFE) == 0xC2 && (*p & 0xC0) == 0x80) {
            v = (v << 6) | (*p & 0x3F);
            p++;
            len--;
        }
        /* normal hashpjw follows for v */
        h = (h << 4) + v;
        g = h & 0xf0000000;
        if (g) {
            h ^= (g >> 24);
            h ^= g;
        }
    }
    return h;
}

/* Lazily grown per-atom hash cache: atom hashes are immutable and hashpjw's
 * xor-folding clears the top nibble, so any value with the top bit set can
 * serve as the empty sentinel. Sizing/stores are benignly racy: the value
 * written for an index is always the same, and the array pointer is only
 * ever swapped after the old contents were copied. */
#define PHASH2_ATOM_CACHE_EMPTY UINT32_C(0xFFFFFFFF)

static uint32_t phash2_atom_hash(term t, GlobalContext *glb)
{
    atom_index_t index = term_to_atom_index(t);
    uint32_t *cache = glb->phash2_atom_cache;
    size_t capacity = glb->phash2_atom_cache_capacity;
    if (LIKELY(cache != NULL && (size_t) index < capacity)) {
        uint32_t cached = cache[index];
        if (LIKELY(cached != PHASH2_ATOM_CACHE_EMPTY)) {
            return cached;
        }
        uint32_t h = phash2_atom_hash_compute(t, glb);
        cache[index] = h;
        return h;
    }

    size_t new_capacity = ((size_t) index + 1024) & ~(size_t) 1023;
    uint32_t *new_cache = malloc(new_capacity * sizeof(uint32_t));
    if (IS_NULL_PTR(new_cache)) {
        return phash2_atom_hash_compute(t, glb);
    }
    memset(new_cache, 0xFF, new_capacity * sizeof(uint32_t));
    if (cache != NULL) {
        memcpy(new_cache, cache, capacity * sizeof(uint32_t));
    }
    uint32_t h = phash2_atom_hash_compute(t, glb);
    new_cache[index] = h;
    // Publish the grown cache; the previous array is retired, not freed, as
    // another scheduler may still be reading it (a few KB per growth step,
    // bounded by the atom count).
    glb->phash2_atom_cache = new_cache;
    glb->phash2_atom_cache_capacity = new_capacity;
    return h;
}

/* Is this term a small integer in the 0..255 range? (ERTS is_byte()) */
static inline bool phash2_is_byte(term t)
{
    if (!term_is_integer(t)) {
        return false;
    }
    avm_int_t v = term_to_int(t);
    return v >= 0 && v <= 255;
}

/* Hash the integer value of an integer term that does not fit the ERTS
 * small range, using the ERTS bignum scheme (64-bit digits of the
 * magnitude, HCONST_10 for negative and HCONST_11 for positive). */
static uint32_t hash_int64_as_bignum(uint32_t hash, int64_t value)
{
    uint32_t con;
    uint64_t magnitude;
    if (value < 0) {
        con = HCONST_10;
        magnitude = (uint64_t) (-(value + 1)) + 1;
    } else {
        con = HCONST_11;
        magnitude = (uint64_t) value;
    }
    uint32_t x = (uint32_t) (magnitude & 0xffffffff);
    uint32_t y = (uint32_t) (magnitude >> 32);
    UINT32_HASH_2(x, y, con);
    return hash;
}

uint32_t phash2_hash(term t, GlobalContext *glb)
{
    uint32_t hash;
    uint32_t hash_xor_pairs = 0;

    /* Optimization. Simple cases before declaration of estack. */
    if (term_is_atom(t)) {
        /* Fast, but the poor hash value should be mixed. */
        return phash2_atom_hash(t, glb);
    }
    if (term_is_any_integer(t) && !term_is_bigint(t)) {
        int64_t small = term_maybe_unbox_int64(t);
        if (small >= ERTS_SMALL_MIN && small <= ERTS_SMALL_MAX) {
            if (!IS_SSMALL28(small)) {
                hash = 0;
                NOT_SSMALL28_HASH(small);
                return hash;
            }
            hash = 0;
            SINT32_HASH(small, HCONST);
            return hash;
        }
        /* 64-bit value out of the ERTS small range: bignum on BEAM. */
        return hash_int64_as_bignum(0, small);
    }

    struct HashStack stack;
    hash_stack_init(&stack);
    hash = 0;

    term term_v = t;
    for (;;) {
        if (term_is_nonempty_list(term_v)) {
            /* Optimization for strings. */
            uint32_t sh = 0;
            int c = 0;
            term head = term_get_list_head(term_v);
            while (phash2_is_byte(head)) {
                sh = (sh << 8) + (uint32_t) term_to_int(head);
                if (c == 3) {
                    UINT32_HASH(sh, HCONST_4);
                    c = sh = 0;
                } else {
                    c++;
                }
                term_v = term_get_list_tail(term_v);
                if (!term_is_nonempty_list(term_v)) {
                    break;
                }
                head = term_get_list_head(term_v);
            }
            if (c > 0) {
                UINT32_HASH(sh, HCONST_4);
            }
            if (term_is_nonempty_list(term_v)) {
                hash_stack_push(&stack, term_get_list_tail(term_v));
                term_v = term_get_list_head(term_v);
            }
            /* else: term_v is the (possibly improper) tail, handled next */
            if (term_is_nonempty_list(term_v)) {
                continue;
            }
            /* fall through to hash the non-cons term_v (nil or improper
             * tail) on the next loop iteration */
            continue;
        } else if (term_is_tuple(term_v)) {
            int arity = term_get_tuple_arity(term_v);
            UINT32_HASH(arity, HCONST_9);
            if (arity == 0) {
                goto hash2_common;
            }
            for (int i = arity - 1; i >= 1; i--) {
                hash_stack_push(&stack, term_get_tuple_element(term_v, i));
            }
            term_v = term_get_tuple_element(term_v, 0);
            continue;
        } else if (term_is_map(term_v)) {
            int size = term_get_map_size(term_v);
            UINT32_HASH(size, HCONST_16);
            if (size == 0) {
                goto hash2_common;
            }
            /* Pair hashes are xor:ed together so the traversal order does
             * not matter (same scheme as ERTS). */
            hash_stack_push(&stack, (term) hash_xor_pairs);
            hash_stack_push(&stack, (term) hash);
            hash_stack_push(&stack, HASH_MAP_TAIL);
            hash = 0;
            hash_xor_pairs = 0;
            for (int i = size - 1; i >= 0; i--) {
                hash_stack_push(&stack, HASH_MAP_PAIR);
                hash_stack_push(&stack, term_get_map_value(term_v, i));
                hash_stack_push(&stack, term_get_map_key(term_v, i));
            }
            goto hash2_common;
        } else if (term_is_bigint(term_v)) {
            const intn_digit_t *digits;
            size_t digits_len;
            intn_integer_sign_t sign;
            term_to_bigint(term_v, &digits, &digits_len, &sign);
            /* Normalize: ERTS bignums have no leading zero digits (the
             * AtomVM digit array is zero-extended to a term multiple). */
            while (digits_len > 0 && digits[digits_len - 1] == 0) {
                digits_len--;
            }
            uint32_t con = (sign == IntNNegativeInteger) ? HCONST_10 : HCONST_11;
            size_t i = 0;
            do {
                uint32_t x = i < digits_len ? digits[i] : 0;
                i++;
                uint32_t y = i < digits_len ? digits[i] : 0;
                i++;
                UINT32_HASH_2(x, y, con);
            } while (i < digits_len);
            goto hash2_common;
        } else if (term_is_any_integer(term_v)) {
            int64_t small = term_maybe_unbox_int64(term_v);
            if (small >= ERTS_SMALL_MIN && small <= ERTS_SMALL_MAX) {
                if (!IS_SSMALL28(small)) {
                    NOT_SSMALL28_HASH(small);
                } else {
                    SINT32_HASH(small, HCONST);
                }
            } else {
                hash = hash_int64_as_bignum(hash, small);
            }
            goto hash2_common;
        } else if (term_is_atom(term_v)) {
            if (hash == 0) {
                /* Fast, but the poor hash value should be mixed. */
                hash = phash2_atom_hash(term_v, glb);
            } else {
                UINT32_HASH(phash2_atom_hash(term_v, glb), HCONST_3);
            }
            goto hash2_common;
        } else if (term_is_nil(term_v)) {
            if (hash == 0) {
                hash = 3468870702UL;
            } else {
                /* NIL_DEF is the ERTS type code for nil (0x02) */
                UINT32_HASH(2, HCONST_2);
            }
            goto hash2_common;
        } else if (term_is_bitstring(term_v)) {
            uint32_t con = HCONST_13 + hash;
            size_t sz = term_binary_size(term_v);
            uint8_t bitsize = term_is_sub_binary(term_v)
                ? term_get_sub_binary_num_trailing_bits(term_v)
                : 0;
            const uint8_t *bptr = (const uint8_t *) term_binary_data(term_v);
            if (sz == 0 && bitsize == 0) {
                hash = con;
            } else {
                hash = block_hash(bptr, sz, con);
                if (bitsize > 0) {
                    UINT32_HASH_2(bitsize, (bptr[sz] >> (8 - bitsize)), HCONST_15);
                }
            }
            goto hash2_common;
        } else if (term_is_float(term_v)) {
            avm_float_t fd = term_to_float(term_v);
            if (fd == 0.0) {
                /* ensure positive 0.0 */
                fd = 0.0;
            }
            union
            {
                double d;
                uint32_t w[2];
            } ff;
            ff.d = (double) fd;
#if defined(__BYTE_ORDER__) && (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)
            UINT32_HASH_2(ff.w[0], ff.w[1], HCONST_12);
#else
            UINT32_HASH_2(ff.w[1], ff.w[0], HCONST_12);
#endif
            goto hash2_common;
        } else if (term_is_local_pid(term_v)) {
            UINT32_HASH(term_to_local_process_id(term_v), HCONST_5);
            goto hash2_common;
        } else if (term_is_local_port(term_v)) {
            uint64_t number = (uint64_t) term_to_local_process_id(term_v);
            uint32_t low = (uint32_t) (number & 0xffffffff);
            uint32_t high = (uint32_t) ((number >> 32) & 0xffffffff);
            UINT32_HASH_2(low, high, HCONST_6);
            goto hash2_common;
        } else if (term_is_local_reference(term_v)) {
            /* ERTS hashes numbers[0] only. Local refs are not portable
             * between VM instances anyway. */
            uint64_t ticks = term_to_ref_ticks(term_v);
            UINT32_HASH((uint32_t) (ticks & 0xffffffff), HCONST_7);
            goto hash2_common;
        } else if (term_is_external_fun(term_v)) {
            const term *boxed_value = term_to_const_term_ptr(term_v);
            term module = boxed_value[1];
            term function = boxed_value[2];
            avm_int_t arity = term_to_int(boxed_value[3]);
            UINT32_HASH_2(arity, phash2_atom_hash(module, glb), HCONST);
            UINT32_HASH(phash2_atom_hash(function, glb), HCONST_14);
            goto hash2_common;
        } else if (term_is_function(term_v)) {
            const term *boxed_value = term_to_const_term_ptr(term_v);
            size_t num_free = (((uintptr_t) boxed_value[0]) >> 6) - 2;
            term module;
            uint32_t index;
            uint32_t old_uniq;
            size_t free_index;
            if (term_is_atom(boxed_value[1])) {
                module = boxed_value[1];
                index = (uint32_t) term_to_int(boxed_value[4]);
                old_uniq = (uint32_t) term_to_int(boxed_value[5]);
                free_index = 6;
                num_free -= 3;
            } else {
                Module *mod = (Module *) boxed_value[1];
                module = module_get_name(mod);
                uint32_t arity, f_old_index;
                index = (uint32_t) term_to_int(boxed_value[2]);
                module_get_fun_arity_old_index_uniq(mod, index, &arity, &f_old_index, &old_uniq);
                free_index = 3;
            }
            UINT32_HASH_2(num_free, phash2_atom_hash(module, glb), HCONST);
            UINT32_HASH_2(index, old_uniq, HCONST);
            if (num_free == 0) {
                goto hash2_common;
            }
            for (size_t i = num_free - 1; i >= 1; i--) {
                hash_stack_push(&stack, boxed_value[free_index + i]);
            }
            term_v = boxed_value[free_index];
            continue;
        } else {
            /* External pids/ports/refs and exotic types are not needed by
             * the compiler tool-chain; hash their words approximately.
             * These are never portable between VM instances. */
            UINT32_HASH((uint32_t) term_v, HCONST_5);
            goto hash2_common;
        }

    hash2_common:
        /* hash always has the hash value of the previous term, compounded
         * or otherwise. */
        for (;;) {
            if (hash_stack_is_empty(&stack)) {
                hash_stack_destroy(&stack);
                return hash;
            }
            term_v = hash_stack_pop(&stack);
            if (term_v == HASH_MAP_TAIL) {
                hash = (uint32_t) hash_stack_pop(&stack);
                UINT32_HASH(hash_xor_pairs, HCONST_19);
                hash_xor_pairs = (uint32_t) hash_stack_pop(&stack);
                continue;
            }
            if (term_v == HASH_MAP_PAIR) {
                hash_xor_pairs ^= hash;
                hash = 0;
                continue;
            }
            break;
        }
    }
}
