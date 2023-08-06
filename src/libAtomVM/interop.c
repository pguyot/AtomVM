/*
 * This file is part of AtomVM.
 *
 * Copyright 2018 Davide Bettio <davide@uninstall.it>
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

#include "interop.h"

#include "bitstring.h"
#include "defaultatoms.h"
#include "tempstack.h"
#include "term.h"
#include "term_typedef.h"
#include "valueshashtable.h"
#include <stdint.h>

char *interop_term_to_string(term t, int *ok)
{
    if (term_is_list(t)) {
        return interop_iolist_to_string(t, ok);

    } else if (term_is_binary(t)) {
        char *str = interop_binary_to_string(t);
        *ok = str != NULL;
        return str;

    } else {
        // TODO: implement also for other types?
        *ok = 0;
        return NULL;
    }
}

char *interop_binary_to_string(term binary)
{
    int len = term_binary_size(binary);

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    memcpy(str, term_binary_data(binary), len);

    str[len] = 0;

    return str;
}

char *interop_iolist_to_string(term list, int *ok)
{
    size_t len;
    switch (interop_iolist_size(list, &len)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            *ok = 0;
            return NULL;
        case InteropBadArg:
            *ok = 0;
            return NULL;
    }

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }

    switch (interop_write_iolist(list, str)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(str);
            *ok = 0;
            return NULL;
        case InteropBadArg:
            free(str);
            *ok = 0;
            return NULL;
    }

    str[len] = 0;

    *ok = 1;
    return str;
}

char *interop_list_to_string(term list, int *ok)
{
    int proper;
    int len = term_list_length(list, &proper);
    if (UNLIKELY(!proper)) {
        *ok = 0;
        return NULL;
    }

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }

    term t = list;
    for (int i = 0; i < len; i++) {
        term byte_value_term = term_get_list_head(t);
        if (UNLIKELY(!term_is_integer(byte_value_term))) {
            *ok = 0;
            free(str);
            return NULL;
        }

        if (UNLIKELY(!term_is_uint8(byte_value_term))) {
            *ok = 0;
            free(str);
            return NULL;
        }
        uint8_t byte_value = term_to_uint8(byte_value_term);

        str[i] = (char) byte_value;
        t = term_get_list_tail(t);
    }
    str[len] = 0;

    *ok = 1;
    return str;
}

char *interop_atom_to_string(Context *ctx, term atom)
{
    int atom_index = term_to_atom_index(atom);
    AtomString atom_string = (AtomString) valueshashtable_get_value(ctx->global->atoms_ids_table, atom_index, (unsigned long) NULL);
    int len = atom_string_len(atom_string);

    char *str = malloc(len + 1);
    if (IS_NULL_PTR(str)) {
        return NULL;
    }
    atom_string_to_c(atom_string, str, len + 1);

    return str;
}

term interop_proplist_get_value(term list, term key)
{
    return interop_proplist_get_value_default(list, key, term_nil());
}

term interop_proplist_get_value_default(term list, term key, term default_value)
{
    term t = list;

    while (!term_is_nil(t)) {
        term *t_ptr = term_get_list_ptr(t);

        term head = t_ptr[1];
        if (term_is_tuple(head) && term_get_tuple_arity(head) == 2 && term_get_tuple_element(head, 0) == key) {
            return term_get_tuple_element(head, 1);

        } else if (term_is_atom(head)) {
            if (head == key) {
                return TRUE_ATOM;
            }
        }

        t = *t_ptr;
    }

    return default_value;
}

<<<<<<< HEAD
inline InteropFunctionResult interop_iolist_fold(term t, interop_iolist_fold_fun fold_fun, void *accum)
{
    if (term_is_binary(t)) {
        return fold_fun(t, accum);
=======
static enum UnicodeConversionResult interop_binary_conversion(term t, uint8_t *output, size_t *output_len, size_t *rest_crsr, enum CharDataEncoding in_encoding, enum CharDataEncoding out_encoding)
{
    size_t len = term_binary_size(t);
    if (in_encoding == Latin1Encoding && out_encoding == Latin1Encoding) {
        if (output) {
            memcpy(output, term_binary_data(t), len);
        }
        *output_len = len;
        return UnicodeOk;
    }
    size_t result = 0;
    size_t input_index;
    const uint8_t *input = (const uint8_t *) term_binary_data(t);
    if (in_encoding == Latin1Encoding) {
        for (input_index = 0; input_index < len; input_index++) {
            size_t char_size;
            if (UNLIKELY(!bitstring_utf8_encode(input[input_index], output, &char_size))) {
                *rest_crsr = input_index;
                *output_len = result;
                return UnicodeError;
            }
            result += char_size;
            if (output) {
                output += char_size;
            }
        }
        *output_len = result;
        return UnicodeOk;
    }
    input_index = 0;
    while (input_index < len) {
        size_t char_size;
        uint32_t c;
        enum UnicodeTransformDecodeResult decode_result = bitstring_utf8_decode(input + input_index, len - input_index, &c, &char_size);
        if (UNLIKELY(decode_result != UnicodeTransformDecodeSuccess)) {
            *rest_crsr = input_index;
            *output_len = result;
            return decode_result == UnicodeTransformDecodeIncomplete ? UnicodeIncompleteTransform : UnicodeError;
        }
        if (out_encoding == Latin1Encoding) {
            if (c > 255) {
                *rest_crsr = input_index;
                *output_len = result;
                return UnicodeError;
            }
            if (output) {
                *output++ = c;
            }
            result++;
        } else {
            if (output) {
                memcpy(output, input + input_index, char_size);
                output += char_size;
            }
            result += char_size;
        }
        input_index += char_size;
    }
    *output_len = result;
    return UnicodeOk;
}

static term compute_rest(term t, size_t temp_stack_size, struct TempStack *temp_stack, Heap *heap)
{
    term rest_list = t;
    while (temp_stack_size) {
        t = temp_stack_pop(temp_stack);
        temp_stack_size--;
        rest_list = term_list_prepend(rest_list, t, heap);
    }
    return rest_list;
}

enum UnicodeConversionResult interop_chardata_to_bytes(term t, size_t *size, uint8_t *output, size_t *rest_size, term *rest, enum CharDataEncoding in_encoding, enum CharDataEncoding out_encoding, Heap *heap)
{
    if (term_is_binary(t)) {
        size_t bin_size;
        size_t rest_crsr;
        enum UnicodeConversionResult conv_result = interop_binary_conversion(t, output, &bin_size, &rest_crsr, in_encoding, out_encoding);
        if (size) {
            *size = bin_size;
        }
        if (LIKELY(conv_result == UnicodeOk)) {
            return UnicodeOk;
        }
        if (rest_size) {
            *rest_size = term_sub_binary_heap_size(t, term_binary_size(t) - rest_crsr);
        }
        if (rest) {
            *rest = term_alloc_sub_binary(t, rest_crsr, term_binary_size(t) - rest_crsr, heap);
        }
        return conv_result;
>>>>>>> 729a11a2 (Add missing unicode module)
    }

    if (UNLIKELY(!term_is_list(t))) {
        return UnicodeError;
    }

<<<<<<< HEAD
=======
    unsigned long acc = 0;
    size_t temp_stack_size = 0;

>>>>>>> 729a11a2 (Add missing unicode module)
    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        return UnicodeMemoryAllocFail;
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        temp_stack_destroy(&temp_stack);
        return UnicodeMemoryAllocFail;
    }

    while (!temp_stack_is_empty(&temp_stack)) {
<<<<<<< HEAD
        if (term_is_integer(t) || term_is_binary(t)) {
            InteropFunctionResult result = fold_fun(t, accum);
            if (UNLIKELY(result != InteropOk)) {
                temp_stack_destroy(&temp_stack);
                return result;
            } else {
                t = temp_stack_pop(&temp_stack);
            }
=======
        // If it's a string, input encoding is always unicode
        if (term_is_integer(t)) {
            avm_int_t c = term_to_int(t);
            if (c < 0) {
                if (size) {
                    *size = acc;
                }
                if (rest_size) {
                    *rest_size = temp_stack_size * CONS_SIZE;
                }
                if (rest) {
                    *rest = compute_rest(t, temp_stack_size, &temp_stack, heap);
                }
                temp_stack_destroy(&temp_stack);
                return UnicodeError;
            }
            switch (out_encoding) {
                case Latin1Encoding:
                    if (c > 255) {
                        if (size) {
                            *size = acc;
                        }
                        if (rest_size) {
                            *rest_size = temp_stack_size * CONS_SIZE;
                        }
                        if (rest) {
                            *rest = compute_rest(t, temp_stack_size, &temp_stack, heap);
                        }
                        temp_stack_destroy(&temp_stack);
                        return UnicodeError;
                    }
                    acc++;
                    if (output) {
                        *output++ = c;
                    }
                    break;
                case UTF8Encoding: {
                    size_t char_size;
                    if (UNLIKELY(!bitstring_utf8_encode(c, output, &char_size))) {
                        if (size) {
                            *size = acc;
                        }
                        if (rest_size) {
                            *rest_size = temp_stack_size * CONS_SIZE;
                        }
                        if (rest) {
                            *rest = compute_rest(t, temp_stack_size, &temp_stack, heap);
                        }
                        temp_stack_destroy(&temp_stack);
                        return UnicodeError;
                    }
                    acc += char_size;
                    if (output) {
                        output += char_size;
                    }
                } break;
            }
            t = temp_stack_pop(&temp_stack);
            temp_stack_size--;
>>>>>>> 729a11a2 (Add missing unicode module)

        } else if (term_is_nil(t)) {
            t = temp_stack_pop(&temp_stack);
            temp_stack_size--;

        } else if (term_is_nonempty_list(t)) {
            if (UNLIKELY(temp_stack_push(&temp_stack, term_get_list_tail(t)) != TempStackOk)) {
                temp_stack_destroy(&temp_stack);
                return UnicodeMemoryAllocFail;
            }
            t = term_get_list_head(t);
            temp_stack_size++;

<<<<<<< HEAD
=======
        } else if (term_is_binary(t)) {
            size_t bin_size;
            size_t rest_crsr;
            enum UnicodeConversionResult conv_result = interop_binary_conversion(t, output, &bin_size, &rest_crsr, in_encoding, out_encoding);
            acc += bin_size;
            if (UNLIKELY(conv_result != UnicodeOk)) {
                if (size) {
                    *size = acc;
                }
                if (rest_size) {
                    *rest_size = temp_stack_size * CONS_SIZE + term_sub_binary_heap_size(t, term_binary_size(t) - rest_crsr);
                }
                if (rest) {
                    *rest = compute_rest(term_alloc_sub_binary(t, rest_crsr, term_binary_size(t) - rest_crsr, heap), temp_stack_size, &temp_stack, heap);
                }
                if (conv_result == UnicodeIncompleteTransform) {
                    while (!temp_stack_is_empty(&temp_stack)) {
                        t = temp_stack_pop(&temp_stack);
                        if (!term_is_nil(t)) {
                            conv_result = UnicodeError;
                            break;
                        }
                    }
                }
                temp_stack_destroy(&temp_stack);
                return conv_result;
            }
            if (output) {
                output += bin_size;
            }
            t = temp_stack_pop(&temp_stack);
            temp_stack_size--;

>>>>>>> 729a11a2 (Add missing unicode module)
        } else {
            if (size) {
                *size = acc;
            }
            if (rest_size) {
                *rest_size = temp_stack_size * CONS_SIZE;
            }
            if (rest) {
                *rest = compute_rest(t, temp_stack_size, &temp_stack, heap);
            }

            temp_stack_destroy(&temp_stack);
            return UnicodeError;
        }
    }

    temp_stack_destroy(&temp_stack);

<<<<<<< HEAD
    return InteropOk;
}

static inline InteropFunctionResult size_fold_fun(term t, void *accum)
{
    size_t *size = (size_t *) accum;
    if (term_is_integer(t)) {
        *size += 1;
    } else if (term_is_binary(t)) {
        *size += term_binary_size(t);
=======
    if (size) {
        *size = acc;
    }
    return UnicodeOk;
}

static enum UnicodeConversionResult interop_binary_to_list(term t, uint32_t *output, size_t *output_len, size_t *rest_crsr, enum CharDataEncoding in_encoding)
{
    size_t len = term_binary_size(t);
    size_t result = 0;
    const uint8_t *input = (const uint8_t *) term_binary_data(t);
    if (in_encoding == UTF8Encoding) {
        size_t input_index = 0;
        while (input_index < len) {
            size_t char_size;
            uint32_t c;
            enum UnicodeTransformDecodeResult decode_result = bitstring_utf8_decode(input + input_index, len - input_index, &c, &char_size);
            if (UNLIKELY(decode_result != UnicodeTransformDecodeSuccess)) {
                *rest_crsr = input_index;
                *output_len = result;
                return decode_result == UnicodeTransformDecodeIncomplete ? UnicodeIncompleteTransform : UnicodeError;
            }
            if (output) {
                *output++ = c;
            }
            result += 1;
            input_index += char_size;
        }
    } else {
        result += len;
        if (output) {
            size_t input_index;
            for (input_index = 0; input_index < len; input_index++) {
                *output++ = input[input_index];
            }
        }
    }
    *output_len = result;
    return UnicodeOk;
}

enum UnicodeConversionResult interop_chardata_to_list(term t, size_t *size, uint32_t *output, size_t *rest_size, term *rest, enum CharDataEncoding in_encoding, Heap *heap)
{
    if (term_is_binary(t)) {
        size_t rest_crsr;
        size_t list_size;
        enum UnicodeConversionResult conv_result = interop_binary_to_list(t, output, &list_size, &rest_crsr, in_encoding);
        if (size) {
            *size = list_size;
        }
        if (LIKELY(conv_result == UnicodeOk)) {
            return UnicodeOk;
        }
        if (rest_size) {
            *rest_size = term_sub_binary_heap_size(t, term_binary_size(t) - rest_crsr);
        }
        if (rest) {
            *rest = term_alloc_sub_binary(t, rest_crsr, term_binary_size(t) - rest_crsr, heap);
        }
        return conv_result;
    }

    if (UNLIKELY(!term_is_list(t))) {
        return UnicodeError;
    }

    unsigned long acc = 0;
    size_t temp_stack_size = 0;

    struct TempStack temp_stack;
    if (UNLIKELY(temp_stack_init(&temp_stack) != TempStackOk)) {
        return UnicodeMemoryAllocFail;
    }

    if (UNLIKELY(temp_stack_push(&temp_stack, t) != TempStackOk)) {
        temp_stack_destroy(&temp_stack);
        return UnicodeMemoryAllocFail;
>>>>>>> 729a11a2 (Add missing unicode module)
    }
    return InteropOk;
}

<<<<<<< HEAD
InteropFunctionResult interop_iolist_size(term t, size_t *size)
{
    *size = 0;
    return interop_iolist_fold(t, size_fold_fun, size);
}

static inline InteropFunctionResult write_string_fold_fun(term t, void *accum)
{
    char **p = (char **) accum;
    if (term_is_integer(t)) {
        **p = term_to_int(t);
        (*p)++;
    } else if (term_is_binary(t)) {
        int len = term_binary_size(t);
        memcpy(*p, term_binary_data(t), len);
        *p += len;
    }
    return InteropOk;
=======
    while (!temp_stack_is_empty(&temp_stack)) {
        // If it's a string, input encoding is always unicode
        if (term_is_integer(t)) {
            avm_int_t c = term_to_int(t);
            if (c < 0) {
                if (size) {
                    *size = acc;
                }
                if (rest_size) {
                    *rest_size = temp_stack_size * CONS_SIZE;
                }
                if (rest) {
                    *rest = compute_rest(t, temp_stack_size, &temp_stack, heap);
                }
                temp_stack_destroy(&temp_stack);
                return UnicodeError;
            }
            if (output) {
                *output++ = c;
            }
            acc++;
            t = temp_stack_pop(&temp_stack);
            temp_stack_size--;

        } else if (term_is_nil(t)) {
            t = temp_stack_pop(&temp_stack);
            temp_stack_size--;

        } else if (term_is_nonempty_list(t)) {
            if (UNLIKELY(temp_stack_push(&temp_stack, term_get_list_tail(t)) != TempStackOk)) {
                temp_stack_destroy(&temp_stack);
                return UnicodeMemoryAllocFail;
            }
            t = term_get_list_head(t);
            temp_stack_size++;

        } else if (term_is_binary(t)) {
            size_t rest_crsr;
            size_t list_size;
            enum UnicodeConversionResult conv_result = interop_binary_to_list(t, output, &list_size, &rest_crsr, in_encoding);
            acc += list_size;
            if (UNLIKELY(conv_result != UnicodeOk)) {
                if (size) {
                    *size = acc;
                }
                if (rest_size) {
                    *rest_size = temp_stack_size * CONS_SIZE + term_sub_binary_heap_size(t, term_binary_size(t) - rest_crsr);
                }
                if (rest) {
                    *rest = compute_rest(term_alloc_sub_binary(t, rest_crsr, term_binary_size(t) - rest_crsr, heap), temp_stack_size, &temp_stack, heap);
                }
                if (conv_result == UnicodeIncompleteTransform) {
                    while (!temp_stack_is_empty(&temp_stack)) {
                        t = temp_stack_pop(&temp_stack);
                        if (!term_is_nil(t)) {
                            conv_result = UnicodeError;
                            break;
                        }
                    }
                }
                temp_stack_destroy(&temp_stack);
                return conv_result;
            }
            if (output) {
                output += list_size;
            }
            t = temp_stack_pop(&temp_stack);
            temp_stack_size--;

        } else {
            if (size) {
                *size = acc;
            }
            if (rest_size) {
                *rest_size = temp_stack_size * CONS_SIZE;
            }
            if (rest) {
                *rest = compute_rest(t, temp_stack_size, &temp_stack, heap);
            }

            temp_stack_destroy(&temp_stack);
            return UnicodeError;
        }
    }

    temp_stack_destroy(&temp_stack);

    if (size) {
        *size = acc;
    }
    return UnicodeOk;
>>>>>>> 729a11a2 (Add missing unicode module)
}

InteropFunctionResult interop_write_iolist(term t, char *p)
{
    return interop_iolist_fold(t, write_string_fold_fun, (void *) &p);
}

term interop_map_get_value(GlobalContext *glb, term map, term key)
{
    return interop_map_get_value_default(glb, map, key, term_nil());
}

term interop_map_get_value_default(GlobalContext *glb, term map, term key, term default_value)
{
    int pos = term_find_map_pos(map, key, glb);
    if (pos == TERM_MAP_NOT_FOUND) {
        return default_value;
    } else if (UNLIKELY(pos == TERM_MAP_MEMORY_ALLOC_FAIL)) {
        // TODO: do not crash, handle out of memory
        AVM_ABORT();
    } else {
        return term_get_map_value(map, pos);
    }
}

int interop_atom_term_select_int(const AtomStringIntPair *table, term atom, GlobalContext *global)
{
    int i;
    for (i = 0; table[i].as_val != NULL; i++) {
        if (globalcontext_is_term_equal_to_atom_string(global, atom, table[i].as_val)) {
            return table[i].i_val;
        }
    }
    return table[i].i_val;
}

term interop_kv_get_value_default(term kv, AtomString key, term default_value, GlobalContext *glb)
{
    term key_term = globalcontext_existing_term_from_atom_string(glb, key);
    if (term_is_invalid_term(key_term)) {
        return default_value;
    }

    if (term_is_nonempty_list(kv)) {
        return interop_proplist_get_value_default(kv, key_term, default_value);
    } else if (term_is_map(kv)) {
        return interop_map_get_value_default(glb, kv, key_term, default_value);
    } else {
        return default_value;
    }
}
