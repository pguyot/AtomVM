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

/*
 * WS2812 (NeoPixel) LED driver using a PIO state machine, e.g. for the RGB
 * LED of Waveshare RP2040-Zero (GP16).
 */

#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <hardware/gpio.h>
#include <hardware/pio.h>
#include <pico/time.h>
#pragma GCC diagnostic pop

#include "context.h"
#include "defaultatoms.h"
#include "erl_nif.h"
#include "erl_nif_priv.h"
#include "globalcontext.h"
#include "interop.h"
#include "memory.h"
#include "nifs.h"
#include "rp2_sys.h"
#include "term.h"

#include "ws2812.pio.h"

// #define ENABLE_TRACE
#include "trace.h"

#define WS2812_FREQ_HZ 800000
// Time to shift out one pixel (24 bits at 800 kHz)
#define WS2812_PIXEL_US 30
// Low time latching the colors. WS2812 needs 50 us, WS2812B up to 280 us.
#define WS2812_RESET_US 300

static ErlNifResourceType *ws2812_resource_type;

struct WS2812Resource
{
    PIO pio;
    uint sm;
    uint offset;
    uint pin;
    absolute_time_t next_write;
};

static term create_pair(Context *ctx, term term1, term term2)
{
    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, term1);
    term_put_tuple_element(ret, 1, term2);
    return ret;
}

static bool get_ws2812_resource(Context *ctx, term resource_term, struct WS2812Resource **rsrc_obj)
{
    void *rsrc_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), resource_term, ws2812_resource_type, &rsrc_obj_ptr))) {
        return false;
    }
    *rsrc_obj = (struct WS2812Resource *) rsrc_obj_ptr;
    return true;
}

static void ws2812_release(struct WS2812Resource *rsrc_obj)
{
    pio_sm_set_enabled(rsrc_obj->pio, rsrc_obj->sm, false);
    pio_remove_program_and_unclaim_sm(&ws2812_program, rsrc_obj->pio, rsrc_obj->sm, rsrc_obj->offset);
    gpio_deinit(rsrc_obj->pin);
    rsrc_obj->pio = NULL;
}

static bool is_color_component(term t)
{
    if (!term_is_integer(t)) {
        return false;
    }
    avm_int_t value = term_to_int(t);
    return value >= 0 && value <= 255;
}

static bool is_rgb_tuple(term t)
{
    return term_is_tuple(t) && term_get_tuple_arity(t) == 3
        && is_color_component(term_get_tuple_element(t, 0))
        && is_color_component(term_get_tuple_element(t, 1))
        && is_color_component(term_get_tuple_element(t, 2));
}

static term nif_ws2812_init(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_integer);
    avm_int_t pin = term_to_int(argv[0]);
    if (UNLIKELY(pin < 0 || pin >= (avm_int_t) NUM_BANK0_GPIOS)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    PIO pio;
    uint sm;
    uint offset;
    if (UNLIKELY(!pio_claim_free_sm_and_add_program_for_gpio_range(&ws2812_program, &pio, &sm, &offset, (uint) pin, 1, true))) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return create_pair(ctx, ERROR_ATOM, globalcontext_make_atom(ctx->global, ATOM_STR("\x7", "no_pio")));
    }

    struct WS2812Resource *rsrc_obj = enif_alloc_resource(ws2812_resource_type, sizeof(struct WS2812Resource));
    if (IS_NULL_PTR(rsrc_obj)) {
        pio_remove_program_and_unclaim_sm(&ws2812_program, pio, sm, offset);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    rsrc_obj->pio = pio;
    rsrc_obj->sm = sm;
    rsrc_obj->offset = offset;
    rsrc_obj->pin = (uint) pin;
    rsrc_obj->next_write = get_absolute_time();
    ws2812_program_init(pio, sm, offset, (uint) pin, WS2812_FREQ_HZ);

    if (UNLIKELY(memory_ensure_free(ctx, TERM_BOXED_RESOURCE_SIZE) != MEMORY_GC_OK)) {
        ws2812_release(rsrc_obj);
        enif_release_resource(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    term obj = term_from_resource(rsrc_obj, &ctx->heap);
    enif_release_resource(rsrc_obj);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ws2812_release(rsrc_obj);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    return create_pair(ctx, OK_ATOM, obj);
}

static term nif_ws2812_deinit(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WS2812Resource *rsrc_obj;
    if (UNLIKELY(!get_ws2812_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->pio)) {
        RAISE_ERROR(BADARG_ATOM);
    }
    ws2812_release(rsrc_obj);

    return OK_ATOM;
}

static term nif_ws2812_write(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);

    struct WS2812Resource *rsrc_obj;
    if (UNLIKELY(!get_ws2812_resource(ctx, argv[0], &rsrc_obj))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    if (IS_NULL_PTR(rsrc_obj->pio)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Validate the whole list first so that a bad element doesn't send a
    // partial frame
    term list = argv[1];
    while (term_is_nonempty_list(list)) {
        if (UNLIKELY(!is_rgb_tuple(term_get_list_head(list)))) {
            RAISE_ERROR(BADARG_ATOM);
        }
        list = term_get_list_tail(list);
    }
    if (UNLIKELY(!term_is_nil(list))) {
        RAISE_ERROR(BADARG_ATOM);
    }

    // Wait for the previous frame to be latched
    busy_wait_until(rsrc_obj->next_write);

    list = argv[1];
    while (term_is_nonempty_list(list)) {
        term pixel = term_get_list_head(list);
        uint32_t r = (uint32_t) term_to_int(term_get_tuple_element(pixel, 0));
        uint32_t g = (uint32_t) term_to_int(term_get_tuple_element(pixel, 1));
        uint32_t b = (uint32_t) term_to_int(term_get_tuple_element(pixel, 2));
        // GRB order, MSB first, in the 24 most significant bits
        pio_sm_put_blocking(rsrc_obj->pio, rsrc_obj->sm, (g << 24) | (r << 16) | (b << 8));
        list = term_get_list_tail(list);
    }

    while (!pio_sm_is_tx_fifo_empty(rsrc_obj->pio, rsrc_obj->sm)) {
        tight_loop_contents();
    }
    // The last pixel may still be shifting out of the OSR
    rsrc_obj->next_write = make_timeout_time_us(WS2812_PIXEL_US + WS2812_RESET_US);

    return OK_ATOM;
}

static void ws2812_resource_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);
    struct WS2812Resource *rsrc_obj = (struct WS2812Resource *) obj;
    if (!IS_NULL_PTR(rsrc_obj->pio)) {
        ws2812_release(rsrc_obj);
    }
}

static const ErlNifResourceTypeInit WS2812ResourceTypeInit = {
    .members = 1,
    .dtor = ws2812_resource_dtor,
};

//
// NIF structs
//
static const struct Nif ws2812_init_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ws2812_init
};
static const struct Nif ws2812_deinit_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ws2812_deinit
};
static const struct Nif ws2812_write_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_ws2812_write
};

static void ws2812_nif_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    ws2812_resource_type = enif_init_resource_type(&env, "ws2812_resource", &WS2812ResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

static const struct Nif *ws2812_nif_get_nif(const char *nifname)
{
    if (strncmp("ws2812:", nifname, 7) != 0) {
        return NULL;
    }
    const char *rest = nifname + 7;
    if (strcmp("init/1", rest) == 0) {
        TRACE("Resolved ws2812 nif %s ...\n", nifname);
        return &ws2812_init_nif;
    }
    if (strcmp("deinit/1", rest) == 0) {
        TRACE("Resolved ws2812 nif %s ...\n", nifname);
        return &ws2812_deinit_nif;
    }
    if (strcmp("write/2", rest) == 0) {
        TRACE("Resolved ws2812 nif %s ...\n", nifname);
        return &ws2812_write_nif;
    }
    return NULL;
}

REGISTER_NIF_COLLECTION(ws2812, ws2812_nif_init, NULL, ws2812_nif_get_nif)
