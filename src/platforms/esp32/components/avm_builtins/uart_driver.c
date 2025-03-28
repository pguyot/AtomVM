/*
 * This file is part of AtomVM.
 *
 * Copyright 2020-2022 Davide Bettio <davide@uninstall.it>
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

#include <sdkconfig.h>
#ifdef CONFIG_AVM_ENABLE_UART_PORT_DRIVER

#include <assert.h>
#include <string.h>

#include <driver/uart.h>
#include <esp_log.h>
#if ESP_IDF_VERSION >= ESP_IDF_VERSION_VAL(5, 2, 0)
#include <soc/gpio_num.h>
#endif

#include "atom.h"
#include "bif.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "module.h"
#include "platform_defaultatoms.h"
#include "port.h"
#include "scheduler.h"
#include "smp.h"
#include "term.h"
#include "utils.h"

#include "trace.h"

#include "esp32_sys.h"
#include "sys.h"

static Context *uart_driver_create_port(GlobalContext *global, term opts);
static NativeHandlerResult uart_driver_consume_mailbox(Context *ctx);

#define TAG "uart_driver"
#define UART_BUF_SIZE 256
#define NO_REF 0
#define NO_READER term_invalid_term()
#define PIN_ERROR -2

#ifndef GPIO_NUM_MAX // remove conditional after ESP_IDF v5.1 is deprecated
#define GPIO_NUM_MAX SOC_GPIO_PIN_COUNT
#endif

struct UARTData
{
    QueueHandle_t rxqueue;
    EventListener listener;
    term reader_process_pid;
    uint64_t reader_ref_ticks;
    uint8_t uart_num;
#ifndef AVM_NO_SMP
    Mutex *reader_lock;
#endif
};

static const AtomStringIntPair parity_table[] = {
    { ATOM_STR("\x4", "none"), UART_PARITY_DISABLE },
    { ATOM_STR("\x4", "even"), UART_PARITY_EVEN },
    { ATOM_STR("\x3", "odd"), UART_PARITY_ODD },
    SELECT_INT_DEFAULT(-1)
};

static const AtomStringIntPair flow_control_table[] = {
    { ATOM_STR("\x4", "none"), UART_HW_FLOWCTRL_DISABLE },
    { ATOM_STR("\x4", "hardware"), UART_HW_FLOWCTRL_CTS_RTS },
    { ATOM_STR("\x3", "software"), -1 },
    SELECT_INT_DEFAULT(-1)
};

enum uart_cmd
{
    UARTInvalidCmd = 0,
    UARTReadCmd = 1,
    UARTWriteCmd = 2,
    UARTCloseCmd = 3,
    UARTCancelCmd = 4
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "read"), UARTReadCmd },
    { ATOM_STR("\x5", "write"), UARTWriteCmd },
    { ATOM_STR("\x5", "close"), UARTCloseCmd },
    { ATOM_STR("\xB", "cancel_read"), UARTCancelCmd },
    SELECT_INT_DEFAULT(UARTInvalidCmd)
};

static void safe_update_reader_data(struct UARTData *uart_data, term pid, uint64_t ref_ticks)
{
    SMP_MUTEX_LOCK(uart_data->reader_lock);
    uart_data->reader_process_pid = pid;
    uart_data->reader_ref_ticks = ref_ticks;
    SMP_MUTEX_UNLOCK(uart_data->reader_lock);
}

EventListener *uart_interrupt_callback(GlobalContext *glb, EventListener *listener)
{
    struct UARTData *uart_data = GET_LIST_ENTRY(listener, struct UARTData, listener);

    uart_event_t event;
    if (xQueueReceive(uart_data->rxqueue, (void *) &event, (TickType_t) 0)) {
        switch (event.type) {
            case UART_DATA_BREAK:
            case UART_DATA:
                if (uart_data->reader_process_pid != term_invalid_term()) {
                    int bin_size = term_binary_heap_size(event.size);

                    Heap heap;
                    if (UNLIKELY(memory_init_heap(&heap, bin_size + REF_SIZE + TUPLE_SIZE(2) * 2) != MEMORY_GC_OK)) {
                        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                        AVM_ABORT();
                    }

                    term bin = term_create_uninitialized_binary(event.size, &heap, glb);
                    uint8_t *bin_buf = (uint8_t *) term_binary_data(bin);
                    uart_read_bytes(uart_data->uart_num, bin_buf, event.size, portMAX_DELAY);

                    term ok_tuple = term_alloc_tuple(2, &heap);
                    term_put_tuple_element(ok_tuple, 0, OK_ATOM);
                    term_put_tuple_element(ok_tuple, 1, bin);

                    term ref = term_from_ref_ticks(uart_data->reader_ref_ticks, &heap);

                    term result_tuple = term_alloc_tuple(2, &heap);
                    term_put_tuple_element(result_tuple, 0, ref);
                    term_put_tuple_element(result_tuple, 1, ok_tuple);

                    int local_pid = term_to_local_process_id(uart_data->reader_process_pid);
                    globalcontext_send_message(glb, local_pid, result_tuple);

                    memory_destroy_heap(&heap, glb);
                    safe_update_reader_data(uart_data, NO_READER, NO_REF);
                }
                break;
            case UART_FIFO_OVF:
                ESP_LOGE(TAG, "FIFO overflow!");
                break;
            case UART_BUFFER_FULL:
                ESP_LOGW(TAG, "buffer is full!");
                break;
            case UART_BREAK:
                // TODO: handle single NULL char event?, or treat this like UART_DATA & UART_DATA_BREAK?
                break;
            case UART_PARITY_ERR:
                ESP_LOGE(TAG, "parity error detected!");
                break;
            case UART_FRAME_ERR:
                ESP_LOGE(TAG, "frame error detected!");
                break;
            case UART_PATTERN_DET:
                // MAYBE: add pattern detection for incoming uart messages
                break;
            default:
                break;
        }
    }

    return listener;
}

static int get_uart_pin_opt(term opts, term pin_name)
{
    _Static_assert(PIN_ERROR < UART_PIN_NO_CHANGE);
    term value = interop_proplist_get_value_default(opts, pin_name, DEFAULT_ATOM);
    if (value == DEFAULT_ATOM) {
        return UART_PIN_NO_CHANGE;
    } else if (!term_is_integer(value)) {
        ESP_LOGE(TAG, "pin must be an integer!");
        return PIN_ERROR;
    } else {
        int pin_num = term_to_int(value);
        if (UNLIKELY(pin_num < UART_PIN_NO_CHANGE || pin_num > GPIO_NUM_MAX)) {
            ESP_LOGE(TAG, "pin number %i is out of range [default | -1..%i]", pin_num, GPIO_NUM_MAX);
            return PIN_ERROR;
        }
        return term_to_int(value);
    }
}

Context *uart_driver_create_port(GlobalContext *global, term opts)
{
    Context *ctx = context_new(global);

    term uart_name_term = interop_kv_get_value_default(opts, ATOM_STR("\xA", "peripheral"),
        UNDEFINED_ATOM, global);
    term uart_speed_term = interop_proplist_get_value_default(opts, SPEED_ATOM, term_from_int(115200));

    term data_bits_term = interop_proplist_get_value_default(opts, DATA_BITS_ATOM, term_from_int(8));
    term stop_bits_term = interop_proplist_get_value_default(opts, STOP_BITS_ATOM, term_from_int(1));
    term flow_control_term = interop_proplist_get_value_default(opts, FLOW_CONTROL_ATOM, NONE_ATOM);
    term parity_term = interop_proplist_get_value_default(opts, PARITY_ATOM, NONE_ATOM);

    int tx_pin = get_uart_pin_opt(opts, TX_ATOM);
    int rx_pin = get_uart_pin_opt(opts, RX_ATOM);
    int rts_pin = get_uart_pin_opt(opts, RTS_ATOM);
    int cts_pin = get_uart_pin_opt(opts, CTS_ATOM);
    if (UNLIKELY(tx_pin == PIN_ERROR || rx_pin == PIN_ERROR
            || rts_pin == PIN_ERROR || cts_pin == PIN_ERROR)) {
        // error already logged in get_uart_pin_opts
        return NULL;
    }

    term event_queue_len_term = interop_proplist_get_value_default(opts, EVENT_QUEUE_LEN_ATOM, term_from_int(16));
    if (!term_is_integer(event_queue_len_term)) {
        ESP_LOGE(TAG, "event_queue_len must be an integer");
        return NULL;
    }
    int event_queue_len = term_to_int(event_queue_len_term);

    int ok;
    char *uart_name = interop_term_to_string(uart_name_term, &ok);
    if (!uart_name || !ok) {
        ESP_LOGE(TAG, "name must be character or binary string");
        return NULL;
    }

    uint8_t uart_num;
    if (!strcmp(uart_name, "UART0")) {
        uart_num = UART_NUM_0;
    } else if (!strcmp(uart_name, "UART1")) {
        uart_num = UART_NUM_1;
    }
#if defined(CONFIG_IDF_TARGET_ESP32) || defined(CONFIG_IDF_TARGET_ESP32S3)
    else if (!strcmp(uart_name, "UART2")) {
        uart_num = UART_NUM_2;
    }
#endif
    else {
        free(uart_name);
        ESP_LOGE(TAG, "invalid uart bus name!");
        return NULL;
    }
    free(uart_name);

    avm_int_t uart_speed = term_to_int(uart_speed_term);

    int data_bits;
    switch (term_to_int(data_bits_term)) {
        case 8:
            data_bits = UART_DATA_8_BITS;
            break;
        case 7:
            data_bits = UART_DATA_7_BITS;
            break;
        case 6:
            data_bits = UART_DATA_6_BITS;
            break;
        case 5:
            data_bits = UART_DATA_5_BITS;
            break;
        default:
            ESP_LOGE(TAG, "invalid data_bits!");
            return NULL;
    }

    int stop_bits;
    switch (term_to_int(stop_bits_term)) {
        case 1:
            stop_bits = UART_STOP_BITS_1;
            break;
        case 2:
            stop_bits = UART_STOP_BITS_2;
            break;
        default:
            ESP_LOGE(TAG, "invalid stop_bits!");
            return NULL;
    }

    uart_hw_flowcontrol_t flow_control = interop_atom_term_select_int(flow_control_table, flow_control_term, ctx->global);
    if (flow_control < 0) {
        ESP_LOGE(TAG, "invalid flow_control!");
        return NULL;
    }

    uart_parity_t parity = interop_atom_term_select_int(parity_table, parity_term, ctx->global);
    if (parity < 0) {
        ESP_LOGE(TAG, "invalid parity!");
        return NULL;
    }

    uart_config_t uart_config = {
#if ESP_IDF_VERSION_MAJOR >= 5
        .source_clk = UART_SCLK_DEFAULT,
#endif
        .baud_rate = uart_speed,
        .data_bits = data_bits,
        .parity = parity,
        .stop_bits = stop_bits,
        .flow_ctrl = flow_control
    };
    uart_param_config(uart_num, &uart_config);

    uart_set_pin(uart_num, tx_pin, rx_pin, rts_pin, cts_pin);

    size_t alloc_size = sizeof(struct UARTData);
    struct UARTData *uart_data = malloc(alloc_size);
    if (IS_NULL_PTR(uart_data)) {
        fprintf(stderr, "Failed to allocate memory size %i: %s:%i.\n", (int) alloc_size, __FILE__, __LINE__);
        AVM_ABORT();
    }
    uart_data->listener.handler = uart_interrupt_callback;
    sys_register_listener(global, &uart_data->listener);
    uart_data->reader_process_pid = term_invalid_term();
    uart_data->reader_ref_ticks = 0;
    uart_data->uart_num = uart_num;
    ctx->native_handler = uart_driver_consume_mailbox;
    ctx->platform_data = uart_data;

    if (uart_driver_install(uart_num, UART_BUF_SIZE, 0, event_queue_len, &uart_data->rxqueue, 0) != ESP_OK) {
        ESP_LOGE(TAG, "failed to install uart driver.");
        free(uart_data);
        return NULL;
    }
    uart_data->listener.sender = uart_data->rxqueue;
    if (xQueueAddToSet(uart_data->rxqueue, event_set) != pdPASS) {
        ESP_LOGE(TAG, "failed to establish uart queue.");
        free(uart_data);
        return NULL;
    }

#ifndef AVM_NO_SMP
    uart_data->reader_lock = smp_mutex_create();
#endif

    return ctx;
}

static void uart_driver_do_read(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct UARTData *uart_data = ctx->platform_data;
    term pid = gen_message.pid;
    term ref = gen_message.ref;
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    int local_pid = term_to_local_process_id(pid);

    if (uart_data->reader_process_pid != term_invalid_term()) {
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "[uart_driver_do_read] Failed to allocate space for error tuple");
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }

        term ealready = globalcontext_make_atom(glb, ATOM_STR("\x8", "ealready"));
        term error_tuple = port_create_error_tuple(ctx, ealready);
        port_send_reply(ctx, pid, ref, error_tuple);
        return;
    }

    size_t count;
    uart_get_buffered_data_len(uart_data->uart_num, &count);

    if (count > 0) {
        int bin_size = term_binary_heap_size(count);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, bin_size + TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            ESP_LOGE(TAG, "[uart_driver_do_read] Failed to allocate space for return value");
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
        }

        term bin = term_create_uninitialized_binary(count, &ctx->heap, glb);
        uint8_t *bin_buf = (uint8_t *) term_binary_data(bin);
        uart_read_bytes(uart_data->uart_num, bin_buf, count, portMAX_DELAY);

        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, bin);

        port_send_reply(ctx, pid, ref, ok_tuple);

    } else {
        safe_update_reader_data(uart_data, pid, ref_ticks);
    }
}

static void uart_driver_do_cancel_read(Context *ctx, GenMessage gen_message)
{
    struct UARTData *uart_data = ctx->platform_data;

    safe_update_reader_data(uart_data, NO_READER, NO_REF);

    term pid = gen_message.pid;
    term ref = gen_message.ref;

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "[uart_driver_do_read] Failed to allocate space for return value");
        globalcontext_send_message(ctx->global, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
    }

    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void uart_driver_do_write(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct UARTData *uart_data = ctx->platform_data;
    term msg = gen_message.req;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    term data = term_get_tuple_element(msg, 1);

    size_t buffer_size;
    switch (interop_iolist_size(data, &buffer_size)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            ESP_LOGE(TAG, "[uart_driver_do_write] Failed to allocate memory");
            return;
        case InteropBadArg:
            ESP_LOGE(TAG, "[uart_driver_do_write] Bad arg");
            return;
    }
    void *buffer = malloc(buffer_size);
    switch (interop_write_iolist(data, buffer)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(buffer);
            ESP_LOGE(TAG, "[uart_driver_do_write] Failed to allocate memory");
            return;
        case InteropBadArg:
            free(buffer);
            ESP_LOGE(TAG, "[uart_driver_do_write] Bad arg");
            return;
    }

    uart_write_bytes(uart_data->uart_num, buffer, buffer_size);

    free(buffer);

    int local_pid = term_to_local_process_id(pid);
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "[uart_driver_do_write] Failed to allocate space for return value");
        globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
    }

    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void uart_driver_do_close(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct UARTData *uart_data = ctx->platform_data;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    sys_unregister_listener(glb, &uart_data->listener);
#ifndef AVM_NO_SMP
    smp_mutex_destroy(uart_data->reader_lock);
#endif

    term result;
    esp_err_t err = uart_driver_delete(uart_data->uart_num);
    if (UNLIKELY(err != ESP_OK)) {
        ESP_LOGE(TAG, "Failed to delete UART driver. Error: %s", esp_err_to_name(err));
        result = ERROR_ATOM;
    } else {
        result = OK_ATOM;
    }

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        ESP_LOGE(TAG, "[uart_driver_do_close] Failed to allocate space for return value");
        globalcontext_send_message(glb, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
    }

    port_send_reply(ctx, pid, ref, result);

    free(uart_data);
    ctx->platform_data = NULL;
}

static NativeHandlerResult uart_driver_consume_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    bool is_closed = false;
    while (mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        term msg = message->message;
        GenMessage gen_message;
        if (UNLIKELY(port_parse_gen_message(msg, &gen_message) != GenCallMessage)) {
            ESP_LOGW(TAG, "Received invalid message.");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            return NativeContinue;
        }

        uint64_t ref_ticks = term_to_ref_ticks(gen_message.ref);

        int local_pid = term_to_local_process_id(gen_message.pid);

        if (is_closed) {
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + REF_SIZE) != MEMORY_GC_OK)) {
                ESP_LOGE(TAG, "[uart_driver_consume_mailbox] Failed to allocate space for error tuple");
                globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            }

            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, NOPROC_ATOM);

            term result_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(result_tuple, 0, term_from_ref_ticks(ref_ticks, &ctx->heap));
            term_put_tuple_element(result_tuple, 1, error_tuple);

            globalcontext_send_message(glb, local_pid, result_tuple);

            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        term req = gen_message.req;
        term cmd_term = term_is_atom(req) ? req : term_get_tuple_element(req, 0);

        enum uart_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (cmd) {
            case UARTReadCmd:
                TRACE("read\n");
                uart_driver_do_read(ctx, gen_message);
                break;

            case UARTWriteCmd:
                TRACE("write\n");
                uart_driver_do_write(ctx, gen_message);
                break;

            case UARTCloseCmd:
                TRACE("close\n");
                uart_driver_do_close(ctx, gen_message);
                is_closed = true;
                break;

            case UARTCancelCmd:
                TRACE("cancel_read\n");
                uart_driver_do_cancel_read(ctx, gen_message);
                break;

            default:
                TRACE("uart: error: unrecognized command.\n");
        }

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return is_closed ? NativeTerminate : NativeContinue;
}

REGISTER_PORT_DRIVER(uart, NULL, NULL, uart_driver_create_port)

#endif
