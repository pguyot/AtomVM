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
 * USB MIDI port driver.
 *
 * Exposes a class-compliant USB MIDI 1.0 device (one cable, one IN and one
 * OUT jack) as a byte stream: bytes written are MIDI messages (without
 * running status) converted to USB MIDI event packets by TinyUSB, and bytes
 * read are MIDI messages extracted from event packets received from the
 * host.
 */

#ifdef AVM_USB_MIDI_PORT_DRIVER_ENABLED

#include <string.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <pico/time.h>
#include <pico/util/queue.h>
#include <tusb.h>
#pragma GCC diagnostic pop

#include "atom.h"
#include "context.h"
#include "debug.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "interop.h"
#include "mailbox.h"
#include "port.h"
#include "smp.h"
#include "term.h"
#include "utils.h"

#include "rp2_sys.h"

static NativeHandlerResult usb_midi_driver_consume_mailbox(Context *ctx);
static void usb_midi_send_error_reply(Context *ctx, term pid, term ref, AtomString reason_atom_str);

#define USB_MIDI_BUF_SIZE 128

// Abort a write after this many ms of no progress (host stalled or unplugged).
#define USB_MIDI_WRITE_TIMEOUT_MS 500

// TinyUSB MIDI instance (not interface number) and virtual cable
#define USB_MIDI_ITF 0
#define USB_MIDI_CABLE 0

struct USBMIDIData
{
    queue_t rxqueue;
    struct EventListener listener;
    GlobalContext *global;
    term reader_process_pid;
    uint64_t reader_ref_ticks;
#ifndef AVM_NO_SMP
    Mutex *reader_lock;
#endif
};

enum usb_midi_cmd
{
    USBMIDIInvalidCmd = 0,
    USBMIDIReadCmd = 1,
    USBMIDIWriteCmd = 2,
    USBMIDICloseCmd = 3,
    USBMIDICancelCmd = 4,
    USBMIDIMountedCmd = 5
};

static const AtomStringIntPair cmd_table[] = {
    { ATOM_STR("\x4", "read"), USBMIDIReadCmd },
    { ATOM_STR("\x5", "write"), USBMIDIWriteCmd },
    { ATOM_STR("\x5", "close"), USBMIDICloseCmd },
    { ATOM_STR("\xB", "cancel_read"), USBMIDICancelCmd },
    { ATOM_STR("\x7", "mounted"), USBMIDIMountedCmd },
    SELECT_INT_DEFAULT(USBMIDIInvalidCmd)
};

// Singleton driver data, protected by s_open_lock
static struct USBMIDIData *s_midi_data = NULL;

#ifndef AVM_NO_SMP
static Mutex *s_open_lock = NULL;
#endif

static void usb_midi_driver_init(GlobalContext *global)
{
    UNUSED(global);
#ifndef AVM_NO_SMP
    s_open_lock = smp_mutex_create();
    if (IS_NULL_PTR(s_open_lock)) {
        fprintf(stderr, "usb_midi: failed to create open lock\n");
        AVM_ABORT();
    }
#endif
}

// TinyUSB MIDI receive callback -- invoked by tud_task() in the USB task
// interrupt. The TinyUSB lock is already held by the caller of tud_task(); do
// not relock.
void tud_midi_rx_cb(uint8_t itf)
{
    if (s_midi_data == NULL || itf != USB_MIDI_ITF) {
        return;
    }
    uint32_t available = tud_midi_n_available(itf, USB_MIDI_CABLE);
    if (available > 0) {
        sys_try_post_listener_event_from_isr(s_midi_data->global, &s_midi_data->rxqueue, &available);
    }
}

static EventListener *usb_midi_listener_handler(GlobalContext *glb, EventListener *base_listener)
{
    struct USBMIDIData *midi_data = GET_LIST_ENTRY(base_listener, struct USBMIDIData, listener);

    uint32_t event_val;
    if (!queue_try_remove(&midi_data->rxqueue, &event_val)) {
        return base_listener;
    }

    SMP_MUTEX_LOCK(midi_data->reader_lock);
    if (midi_data->reader_process_pid == term_invalid_term()) {
        SMP_MUTEX_UNLOCK(midi_data->reader_lock);
        return base_listener;
    }

    uint8_t buf[USB_MIDI_BUF_SIZE];
    sys_tinyusb_lock(glb);
    uint32_t rx_size = tud_midi_n_stream_read(USB_MIDI_ITF, USB_MIDI_CABLE, buf, sizeof(buf));
    sys_tinyusb_unlock(glb);
    if (rx_size == 0) {
        SMP_MUTEX_UNLOCK(midi_data->reader_lock);
        return base_listener;
    }

    term reader_pid = midi_data->reader_process_pid;
    uint64_t reader_ref_ticks = midi_data->reader_ref_ticks;
    midi_data->reader_process_pid = term_invalid_term();
    midi_data->reader_ref_ticks = 0;
    SMP_MUTEX_UNLOCK(midi_data->reader_lock);

    int bin_size = term_binary_heap_size(rx_size);

    Heap heap;
    if (UNLIKELY(memory_init_heap(&heap, bin_size + TERM_BOXED_REFERENCE_SHORT_SIZE + TUPLE_SIZE(2) * 2) != MEMORY_GC_OK)) {
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        AVM_ABORT();
    }

    term bin = term_create_uninitialized_binary(rx_size, &heap, glb);
    memcpy((void *) term_binary_data(bin), buf, rx_size);

    term ok_tuple = term_alloc_tuple(2, &heap);
    term_put_tuple_element(ok_tuple, 0, OK_ATOM);
    term_put_tuple_element(ok_tuple, 1, bin);

    term ref = term_from_ref_ticks(reader_ref_ticks, &heap);

    term result_tuple = term_alloc_tuple(2, &heap);
    term_put_tuple_element(result_tuple, 0, ref);
    term_put_tuple_element(result_tuple, 1, ok_tuple);

    int local_pid = term_to_local_process_id(reader_pid);
    globalcontext_send_message(glb, local_pid, result_tuple);

    memory_destroy_heap(&heap, glb);

    return base_listener;
}

static Context *usb_midi_driver_create_port(GlobalContext *global, term opts)
{
    UNUSED(opts);

    SMP_MUTEX_LOCK(s_open_lock);
    if (s_midi_data != NULL) {
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "usb_midi: MIDI interface already in use\n");
        return NULL;
    }

    struct USBMIDIData *midi_data = malloc(sizeof(struct USBMIDIData));
    if (IS_NULL_PTR(midi_data)) {
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

#ifndef AVM_NO_SMP
    midi_data->reader_lock = smp_mutex_create();
    if (IS_NULL_PTR(midi_data->reader_lock)) {
        free(midi_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }
#endif

    Context *ctx = context_new(global);
    if (IS_NULL_PTR(ctx)) {
#ifndef AVM_NO_SMP
        smp_mutex_destroy(midi_data->reader_lock);
#endif
        free(midi_data);
        SMP_MUTEX_UNLOCK(s_open_lock);
        fprintf(stderr, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
        return NULL;
    }

    queue_init(&midi_data->rxqueue, sizeof(uint32_t), EVENT_QUEUE_LEN);
    midi_data->listener.handler = usb_midi_listener_handler;
    midi_data->listener.queue = &midi_data->rxqueue;
    midi_data->global = global;
    midi_data->reader_process_pid = term_invalid_term();
    midi_data->reader_ref_ticks = 0;

    // TinyUSB device stack is initialized at boot (see sys.c)
    sys_register_listener(global, &midi_data->listener);

    s_midi_data = midi_data;
    SMP_MUTEX_UNLOCK(s_open_lock);

    ctx->native_handler = usb_midi_driver_consume_mailbox;
    ctx->platform_data = midi_data;

    return ctx;
}

static void usb_midi_driver_do_read(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct USBMIDIData *midi_data = ctx->platform_data;
    term pid = gen_message.pid;
    term ref = gen_message.ref;
    uint64_t ref_ticks = term_to_ref_ticks(ref);

    int local_pid = term_to_local_process_id(pid);

    SMP_MUTEX_LOCK(midi_data->reader_lock);
    if (midi_data->reader_process_pid != term_invalid_term()) {
        SMP_MUTEX_UNLOCK(midi_data->reader_lock);
        usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x8", "ealready"));
        return;
    }

    // Try immediate read
    uint8_t buf[USB_MIDI_BUF_SIZE];
    sys_tinyusb_lock(glb);
    uint32_t rx_size = tud_midi_n_stream_read(USB_MIDI_ITF, USB_MIDI_CABLE, buf, sizeof(buf));
    sys_tinyusb_unlock(glb);
    if (rx_size > 0) {
        SMP_MUTEX_UNLOCK(midi_data->reader_lock);
        int bin_size = term_binary_heap_size(rx_size);
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, bin_size + TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            fprintf(stderr, "usb_midi: Failed to allocate return value\n");
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        }
        term bin = term_create_uninitialized_binary(rx_size, &ctx->heap, glb);
        memcpy((void *) term_binary_data(bin), buf, rx_size);

        term ok_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(ok_tuple, 0, OK_ATOM);
        term_put_tuple_element(ok_tuple, 1, bin);

        port_send_reply(ctx, pid, ref, ok_tuple);
    } else {
        midi_data->reader_process_pid = pid;
        midi_data->reader_ref_ticks = ref_ticks;
        SMP_MUTEX_UNLOCK(midi_data->reader_lock);
    }
}

static void usb_midi_driver_do_cancel_read(Context *ctx, GenMessage gen_message)
{
    struct USBMIDIData *midi_data = ctx->platform_data;
    term req = gen_message.req;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    // `{cancel_read, ReadRef}` only clears if ReadRef matches the stored
    // reader, avoiding the race where a cancel wipes a freshly-installed
    // reader belonging to a different request. Bare `cancel_read` clears
    // unconditionally.
    bool clear_unconditionally = term_is_atom(req);
    uint64_t target_ref_ticks = 0;
    if (!clear_unconditionally) {
        if (UNLIKELY(term_get_tuple_arity(req) < 2 || !term_is_local_reference(term_get_tuple_element(req, 1)))) {
            usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "badarg"));
            return;
        }
        target_ref_ticks = term_to_ref_ticks(term_get_tuple_element(req, 1));
    }

    SMP_MUTEX_LOCK(midi_data->reader_lock);
    if (clear_unconditionally || midi_data->reader_ref_ticks == target_ref_ticks) {
        midi_data->reader_process_pid = term_invalid_term();
        midi_data->reader_ref_ticks = 0;
    }
    SMP_MUTEX_UNLOCK(midi_data->reader_lock);

    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        fprintf(stderr, "usb_midi: Failed to allocate return value\n");
        globalcontext_send_message(ctx->global, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    port_send_reply(ctx, pid, ref, OK_ATOM);
}

static void usb_midi_send_error_reply(Context *ctx, term pid, term ref, AtomString reason_atom_str)
{
    GlobalContext *glb = ctx->global;
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2) * 2, 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        globalcontext_send_message(glb, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    term reason = globalcontext_make_atom(glb, reason_atom_str);
    term error_tuple = port_create_error_tuple(ctx, reason);
    port_send_reply(ctx, pid, ref, error_tuple);
}

static void usb_midi_send_ok_reply(Context *ctx, term pid, term ref, term value)
{
    if (UNLIKELY(memory_ensure_free_with_roots(ctx, TUPLE_SIZE(2), 1, &ref, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        globalcontext_send_message(ctx->global, term_to_local_process_id(pid), OUT_OF_MEMORY_ATOM);
        return;
    }
    port_send_reply(ctx, pid, ref, value);
}

static void usb_midi_driver_do_write(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    term msg = gen_message.req;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    if (UNLIKELY(!term_is_tuple(msg) || term_get_tuple_arity(msg) < 2)) {
        usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "badarg"));
        return;
    }
    term data = term_get_tuple_element(msg, 1);
    int local_pid = term_to_local_process_id(pid);

    size_t buffer_size;
    switch (interop_iolist_size(data, &buffer_size)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        case InteropBadArg:
            usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "badarg"));
            return;
    }
    void *buffer = malloc(buffer_size);
    if (IS_NULL_PTR(buffer)) {
        globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
        return;
    }
    switch (interop_write_iolist(data, buffer)) {
        case InteropOk:
            break;
        case InteropMemoryAllocFail:
            free(buffer);
            globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            return;
        case InteropBadArg:
            free(buffer);
            usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "badarg"));
            return;
    }

    // If the host hasn't configured the device yet (or it is unplugged),
    // silently drop the data, like a DIN MIDI OUT with nothing connected.
    sys_tinyusb_lock(glb);
    bool initially_mounted = tud_midi_n_mounted(USB_MIDI_ITF);
    sys_tinyusb_unlock(glb);
    if (!initially_mounted) {
        free(buffer);
        usb_midi_send_ok_reply(ctx, pid, ref, OK_ATOM);
        return;
    }

    size_t written = 0;
    uint32_t deadline = to_ms_since_boot(get_absolute_time()) + USB_MIDI_WRITE_TIMEOUT_MS;
    bool host_gone = false;
    bool timed_out = false;
    while (written < buffer_size) {
        sys_tinyusb_lock(glb);
        bool mounted = tud_midi_n_mounted(USB_MIDI_ITF);
        uint32_t chunk = 0;
        if (mounted) {
            chunk = tud_midi_n_stream_write(USB_MIDI_ITF, USB_MIDI_CABLE,
                (const uint8_t *) buffer + written,
                buffer_size - written);
        }
        sys_tinyusb_unlock(glb);

        if (!mounted) {
            host_gone = true;
            break;
        }
        if (chunk > 0) {
            written += chunk;
            // Reset the deadline on any progress so a slow but moving
            // host can drain a buffer larger than the FIFO without tripping.
            deadline = to_ms_since_boot(get_absolute_time()) + USB_MIDI_WRITE_TIMEOUT_MS;
        } else {
            if ((int32_t) (to_ms_since_boot(get_absolute_time()) - deadline) >= 0) {
                timed_out = true;
                break;
            }
            sleep_ms(1);
        }
    }

    free(buffer);

    if (host_gone) {
        usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x6", "closed"));
        return;
    }
    if (timed_out) {
        usb_midi_send_error_reply(ctx, pid, ref, ATOM_STR("\x7", "timeout"));
        return;
    }

    usb_midi_send_ok_reply(ctx, pid, ref, OK_ATOM);
}

static void usb_midi_driver_do_mounted(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    sys_tinyusb_lock(glb);
    bool mounted = tud_midi_n_mounted(USB_MIDI_ITF);
    sys_tinyusb_unlock(glb);
    usb_midi_send_ok_reply(ctx, gen_message.pid, gen_message.ref, mounted ? TRUE_ATOM : FALSE_ATOM);
}

static void usb_midi_driver_do_close(Context *ctx, GenMessage gen_message)
{
    GlobalContext *glb = ctx->global;
    struct USBMIDIData *midi_data = ctx->platform_data;
    term pid = gen_message.pid;
    term ref = gen_message.ref;

    SMP_MUTEX_LOCK(s_open_lock);
    sys_tinyusb_lock(glb);
    s_midi_data = NULL;
    sys_tinyusb_unlock(glb);
    SMP_MUTEX_UNLOCK(s_open_lock);

    sys_unregister_listener(glb, &midi_data->listener);
    queue_free(&midi_data->rxqueue);

    SMP_MUTEX_LOCK(midi_data->reader_lock);
    term pending_reader_pid = midi_data->reader_process_pid;
    uint64_t pending_reader_ref_ticks = midi_data->reader_ref_ticks;
    midi_data->reader_process_pid = term_invalid_term();
    SMP_MUTEX_UNLOCK(midi_data->reader_lock);

    if (pending_reader_pid != term_invalid_term()) {
        BEGIN_WITH_STACK_HEAP(TERM_BOXED_REFERENCE_SHORT_SIZE + TUPLE_SIZE(2) * 2, heap)
        term error_tuple = term_alloc_tuple(2, &heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(glb, ATOM_STR("\x6", "closed")));
        term reader_ref = term_from_ref_ticks(pending_reader_ref_ticks, &heap);
        term reply = term_alloc_tuple(2, &heap);
        term_put_tuple_element(reply, 0, reader_ref);
        term_put_tuple_element(reply, 1, error_tuple);
        globalcontext_send_message(glb, term_to_local_process_id(pending_reader_pid), reply);
        END_WITH_STACK_HEAP(heap, glb)
    }

#ifndef AVM_NO_SMP
    smp_mutex_destroy(midi_data->reader_lock);
#endif

    usb_midi_send_ok_reply(ctx, pid, ref, OK_ATOM);

    free(midi_data);
    ctx->platform_data = NULL;
}

static NativeHandlerResult usb_midi_driver_consume_mailbox(Context *ctx)
{
    GlobalContext *glb = ctx->global;
    bool is_closed = false;
    while (mailbox_has_next(&ctx->mailbox)) {
        Message *message = mailbox_first(&ctx->mailbox);
        term msg = message->message;
        GenMessage gen_message;
        if (UNLIKELY(port_parse_gen_message(msg, &gen_message) != GenCallMessage)) {
            fprintf(stderr, "usb_midi: Received invalid message.\n");
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        if (is_closed) {
            uint64_t ref_ticks = term_to_ref_ticks(gen_message.ref);
            int local_pid = term_to_local_process_id(gen_message.pid);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) * 2 + TERM_BOXED_REFERENCE_SHORT_SIZE) != MEMORY_GC_OK)) {
                fprintf(stderr, "usb_midi: Failed to allocate error tuple\n");
                globalcontext_send_message(glb, local_pid, OUT_OF_MEMORY_ATOM);
            } else {
                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, NOPROC_ATOM);

                term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);

                term result_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(result_tuple, 0, ref);
                term_put_tuple_element(result_tuple, 1, error_tuple);

                globalcontext_send_message(glb, local_pid, result_tuple);
            }

            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }

        term req = gen_message.req;
        // req is unvalidated: an immediate would be misread as a boxed tuple pointer below.
        if (UNLIKELY(!term_is_atom(req) && !(term_is_tuple(req) && term_get_tuple_arity(req) >= 1))) {
            usb_midi_send_error_reply(ctx, gen_message.pid, gen_message.ref, ATOM_STR("\xF", "unknown_command"));
            mailbox_remove_message(&ctx->mailbox, &ctx->heap);
            continue;
        }
        term cmd_term = term_is_atom(req) ? req : term_get_tuple_element(req, 0);

        enum usb_midi_cmd cmd = interop_atom_term_select_int(cmd_table, cmd_term, ctx->global);
        switch (cmd) {
            case USBMIDIReadCmd:
                usb_midi_driver_do_read(ctx, gen_message);
                break;
            case USBMIDIWriteCmd:
                usb_midi_driver_do_write(ctx, gen_message);
                break;
            case USBMIDICloseCmd:
                usb_midi_driver_do_close(ctx, gen_message);
                is_closed = true;
                break;
            case USBMIDICancelCmd:
                usb_midi_driver_do_cancel_read(ctx, gen_message);
                break;
            case USBMIDIMountedCmd:
                usb_midi_driver_do_mounted(ctx, gen_message);
                break;
            default:
                fprintf(stderr, "usb_midi: unrecognized command\n");
                usb_midi_send_error_reply(ctx, gen_message.pid, gen_message.ref, ATOM_STR("\xF", "unknown_command"));
                break;
        }

        mailbox_remove_message(&ctx->mailbox, &ctx->heap);
    }
    return is_closed ? NativeTerminate : NativeContinue;
}

// Called when the VM exits. A port that is still open is not closed: its
// context frees midi_data, but its listener must be unregistered first, as
// globalcontext_destroy() would free it too.
static void usb_midi_driver_destroy(GlobalContext *global)
{
    struct USBMIDIData *midi_data = s_midi_data;
    if (midi_data != NULL) {
        sys_tinyusb_lock(global);
        s_midi_data = NULL;
        sys_tinyusb_unlock(global);
        sys_unregister_listener(global, &midi_data->listener);
        queue_free(&midi_data->rxqueue);
#ifndef AVM_NO_SMP
        smp_mutex_destroy(midi_data->reader_lock);
#endif
    }
#ifndef AVM_NO_SMP
    smp_mutex_destroy(s_open_lock);
    s_open_lock = NULL;
#endif
}

REGISTER_PORT_DRIVER(usb_midi, usb_midi_driver_init, usb_midi_driver_destroy, usb_midi_driver_create_port)

#endif
