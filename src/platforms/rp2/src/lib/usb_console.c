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
 * stdio over a USB CDC interface of AtomVM's own TinyUSB device, used when
 * USB port drivers (e.g. MIDI) own TinyUSB and the Pico SDK's stdio_usb
 * cannot be used.
 */

#ifdef AVM_USB_CONSOLE_ENABLED

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wpedantic"
#include <hardware/sync.h>
#include <pico/bootrom.h>
#include <pico/stdio.h>
#include <pico/stdio/driver.h>
#include <pico/time.h>
#include <tusb.h>
#pragma GCC diagnostic pop

#include "rp2_sys.h"

// CDC instance used for the console
#define USB_CONSOLE_ITF 0

// Give up writing after this long without progress (host not reading)
#define USB_CONSOLE_WRITE_TIMEOUT_MS 100

// Maximum time to wait for the TinyUSB lock from thread context
#define USB_CONSOLE_LOCK_TIMEOUT_MS 100

static bool in_interrupt(void)
{
    return __get_current_exception() != 0;
}

static bool usb_console_lock(void)
{
    // In interrupt context (e.g. printf from the USB task itself, which holds
    // the lock), never block: drop output instead of deadlocking.
    if (in_interrupt()) {
        return rp2_usb_try_lock();
    }
    return rp2_usb_lock_until(make_timeout_time_ms(USB_CONSOLE_LOCK_TIMEOUT_MS));
}

static void usb_console_out_chars(const char *buf, int length)
{
    if (!usb_console_lock()) {
        return;
    }
    absolute_time_t deadline = make_timeout_time_ms(USB_CONSOLE_WRITE_TIMEOUT_MS);
    int written = 0;
    while (written < length && tud_cdc_n_connected(USB_CONSOLE_ITF)) {
        uint32_t chunk = tud_cdc_n_write(USB_CONSOLE_ITF, buf + written, (uint32_t) (length - written));
        tud_cdc_n_write_flush(USB_CONSOLE_ITF);
        if (chunk > 0) {
            written += (int) chunk;
            deadline = make_timeout_time_ms(USB_CONSOLE_WRITE_TIMEOUT_MS);
            continue;
        }
        if (in_interrupt() || time_reached(deadline)) {
            break;
        }
        // FIFO is full: let the USB task (which needs the lock) send it
        rp2_usb_unlock();
        sleep_us(500);
        if (!usb_console_lock()) {
            return;
        }
    }
    rp2_usb_unlock();
}

static void usb_console_out_flush(void)
{
    if (!usb_console_lock()) {
        return;
    }
    if (tud_cdc_n_connected(USB_CONSOLE_ITF)) {
        tud_cdc_n_write_flush(USB_CONSOLE_ITF);
    }
    rp2_usb_unlock();
}

static int usb_console_in_chars(char *buf, int length)
{
    if (!usb_console_lock()) {
        return PICO_ERROR_NO_DATA;
    }
    int result = PICO_ERROR_NO_DATA;
    if (tud_cdc_n_connected(USB_CONSOLE_ITF) && tud_cdc_n_available(USB_CONSOLE_ITF)) {
        uint32_t count = tud_cdc_n_read(USB_CONSOLE_ITF, buf, (uint32_t) length);
        if (count > 0) {
            result = (int) count;
        }
    }
    rp2_usb_unlock();
    return result;
}

// Like the Pico SDK's stdio_usb, setting the console to 1200 baud reboots
// into BOOTSEL mode, for flashing without pressing the button.
#define USB_CONSOLE_RESET_MAGIC_BAUD_RATE 1200

void tud_cdc_line_coding_cb(uint8_t itf, cdc_line_coding_t const *p_line_coding)
{
    if (itf == USB_CONSOLE_ITF && p_line_coding->bit_rate == USB_CONSOLE_RESET_MAGIC_BAUD_RATE) {
        reset_usb_boot(0, 0);
    }
}

static stdio_driver_t usb_console_stdio_driver = {
    .out_chars = usb_console_out_chars,
    .out_flush = usb_console_out_flush,
    .in_chars = usb_console_in_chars,
#if PICO_STDIO_ENABLE_CRLF_SUPPORT
    .crlf_enabled = true,
#endif
};

bool usb_console_connected(void)
{
    if (!rp2_usb_lock_until(make_timeout_time_ms(USB_CONSOLE_LOCK_TIMEOUT_MS))) {
        return false;
    }
    bool connected = tud_cdc_n_connected(USB_CONSOLE_ITF);
    rp2_usb_unlock();
    return connected;
}

void usb_console_init(int wait_timeout_ms)
{
    rp2_usb_init();
    stdio_set_driver_enabled(&usb_console_stdio_driver, true);

    if (wait_timeout_ms == 0) {
        return;
    }
    absolute_time_t until = wait_timeout_ms < 0 ? at_the_end_of_time : make_timeout_time_ms(wait_timeout_ms);
    while (!usb_console_connected()) {
        if (time_reached(until)) {
            return;
        }
        sleep_ms(10);
    }
    // Give the terminal some time to be ready after asserting DTR
    sleep_ms(100);
}

#endif
