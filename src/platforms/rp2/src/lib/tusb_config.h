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
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

#ifndef _ATOMVM_TUSB_CONFIG_H
#define _ATOMVM_TUSB_CONFIG_H

#ifdef __cplusplus
extern "C" {
#endif

#ifndef CFG_TUSB_MCU
#error CFG_TUSB_MCU must be defined by the build system (Pico SDK sets this automatically)
#endif

#ifndef CFG_TUSB_OS
#define CFG_TUSB_OS OPT_OS_PICO
#endif

#ifndef CFG_TUSB_RHPORT0_MODE
#define CFG_TUSB_RHPORT0_MODE OPT_MODE_DEVICE
#endif

#ifndef CFG_TUSB_DEBUG
#define CFG_TUSB_DEBUG 0
#endif

#define CFG_TUD_ENABLED 1

#define CFG_TUD_ENDPOINT0_SIZE 64

// CDC is enabled for the usb_cdc port driver, for the USB console, and also
// when no AtomVM USB port driver is enabled, as this file may then be picked
// up by the Pico SDK's stdio_usb.
#if defined(AVM_USB_CDC_PORT_DRIVER_ENABLED) || defined(AVM_USB_CONSOLE_ENABLED) || !defined(AVM_USB_MIDI_PORT_DRIVER_ENABLED)
#define CFG_TUD_CDC 1
#else
#define CFG_TUD_CDC 0
#endif
#define CFG_TUD_MSC 0
#define CFG_TUD_HID 0
#ifdef AVM_USB_MIDI_PORT_DRIVER_ENABLED
#define CFG_TUD_MIDI 1
#else
#define CFG_TUD_MIDI 0
#endif
#define CFG_TUD_VENDOR 0

#define CFG_TUD_CDC_RX_BUFSIZE 256
#define CFG_TUD_CDC_TX_BUFSIZE 256
#define CFG_TUD_CDC_EP_BUFSIZE 64

#define CFG_TUD_MIDI_RX_BUFSIZE 128
#define CFG_TUD_MIDI_TX_BUFSIZE 128
#define CFG_TUD_MIDI_EP_BUFSIZE 64

#ifdef __cplusplus
}
#endif

#endif
