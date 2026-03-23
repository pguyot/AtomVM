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
 * @file sys.c
 * @brief Minimal platform implementation for QEMU semihosting test runner.
 */

#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include <avmpack.h>
#include <defaultatoms.h>
#include <scheduler.h>
#include <sys.h>

// #define ENABLE_TRACE
#include <trace.h>

static uint64_t monotonic_counter;

void platform_defaultatoms_init(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_init_platform(GlobalContext *glb)
{
    glb->platform_data = NULL;
}

void sys_free_platform(GlobalContext *glb)
{
    UNUSED(glb);
}

void sys_poll_events(GlobalContext *glb, int timeout_ms)
{
    UNUSED(glb);
    UNUSED(timeout_ms);
}

void sys_listener_destroy(struct ListHead *item)
{
    UNUSED(item);
}

void sys_register_listener(GlobalContext *global, EventListener *listener)
{
    UNUSED(global);
    UNUSED(listener);
}

void sys_unregister_listener(GlobalContext *global, EventListener *listener)
{
    UNUSED(global);
    UNUSED(listener);
}

void sys_register_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_unregister_select_event(GlobalContext *global, ErlNifEvent event, bool is_write)
{
    UNUSED(global);
    UNUSED(event);
    UNUSED(is_write);
}

void sys_time(struct timespec *t)
{
    uint64_t now = sys_monotonic_time_u64();
    t->tv_sec = (time_t) (now / 1000);
    t->tv_nsec = ((long) (now % 1000)) * 1000000;
}

void sys_monotonic_time(struct timespec *t)
{
    uint64_t now = sys_monotonic_time_u64();
    t->tv_sec = (time_t) (now / 1000);
    t->tv_nsec = ((long) (now % 1000)) * 1000000;
}

uint64_t sys_monotonic_time_u64(void)
{
    // Simple incrementing counter - sufficient for test execution ordering
    return ++monotonic_counter;
}

uint64_t sys_monotonic_time_ms_to_u64(uint64_t ms)
{
    return ms;
}

uint64_t sys_monotonic_time_u64_to_ms(uint64_t t)
{
    return t;
}

enum OpenAVMResult sys_open_avm_from_file(GlobalContext *global, const char *path, struct AVMPackData **data)
{
    UNUSED(global);
    UNUSED(path);
    UNUSED(data);
    return AVM_OPEN_NOT_SUPPORTED;
}

Module *sys_load_module_from_file(GlobalContext *global, const char *path)
{
    UNUSED(global);
    UNUSED(path);
    return NULL;
}

Context *sys_create_port(GlobalContext *glb, const char *driver_name, term opts)
{
    UNUSED(glb);
    UNUSED(driver_name);
    UNUSED(opts);
    return NULL;
}

term sys_get_info(Context *ctx, term key)
{
    UNUSED(ctx);
    UNUSED(key);
    return UNDEFINED_ATOM;
}

#ifndef AVM_NO_JIT
ModuleNativeEntryPoint sys_map_native_code(const uint8_t *native_code, size_t size, size_t offset)
{
    UNUSED(size);
#if defined(__arm__)
    // Set Thumb bit for ARM
    return (ModuleNativeEntryPoint) ((uintptr_t) (native_code + offset) | 1);
#else
    return (ModuleNativeEntryPoint) (native_code + offset);
#endif
}
#endif
