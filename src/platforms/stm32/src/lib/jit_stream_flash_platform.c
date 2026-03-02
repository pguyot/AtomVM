/*
 * This file is part of AtomVM.
 *
 * Copyright 2026 by Paul Guyot <pguyot@kallisys.net>
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

#ifndef AVM_NO_JIT

#include "jit_stream_flash.h"

#include <stdio.h>

#include "nifs.h"

struct JSFlashPlatformContext *jit_stream_flash_platform_init(void)
{
    // TODO: initialize HAL flash context
    return (struct JSFlashPlatformContext *) 1;
}

void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *pf_ctx)
{
    UNUSED(pf_ctx);
}

bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr)
{
    UNUSED(pf_ctx);
    UNUSED(addr);
    // TODO: implement using HAL_FLASHEx_Erase
    fprintf(stderr, "jit_stream_flash_platform_erase_sector: not yet implemented on STM32\n");
    return false;
}

bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr, const uint8_t *data)
{
    UNUSED(pf_ctx);
    UNUSED(addr);
    UNUSED(data);
    // TODO: implement using HAL_FLASH_Program
    fprintf(stderr, "jit_stream_flash_platform_write_page: not yet implemented on STM32\n");
    return false;
}

uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr)
{
    // Set Thumb bit for ARM
    return addr | 0x1;
}

uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr)
{
    // Clear Thumb bit
    return addr & ~0x1UL;
}

REGISTER_NIF_COLLECTION(jit_stream_flash, jit_stream_flash_init, NULL, jit_stream_flash_get_nif)

#endif // AVM_NO_JIT
