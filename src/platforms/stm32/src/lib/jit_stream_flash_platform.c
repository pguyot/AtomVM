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

/*
 * Include the STM32 HAL first to get hardware flash definitions.
 * The HAL may define FLASH_PAGE_SIZE (on page-based families like G0/L4)
 * and FLASH_SECTOR_SIZE (on H7) which conflict with our logical definitions
 * in jit_stream_flash_platform.h. We undef them before including our header.
 */
#include "stm32_hal_platform.h"
#include "avm_devcfg.h"

/* Remove HAL flash size definitions to avoid conflict with our header */
#undef FLASH_PAGE_SIZE
#undef FLASH_SECTOR_SIZE

#include "jit_stream_flash.h"

#include <stdio.h>
#include <string.h>

#include "nifs.h"
#include "portnifloader.h"

/*
 * Per-family flash programming configuration.
 *
 * Group 1 (page erase, doubleword program): G0, G4, L4, L5, WB
 *   - Erase: FLASH_TYPEERASE_PAGES, Page/NbPages fields
 *   - Program: FLASH_TYPEPROGRAM_DOUBLEWORD (8 bytes)
 *   - HAL_FLASH_Program third param: uint64_t value
 *
 * Group 2 (sector erase, quadword program): H5, U3, U5
 *   - Erase: FLASH_TYPEERASE_SECTORS, Sector/NbSectors fields
 *   - Program: FLASH_TYPEPROGRAM_QUADWORD (16 bytes)
 *   - HAL_FLASH_Program third param: uint32_t data address
 *
 * Group 3 (sector erase, word program): F2, F4, F7
 *   - Erase: FLASH_TYPEERASE_SECTORS, variable sector sizes
 *   - Program: FLASH_TYPEPROGRAM_WORD (4 bytes)
 *   - HAL_FLASH_Program third param: uint64_t value
 *
 * Group 4 (sector erase, flashword program): H7
 *   - Erase: FLASH_TYPEERASE_SECTORS, 128KB sectors
 *   - Program: FLASH_TYPEPROGRAM_FLASHWORD (32 bytes)
 *   - HAL_FLASH_Program third param: uint32_t data address
 */
#if defined(STM32G0XX) || defined(STM32G4XX) || defined(STM32L4XX) \
    || defined(STM32L5XX) || defined(STM32WBXX)
#define STM32_FLASH_GROUP 1
#define STM32_PROGRAM_TYPE FLASH_TYPEPROGRAM_DOUBLEWORD
#define STM32_PROGRAM_SIZE 8
#elif defined(STM32H5XX) || defined(STM32U3XX) || defined(STM32U5XX)
#define STM32_FLASH_GROUP 2
#define STM32_PROGRAM_TYPE FLASH_TYPEPROGRAM_QUADWORD
#define STM32_PROGRAM_SIZE 16
#elif defined(STM32F2XX) || defined(STM32F4XX) || defined(STM32F7XX)
#define STM32_FLASH_GROUP 3
#define STM32_PROGRAM_TYPE FLASH_TYPEPROGRAM_WORD
#define STM32_PROGRAM_SIZE 4
#elif defined(STM32H7XX)
#define STM32_FLASH_GROUP 4
#define STM32_PROGRAM_TYPE FLASH_TYPEPROGRAM_FLASHWORD
#define STM32_PROGRAM_SIZE 32
#else
#error "Unknown STM32 family for JIT flash programming"
#endif

/*
 * Hardware erase granularity for page-based and uniform-sector families.
 * Used to compute page/sector numbers for HAL_FLASHEx_Erase.
 * For variable-sector families (F2/F4/F7), this is handled by
 * stm32_get_flash_sector() instead.
 */
#if STM32_FLASH_GROUP == 1 || STM32_FLASH_GROUP == 2
#if defined(STM32G0XX) || defined(STM32G4XX) || defined(STM32L4XX)
#define STM32_HW_ERASE_SIZE 2048U
#elif defined(STM32L5XX) || defined(STM32WBXX)
#define STM32_HW_ERASE_SIZE 4096U
#elif defined(STM32H5XX) || defined(STM32U3XX) || defined(STM32U5XX)
#define STM32_HW_ERASE_SIZE 8192U
#endif
#endif

/*
 * Bank computation for dual-bank devices.
 * Returns FLASH_BANK_1 or FLASH_BANK_2 based on flash address.
 * Only defined for families whose FLASH_EraseInitTypeDef has a Banks field.
 * WB has no Banks field; single-bank F2/F4/F7 have no Banks field either.
 */
#if !defined(STM32WBXX) && (STM32_FLASH_GROUP != 3 || defined(FLASH_OPTCR_DB1M))
static uint32_t stm32_get_flash_bank(uintptr_t addr)
{
#ifdef FLASH_BANK_2
    uint32_t flash_size = CFG_FLASH_END - FLASH_START_ADDRESS;
    uint32_t bank_size = flash_size / 2;
    if (addr >= FLASH_START_ADDRESS + bank_size) {
        return FLASH_BANK_2;
    }
#else
    UNUSED(addr);
#endif
    return FLASH_BANK_1;
}
#endif

/*
 * Page/sector number computation for uniform-erase families (Groups 1 & 2).
 * Returns bank-relative page/sector number for dual-bank devices.
 */
#if STM32_FLASH_GROUP == 1 || STM32_FLASH_GROUP == 2
static uint32_t stm32_get_flash_unit(uintptr_t addr)
{
    uintptr_t base = FLASH_START_ADDRESS;
#ifdef FLASH_BANK_2
    uint32_t flash_size = CFG_FLASH_END - FLASH_START_ADDRESS;
    uint32_t bank_size = flash_size / 2;
    if (addr >= FLASH_START_ADDRESS + bank_size) {
        base = FLASH_START_ADDRESS + bank_size;
    }
#endif
    return (uint32_t) ((addr - base) / STM32_HW_ERASE_SIZE);
}
#endif

/*
 * Sector number computation for variable-sector families (F2/F4/F7).
 * The JIT area is beyond AVM_APP_ADDRESS, which is always in the
 * large-sector region of these devices.
 */
#if defined(STM32F2XX) || defined(STM32F4XX)
static uint32_t stm32_get_flash_sector(uintptr_t addr)
{
    uint32_t offset = (uint32_t) (addr - FLASH_START_ADDRESS);
    if (offset < 0x10000U) {
        return offset / (16U * 1024U); /* Sectors 0-3: 16KB each */
    } else if (offset < 0x20000U) {
        return 4; /* Sector 4: 64KB */
    } else {
        return 5 + (offset - 0x20000U) / (128U * 1024U); /* Sectors 5+: 128KB each */
    }
}
#elif defined(STM32F7XX)
static uint32_t stm32_get_flash_sector(uintptr_t addr)
{
    uint32_t offset = (uint32_t) (addr - FLASH_START_ADDRESS);
    if (offset < 0x20000U) {
        return offset / (32U * 1024U); /* Sectors 0-3: 32KB each */
    } else if (offset < 0x40000U) {
        return 4; /* Sector 4: 128KB */
    } else {
        return 5 + (offset - 0x40000U) / (256U * 1024U); /* Sectors 5+: 256KB each */
    }
}
#elif defined(STM32H7XX)
static uint32_t stm32_get_flash_sector(uintptr_t addr)
{
    uintptr_t base = FLASH_START_ADDRESS;
#ifdef FLASH_BANK_2
    uint32_t flash_size = CFG_FLASH_END - FLASH_START_ADDRESS;
    uint32_t bank_size = flash_size / 2;
    if (addr >= FLASH_START_ADDRESS + bank_size) {
        base = FLASH_START_ADDRESS + bank_size;
    }
#endif
    return (uint32_t) ((addr - base) / (128U * 1024U));
}
#endif

/*
 * Invalidate instruction cache after flash modification.
 * Required for code execution from modified flash regions.
 */
static void stm32_invalidate_icache(void)
{
#if defined(STM32H5XX) || defined(STM32L5XX) || defined(STM32U3XX) || defined(STM32U5XX)
    /* Cortex-M33 with ICACHE peripheral */
    HAL_ICACHE_Invalidate();
#elif defined(STM32F7XX) || defined(STM32H7XX)
    /* Cortex-M7 with L1 I-cache and D-cache */
    SCB_InvalidateICache();
    SCB_InvalidateDCache();
#elif defined(__HAL_FLASH_INSTRUCTION_CACHE_RESET)
    /* Cortex-M4 with ART accelerator (F2, F4, G4, L4, WB) */
    __HAL_FLASH_INSTRUCTION_CACHE_DISABLE();
    __HAL_FLASH_INSTRUCTION_CACHE_RESET();
    __HAL_FLASH_INSTRUCTION_CACHE_ENABLE();
#endif
    /* Cortex-M0+ (G0) has no instruction cache - nothing to do */
    __ISB();
    __DSB();
}

/*
 * Portable error flag clearing macro.
 * Different families define error flags differently.
 */
#ifdef FLASH_FLAG_ALL_ERRORS
#define STM32_CLEAR_FLASH_ERRORS() __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_ALL_ERRORS)
#elif defined(FLASH_FLAG_SR_ERRORS)
#define STM32_CLEAR_FLASH_ERRORS() __HAL_FLASH_CLEAR_FLAG(FLASH_FLAG_SR_ERRORS)
#else
#define STM32_CLEAR_FLASH_ERRORS() ((void) 0)
#endif

struct JSFlashPlatformContext *jit_stream_flash_platform_init(void)
{
    return (struct JSFlashPlatformContext *) 1;
}

void jit_stream_flash_platform_destroy(struct JSFlashPlatformContext *pf_ctx)
{
    UNUSED(pf_ctx);
}

bool jit_stream_flash_platform_erase_sector(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr)
{
    UNUSED(pf_ctx);

    HAL_StatusTypeDef status;
    uint32_t error = 0;

    status = HAL_FLASH_Unlock();
    if (status != HAL_OK) {
        fprintf(stderr, "HAL_FLASH_Unlock failed: %d\n", (int) status);
        return false;
    }

    STM32_CLEAR_FLASH_ERRORS();

    FLASH_EraseInitTypeDef erase;
    memset(&erase, 0, sizeof(erase));

#if STM32_FLASH_GROUP == 1
    /* Page-based erase: G0, G4, L4, L5, WB */
    erase.TypeErase = FLASH_TYPEERASE_PAGES;
    erase.Page = stm32_get_flash_unit(addr);
    erase.NbPages = FLASH_SECTOR_SIZE / STM32_HW_ERASE_SIZE;
#if !defined(STM32WBXX)
    /* G0, G4, L4, L5 have Banks field in FLASH_EraseInitTypeDef; WB does not */
    erase.Banks = stm32_get_flash_bank(addr);
#endif

#elif STM32_FLASH_GROUP == 2
    /* Uniform sector erase: H5, U3, U5 (8KB "sectors") */
    erase.TypeErase = FLASH_TYPEERASE_SECTORS;
    erase.Sector = stm32_get_flash_unit(addr);
    erase.NbSectors = FLASH_SECTOR_SIZE / STM32_HW_ERASE_SIZE;
    erase.Banks = stm32_get_flash_bank(addr);

#elif STM32_FLASH_GROUP == 3
    /* Variable sector erase: F2, F4, F7 */
    erase.TypeErase = FLASH_TYPEERASE_SECTORS;
    erase.Sector = stm32_get_flash_sector(addr);
    erase.NbSectors = 1;
    erase.VoltageRange = FLASH_VOLTAGE_RANGE_3; /* 2.7-3.6V */
#if defined(FLASH_OPTCR_DB1M)
    /* Dual-bank F4/F7 (2MB devices) */
    erase.Banks = stm32_get_flash_bank(addr);
#endif

#elif STM32_FLASH_GROUP == 4
    /* Uniform sector erase: H7 (128KB sectors) */
    erase.TypeErase = FLASH_TYPEERASE_SECTORS;
    erase.Sector = stm32_get_flash_sector(addr);
    erase.NbSectors = 1;
    erase.Banks = stm32_get_flash_bank(addr);
#endif

    status = HAL_FLASHEx_Erase(&erase, &error);

    HAL_FLASH_Lock();

    if (status != HAL_OK || error != 0xFFFFFFFFU) {
        fprintf(stderr, "HAL_FLASHEx_Erase failed: status=%d error=0x%lx\n",
            (int) status, (unsigned long) error);
        return false;
    }

    stm32_invalidate_icache();

    return true;
}

bool jit_stream_flash_platform_write_page(struct JSFlashPlatformContext *pf_ctx, uintptr_t addr, const uint8_t *data)
{
    UNUSED(pf_ctx);

    HAL_StatusTypeDef status;

    status = HAL_FLASH_Unlock();
    if (status != HAL_OK) {
        fprintf(stderr, "HAL_FLASH_Unlock failed: %d\n", (int) status);
        return false;
    }

    STM32_CLEAR_FLASH_ERRORS();

    /* Program the page in hardware program-unit sized chunks */
    for (size_t offset = 0; offset < FLASH_PAGE_SIZE; offset += STM32_PROGRAM_SIZE) {

#if STM32_FLASH_GROUP == 1
        /* Doubleword programming: pass uint64_t value directly */
        uint64_t dword;
        memcpy(&dword, data + offset, sizeof(dword));
        status = HAL_FLASH_Program(STM32_PROGRAM_TYPE, addr + offset, dword);

#elif STM32_FLASH_GROUP == 2
        /* Quadword programming: pass pointer to 16-byte aligned data */
        uint8_t aligned_buf[16] __attribute__((aligned(16)));
        memcpy(aligned_buf, data + offset, 16);
        status = HAL_FLASH_Program(STM32_PROGRAM_TYPE, addr + offset,
            (uint32_t) aligned_buf);

#elif STM32_FLASH_GROUP == 3
        /* Word programming: pass uint64_t value (only 32 bits used) */
        uint32_t word;
        memcpy(&word, data + offset, sizeof(word));
        status = HAL_FLASH_Program(STM32_PROGRAM_TYPE, addr + offset,
            (uint64_t) word);

#elif STM32_FLASH_GROUP == 4
        /* Flashword programming: pass pointer to 32-byte aligned data */
        uint8_t aligned_buf[32] __attribute__((aligned(32)));
        memcpy(aligned_buf, data + offset, 32);
        status = HAL_FLASH_Program(STM32_PROGRAM_TYPE, addr + offset,
            (uint32_t) aligned_buf);
#endif

        if (status != HAL_OK) {
            fprintf(stderr, "HAL_FLASH_Program failed at offset %zu: %d\n",
                offset, (int) status);
            HAL_FLASH_Lock();
            return false;
        }
    }

    HAL_FLASH_Lock();

    stm32_invalidate_icache();

    return true;
}

uintptr_t jit_stream_flash_platform_ptr_to_executable(uintptr_t addr)
{
    /* Set Thumb bit for ARM */
    return addr | 0x1;
}

uintptr_t jit_stream_flash_platform_executable_to_ptr(uintptr_t addr)
{
    /* Clear Thumb bit */
    return addr & ~0x1UL;
}

REGISTER_NIF_COLLECTION(jit_stream_flash, jit_stream_flash_init, NULL, jit_stream_flash_get_nif)

#endif /* AVM_NO_JIT */
