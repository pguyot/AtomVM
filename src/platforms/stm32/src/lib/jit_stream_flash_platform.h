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

#ifndef _JIT_STREAM_FLASH_PLATFORM_H_
#define _JIT_STREAM_FLASH_PLATFORM_H_

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * FLASH_SECTOR_SIZE defines the logical erase unit for the common
 * jit_stream_flash.c code. It must match the hardware erase granularity
 * of the STM32 family, because the common code assumes that erasing at an
 * address erases exactly FLASH_SECTOR_SIZE bytes.
 *
 * For sector-based families (F2/F4/F7/H7) with large erase units
 * (128-256KB), the common code's malloc(FLASH_SECTOR_SIZE) sector
 * buffer requires a significant heap allocation that may be impractical
 * on devices with limited RAM.
 */
#if defined(STM32F2XX) || defined(STM32F4XX)
/* Variable sectors: JIT area is in the 128KB sector region */
#define FLASH_SECTOR_SIZE (128 * 1024)
#elif defined(STM32F7XX)
/* Variable sectors: JIT area is in the 256KB sector region */
#define FLASH_SECTOR_SIZE (256 * 1024)
#elif defined(STM32H7XX)
/* Uniform 128KB sectors */
#define FLASH_SECTOR_SIZE (128 * 1024)
#elif defined(STM32H5XX) || defined(STM32U3XX) || defined(STM32U5XX)
/* Uniform 8KB sectors */
#define FLASH_SECTOR_SIZE (8 * 1024)
#elif defined(STM32L5XX) || defined(STM32WBXX)
/* Uniform 4KB pages */
#define FLASH_SECTOR_SIZE (4 * 1024)
#elif defined(STM32G0XX) || defined(STM32G4XX) || defined(STM32L4XX)
/* Uniform 2KB pages */
#define FLASH_SECTOR_SIZE (2 * 1024)
#else
#error "Unknown STM32 family for JIT flash sector size"
#endif

/*
 * FLASH_PAGE_SIZE is the write buffer size used by jit_stream_flash.c.
 * It must be >= the hardware program unit (32 bytes on H7, 16 bytes on
 * H5/U3/U5, 8 bytes on G0/G4/L4/L5/WB, 4 bytes on F2/F4/F7).
 * 256 bytes is a common value that works for all families.
 */
#define FLASH_PAGE_SIZE 256

#ifdef __cplusplus
}
#endif

#endif // _JIT_STREAM_FLASH_PLATFORM_H_
