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

// STM32 flash constants for JIT stream buffering.
// The actual flash erase/program granularity varies by STM32 family
// (2KB pages on L4/G0/G4/WB, 8KB on H5/U5, 16-128KB sectors on F4/F7,
// 128KB sectors on H7), but these constants are used by the common
// jit_stream_flash.c for page buffering and sector management.
#define FLASH_SECTOR_SIZE 4096
#define FLASH_PAGE_SIZE 256

#ifdef __cplusplus
}
#endif

#endif // _JIT_STREAM_FLASH_PLATFORM_H_
