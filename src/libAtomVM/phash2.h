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

#ifndef _PHASH2_H_
#define _PHASH2_H_

#include <stdint.h>

#include "term.h"

#ifdef __cplusplus
extern "C" {
#endif

struct GlobalContext;

/**
 * @brief Bit-exact port of ERTS make_hash2 (the hash behind erlang:phash2).
 *
 * @details The full 32-bit hash. erlang:phash2/1 truncates this to 27 bits,
 * erlang:phash2/2 reduces it modulo the range.
 * @param t the term to hash.
 * @param glb the global context.
 * @return the 32-bit hash value, identical to Erlang/OTP's make_hash2.
 */
uint32_t phash2_hash(term t, struct GlobalContext *glb);

#ifdef __cplusplus
}
#endif

#endif // _PHASH2_H_
