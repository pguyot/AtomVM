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

#ifndef AVM_NO_JIT

#include "context.h"
#include "globalcontext.h"
#include "jit.h"
#include "module.h"
#include "sys.h"
#include "term.h"

#include <stdint.h>

ModuleNativeEntryPoint jit_stream_entry_point(Context *ctx, term jit_stream)
{
    UNUSED(ctx);
    UNUSED(jit_stream);
    return NULL;
}

bool sys_get_cache_native_code(GlobalContext *global, Module *mod, uint16_t *version, ModuleNativeEntryPoint *entry_point, uint32_t *labels)
{
    UNUSED(global);
    UNUSED(mod);
    UNUSED(version);
    UNUSED(entry_point);
    UNUSED(labels);
    return false;
}

void sys_set_cache_native_code(GlobalContext *global, Module *mod, uint16_t version, ModuleNativeEntryPoint entry_point, uint32_t labels)
{
    UNUSED(global);
    UNUSED(mod);
    UNUSED(version);
    UNUSED(entry_point);
    UNUSED(labels);
}

#endif
