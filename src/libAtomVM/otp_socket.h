/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 Fred Dushin <fred@dushin.net>
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

#ifndef _OTP_SOCKET_H_
#define _OTP_SOCKET_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <erl_nif.h>
#include <globalcontext.h>
#include <nifs.h>
#include <term.h>

struct SocketFd
{
    int fd;
    term pid;
    uint64_t ref_ticks;
    int32_t selecting_process_id;
    ErlNifMonitor selecting_process_monitor;
};

const struct Nif *otp_socket_nif_get_nif(const char *nifname);

void otp_socket_init(GlobalContext *global);

#ifdef __cplusplus
}
#endif

#endif
