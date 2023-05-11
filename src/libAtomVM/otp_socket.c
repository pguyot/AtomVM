/*
 * This file is part of AtomVM.
 *
 * Copyright 2023 by Fred Dushin <fred@dushin.net>
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

#include <context.h>
#include <defaultatoms.h>
#include <dictionary.h>
#include <erl_nif_priv.h>
#include <globalcontext.h>
#include <interop.h>
#include <list.h>
#include <mailbox.h>
#include <nifs.h>
#include <otp_socket.h>
#include <otp_socket_platform.h>
#include <port.h>
#include <posix_nifs.h>
#include <scheduler.h>
#include <sys.h>
#include <term.h>
#include <utils.h>

#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

// #define ENABLE_TRACE
#include <trace.h>

static const char *const addr_atom = ATOM_STR("\x4", "addr");
static const char *const any_atom = ATOM_STR("\x3", "any");
static const char *const closed_atom = ATOM_STR("\x6", "closed");
static const char *const dgram_atom = ATOM_STR("\x5", "dgram");
static const char *const inet_atom = ATOM_STR("\x4", "inet");
static const char *const ip_atom = ATOM_STR("\x2", "ip");
static const char *const linger_atom = ATOM_STR("\x6", "linger");
static const char *const loopback_atom = ATOM_STR("\x8", "loopback");
static const char *const onoff_atom = ATOM_STR("\x5", "onoff");
static const char *const port_atom = ATOM_STR("\x4", "port");
static const char *const reuseaddr_atom = ATOM_STR("\x9", "reuseaddr");
static const char *const stream_atom = ATOM_STR("\x6", "stream");
static const char *const tcp_atom = ATOM_STR("\x3", "tcp");
static const char *const udp_atom = ATOM_STR("\x3", "udp");

#define ADDR_ATOM globalcontext_make_atom(global, addr_atom)
#define CLOSE_INTERNAL_ATOM globalcontext_make_atom(global, close_internal_atom)
#define ACCEPT_ATOM globalcontext_make_atom(global, accept_atom)
#define RECV_ATOM globalcontext_make_atom(global, recv_atom)

#define DEFAULT_BUFFER_SIZE 512
#define MIN(A, B) (((A) < (B)) ? (A) : (B))

static ErlNifResourceType *socket_resource_type;

//
// resource operations
//

void socket_dtor(ErlNifEnv *caller_env, void *obj)
{
    UNUSED(caller_env);

    struct SocketFd *fd_obj = (struct SocketFd *) obj;
    if (fd_obj->fd) {
        close(fd_obj->fd);
        fd_obj->fd = 0;
    }
}

void socket_stop(ErlNifEnv *caller_env, void *obj, ErlNifEvent event, int is_direct_call)
{
    UNUSED(caller_env);
    UNUSED(event);
    UNUSED(is_direct_call);

    struct SocketFd *fd_obj = (struct SocketFd *) obj;

    if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        enif_demonitor_process(caller_env, fd_obj, &fd_obj->selecting_process_monitor);
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }

    TRACE("socket_stop called on fd=%i\n", fd_obj->fd);
}

void socket_down(ErlNifEnv *caller_env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    UNUSED(caller_env);
    UNUSED(pid);
    UNUSED(mon);

    struct SocketFd *fd_obj = (struct SocketFd *) obj;

    TRACE("socket_down called on process_id=%i fd=%i\n", *pid, fd_obj->fd);

    if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        enif_select(caller_env, fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil());
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }
}

static const ErlNifResourceTypeInit SocketResourceTypeInit = {
    .members = 3,
    .dtor = socket_dtor,
    .stop = socket_stop,
    .down = socket_down,
};

// register the socket_fd resource type
void otp_socket_init(GlobalContext *global)
{
    ErlNifEnv env;
    erl_nif_env_partial_init_from_globalcontext(&env, global);
    socket_resource_type = enif_init_resource_type(&env, "socket_fd", &SocketResourceTypeInit, ERL_NIF_RT_CREATE, NULL);
}

//
// socket operations
//

static uint32_t socket_tuple_to_addr(term addr_tuple)
{
    return ((term_to_int32(term_get_tuple_element(addr_tuple, 0)) & 0xFF) << 24)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 1)) & 0xFF) << 16)
        | ((term_to_int32(term_get_tuple_element(addr_tuple, 2)) & 0xFF) << 8)
        | (term_to_int32(term_get_tuple_element(addr_tuple, 3)) & 0xFF);
}

static term socket_tuple_from_addr(Context *ctx, uint32_t addr)
{
    term terms[4];
    terms[0] = term_from_int32((addr >> 24) & 0xFF);
    terms[1] = term_from_int32((addr >> 16) & 0xFF);
    terms[2] = term_from_int32((addr >> 8) & 0xFF);
    terms[3] = term_from_int32(addr & 0xFF);

    return port_create_tuple_n(ctx, 4, terms);
}

static inline int get_domain(GlobalContext *global, term domain_term)
{
    if (globalcontext_is_term_equal_to_atom_string(global, domain_term, inet_atom)) {
        return PF_INET;
    } else {
        // TODO should raise an error
        AVM_LOGW(TAG, "Unsupported domain.  Defaulting to inet.");
        return PF_INET;
    }
}

static inline int get_type(GlobalContext *global, term type_term)
{
    if (globalcontext_is_term_equal_to_atom_string(global, type_term, stream_atom)) {
        return SOCK_STREAM;
    } else if (globalcontext_is_term_equal_to_atom_string(global, type_term, dgram_atom)) {
        return SOCK_DGRAM;
    } else {
        // TODO should raise an error
        AVM_LOGW(TAG, "Unsupported type.  Defaulting to stream.");
        return SOCK_STREAM;
    }
}

static inline int get_protocol(GlobalContext *global, term protocol_term)
{
    if (globalcontext_is_term_equal_to_atom_string(global, protocol_term, ip_atom)) {
        return IPPROTO_IP;
    } else if (globalcontext_is_term_equal_to_atom_string(global, protocol_term, tcp_atom)) {
        return IPPROTO_TCP;
    } else if (globalcontext_is_term_equal_to_atom_string(global, protocol_term, udp_atom)) {
        return IPPROTO_UDP;
    } else {
        // TODO should raise an error
        AVM_LOGW(TAG, "Unsupported protocol.  Defaulting to ip.");
        return IPPROTO_IP;
    }
}

static void send_message(term pid, term message, GlobalContext *global)
{
    int local_process_id = term_to_local_process_id(pid);

    Context *target = globalcontext_get_process_lock(global, local_process_id);
    mailbox_send(target, message);
    globalcontext_get_process_unlock(global, target);
}

//
// open
//

static term nif_socket_open(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_open\n");

    UNUSED(argc);

    GlobalContext *global = ctx->global;

    term domain_term = argv[0];
    if (term_is_invalid_term(domain_term)) {
        domain_term = globalcontext_make_atom(global, inet_atom);
    }
    term type_term = argv[1];
    if (term_is_invalid_term(type_term)) {
        type_term = globalcontext_make_atom(global, stream_atom);
    }
    term protocol_term = argv[2];
    if (term_is_invalid_term(protocol_term)) {
        protocol_term = globalcontext_make_atom(global, tcp_atom);
    }

    int fd = socket(get_domain(global, domain_term), get_type(global, type_term), get_protocol(global, protocol_term));
    if (fd == -1) {

        AVM_LOGE(TAG, "Failed to initialize socket.");

        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term error_tuple = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
        term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
        return error_tuple;

    } else {

        struct SocketFd *fd_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketFd));

        if (IS_NULL_PTR(fd_obj)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        fd_obj->fd = fd;
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        fd_obj->pid = term_invalid_term();
        TRACE("nif_socket_open: Created socket fd=%i\n", fd_obj->fd);

        term obj = enif_make_resource(erl_nif_env_from_context(ctx), fd_obj);
        enif_release_resource(fd_obj);

        size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2) + REF_SIZE;
        if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term socket_term = term_alloc_tuple(2, &ctx->heap);
        uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
        term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
        term_put_tuple_element(socket_term, 0, obj);
        term_put_tuple_element(socket_term, 1, ref);

        term result = term_alloc_tuple(2, &ctx->heap);
        term_put_tuple_element(result, 0, OK_ATOM);
        term_put_tuple_element(result, 1, socket_term);

        return result;
    }
}

static term get_socket(term socket_term)
{
    return term_get_tuple_element(socket_term, 0);
}

static bool term_is_socket(term socket_term)
{
    bool ret = term_is_tuple(socket_term)
        && term_get_tuple_arity(socket_term) == 2
        && term_is_binary(term_get_tuple_element(socket_term, 0))
        && term_is_reference(term_get_tuple_element(socket_term, 1));

    TRACE("term is a socket: %i\n", ret);

    return ret;
}

//
// close
//

static term nif_socket_close(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_close\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        if (fd_obj->selecting_process_id != INVALID_PROCESS_ID) {

            //
            // If we are in select, then stop selecting
            //
            if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil()) < 0)) {
                RAISE_ERROR(BADARG_ATOM);
            }

            //
            // If there is a process (other than ourself) who is waiting on select
            // the send a {closed, Ref} message to it, so that it can break
            // out of its receive statement.
            //
            if (!term_is_invalid_term(fd_obj->pid)) {

                if (term_to_local_process_id(fd_obj->pid) != ctx->process_id) {

                    // send a {closed, Ref | undefined} message to the pid
                    if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + REF_SIZE) != MEMORY_GC_OK)) {
                        AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                    }

                    term error_tuple = term_alloc_tuple(2, &ctx->heap);
                    term_put_tuple_element(error_tuple, 0, CLOSED_ATOM);
                    term ref = (fd_obj->ref_ticks == 0) ? UNDEFINED_ATOM : term_from_ref_ticks(fd_obj->ref_ticks, &ctx->heap);
                    term_put_tuple_element(error_tuple, 1, ref);

                    TRACE("nif_socket_close: Sending msg to process %i\n", term_to_local_process_id(fd_obj->pid));
                    send_message(fd_obj->pid, error_tuple, ctx->global);
                }
            } else {
                AVM_LOGW(TAG, "Selectable socket %i was closed but no known pid is waiting.  This shouldn't happen.\n", fd_obj->fd);
            }
        }

        int res = close(fd_obj->fd);
        if (UNLIKELY(res != 0)) {
            AVM_LOGW(TAG, "Failed to close socket %i", res);
        }

        TRACE("nif_socket_close: Clearing pid for socket fd=%i\n", fd_obj->fd);
        fd_obj->fd = 0;
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        fd_obj->pid = term_invalid_term();
        fd_obj->ref_ticks = 0;
    } else {
        TRACE("Double close on socket fd %i", fd_obj->fd);
    }

    return OK_ATOM;
}

//
// select
//

static term nif_socket_select(Context *ctx, term argv[], enum ErlNifSelectFlags mode)
{
    TRACE("nif_socket_select\n");

    VALIDATE_VALUE(argv[0], term_is_socket);

    term process_pid_term = argv[1];
    VALIDATE_VALUE(process_pid_term, term_is_pid);
    int32_t process_pid = term_to_local_process_id(process_pid_term);
    TRACE("process_pid=%i\n", (int) process_pid);

    term select_ref_term = argv[2];
    if (select_ref_term != UNDEFINED_ATOM) {
        VALIDATE_VALUE(select_ref_term, term_is_reference);
    }
    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    TRACE("fd_obj->fd=%i\n", (int) fd_obj->fd);

    ErlNifEnv *env = erl_nif_env_from_context(ctx);
    if (fd_obj->selecting_process_id != process_pid && fd_obj->selecting_process_id != INVALID_PROCESS_ID) {
        if (UNLIKELY(enif_demonitor_process(env, fd_obj, &fd_obj->selecting_process_monitor) != 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
    }
    // Monitor first as select is less likely to fail and it's less expensive to demonitor
    // if select fails than to stop select if monitor fails
    if (fd_obj->selecting_process_id != process_pid) {
        if (UNLIKELY(enif_monitor_process(env, fd_obj, &process_pid, &fd_obj->selecting_process_monitor) != 0)) {
            RAISE_ERROR(BADARG_ATOM);
        }
        fd_obj->selecting_process_id = process_pid;
    }

    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, mode, fd_obj, &process_pid, select_ref_term) < 0)) {
        enif_demonitor_process(env, fd_obj, &fd_obj->selecting_process_monitor);
        fd_obj->selecting_process_id = INVALID_PROCESS_ID;
        RAISE_ERROR(BADARG_ATOM);
    }

    TRACE("nif_socket_select: Setting pid for socket fd %i to %i\n", fd_obj->fd, process_pid);
    fd_obj->pid = process_pid_term;
    fd_obj->ref_ticks = (select_ref_term == UNDEFINED_ATOM) ? 0 : term_to_ref_ticks(select_ref_term);

    return OK_ATOM;
}

static term nif_socket_select_read(Context *ctx, int argc, term argv[])
{
    UNUSED(argc);
    return nif_socket_select(ctx, argv, ERL_NIF_SELECT_READ);
}

static term nif_socket_select_stop(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_stop\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (UNLIKELY(enif_select(erl_nif_env_from_context(ctx), fd_obj->fd, ERL_NIF_SELECT_STOP, fd_obj, NULL, term_nil()) < 0)) {
        RAISE_ERROR(BADARG_ATOM);
    }

    return OK_ATOM;
}

//
// setopt
//

static term nif_socket_setopt(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_setopt\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        term level_tuple = argv[1];
        term value = argv[2];

        term opt = term_get_tuple_element(level_tuple, 1);
        if (globalcontext_is_term_equal_to_atom_string(global, opt, reuseaddr_atom)) {
            int option_value = (value == TRUE_ATOM) ? 1 : 0;
            int res = setsockopt(fd_obj->fd, SOL_SOCKET, SO_REUSEADDR, &option_value, sizeof(int));
            if (UNLIKELY(res != 0)) {

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
                return error_tuple;
            } else {
                return OK_ATOM;
            }
        } else if (globalcontext_is_term_equal_to_atom_string(global, opt, linger_atom)) {
            term onoff = interop_kv_get_value(value, onoff_atom, ctx->global);
            term linger = interop_kv_get_value(value, linger_atom, ctx->global);
            struct linger sl;
            sl.l_onoff = (onoff == TRUE_ATOM) ? 1 : 0;
            sl.l_linger = term_to_int(linger);
            int res = setsockopt(fd_obj->fd, SOL_SOCKET, SO_LINGER, &sl, sizeof(sl));
            if (UNLIKELY(res != 0)) {

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }
                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
                return error_tuple;
            } else {
                return OK_ATOM;
            }
            // TODO add more as needed
            // int flag = 1;
            // int res = setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, (char *) &flag, sizeof(int));
            // if (UNLIKELY(res != 0)) {
            //     AVM_LOGW(TAG, "Failed to set TCP_NODELAY.");
            // }
        } else {
            RAISE_ERROR(BADARG_ATOM);
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// sockname
//

static term nif_socket_sockname(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_sockname\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        int res = getsockname(fd_obj->fd, (struct sockaddr *) &addr, &addrlen);

        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", fd_obj->fd, res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
            return error_tuple;
        } else {
            // {ok, #{addr => {a,b,c,d}, port => integer()}}
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            } else {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term return_value = port_create_tuple2(ctx, OK_ATOM, map);

                return return_value;
            }
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// peername
//

static term nif_socket_peername(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_peername\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        struct sockaddr_in addr;
        socklen_t addrlen = sizeof(addr);
        int res = getpeername(fd_obj->fd, (struct sockaddr *) &addr, &addrlen);

        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to getsockname: fd=%i res=%i.", fd_obj->fd, res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
            return error_tuple;
        } else {
            // {ok, #{addr => {a,b,c,d}, port => integer()}}
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2) + term_map_size_in_terms(2) + TUPLE_SIZE(4)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            } else {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term return_value = port_create_tuple2(ctx, OK_ATOM, map);

                return return_value;
            }
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// bind
//

static term nif_socket_bind(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_bind\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        term sockaddr = argv[1];

        struct sockaddr_in serveraddr;
        memset(&serveraddr, 0, sizeof(serveraddr));
        serveraddr.sin_family = AF_INET;

        if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, any_atom)) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
        } else if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, loopback_atom)) {
            serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (term_is_map(sockaddr)) {
            term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
            serveraddr.sin_port = htons(term_to_int(port));
            term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
            if (globalcontext_is_term_equal_to_atom_string(global, addr, any_atom)) {
                serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
            } else if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
                serveraddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
            } else {
                serveraddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
            }
        }

        socklen_t address_len = sizeof(serveraddr);
        int res = bind(fd_obj->fd, (struct sockaddr *) &serveraddr, address_len);
        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to bind socket: res=%i.", res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
            return error_tuple;
        } else {
            return OK_ATOM;
        }

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// listen
//

static term nif_socket_listen(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_listen\n");

    GlobalContext *global = ctx->global;

    VALIDATE_VALUE(argv[0], term_is_socket);

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        int backlog = argc > 1 ? term_to_int(argv[1]) : 4;
        int res = listen(fd_obj->fd, backlog);
        if (UNLIKELY(res != 0)) {
            AVM_LOGE(TAG, "Unable to listen on socket: res=%i.", res);
            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
            return error_tuple;
        } else {
            return OK_ATOM;
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// accept
//

static term nif_socket_accept(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_accept\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        struct sockaddr_in clientaddr;
        socklen_t clientlen = sizeof(clientaddr);
        int fd = accept(fd_obj->fd, (struct sockaddr *) &clientaddr, &clientlen);
        if (fd == -1) {
            AVM_LOGE(TAG, "Unable to accept on socket %i.", fd_obj->fd);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            int err = errno;
            term reason = (err == ECONNABORTED) ? CLOSED_ATOM : posix_errno_to_term(err, global);
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, reason);
            return error_tuple;

        } else {

            struct SocketFd *conn_fd_obj = enif_alloc_resource(socket_resource_type, sizeof(struct SocketFd));
            conn_fd_obj->fd = fd;
            conn_fd_obj->selecting_process_id = INVALID_PROCESS_ID;
            TRACE("nif_socket_accept: Created socket on accept fd=%i\n", fd_obj->fd);

            term obj = enif_make_resource(erl_nif_env_from_context(ctx), conn_fd_obj);
            enif_release_resource(conn_fd_obj);

            size_t requested_size = TUPLE_SIZE(2) + TUPLE_SIZE(2) + REF_SIZE;
            if (UNLIKELY(memory_ensure_free_with_roots(ctx, requested_size, 1, &obj, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term socket_term = term_alloc_tuple(2, &ctx->heap);
            uint64_t ref_ticks = globalcontext_get_ref_ticks(ctx->global);
            term ref = term_from_ref_ticks(ref_ticks, &ctx->heap);
            term_put_tuple_element(socket_term, 0, obj);
            term_put_tuple_element(socket_term, 1, ref);

            term result = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(result, 0, OK_ATOM);
            term_put_tuple_element(result, 1, socket_term);

            return result;
        }

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// recv/recvfrom
//

static term nif_socket_recv_with_peek(Context *ctx, int argc, term argv[], bool is_recvfrom)
{
    TRACE("nif_socket_recv_with_peek\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {
        int flags = MSG_WAITALL;
        // TODO parameterize buffer size
        ssize_t res = recvfrom(fd_obj->fd, NULL, DEFAULT_BUFFER_SIZE, MSG_PEEK | flags, NULL, NULL);
        TRACE("%li bytes available.\n", (long int) res);
        if (res < 0) {
            AVM_LOGI(TAG, "Unable to receive data on fd %i.  errno=%i", fd_obj->fd, errno);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, posix_errno_to_term(errno, global));
            return error_tuple;

        } else if (res == 0) {

            TRACE("Peer closed socket %i.\n", fd_obj->fd);

            if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(global, closed_atom));
            return error_tuple;

        } else {

            size_t len = (size_t) res;
            ssize_t buffer_size = MIN(len, DEFAULT_BUFFER_SIZE);

            // {ok, Data :: binary()}}
            size_t ensure_packet_avail = term_binary_data_size_in_terms(buffer_size) + BINARY_HEADER_SIZE;
            size_t requested_size = ensure_packet_avail + (is_recvfrom ? (TUPLE_SIZE(2) + term_map_size_in_terms(2)) : 0);

            if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }

            term data = term_create_uninitialized_binary(buffer_size, &ctx->heap, global);
            const char *buffer = term_binary_data(data);

            //
            // receive data on the socket
            //
            struct sockaddr_in addr;
            socklen_t addrlen = sizeof(addr);
            res = recvfrom(fd_obj->fd, (char *) buffer, buffer_size, flags, (struct sockaddr *) &addr, &addrlen);

            TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", fd_obj->fd, (unsigned long) len);

            term payload = term_invalid_term();
            if (is_recvfrom) {
                term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                term port_number = term_from_int(ntohs(addr.sin_port));

                term map = term_alloc_map(2, &ctx->heap);
                term_set_map_assoc(map, 0, ADDR_ATOM, address);
                term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
                payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
            } else {
                payload = port_heap_create_ok_tuple(&ctx->heap, data);
            }

            return payload;
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_socket_recv_without_peek(Context *ctx, int argc, term argv[], bool is_recvfrom)
{
    TRACE("nif_socket_recv_without_peek\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        // TODO plumb through buffer size
        size_t buffer_size = DEFAULT_BUFFER_SIZE;
        char *buffer = malloc(buffer_size);
        term payload = term_invalid_term();

        if (IS_NULL_PTR(buffer)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);

        } else {

            int flags = 0;
            struct sockaddr_in addr;
            socklen_t addrlen = sizeof(addr);
            ssize_t res = recvfrom(fd_obj->fd, (char *) buffer, buffer_size, flags, (struct sockaddr *) &addr, &addrlen);

            if (res < 0) {

                int err = errno;
                term reason = (err == ECONNRESET) ? globalcontext_make_atom(global, ATOM_STR("\xA", "econnreset")) : posix_errno_to_term(err, global);

                if (err == ECONNRESET) {
                    AVM_LOGI(TAG, "Peer closed connection.");
                } else {
                    AVM_LOGE(TAG, "Unable to read data on socket %i.  errno=%i", fd_obj->fd, errno);
                }

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, reason);
                return error_tuple;

            } else if (res == 0) {

                TRACE("Peer closed socket %i.\n", fd_obj->fd);

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, CLOSED_ATOM);
                return error_tuple;

            } else {

                size_t len = (size_t) res;
                TRACE("otp_socket.recv_handler: received data on fd: %i len=%lu\n", fd_obj->fd, (unsigned long) len);

                size_t ensure_packet_avail = term_binary_data_size_in_terms(len) + BINARY_HEADER_SIZE;
                size_t requested_size = REF_SIZE + 2 * TUPLE_SIZE(2) + ensure_packet_avail + (is_recvfrom ? (TUPLE_SIZE(2) + term_map_size_in_terms(2)) : 0);

                if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                term data = term_from_literal_binary(buffer, len, &ctx->heap, global);

                if (is_recvfrom) {
                    term address = socket_tuple_from_addr(ctx, ntohl(addr.sin_addr.s_addr));
                    term port_number = term_from_int(ntohs(addr.sin_port));

                    term map = term_alloc_map(2, &ctx->heap);
                    term_set_map_assoc(map, 0, ADDR_ATOM, address);
                    term_set_map_assoc(map, 1, PORT_ATOM, port_number);
                    term tuple = port_heap_create_tuple2(&ctx->heap, map, data);
                    payload = port_heap_create_ok_tuple(&ctx->heap, tuple);
                } else {
                    payload = port_heap_create_ok_tuple(&ctx->heap, data);
                }
            }

            free(buffer);
            return payload;
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_socket_recv_internal(Context *ctx, int argc, term argv[], bool is_recvfrom)
{
    if (otp_socket_platform_supports_peek()) {
        return nif_socket_recv_with_peek(ctx, argc, argv, is_recvfrom);
    } else {
        return nif_socket_recv_without_peek(ctx, argc, argv, is_recvfrom);
    }
}

static term nif_socket_recv(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_recv\n");
    return nif_socket_recv_internal(ctx, argc, argv, false);
}

static term nif_socket_recvfrom(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_recvfrom\n");
    return nif_socket_recv_internal(ctx, argc, argv, true);
}

//
// send/sendto
//

static term nif_socket_send_internal(Context *ctx, int argc, term argv[], bool is_sendto)
{
    TRACE("nif_socket_send_internal\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        term data = argv[1];
        term dest = term_invalid_term();
        if (is_sendto) {
            dest = argv[2];
        }

        // TODO make non-blocking

        char *buf = NULL;
        size_t len = 0;
        if (term_is_binary(data)) {
            buf = (char *) term_binary_data(data);
            len = term_binary_size(data);
        } else if (term_is_list(data)) {
            InteropFunctionResult result = interop_iolist_size(data, &len);
            if (UNLIKELY(result == InteropMemoryAllocFail)) {
                AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            if (UNLIKELY(result == InteropBadArg)) {
                TRACE("badarg: input data is not an iolist\n");
                RAISE_ERROR(BADARG_ATOM);
            }
            // TODO: send an iolist without allocating a buffer, maybe by iterative sends
            buf = malloc(len);
            if (IS_NULL_PTR(buf)) {
                AVM_LOGW(TAG, "Failed to allocate %lu bytes for buffer\n", (unsigned long) len);
                free(buf);
                RAISE_ERROR(OUT_OF_MEMORY_ATOM);
            }
            if (UNLIKELY(interop_write_iolist(data, buf) != InteropOk)) {
                TRACE("badarg: unable to write iolist\n");
                free(buf);
                RAISE_ERROR(BADARG_ATOM);
            }
        } else {
            TRACE("badarg: input data is not a list\n");
            RAISE_ERROR(BADARG_ATOM);
        }

        int sent_data = -1;

        if (is_sendto) {

            struct sockaddr_in destaddr;
            memset(&destaddr, 0, sizeof(destaddr));
            destaddr.sin_family = AF_INET;

            term port = interop_kv_get_value_default(dest, port_atom, term_from_int(0), global);
            destaddr.sin_port = htons(term_to_int(port));
            term addr = interop_kv_get_value(dest, addr_atom, ctx->global);
            if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
                destaddr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
            } else {
                destaddr.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
            }

            sent_data = sendto(fd_obj->fd, buf, len, 0, (struct sockaddr *) &destaddr, sizeof(destaddr));

        } else {
            sent_data = send(fd_obj->fd, buf, len, 0);
        }

        // {ok, RestData} | {error, Reason}  (RestData currently nil)
        // TODO we should allocate space for the sub-binary here when we support returning Rest
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }

        term ret = term_invalid_term();
        if (sent_data == -1) {

            AVM_LOGE(TAG, "Unable to send data: res=%i.", sent_data);
            term error_tuple = term_alloc_tuple(2, &ctx->heap);
            term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
            term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(global, closed_atom));
            ret = error_tuple;

        } else {
            // TODO allocate substring that references the data that was not sent
            ret = port_create_tuple2(ctx, OK_ATOM, term_nil());
        }

        if (term_is_list(data)) {
            free(buf);
        }
        return ret;

    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

static term nif_socket_send(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_send\n");
    return nif_socket_send_internal(ctx, argc, argv, false);
}

static term nif_socket_sendto(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_sendto\n");
    return nif_socket_send_internal(ctx, argc, argv, true);
}

//
// connect
//

static term nif_socket_connect(Context *ctx, int argc, term argv[])
{
    TRACE("nif_socket_connect\n");
    UNUSED(argc);

    VALIDATE_VALUE(argv[0], term_is_socket);

    GlobalContext *global = ctx->global;

    void *fd_obj_ptr;
    if (UNLIKELY(!enif_get_resource(erl_nif_env_from_context(ctx), get_socket(argv[0]), socket_resource_type, &fd_obj_ptr))) {
        RAISE_ERROR(BADARG_ATOM);
    }
    struct SocketFd *fd_obj = (struct SocketFd *) fd_obj_ptr;
    if (fd_obj->fd) {

        term sockaddr = argv[1];

        // TODO make connect non-blocking
        // int fctl_res = fcntl(socket_data->fd, F_SETFL, O_NONBLOCK);
        // if (fctl_res == -1) {
        //     AVM_LOGE(TAG, "Unable to set socket to non-blocking: fd=%i fctl_res=%i errno=%i", socket_data->fd, fctl_res, errno);
        //     sync_send_error_tuple(ctx, pid, ref_ticks, term_from_int(errno));
        //     return;
        // }

        struct sockaddr_in address;
        memset(&address, 0, sizeof(struct sockaddr_in));
        address.sin_family = AF_INET;

        // TODO is `loopback` a legal value?
        if (globalcontext_is_term_equal_to_atom_string(global, sockaddr, loopback_atom)) {
            address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
        } else if (term_is_map(sockaddr)) {
            term port = interop_kv_get_value_default(sockaddr, port_atom, term_from_int(0), ctx->global);
            address.sin_port = htons(term_to_int(port));
            term addr = interop_kv_get_value(sockaddr, addr_atom, ctx->global);
            if (globalcontext_is_term_equal_to_atom_string(global, addr, loopback_atom)) {
                address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
            } else {
                address.sin_addr.s_addr = htonl(socket_tuple_to_addr(addr));
            }
        }
        socklen_t addr_len = sizeof(struct sockaddr_in);
        int res = connect(fd_obj->fd, (const struct sockaddr *) &address, addr_len);
        if (res == -1) {
            if (errno == EINPROGRESS) {

                // TODO make connect non-blocking
                return UNDEFINED_ATOM;

            } else {
                AVM_LOGE(TAG, "Unable to connect: res=%i errno=%i", res, errno);

                if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
                    AVM_LOGW(TAG, "Failed to allocate memory: %s:%i.\n", __FILE__, __LINE__);
                    RAISE_ERROR(OUT_OF_MEMORY_ATOM);
                }

                term error_tuple = term_alloc_tuple(2, &ctx->heap);
                term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
                term_put_tuple_element(error_tuple, 1, globalcontext_make_atom(global, closed_atom));
                return error_tuple;
            }
        } else if (res == 0) {
            return OK_ATOM;
        } else {
            // won't happen according to connect(2)
            return UNDEFINED_ATOM;
        }
    } else {
        RAISE_ERROR(BADARG_ATOM);
    }
}

//
// Nifs
//

static const struct Nif socket_open_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_open
};
static const struct Nif socket_close_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_close
};
static const struct Nif socket_select_stop_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_select_stop
};
static const struct Nif socket_setopt_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_setopt
};
static const struct Nif socket_bind_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_bind
};
static const struct Nif socket_listen_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_listen
};
static const struct Nif socket_sockname_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_sockname
};
static const struct Nif socket_peername_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_peername
};
static const struct Nif socket_select_read_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_select_read
};
static const struct Nif socket_accept_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_accept
};
static const struct Nif socket_recv_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_recv
};
static const struct Nif socket_recvfrom_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_recvfrom
};
static const struct Nif socket_send_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_send
};
static const struct Nif socket_sendto_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_sendto
};
static const struct Nif socket_connect_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_socket_connect
};

const struct Nif *otp_socket_nif_get_nif(const char *nifname)
{
    if (strcmp("socket:open/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_open_nif;
    }
    if (strcmp("socket:close/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_close_nif;
    }
    if (strcmp("socket:nif_select_stop/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_select_stop_nif;
    }
    if (strcmp("socket:setopt/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_setopt_nif;
    }
    if (strcmp("socket:bind/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_bind_nif;
    }
    if (strcmp("socket:listen/1", nifname) == 0 || strcmp("socket:listen/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_listen_nif;
    }
    if (strcmp("socket:sockname/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_sockname_nif;
    }
    if (strcmp("socket:peername/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_peername_nif;
    }
    if (strcmp("socket:nif_select_read/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_select_read_nif;
    }
    if (strcmp("socket:nif_accept/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_accept_nif;
    }
    if (strcmp("socket:nif_recv/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_recv_nif;
    }
    if (strcmp("socket:nif_recvfrom/1", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_recvfrom_nif;
    }
    if (strcmp("socket:send/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_send_nif;
    }
    if (strcmp("socket:sendto/3", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_sendto_nif;
    }
    if (strcmp("socket:connect/2", nifname) == 0) {
        TRACE("Resolved platform nif %s ...\n", nifname);
        return &socket_connect_nif;
    }
    return NULL;
}
