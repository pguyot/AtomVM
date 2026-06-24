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
#include <globalcontext.h>
#include <inet.h>
#include <interop.h>
#include <nifs.h>
#include <otp_net.h>
#include <port.h>
#include <posix_nifs.h>
#include <term.h>

#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>

#ifdef HAVE_GETIFADDRS
#include <ifaddrs.h>
#include <net/if.h>
#endif

// #define ENABLE_TRACE
#include <trace.h>

#define UNKNOWN_TABLE_VALUE -1

static const AtomStringIntPair protocol_table[] = {
    { ATOM_STR("\x3", "tcp"), IPPROTO_TCP },
    { ATOM_STR("\x3", "udp"), IPPROTO_UDP },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

static const AtomStringIntPair type_table[] = {
    { ATOM_STR("\x5", "dgram"), SOCK_DGRAM },
    { ATOM_STR("\x6", "stream"), SOCK_STREAM },
    SELECT_INT_DEFAULT(UNKNOWN_TABLE_VALUE)
};

//
// utilities
//

static inline term make_error_tuple(term reason, Context *ctx)
{
    term error_tuple = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(error_tuple, 0, ERROR_ATOM);
    term_put_tuple_element(error_tuple, 1, reason);
    return error_tuple;
}

static term eai_errno_to_term(int err, GlobalContext *glb)
{
    switch (err) {
        case EAI_AGAIN:
            return globalcontext_make_atom(glb, ATOM_STR("\x8", "eaiagain"));
        case EAI_BADFLAGS:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaibadflags"));
        case EAI_FAIL:
            return globalcontext_make_atom(glb, ATOM_STR("\x7", "eaifail"));
        case EAI_FAMILY:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eaifamily"));
        case EAI_MEMORY:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eaimemory"));
        case EAI_NONAME:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eainoname"));
        case EAI_SERVICE:
            return globalcontext_make_atom(glb, ATOM_STR("\xA", "eaiservice"));
        case EAI_SOCKTYPE:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaisocktype"));
#ifdef HAVE_EXTENDED_EAI_ERRNO
        case EAI_BADHINTS:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaibadhints"));
#ifdef HAVE_EAI_OVERFLOW
        case EAI_OVERFLOW:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaioverflow"));
#endif
        case EAI_PROTOCOL:
            return globalcontext_make_atom(glb, ATOM_STR("\xB", "eaiprotocol"));
        case EAI_SYSTEM:
            return globalcontext_make_atom(glb, ATOM_STR("\x9", "eaisystem"));
#endif
    }
    return term_from_int(err);
}

/**
 * @brief Make a getaddrino result item as part of the iteration
 * @param keys pointer to a term to store the keys of the map. If it's
 *             invalid_term, a non-shared map will be created and the keys term
 *             will be updated. Otherwise, it's used to create a shared map
 * @param ai_protocol protocol field of the addrinfo
 * @param ai_socktype socktype field of the addrinfo
 * @param inner_addr IP address  that will be stored in both address and addr
 *             entries of the map
 * @param global the global context
 * @return the getaddrinfo result item term
 * @param heap the heap to create terms in, should have sufficient free space
 * @details This function is called in a loop to create optimized maps that
 * share keys.
 */
static term make_getaddrinfo_result(term *keys, int ai_protocol, int ai_socktype, term inner_addr, GlobalContext *global, Heap *heap)
{
    term result_map;
    if (term_is_invalid_term(*keys)) {
        result_map = term_alloc_map(5, heap);
    } else {
        result_map = term_alloc_map_maybe_shared(5, *keys, heap);
    }

    // in the current implementation, this will always be `inet`
    term family_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "family"));
    term family = globalcontext_make_atom(global, ATOM_STR("\x4", "inet"));
    term_set_map_assoc(result_map, 0, family_atom, family);

    term protocol_atom = globalcontext_make_atom(global, ATOM_STR("\x8", "protocol"));
    term protocol = interop_atom_term_select_atom(protocol_table, ai_protocol, global);
    term_set_map_assoc(result_map, 1, protocol_atom, term_is_invalid_term(protocol) ? UNDEFINED_ATOM : protocol);

    term type_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "type"));
    term type = interop_atom_term_select_atom(type_table, ai_socktype, global);
    term_set_map_assoc(result_map, 2, type_atom, term_is_invalid_term(type) ? UNDEFINED_ATOM : type);

    // embed the inner_addr, but reference it from both address and addr
    // for compatibility with OTP
    term address_atom = globalcontext_make_atom(global, ATOM_STR("\x7", "address"));
    term_set_map_assoc(result_map, 3, address_atom, inner_addr);

    term addr_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "addr"));
    term_set_map_assoc(result_map, 4, addr_atom, inner_addr);

    if (term_is_invalid_term(*keys)) {
        *keys = term_get_map_keys(result_map);
    }

    return result_map;
}

//
// net:getaddrinfo/1
//

static term nif_net_getaddrinfo(Context *ctx, int argc, term argv[])
{
    TRACE("nif_net_getaddrinfo\n");
    UNUSED(argc);

    GlobalContext *global = ctx->global;

    term host = argv[0];
    term service = argv[1];

    if (host == UNDEFINED_ATOM && service == UNDEFINED_ATOM) {
        TRACE("Host and Service params may not both be undefined\n");
        RAISE_ERROR(BADARG_ATOM);
    }

    char *host_str = NULL;
    if (host != UNDEFINED_ATOM) {
        int ok;
        host_str = interop_term_to_string(host, &ok);
        if (!ok) {
            RAISE_ERROR(BADARG_ATOM);
        }
        TRACE("Host: %s\n", host_str);
    }

    char *service_str = NULL;
    if (service != UNDEFINED_ATOM) {
        int ok;
        service_str = interop_term_to_string(service, &ok);
        if (!ok) {
            free(host_str);
            RAISE_ERROR(BADARG_ATOM);
        }
        TRACE("Service: %s\n", service_str);
    }

    avm_uint_t port = 0;
#ifdef HAVE_SERVBYNAME
    if (!IS_NULL_PTR(service_str)) {
        struct servent *svt = getservbyname(service_str, NULL);
        if (!IS_NULL_PTR(svt)) {
            port = ntohs(svt->s_port);
        }
    }
#endif
    TRACE("port: %zu\n", port);

    // for now, we are only supporting IPv4 addresses
    struct addrinfo hints;
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = PF_INET;

    struct addrinfo *host_info;
    int err = getaddrinfo(host_str, service_str, &hints, &host_info);

    // some implementations do not support service filters
    if (err == EAI_SERVICE) {
        fprintf(stderr, "WARNING: EAI_SERVICE unsupported on this platform.\n");
        err = getaddrinfo(host_str, NULL, &hints, &host_info);
    }

    free(host_str);
    free(service_str);

    if (err != 0 && err != EAI_SERVICE) {
        if (UNLIKELY(memory_ensure_free(ctx, TUPLE_SIZE(2)) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(eai_errno_to_term(err, global), ctx);
    }
    TRACE("getaddrinfo succeeded\n");

    size_t num_addrinfos = 0;
    for (struct addrinfo *p = host_info; p != NULL; p = p->ai_next) {
        num_addrinfos++;
    }
    TRACE("num_addrinfos: %zu\n", num_addrinfos);

    if (num_addrinfos == 0) {
        return term_nil();
    }

    // {ok, [#{
    //      family => Family :: atom()
    //      protocol => Protocol :: atom()
    //      type -> Type :: atom()
    //      address, addr =>
    //          #{
    //              addr => Address :: {0..255, 0..255, 0..255, 0..255},
    //              port => 0..65535,
    //              family => inet
    //          }
    // }]}
    // Note.  We might over-allocate for some more esoteric calls

    // Determine the number of entries, if we have ai_protocol or ai_socktype as unspec, return two
    size_t nb_results = 0;
    size_t requested_size = TUPLE_SIZE(2); // {ok, _}
    for (struct addrinfo *p = host_info; p != NULL; p = p->ai_next) {
        // Each list item is:
        // 1 CONS
        // 1 IPv4 address
        // 1 map with 5 items (family, protocol, type, address, addr)
        // 1 map with 3 items (addr, port, family)
        requested_size += CONS_SIZE + INET_ADDR4_TUPLE_SIZE;
        // First result: regular maps
        // Subsequent results: shared maps
        if (nb_results) {
            requested_size += TERM_MAP_SHARED_SIZE(5) + TERM_MAP_SHARED_SIZE(3);
        } else {
            requested_size += TERM_MAP_SIZE(5) + TERM_MAP_SIZE(3);
        }
        nb_results++;
        // If protocol or socktype are unspecified (what esp-idf returns), add
        // another entry so we'll have tcp and udp
        if (p->ai_protocol == 0 || p->ai_socktype == 0) {
            nb_results++;
            // We only need cons and shared maps here as the IP address will be shared
            requested_size += CONS_SIZE + TERM_MAP_SHARED_SIZE(5) + TERM_MAP_SHARED_SIZE(3);
        }
    }
    if (UNLIKELY(memory_ensure_free(ctx, requested_size) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term infos = term_nil();
    term result_keys = term_invalid_term();
    term addrinfo_keys = term_invalid_term();
    term family_atom = globalcontext_make_atom(global, ATOM_STR("\x6", "family"));
    term inet_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "inet"));
    for (struct addrinfo *p = host_info; p != NULL; p = p->ai_next) {
        term addrinfo_map;
        if (term_is_invalid_term(addrinfo_keys)) {
            addrinfo_map = term_alloc_map(3, &ctx->heap);
        } else {
            addrinfo_map = term_alloc_map_maybe_shared(3, addrinfo_keys, &ctx->heap);
        }
        // The inner addr contains a family, port, and addr
        term addr_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "addr"));
        term_set_map_assoc(addrinfo_map, 0, family_atom, inet_atom);
        term_set_map_assoc(addrinfo_map, 1, PORT_ATOM, term_from_int(port));
        term address = inet_make_addr4(ntohl(((struct sockaddr_in *) p->ai_addr)->sin_addr.s_addr), &ctx->heap);
        term_set_map_assoc(addrinfo_map, 2, addr_atom, address);

        if (term_is_invalid_term(addrinfo_keys)) {
            addrinfo_keys = term_get_map_keys(addrinfo_map);
        }

        if (p->ai_protocol != 0 && p->ai_socktype != 0) {
            term result_map = make_getaddrinfo_result(&result_keys, p->ai_protocol, p->ai_socktype, addrinfo_map, ctx->global, &ctx->heap);
            infos = term_list_prepend(result_map, infos, &ctx->heap);
        } else {
            term tcp_map = make_getaddrinfo_result(&result_keys, IPPROTO_TCP, SOCK_STREAM, addrinfo_map, ctx->global, &ctx->heap);
            infos = term_list_prepend(tcp_map, infos, &ctx->heap);
            term udp_map = make_getaddrinfo_result(&result_keys, IPPROTO_UDP, SOCK_DGRAM, addrinfo_map, ctx->global, &ctx->heap);
            infos = term_list_prepend(udp_map, infos, &ctx->heap);
        }
    }
    freeaddrinfo(host_info);

    term ret = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(ret, 0, OK_ATOM);
    term_put_tuple_element(ret, 1, infos);

#ifdef ENABLE_TRACE
    fprintf(stdout, "host info: ");
    term_display(stdout, ret, ctx);
    fprintf(stdout, "\n");
#endif

    return ret;
}

//
// net:gethostname/0
//
#ifdef HAVE_GETHOSTNAME
static term nif_net_gethostname(Context *ctx, int argc, term argv[])
{
    TRACE("nif_net_gethostname\n");
    UNUSED(argc);
    UNUSED(argv);

    char buf[256];
    int r = gethostname(buf, sizeof(buf));
    if (UNLIKELY(r != 0)) {
        if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(posix_errno_to_term(errno, ctx->global), ctx);
    }
    size_t len = strlen(buf);
    if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2) + LIST_SIZE(len, 1), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }

    term result = term_alloc_tuple(2, &ctx->heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, interop_bytes_to_list(buf, len, &ctx->heap));

    return result;
}
#endif

//
// net:getifaddrs/0
//
#ifdef HAVE_GETIFADDRS

static int count_iface_flags(unsigned int f)
{
    int n = 0;
    if (f & IFF_UP) {
        n++;
    }
    if (f & IFF_BROADCAST) {
        n++;
    }
    if (f & IFF_LOOPBACK) {
        n++;
    }
    if (f & IFF_POINTOPOINT) {
        n++;
    }
    if (f & IFF_RUNNING) {
        n++;
    }
    if (f & IFF_MULTICAST) {
        n++;
    }
    return n;
}

// Build the {flags, [...]} list of interface flag atoms.
static term make_iface_flags(unsigned int f, GlobalContext *global, Heap *heap)
{
    term flags = term_nil();
    if (f & IFF_MULTICAST) {
        flags = term_list_prepend(globalcontext_make_atom(global, ATOM_STR("\x9", "multicast")), flags, heap);
    }
    if (f & IFF_RUNNING) {
        flags = term_list_prepend(globalcontext_make_atom(global, ATOM_STR("\x7", "running")), flags, heap);
    }
    if (f & IFF_POINTOPOINT) {
        flags = term_list_prepend(globalcontext_make_atom(global, ATOM_STR("\xC", "pointtopoint")), flags, heap);
    }
    if (f & IFF_LOOPBACK) {
        flags = term_list_prepend(globalcontext_make_atom(global, ATOM_STR("\x8", "loopback")), flags, heap);
    }
    if (f & IFF_BROADCAST) {
        flags = term_list_prepend(globalcontext_make_atom(global, ATOM_STR("\x9", "broadcast")), flags, heap);
    }
    if (f & IFF_UP) {
        flags = term_list_prepend(globalcontext_make_atom(global, ATOM_STR("\x2", "up")), flags, heap);
    }
    return flags;
}

// Build an IPv6 address tuple ({0..65535} x 8) from 16 raw bytes.
static term make_addr6(const uint8_t *a, Heap *heap)
{
    term t = term_alloc_tuple(8, heap);
    for (int i = 0; i < 8; i++) {
        term_put_tuple_element(t, i, term_from_int((a[2 * i] << 8) | a[2 * i + 1]));
    }
    return t;
}

// The first entry of the next interface group (entries sharing ifa_name are
// consecutive in the getifaddrs() result).
static struct ifaddrs *iface_group_end(struct ifaddrs *start)
{
    const char *name = start->ifa_name ? start->ifa_name : "";
    struct ifaddrs *e = start;
    while (e != NULL && strcmp(e->ifa_name ? e->ifa_name : "", name) == 0) {
        e = e->ifa_next;
    }
    return e;
}

// Heap words needed to represent one interface group as {Name, Opts}.
static size_t iface_group_size(struct ifaddrs *start, struct ifaddrs *end)
{
    size_t namelen = strlen(start->ifa_name ? start->ifa_name : "");
    size_t sz = 2 * namelen + TUPLE_SIZE(2) + 2;
    sz += TUPLE_SIZE(2) + 2 + 2 * count_iface_flags(start->ifa_flags);
    for (struct ifaddrs *e = start; e != end; e = e->ifa_next) {
        if (e->ifa_addr != NULL) {
            if (e->ifa_addr->sa_family == AF_INET) {
                sz += (TUPLE_SIZE(4) + TUPLE_SIZE(2) + 2) * 2;
            } else if (e->ifa_addr->sa_family == AF_INET6) {
                sz += (TUPLE_SIZE(8) + TUPLE_SIZE(2) + 2) * 2;
            }
        }
    }
    return sz;
}

static term nif_net_getifaddrs(Context *ctx, int argc, term argv[])
{
    TRACE("nif_net_getifaddrs\n");
    UNUSED(argc);
    UNUSED(argv);

    GlobalContext *global = ctx->global;

    struct ifaddrs *ifap = NULL;
    if (UNLIKELY(getifaddrs(&ifap) != 0)) {
        if (UNLIKELY(memory_ensure_free_opt(ctx, TUPLE_SIZE(2), MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
            RAISE_ERROR(OUT_OF_MEMORY_ATOM);
        }
        return make_error_tuple(posix_errno_to_term(errno, global), ctx);
    }

    // Pass 1: size the whole result, so we can allocate once and build without
    // triggering a GC mid-construction (a 0-arity NIF has no argv roots to keep
    // a partially-built term alive across allocations).
    size_t total = TUPLE_SIZE(2);
    for (struct ifaddrs *cur = ifap; cur != NULL;) {
        struct ifaddrs *end = iface_group_end(cur);
        total += iface_group_size(cur, end);
        cur = end;
    }
    if (UNLIKELY(memory_ensure_free_opt(ctx, total, MEMORY_CAN_SHRINK) != MEMORY_GC_OK)) {
        freeifaddrs(ifap);
        RAISE_ERROR(OUT_OF_MEMORY_ATOM);
    }
    Heap *heap = &ctx->heap;

    // Pass 2: build. No further heap allocation calls below, so terms are stable.
    term addr_atom = globalcontext_make_atom(global, ATOM_STR("\x4", "addr"));
    term netmask_atom = globalcontext_make_atom(global, ATOM_STR("\x7", "netmask"));
    term flags_atom = globalcontext_make_atom(global, ATOM_STR("\x5", "flags"));
    term result_list = term_nil();
    for (struct ifaddrs *cur = ifap; cur != NULL;) {
        struct ifaddrs *end = iface_group_end(cur);
        const char *name = cur->ifa_name ? cur->ifa_name : "";
        size_t namelen = strlen(name);

        // Emit addresses in order (addr before netmask) by walking the group
        // backwards while prepending.
        term opts = term_nil();
        struct ifaddrs *prev;
        for (struct ifaddrs *e = end; e != cur; e = prev) {
            // step back one (singly linked list, so re-scan from cur)
            prev = cur;
            while (prev->ifa_next != e) {
                prev = prev->ifa_next;
            }
            struct ifaddrs *ent = prev;
            if (ent->ifa_addr == NULL) {
                continue;
            }
            if (ent->ifa_addr->sa_family == AF_INET) {
                uint32_t a = ntohl(((struct sockaddr_in *) ent->ifa_addr)->sin_addr.s_addr);
                uint32_t m = ent->ifa_netmask
                    ? ntohl(((struct sockaddr_in *) ent->ifa_netmask)->sin_addr.s_addr)
                    : 0;
                term nm = term_alloc_tuple(2, heap);
                term_put_tuple_element(nm, 0, netmask_atom);
                term_put_tuple_element(nm, 1, inet_make_addr4(m, heap));
                opts = term_list_prepend(nm, opts, heap);
                term ad = term_alloc_tuple(2, heap);
                term_put_tuple_element(ad, 0, addr_atom);
                term_put_tuple_element(ad, 1, inet_make_addr4(a, heap));
                opts = term_list_prepend(ad, opts, heap);
            } else if (ent->ifa_addr->sa_family == AF_INET6) {
                const uint8_t *a = ((struct sockaddr_in6 *) ent->ifa_addr)->sin6_addr.s6_addr;
                term nm = term_alloc_tuple(2, heap);
                term_put_tuple_element(nm, 0, netmask_atom);
                if (ent->ifa_netmask) {
                    const uint8_t *m = ((struct sockaddr_in6 *) ent->ifa_netmask)->sin6_addr.s6_addr;
                    term_put_tuple_element(nm, 1, make_addr6(m, heap));
                } else {
                    term_put_tuple_element(nm, 1, make_addr6(a, heap));
                }
                opts = term_list_prepend(nm, opts, heap);
                term ad = term_alloc_tuple(2, heap);
                term_put_tuple_element(ad, 0, addr_atom);
                term_put_tuple_element(ad, 1, make_addr6(a, heap));
                opts = term_list_prepend(ad, opts, heap);
            }
        }

        // Prepend {flags, [...]} so it heads the option list.
        term flags_tuple = term_alloc_tuple(2, heap);
        term_put_tuple_element(flags_tuple, 0, flags_atom);
        term_put_tuple_element(flags_tuple, 1, make_iface_flags(cur->ifa_flags, global, heap));
        opts = term_list_prepend(flags_tuple, opts, heap);

        term iface = term_alloc_tuple(2, heap);
        term_put_tuple_element(iface, 0, interop_bytes_to_list(name, namelen, heap));
        term_put_tuple_element(iface, 1, opts);
        result_list = term_list_prepend(iface, result_list, heap);

        cur = end;
    }

    freeifaddrs(ifap);

    term result = term_alloc_tuple(2, heap);
    term_put_tuple_element(result, 0, OK_ATOM);
    term_put_tuple_element(result, 1, result_list);
    return result;
}
#endif

//
// Nifs
//

static const struct Nif net_getaddrinfo_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_net_getaddrinfo
};
#ifdef HAVE_GETHOSTNAME
static const struct Nif net_gethostname_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_net_gethostname
};
#endif
#ifdef HAVE_GETIFADDRS
static const struct Nif net_getifaddrs_nif = {
    .base.type = NIFFunctionType,
    .nif_ptr = nif_net_getifaddrs
};
#endif

//
// Entrypoints
//

const struct Nif *otp_net_nif_get_nif(const char *nifname)
{
    if (strncmp("net:", nifname, 4) == 0) {
        const char *rest = nifname + 4;
        if (strcmp("getaddrinfo_nif/2", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &net_getaddrinfo_nif;
        }
#ifdef HAVE_GETHOSTNAME
        if (strcmp("gethostname/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &net_gethostname_nif;
        }
#endif
#ifdef HAVE_GETIFADDRS
        if (strcmp("getifaddrs/0", rest) == 0) {
            TRACE("Resolved platform nif %s ...\n", nifname);
            return &net_getifaddrs_nif;
        }
#endif
    }
    return NULL;
}

void otp_net_init(GlobalContext *global)
{
    UNUSED(global);

    // noop
}
