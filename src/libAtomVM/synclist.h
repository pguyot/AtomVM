/*
 * This file is part of AtomVM.
 *
 * Copyright 2022 Paul Guyot <pguyot@kallisys.net>
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

#ifndef _SYNCLIST_H_
#define _SYNCLIST_H_

#include <stdio.h>

#include "list.h"

#ifndef AVM_NO_SMP

#include "smp.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifndef TYPEDEF_RWLOCK
#define TYPEDEF_RWLOCK
typedef struct RWLock RWLock;
#endif

#ifdef SMP_ATOMIC_RWLOCK

struct SyncList
{
    struct AtomicRWLock lock;
    struct ListHead head;
};

// The lock operations use C11 atomics and are C-only; C++ translation
// units (NIF components) only get the struct layout.
#ifndef __cplusplus

static inline void synclist_init(struct SyncList *synclist)
{
    smp_atomic_rwlock_init(&synclist->lock);
    list_init(&synclist->head);
}

static inline void synclist_destroy(struct SyncList *synclist)
{
    (void) synclist;
}

static inline struct ListHead *synclist_rdlock(struct SyncList *synclist)
{
    smp_atomic_rwlock_rdlock(&synclist->lock);
    return &synclist->head;
}

static inline struct ListHead *synclist_tryrdlock(struct SyncList *synclist)
{
    if (smp_atomic_rwlock_tryrdlock(&synclist->lock)) {
        return &synclist->head;
    }
    return NULL;
}

static inline struct ListHead *synclist_wrlock(struct SyncList *synclist)
{
    smp_atomic_rwlock_wrlock(&synclist->lock);
    return &synclist->head;
}

static inline struct ListHead *synclist_nolock(struct SyncList *synclist)
{
    return &synclist->head;
}

static inline void synclist_unlock(struct SyncList *synclist)
{
    smp_atomic_rwlock_unlock(&synclist->lock);
}

#endif // !defined(__cplusplus)

#else

struct SyncList
{
    RWLock *lock;
    struct ListHead head;
};

static inline void synclist_init(struct SyncList *synclist)
{
    synclist->lock = smp_rwlock_create();
    list_init(&synclist->head);
}

static inline void synclist_destroy(struct SyncList *synclist)
{
    smp_rwlock_destroy(synclist->lock);
}

static inline struct ListHead *synclist_rdlock(struct SyncList *synclist)
{
    smp_rwlock_rdlock(synclist->lock);
    return &synclist->head;
}

static inline struct ListHead *synclist_tryrdlock(struct SyncList *synclist)
{
    if (smp_rwlock_tryrdlock(synclist->lock)) {
        return &synclist->head;
    }
    return NULL;
}

static inline struct ListHead *synclist_wrlock(struct SyncList *synclist)
{
    smp_rwlock_wrlock(synclist->lock);
    return &synclist->head;
}

static inline struct ListHead *synclist_nolock(struct SyncList *synclist)
{
    return &synclist->head;
}

static inline void synclist_unlock(struct SyncList *synclist)
{
    smp_rwlock_unlock(synclist->lock);
}

#endif

// These call the lock operations, which are C-only in the atomic-rwlock
// configuration (see above).
#if !(defined(SMP_ATOMIC_RWLOCK) && defined(__cplusplus))

static inline void synclist_prepend(struct SyncList *synclist, struct ListHead *new_item)
{
    struct ListHead *head = synclist_wrlock(synclist);
    list_prepend(head, new_item);
    synclist_unlock(synclist);
}

static inline void synclist_append(struct SyncList *synclist, struct ListHead *new_item)
{
    struct ListHead *head = synclist_wrlock(synclist);
    list_insert(new_item, head->prev, head);
    synclist_unlock(synclist);
}

static inline void synclist_remove(struct SyncList *synclist, struct ListHead *item)
{
    synclist_wrlock(synclist);
    list_remove(item);
    synclist_unlock(synclist);
}

static inline bool synclist_is_empty(struct SyncList *synclist)
{
    bool result;
    struct ListHead *head = synclist_rdlock(synclist);
    result = (head->next == head) && (head->prev == head);
    synclist_unlock(synclist);
    return result;
}

#endif // !(SMP_ATOMIC_RWLOCK && __cplusplus)

#ifdef __cplusplus
}
#endif

#else

#define SyncList ListHead
#define synclist_init(list) list_init(list)
#define synclist_rdlock(list) list
#define synclist_tryrdlock(list) list
#define synclist_wrlock(list) list
#define synclist_nolock(list) list
#define synclist_unlock(list) UNUSED(list)
#define synclist_destroy(list) UNUSED(list)
#define synclist_prepend(list, new_item) list_prepend(list, new_item)
#define synclist_append(list, new_item) list_append(list, new_item)
#define synclist_remove(list, new_item) list_remove(new_item)
#define synclist_is_empty(list) list_is_empty(list)

#endif

#endif
