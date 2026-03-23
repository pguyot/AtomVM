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

/**
 * @file mapped_file.c
 * @brief read()-based file loader for semihosting (no mmap available).
 */

#include "mapped_file.h"

#include "utils.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

MappedFile *mapped_file_open_beam(const char *file_name)
{
    MappedFile *mf = malloc(sizeof(MappedFile));
    if (IS_NULL_PTR(mf)) {
        fprintf(stderr, "Unable to allocate MappedFile struct\n");
        return NULL;
    }

    mf->fd = open(file_name, O_RDONLY);
    if (UNLIKELY(mf->fd < 0)) {
        free(mf);
        fprintf(stderr, "Unable to open %s (errno=%d)\n", file_name, errno);
        return NULL;
    }

    // Use lseek to determine file size (more portable with semihosting than fstat)
    off_t end = lseek(mf->fd, 0, SEEK_END);
    if (end < 0) {
        fprintf(stderr, "Cannot determine size of %s (errno=%d)\n", file_name, errno);
        close(mf->fd);
        free(mf);
        return NULL;
    }
    mf->size = (unsigned long) end;
    lseek(mf->fd, 0, SEEK_SET);

    mf->mapped = malloc(mf->size);
    if (IS_NULL_PTR(mf->mapped)) {
        fprintf(stderr, "Cannot allocate %lu bytes for %s\n", mf->size, file_name);
        close(mf->fd);
        free(mf);
        return NULL;
    }

    unsigned long total_read = 0;
    while (total_read < mf->size) {
        ssize_t n = read(mf->fd, (char *) mf->mapped + total_read, mf->size - total_read);
        if (n <= 0) {
            fprintf(stderr, "Cannot read %s (errno=%d)\n", file_name, errno);
            free(mf->mapped);
            close(mf->fd);
            free(mf);
            return NULL;
        }
        total_read += (unsigned long) n;
    }

    close(mf->fd);
    mf->fd = -1;

    return mf;
}

void mapped_file_close(MappedFile *mf)
{
    free(mf->mapped);
    free(mf);
}
