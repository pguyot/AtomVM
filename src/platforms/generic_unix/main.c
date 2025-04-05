/*
 * This file is part of AtomVM.
 *
 * Copyright 2017-2023 Davide Bettio <davide@uninstall.it>
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

#include <libgen.h>
#include <signal.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "atom.h"
#include "avm_version.h"
#include "avmpack.h"
#include "bif.h"
#include "context.h"
#include "defaultatoms.h"
#include "globalcontext.h"
#include "iff.h"
#include "mapped_file.h"
#include "module.h"
#include "synclist.h"
#include "sys.h"
#include "term.h"
#include "utils.h"

void print_help(const char *program_name)
{
    printf(
        "\n"
        "Syntax:\n"
        "\n"
        "    %s [-h] [-v] <path-to-avm-or-beam-file>+\n"
        "\n"
        "Options:\n"
        "\n"
        "    -h         Print this help and exit.\n"
        "    -v         Print the AtomVM version and exit.\n"
        "\n"
        "Supply one or more AtomVM packbeam (.avm) files to start your application.\n"
        "\n"
        "Example:\n"
        "\n"
        "    $ %s /path/to/my/application.avm /path/to/atomvmlib.avm\n"
        "\n",
        program_name, program_name);
}

static GlobalContext *glb;
static struct sigaction sa_previous;

void sigterm_handler(int signal)
{
    fprintf(stderr, "\n======================================================\n");
    fprintf(stderr, "Signal %d received, dumping core\n", signal);
    struct ListHead *item;
    fprintf(stderr, "======================================================\n");
    fprintf(stderr, "Reference counted binaries\n");
    struct ListHead *refc_binaries = synclist_nolock(&glb->refc_binaries);
    LIST_FOR_EACH (item, refc_binaries) {
        struct RefcBinary *refc = GET_LIST_ENTRY(item, struct RefcBinary, head);
        if (refc->resource_type) {
            fprintf(stderr, "Resource of type %s, ref_count = %d, data = %p\n", refc->resource_type->name, (int) refc->ref_count, refc->data);
        } else {
            fprintf(stderr, "Refc binary, ref_count = %d, size = %zu, data = %p\n", (int) refc->ref_count, refc->size, refc->data);
        }
    }
    fprintf(stderr, "======================================================\n");
    fprintf(stderr, "Processes\n");
    struct ListHead *processes_table = synclist_nolock(&glb->processes_table);
    fprintf(stderr, "------------------------------------------------------\n");
    LIST_FOR_EACH (item, processes_table) {
        Context *p = GET_LIST_ENTRY(item, Context, processes_table_head);
        context_dump(p);
        fprintf(stderr, "------------------------------------------------------\n");
    }
    fprintf(stderr, "======================================================\n");
    sigaction(signal, &sa_previous, NULL);
    raise(signal);
}

int main(int argc, char **argv)
{
    int c;
    while ((c = getopt(argc, argv, "hv")) != -1) {
        switch (c) {
            case 'h':
                print_help(argv[0]);
                return EXIT_SUCCESS;

            case 'v':
                printf(ATOMVM_VERSION "\n");
                return EXIT_SUCCESS;

            default:
                break;
        }
    }

    if (argc < 2) {
        printf("Syntax Error!  Missing .beam or .avm files.\n");
        print_help(argv[0]);
        return EXIT_FAILURE;
    }

    glb = globalcontext_new();
    struct sigaction sa;
    bzero(&sa, sizeof(sa));
    sa.sa_handler = sigterm_handler;
    sigaction(SIGTERM, &sa, &sa_previous);

    Module *startup_module = NULL;

    for (int i = 1; i < argc; ++i) {
        const char *ext = strrchr(argv[i], '.');
        if (ext && strcmp(ext, ".avm") == 0) {
            struct AVMPackData *avmpack_data;
            if (UNLIKELY(sys_open_avm_from_file(glb, argv[i], &avmpack_data) != AVM_OPEN_OK)) {
                fprintf(stderr, "Failed opening %s.\n", argv[i]);
                return EXIT_FAILURE;
            }
            synclist_append(&glb->avmpack_data, &avmpack_data->avmpack_head);

            if (IS_NULL_PTR(startup_module)) {
                const void *startup_beam = NULL;
                const char *startup_module_name;
                uint32_t startup_beam_size;
                avmpack_find_section_by_flag(avmpack_data->data, 1, &startup_beam, &startup_beam_size, &startup_module_name);

                if (startup_beam) {
                    avmpack_data->in_use = true;
                    startup_module = module_new_from_iff_binary(glb, startup_beam, startup_beam_size);
                    if (IS_NULL_PTR(startup_module)) {
                        fprintf(stderr, "Cannot load startup module: %s\n", startup_module_name);
                        return EXIT_FAILURE;
                    }
                    globalcontext_insert_module(glb, startup_module);
                    startup_module->module_platform_data = NULL;
                }
            }

        } else if (ext && (strcmp(ext, ".beam") == 0)) {
            MappedFile *mapped_file = mapped_file_open_beam(argv[i]);
            if (!iff_is_valid_beam(mapped_file->mapped)) {
                fprintf(stderr, "%s has invalid beam format.\n", argv[i]);
                return EXIT_FAILURE;
            }
            Module *mod = module_new_from_iff_binary(glb, mapped_file->mapped, mapped_file->size);
            if (IS_NULL_PTR(mod)) {
                fprintf(stderr, "Cannot load module: %s\n", argv[i]);
                return EXIT_FAILURE;
            }
            globalcontext_insert_module(glb, mod);
            mod->module_platform_data = NULL;
            if (IS_NULL_PTR(startup_module) && module_search_exported_function(mod, ATOM_STR("\5", "start"), 0, glb) != 0) {
                startup_module = mod;
            }

        } else {
            fprintf(stderr, "%s is not an AVM or a BEAM file.\n", argv[i]);
            return EXIT_FAILURE;
        }
    }

    if (IS_NULL_PTR(startup_module)) {
        fprintf(stderr, "Unable to locate entrypoint.\n");
        return EXIT_FAILURE;
    }

    Context *ctx = context_new(glb);
    ctx->leader = 1;

    context_execute_loop(ctx, startup_module, "start", 0);

    term ret_value = ctx->x[0];
    fprintf(stderr, "Return value: ");
    term_display(stderr, ret_value, ctx);
    fprintf(stderr, "\n");

    int status;
    if (ret_value == OK_ATOM) {
        status = EXIT_SUCCESS;
    } else {
        status = EXIT_FAILURE;
    }

    context_destroy(ctx);

    sigaction(SIGTERM, &sa_previous, NULL);
    globalcontext_destroy(glb);

    return status;
}
