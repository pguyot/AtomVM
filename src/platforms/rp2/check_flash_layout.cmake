#
# This file is part of AtomVM.
#
# Copyright 2026 Paul Guyot <pguyot@kallisys.net>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

# Post-build check: the firmware image must end below LIMIT, the fixed flash
# offset where .avm packs are flashed (LIB_AVM in src/main.c, the test
# modules for rp2_tests). An image crossing that boundary is silently
# corrupted when the .avm uf2 is flashed over its tail, and the device then
# takes a wild jump at boot.
#
# Expected arguments: -DNM=<nm> -DELF=<image.elf> -DLIMIT=<address>

execute_process(
    COMMAND "${NM}" --defined-only "${ELF}"
    OUTPUT_VARIABLE _symbols
    RESULT_VARIABLE _result
)
if (NOT _result EQUAL 0)
    message(FATAL_ERROR "check_flash_layout: ${NM} failed on ${ELF}")
endif()

string(REGEX MATCH "([0-9a-fA-F]+)[ \t]+[A-Za-z][ \t]+__flash_binary_end" _match "${_symbols}")
if (NOT _match)
    message(FATAL_ERROR "check_flash_layout: __flash_binary_end not found in ${ELF}")
endif()

math(EXPR _end "0x${CMAKE_MATCH_1}")
math(EXPR _limit "${LIMIT}")
if (_end GREATER _limit)
    math(EXPR _overflow "${_end} - ${_limit}")
    math(EXPR _end_hex "${_end}" OUTPUT_FORMAT HEXADECIMAL)
    message(FATAL_ERROR
        "${ELF}: flash image ends at ${_end_hex}, ${_overflow} bytes past "
        "${LIMIT} where .avm packs are flashed. Flashing an .avm would "
        "corrupt the firmware. Reduce the image size or move the .avm area.")
endif()
