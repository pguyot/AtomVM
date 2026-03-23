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

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR riscv32)

find_program(RISCV_CC riscv-none-elf-gcc NAMES riscv-none-elf-gcc riscv32-unknown-elf-gcc riscv64-unknown-elf-gcc REQUIRED)
find_program(RISCV_CXX riscv-none-elf-g++ NAMES riscv-none-elf-g++ riscv32-unknown-elf-g++ riscv64-unknown-elf-g++ REQUIRED)
find_program(RISCV_AR riscv-none-elf-ar NAMES riscv-none-elf-ar riscv32-unknown-elf-ar riscv64-unknown-elf-ar)
find_program(RISCV_AS riscv-none-elf-as NAMES riscv-none-elf-as riscv32-unknown-elf-as riscv64-unknown-elf-as)
find_program(RISCV_NM riscv-none-elf-nm NAMES riscv-none-elf-nm riscv32-unknown-elf-nm riscv64-unknown-elf-nm)
find_program(RISCV_STRIP riscv-none-elf-strip NAMES riscv-none-elf-strip riscv32-unknown-elf-strip riscv64-unknown-elf-strip)

mark_as_advanced(RISCV_CC RISCV_CXX RISCV_AR RISCV_AS RISCV_NM RISCV_STRIP)

set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
set(CMAKE_C_COMPILER ${RISCV_CC})
set(CMAKE_CXX_COMPILER ${RISCV_CXX})
set(CMAKE_ASM_COMPILER ${RISCV_CC})
