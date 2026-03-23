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

cmake_minimum_required(VERSION 3.13)
project(AtomVM-QEMU-Semihosting C ASM)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../../../CMakeModules")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Options
option(AVM_DISABLE_JIT "Disable just in time compilation." ON)
option(AVM_USE_32BIT_FLOAT "Use 32 bit floats." OFF)
option(AVM_VERBOSE_ABORT "Print module and line number on VM abort" ON)
option(AVM_CREATE_STACKTRACES "Create stacktraces" ON)
option(AVM_ENABLE_OLD_OTP_VERSIONS "Enable OTP version < 26" OFF)
option(AVM_PRINT_PROCESS_CRASH_DUMPS "Print crash reports when processes die with non-standard reasons" ON)

# No SMP or task drivers on bare metal
set(AVM_DISABLE_SMP ON FORCE)
set(AVM_DISABLE_TASK_DRIVER ON FORCE)

# Include an error if the user forgets to specify a toolchain
if (NOT CMAKE_TOOLCHAIN_FILE)
    message(FATAL_ERROR "Cross compiling only. Please use -DCMAKE_TOOLCHAIN_FILE=cmake/arm-toolchain.cmake or cmake/riscv32-toolchain.cmake")
endif ()

# JIT configuration
if (NOT AVM_DISABLE_JIT)
    if (CMAKE_SYSTEM_PROCESSOR STREQUAL "cortex-m3")
        set(AVM_JIT_TARGET_ARCH "armv6m")
    elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "riscv32")
        set(AVM_JIT_TARGET_ARCH "riscv32")
    else()
        message(FATAL_ERROR "JIT not supported for ${CMAKE_SYSTEM_PROCESSOR}")
    endif()
endif()

# Bare-metal: force-disable POSIX functions that picolibc semihosting doesn't support
set(HAVE_MKFIFO "" CACHE INTERNAL "" FORCE)
set(HAVE_UNLINK "" CACHE INTERNAL "" FORCE)
set(HAVE_EXECVE "" CACHE INTERNAL "" FORCE)
set(HAVE_OPENDIR "" CACHE INTERNAL "" FORCE)
set(HAVE_CLOSEDIR "" CACHE INTERNAL "" FORCE)
set(HAVE_READDIR "" CACHE INTERNAL "" FORCE)
set(HAVE_CLOSEFROM "" CACHE INTERNAL "" FORCE)
set(HAVE_GETCWD "" CACHE INTERNAL "" FORCE)
set(HAVE_CLOCK_SETTIME "" CACHE INTERNAL "" FORCE)
set(HAVE_SETTIMEOFDAY "" CACHE INTERNAL "" FORCE)
set(HAVE_SOCKET "" CACHE INTERNAL "" FORCE)
set(HAVE_SELECT "" CACHE INTERNAL "" FORCE)

# Semihosting provides open/close/read/write via the host OS
set(HAVE_OPEN 1 CACHE INTERNAL "" FORCE)
set(HAVE_CLOSE 1 CACHE INTERNAL "" FORCE)

# Build picolibc with semihosting
include(cmake/picolibc.cmake)

# Use picolibc headers instead of toolchain newlib headers
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -isystem ${PICOLIBC_INCLUDE_DIR}")

# Architecture-specific settings
if (CMAKE_SYSTEM_PROCESSOR STREQUAL "cortex-m3")
    set(ARCH_FLAGS "-mcpu=cortex-m3 -mthumb")
    set(LINKER_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/cmake/qemu-arm.ld")
    set(QEMU_MACHINE "mps2-an385")
    set(QEMU_BINARY "qemu-system-arm")
elseif (CMAKE_SYSTEM_PROCESSOR STREQUAL "riscv32")
    set(ARCH_FLAGS "-march=rv32imac -mabi=ilp32")
    set(LINKER_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/cmake/qemu-riscv32.ld")
    set(QEMU_MACHINE "virt")
    set(QEMU_BINARY "qemu-system-riscv32")
else()
    message(FATAL_ERROR "Unsupported processor: ${CMAKE_SYSTEM_PROCESSOR}")
endif()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ARCH_FLAGS} -O2 -ffunction-sections -fdata-sections")
set(LINKER_FLAGS "-nostdlib -nostartfiles -Wl,--gc-sections -T${LINKER_SCRIPT} ${ARCH_FLAGS}")

# For CMake try_compile (e.g. check_c_source_compiles in libAtomVM), use
# nosys.specs so the toolchain's default libc can link test programs.
# We keep CMAKE_TRY_COMPILE_TARGET_TYPE as STATIC_LIBRARY from the toolchain
# to avoid unnecessary link tests, but set CMAKE_REQUIRED_FLAGS for any that
# do need to link.
set(CMAKE_REQUIRED_FLAGS "${LINKER_FLAGS}")

# Find host Erlang tooling (needed for compiling .erl test files)
find_package(Erlang REQUIRED)

# Build libAtomVM
add_subdirectory(../../libAtomVM libAtomVM)
add_dependencies(libAtomVM picolibc)

# When JIT is enabled, libAtomVM defines AVM_NO_EMU (PUBLIC) which disables
# the bytecode emulator, expecting all code to be JIT-compiled or precompiled.
# For QEMU semihosting we load plain beams and rely on the emulator as
# fallback, so remove AVM_NO_EMU from both the library and its interface.
if (NOT AVM_DISABLE_JIT)
    get_target_property(_defs libAtomVM COMPILE_DEFINITIONS)
    if (_defs)
        list(REMOVE_ITEM _defs "AVM_NO_EMU")
        set_target_properties(libAtomVM PROPERTIES COMPILE_DEFINITIONS "${_defs}")
    endif()
    get_target_property(_idefs libAtomVM INTERFACE_COMPILE_DEFINITIONS)
    if (_idefs)
        list(REMOVE_ITEM _idefs "AVM_NO_EMU")
        set_target_properties(libAtomVM PROPERTIES INTERFACE_COMPILE_DEFINITIONS "${_idefs}")
    endif()
endif()

# Platform library
set(PLATFORM_LIB_SUFFIX ${CMAKE_SYSTEM_NAME}-${CMAKE_SYSTEM_PROCESSOR})

set(PLATFORM_SOURCE_FILES
    sys.c
    platform_nifs.c
    mapped_file.c
    jit_stream_stub.c
    ../../libAtomVM/portnifloader.c
)

if (CMAKE_SYSTEM_PROCESSOR STREQUAL "cortex-m3")
    list(APPEND PLATFORM_SOURCE_FILES startup_arm.S)
endif()

add_library(libAtomVM${PLATFORM_LIB_SUFFIX} ${PLATFORM_SOURCE_FILES})
target_compile_features(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC c_std_11)
target_link_libraries(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC libAtomVM)
target_include_directories(libAtomVM${PLATFORM_LIB_SUFFIX} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
add_dependencies(libAtomVM${PLATFORM_LIB_SUFFIX} picolibc)

# Build test-erlang executable
add_executable(test-erlang.elf ../../../tests/test.c)
target_compile_features(test-erlang.elf PUBLIC c_std_11)
target_include_directories(test-erlang.elf PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ../../libAtomVM
)
target_link_libraries(test-erlang.elf PRIVATE libAtomVM libAtomVM${PLATFORM_LIB_SUFFIX})
add_dependencies(test-erlang.elf picolibc)

# Semihosting-specific defines: use compile-time path to beam files
# Beams are always loaded from the base erlang_tests directory;
# JIT compilation happens at runtime, not via precompiled beams.
target_compile_definitions(test-erlang.elf PRIVATE
    QEMU_SEMIHOSTING
    QEMU_SEMIHOSTING_BEAM_DIR="${CMAKE_BINARY_DIR}/erlang_tests"
)

# Link picolibc semihosting CRT, libc, libsemihost, and libgcc
target_link_libraries(test-erlang.elf PRIVATE
    ${PICOLIBC_LIB_DIR}/crt0-semihost.o
    ${PICOLIBC_LIB_DIR}/libc.a
    ${PICOLIBC_LIB_DIR}/libsemihost.a
    gcc
    ${PICOLIBC_LIB_DIR}/libc.a
)

set_target_properties(test-erlang.elf PROPERTIES
    LINK_FLAGS "${LINKER_FLAGS}"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/tests"
)

# Build PackBEAM tool (escript - runs on host) and erlang test modules.
# JIT precompilation is skipped: beams are JIT-compiled at runtime.
# We temporarily force AVM_DISABLE_JIT so erlang_tests only produces plain
# beams (no JIT precompilation which would require the jit library).
# PackBEAM binary dir must be 'tools/packbeam' to match paths expected by
# erlang_tests/code_load/CMakeLists.txt (${CMAKE_BINARY_DIR}/tools/packbeam/packbeam).
set(_saved_AVM_DISABLE_JIT ${AVM_DISABLE_JIT})
set(AVM_DISABLE_JIT ON)
if (NOT "${CMAKE_GENERATOR}" MATCHES "Xcode")
    # PackBEAM's project() enables CXX by default, but it only uses rebar3
    # (no C/CXX compilation). Skip the CXX compiler check which would fail
    # with the bare-metal cross-compiler.
    set(CMAKE_CXX_COMPILER_WORKS TRUE)
    add_subdirectory(../../../tools/packbeam tools/packbeam)
    add_subdirectory(../../../tests/erlang_tests erlang_tests)
    add_dependencies(test-erlang.elf erlang_test_modules)
endif()
set(AVM_DISABLE_JIT ${_saved_AVM_DISABLE_JIT})

# Generate a helper script to run tests under QEMU
file(WRITE "${CMAKE_BINARY_DIR}/run-qemu-tests.sh" "\
#!/bin/sh
# Run test-erlang under QEMU with semihosting
set -e
cd \"$(dirname \"$0\")\"
${QEMU_BINARY} \\
    -machine ${QEMU_MACHINE} \\
    -nographic \\
    -semihosting-config enable=on,target=native \\
    -kernel tests/test-erlang.elf \\
    \"$@\"
")
