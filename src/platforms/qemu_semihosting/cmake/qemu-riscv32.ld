/*
 * Linker script for QEMU riscv32 virt machine
 * RAM at 0x80000000 (128MB)
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

MEMORY
{
    RAM (rwx) : ORIGIN = 0x80000000, LENGTH = 128M
}

ENTRY(_start)

_estack = ORIGIN(RAM) + LENGTH(RAM);
_stack = _estack;

SECTIONS
{
    .text :
    {
        . = ALIGN(4);
        *(.text.init)
        *(.text)
        *(.text*)
        . = ALIGN(4);
        _etext = .;
    } >RAM

    .rodata :
    {
        . = ALIGN(4);
        *(.rodata)
        *(.rodata*)
        *(.srodata)
        *(.srodata*)
        . = ALIGN(4);
    } >RAM

    .preinit_array :
    {
        PROVIDE_HIDDEN(__preinit_array_start = .);
        PROVIDE_HIDDEN(__bothinit_array_start = .);
        KEEP(*(.preinit_array*))
        PROVIDE_HIDDEN(__preinit_array_end = .);
    } >RAM

    .init_array :
    {
        PROVIDE_HIDDEN(__init_array_start = .);
        KEEP(*(SORT(.init_array.*)))
        KEEP(*(.init_array*))
        PROVIDE_HIDDEN(__init_array_end = .);
        PROVIDE_HIDDEN(__bothinit_array_end = .);
    } >RAM

    .fini_array :
    {
        PROVIDE_HIDDEN(__fini_array_start = .);
        KEEP(*(SORT(.fini_array.*)))
        KEEP(*(.fini_array*))
        PROVIDE_HIDDEN(__fini_array_end = .);
    } >RAM

    .data :
    {
        . = ALIGN(4);
        _sdata = .;
        __global_pointer$ = . + 0x800;
        *(.data)
        *(.data*)
        *(.sdata)
        *(.sdata*)
        . = ALIGN(4);
        _edata = .;
    } >RAM

    .bss :
    {
        . = ALIGN(4);
        _sbss = .;
        __bss_start__ = _sbss;
        *(.bss)
        *(.bss*)
        *(.sbss)
        *(.sbss*)
        *(COMMON)
        . = ALIGN(4);
        _ebss = .;
        __bss_end__ = _ebss;
    } >RAM

    . = ALIGN(8);
    PROVIDE(end = .);
    PROVIDE(_end = .);

    .riscv.attributes 0 : { *(.riscv.attributes) }
}
