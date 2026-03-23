/*
 * Linker script for QEMU riscv32 virt machine with picolibc semihosting
 * RAM at 0x80000000 (128MB)
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

MEMORY
{
    ram (rwx) : ORIGIN = 0x80000000, LENGTH = 128M
}

ENTRY(_start)

SECTIONS
{
    .text :
    {
        . = ALIGN(4);
        KEEP(*(.text.init.enter))
        KEEP(*(.data.init.enter))
        KEEP(*(.init))
        *(.text)
        *(.text*)
        . = ALIGN(4);
        __etext = .;
    } >ram

    .rodata :
    {
        . = ALIGN(4);
        *(.rodata)
        *(.rodata*)
        *(.srodata)
        *(.srodata*)
        . = ALIGN(4);
    } >ram

    .preinit_array :
    {
        PROVIDE_HIDDEN(__preinit_array_start = .);
        KEEP(*(.preinit_array*))
        PROVIDE_HIDDEN(__preinit_array_end = .);
    } >ram

    .init_array :
    {
        PROVIDE_HIDDEN(__init_array_start = .);
        KEEP(*(SORT(.init_array.*)))
        KEEP(*(.init_array*))
        PROVIDE_HIDDEN(__init_array_end = .);
    } >ram

    .fini_array :
    {
        PROVIDE_HIDDEN(__fini_array_start = .);
        KEEP(*(SORT(.fini_array.*)))
        KEEP(*(.fini_array*))
        PROVIDE_HIDDEN(__fini_array_end = .);
    } >ram

    __data_source = LOADADDR(.data);

    .data :
    {
        . = ALIGN(4);
        __data_start = .;
        __global_pointer$ = . + 0x800;
        *(.data)
        *(.data*)
        *(.sdata)
        *(.sdata*)
        . = ALIGN(4);
        __edata = .;
    } >ram

    .bss :
    {
        . = ALIGN(4);
        __bss_start = .;
        *(.bss)
        *(.bss*)
        *(.sbss)
        *(.sbss*)
        *(COMMON)
        . = ALIGN(4);
        __bss_end = .;
    } >ram

    /* TLS sections (picolibc expects these even if unused) */
    .tdata :
    {
        . = ALIGN(4);
        __tdata_start = .;
        __tls_base = .;
        *(.tdata .tdata.*)
        __tdata_end = .;
    } >ram

    .tbss :
    {
        . = ALIGN(4);
        __tbss_start = .;
        *(.tbss .tbss.*)
        __tbss_end = .;
        __tls_end = .;
    } >ram

    /* Size symbols required by picolibc CRT for data copy and BSS zeroing */
    __data_size = __edata - __data_start;
    __bss_size = __bss_end - __bss_start;

    __tdata_source = LOADADDR(.tdata);
    __tdata_size = __tdata_end - __tdata_start;
    __tbss_size = __tbss_end - __tbss_start;
    __tls_size = __tls_end - __tls_base;
    __tls_align = MAX(ALIGNOF(.tdata), ALIGNOF(.tbss));

    . = ALIGN(8);
    PROVIDE(end = .);
    PROVIDE(_end = .);
    __heap_start = .;
    __heap_end = ORIGIN(ram) + LENGTH(ram) - 8192;
    __stack = ORIGIN(ram) + LENGTH(ram);

    .riscv.attributes 0 : { *(.riscv.attributes) }
}
