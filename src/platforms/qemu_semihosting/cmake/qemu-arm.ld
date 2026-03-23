/*
 * Linker script for QEMU MPS2-AN385 (Cortex-M3) with picolibc semihosting
 * 4MB Flash at 0x00000000, 4MB RAM at 0x20000000
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

MEMORY
{
    flash (rx)  : ORIGIN = 0x00000000, LENGTH = 4M
    ram (rwx)   : ORIGIN = 0x20000000, LENGTH = 4M
}

ENTRY(_start)

/*
 * Picolibc CRT expects these symbols for stack and heap setup.
 */

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
        *(.glue_7)
        *(.glue_7t)
        *(.eh_frame)
        KEEP(*(.fini))
        . = ALIGN(4);
        __etext = .;
    } >flash

    .rodata :
    {
        . = ALIGN(4);
        *(.rodata)
        *(.rodata*)
        *(.srodata)
        *(.srodata*)
        . = ALIGN(4);
    } >flash

    .ARM.extab :
    {
        *(.ARM.extab* .gnu.linkonce.armextab.*)
    } >flash

    .ARM :
    {
        __exidx_start = .;
        *(.ARM.exidx*)
        __exidx_end = .;
    } >flash

    .preinit_array :
    {
        PROVIDE_HIDDEN(__preinit_array_start = .);
        KEEP(*(.preinit_array*))
        PROVIDE_HIDDEN(__preinit_array_end = .);
    } >flash

    .init_array :
    {
        PROVIDE_HIDDEN(__init_array_start = .);
        KEEP(*(SORT(.init_array.*)))
        KEEP(*(.init_array*))
        PROVIDE_HIDDEN(__init_array_end = .);
    } >flash

    .fini_array :
    {
        PROVIDE_HIDDEN(__fini_array_start = .);
        KEEP(*(SORT(.fini_array.*)))
        KEEP(*(.fini_array*))
        PROVIDE_HIDDEN(__fini_array_end = .);
    } >flash

    __data_source = LOADADDR(.data);

    .data :
    {
        . = ALIGN(4);
        __data_start = .;
        *(.data)
        *(.data*)
        . = ALIGN(4);
        __edata = .;
    } >ram AT> flash

    .bss :
    {
        . = ALIGN(4);
        __bss_start = .;
        *(.bss)
        *(.bss*)
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
    } >ram AT> flash

    .tbss :
    {
        . = ALIGN(4);
        __tbss_start = .;
        *(.tbss .tbss.*)
        __tbss_end = .;
        __tls_end = .;
    } >ram

    __tdata_source = LOADADDR(.tdata);
    __tdata_size = __tdata_end - __tdata_start;
    __tbss_size = __tbss_end - __tbss_start;
    __tls_size = __tls_end - __tls_base;
    __tls_align = MAX(ALIGNOF(.tdata), ALIGNOF(.tbss));

    . = ALIGN(8);
    __heap_start = .;
    __heap_end = ORIGIN(ram) + LENGTH(ram) - 4096;
    __stack = ORIGIN(ram) + LENGTH(ram);

    .ARM.attributes 0 : { *(.ARM.attributes) }
}
