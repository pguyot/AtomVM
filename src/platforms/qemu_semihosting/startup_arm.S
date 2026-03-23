/*
 * Minimal vector table for QEMU MPS2-AN385 (Cortex-M3) with picolibc
 *
 * SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
 */

    .syntax unified
    .cpu cortex-m3
    .thumb

    .section .isr_vector, "a", %progbits
    .align 2
    .globl __isr_vector
__isr_vector:
    .word __stack           /* Initial SP */
    .word _start            /* Reset handler - picolibc CRT entry */
    .word _fault_handler    /* NMI */
    .word _fault_handler    /* HardFault */
    .word _fault_handler    /* MemManage */
    .word _fault_handler    /* BusFault */
    .word _fault_handler    /* UsageFault */
    .word 0                 /* Reserved */
    .word 0                 /* Reserved */
    .word 0                 /* Reserved */
    .word 0                 /* Reserved */
    .word _fault_handler    /* SVCall */
    .word _fault_handler    /* DebugMonitor */
    .word 0                 /* Reserved */
    .word _fault_handler    /* PendSV */
    .word _fault_handler    /* SysTick */

    .section .text
    .thumb_func
    .globl _fault_handler
    .type _fault_handler, %function
_fault_handler:
    /* Print fault info via semihosting then exit */
    mrs r0, psp
    mrs r1, msp
    mrs r2, xpsr
    /* Semihosting SYS_REPORTEXC (0x18) with ADP_Stopped_RunTimeError (0x20023) */
    mov r0, #0x18
    ldr r1, =0x20023
    bkpt #0xAB
    b .
