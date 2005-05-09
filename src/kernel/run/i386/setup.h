#include "multiboot.h"
#include "mm.h"

/** System call interrupt vector. */
#define SYSCALL_VECT    0x34

/* TODO: chose more useful values? */
#define MASTER_VECT     0x20
#define SLAVE_VECT      0x28

#define PICM            0x20
#define PICMI           0x21
#define PICS            0xA0
#define PICSI           0xA1

#define ICW1            0x11
#define ICW4            0x01

#define NB_IRQS         16

/* System tables sizes. */
#define IDT_SIZE        256
#define GDT_SIZE        4
#define NB_TRAPS        20

/* Kernel segment selectors. */
#define KERNEL_CS       0x0010
#define KERNEL_DS       0x0018

#ifndef ASM

#define MEM_SIZE	mem_size
#endif
