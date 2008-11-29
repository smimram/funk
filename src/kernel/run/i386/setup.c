/* setup.c [part of the funk project]
   [functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : kernel setup code
 * copyright   : (C) 2005 by nicolas guenot
 * email       : nicolas.guenot@ens-lyon.org

 *******************************************************************************
 *                                                                             *
 * This program is free software; you can redistribute it and/or               *
 * modify it under the terms of the GNU General Public License                 *
 * as published by the Free Software Foundation; either version 2              *
 * of the License, or (at your option) any later version.                      *
 *                                                                             *
 * This program is distributed in the hope that it will be useful,             *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of              *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                *
 * GNU General Public License for more details.                                *
 *                                                                             *
 * You should have received a copy of the GNU General Public License           *
 * along with this program; if not, write to the Free Software                 *
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. *
 *                                                                             *
 ******************************************************************************/

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "libc-dummy.h"
#include "asm-utils.h"

#include "setup.h"
#include "kernel.h"

extern void (*irqs[NB_IRQS])(void);
extern void (*traps[NB_TRAPS])(void);

/** The current IRQ mask. */
int irq_mask;

/** Generate an interrupt. */
void do_interrupt(int id)
{
  int res;
  printf("raising interrupt %i\n",id);
  __asm__ __volatile__ ("int %1"
      : "=a" (res)
      : "i" (SYSCALL_VECT), "0" (id));
  printf("returned %d\n", res);
}

#define IRQ_BUFFER_LEN 1024
unsigned char triggered_irq[IRQ_BUFFER_LEN];
int ts, te = 0; /* Start, end positions */

static __inline void polled_irq(unsigned char irq)
{
  triggered_irq[te++] = irq;
  if (te >= IRQ_BUFFER_LEN)
    te = 0;
}

CAMLprim value caml_funk_poll_irq(value unit)
{
  CAMLparam1(unit);
  unsigned char ans = -1;

  if (te == ts)
    CAMLreturn(Val_int(-1));
  else
  {
    ans = triggered_irq[ts++];
    if (ts >= IRQ_BUFFER_LEN)
      ts = 0;
    CAMLreturn(Val_int(ans));
  }
}

static __inline void EOI_MASTER()
{
  __asm__ __volatile__("\
      mov $0x20,%%al; \
      out %%al,$0x20; \
      " : : : "al");
}

static __inline void EOI_SLAVE()
{
  __asm__ __volatile__("\
      mov $0xa0,%%al; \
      out %%al,$0x20; \
      " : : : "al");
}

/** System call interrupt handler. */
static void syscall_handler(int num)
{
  /* returning id + 145 */
  __asm__ __volatile__("addl $145, %eax; iret");
}

/* TODO: write this in asm to save the registers!!!! */
/** Void interrupt handler. */
static void void_handler()
{
  *((char *)0xb8000) = 'v';
  __asm__ __volatile__("iret");
}

void fastcall do_trap(unsigned int num)
{
  const static char *const mess[NB_TRAPS] = {
    "divide_error",             /*  0 */
    "debug",                    /*  1 */
    "nmi",                      /*  2 */
    "int3",                     /*  3 */
    "overflow",                 /*  4 */
    "bounds",                   /*  5 */
    "invalid_op",               /*  6 */
    "device_not_available",     /*  7 */
    "double_fault",             /*  8 */
    "coprocessor_seg_overrun",  /*  9 */
    "invalid_TSS",              /* 10 */
    "segment_not_present",      /* 11 */
    "stack_segment",            /* 12 */
    "general_protection",       /* 13 */
    "page_fault",               /* 14 */
    "spurious_interrupt_bug",   /* 15 */
    "coprocessor_error",        /* 16 */
    "alignment_check",          /* 17 */
    "machine_check",            /* 18 */
    "simd_coprocessor_error",   /* 19 */
  };

  switch(num) {
    case 1: /* debug */
      break;
    case 14: {
      void *address;
      __asm__("movl %%cr2,%0" : "=r" (address));
      c_printf("page fault for address %p\n",address);
    }
    default:
      /* c_printf is not reentrant but we won't come back anyway... */
      c_printf("%s !\n",mess[num]);
      hang();
  }
}

void fastcall do_irq(unsigned int num)
{
  ((char *)0xb8000)[2*(num+1)] ^= 'a'-'A';
  if (num >=1 && num <= 15)
    polled_irq(num);
  EOI_MASTER();
  if (num >= 8)
    EOI_SLAVE();
}

/* Interrupt descriptor parts macro. */
#define INTDESCRIPTOR_LO(f) (((unsigned int)f) & 0x0000FFFFUL) | (KERNEL_CS<<16)
#define INTDESCRIPTOR_HI(f) (((unsigned int)f) & 0xFFFF0000UL) | 0xef00UL
#define SETINT(n,f) do { \
  idt_contents[n].lo = INTDESCRIPTOR_LO(f); \
  idt_contents[n].hi = INTDESCRIPTOR_HI(f); \
} while(0)

static struct content
{
  unsigned lo,hi;
}
/** Interrupt Descriptors Table contents. */
idt_contents[IDT_SIZE],
  /** GLOBAL descriptor table contents. */
  gdt_contents[] =
{
  [KERNEL_CS/sizeof(*gdt_contents)] = { 0x0000FFFF, 0x00CF9A00 },
  [KERNEL_DS/sizeof(*gdt_contents)] = { 0x0000FFFF, 0x00CF9200 },
};

/** System idt register value. */
static struct system_table
{
  unsigned short int limit;
  unsigned int base_addr;
  short int padding;
} __attribute__((packed))
system_table =
{
  .limit     = sizeof(gdt_contents)-1,
  .base_addr = (unsigned int) gdt_contents,
},
  idt_table =
{
  .limit     = sizeof(idt_contents)-1,
  .base_addr = (unsigned int) idt_contents,
};

void setup_kernel()
{
  setup_pagination();

  /* Load the gdt register. */
  __asm__ __volatile__("lgdt %0" : : "m" (system_table));
  /* Update the segment registers. */
  __asm__ __volatile__("\
      movw %w0, %%ds; \
      movw %w0, %%es; \
      movw %w0, %%fs; \
      movw %w0, %%gs; \
      movw %w0, %%ss" : : "r" (KERNEL_DS));
  __asm__ __volatile__("ljmp %0, $1f; 1:" : : "i" (KERNEL_CS));
}

/** Remap the PIC so that IRQ0 will start at MASTER_VECT and IRQ8 at SLAVE_VECT. */
static __inline void setup_pic()
{
  /* Send ICW1. */
  outb(ICW1, PICM);
  outb(ICW1, PICS);

  /* Remap PICs. */
  outb(MASTER_VECT, PICMI);
  outb(SLAVE_VECT, PICSI);

  /* Send CW3. */
  outb(4, PICMI); /* IRQ2 -> connection to slave. */
  outb(2, PICSI);

  /* Send ICW4. */
  outb(ICW4, PICMI);
  outb(ICW4, PICSI);

  /* Disable interrupts (excepting IRQ2). */
  outb(0xfb, PICMI);
  outb(0xff, PICSI);

  irq_mask = 0xfffb;

  /* TODO: remove this and really use the mask!. */
  outb(0x00, PICMI);
  outb(0x00, PICSI);
}

void setup_idt()
{
  int i;

  setup_pic();

  /* Setup the idt contents. */
  for (i = 0; i < IDT_SIZE; i++)
    SETINT(i, void_handler);
  for (i = 0; i < NB_TRAPS; i++)
    SETINT(i, traps[i]);
  for (i = MASTER_VECT; i < MASTER_VECT + 8; i++)
    SETINT(i, irqs[i - MASTER_VECT]);
  for (i = SLAVE_VECT; i < SLAVE_VECT + 8; i++)
    SETINT(i, irqs[i - SLAVE_VECT + 8]);
  SETINT(SYSCALL_VECT, syscall_handler);
  /* Load the idt register. */
  __asm__ __volatile__("lidt %0" : : "m" (idt_table));
  __asm__ __volatile__("sti");
  map_page_range(page_base, 0, FRAME_SIZE, 0, 0); /* No more *NULL, niark niark... */
}

CAMLprim value caml_funk_setup_idt(value unit)
{
  CAMLparam1(unit);
  setup_idt();
  CAMLreturn(Val_unit);
}

/** Sets up the memory for malloc to work 
 * See libc-dummy.c for details. */
void setup_memory (void* start, void* end)
{  
  int s = sizeof(void*);
  /* Total available space */
  int size = (end - start) - 7*s;
#ifdef DEBUG
  c_printf ("size: %d\n", size);
#endif

  *(void**) start         = (start + 4*s);
  *(void**) (start + s)   = 0;
  *(void**) (start + 2*s) = end;
  *(int*)   (start + 3*s) = size;
  *(void**) (end   - 3*s) = (start + 4);
  *(void**) (end   - 2*s) = NULL;
  *(int*)   (end   - s)   = 0;
#ifdef DEBUG
  c_printf ("start: %p (prev: %p, next: %p, size: %d)\n", start+4, *(void**)(start), *(void**)(start+4), *(int*)(start+8));
  c_printf ("end: %p (prev: %p, next: %p, size: %d)\n", end, *(void**)(end-12), *(void**)(end-8), *(int*)(end-4));
#endif
}
