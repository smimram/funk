/* kernel.c [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : kernel's entry point
 * copyright   : (C) 2005 by nicolas guenot, samuel thibault,
 *               samuel mimram
 * email       : nicolas.guenot@ens-lyon.org, samuel.thibault@ens-lyon.org
 *               samuel.mimram@ens-lyon.org

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

/* COMMENTS:
 * 12/03/2005 - this file defines the entry point for the kernel, which
 * is called by the assembler boot code loaded by the boot-loader (which
 * must be a multiboot-compliant one: for example, grub).
 */

#include "multiboot.h"
#include "libc-dummy.h"
#include "threads.h"
#include "asm-utils.h"
#include "kernel.h"
#include <caml/callback.h>

/* external functions prototypes */
extern void caml_startup(char **args);
extern void setup_kernel();
extern void setup_memory(void* start, void* end);

static value mlkernel_arg = Val_int(0);

/* macro used to check the flags in mbi struct */
#define CHECK_FLAG(flags,bit)   ((flags) & (1 << (bit)))

unsigned long mem_size;

/* kernel entry point function, called from assembler code */
void kernel_entry(unsigned long magic,unsigned long addr)
{
  /* we will need a multiboot info pointer */
  multiboot_info_t *mbi;
  memory_map_t *mmap;
  unsigned long mmap_size;
  unsigned long block_size = 1<<20; /* assume at least 1MB upper memory */
  static char * argv[]={ "ocaml", NULL };

  /* check the multiboot-compliant magic number */
  if(magic!=MULTIBOOT_BOOTLOADER_MAGIC) return;

  /* now set mbi to the right address */
  mbi = (multiboot_info_t *) addr;
  mmap = (memory_map_t *) mbi->mmap_addr;
  mmap_size = mbi->mmap_length;

  if(CHECK_FLAG(mbi->flags,6))
    while (mmap_size>0)
    {
      if ((void *) mmap->base_addr_low == &_begin) {
	block_size = mmap->length_low;
	break;
      }
      mmap_size-=sizeof(*mmap);
      mmap++;
    }
  if (&_begin+block_size<&__bss_end) {
	  c_printf("not enough memory for funk: %lu upper memory needed",&__bss_end-&_begin);
	  while(1);
  }
  /* clean bss */
  memset(&__bss_start,0,&__bss_end-&__bss_start); 
  /* gcc seems bugged and move some of the following affectations *before* memset... */
  __asm__ __volatile__("" : : : "memory");
  /* TODO: more accurate value */
  mem_size = (unsigned long)(&_begin+block_size);
  heap = &_end;
  heaplimit = &_begin+block_size;
  last_seen = heap + 2 + 4*sizeof(void*);
  /* then we can setup the kernel */
  setup_kernel();
  /* We also setup the memory */
  setup_memory(heap + HEAP_OFFSET, heaplimit);
  /* then call the caml startup function */
  caml_startup(argv);
  /* initialize threads */
  thread_init();
  /* start the ml kernel... */
  create_thread(caml_named_value("mlkernel_entry"), &mlkernel_arg);
  /* and stay idle */
  while (1) {
    sched_yield();
    hlt();
  }
}
