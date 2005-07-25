/* boot.S [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : kernel's entry point
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

/* COMMENTS:
 * 12/03/2005 - this file is the boot code for the sss kernel. it must be
 * loaded by a multiboot-compliant bootloader such as grub, and is meant
 * to be compiled using gas. it has been adapted from the "boot.S" file
 * found in the multiboot specification (for further details, please see
 * at http://www.gnu.org/software/grub/manual/multiboot/).
 */

#define ASM
#include "../include/multiboot.h"

.text
.globl multiboot_entry

  /* align on 32 bits boundary */
  .align  4

  /* multiboot header */
  multiboot_header:
  .long    MULTIBOOT_HEADER_MAGIC                           /* magic */
  .long    MULTIBOOT_HEADER_FLAGS                           /* flags */
  .long    -(MULTIBOOT_HEADER_MAGIC+MULTIBOOT_HEADER_FLAGS) /* checksum */
  .long    multiboot_header                                 /* header_addr */
  .long    multiboot_entry                                  /* load_addr */
  .long    _edata                                           /* load_end_addr */
  .long    _end                                             /* bss_end_addr */
  .long    multiboot_entry                                  /* entry_addr */

  multiboot_entry:
  /* initialize the stack pointer */
  movl    $(stack+STACK_SIZE),%esp
  /* initializes the FPU (resets all the registers and flags to their default values) */
  finit
  /* reset eflags */
  pushl   $0
  popf
  /* push multiboot information struct. pointer */
  pushl   %ebx
  /* push the magic value */
  pushl   %eax
  /* now enter the C entry point function */
  call    kernel_entry
  /* halt */
  loop:   hlt
  jmp     loop

/* our stack area */
.comm    stack,STACK_SIZE

