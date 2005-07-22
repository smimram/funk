/* boot.S [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : kernel's interrupt handlers
 * copyright   : (C) 2005 by samuel thibault
 * email       : samuel.thibault@ens-lyon.org

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

#define ASM
#include "setup.h"

.text

  /* align on 32 bits boundary */
  .align  4

#define ENTRY(name) \
	.globl name; \
	.align 4; \
	name:

vector=0
.data
ENTRY(traps)
.text
.rept NB_TRAPS
	.align 4
1:	pushl $vector ;
	jmp _trap
.data
	.long 1b
.text
vector=vector+1
.endr

vector=0
.data
ENTRY(irqs)
.text
.rept NB_IRQS
	.align 4
1:	pushl $vector ;
	jmp _irq
.data
	.long 1b
.text
vector=vector+1
.endr

.align 4
_irq:
  xchgl 0(%esp),%eax
  pusha
  cld
  incl interrupt_disabled
  call do_irq
  jmp _ret

.align 4
_trap:
  xchgl 0(%esp),%eax
  pusha
  cld
  incl interrupt_disabled
  call do_trap

_ret:
  decl interrupt_disabled
  popa
  pop %eax
  iret
