/* funk_stubs.c [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : caml/C wrappers module
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
#include <caml/mlvalues.h>
#include <caml/memory.h>

#include "libc-dummy.h"
#include "multiboot.h"
#include "output.h"

/* external functions definitions */
extern void do_interrupt(int id);

CAMLprim value caml_funk_inb(value port)
{
  CAMLparam1(port);
  CAMLreturn(Val_int(inb(Int_val(port))));
}

CAMLprim value caml_funk_outb(value port, value val)
{
  CAMLparam2(val, port);
  outb(Int_val(val), Int_val(port));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_inb_p(value port)
{
  CAMLparam1(port);
  CAMLreturn(Val_int(inb_p(Int_val(port))));
}

CAMLprim value caml_funk_outb_p(value port, value val)
{
  CAMLparam2(val, port);
  outb_p(Int_val(val), Int_val(port));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_inw(value port)
{
  CAMLparam1(port);
  CAMLreturn(Val_int(inw(Int_val(port))));
}

CAMLprim value caml_funk_outw(value port, value val)
{
  CAMLparam2(val, port);
  outw(Int_val(val), Int_val(port));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_inl(value port)
{
  CAMLparam1(port);
  CAMLreturn(caml_copy_int32(inl(Int_val(port))));
}

CAMLprim value caml_funk_outl(value port, value val)
{
  CAMLparam2(val, port);
  outl(Int32_val(val), Int_val(port));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_make_interrupt(value id)
{
  CAMLparam1(id);
  do_interrupt(Int_val(id));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_usleep(value delay)
{
  CAMLparam1(delay);
  usleep(Int_val(delay));
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_gettimeofday()
{
  CAMLparam0();
  struct timeval tv;
  int time;
  gettimeofday(&tv, NULL);
  time = tv.tv_sec * 1000000ULL + tv.tv_usec;
  CAMLreturn(Int_val(time));
}

/*
 * Generic CPUID function
 * clear %ecx since some cpus (Cyrix MII) do not set or clear %ecx
 * resulting in stale register contents being returned.
 */
static __inline void cpuid(int op, unsigned int *eax, unsigned int *ebx, unsigned int *ecx, unsigned int *edx)
{
        __asm__("cpuid"
                : "=a" (*eax),
                  "=b" (*ebx),
                  "=c" (*ecx),
                  "=d" (*edx)
                : "0" (op), "c"(0));
}

CAMLprim value caml_funk_cpuid(value op)
{
  CAMLparam1(op);
  unsigned int eax, ebx, ecx, edx;
  CAMLlocal1(ans);
  cpuid(Int32_val(op), &eax, &ebx, &ecx, &edx);
  ans = caml_alloc_tuple(4);
  Store_field(ans, 0, caml_copy_int32(eax));
  Store_field(ans, 1, caml_copy_int32(ebx));
  Store_field(ans, 2, caml_copy_int32(ecx));
  Store_field(ans, 3, caml_copy_int32(edx));
  CAMLreturn(ans);
}

CAMLprim value caml_funk_cli(value vunit)
{
  CAMLparam1(vunit);
  __asm__("cli");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_funk_sti(value vunit)
{
  CAMLparam1(vunit);
  __asm__("sti");
  CAMLreturn(Val_unit);
}
