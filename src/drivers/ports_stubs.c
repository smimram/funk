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

