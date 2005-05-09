/* ocamlwrapper.c [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : ocaml functions wrapper for the kernel
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

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include "libc-dummy.h"

/* kernel entry wrapper function */
void mlkernel_entry()
{
  static value *mlk_cls = NULL;
  if(mlk_cls==NULL) mlk_cls = caml_named_value("mlkernel_entry");
  caml_callback(*mlk_cls,Val_int(0));
}

