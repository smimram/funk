(* mlkernel.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : kernel's ocaml entry point
 * copyright   : (C) 2005 by nicolas guenot, samuel mimram
 * email       : nicolas.guenot@ens-lyon.org, samuel.mimram@ens-lyon.org

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
******************************************************************************)

(**
  * Main kernel function.
  *
  * @author Brice Goglin, Nicolas Guenot, Samuel Mimram
  **)

(* COMMENTS:
 * 12/03/2005 - this file defines the entry point for the ocaml part of the
 * kernel, which is called by the kernel entry function defined in the
 * kernel.c source file.
 *)

let dying_msg = "Uncatched exception: %s\nI'm dying now...\n%!"

(* ml kernel entry point *)
let mlkernel_entry arg =
  try Utils.kprintf "KERNEL" "Funk est dans le jazz\n";
  with e -> let _ = Utils.kprintf "mlkernel" dying_msg (Printexc.to_string e)
            in exit 0

(* export the kernel entry function to C *)
let _ = Callback.register "mlkernel_entry" mlkernel_entry

