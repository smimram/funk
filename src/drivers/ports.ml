(* funk.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : funk functions general module
 * copyright   : (C) 2005 by samuel mimram, nicolas guenot
 * email       : samuel.mimram@ens-lyon.org, nicolas.guenot@ens-lyon.org

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
  * Funk kernel's system functions.
  *
  * This should only be used by the kernel and not by user programs.
  *
  * @author Nicolas Guenot, Samuel Mimram
  *)


(** ports input/output functions *)
external inb : int -> int = "caml_funk_inb"
external outb : int -> int -> unit = "caml_funk_outb"
external inb_p : int -> int = "caml_funk_inb_p"
external outb_p : int -> int -> unit = "caml_funk_outb_p"
external inw : int -> int = "caml_funk_inw"
external outw : int -> int -> unit = "caml_funk_outw"
external inl : int -> int32 = "caml_funk_inl"
external outl : int -> int32 -> unit = "caml_funk_outl"
