(* funk.mli [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : funk functions general interface
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

(** Input on a port. *)
val inb : int -> int

(** [outb port val] outputs value [val] on port [port]. *)
val outb : int -> int -> unit

(** Same as [inb] but makes a pause after until input completes. *)
val inb_p : int -> int

(** Same as [outb] but makes a pause after until output completes. *)
val outb_p : int -> int -> unit

val inw : int -> int

val outw : int -> int -> unit

val inl : int -> int32

val outl : int -> int32 -> unit

