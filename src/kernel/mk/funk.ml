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

(** process context *)
type context = {
  mutable uid : Vfs_defs.uid;
  mutable gid : Vfs_defs.gid;
  mutable wd : Vfs_defs.path;
  mutable wd_handle : Vfs_defs.wd_handle;
  mutable umask : Vfs_defs.file_perm;
}

(** ports input/output functions *)
external inb : int -> int = "caml_funk_inb"
external outb : int -> int -> unit = "caml_funk_outb"
external inb_p : int -> int = "caml_funk_inb_p"
external outb_p : int -> int -> unit = "caml_funk_outb_p"
external inw : int -> int = "caml_funk_inw"
external outw : int -> int -> unit = "caml_funk_outw"
external inl : int -> int32 = "caml_funk_inl"
external outl : int -> int32 -> unit = "caml_funk_outl"

(** Generate an interrupt. *)
external make_interrupt : int -> unit = "caml_funk_make_interrupt"

(** Return time of day in seconds. *)
external gettimeofday : unit -> int = "caml_funk_gettimeofday"

(** usleep *)
external usleep : int -> unit = "caml_funk_usleep"

(* The real kprintf function needs the VFS. *)
(* As the VFS is not available during early boot *)
(* early kprintf is used in between to store non saved messages *)
(* early_kprintf will be set to Filecmds.cat_to_append "dmesg_file" *)
(* during VFS initialization. *)

let early_messages = DynArray.create ()

let early_fkprintf (s : string) =
  DynArray.add early_messages s

let fkprintf = ref early_fkprintf

(** Print a kernel message. *)
let kprintf identifier =
  let print message =
    let string = Printf.sprintf "[%s] %s" identifier message
    in
      Printf.printf "%s%!" string;
      !fkprintf string
  in
    Printf.kprintf print

let set_fkprintf func =
  fkprintf := func;
  DynArray.iter !fkprintf early_messages;
  DynArray.clear early_messages;
  kprintf "LOG" "Restored early messages\n"

let get_fkprintf () =
  !fkprintf

external cpuid : int32 -> (int32 * int32 * int32 * int32) = "caml_funk_cpuid"

external cli : unit -> unit = "caml_funk_cli"
external sti : unit -> unit = "caml_funk_sti"
