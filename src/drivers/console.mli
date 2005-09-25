(* console.mli [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : console interface
 * copyright   : (C) 2005 by samuel mimram, samuel thibault,
                 nicolas guenot
 * email       : samuel.mimram@ens-lyon.org, samuel.thibault@ens-lyon.org,
                 nicolas.guenot@ens-lyon.org

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
  * Functions for handling consoles.
  *
  * @author Nicolas Guénot, Samuel Mimram, Samuel Thibault *)

(** Width of the screen. *)
val len_x : int

(** Height of the screen.*)
val len_y : int

(** The type of a console. *)
type t

(** Colors *)
type color =
  | Black
  | Blue
  | Green
  | Cyan
  | Red
  | Magenta
  | Brown
  | Light_gray
  | Dark_gray
  | Light_blue
  | Light_green
  | Light_cyan
  | Light_red
  | Light_magenta
  | Yellow
  | White

(** Create a new console. *)
val make : unit -> t

(** Get the currently shown console. *)
val get_current_console : unit -> t

(** Check if a console is the currently displayed console. You cannot use [=]
  * since we use functionnal values in type [t]. *)
val is_current_console : t -> bool

(** Display a console (this will change the current console. *)
val display : t -> unit

(** Clear a console. *)
val clear : t -> unit

(** Add a char in a console. *)
val put_char : t -> char -> unit

(** A char was received from the keyboard. *)
val receive_char : t -> char -> unit

(** Print a string in a console. *)
val print_string : t -> string -> unit

val print_string_c : t -> string -> color -> color -> unit

val input_line : t -> string

(* No more serial port for now *)
(* val serial_console : int -> unit *)
