(* misc.mli [part of the funk project]
 *************************************
 * contents  : miscellaneous utilities module interface
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* process id type *)
type pid

(* free pid generation function *)
val get_free_pid : unit -> pid

