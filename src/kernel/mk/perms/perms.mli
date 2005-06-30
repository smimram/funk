(* perms.mli [part of the funk project]
 **************************************
 * contents  : permissions system module interface
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* permissions account type *)
type perms

(* permissions account creation function *)
val create_perms : unit -> perms

