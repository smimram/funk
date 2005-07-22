(* process.mli [part of the funk project]
 ****************************************
 * contents  : process representation module interface
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* process kernel structure type *)
type process

(* process structure creation function. *)
(* the given pid is the father's one    *)
val create_process : Misc.pid -> process

