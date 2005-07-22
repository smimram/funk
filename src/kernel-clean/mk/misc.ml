(* misc.ml [part of the funk project]
 *************************************
 * contents  : miscellaneous utilities module
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* process id type *)
type pid = int

(* current process id reference *)
let current_pid = ref (-1)

(* free pid generation function *)
let get_free_pid () =
  let _ = (current_pid := !current_pid + 1)
  in !current_pid

