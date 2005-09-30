(* perms.ml [part of the funk project]
 *************************************
 * contents  : permissions system module
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* single permission type *)
type permission =
    Send_message of Misc.pid list
  | Schedule
  | Kill         of Misc.pid list
  | Alloc_pages  of Misc.pid list
  | Free_pages   of Misc.pid list
  | Set_handler  of int list
  | Set_perms    of Misc.pid list

(* permissions account type *)
type perms = permission list

(* permissions account creation function *)
let create_perms () = []

