(* process.ml [part of the funk project]
 ***************************************
 * contents  : process representation module
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* process kernel structure type : the first pid *)
(* is the process id, the last one is the father *)
(* id, and there's also the mailbox, permissions *)
(* account and pages table.                      *)
type process = Misc.pid * Mbox.mbox * Perms.perms
             * Mm.page_table * Misc.pid

(* process structure creation function *)
let create_process father =
  let pid     = Misc.get_free_pid()
  in let mbx  = Mbox.create_mbox()
  in let prm  = Perms.create_perms()
  in let pgtb = Mm.create_page_table()
  in let fthr = father
  in pid,mbx,prm,pgtb,fthr

