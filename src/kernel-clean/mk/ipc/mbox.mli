(* mbox.mli [part of the funk project]
 *************************************
 * contents  : mailbox system module interface
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* message and messages box types *)
type message
type mbox

(* exception raised in special situations *)
exception Empty_mbox
exception Full_mbox
exception Invalid_message

(* message packing/unpacking functions *)
(***************************************)

(* message creation function *)
val create_msg : Misc.pid -> string -> message

(* message sender retrieving function *)
val get_msg_sender : message -> Misc.pid

(* message body retrieving function *)
val get_msg_body : message -> string

(* mailbox manipulation functions *)
(**********************************)

(* new message box creation function *)
val create_mbox : unit -> mbox

(* message sending to a given message box. *)
(* pay attention that given pid is the pid *)
(* of the sending process, not the one of  *)
(* the receiving process                   *)
val send_msg : message -> mbox -> unit

(* first unread message retrieving function *)
val receive_msg : mbox -> message

