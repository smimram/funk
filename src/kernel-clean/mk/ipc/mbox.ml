(* mbox.ml [part of the funk project]
 ************************************
 * contents  : mailbox system module
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)

(* exception raised in special situations *)
exception Empty_mbox
exception Full_mbox
exception Invalid_message 

(* message type, holding the pid of the sender *)
(* and the body of the message as a string     *)
type message = Misc.pid * string

(* mailbox type : a queue of incoming messages *)
type mbox = message Queue.t

(* maximum number of messages in a mailbox *)
let max_mbox_size = 10

(* maximum size of a message *)
let max_message_size = 100

(* message packing/unpacking functions *)
(***************************************)

(* message creation function *)
let create_msg sender body = sender,body

(* message sender retrieving function *)
let get_msg_sender = fst

(* message body retrieving function *)
let get_msg_body = snd

(* mailbox manipulation functions *)
(**********************************)

(* new message box creation function *)
let create_mbox () = Queue.create()

(* message sending to a given message box function *)
let send_msg msg dest_box =
  if (String.length (get_msg_body msg)) > max_message_size
    then raise Invalid_message
  else if (Queue.length dest_box) = max_mbox_size
    then raise Full_mbox
  else Queue.add msg dest_box

(* first unread message retrieving function *)
let receive_msg box =
  if Queue.is_empty box then raise Empty_mbox
  else Queue.take box

