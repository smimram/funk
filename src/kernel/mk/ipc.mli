(**
  * Functions for handling inter-process communications (IPC).
  *
  * @author Samuel Mimram
  **)

(** The argument of an IPC was of the wrong type. *)
exception Invalid_type

(** Internal representation of caml types. *)
type type_t =
  | Int (** an integer *)
  | String (** a string *)
  | Tuple of type_t array (** a tuple or a record *)

(** The type of an IPC argument. *)
type argument

(** [add name f t] adds the IPC function [f] which argument if of type [t] 
  * under the name [name]. *)
val add : string -> (argument -> unit) -> type_t -> unit

(** Execute an IPC. *)
val exec : string -> type_t -> argument -> unit

(** Get an argument corresponding to a value. *)
val arg_of_val : type_t -> 'a -> argument
                                             
(** Get the value of an argument. *)
val val_of_arg : type_t -> argument -> 'a
