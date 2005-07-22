(* Old API *)
(** Internal representation of caml types. *)
type type_t =
  | Int (** an integer *)
  | String (** a string *)
  | Tuple of type_t array (** a tuple or a record *)

exception Invalid_type

type argument
               
(** The IPC : name -> type of the arg + function. *)
let ipcs = (Hashtbl.create 100 : ((string, ('a * type_t)) Hashtbl.t))

let add name f arg_t =
  Hashtbl.add ipcs name (f, arg_t)
    
let exec name arg_t arg =
  let ipc_f, ipc_arg_t =
    try
      Hashtbl.find ipcs name
    with
        Not_found -> raise Invalid_type
  in
    ipc_f arg

(** TODO: marshall / unmarshall the argument *)
let arg_of_val t arg =
  Obj.magic arg
      
let val_of_arg t arg =
  Obj.magic arg
