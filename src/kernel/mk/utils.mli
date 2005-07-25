(** Print a kernel message. *)
val kprintf :  string -> ('a, unit, string, unit) format4 -> 'a

(** Set the definitive routine to write kernel messages in file *)
val set_fkprintf :  (string -> unit) -> unit

val get_fkprintf : unit -> (string -> unit)
