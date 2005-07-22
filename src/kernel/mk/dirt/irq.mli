(**
  * Functions for setting IRQ handlers.
  *
  * @author Samuel Mimram
  **)

(** Initialize the IRQ handler. *)
val init : unit -> unit

(** Sleep until an IRQ has been triggered. *)
val wait : int -> unit
