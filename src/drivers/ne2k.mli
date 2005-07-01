(**
  * Realtek 8029 network interface driver.
  *
  * @author Brice Goglin, Samuel Mimram
  *)

(** Initialize the driver and register the device if present. *)
val init : unit -> unit

(* TODO: remove *)
val test : unit -> unit
