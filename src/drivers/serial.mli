(**
  * Functions for accessing serial ports.
  *
  * @author Samuel Mimram
  **)

(** Be careful, the port numbers are zero-based. *)

(** Send a char on a serial port. *)
val send_char : int -> char -> unit

val send_string : int -> string -> unit

(** Copy [Funk.kprintf] messages on a serial port. *)
val echo_kprintf : int -> unit

(** Emulate keyboard when data is received on a serial port. *)
val capture_keyboard : int -> unit
