(**
  * Functions for handling keyboards.
  *
  * @author Samuel Mimram
  **)

(** A special key. *)
type special_key =
  | F1
  | F2
  | Page_up
  | Page_down
  | Left
  | Right
  | Up
  | Down

(** A character. *)
type chr = Char of char | Special_key of special_key

(** The state of the keyboard. *)
type keyboard_state =
    {
      mutable ks_shift : bool;
      mutable ks_alt : bool;
      mutable ks_altgr : bool;
      mutable ks_ctrl : bool;
    }

(** Initialize the keyboard (start polling on IRQ 1). *)
val init : unit -> unit

(** Set the handler for keys. *)
val on_key : (chr -> keyboard_state -> unit) -> unit

(** Act like if a key had been pressed. *)
val simulate_key : chr -> keyboard_state -> unit
