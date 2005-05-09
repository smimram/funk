(**
  * Functions related to memory.
  *
  * @author Samuel Mimram
  **)

(** Total memory size, in octets. *)
val size : unit -> int

(** Initialize memory (hum). *)
val init : unit -> unit

(** [remap_page_range phys_addr len virt_addr attr] remaps a page range. *)
(* val remap_page_range : nativeint -> nativeint -> nativeint -> int -> unit *)

(** The type of a pointer to the momory. *)
type t

(** Get a pointer to the memory at given address. *)
val of_addr : int -> t

(** Get a string corresponding to a portion of memory. Warning: use unsafe_*
  * functions or compile with -unsafe.
  *
  * @deprecated Use [setb], etc. functions instead. *)
val to_string : t -> string

(** [setb mem n val] sets the [n]-th byte of [mem]. *)
val setb : t -> int -> int -> unit

(** [setb mem n val] sets the [n]-th word of [mem]. *)          
val setb : t -> int -> int -> unit

(** [memset mem pos len val] fills memory [mem] with [len] bytes [val] starting
  * at position [pos]. *)
val memset : t -> int -> int -> int -> unit 

(** Do not use this... *)
val malloc : int -> t

module MM :
sig
  (** A page directory. *)
  type pgd

  (** [remap_page_range pgd phys_addr len virt_addr flags]. *)
  val remap_page_range : pgd -> nativeint -> nativeint -> nativeint -> int -> unit
end
