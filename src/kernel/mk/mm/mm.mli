(* Crap that will be deleted soon.
* Provided just in order to be able to compile *)
(* mm.mli [part of the funk project]
 ***********************************
 * contents  : memory management module interface
 * copyright : (C) 2005 by the authors
 * published under GPL (see COPYING in root directory)
 *)


(** An entry of the page table *)
type page_table_entry

exception Invalid_page_table_entry of page_table_entry

(** process pages table type *)
type page_table

(** pages table creation function *)
val create_page_table : unit -> page_table

val from_page_table_entry : page_table_entry -> int
