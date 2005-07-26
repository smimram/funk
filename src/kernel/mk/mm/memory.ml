let kprintf f = Utils.kprintf "MEM" f

external size : unit -> int = "caml_funk_mem_size"

let init () =
  kprintf "Total size: %d Mo.\n%!" (size () / (1024 * 1024))

(* TODO: nativeint? *)
external get_mem : int -> string = "caml_funk_get_mem"                                                                                 
type t = string

let of_addr = get_mem

let to_string m = m

let setb m n v =
  String.unsafe_set m n (char_of_int v)

let setw m n v =
  String.unsafe_set m (2 * n) (char_of_int (v lsr 8));
  String.unsafe_set m (2 * n + 1) (char_of_int (v land 0xff))

let memset m pos len v =
  String.unsafe_fill m pos len (char_of_int v)

external malloc : int -> t = "caml_funk_malloc"

module MM =
struct
  type pgd = nativeint
    
  external remap_page_range : pgd -> nativeint -> nativeint -> nativeint -> int -> unit = "caml_funk_remap_pr"
end

