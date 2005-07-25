
(* The real kprintf function needs the VFS. *)
(* As the VFS is not available during early boot *)
(* early kprintf is used in between to store non saved messages *)
(* early_kprintf will be set to Filecmds.cat_to_append "dmesg_file" *)
(* during VFS initialization. *)

let early_messages = DynArray.create ()

let early_fkprintf (s : string) =
  DynArray.add early_messages s

let fkprintf = ref early_fkprintf

(** Print a kernel message. *)
let kprintf identifier =
  let print message =
    let string = Printf.sprintf "[%s] %s" identifier message
    in
      Printf.printf "%s%!" string;
      !fkprintf string
  in
    Printf.kprintf print

let set_fkprintf func =
  fkprintf := func;
  DynArray.iter !fkprintf early_messages;
  DynArray.clear early_messages;
  kprintf "LOG" "Restored early messages\n"

let get_fkprintf () =
  !fkprintf

