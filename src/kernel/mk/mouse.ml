let kprintf f = Funk.kprintf "Mouse" f

(** Aux device input buffer. *)
let aux_input_port = 0x60

(** Aux device output buffer. *)
let aux_output_port = 0x60

(** Aux device command buffer. *)
let aux_cmd_port = 0x64
(** Send aux device data. *)
let aux_cmd_send_data = 0xd4
(** Disable aux port. *)
let aux_cmd_disable = 0xa7
(** Enable aux port. *)
let aux_cmd_enable = 0xa8
                       
(** Aux device status register. *)
let aux_status_port = 0x64

(** Output buffer is full. *)
let out_buf_full_mask = 0x21
(** Input buffer is full. *)
let in_buf_full_mask = 0x02

let write_mouse v =
  Funk.outb_p aux_cmd_port aux_cmd_send_data;
  Funk.outb_p aux_output_port v

let read_byte () =
  if Funk.inb aux_status_port land out_buf_full_mask = out_buf_full_mask then
    Funk.inb_p aux_input_port
  else
    raise Not_found (* TODO: better exception *)

let poll_mouse () =
  while true
  do
    Irq.wait 12;
    kprintf "Event received!\n%!"
  done
      
let init () =
  (* Unmask interrupt. *)
  Funk.outb_p 0xa1 ((Funk.inb 0xa1) land 0xef);
  Funk.outb_p aux_cmd_port aux_cmd_enable;
  kprintf "PS/2 mouse enabled.\n%!";
  ignore (KThread.create poll_mouse ());
  kprintf "Polling for events.\n%!"


