(* TODO: see http://www.beyondlogic.org/serial/serial.htm *)

let kprintf f = Utils.kprintf "Serial" f

let port_addr = [|0x3f8; 0x2f8; 0x3e8; 0x2e8|]

(** Line Status Register. *)
let offs_lsr = 5
                 
exception Invalid_port
                 
exception No_data

(* TODO: use IRQs *)
let get_byte port =
  if Ports.inb (port_addr.(port) + offs_lsr) land 0x01 <> 0x01 then
    raise No_data
  else
    Ports.inb port_addr.(port)

let send_byte port b =
  Ports.outb port_addr.(port) b

let send_char port c =
  send_byte port (int_of_char c)

let send_string port s =
  for i = 0 to String.length s - 1
  do
    send_char port s.[i]
  done

let echo_kprintf port =
  let old_kprintf = Utils.get_fkprintf () in
    Utils.set_fkprintf
      (fun s ->
         String.iter (send_char port) s;
         old_kprintf s)

let poll_input port =
  while true
  do
    Irq.wait 4;
    while (Ports.inb (port_addr.(port) + 5)) land 0x01 <> 0
    do
      let c = Ports.inb port_addr.(port) in
      let c = if c = 13 then 10 else c in
        Keyboard.simulate_key
          (Keyboard.Char (char_of_int c))
          {
            Keyboard.ks_shift = false;
            Keyboard.ks_alt = false;
            Keyboard.ks_altgr = false;
            Keyboard.ks_ctrl = false;
          }
    done
  done
      
let capture_keyboard port =
  (* Turn off interrupts. *)
  Ports.outb (port_addr.(port) + 1) 0x00;
  (* Set PIC. *)
  Ports.outb 0x21
    ((Ports.inb 0x21) land
     (match port with
       | 0 -> 0xef (* irq 4 *)
       | 1 -> 0xf7 (* irq 3 *)
       | 2 -> 0xef
       | 3 -> 0xf7
       | _ -> raise Invalid_port
    ));
  (* Interrupt when data has been received. *)
  Ports.outb (port_addr.(port) + 1) 0x01;
  ignore (KThread.create poll_input port);
  kprintf "Polling for keyboard input.\n%!"
