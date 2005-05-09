(* TODO: use an inductive type instead of irq numbers? *)

let kprintf f = Funk.kprintf "IRQ" f

let nb_irq = 16

(** Threads waiting for an IRQ. *)
(* TODO: use conditions *)
let waiting = Array.init nb_irq (fun _ -> [])

external setup_idt : unit -> unit = "caml_funk_setup_idt"

external get_polled_irq : unit -> int = "caml_funk_poll_irq"

let poll_irq () =
  while true
  do
    (* kprintf "Polling for IRQ.\n%!"; *)
    let irq = get_polled_irq () in
      if irq <> -1 then
        (
          if irq <> 1 && irq <> 4 then
            kprintf "Received irq: %d\n%!" irq;
          List.iter (fun t -> KThread.wake t) waiting.(irq);
          waiting.(irq) <- []
        );
      KThread.yield ()
  done
    
let init () =
  setup_idt ();
  kprintf "IDT ready.\n%!";
  ignore (KThread.create poll_irq ());
  kprintf "Polling for interruptions.\n%!"

let wait irq =
  (* kprintf "Waiting for irq %d\n%!" irq; *)
  waiting.(irq) <- (KThread.self ())::waiting.(irq);
  KThread.sleep ()
