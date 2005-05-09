(* Ne2k.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : Ne2k network interface driver module
 * copyright   : (C) 2005 by samuel mimram, samuel thibault,
                 nicolas guenot
 * email       : samuel.mimram@ens-lyon.org, samuel.thibault@ens-lyon.org,
                 nicolas.guenot@ens-lyon.org

*******************************************************************************
*                                                                             *
* This program is free software; you can redistribute it and/or               *
* modify it under the terms of the GNU General Public License                 *
* as published by the Free Software Foundation; either version 2              *
* of the License, or (at your option) any later version.                      *
*                                                                             *
* This program is distributed in the hope that it will be useful,             *
* but WITHOUT ANY WARRANTY; without even the implied warranty of              *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                *
* GNU General Public License for more details.                                *
*                                                                             *
* You should have received a copy of the GNU General Public License           *
* along with this program; if not, write to the Free Software                 *
* Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. *
*                                                                             *
******************************************************************************)

(**
  * Ne2k network interface driver.
  *
  * @author Brice Goglin, Samuel Mimram
  **)

(* Embedding message prefix. *)
let kprintf f = Funk.kprintf "Ne2k" f

(* Vendor and device number of the PCI device
 * registered by this driver. *)
let vendor = 0x10ec
and device = 0x8029

(* Size of IO Port range. *)
let ne_CMD      = 0x00
let ne_DATAPORT = 0x10 (* NatSemi-defined port window offset. *)
let ne_RESET    = 0x1f (* Issue a read to reset, a write to clear. *)
let ne_EXTEND   = 0x20

let tx_PAGES = 12 (* Two Tx slots. *)

let neSM_START_PG = 0x40 (* First page of TX buffer *)
let neSM_STOP_PG  = 0x80 (* Last page +1 of RX ring *)

(* Some generic ethernet register configurations. *)
let e8390_TX_IRQ_MASK = 0xa  (* For register EN0_ISR *)
let e8390_RX_IRQ_MASK = 0x5
let e8390_RXCONFIG    = 0x4  (* EN0_RXCR: broadcasts, no multicast,errors *)
let e8390_RXOFF       = 0x20 (* EN0_RXCR: Accept no packets *)
let e8390_TXCONFIG    = 0x00 (* EN0_TXCR: Normal transmit mode *)
let e8390_TXOFF       = 0x02 (* EN0_TXCR: Transmitter off *)

(* Register accessed at EN_CMD, the 8390 base addr. *)
let e8390_CMD    = 0x00 (* The command register (for all pages) *)
let e8390_STOP   = 0x01 (* Stop and reset the chip *)
let e8390_START  = 0x02 (* Start the chip, clear reset *)
let e8390_TRANS  = 0x04 (* Transmit a frame *)
let e8390_RREAD  = 0x08 (* Remote read *)
let e8390_RWRITE = 0x10 (* Remote write *)
let e8390_NODMA  = 0x20 (* Remote DMA *)

let e8390_PAGE0  = 0x00 (* Select page chip registers *)
let e8390_PAGE1  = 0x40 (* using the two high-order bits *)
let e8390_PAGE2  = 0x80 (* Page 3 is invalid. *)

(* Page 0 register offsets. *)
let en0_CLDALO   = 0x01 (* Low byte of current local dma addr RD *)
let en0_STARTPG  = 0x01 (* Starting page of ring bfr WR *)
let en0_CLDAHI   = 0x02 (* High byte of current local dma addr RD *)
let en0_STOPPG   = 0x02 (* Ending page +1 of ring bfr WR *)
let en0_BOUNDARY = 0x03 (* Boundary page of ring bfr RD WR *)
let en0_TSR      = 0x04 (* Transmit status reg RD *)
let en0_TPSR     = 0x04 (* Transmit starting page WR *)
let en0_NCR      = 0x05 (* Number of collision reg RD *)
let en0_TCNTLO   = 0x05 (* Low byte of tx byte count WR *)
let en0_FIFO     = 0x06 (* FIFO RD *)
let en0_TCNTHI   = 0x06 (* High byte of tx byte count WR *)
let en0_ISR      = 0x07 (* Interrupt status reg RD WR *)
let en0_CRDALO   = 0x08 (* low byte of current remote dma address RD *)
let en0_RSARLO   = 0x08 (* Remote start address reg 0 *)
let en0_CRDAHI   = 0x09 (* high byte, current remote dma address RD *)
let en0_RSARHI   = 0x09 (* Remote start address reg 1 *)
let en0_RCNTLO   = 0x0a (* Remote byte count reg WR *)
let en0_RCNTHI   = 0x0b (* Remote byte count reg WR *)
let en0_RSR      = 0x0c (* rx status reg RD *)
let en0_RXCR     = 0x0c (* RX configuration reg WR *)
let en0_TXCR     = 0x0d (* TX configuration reg WR *)
let en0_COUNTER0 = 0x0d (* Rcv alignment error counter RD *)
let en0_DCFG     = 0x0e (* Data configuration reg WR *)
let en0_COUNTER1 = 0x0e (* Rcv CRC error counter RD *)
let en0_IMR      = 0x0f (* Interrupt mask reg WR *)
let en0_COUNTER2 = 0x0f (* Rcv missed frame error counter RD *)

(* Page 1 register offsets. *)
let en1_CURPAG   = 0x07 (* Current memory page RD WR *)

(* Bits in EN0_ISR - Interrupt status register *)
let enISR_RX       = 0x01 (* Receiver, no error *)
let enISR_TX       = 0x02 (* Transmitter, no error *)
let enISR_RX_ERR   = 0x04 (* Receiver, with error *)
let enISR_TX_ERR   = 0x08 (* Transmitter, with error *)
let enISR_OVER     = 0x10 (* Receiver overwrote the ring *)
let enISR_COUNTERS = 0x20 (* Counters need emptying *)
let enISR_RDC      = 0x40 (* Remote dma complete *)
let enISR_RESET    = 0x80 (* Reset completed *)
let enISR_ALL      = 0x3f (* Interrupts we will enable *)

(* Bits in received packet status byte and en0_RSR. *)
let enRSR_RXOK     = 0x01 (* Received a good packet *)
                       
(** Cannot allocate ethernet device. *)
exception Out_free_res
(** Card failure. *)
exception Out_free_netdev

(* Do a preliminary verification that we have a 8390. *)
let check_8390 ioaddr reg0 =
  Funk.outb (ioaddr + e8390_CMD) (e8390_NODMA + e8390_PAGE1 + e8390_STOP);
  let regd = Funk.inb (ioaddr + 0x0d)
  in
    Funk.outb (ioaddr + 0x0d) 0xff;
    Funk.outb (ioaddr + e8390_CMD) (e8390_NODMA + e8390_PAGE0);
    ignore(Funk.inb (ioaddr + en0_COUNTER0)); (* Clear the counter by reading. *)
    if Funk.inb (ioaddr + en0_COUNTER0) != 0 then
      (
	Funk.outb ioaddr reg0;
        Funk.outb (ioaddr + 0x0d) regd; (* Restore the old values. *)
	raise Out_free_res;
      );
    kprintf "Successfully checked 8390.\n"

(** Reset card. Who knows what dain-bramaged state it was left in. *)
let reset_card ioaddr =
  Funk.outb (ioaddr + ne_RESET) (Funk.inb (ioaddr + ne_RESET));
  Funk.usleep 10000;
  if Funk.inb (ioaddr + en0_ISR) land enISR_RESET == 0 then
    begin
      kprintf "Card failure (no reset ack).\n";
      raise Out_free_netdev;
    end;
  Funk.outb (ioaddr + en0_ISR) 0xff; (* Ack all intr. *)
  kprintf "Successfully reseted card\n"

let init_registers ioaddr =
  Funk.outb (ioaddr + e8390_CMD) (e8390_NODMA + e8390_PAGE0 + e8390_STOP); (* Select page 0 *)
  Funk.outb (ioaddr + en0_DCFG) 0x49; (* Set word-wide access. *)
  Funk.outb (ioaddr + en0_RCNTLO) 0x00; (* Clear the count regs. *)
  Funk.outb (ioaddr + en0_RCNTHI) 0x00;
  Funk.outb (ioaddr + en0_IMR) 0x00; (* Mask completion irq. *)
  Funk.outb (ioaddr + en0_ISR) 0xff;
  Funk.outb (ioaddr + en0_RXCR) e8390_RXOFF; (* 0x20 Set to monitor *)
  Funk.outb (ioaddr + en0_TXCR) e8390_TXOFF; (* 0x02 and loopback mode. *)
  Funk.outb (ioaddr + en0_RCNTLO) 32;
  Funk.outb (ioaddr + en0_RCNTHI) 0x00;
  Funk.outb (ioaddr + en0_RSARLO) 0x00; (* DMA starting at 0x0000. *)
  Funk.outb (ioaddr + en0_RSARHI) 0x00;
  Funk.outb (ioaddr + e8390_CMD) (e8390_RREAD + e8390_START);
  kprintf "Successfully initialized registers\n"

let saprom ioaddr =
  let arr = Array.init 32 (fun _ -> Funk.inb (ioaddr + ne_DATAPORT)) in
    kprintf "Successfully read SA_PROM\n";
    arr

let card_open ioaddr irq =
  (* request irq *)
  Funk.outb (ioaddr + ne_CMD) (0xc0 + e8390_NODMA); (* Page 3. *)
  Funk.outb (ioaddr + 0x20) (Funk.inb (ioaddr + 0x20) lor 0x80)

type device =
    {
      irq : int;
      base_addr : int;
      dev_addr : int array;
      tx_start_page : int;
      rx_start_page : int;
      stop_page : int;
    }

let dev = ref None

exception No_device
               
let get_dev () =
  match !dev with
    | Some d -> d
    | None -> raise No_device
                  
(** Set full-duplex. *)
let set_fdx ioaddr =
  Funk.outb (ioaddr + ne_CMD) (0xc0 + e8390_NODMA); (* Page 3. *)
  Funk.outb (ioaddr + 0x01) 0xc0; (* Enable writes to CONFIG3. *)
  Funk.outb (ioaddr + 0x06) 0x40; (* Enable full duplex. *)
  Funk.outb (ioaddr + 0x01) 0x00; (* Disable writes to CONFIG3. *)
  Funk.outb (ioaddr + ne_CMD) (e8390_PAGE0 + e8390_NODMA); (* Page 0. *)
  kprintf "Set in full duplex mode.\n%!"

(** Initialize 8390 hardware. *)
let ns8390_init () =
  let dev = get_dev () in
    (* Follow National Semi's recommendations for initing the DP83902. *)
    Funk.outb_p (dev.base_addr + e8390_CMD) (e8390_NODMA + e8390_PAGE0 + e8390_STOP); (* 0x21 *)
    Funk.outb_p (dev.base_addr + en0_DCFG) 0x48; (* TODO: maybe 0x49? *)
    (* Clear the remote byte count registers. *)
    Funk.outb_p (dev.base_addr + en0_RCNTLO) 0x00;
    Funk.outb_p (dev.base_addr + en0_RCNTHI) 0x00;
    (* Set to monitor and loopback mode -- this is vital! *)
    Funk.outb_p (dev.base_addr + en0_RXCR) e8390_RXOFF;
    Funk.outb_p (dev.base_addr + en0_TXCR) e8390_TXOFF;
    (* Set the transmit page and receive ring. *)
    Funk.outb_p (dev.base_addr + en0_TPSR) dev.tx_start_page;
    (* TODO: ei_local->tx1 = ei_local->tx2 = 0; *)
    Funk.outb_p (dev.base_addr + en0_STARTPG) dev.rx_start_page;
    Funk.outb_p (dev.base_addr + en0_BOUNDARY) (dev.stop_page - 1); (* 3c503 says 0x3f,NS0x26 *)
    (* TODO: ei_local->current_page = ei_local->rx_start_page;               /*
     * assert boundary+1 */ *)
    Funk.outb_p (dev.base_addr + en0_STOPPG) (dev.stop_page);
    (* Clear the pending interrupts and mask. *)
    Funk.outb_p (dev.base_addr + en0_ISR) 0xff;
    Funk.outb_p (dev.base_addr + en0_IMR) 0x00;
    (* Copy the station address into the DS8390 registers. *)
    Funk.outb_p (dev.base_addr + e8390_CMD) (e8390_NODMA + e8390_PAGE1 + e8390_STOP); (* 0x61 *)
    for i = 0 to 5
    do
      Funk.outb_p (dev.base_addr + 1) dev.dev_addr.(i);
      (* TODO: for debugging, remove. *)
      if Funk.inb_p (dev.base_addr + 1) <> dev.dev_addr.(i) then raise Out_free_netdev
    done;
    Funk.outb_p (dev.base_addr + en1_CURPAG) dev.rx_start_page;
    Funk.outb_p (dev.base_addr + e8390_CMD) (e8390_NODMA + e8390_PAGE0 + e8390_STOP);
    
    (* Initiate chip processing. *)
    Funk.outb_p (dev.base_addr + en0_ISR) 0xff;
    Funk.outb_p (dev.base_addr + en0_IMR) enISR_ALL;
    Funk.outb_p (dev.base_addr + e8390_CMD) (e8390_NODMA + e8390_PAGE0 + e8390_START);
    Funk.outb_p (dev.base_addr + en0_TXCR) e8390_TXCONFIG; (* Xmit on. *)
    (* 3c503 TechMan says rxconfig only after the NIC is started. *)
    Funk.outb_p (dev.base_addr + en0_RXCR) e8390_RXCONFIG
    (* TODO: set multicast list *)
                                    
(** Device probing routine.
  * Called for each PCI device matching vendor and device numbers. *)
let probe unit =
  kprintf "Trying to register one device...\n";
  try
    (* Enable device. *)
    let ioaddr = 
      (* kprintf "Looking for IO Ports in resource bar #0\n"; *)
      match unit.Pci.resource.(0) with
        | Some (Pci.IO_Ports addr) ->
            let addr = Int32.to_int addr in
              (kprintf "Found IO Ports at 0x%x.\n" addr);
              addr
        | Some (Pci.IO_Memory _) ->
            raise (Invalid_argument "Found IO Memory instead of IO Ports.")
        | None ->
            raise (Invalid_argument "IO Ports not found.")
    in
    let irq = unit.Pci.irq
    in
      kprintf "Device has IRQ %d.\n" irq;
      let reg0 = Funk.inb ioaddr
      in
        if reg0 == 0xff then raise Out_free_res;
        check_8390 ioaddr reg0;
        (* Alloc netdev. *)
        reset_card ioaddr;
        init_registers ioaddr;
        let saprom = saprom ioaddr in
          (* We always set the 8390 registers for word mode. *)
          Funk.outb (ioaddr + en0_DCFG) 0x49;
          (* let start_page = neSM_START_PG in
          let stop_page = neSM_STOP_PG in *)
          card_open ioaddr irq;
          dev :=
          Some
            {
              irq = irq;
              base_addr = ioaddr;
              dev_addr = Array.init 6 (fun i -> saprom.(i));
              tx_start_page = neSM_START_PG;
              rx_start_page = neSM_START_PG + tx_PAGES;
              stop_page = neSM_STOP_PG;
            };
          ns8390_init ();
          set_fdx ioaddr;
          kprintf "Device successfully registered (%2.2X:%2.2X:%2.2X:%2.2X:%2.2X:%2.2X).\n" saprom.(0) saprom.(1) saprom.(2) saprom.(3) saprom.(4) saprom.(5)
  with
      Invalid_argument s ->
        unit.Pci.acquired <- false;
        kprintf "%s\n" s;
        kprintf "Registration failed.\n"

(** A packet header. *)
type e8390_pkt_hdr =
    {
      status : int; (* status *)
      next : int; (* pointer to the next packet *)
      count : int; (* header + packet length in bytes *)
    }

(* sizeof(e8390_pkt_hdr) *)
let e8390_pkt_hdr_len = 4
    
(** Grab the 8390 specific header. Similar to the ne2k_block_input routine, but
  * we don't need to be concerned with ring wrap as the header will be at
  * the start of a page, so we optimize accordingly. *)
let get_8390_hdr ring_page =
  let dev = get_dev () in
    Funk.outb (dev.base_addr + ne_CMD) (e8390_NODMA + e8390_PAGE0 + e8390_START);
    Funk.outb (dev.base_addr + en0_RCNTLO) e8390_pkt_hdr_len;
    Funk.outb (dev.base_addr + en0_RCNTHI) 0;
    Funk.outb (dev.base_addr + en0_RSARLO) 0; (* On page boundary *)
    Funk.outb (dev.base_addr + en0_RSARHI) ring_page;
    Funk.outb (dev.base_addr + ne_CMD) (e8390_RREAD + e8390_START);
    let b0 = Funk.inw (dev.base_addr + ne_DATAPORT) in
    let b1 = Funk.inw (dev.base_addr + ne_DATAPORT) in
      Funk.outb (dev.base_addr + en0_ISR) enISR_RDC;
      {
        status = b0 lsr 8;
        next = b0 land 0xff;
        count = b1;
      }

(** Block input and output, similar to the Crynwr packet driver. If you
  * are porting to a new ethercard, look at the packet driver source for hints.
  * The NEx000 doesn't share the on-board packet memory -- you have to put
  * the packet out through the "remote DMA" dataport using outb. *)
let ne2k_block_input count ring_offset =
  let dev = get_dev () in
  let buf = String.create count in
    Funk.outb (dev.base_addr + ne_CMD) (e8390_NODMA + e8390_PAGE0 + e8390_START);
    Funk.outb (dev.base_addr + en0_RCNTLO) (count land 0xff);
    Funk.outb (dev.base_addr + en0_RCNTHI) (count lsr 8);
    Funk.outb (dev.base_addr + en0_RSARLO) (ring_offset land 0xff);
    Funk.outb (dev.base_addr + en0_RSARHI) (ring_offset lsr 8);
    Funk.outb (dev.base_addr + ne_CMD) (e8390_RREAD + e8390_START);
    for i = 0 to (count / 2) - 1
    do
      (* TODO: use insw? *)
      let w = Funk.inw (dev.base_addr + ne_DATAPORT) in
        buf.[2 * i] <- char_of_int (w lsr 8);
        buf.[2 * i + 1] <- char_of_int (w land 0xff);
    done;
    if count mod 2 = 1 then
      buf.[count - 1] <- char_of_int (Funk.inb (dev.base_addr + ne_DATAPORT));
    Funk.outb (dev.base_addr + en0_ISR) enISR_RDC; (* Ack intr. *)
    buf

exception Nothing_to_read
exception Invalid_packet

(* TODO: get up to 10 packets ? *)
let block_input () =
  let dev = get_dev () in
  let ans = ref "" in
    (* Get the rx page (incoming packet pointer). *)
    Funk.outb_p (dev.base_addr + e8390_CMD) (e8390_NODMA + e8390_PAGE1);
    let rxing_page = Funk.inb_p (dev.base_addr + en1_CURPAG) in
      Funk.outb_p (dev.base_addr + e8390_CMD) (e8390_NODMA + e8390_PAGE0);
      (* Remove one frame from the ring.  Boundary is always a page behind. *)
      let this_frame = Funk.inb_p (dev.base_addr + en0_BOUNDARY) + 1 in
      let this_frame = if this_frame >= dev.stop_page then dev.rx_start_page else this_frame in
        if this_frame = rxing_page then raise Nothing_to_read;
        let current_offset = this_frame lsl 8 in
        let hdr = get_8390_hdr this_frame in
        let pkt_len = hdr.count - e8390_pkt_hdr_len in
        let pkt_stat = hdr.status in
        let next_frame = this_frame + 1 + ((pkt_len + 4) lsr 8) in
          kprintf "header stt: %d, next: %d, cnt: %d\n%!" hdr.status hdr.next hdr.count;
          (* TODO: check for bogosity *)
          if pkt_len < 60 || pkt_len > 1518 then
            (
              kprintf "Invalid packet size.\n%!";
              raise Invalid_packet
            );
          if pkt_stat land 0x0f <> enRSR_RXOK then
            (
              kprintf "Bogus packet.\n%!";
              raise Invalid_packet
            );
          ans := ne2k_block_input pkt_len (current_offset + e8390_pkt_hdr_len);
          Funk.outb_p (dev.base_addr + en0_BOUNDARY) (hdr.next - 1);
          (* We used to also ack ENISR_OVER here, but that would sometimes mask
           * a real overrun, leaving the 8390 in a stopped state with rec'vr off. *)
          Funk.outb_p (dev.base_addr + en0_ISR) (enISR_RX + enISR_RX_ERR);
          !ans

let poll_packets () =
  let dev = get_dev () in
    while true
    do
      Irq.wait dev.irq;
      try
        kprintf "Packet received\n%!";
        ignore (block_input ())
      with
        | _ -> ()
    done

(** Main driver initialization routine. *)
let init () =
  kprintf "Initializing Ne2k driver\n";
  Pci.register_driver vendor device
    (fun u ->
       probe u;
       if u.Pci.acquired then
         (
           ignore (KThread.create poll_packets ());
           kprintf "Polling for packets.\n%!"
         )
    )

let test () =
  kprintf "rcvd: \"%s\"\n%!" (block_input ())
