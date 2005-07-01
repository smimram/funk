(**
  * Functions for handling DMA transfers.
  *
  * @author Samuel Mimram
  **)

(* TODO: nicer buffers *)
(* TODO: handle 16 bit xfers *)

(** The given channel number was invalid (it should be between 0 and 7). *)
exception Invalid_channel

type buffer = nativeint
  
external get_buffer : int -> buffer = "caml_funk_dma_get_buffer"

external string_of_buffer : buffer -> int -> string -> unit = "caml_funk_dma_string_of_buffer"

external free_buffer : buffer -> unit = "caml_funk_dma_free_buffer"
  
(** Page register corresponding to a channel. *)
let get_page_port chan =
  [| 0x87; 0x83; 0x81; 0x82; 0x8f; 0x8b; 0x89; 0x8a |].(chan)

(** Get the address port. *)
let get_addr_port chan =
  if chan < 4 then
    chan * 2
  else if chan < 8 then
    0xc0 + (chan - 4) * 4
  else
    raise Invalid_channel

(** Get the count port. *)
let get_count_port chan =
  if chan < 4 then
    chan * 2 + 1
  else if chan < 8 then
    0xc0 + (chan - 4) * 4 + 2
  else
    raise Invalid_channel
      
(**
  * [xfer channel buffer length read cmd]
  * sets up a DMA trasfer between a device and memory.
  * [channel] is the DMA channel and [page] is the page register of that 
  * If [read] is [true], then transfer will be from memory to device,
  * else from the device to memory.
  * [cmd] is a function which will be executed to initiate the transfer. *)
let xfer channel buffer length read cmd =
    (* Calculate DMA page and offset *)
  let buf = get_buffer length in
  let page = Nativeint.to_int (Nativeint.shift_right buf 16) in
  let offset = (Nativeint.to_int buf) land 0xffff in
  let length = length - 1 in (* with DMA, if you want k bytes, you ask for k - 1 *)
    Funk.cli (); (* Disable IRQs *)
    Funk.outb 0x0a (channel lor 4); (* Set channel mask bit *)
    Funk.outb 0x0c 0; (* Clear flip flop *)
    Funk.outb 0x0b ((if read then 0x48 else 0x44) + channel); (* Mode (write+single+r/w) *)
    Funk.outb (get_page_port channel) page; (* Page *)
    Funk.outb (get_addr_port channel) (offset land 0xff); (* Offset: low byte *)
    Funk.outb (get_addr_port channel) (offset lsr 8); (* Offset: high byte *)
    Funk.outb (get_count_port channel) (length land 0xff); (* Length: low byte *)
    Funk.outb (get_count_port channel) (length lsr 8); (* Length: high byte *)
    Funk.outb 0x0a channel; (* Clear DMA mask bit *)
    Funk.sti (); (* Enable IRQs *)
    cmd ();
    string_of_buffer buf length buffer;
    free_buffer buf
