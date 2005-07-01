(**
  * Functions for accessing to floppies.
  *
  * @author Samuel Mimram
  **)

(* Much inspired from the FreeDOS32 Floppy Driver by Salvo Isaja (under GPL). *)

let kprintf f = Funk.kprintf "Floppy" f

exception Timeout
exception Invalid_track
exception Seek_error
exception Drive_not_available
(** There is no disk in the drive. *)
exception No_disk
exception Fdc_error
                  
(** A Cylinder/Head/Sector address. *)
type chs =
    {
      c : int; (** cylinder *)
      h : int; (** head *)
      s : int; (** sector *)
    }
                  
(** The format of a floppy disk. *)
type fmt =
    {
      tracks : int; (** number of tracks *)
      sec_per_trk : int; (** number of sectors per track *)
      rate : int; (** sector size and data transfer rate *)
      sr_hut : int; (** Step Rate and Head Unload Time *)
      descr : string; (** description of the format *)
    }

(** Parameters used to handle a floppy drive. *)
type drive_params =
    {
      cmos_type : int; (** drive type read from CMOS *)
      hlt : int; (** Head Load Time (in SPECIFY format) *) 
      drive_type : string;
    }

type drive =
    {
      dp : drive_params; (** parameters for this drive *)
      mutable fmt : fmt; (** format of the current floppy disk *)
      mutable track : int; (** current floppy track sought *)
      mutable changed : bool; (** disk has been changed during the last command *)
    }

type controller =
    {
      base_port : int;
      result : int array; (** last result bytes returned *)
      mutable result_len : int; (** number of last result bytes returned *)
      mutable drive : drive array;
      mutable dor : int; (** reflects the Digital Output Register *)
      mutable sr0 : int; (** status Register 0 after a sense interrupt *)
    }

let primary_fdc =
  {
    base_port = 0x3f0;
    result = Array.make 7 0;
    result_len = 0;
    drive = [||];
    dor = 0;
    sr0 = 0;
  }
 
type fdc_transfer = Fdc_read | Fdc_write

(** Geometry and other format specifications for floppy disks *)
let floppy_formats =
  [|
    (* SIZE SPT HD TRK STR GAP3 RATE SRHUT GAP3F NAME NR DESCRIPTION *)
    0,     0, 0,  0, 0, 0x00, 0x00, 0x00, 0x00, "";  (*  0 no testing *)
    720,   9, 2, 40, 0, 0x2A, 0x02, 0xDF, 0x50, "d360"; (*  1 360KB PC *)
    2400, 15, 2, 80, 0, 0x1B, 0x00, 0xDF, 0x54, "h1200"; (*  2 1.2MB AT *)
    720,   9, 1, 80, 0, 0x2A, 0x02, 0xDF, 0x50, "D360"; (*  3 360KB SS 3.5'' *)
    1440,  9, 2, 80, 0, 0x2A, 0x02, 0xDF, 0x50, "D720"; (* 4 720KB 3.5'' *)
    720,   9, 2, 40, 1, 0x23, 0x01, 0xDF, 0x50, "h360"; (* 5 360KB AT *)
    1440,  9, 2, 80, 0, 0x23, 0x01, 0xDF, 0x50, "h720"; (* 6 720KB AT *)
    (* 2880, 18, 2, 80, 0, 0x1B, 0x00, 0xDF, 0x6C, "H1440"; (* 7 1.44MB 3.5'' *)*)
    2880, 18, 2, 80, 0, 0x1B, 0x00, 0xCF, 0x6C, "H1440"; (* 7 1.44MB 3.5'' *)
    5760, 36, 2, 80, 0, 0x1B, 0x43, 0xAF, 0x54, "E2880"; (* 8 2.88MB 3.5'' *)
    6240, 39, 2, 80, 0, 0x1B, 0x43, 0xAF, 0x28, "E3120"; (* 9 3.12MB 3.5'' *)

    2880, 18, 2, 80, 0, 0x25, 0x00, 0xDF, 0x02, "h1440"; (* 10 1.44MB 5.25'' *)
    3360, 21, 2, 80, 0, 0x1C, 0x00, 0xCF, 0x0C, "H1680"; (* 11 1.68MB 3.5'' *)
    820,  10, 2, 41, 1, 0x25, 0x01, 0xDF, 0x2E, "h410"; (* 12 410KB 5.25'' *)
    1640, 10, 2, 82, 0, 0x25, 0x02, 0xDF, 0x2E, "H820"; (* 13 820KB 3.5'' *)
    2952, 18, 2, 82, 0, 0x25, 0x00, 0xDF, 0x02, "h1476"; (* 14 1.48MB 5.25'' *)
    3444, 21, 2, 82, 0, 0x25, 0x00, 0xDF, 0x0C, "H1722"; (* 15 1.72MB 3.5'' *)
    840,  10, 2, 42, 1, 0x25, 0x01, 0xDF, 0x2E, "h420"; (* 16 420KB 5.25'' *)
    1660, 10, 2, 83, 0, 0x25, 0x02, 0xDF, 0x2E, "H830"; (* 17 830KB 3.5'' *)
    2988, 18, 2, 83, 0, 0x25, 0x00, 0xDF, 0x02, "h1494"; (* 18 1.49MB 5.25'' *)
    3486, 21, 2, 83, 0, 0x25, 0x00, 0xDF, 0x0C, "H1743"; (* 19 1.74MB 3.5'' *)

    1760, 11, 2, 80, 0, 0x1C, 0x09, 0xCF, 0x00, "h880"; (* 20 880KB 5.25'' *)
    2080, 13, 2, 80, 0, 0x1C, 0x01, 0xCF, 0x00, "D1040"; (* 21 1.04MB 3.5'' *)
    2240, 14, 2, 80, 0, 0x1C, 0x19, 0xCF, 0x00, "D1120"; (* 22 1.12MB 3.5'' *)
    3200, 20, 2, 80, 0, 0x1C, 0x20, 0xCF, 0x2C, "h1600"; (* 23 1.6MB 5.25'' *)
    3520, 22, 2, 80, 0, 0x1C, 0x08, 0xCF, 0x2e, "H1760"; (* 24 1.76MB 3.5'' *)
    3840, 24, 2, 80, 0, 0x1C, 0x20, 0xCF, 0x00, "H1920"; (* 25 1.92MB 3.5'' *)
    6400, 40, 2, 80, 0, 0x25, 0x5B, 0xCF, 0x00, "E3200"; (* 26 3.20MB 3.5'' *)
    7040, 44, 2, 80, 0, 0x25, 0x5B, 0xCF, 0x00, "E3520"; (* 27 3.52MB 3.5'' *)
    7680, 48, 2, 80, 0, 0x25, 0x63, 0xCF, 0x00, "E3840"; (* 28 3.84MB 3.5'' *)

    3680, 23, 2, 80, 0, 0x1C, 0x10, 0xCF, 0x00, "H1840"; (* 29 1.84MB 3.5'' *)
    1600, 10, 2, 80, 0, 0x25, 0x02, 0xDF, 0x2E, "D800"; (* 30 800KB 3.5'' *)
    3200, 20, 2, 80, 0, 0x1C, 0x00, 0xCF, 0x2C, "H1600"; (* 31 1.6MB 3.5'' *)
  |]

(** Parameters to manage a floppy disk drive. *)
(* Head load time is 16 ms for all drives except 2880 KiB, that have 15 ms. *)
let default_drive_params =
  [|
   (* T HLT SPUP SPDN SEL INTT AUTODETECT FORMATS NAT NAME *)
    0,  0, 1000, 3000, 20, 3000, [7; 4; 8; 2; 1; 5; 3; 10], 0, "unknown";
    1,  4, 1000, 3000, 20, 3000, [1; 0; 0; 0; 0; 0; 0; 0], 1, "5.25\" DD, 360 KiB";
    2,  8,  400, 3000, 20, 3000, [2; 5; 6; 23; 10; 20; 12; 0], 2, "5.25\" HD, 1200 KiB";
    3,  4, 1000, 3000, 20, 3000, [4; 22; 21; 30; 3; 0; 0; 0], 4, "3.5\" DD, 720 KiB";
(*    4,  1,  400, 3000, 20, 1500, [7; 4; 25; 22; 31; 21; 29; 11], 7, "3.5\" HD, 1440 KiB"; *)
    4,  8,  400, 3000, 20, 1500, [7; 4; 25; 22; 31; 21; 29; 11], 7, "3.5\" HD, 1440 KiB";
    5, 15,  400, 3000, 20, 3000, [7; 8; 4; 25; 28; 22; 31; 21], 8, "3.5\" ED, 2880 KiB AMI";
    6, 15,  400, 3000, 20, 3000, [7; 8; 4; 25; 28; 22; 31; 21], 8, "3.5\" ED, 2880 KiB";
  |]

(** The IRQ of the fdc. *)
let fdc_irq = 6

(** Was the irq signalled? *)
(* TODO: mutex!!! *)
let irq_signalled = ref false

(** RW: Digital Output Register *)
let fdc_dor = 0x02
(** R: Main Status Register. *)
let fdc_msr = 0x04
(** W: Data Rate Select register. *)
let fdc_drs = 0x04
(** RW: Data register. *)
let fdc_data = 0x05
(** R : Digital Input Register. *)
let fdc_dir = 0x07
(** W : Configuration Control Register. *)
let fdc_ccr = 0x07

(* Command bytes (these are NEC765 commands + options such as MFM, etc). *)
(** Specify drive timings. *)
let cmd_specify = 0x03
(** Recalibrate. *)
let cmd_recal = 0x07
(** Sense interrupt status. *)
let cmd_sense_int = 0x08
(** Seek track. *)
let cmd_seek = 0x0f
(** Get FDC version. *)
let cmd_version = 0x10
(** Configure. *)
let cmd_config = 0x13
(** Read sector id. *)
let cmd_read_id = 0x4a
(** Format track (+ MFM). *)
let cmd_format = 0x4d
(** Write data (+ MT, MFM). *)
let cmd_write = 0xc5
(** Read data (+ MT, MFM, SK). *)
let cmd_read = 0xe6
                 
(** Number of the DMA channel for floppy transfer. *)
let dma_channel = 2
                    
(** Send a byte to the controller FIFO. *)
let sendbyte base_port byte =
  let tmo = ref 0 in
  let loop = ref true in
    while !loop && !tmo < 127
    do
      incr tmo;
      let msr = Funk.inb (base_port + fdc_msr) in
        if ((msr land 0xc0) = 0x80) then
          (
            Funk.outb (base_port + fdc_data) byte;
            loop := false
          );
        ignore (Funk.inb 0x80); (* delay *)
    done;
    if !loop then
      raise Timeout

(** Get a byte from the controller FIFO. *)
let getbyte base_port =
  let tmo = ref 0 in
  let ans = ref None in
    while !ans = None && !tmo < 127
    do
      incr tmo;
      let msr = Funk.inb (base_port + fdc_msr) in
        if ((msr land 0xd0) = 0xd0) then
          ans := Some (Funk.inb (base_port + fdc_data));
        ignore (Funk.inb 0x80) (* delay *)
    done;
    match !ans with
      | Some b -> b
      | None -> raise Timeout

let wait_fdc fdc =
  (* TODO: timeout *)
  while not !irq_signalled do KThread.yield () done;
  fdc.result_len <- 0;
  while (fdc.result_len < 7 && (Funk.inb (fdc.base_port + fdc_msr)) land 0x10 <> 0)
  do
    fdc.result.(fdc.result_len) <- Funk.inb fdc.base_port;
    fdc.result_len <- fdc.result_len + 1
  done;
  irq_signalled := false

let is_motor_on fdc drive =
  fdc.dor land (1 lsl (drive + 4)) <> 0

(** Turn the motor off. *)
let motor_off fdc drive =
  if is_motor_on fdc drive then
    (
      fdc.dor <- fdc.dor land lnot (1 lsl (drive + 4));
      Funk.outb (fdc.base_port + fdc_dor) fdc.dor;
    )

(** Turn the motor on. *)
let motor_on fdc drive =
  if not (is_motor_on fdc drive) then
    (
      fdc.dor <- fdc.dor lor (1 lsl (drive + 4));
      fdc.dor <- (fdc.dor land 0xfc) lor drive;
      Funk.outb (fdc.base_port + fdc_dor) fdc.dor
    )
    (* TODO: wait for completion (sleep?). *)

(** Was there a disk change? *)
let disk_changed fdc drive =
  motor_on fdc drive;
  let ch = (Funk.inb (fdc.base_port + fdc_dir) land 0x80 <> 0) in
    motor_off fdc drive;
    ch || fdc.drive.(drive).changed

(** Recalibrates a drive (seek head to track 0).
  * Since the RECALIBRATE command sends up to 79 pulses to the head,
  * this function issues as many RECALIBRATE as needed.
  * The drive is supposed to be selected (motor on). *)
let recalibrate fdc drive =
  kprintf "Recalibrating...\n%!";  
  for k = 0 to 12
  do
        sendbyte fdc.base_port cmd_recal;
        sendbyte fdc.base_port drive;
        wait_fdc fdc; 
        (* Send a "sense interrupt status" command. *)
        sendbyte fdc.base_port cmd_sense_int;
        fdc.sr0 <- getbyte fdc.base_port;
        fdc.drive.(drive).track <- getbyte fdc.base_port;
        (* Exit if Unit Check is not set *)
        (* if fdc.sr0 land 0x10 then raise Fdc_error *)
   done;
   kprintf "Calibration result on drive %u: SR0=%02x, Track=%u\n%!" drive fdc.sr0 fdc.drive.(drive).track

(** Seek a drive to the specified track.
  * The drive is supposed to be selected (motor on). *)
let seek fdc drive track =
  if fdc.drive.(drive).track <> track then
    (
      if track >= fdc.drive.(drive).fmt.tracks then
        raise Invalid_track;
      sendbyte fdc.base_port cmd_seek;
      sendbyte fdc.base_port drive;
      sendbyte fdc.base_port track;
      wait_fdc fdc;
      sendbyte fdc.base_port cmd_sense_int;
      fdc.sr0 <- getbyte fdc.base_port;
      fdc.drive.(drive).track <- getbyte fdc.base_port;
      if fdc.sr0 <> 0x20 + drive || fdc.drive.(drive).track <> track then
        raise Seek_error;
      kprintf "Seek result on drive %u: SR0=%02x, Track=%u\n%!" drive fdc.sr0 fdc.drive.(drive).track
    )

(** Gets the position of the heads by reading the next sector identifier.
  * The drive is supposed to be selected (motor on). *)
let read_id fdc drive =
  sendbyte fdc.base_port cmd_read_id;
  sendbyte fdc.base_port drive;
  wait_fdc fdc;
  if fdc.result.(0) land 0xc0 <> 0 then raise Fdc_error;
  {
    c = fdc.result.(3);
    h = fdc.result.(4);
    s = fdc.result.(5);
  }

(** Program data rate and specify drive timings using the SPECIFY command. *)
let specify fdc drive =
  Funk.outb (fdc.base_port + fdc_ccr) fdc.drive.(drive).fmt.rate;
  sendbyte fdc.base_port cmd_specify;
  sendbyte fdc.base_port fdc.drive.(drive).fmt.sr_hut;
  sendbyte fdc.base_port (fdc.drive.(drive).dp.hlt lsl 1) (* Always DMA. *)

(** Reset the controller to a known state. *)
let reset_fdc fdc =
  Funk.outb (fdc.base_port + fdc_dor) 0; (* Stop the motor and disable IRQ/DMA. *)
  Funk.outb (fdc.base_port + fdc_dor) 0x0c; (* Re-enable IRQ/DMA and release reset. *)
  fdc.dor <- 0x0c;
  wait_fdc fdc;
  (* FDC specs say to sense interrupt status four times *)
  for k = 0 to (Array.length (fdc.drive)) - 1
  do
    (* Send a "sense interrupt status" command *)
    sendbyte fdc.base_port cmd_sense_int;
    fdc.sr0 <- getbyte fdc.base_port;
    fdc.drive.(k).track <- getbyte fdc.base_port
  done

(** Get a drive to a known state. *)
let reset_drive fdc drive =
  if fdc.drive.(drive).dp.cmos_type <> 0 then
    (
      fdc.drive.(drive).changed <- false;
      fdc.drive.(drive).track <- 0;
      specify fdc drive;
      motor_on fdc drive;
      seek fdc drive 5;
      recalibrate fdc drive;
      motor_off fdc drive;
      fdc.drive.(drive).changed <- true
    )

(** Transfer sectors between the disk and the DMA buffer.
  * The drive is supposed to be selected (motor on). *)
let xfer fdc drive chs buffer nb_sectors rw =
  (* TODO: wait for motor to spin quickly enough *)
  Dma.xfer dma_channel buffer (512 * nb_sectors) (rw = Fdc_write)
    (fun () ->
       sendbyte fdc.base_port (if rw = Fdc_write then cmd_write else cmd_read);
       sendbyte fdc.base_port ((chs.h lsl 2) lor drive);
       sendbyte fdc.base_port chs.c;
       sendbyte fdc.base_port chs.h;
       sendbyte fdc.base_port chs.s;
       sendbyte fdc.base_port 2; (* 512 bytes per sector. *)
       sendbyte fdc.base_port fdc.drive.(drive).fmt.sec_per_trk;
       sendbyte fdc.base_port 0xff; (*  DTL (bytes to transfer) = unused. *)
       wait_fdc fdc;
       if fdc.result.(0) land 0xc0 <> 0 then raise Fdc_error
    )

(** Read sectors from the floppy, up to the end of the cylinder. *)
let read fdc drive chs buffer nb_sectors =
  if fdc.drive.(drive).dp.cmos_type = 0 then
    raise Drive_not_available;
  (* TODO: wait until the drive is not busy anymore *)
  motor_on fdc drive;
  specify fdc drive;
  (* TODO: try multiple (3) times. *)
  (* Move head to right track. *)
  seek fdc drive chs.c;
  if Funk.inb (fdc.base_port + fdc_dir) land 0x80 <> 0 then
    raise No_disk;
  xfer fdc drive chs buffer nb_sectors Fdc_read

(* TODO:
 * - write
 * - read / write whole cylinders
 *)

let get_default_drive cmos_type =
  {
    dp =
      (
        let t, hlt, spup, spdn, sel, intt, afmts, nat, name = default_drive_params.(cmos_type) in (* TODO: check for Out_of_bounds *)
          {
            cmos_type = cmos_type;
            hlt = hlt;
            drive_type = name;
          }
      );
    fmt = (* TODO: check the format *)
      (
        let size, spt, hd, trk, str, gap3, rate, sr_hut, gap3f, name = floppy_formats.(7) in
          {
            tracks = trk;
            sec_per_trk = spt;
            rate = rate;
            sr_hut = sr_hut;
            descr = name;
          }
      );
    track = 0;
    changed = true;
  }

let irq_handler () =
  while true
  do
    Irq.wait 6;
    irq_signalled := true
  done

let init () =
  try
    ignore (KThread.create irq_handler ());
    kprintf "Polling for IRQ.\n%!";
    (* Read floppy drives types from CMOS memory (up to two drives).
     * They are supposed to belong to the primary FDC. *)
    Funk.outb 0x70 0x10;
    let k = Funk.inb 0x71 in
    let cmos_drive0 = (k lsr 4) land 0xf in
    let cmos_drive1 = k land 0xf in
      if cmos_drive0 <> 0 then
        primary_fdc.drive <- [|get_default_drive cmos_drive0|];
      if cmos_drive1 <> 0 then
        primary_fdc.drive <- [|primary_fdc.drive.(0); get_default_drive cmos_drive1|];
      (
        sendbyte primary_fdc.base_port cmd_version;
        match getbyte primary_fdc.base_port with
          | 0x80 -> kprintf "NEC765 FDC found.\n%!"
          | 0x90 -> kprintf "Enhanced FDC found.\n%!"
          | _ -> kprintf "FDC not found.\n%!" (* TODO: stop here *)
      );
      for i = 0 to (Array.length primary_fdc.drive) - 1
      do
        kprintf "Drive %d detected: %s.\n%!" i primary_fdc.drive.(i).dp.drive_type
      done
  with
    | Timeout -> kprintf "Timeout while detecting FDC.\n%!"

let shutdown () =
  Funk.outb (primary_fdc.base_port + fdc_dor) 0x0c

let test () =
  Printf.printf "Resetting FDC... %!";
  reset_fdc primary_fdc;
  Printf.printf "ok\n%!";
  Printf.printf "Resetting drive... %!";
  reset_drive primary_fdc 0;
  Printf.printf "ok\n%!";
  Printf.printf "Spinning motor up... %!";
  motor_on primary_fdc 0;
  Printf.printf "ok\n%!";
  let buf = String.create 512 in
    read primary_fdc 0 {c = 0; h = 0; s = 0} buf 1;
    for i = 0 to 251
    do
      Printf.printf "%0x " (int_of_char buf.[i])
    done
