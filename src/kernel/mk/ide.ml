(**
  * Functions for handling the ide bus and its devices.
  *
  * @author Samuel Mimram
  **)

(** Data register (read/write). *)
let offs_ata_data = 0x00

(** Error register (read).*)
let offs_ata_error = 0x01

(** Number of cylinders lsb (write). *)
let offs_ata_cyl_lsb = 0x04

(** Number of cylinders msb (write). *)
let offs_ata_cyl_msb = 0x05

(** Drive register (write). *)
let offs_ata_drive = 0x06
(** Bits that must be set. *)
let mask_ata_drive_ibm = 0xa0
(** Use LBA. *)
let mask_ata_drive_lba = 0x40
(** Use master. *)
let mask_ata_drive_master = 0x00
(** Use slave. *)
let mask_ata_drive_slave = 0x10

(** Status register (read). *)
let offs_ata_status = 0x07
(** Get a drive status. *)
let mask_ata_status_busy = 0x80

(** Command register (write). *)
let offs_ata_cmd = 0x07
               
(** Device control register (write). *)
let offs_ata_device_control = 0x206
(** Disable interrupts. *)
let mask_ata_ctrl_noints = 0x02
(** Reset controller. *)
let mask_ata_ctrl_reset = 0x04
(** 4 head bits. *)
let mask_ata_ctrl_4bits = 0x08

let mask_ata_identify = 0xec
let mask_atapi_identify = 0xa1

(** Type of a device. *)
type device_type = Dev_ATA | Dev_ATAPI

(** Informations about a device (e.g. geometry). *)
type device_info =
    {
      log_cyl : int; (** number of logical cylinders *)
      log_heads : int; (** number of logical heads *)
      log_sect : int; (** number of sectors per track *)
      blk_size : int; (** size of a block in bytes *)
      serial_nb : string; (** serial number *)
      model_nb : string; (** model number *)
      (* cap_lba : int option; (** LBA capacity, if it can do LBA *) *)
      nb_blk : int (** total number of blocks *)
    }
                               
type device =
    {
      dev_type : device_type; (** device type *)
      dev_info : device_info (** informations about the device *)
    }
                                                 
type controller_state = Ctrl_not_present | Ctrl_ready

(** A controller. *)
type controller =
    {
      io_addr : int;
      irq : int;
      mutable master_dev : device option;
      mutable slave_dev : device option
    }

(** Primary controller. *)
let prim_controller =
  {
    io_addr = 0x1f0;
    irq = 14;
    master_dev = None;
    slave_dev = None
  }

(** Secondary controller. *)
let sec_controller =
  {
    io_addr = 0x170;
    irq = 15;
    master_dev = None;
    slave_dev = None
  }

let send_ctrl base_addr v =
  Funk.outb_p (base_addr + offs_ata_device_control) v

let get_error base_addr =
  Funk.inb_p (base_addr + offs_ata_error)

let select_device base_addr master =
    Funk.outb_p
      (base_addr + offs_ata_drive)
      (mask_ata_drive_ibm lor (if master then mask_ata_drive_master else mask_ata_drive_slave))

exception No_drive

let get_device_info ctrl master dev_type =
  (* Disable interrupts. *)
  send_ctrl ctrl.io_addr (mask_ata_ctrl_noints land mask_ata_ctrl_4bits);
  (* Select the device. *)
  select_device ctrl.io_addr master;
  Funk.outb_p
    (ctrl.io_addr + offs_ata_cmd)
    (match dev_type with
       | Dev_ATA -> mask_ata_identify
       | Dev_ATAPI -> mask_atapi_identify);
  
  (* TODO: wait until not busy. *)

  let buf = Array.make 512 0 in
    for i = 0 to 254 
    do
      let w = Funk.inw ctrl.io_addr in
        buf.(2 * i) <- (w lsr 16) land 0xff;
        buf.(2 * i + 1) <- w land 0xff
    done;
    let log_cyl = (buf.(2) lsl 16) + buf.(3) in
    let log_heads = (buf.(6) lsl 16) + buf.(7) in
    let log_sect = (buf.(12) lsl 16) + buf.(13) in
    let sn = String.make 20 ' ' in
    let mn = String.make 40 ' ' in
      for i = 20 to 39
      do
        sn.[i - 20] <- char_of_int (buf.(i));
      done;
      for i = 54 to 93
      do
        mn.[i - 54] <- char_of_int (buf.(i));
      done;
      {
        log_cyl = log_cyl;
        log_heads = log_heads;
        log_sect = log_sect;
        blk_size = 512;
        serial_nb = sn;
        model_nb = mn;
        (* TODO: handle LBA *)
        nb_blk = log_cyl * log_heads * log_sect
      }

let init_controller ctrl master =
  let set_dev v =
    if master then
      ctrl.master_dev <- Some v
    else
      ctrl.slave_dev <- Some v
  in
  let get_status () =
    Funk.inb_p (ctrl.io_addr + offs_ata_status)
  in
  let select_drive = select_device ctrl.io_addr in
  let send_ctrl = send_ctrl ctrl.io_addr in
  let get_error () = get_error ctrl.io_addr in
    try
      (* Get the status of the drive. *)
      select_drive master;
      let status = get_status () in
        if (status land 0xf8 = 0xf8) || (status land mask_ata_status_busy = mask_ata_status_busy) then raise No_drive;
        (* Reset. *)
        send_ctrl (mask_ata_ctrl_noints lor mask_ata_ctrl_reset);
        send_ctrl mask_ata_ctrl_noints;
        ignore (get_error ());
        (* The folowing command produces "dma: command 0xf6 not supported" with
         * qemu. *)
        send_ctrl mask_ata_ctrl_4bits;

        (* TODO: stop some day! *)
        while (get_status () land mask_ata_status_busy = mask_ata_status_busy) do () done;

        (* Detect devices. *)
        if Funk.inb_p (ctrl.io_addr + offs_ata_cyl_lsb) = 0x14 &&
           Funk.inb_p (ctrl.io_addr + offs_ata_cyl_msb) = 0xeb then
          (
            set_dev
              {
                dev_type = Dev_ATAPI;
                dev_info = get_device_info ctrl master Dev_ATAPI
              };
            Funk.kprintf "IDE" "CD-ROM found.\n"
          )
        else (* It wasn't an ATAPI device, it is therefore a hard disk. *)
          (
            select_drive master;
            Funk.outb_p (ctrl.io_addr + offs_ata_error) 0x58;
            Funk.outb_p (ctrl.io_addr + offs_ata_cyl_lsb) 0xa5;
            if get_error () <> 0x58 &&
               Funk.inb_p (ctrl.io_addr + offs_ata_cyl_lsb) = 0xa5 then
              (
                set_dev
                  {
                    dev_type = Dev_ATA;
                    dev_info = get_device_info ctrl master Dev_ATA
                  };
                Funk.kprintf "IDE" "HD found.\n"
              )
            else
              raise No_drive
          );
        let dev =
          match
            (
              if master then
                ctrl.master_dev
              else
                ctrl.slave_dev
           ) with
           | Some dev -> dev
           | None -> assert false
        in
        let di = dev.dev_info in
          Funk.kprintf "IDE" "cyl: %d, hd: %d, sz: %d\n" di.log_cyl di.log_cyl di.log_sect
    with No_drive -> ()

(** Scan the IDE bus. *)
let scan () =
  init_controller prim_controller true;
  init_controller prim_controller false;
  init_controller sec_controller true;
  init_controller sec_controller false
