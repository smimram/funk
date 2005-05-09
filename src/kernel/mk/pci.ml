(* pci.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : pci module
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
  * Functions for hanling the PCI bus.
  *
  * @author Brice Goglin, Samuel Mimram
  **)

type resource = IO_Ports of int32 | IO_Memory of int32

type pci_unit =
    {
      bus : int;
      unt : int; (** unit *)
      funct : int; (** function *)
      device : int; (** device *)
      vendor : int; (** vendor *)
      baseclass : int; (** class *)
      subclass : int; (** subclass *)
      prog_if : int; (** programming interface *)
      resource : resource option array; (** I/O ports and memory addresses *)
      irq : int; (* IRQ *)
      mutable acquired : bool;
    }

let unit_compare u1 u2 =
  if u1.bus <> u2.bus then
    u1.bus - u2.bus
  else if u1.unt <> u2.unt then
    u1.unt - u2.unt
  else
    u1.funct - u2.funct

let pci_any_vendor = -1
and pci_any_device = -1

let unit_match unit vendor device =
  (vendor == pci_any_vendor || unit.vendor == vendor)
  && (device == pci_any_device || unit.device == device)

let units = ref ([] : pci_unit list)

(*
let string_of_vendor vendor =
  let i = ref 0 in
  let ans = ref "" in
    while !ans = "" && !i < Array.length Pci_ids.vendor
    do
      let vid, vn = Pci_ids.vendor.(!i) in
        if vid = vendor then
          ans := vn;
        incr i
    done;
    if !ans = "" then
      raise Not_found;
    !ans

let string_of_device vendor device =
  let i = ref 0 in
  let ans = ref "" in
    while !ans = "" && !i < Array.length Pci_ids.device
    do
      let vid, did, dn = Pci_ids.device.(!i) in
        if vid = vendor && did = device then
          ans := dn;
        incr i
    done;
    if !ans = "" then
      raise Not_found;
    !ans

let string_of_class cls =
  let i = ref 0 in
  let ans = ref "" in
    while !ans = "" && !i < Array.length Pci_ids.cls
    do
      let cid, cn = Pci_ids.cls.(!i) in
        if cid = cls then
          ans := cn;
        incr i
    done;
    if !ans = "" then
      raise Not_found;
    !ans

let string_of_subclass cls subcls =
  let i = ref 0 in
  let ans = ref "" in
    while !ans = "" && !i < Array.length Pci_ids.subclass
    do
      let cid, scid, scn = Pci_ids.subclass.(!i) in
        if cid = cls && scid = subcls then
          ans := scn;
        incr i
    done;
    if !ans = "" then
      raise Not_found;
    !ans
*)

(*
 Pci address (numbers are the number of bits, low-order first):
 int type     : 2;
 int reg      : 6;
 /** Id of the unit function */
 int func     : 3;
 /** Id of the PCI unit */
 int unit     : 5;
 /** Id of the PCI bus */
 int bus      : 8;
 int reserved : 7;
 int ecd      : 1;
 *)
type address =
    {
      mutable addr_type : int;
      mutable addr_reg : int;
      mutable addr_func : int; (** id of the unit function *)
      mutable addr_unit : int; (** id of the PCI unit *)
      mutable addr_bus : int; (** id of the PCI bus *)
      mutable addr_ecd : int
    }

let int32_of_address a =
  let addr =
    a.addr_type
    + (a.addr_reg lsl 2)
    + (a.addr_func lsl 8)
    + (a.addr_unit lsl 11)
    + (a.addr_bus lsl 16)
  in
    Int32.add (Int32.of_int addr) (Int32.shift_left (Int32.of_int a.addr_ecd) 31)

let read addr =
  Funk.outl 0xcf8 (int32_of_address addr);
  Funk.inl 0xcfc

let scan () =
  let addr =
    {
      addr_type = 0;
      addr_reg = 0;
      addr_func = 0;
      addr_unit = 0;
      addr_bus = 0;
      addr_ecd = 1;
    }
  in
    for bus = 0 to 5
    do
      addr.addr_bus <- bus;
      for u = 0 to 31 (* units *)
      do
        addr.addr_unit <- u;
        for f = 0 to 7 (* functions *)
        do
          addr.addr_func <- f;
          addr.addr_reg <- 0;
          let ans = read addr in
          let id = (Int32.to_int (Int32.shift_right ans 16)) land 0xffff in
          let manufacturer = (Int32.to_int ans) land 0xffff in
            if id <> 0xffff then (* if there is a device *)
              (
                addr.addr_reg <- 2;
                let ans = read addr in
                let base_class = (Int32.to_int (Int32.shift_right ans 24)) land 0xff in
                let ans = Int32.to_int ans in
                let sub_class = ((ans lsr 16) land 0xff) in
                let prog_if = ((ans lsr 8) land 0xff) in
                let revision = ans land 0xff in
                let resource = Array.make 6 None in
                  for ma = 0 to 5 (* Exported resources *)
                  do
                    addr.addr_reg <- 4 + ma;
                    let ans = read addr in
                      if (Int32.to_int ans) land 0x1 = 1 then
                        resource.(ma) <- Some (IO_Ports (Int32.logand ans (Int32.lognot Int32.one)))
                      else if ans <> Int32.zero then
                        resource.(ma) <- Some (IO_Memory (Int32.logand ans (Int32.lognot (Int32.of_string "0xf"))))
                  done;
                  addr.addr_reg <- 15;
                  let irq = Int32.to_int (read addr) land 0xff in
		  let unit =
		    {
                      bus = bus;
                      unt = u;
                      funct = f;
                      device = id;
                      vendor = manufacturer;
                      baseclass = base_class;
                      subclass = sub_class;
                      prog_if = prog_if;
                      resource = resource;
                      irq = irq;
		      acquired = false; (* initialized as free *)
                    } 
		  in units := List.merge unit_compare !units [unit]
              )    
        done
      done
    done;
    Funk.kprintf "PCI" "%d devices found.\n" (List.length !units)

let get_units () =
  !units
  
(* This routine is called by driver to register their devices. *)
(* Their might be a race condition with the acquired field... *)
let register_driver vendor device probe_func =
  let f unit =
    if not unit.acquired && unit_match unit vendor device
    then begin
      unit.acquired <- true;
      probe_func unit
    end
  in List.iter f !units
