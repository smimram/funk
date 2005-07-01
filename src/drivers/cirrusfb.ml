(* cirrusfb.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : Cirrus Logic FrameBuffer driver module
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
  * Cirrus Logic FrameBuffer driver.
  *
  * @author Brice Goglin
  **)

(* Embedding message prefix. *)
let kprintf f = Funk.kprintf "CirrusFB" f

(* Vendor and device number of the PCI device
 * registered by this driver. *)
let vendor = 0x1013
and device = 0x00b8

(** Device probing routine.
  * Called for each PCI device matching vendor and device numbers. *)
let probe unit =
  kprintf "Trying to register one device...\n";
  try
    let display,regs =
      match (unit.Pci.resource.(0),unit.Pci.resource.(1)) with
	| (Some (Pci.IO_Ports ports), Some (Pci.IO_Memory mem)) -> mem,ports
	| (Some (Pci.IO_Memory mem), Some (Pci.IO_Ports ports)) -> mem,ports
	| (Some (Pci.IO_Memory mem), Some (Pci.IO_Memory mem2)) -> mem,mem2
	| _ -> raise (Invalid_argument "Wrong IO Resources found.")
    in
      kprintf "Display is at 0x%x, Registers are at 0x%x\n" (Int32.to_int display) (Int32.to_int regs);
      kprintf "Device successfully registered.\n"
  with
      Invalid_argument s ->
        unit.Pci.acquired <- false;
        kprintf "%s\n" s;
        kprintf "Registration failed.\n"

(** Main driver initialization routine. *)
let init () =
  kprintf "Initializing CirrusFB driver\n";
  Pci.register_driver vendor device probe
