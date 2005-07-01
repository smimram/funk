(* pci.mli [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : pci interface
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

(** A resource of a PCI unit. *)
type resource = IO_Ports of int32 | IO_Memory of int32

(** A PCI unit *)
type pci_unit =
    {
      bus : int; (** bus number *)
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

(** Scan the PCI bus. *)
val scan : unit -> unit

(** Get a list of PCI units. You should have called [scan] before. *)
val get_units : unit -> pci_unit list
  
(**
  * This routine is called by driver to register their devices. 
  * Warning: there might be a race condition with the acquired field... *)
val register_driver : int -> int -> (pci_unit -> unit) -> unit
