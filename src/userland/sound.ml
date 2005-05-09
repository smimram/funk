(* sound.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : userland sound functions
 * copyright   : (C) 2005 by samuel mimram, nicolas guenot
 * email       : samuel.mimram@ens-lyon.org, nicolas.guenot@ens-lyon.org

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
  * Userland sound functions
  *
  * @author Brice Goglin
  **)

(* Speaker and Timer constants *)
let clock_tick_rate = 1193182
let timer_data_port = 0x42
let timer_control_port = 0x43
let timer_oscillator_cmd = 0xb6
let speaker_control_port = 0x61
let speaker_timer_driven_enable_cmd = 0x3
let speaker_timer_disable_cmd = 0xff lxor speaker_timer_driven_enable_cmd

(* Put Timer in oscillator mode *)
let oscillator_timer () =
  ignore (Funk.outb_p timer_control_port timer_oscillator_cmd)

(* Set Timer frequency *)
let set_timer_frequency freq =
  let count = clock_tick_rate/freq in
    ignore (Funk.outb_p timer_data_port (count land 0xff));
    ignore (Funk.outb_p timer_data_port ((count lsr 8) land 0xff))

(* Enable/disable the internal speaker in timer driven mode *)
let enable_speaker () =
  let status = Funk.inb_p speaker_control_port in
    ignore (Funk.outb_p speaker_control_port (status lor speaker_timer_driven_enable_cmd))
let disable_speaker () =
  let status = Funk.inb_p speaker_control_port in
    ignore (Funk.outb_p speaker_control_port (status land speaker_timer_disable_cmd))

(* Beep during delay milliseconds with frequency freq Hz *)
let beep freq delay =
  oscillator_timer ();
  set_timer_frequency freq;
  enable_speaker ();
  Funk.usleep (delay*1000);
  disable_speaker ()
