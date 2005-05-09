(* vfs.mli [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : virtual filesystem interface
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
  * Virtual File-system interface.
  *
  * @author Brice Goglin
  **)

(* File Access API *)

val init : Vfs_defs.fsname -> Vfs_defs.wd_handle

val change_cwd : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.wd_handle * Vfs_defs.path

val mount : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.fsname -> unit

val umount : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> unit

val movemount : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.path -> unit

val statfs : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.statfs

val mkdir : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.file_perm option -> unit

val rmdir : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> unit

val create : Vfs_defs.uid  -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.file_perm option -> unit

val link : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.path -> unit

val rename : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.path -> unit

val symlink : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.path -> unit

val unlink : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> unit

val stat : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.stats

val lstat : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.stats

val readlink : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.path

val open_file : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.open_flag list -> Vfs_defs.file_perm option -> Vfs_defs.open_file

val close_file : Vfs_defs.open_file -> unit

val read_file : Vfs_defs.open_file -> Vfs_defs.size -> Vfs_defs.size * Vfs_defs.data

val write_file : Vfs_defs.open_file -> Vfs_defs.data -> Vfs_defs.size -> Vfs_defs.size

val seek_file : Vfs_defs.open_file -> Vfs_defs.size -> Vfs_defs.seek_command -> Vfs_defs.size

val truncate_file : Vfs_defs.open_file -> Vfs_defs.size -> unit

val open_dir : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.open_dir

val close_dir : Vfs_defs.open_dir -> unit

val tell_dir : Vfs_defs.open_dir -> Vfs_defs.size

val seek_dir : Vfs_defs.open_dir -> Vfs_defs.size -> unit

val rewind_dir : Vfs_defs.open_dir -> unit

val read_dir : Vfs_defs.open_dir -> Vfs_defs.path

val chmod : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.file_perm -> unit

val chown : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.uid option -> Vfs_defs.gid option -> unit

val lchown : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> Vfs_defs.uid option -> Vfs_defs.gid option -> unit

val utime : Vfs_defs.uid -> Vfs_defs.gid -> Vfs_defs.wd_handle -> Vfs_defs.path -> (Vfs_defs.time * Vfs_defs.time) option -> unit

(* File System Specific API *)

val register_fs : Vfs_defs.fs -> unit

val get_mntpnt_id : unit -> Vfs_defs.mount_point_id

val current_time : unit -> Vfs_defs.time
