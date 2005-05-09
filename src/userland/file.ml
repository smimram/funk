(* file.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : userland low-level file module
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
  * Userland low-level file functions.
  *
  * @author Brice Goglin
  **)

(*
 * These functions are Unix syscall-like functions.
 * They may raise exceptions.
 *)

(**
  * Attribute functions
  **)

let stat ctx path =
  Vfs.stat ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

let lstat ctx path =
  Vfs.lstat ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

let readlink ctx path =
  Vfs.readlink ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

let chmod ctx path perms =
  Vfs.chmod ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path perms

let chown ctx path new_uid new_gid =
  Vfs.chown ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path new_uid new_gid

let lchown ctx path new_uid new_gid =
  Vfs.lchown ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path new_uid new_gid

let utime ctx path times =
  Vfs.utime ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path times

(**
  * Filename functions
  **)

let mkdir ctx path mode =
  Vfs.mkdir ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path mode

let rmdir ctx path =
  Vfs.rmdir ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

let create ctx path mode =
  Vfs.create ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path mode

let link ctx oldpath newpath =
  Vfs.link ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle oldpath newpath

let rename ctx oldpath newpath =
  Vfs.rename ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle oldpath newpath

let symlink ctx newpath target =
  Vfs.symlink ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle newpath target

let unlink ctx path =
  Vfs.unlink ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

(**
  * Data access
  **)

let open_file ctx path flags mode =
  Vfs.open_file ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path flags mode

let close_file file =
  Vfs.close_file file

let truncate_file file length =
  Vfs.truncate_file file length

let seek_file file length command =
  Vfs.seek_file file length command

let read file length buffer offset_in_buffer =
  let read = ref 0
  in let loop = ref true
  in
    while !read < length && !loop do
      let size,data = Vfs.read_file file (length - !read)
      in
	if size = 0
	then loop := false (* end of file *)
	else begin
	  String.blit data 0 buffer (offset_in_buffer + !read) size;
	  read := !read + size
	end
    done;
    !read

let write file length buffer offset_in_buffer =
  let written = ref 0
  in let loop = ref true
  in
    while !written < length && !loop do
      let to_write = length - !written
      in let data = String.sub buffer (offset_in_buffer + !written) to_write
      in let size = Vfs.write_file file data to_write
      in
	if size = 0
	then loop := false (* an error occured ? *)
	else written := !written + size
    done;
    !written

(**
  * Directory traversal
  **)

let open_dir ctx path =
  Vfs.open_dir ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

let close_dir dir =
  Vfs.close_dir dir

let tell_dir dir =
  Vfs.tell_dir dir

let seek_dir dir seek =
  Vfs.seek_dir dir seek

let rewind_dir dir =
  Vfs.rewind_dir dir

let read_dir dir =
  Vfs.read_dir dir

(**
  * Mounting
  **)

let mount ctx path name =
  Vfs.mount ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path name

let umount ctx path =
  Vfs.umount ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path

let movemount ctx oldpath newpath =
  Vfs.movemount ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle oldpath newpath

let statfs ctx path =
  Vfs.statfs ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path
