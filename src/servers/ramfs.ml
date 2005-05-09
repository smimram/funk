(* ramfs.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : ram filesystem module
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
  * Ram File-system.
  *
  * @author Brice Goglin
  **)

open Vfs_defs

type ramfs_mount_point = {
  mutable last_ino : ino;
}

let ramfs_mntpnts = Hashtbl.create 16

let rec ramfs_fs = {
  fs_name = "ramfs";
  fs_mount = ramfs_mount;
  fs_umount = ramfs_umount;
  fs_get_root_ino = ramfs_get_root_ino;
  fs_get_new_ino = ramfs_get_new_ino;
  fs_read_inode = ramfs_read_inode;
  fs_lookup = ramfs_lookup;
}

and ramfs_get_root_ino mntpnt = 1

and ramfs_get_new_ino mntpnt =
  let ramfs_mntpnt = Hashtbl.find ramfs_mntpnts (Hashtbl.hash mntpnt.id)
  in
    ramfs_mntpnt.last_ino <- ramfs_mntpnt.last_ino + 1;
    ramfs_mntpnt.last_ino

and ramfs_read_inode mntpnt ino =
  assert (ino == 1);
  let time = Vfs.current_time ()
  in
    (
      0 (* size *),
      s_IRWXU lor s_IRUGO lor s_IXUGO (* mode *),
      0 (* uid *),
      0 (* gid *),
      2 (* nlink *),
      time (* atime *),
      time (* ctime *),
      time (* mtime *)
    )

and ramfs_lookup mntpnt ino path =
  let ramfs_mntpnt = Hashtbl.find ramfs_mntpnts (Hashtbl.hash mntpnt.id)
  in
    (* should do a real lookup and return the child *)
    ino

and ramfs_umount mntpnt =
  Hashtbl.remove ramfs_mntpnts (Hashtbl.hash mntpnt.id);
  Funk.kprintf "RamFS" "Stopping an instance of RAM File System\n"

and ramfs_mount () =
  let time = Vfs.current_time () in
  let rec mntpnt = {
    id = Vfs.get_mntpnt_id ();
    root = root_dentry;
    blksize = 4096;
    mounter = None;
    fs = ramfs_fs;
    use = 0;
  }
  and common = {
    mntpnt = mntpnt;
    ino = 1;
    size = 0;
    mode = s_IRWXU lor s_IRUGO lor s_IXUGO;
    uid = 0;
    gid = 0;
    nlink = 2;
    atime = time;
    ctime = time;
    mtime = time;
  }
  and root_inode = {
    common = common;
    data = Dir {
      children = DynArray.create ();
    }
  }
  and root_dentry = {
    name = "/"; (* should not be used *)
    inode = root_inode;
    parent = None;
    mounted = None;
  }

  in let ramfs_mntpnt = {
    last_ino = 1;
  }

  in
    Hashtbl.add ramfs_mntpnts (Hashtbl.hash mntpnt.id) ramfs_mntpnt;
    Funk.kprintf "RamFS" "Starting a new instance of RAM File System\n";
    mntpnt

let init () =
  try
    Vfs.register_fs ramfs_fs
  with
    | EEXIST(_) -> Funk.kprintf "RamFS" "Already registered\n"
