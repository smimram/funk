(* vfs_defs.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : virtual filesystem constants
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
  * Virtual File-system Constants.
  *
  * @author Brice Goglin
  **)

(* Macros defining symbolic file permissions *)

let s_ISUID = 0o4000
let s_ISGID = 0o2000
let s_ISVTX = 0o1000
let s_IRUSR = 0o0400
let s_IWUSR = 0o0200
let s_IXUSR = 0o0100
let s_IRGRP = 0o0040
let s_IWGRP = 0o0020
let s_IXGRP = 0o0010
let s_IROTH = 0o0004
let s_IWOTH = 0o0002
let s_IXOTH = 0o0001

let s_IRWXU = s_IRUSR lor s_IWUSR lor s_IXUSR
let s_IRWXG = s_IRGRP lor s_IWGRP lor s_IXGRP
let s_IRWXO = s_IROTH lor s_IWOTH lor s_IXOTH

let s_IRUGO = s_IRUSR lor s_IRGRP lor s_IROTH
let s_IWUGO = s_IWUSR lor s_IWGRP lor s_IWOTH
let s_IXUGO = s_IXUSR lor s_IXGRP lor s_IXOTH

let s_IACCESS = s_IRWXU lor s_IRWXG lor s_IRWXO
let s_IALL = s_IACCESS lor s_ISUID lor s_ISGID lor s_ISVTX

(* Various types *)

type path = string
type data = string

(* Attribute types *)

type ino = int
type file_perm = int
type uid = int
type gid = int
type size = int
type time = int

type file_kind =
   S_REG                       (** Regular file *)
 | S_DIR                       (** Directory *)
 | S_CHR                       (** Character device *)
 | S_BLK                       (** Block device *)
 | S_LNK                       (** Symbolic link *)
 | S_FIFO                      (** Named pipe *)
 | S_SOCK                      (** Socket *)

type stats =
  {
    st_ino : ino;               (** Inode number *)
    st_kind : file_kind;        (** Kind of the file *)
    st_perm : file_perm;        (** Access rights *)
    st_nlink : int;             (** Number of links *)
    st_uid : uid;               (** User id of the owner *)
    st_gid : gid;               (** Group ID of the file's group *)
    st_size : size;             (** Size in bytes *)
    st_atime : time;            (** Last access time *)
    st_ctime : time;            (** Last status change time *)
    st_mtime : time;            (** Last modification time *)
  }

(* FS types *)

type fsname = string

type statfs = {
  sf_type : fsname;
  sf_path : path;
  sf_blksize : size;
}

(* Various enums *)

type open_flag =
    O_RDONLY                    (** Open for reading *)
  | O_WRONLY                    (** Open for writing *)
  | O_RDWR                      (** Open for reading and writing *)
  | O_NONBLOCK                  (** Open in non-blocking mode *)
  | O_APPEND                    (** Open for append *)
  | O_CREAT                     (** Create if nonexistent *)
  | O_TRUNC                     (** Truncate to 0 length if existing *)
  | O_EXCL                      (** Fail if existing *)
  | O_NOCTTY                    (** Don't make this dev a controlling tty *)
  | O_DSYNC                     (** Writes complete as `Synchronised I/O data integrity completion' *)
  | O_SYNC                      (** Writes complete as `Synchronised I/O file integrity completion' *)
  | O_RSYNC                     (** Reads complete as writes (depending on O_SYNC/O_DSYNC) *)

type seek_command =
    SEEK_SET (** indicates positions relative to the beginning of the file *)
  | SEEK_CUR (** indicates positions relative to the current position *)
  | SEEK_END (** indicates positions relative to the end of the file *)

(* Internal types *)

type mount_point_id = int

type symlink_data = {
  target : path;
}

(* partial blocks are not allowed. *)
(* blocks at either fully allocated, or not allocated at all. *)
type file_block = data option

type file_data = {
  blocks : file_block DynArray.t;
}

type dir_data = {
  children : dentry DynArray.t;
}

and specific_data =
    File of file_data
  | Dir of dir_data
  | Symlink of symlink_data

and inode_common_data = {
  mntpnt : mount_point;
  ino : ino;
  mutable size : size;
  mutable mode : file_perm;
  mutable uid : uid;
  mutable gid : gid;
  mutable nlink : int;
  mutable atime : time;
  mutable ctime : time;
  mutable mtime : time;
}

and inode = {
  common : inode_common_data;
  data : specific_data;
}

and dentry = {
  name : path;
  mutable inode : inode;
  parent : dentry option;
  mutable mounted : dentry option;
}

and mount_point = {
  id : mount_point_id;
  root : dentry;
  blksize : size;
  mutable mounter : dentry option;
  fs : fs;
  mutable use : int;
}

and fs = {
  fs_name : fsname;
  fs_mount : unit -> mount_point;
  fs_umount : mount_point -> unit;
  fs_get_root_ino : mount_point -> ino;
  fs_get_new_ino : mount_point -> ino;
  fs_read_inode : mount_point -> ino -> size * file_perm * uid * gid * int * time * time * time;
  fs_lookup : mount_point -> ino -> path -> ino;
}

type open_file = {
  file_data : file_data;
  file_common : inode_common_data;
  file_flags : open_flag list;
  mutable file_seek : size;
}

type open_dir = {
  dir_data : dir_data;
  dir_common : inode_common_data;
  mutable dir_seek : size;
}

type wd_handle = dentry

(* Exceptions *)

exception ENOENT of path
type arg_of_EEXIST = EEXIST_Path of path | EEXIST_FsName of fsname
exception EEXIST of arg_of_EEXIST
exception ENOTDIR of path
exception EISDIR of path
exception ENOTEMPTY of path
type arg_of_EINVAL = EINVAL_Path of path | EINVAL_Open_flags of (open_flag list) | EINVAL_Mode
exception EINVAL of arg_of_EINVAL
exception EBADF of open_file
exception EXDEV of path * path
exception EBUSY of path
exception EACCES of path
exception ENODEV of fsname
exception EPERM of path
