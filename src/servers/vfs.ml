(* vfs.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : virtual filesystem module
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
  * Virtual File-system.
  *
  * @author Brice Goglin
  **)

open Vfs_defs

(* Internal exceptions (used during lookup) *)

exception Dentry_not_found of dentry * path
exception Dentry_not_a_directory of dentry
exception Is_last_dentry of path

(* Root mount point *)

let root_mntpnt = ref None

(* Timing *)

let current_time () = Funk.gettimeofday ()

let notify_write_access common time =
  common.ctime <- time;
  common.mtime <- time

let notify_read_access common time =
  common.atime <- time

let notify_change_access common time =
  common.ctime <- time

(* Permissions *)

let is_in_group gid mygid =
  gid = mygid (* TODO: a user might be in several group *)

let is_owner dentry uid =
  let common = dentry.inode.common
  in uid = common.uid || uid = 0

let has_read_access dentry uid gid =
  let common = dentry.inode.common
  in (
      if uid = common.uid
      then common.mode land s_IRUSR
      else if is_in_group common.gid gid
      then common.mode land s_IRGRP
      else common.mode land s_IROTH
    ) != 0

let has_write_access dentry uid gid =
  let common = dentry.inode.common
  in (
      if uid = common.uid
      then common.mode land s_IWUSR
      else if is_in_group common.gid gid
      then common.mode land s_IWGRP
      else common.mode land s_IWOTH
    ) != 0

let has_exec_access dentry uid gid =
  let common = dentry.inode.common
  in (
      if uid = common.uid
      then common.mode land s_IXUSR
      else if is_in_group common.gid gid
      then common.mode land s_IXGRP
      else common.mode land s_IXOTH
    ) != 0

(* Directory contents and size *)

let get_dir_dentry dir_data index = DynArray.get dir_data.children index

let add_to_directory_size common len =
  common.size <- common.size + len

let remove_from_directory_size common len =
  common.size <- common.size - len

(* Mounter and mounted dentry *)

let rec get_mounted_dentry dentry =
  match dentry.mounted with
    | None -> dentry
    | Some mounted -> get_mounted_dentry mounted

let rec get_mounter_dentry dentry =
  if dentry.parent = None
  then
    match dentry.inode.common.mntpnt.mounter with
      | None -> dentry
      | Some mounter -> get_mounter_dentry mounter
  else dentry

let get_mounter_parent dentry =
  let mounter = get_mounter_dentry dentry
  in mounter.parent

(* Dentry lookup *)

let compare_dentry_name name dentry =
  dentry.name = name

let lookup_dentry parent_dentry name uid gid =
  if not (has_exec_access parent_dentry uid gid) then raise (EACCES parent_dentry.name);
  match parent_dentry.inode.data with
    | Dir x -> begin
	if name = "." then parent_dentry, -1
	else if name = ".." then
	  match get_mounter_parent parent_dentry with
	    | None -> parent_dentry, -1
	    | Some parent -> parent, -1
	else
	  try
	    let i = DynArray.index_of (compare_dentry_name name) x.children
	    in let dentry = get_dir_dentry x i
	    in (get_mounted_dentry dentry, i)
	  with
	      Not_found -> raise (Dentry_not_found (parent_dentry, name))
      end
    | _ -> raise (Dentry_not_a_directory parent_dentry)

(* Path lookup *)

let get_first_name_from_path path =
  let length = String.length path
  in
    try
      let endw =	String.index path '/'
      in let subname = String.sub path 0 endw
      in let noslash = ref (endw+1)
      in begin
	try
	  (* remove ending '/'. starting '/' are removed in lookup_parent/path *)
	  while String.get path !noslash = '/' do incr noslash done;
	  let subpath = String.sub path !noslash (length - !noslash)
	  in (subname,subpath)
	with
	    Invalid_argument(_) -> (subname,"")
	end
    with
	Not_found -> raise (Is_last_dentry path)

type lookup_flag =
    LOOKUP_PARENT (* return the parent of the target of the path *)
  | LOOKUP_FOLLOW (* traverse symlinks even at the end of the path *)

let rec generic_path_lookup start_dentry path flags uid gid =
  match start_dentry.inode.data with
    | Dir x -> begin
	let length = String.length path
	in
	  if length = 0 then
	    begin
	      (* use "." *)
	      if List.mem LOOKUP_PARENT flags
	      then start_dentry, "."
	      else
		let dentry,i = lookup_dentry start_dentry "." uid gid
		in dentry,""
	    end
	  else if String.get path 0 = '/' then
	    match !root_mntpnt with
	      | None -> assert false (* Vfs.init() has not been called ?! *)
	      | Some mntpnt ->
		  generic_path_lookup mntpnt.root (String.sub path 1 (length - 1)) flags uid gid
	  else
	    try
	      (* extract first subname *)
	      let subname, subpath = get_first_name_from_path path
	      in let dentry,i = lookup_dentry start_dentry subname uid gid
	      in generic_path_lookup dentry subpath flags uid gid
	    with
		Is_last_dentry(subname) -> (* "subname" *)
		  if List.mem LOOKUP_PARENT flags
		  then start_dentry, subname
		  else
		    let dentry,i = lookup_dentry start_dentry subname uid gid
		    in
		      match dentry.inode.data with
			| Symlink x ->
			    if List.mem LOOKUP_FOLLOW flags
			    then generic_path_lookup start_dentry x.target [LOOKUP_FOLLOW] uid gid
			    else dentry,""
			| _ -> dentry,""
      end
    | Symlink x -> begin
	match start_dentry.parent with
	  | None -> assert false
	  | Some parent ->
	      let dentry,subname = generic_path_lookup parent x.target [LOOKUP_FOLLOW] uid gid
	      in generic_path_lookup dentry path flags uid gid
      end
    | _ -> assert false

let lookup_path start_dentry path uid gid =
  try
    let dentry,subpath = generic_path_lookup start_dentry path [LOOKUP_FOLLOW] uid gid
    in dentry
  with
    | Dentry_not_found(_,_) -> raise (ENOENT path)
    | Dentry_not_a_directory(_) -> raise (ENOTDIR path)

let lookup_parent start_dentry path uid gid =
  try
    generic_path_lookup start_dentry path [LOOKUP_PARENT;LOOKUP_FOLLOW] uid gid
  with
    | Dentry_not_found(_,_) -> raise (ENOENT path)
    | Dentry_not_a_directory(_) -> raise (ENOTDIR path)

let lookup_path_nofollow start_dentry path uid gid =
  try
    let dentry,subpath = generic_path_lookup start_dentry path [] uid gid
    in dentry
  with
    | Dentry_not_found(_,_) -> raise (ENOENT path)
    | Dentry_not_a_directory(_) -> raise (ENOTDIR path)

(* Path manipulation *)

let gen_full_path dentry =
  let rec generic_gen_full_path dentry path =
    let mounter = get_mounter_dentry dentry
    in
      match mounter.parent with
        | Some parent -> generic_gen_full_path parent (mounter.name ^ "/" ^ path)
        | None -> "/" ^ path
  in generic_gen_full_path dentry ""

(* Path inclusion check *)

let rec is_ancestor dentry1 dentry2 =
    match get_mounter_parent dentry2 with
      | None -> dentry1 == dentry2
      | Some parent -> dentry1 == dentry2 or is_ancestor dentry1 parent

(* Creating entries *)

let new_inode_gid gid parent =
  if parent.inode.common.mode land s_ISGID = s_ISGID
  then parent.inode.common.gid
  else gid

let create_inode_common_data mntpnt mode uid gid size =
  let time = current_time () in {
      mntpnt = mntpnt;
      ino = mntpnt.fs.fs_get_new_ino mntpnt;
      size = size;
      mode = mode;
      nlink = 1;
      uid = uid;
      gid = gid;
      atime = time;
      ctime = time;
      mtime = time;
    }

let create_empty_dir mntpnt mode uid gid = {
  common = create_inode_common_data mntpnt mode uid gid 0;
  data = Dir {
    children = DynArray.create ();
  }
}

let create_file mntpnt mode uid gid = {
  common = create_inode_common_data mntpnt mode uid gid 0;
  data = File {
    blocks = DynArray.create ();
  }
}

let create_symlink mntpnt target uid gid = {
  common = create_inode_common_data mntpnt s_IACCESS uid gid (String.length target);
  data = Symlink {
    target = target;
  }
}

(* Linking dentries *)

let add_dentry_to_parent name inode parent_dentry =
  match parent_dentry.inode.data with
    | Dir x -> begin
	let dentry =
	  {
	    name = name;
	    inode = inode;
	    parent = Some parent_dentry;
	    mounted = None;
	  }
	in
	  DynArray.add x.children dentry;
	  dentry
      end
    | _ -> raise (Dentry_not_a_directory parent_dentry)

(* Unlinking dentries *)

let remove_from_parent_data data i =
  DynArray.delete data.children i

let remove_from_parent parent_dentry i =
  match parent_dentry.inode.data with
    | Dir x -> remove_from_parent_data x i
    | _ -> raise (Dentry_not_a_directory parent_dentry)

(* Blocks *)

let new_empty_block size =
  String.make size '\000'

let setup_file_blocks file length =
  let blksize = file.file_common.mntpnt.blksize
  in let data = file.file_data
  in let new_nblocks = (length + blksize - 1)/blksize
  in let old_nblocks = DynArray.length data.blocks
  in
    if new_nblocks <= 0 then
      DynArray.clear data.blocks
    else if new_nblocks > old_nblocks then
      begin
	(* add last blocks, but do not allocate them *)
	let new_blocks = DynArray.init (new_nblocks - old_nblocks) (fun x -> None)
	in DynArray.append new_blocks data.blocks
      end
    else
      begin
	(* delete last blocks *)
	DynArray.delete_range data.blocks new_nblocks (old_nblocks - new_nblocks);
	(* put 0's in the end of the last remaining block if it's been allocated *)
	match DynArray.get data.blocks (new_nblocks-1) with
	  | None -> ()
	  | Some x ->
	      let offset = length mod blksize in
		if offset > 0 then
		  String.fill x offset (blksize - offset) '\000'
      end

(***********************)
(* High level routines *)
(***********************)
(* All following function can only raise non-internal exceptions *)

(* Filename *)

let mkdir uid gid root path mode =
  let parent,name = lookup_parent root path uid gid
  in
    try
      ignore (lookup_dentry parent name uid gid);
      raise (EEXIST (EEXIST_Path path))
    with
	Dentry_not_found(_,_) ->
	  if not (has_write_access parent uid gid) then raise (EACCES path);
	  let mode = match mode with
	      None -> raise (EINVAL (EINVAL_Mode))
	    | Some mode -> mode
	  in let inode = create_empty_dir parent.inode.common.mntpnt mode uid
	      (new_inode_gid gid parent)
	  in
	    inode.common.nlink <- 2;
	    parent.inode.common.nlink <- parent.inode.common.nlink + 1;
	    notify_write_access parent.inode.common (current_time ());
	    let dentry = add_dentry_to_parent name inode parent
	    in add_to_directory_size parent.inode.common (String.length name)

let rmdir uid gid root path =
  let parent,name = lookup_parent root path uid gid
  in
    if not (has_write_access parent uid gid) then raise (EACCES path);
    match parent.inode.data with
    | Dir x -> begin
	let dentry,i =
	  try
	    lookup_dentry parent name uid gid
	  with
	      Dentry_not_found(_) -> raise (ENOENT path)
	in let inode = dentry.inode
	in match inode.data with
	  | Dir y ->
	      if i < 0 then raise (EINVAL (EINVAL_Path path))
	      else begin
		if DynArray.length y.children > 0 then
		  raise (ENOTEMPTY path);
		parent.inode.common.nlink <- parent.inode.common.nlink - 1;
		remove_from_parent_data x i;
		remove_from_directory_size parent.inode.common (String.length dentry.name);
		notify_write_access parent.inode.common (current_time ())
	      end
	  | _ -> raise (ENOTDIR path)
      end
    | _ -> raise (ENOENT path)

let create uid gid root path mode =
  let parent,name = lookup_parent root path uid gid
  in
    try
      ignore (lookup_dentry parent name uid gid);
      raise (EEXIST (EEXIST_Path path))
    with
	Dentry_not_found(_,_) ->
	  if not (has_write_access parent uid gid) then raise (EACCES path);
	  let mode = match mode with
	      None -> raise (EINVAL (EINVAL_Mode))
	    | Some mode -> mode
	  in let inode = create_file parent.inode.common.mntpnt mode uid
	    (new_inode_gid gid parent)
	  in
	    ignore (add_dentry_to_parent name inode parent);
	    add_to_directory_size parent.inode.common (String.length name);
	    notify_write_access parent.inode.common (current_time ())

let link uid gid root oldpath newpath =
  let newparent,newname = lookup_parent root newpath uid gid
  in
    try
      ignore (lookup_dentry newparent newname uid gid);
      raise (EEXIST (EEXIST_Path newpath))
    with
	Dentry_not_found(_,_) ->
	  if not (has_write_access newparent uid gid) then raise (EACCES newpath);
	  let olddentry = lookup_path_nofollow root oldpath uid gid
	  in let inode = olddentry.inode
	  in match inode.data with
	    | Dir x -> raise (EISDIR oldpath)
	    | _ ->
		if olddentry.inode.common.mntpnt != newparent.inode.common.mntpnt
		then raise (EXDEV (oldpath,newpath));
		inode.common.nlink <- inode.common.nlink + 1;
		ignore (add_dentry_to_parent newname olddentry.inode newparent);
		add_to_directory_size newparent.inode.common (String.length newname);
		let time = current_time ()
		in
		  notify_change_access inode.common time;
		  notify_write_access newparent.inode.common time

let rename uid gid root oldpath newpath =
  let newparent,newname = lookup_parent root newpath uid gid
  in
    try
      ignore (lookup_dentry newparent newname uid gid);
      raise (EEXIST (EEXIST_Path newpath))
    with
	Dentry_not_found(_,_) ->
	  let oldparent,oldname = lookup_parent root oldpath uid gid
	  in let olddentry,j =
	      try
		lookup_dentry oldparent oldname uid gid
	      with
		  Dentry_not_found(_,_) -> raise (ENOENT oldpath)
	  in let time = current_time ()
	  in
	    if not (has_write_access newparent uid gid) then raise (EACCES oldpath);
	    if not (has_write_access oldparent uid gid) then raise (EACCES newpath);
	    if oldparent.inode.common.mntpnt != newparent.inode.common.mntpnt
	    then raise (EXDEV (oldpath,newpath));
	    if is_ancestor olddentry newparent
	    then raise (EINVAL (EINVAL_Path newpath));
	    remove_from_parent oldparent j;
	    remove_from_directory_size oldparent.inode.common (String.length oldname);
	    notify_write_access oldparent.inode.common time;
	    notify_write_access newparent.inode.common time;
	    add_to_directory_size newparent.inode.common (String.length newname);
	    let dentry = add_dentry_to_parent newname olddentry.inode newparent
	    in
	      (* update link to parent dentry in inode if it's a dir *)
	      match olddentry.inode.data with
		| Dir y -> begin
		    oldparent.inode.common.nlink <- oldparent.inode.common.nlink - 1;
		    newparent.inode.common.nlink <- newparent.inode.common.nlink + 1;
		  end
		| _ -> ()

let unlink uid gid root path =
  let parent,name = lookup_parent root path uid gid
  in
    if not (has_write_access parent uid gid) then raise (EACCES path);
    match parent.inode.data with
      | Dir x -> begin
	  let dentry,i =
	    try
	      lookup_dentry parent name uid gid
	    with
		Dentry_not_found(_,_) -> raise (ENOENT path)
	  in let inode = dentry.inode
	  in match inode.data with
	      Dir y -> raise (EISDIR path)
	    | _ ->
		inode.common.nlink <- inode.common.nlink - 1;
		remove_from_parent_data x i;
		remove_from_directory_size parent.inode.common (String.length dentry.name);
		let time = current_time ()
		in
		  notify_change_access inode.common time;
		  notify_write_access parent.inode.common time
	end
      | _ -> raise (ENOTDIR path)

let symlink uid gid root path target =
  let parent,name = lookup_parent root path uid gid
  in
    try
      ignore (lookup_dentry parent name uid gid);
      raise (EEXIST (EEXIST_Path path))
    with
	Dentry_not_found(_,_) ->
	  if not (has_write_access parent uid gid) then raise (EACCES path);
	  let inode = create_symlink parent.inode.common.mntpnt target uid
	    (new_inode_gid gid parent)
	  in
	    ignore (add_dentry_to_parent name inode parent);
	    add_to_directory_size parent.inode.common (String.length name);
	    notify_write_access parent.inode.common (current_time ())

(* Stating files *)

let generic_stat inode =
  let common = inode.common
  in {
      st_ino = common.ino;
      st_perm = common.mode;
      st_kind =
	begin match inode.data with
	  | File x -> S_REG
	  | Dir x -> S_DIR
	  | Symlink x -> S_LNK
        end;
      st_uid = common.uid;
      st_gid = common.gid;
      st_size = common.size;
      st_atime = common.atime;
      st_ctime = common.ctime;
      st_mtime = common.mtime;
      st_nlink = common.nlink;
    }

let stat uid gid root path =
  let dentry = lookup_path root path uid gid
  in let inode = dentry.inode
  in generic_stat inode

let lstat uid gid root path =
  let dentry = lookup_path_nofollow root path uid gid
  in let inode = dentry.inode
  in generic_stat inode

let readlink uid gid root path =
  let dentry = lookup_path_nofollow root path uid gid
  in let inode = dentry.inode
  in match inode.data with
    | Symlink x ->
	notify_read_access dentry.inode.common (current_time ());
	x.target
    | _ -> raise (EINVAL (EINVAL_Path path))

(* Various file routines *)

let seek_file file offset command =
  begin
    match command with
      | SEEK_SET -> file.file_seek <- offset
      | SEEK_CUR -> file.file_seek <- file.file_seek + offset
      | SEEK_END -> file.file_seek <- file.file_common.size + offset
  end;
  file.file_seek

let truncate_file file offset =
  if List.mem O_RDONLY file.file_flags then raise (EBADF file);
  let data = file.file_data
  in
    (* TODO: check length >= 0 *)
    setup_file_blocks file offset;
    notify_write_access file.file_common (current_time ());
    file.file_common.size <- offset

(* Opening and closing files *)

let open_file uid gid root path flags mode =
  (* check read/write flags *)
  if (List.mem O_RDONLY flags && List.mem O_WRONLY flags)
    || (List.mem O_RDONLY flags && List.mem O_RDWR flags)
    || (List.mem O_WRONLY flags && List.mem O_RDWR flags) then
      raise (EINVAL (EINVAL_Open_flags flags));
  let parent,name = lookup_parent root path uid gid
  in let dentry =
    try
      (* full lookup here since we might have a symlink to traverse *)
      let dentry = lookup_path parent name uid gid
      in
	(* file existed, error if O_EXCL and O_CREAT *)
	if List.mem O_CREAT flags && List.mem O_EXCL flags then raise (EEXIST (EEXIST_Path path));
	dentry
    with
	ENOENT(_) ->
	  (* file did not exists, error if not O_CREAT *)
	  if not (List.mem O_CREAT flags) then raise (ENOENT path);
	  create uid gid root path mode;
	  try
	    lookup_path parent name uid gid
	  with
	      (* this one cannot fail *)
	      _ -> assert false
  in let real_flags = flags @
      (* O_RDONLY by default *)
      if not (List.mem O_RDONLY flags)
	&& not (List.mem O_WRONLY flags)
	&& not (List.mem O_RDWR flags) then [O_RDONLY] else []
  in
    if not (List.mem O_WRONLY real_flags) && not (has_read_access dentry uid gid) then
      raise (EACCES path);
    if not (List.mem O_RDONLY real_flags) && not (has_write_access dentry uid gid) then
      raise (EACCES path);
    let inode = dentry.inode
    in let file = match inode.data with
      | File x -> {
	  file_data = x;
	  file_common = inode.common;
	  file_flags = real_flags;
	  file_seek = 0;
	}
      | Dir x -> raise (EISDIR path)
      | Symlink x -> assert false
    in
      (* truncate file if O_TRUNC *)
      if List.mem O_TRUNC flags then truncate_file file 0;
      file

let close_file file =
  ()

(* Reading and writing files *)

let read_file file length =
  if List.mem O_WRONLY file.file_flags then raise (EBADF file);
  let data = file.file_data
  in let blksize = file.file_common.mntpnt.blksize
  in let offset_in_block = file.file_seek mod blksize
  in let block = file.file_seek / blksize
  in let real_length =
    if (length + offset_in_block > blksize)
    then blksize - offset_in_block
    else length
  in let bytes =
      match DynArray.get data.blocks block with
	| None -> new_empty_block real_length
	| Some x -> String.sub x offset_in_block real_length
  in
    notify_read_access file.file_common (current_time ());
    file.file_seek <- file.file_seek + real_length;
    (real_length,bytes)

let write_file file bytes length =
  if List.mem O_RDONLY file.file_flags then raise (EBADF file);
  let data = file.file_data
  in let blksize = file.file_common.mntpnt.blksize
  in
    (* seek to the end in case of O_APPEND *)
    if List.mem O_APPEND file.file_flags then ignore (seek_file file 0 SEEK_END);
    let offset_in_block = file.file_seek mod blksize
    in let block = file.file_seek / blksize
    in let real_length =
	if (length + offset_in_block > blksize)
	then blksize - offset_in_block
	else length
    in
      setup_file_blocks file (file.file_seek + real_length);
      begin
      match (DynArray.get data.blocks block) with
	| Some x -> String.blit bytes 0 x offset_in_block real_length
	| None -> (* time to allocate this block *)
	    let new_block = new_empty_block blksize
	    in
	      String.blit bytes 0 new_block offset_in_block real_length;
	      DynArray.set data.blocks block (Some new_block)
      end;
      notify_write_access file.file_common (current_time ());
      file.file_seek <- file.file_seek + real_length;
      if file.file_seek > file.file_common.size then
	file.file_common.size <- file.file_seek;
      real_length

(* Opening and closing directories *)

let open_dir uid gid root path =
  let dentry = lookup_path root path uid gid
  in
    if not (has_read_access dentry uid gid) then raise (EACCES path);
    let inode = dentry.inode
    in
      match inode.data with
	| Dir x ->
	    let common = dentry.inode.common
	    in
	      {
		dir_data = x;
		dir_common = common;
		dir_seek = 0;
	      }
	| _ -> raise (ENOTDIR path)

let close_dir dir =
  ()

let tell_dir dir =
  dir.dir_seek

let seek_dir dir seek =
  dir.dir_seek <- seek

let rewind_dir dir =
  (* If the dir contents changes, rewind_dir should see it. *)
  dir.dir_seek <- 0

(* Reading directories *)

let read_dir dir =
  try
    let seek = dir.dir_seek
    in
      let name =
	if seek = 0 then "."
	else if seek = 1 then ".."
	else (get_dir_dentry dir.dir_data (dir.dir_seek-2)).name
      in
	dir.dir_seek <- dir.dir_seek + 1;
	notify_read_access dir.dir_common (current_time ());
	name
  with
      DynArray.Invalid_arg(_,_,_) -> ""

(* CWD *)

let change_cwd uid gid old_wd path =
  let dentry = lookup_path old_wd path uid gid
  in let inode = dentry.inode
  in match inode.data with
    | Dir x -> begin
	if not (has_exec_access dentry uid gid) then raise (EACCES path);
	let inode = dentry
	in let full_path = gen_full_path dentry
	in dentry,full_path
      end
    | _ -> raise (ENOTDIR path)

(* File permissions *)

let generic_chown dentry new_uid new_gid =
  let common = dentry.inode.common in
    begin
      match new_uid with
	  Some id ->
	    common.uid <- id;
	    common.mode <- common.mode land (lnot s_ISUID)
	| None -> ()
    end;
    begin
      match new_gid with
	  Some id ->
	    common.gid <- id;
	    common.mode <- common.mode land (lnot s_ISGID)
	| None -> ()
    end

let chown uid gid root path new_uid new_gid =
  let dentry = lookup_path root path uid gid
  in
    if not (is_owner dentry uid) then raise (EPERM path);
    generic_chown dentry new_uid new_gid

let lchown uid gid root path new_uid new_gid =
  let dentry = lookup_path_nofollow root path uid gid
  in
    if not (is_owner dentry uid) then raise (EPERM path);
    generic_chown dentry new_uid new_gid

let chmod uid gid root path mode =
  let dentry = lookup_path root path uid gid
  in
    if not (is_owner dentry uid) then raise (EPERM path);
    match dentry.inode.data with
      | Symlink _ -> assert false (* can't end up with a symlink without _nofollow *)
      | _ ->
	  dentry.inode.common.mode <- mode land s_IALL;
	  notify_change_access dentry.inode.common (current_time ())

let utime uid gid root path times =
  let dentry = lookup_path root path uid gid
  in
    if not (is_owner dentry uid) then raise (EPERM path);
    let time = current_time ()
    in let atime,mtime =
	match times with
	  | Some (atime,mtime) -> atime,mtime
	  | None -> time,time
    in
      dentry.inode.common.atime <- atime;
      dentry.inode.common.ctime <- time;
      dentry.inode.common.mtime <- mtime

(* Filesystems *)

let fslist = ref []

let match_fs_name name fs =
  fs.fs_name = name

let register_fs fs =
  if List.exists (match_fs_name fs.fs_name) !fslist then
    raise (EEXIST (EEXIST_FsName fs.fs_name));
  Funk.kprintf "VFS" "Registered filesystem type %s\n" fs.fs_name;
  fslist := fs :: !fslist

(* VFS Initialization and Termination *)

let init name =
  try
    let fs = List.find (match_fs_name name) !fslist;
    in let mntpnt = fs.fs_mount ()
    in
      mntpnt.use <- mntpnt.use + 1;
      root_mntpnt := Some mntpnt;
      mntpnt.root
  with
      Not_found -> raise (ENODEV name)

let finish =
  ()

(* Mounting and unmounting of filesystems *)

let last_mntpnt_id = ref 0

let get_mntpnt_id () =
  incr last_mntpnt_id;
  !last_mntpnt_id

let mount uid gid root path name =
  let dentry = lookup_path root path uid gid
  in
    if not (is_owner dentry uid) then raise (EPERM path);
    let fs =
      try
	List.find (match_fs_name name) !fslist
      with
	  Not_found -> raise (ENODEV name)
    in let mntpnt = fs.fs_mount ()
    in
      dentry.mounted <- Some mntpnt.root;
      dentry.inode.common.mntpnt.use <- dentry.inode.common.mntpnt.use + 1;
      (* mntpnt.root should belong to uid/gid *)
      mntpnt.mounter <- Some dentry

let umount uid gid root path =
  let dentry = lookup_path root path uid gid
  in let mntpnt = dentry.inode.common.mntpnt
  in
    match mntpnt.mounter with
      | Some mounter ->
	  if not (is_owner mounter uid) then raise (EPERM path);
	  if mntpnt.use > 0 then raise (EBUSY path);
	  mounter.mounted <- None;
	  mounter.inode.common.mntpnt.use <- mounter.inode.common.mntpnt.use - 1;
	  mntpnt.fs.fs_umount mntpnt
      | None -> raise (EBUSY path) (* prevent form unmounting / *)

let movemount uid gid root oldpath newpath =
  let olddentry = lookup_path root oldpath uid gid
  in let newdentry = lookup_path root newpath uid gid
  in let mntpnt = olddentry.inode.common.mntpnt
  in
    match mntpnt.mounter with
      | Some mounter ->
	  if not (is_owner mounter uid) then raise (EPERM oldpath);
	  if is_ancestor olddentry newdentry then raise (EINVAL (EINVAL_Path newpath));
	  if not (is_owner newdentry uid) then raise (EPERM newpath);
	  newdentry.mounted <- Some mntpnt.root;
	  newdentry.inode.common.mntpnt.use <- newdentry.inode.common.mntpnt.use - 1;
	  mntpnt.mounter <- Some newdentry;
	  mounter.mounted <- None;
	  mounter.inode.common.mntpnt.use <- mounter.inode.common.mntpnt.use - 1
      | None -> raise (EBUSY oldpath) (* prevent form moving / *)

let statfs uid gid root path =
  let dentry = lookup_path root path uid gid
  in let mntpnt = dentry.inode.common.mntpnt
  in
    {
      sf_type = mntpnt.fs.fs_name;
      sf_path = gen_full_path mntpnt.root;
      sf_blksize = mntpnt.blksize;
    }
