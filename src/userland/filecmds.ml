(* file.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : userland high-level file command
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
  * Userland high-level file commands.
  *
  * @author Brice Goglin
  **)

(* default creation modes, umask will be removed from these *)
let full_dir_mode = Vfs_defs.s_IACCESS
let default_dir_mode ctx = (full_dir_mode land (lnot ctx.Funk.umask))
let full_file_mode = Vfs_defs.s_IRUGO lor Vfs_defs.s_IWUGO
let default_file_mode ctx = (full_file_mode land (lnot ctx.Funk.umask))

(*
 * These commands are Unix-like commands.
 * They are not supposed to let any exception raise to the user.
 *)

(**
  * Attribute functions
  **)

let show_kind = function
  | Vfs_defs.S_REG -> '-'
  | Vfs_defs.S_DIR -> 'd'
  | Vfs_defs.S_CHR -> 'c'
  | Vfs_defs.S_BLK -> 'b'
  | Vfs_defs.S_LNK -> 'l'
  | Vfs_defs.S_FIFO -> 'p'
  | Vfs_defs.S_SOCK -> 's'

let show_mode mode =
  let test_mode f m = f land m = m in
  (if test_mode mode Vfs_defs.s_IRUSR then "r" else "-") ^
  (if test_mode mode Vfs_defs.s_IWUSR then "w" else "-") ^
  (if test_mode mode (Vfs_defs.s_IXUSR lor Vfs_defs.s_ISUID) then "s" else
   if test_mode mode Vfs_defs.s_IXUSR then "x" else
   if test_mode mode Vfs_defs.s_ISUID then "S" else "-") ^
  (if test_mode mode Vfs_defs.s_IRGRP then "r" else "-") ^
  (if test_mode mode Vfs_defs.s_IWGRP then "w" else "-") ^
  (if test_mode mode (Vfs_defs.s_IXGRP lor Vfs_defs.s_ISGID) then "s" else
   if test_mode mode Vfs_defs.s_IXGRP then "x" else
   if test_mode mode Vfs_defs.s_ISGID then "S" else "-") ^
  (if test_mode mode Vfs_defs.s_IROTH then "r" else "-") ^
  (if test_mode mode Vfs_defs.s_IWOTH then "w" else "-") ^
  (if test_mode mode (Vfs_defs.s_IXOTH lor Vfs_defs.s_ISVTX) then "t" else
   if test_mode mode Vfs_defs.s_IXOTH then "x" else
   if test_mode mode Vfs_defs.s_ISVTX then "T" else "-")

let show_link ctx path =
  let stats = File.lstat ctx path in
    if stats.Vfs_defs.st_kind = Vfs_defs.S_LNK then
      Printf.sprintf " -> %s" (File.readlink ctx path)
    else ""

let string_of_stat stats =
  Printf.sprintf "%c%s %d %d %d %d %d %d %d %d"
    (show_kind stats.Vfs_defs.st_kind)
    (show_mode stats.Vfs_defs.st_perm)
    stats.Vfs_defs.st_ino
    stats.Vfs_defs.st_uid
    stats.Vfs_defs.st_gid
    stats.Vfs_defs.st_nlink
    stats.Vfs_defs.st_size
    stats.Vfs_defs.st_atime
    stats.Vfs_defs.st_ctime
    stats.Vfs_defs.st_mtime

let show_lstat ctx path =
  let stats = File.lstat ctx path
  in Printf.printf "%s %s\n%!" (string_of_stat stats) path

let show_stat ctx path =
  let stats = File.stat ctx path
  in Printf.printf "%s %s\n%!" (string_of_stat stats) path

(**
  * Filename functions
  **)

let mkdir ctx path =
  File.mkdir ctx path (Some (default_dir_mode ctx))

let rmdir ctx path =
  File.rmdir ctx path

let create ctx path =
  File.create ctx path (Some (default_file_mode ctx))

let link ctx oldpath newpath =
  File.link ctx oldpath newpath

let mv ctx oldpath newpath =
  File.rename ctx oldpath newpath

let lns ctx target newpath =
  File.symlink ctx newpath target

let rm ctx path =
  File.unlink ctx path

let touch ctx path =
  try
    ignore(File.stat ctx path);
    (* utime to update times *)
  with
      Vfs_defs.ENOENT(_) ->
	File.create ctx path (Some (default_file_mode ctx))

(**
  * Data access
  **)

let cat_from_file ctx path =
  let file = File.open_file ctx path [Vfs_defs.O_RDONLY] None
  in let stats = File.stat ctx path
  in let data = String.create stats.Vfs_defs.st_size
  in
    ignore (File.read file stats.Vfs_defs.st_size data 0);
    File.close_file file;
    data

let cat_to_file ctx path data =
  let file = File.open_file ctx path
    [Vfs_defs.O_WRONLY;Vfs_defs.O_TRUNC;Vfs_defs.O_CREAT]
    (Some (default_file_mode ctx))
  in
    ignore (File.write file (String.length data) data 0);
    File.close_file file

let append_to_file ctx path data =
  let file = File.open_file ctx path
    [Vfs_defs.O_WRONLY;Vfs_defs.O_APPEND;Vfs_defs.O_CREAT]
    (Some (default_file_mode ctx))
  in let len = (File.stat ctx path).Vfs_defs.st_size
  in
    ignore (File.write file (String.length data) data 0);
    File.close_file file


let lines ctx begin_num end_num path =
  let file = File.open_file ctx path [Vfs_defs.O_RDONLY] None
  in let stats = File.stat ctx path
  in let length = stats.Vfs_defs.st_size
  in let data = String.create stats.Vfs_defs.st_size
  in let lines = ref 0
  in let index = ref 0
  in let loop = ref true
  in
    ignore (File.read file length data 0);
    while !lines < end_num && !loop do
      try
	let endl = String.index_from data !index '\n'
	in
	  lines := !lines + 1;
	  if !lines >= begin_num then Printf.printf "%s%!" (String.sub data !index (endl - !index + 1));
	  index := endl + 1;
      with
	  Not_found ->
	    lines := !lines + 1;
	    if !lines >= begin_num then Printf.printf "%s%!" (String.sub data !index (String.length data - !index));
	    loop := false
    done

let head ctx num path =
  lines ctx 1 num path

let tail ctx num path =
  let file = File.open_file ctx path [Vfs_defs.O_RDONLY] None
  in let stats = File.stat ctx path
  in let length = stats.Vfs_defs.st_size
  in let data = String.create stats.Vfs_defs.st_size
  in let lines = ref 0
  in let index = ref (length - 1)
  in let loop = ref true
  in
    ignore (File.read file length data 0);
    if !index < length-1 then incr lines;
    while !lines < num && !loop do
      try
	index := String.rindex_from data (!index - 1) '\n';
	incr lines
      with
	  Not_found ->
	    index := -1;
	    loop := false
    done;
    Printf.printf "%s%!" (String.sub data (!index + 1) (length - !index - 1))

let wc ctx path =
  let file = File.open_file ctx path [Vfs_defs.O_RDONLY] None
  in let stats = File.stat ctx path
  in let data = String.create stats.Vfs_defs.st_size
  in let lines = ref 1
  in let words = ref 0
  in let chars = ref stats.Vfs_defs.st_size
  in let new_word = ref false
  in
    chars := File.read file !chars data 0;
    for i = 0 to !chars - 1 do
      if String.get data i = '\n' && i < (String.length data) - 1 then lines := !lines + 1;
      if String.get data i = ' '
      then new_word := false
      else if !new_word = false then
	begin
	  words := !words + 1;
	  new_word := true;
	end
    done;
    Printf.printf "%d %d %d\n%!" !lines !words !chars

(**
  * Directory traversal
  **)

let ls ctx path =
  let stats = File.lstat ctx path
  in
    match stats.Vfs_defs.st_kind with
      | Vfs_defs.S_DIR ->
          let dir = File.open_dir ctx path
          in let loop = ref true
          in
            while !loop do
              let name = File.read_dir dir
              in
                if String.length name = 0
                then loop := false
                else begin
                  let fullname = (path ^ "/" ^ name)
		  in let stats = File.lstat ctx fullname
                  in Printf.printf "%s %s%s\n%!"
		       (string_of_stat stats)
		       name
                       (show_link ctx fullname)
                end
            done;
            File.close_dir dir
      | _ -> Printf.printf "%s %s%s\n%!" (string_of_stat stats) path
               (show_link ctx path)

let tree ctx path =
  let rec _tree basepath path =
    let dir = File.open_dir ctx (basepath ^ path)
    in let loop = ref true
    in
      while !loop do
	let name = File.read_dir dir
	in
	  if String.length name = 0
	  then loop := false
	  else begin
	    let stats = File.lstat ctx (basepath ^ path ^ name)
	    in
	      Printf.printf "%s %s%s\n%!"
		(string_of_stat stats)
		(path ^ name)
		(show_link ctx (basepath ^ path ^ name));
	      if name <> "." && name <> ".."
	        && stats.Vfs_defs.st_kind = Vfs_defs.S_DIR then
		_tree basepath (path ^ name ^ "/")
	  end
      done;
      File.close_dir dir
  in _tree (path ^ "/") ""

(**
  * File permissions
  **)

let chmod ctx mode path =
  try
    File.chmod ctx path (int_of_string ("0o" ^ mode))
  with
    | Failure "int_of_string" -> Printf.printf "chmod: invalid argument\n%!"

let chown ctx new_uid path =
  try
    File.chown ctx path (Some (int_of_string new_uid)) None
  with
    | Failure "int_of_string" -> Printf.printf "chown: invalid argument\n%!"

let chgrp ctx new_gid path =
  try
    File.chown ctx path None (Some (int_of_string new_gid))
  with
    | Failure "int_of_string" -> Printf.printf "chgrp: invalid argument\n%!"

let lchown ctx new_uid path =
  try
    File.lchown ctx path (Some (int_of_string new_uid)) None
  with
    | Failure "int_of_string" -> Printf.printf "lchown: invalid argument\n%!"

let lchgrp ctx new_gid path =
  try
    File.lchown ctx path None (Some (int_of_string new_gid))
  with
    | Failure "int_of_string" -> Printf.printf "lchgrp: invalid argument\n%!"

let utime ctx path times =
  try
    File.utime ctx path (
      match times with
	| Some (atime,mtime) -> Some (int_of_string atime,int_of_string mtime)
	| None -> None
    )
  with
    | Failure "int_of_string" -> Printf.printf "lchgrp: invalid argument\n%!"

(**
  * Mounting
  **)

let mount ctx name path =
  try
    File.mount ctx path name
  with
    | Vfs_defs.ENODEV(_) -> Printf.printf "No such FS type \"%s\"\n%!" name
    | Vfs_defs.EPERM(_) -> Printf.printf "Permission denied\n%!"

let umount ctx path =
  try
    File.umount ctx path
  with
    | Vfs_defs.EBUSY(_) -> Printf.printf "Cannot umount \"%s\", file system is busy\n%!" path
    | Vfs_defs.EPERM(_) -> Printf.printf "Permission denied\n%!"

let movemount ctx oldpath newpath =
  try
    File.movemount ctx oldpath newpath
  with
    | Vfs_defs.EBUSY(_) -> Printf.printf "Cannot umount \"%s\", file system is busy\n%!" oldpath
    | Vfs_defs.EPERM(_) -> Printf.printf "Permission denied\n%!"

let statfs ctx path =
  let stats = File.statfs ctx path
  in Printf.printf "%s %s %d\n%!" stats.Vfs_defs.sf_path stats.Vfs_defs.sf_type stats.Vfs_defs.sf_blksize

(* Initialize the hacky kprintf *)

(*
let start_dmesg_file_logging ctx =
  Funk.kprintf "LOG" "Now logging messages to dmesg file\n";
  (* open as root for now, until we have a real auth *)
  let dmesg_file = File.open_file ctx "dmesg"
    [Vfs_defs.O_WRONLY;Vfs_defs.O_APPEND;Vfs_defs.O_CREAT;Vfs_defs.O_TRUNC]
    (Some 0o644)
  in Funk.set_fkprintf (fun s -> ignore (File.write dmesg_file (String.length s) s 0))
*)
