(* shell.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : minimal shell module
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
  * A system shell.
  *
  * @author Brice Goglin, Nicolas Guenot, Samuel Mimram, Samuel Thibault
  **)

let print_c s c =  Console.print_string_c (Console.get_current_console ()) s c Console.Black

let test_thread () =
  for i = 0 to 10
  do
    Printf.printf "Aha I'm here (%d : %d)!\n%!" (KThread.id (KThread.self ())) (Nativeint.to_int (Obj.magic (KThread.self ())));
    KThread.yield ()
  done

let eater s =
  for i = 0 to 10
  do
    KThread.Semaphore.wait s;
    Printf.printf "miam\n%!"
  done

(* TODO: this is for debug, to remove someday *)
let irqkbd = ref true

let s = KThread.Semaphore.create 0
let cmds = Hashtbl.create 100

exception Wrong_arg_nb

(** Fill the commands table. *)
let () =
  Hashtbl.add cmds "beep" (fun ctx -> function freq::duration::_ -> Sound.beep (int_of_string freq) (int_of_string duration) | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "clear" (fun ctx _ -> Console.clear (Console.get_current_console()));
  Hashtbl.add cmds "dmesg" (fun ctx _ -> Printf.printf "%s%!" (Filecmds.cat_from_file ctx "dmesg"));
  Hashtbl.add cmds "date" (fun ctx _ -> Printf.printf "%d brouzoufs elapsed since boot.\n%!" (Funk.gettimeofday ()));
  Hashtbl.add cmds "exception" (fun ctx -> function e::_ -> raise (Invalid_argument e) | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "exit" (fun ctx _ -> KThread.exit ());
  Hashtbl.add cmds "floppy" (fun ctx _ -> Floppy.test ());
  Hashtbl.add cmds "halt"
    (fun ctx _ ->
       Printf.printf "You can now safely shutdown your computer...\n%!";
       exit 0);
  Hashtbl.add cmds "interrupt" (fun ctx -> function i::_ -> Funk.make_interrupt (int_of_string i) | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "irqkbd" (fun ctx _ -> irqkbd := not !irqkbd);
  Hashtbl.add cmds "lspci"
    (fun ctx _ ->
       List.iter
         (fun u ->
            (* Printf.printf
             "PCI device: dev: %x, vend: %x, class: %x, sclass: %x, pi: %x\n%!"
             u.Pci.device u.Pci.vendor u.Pci.baseclass u.Pci.subclass u.Pci.prog_if; *)
            Printf.printf "%s\n%!" (Pci_ids.string_of_unit u)
         ) (Pci.get_units ()));
  Hashtbl.add cmds "message" (fun ctx -> function msg::_ -> Funk.kprintf "test" "%s\n" msg | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "net" (fun _  _ -> Ne2k.test ());
  Hashtbl.add cmds "reboot"
    (fun ctx _ ->
       (* see machine_reboot in Linux 2.6 *)
       for i = 1 to 100 do
         Funk.usleep 50;
         Funk.outb 0x64 0xfe; (* pulse reset low *)
         Funk.usleep 50;
       done);
  Hashtbl.add cmds "yield" (fun ctx _ -> KThread.yield ());
  Hashtbl.add cmds "thread" (fun ctx _ -> ignore (KThread.create test_thread ()));
  Hashtbl.add cmds "troll" (fun ctx _ -> ignore (KThread.create eater s));
  Hashtbl.add cmds "feed" (fun ctx _ -> KThread.Semaphore.post s);

  (* Context specific commands *)
  Hashtbl.add cmds "su" (fun ctx -> function uid::_ -> ctx.Funk.uid <- int_of_string uid | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "cd"
    (fun ctx -> function
       | path::_ ->
	   let new_wd_handle,new_wd = Vfs.change_cwd ctx.Funk.uid ctx.Funk.gid ctx.Funk.wd_handle path (* TODO: use File.change_cwd when file uses the context *)
	   in
	     ctx.Funk.wd <- new_wd;
	     ctx.Funk.wd_handle <- new_wd_handle
       | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "pwd" (fun ctx _ -> Printf.printf "%s\n%!" ctx.Funk.wd);
  Hashtbl.add cmds "umask"
    (fun ctx -> function
       | mask::_ -> ctx.Funk.umask <- (int_of_string ("0o" ^ mask))
       | _ -> Printf.printf "%o\n%!" ctx.Funk.umask
    );
  Hashtbl.add cmds "id" (fun ctx _ -> Printf.printf "uid %d gid %d\n%!" ctx.Funk.uid ctx.Funk.gid);

  (* VFS specific commands *)
  Hashtbl.add cmds "mkdir" (fun ctx -> function path::_ -> Filecmds.mkdir ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "rmdir" (fun ctx -> function path::_ -> Filecmds.rmdir ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "create" (fun ctx -> function path::_ -> Filecmds.create ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "touch" (fun ctx -> function path::_ -> Filecmds.touch ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "ln" (fun ctx -> function oldpath::newpath::_ -> Filecmds.link ctx oldpath newpath | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "mv" (fun ctx -> function oldpath::newpath::_ -> Filecmds.mv ctx oldpath newpath | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "lns" (fun ctx -> function target::newpath::_ -> Filecmds.lns ctx target newpath | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "rm" (fun ctx -> function path::_ -> Filecmds.rm ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "stat" (fun ctx -> function path::_ -> Filecmds.show_stat ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "lstat" (fun ctx -> function path::_ -> Filecmds.show_lstat ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "cat"
     (fun ctx -> function
        | ">"::path::data::_ -> Filecmds.cat_to_file ctx path data
        | ">>"::path::data::_ -> Filecmds.append_to_file ctx path data
        | path::_ -> Printf.printf "%s%!" (Filecmds.cat_from_file ctx path)
        | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "head" (fun ctx -> function num::path::_ -> Filecmds.head ctx (int_of_string num) path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "tail" (fun ctx -> function num::path::_ -> Filecmds.tail ctx (int_of_string num) path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "lines" (fun ctx -> function begin_num::end_num::path::_ -> Filecmds.lines ctx (int_of_string begin_num) (int_of_string end_num) path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "wc" (fun ctx -> function path::_ -> Filecmds.wc ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "ls" (fun ctx -> function path::_ -> Filecmds.ls ctx path | _ -> Filecmds.ls ctx ".");
  Hashtbl.add cmds "tree" (fun ctx -> function path::_ -> Filecmds.tree ctx path | _ -> Filecmds.tree ctx ".");
  Hashtbl.add cmds "chmod" (fun ctx -> function mode::path::_ -> Filecmds.chmod ctx mode path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "chown" (fun ctx -> function uid::path::_ -> Filecmds.chown ctx uid path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "lchown" (fun ctx -> function uid::path::_ -> Filecmds.lchown ctx uid path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "chgrp" (fun ctx -> function gid::path::_ -> Filecmds.chgrp ctx gid path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "lchgrp" (fun ctx -> function gid::path::_ -> Filecmds.lchgrp ctx gid path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "utime" (fun ctx -> function path::atime::mtime::_ -> Filecmds.utime ctx path (Some (atime,mtime)) | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "utime" (fun ctx -> function path::_ -> Filecmds.utime ctx path None | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "mount" (fun ctx -> function name::path::_ -> Filecmds.mount ctx name path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "umount" (fun ctx -> function path::_ -> Filecmds.umount ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "movemount" (fun ctx -> function oldpath::newpath::_ -> Filecmds.movemount ctx oldpath newpath | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "statfs" (fun ctx -> function path::_ -> Filecmds.statfs ctx path | _ -> raise Wrong_arg_nb);
  Hashtbl.add cmds "debugvfs"
    (fun ctx _ ->
       Printf.printf "Adding fake entries in /debug for debugging\n%!";
       File.mkdir ctx "debug" (Some 0o777);
       File.mkdir ctx "debug/prout" (Some 0o755);
       File.create ctx "debug/prout/glop" (Some 0o666);
       File.mkdir ctx "debug/prout/coin" (Some 0o700);
       File.create ctx "debug/prout/pouet" (Some 0o600);
       File.create ctx "debug/prout/coin/pan" (Some 0o644);
       File.link ctx "debug/prout/coin/pan" "debug/ouaf"
    )

let toplevel ctx arg =
  while true
  do
    KThread.yield ();
    Printf.printf "%!";
    if arg <> 0 then begin
      print_c "root" Console.Green;
      print_c "@" Console.Yellow;
      print_c "funk" Console.Blue;
      print_c ":" Console.Magenta;
      print_c ctx.Funk.wd Console.Cyan;
      print_c "# " Console.Red;
    end else begin
      print_c "root" Console.Red;
      print_c "@" Console.Blue;
      print_c "funk" Console.Green;
      print_c ":" Console.Cyan;
      print_c ctx.Funk.wd Console.Magenta;
      print_c "# " Console.Yellow;
    end;

    try
      let cmd =
        if !irqkbd then
          Console.input_line (Console.get_current_console ())
        else
          input_line stdin
      in
        match (Shell_parser.args Shell_lexer.token (Lexing.from_string cmd)) with
          | [] -> ()
          | cmd::args ->
	      try
                (Hashtbl.find cmds cmd) ctx args
	      with
                | Not_found -> Printf.printf "funksh: command not found: %s\n%!" cmd
                | Wrong_arg_nb -> Printf.printf "funksh: wrong number of arguments for %s\n%!" cmd
    with
      | Failure "lexing: empty token" ->
          Printf.printf "funksh: syntax error.\n%!"
      | Vfs_defs.ENOENT(_) | Vfs_defs.EEXIST(_) | Vfs_defs.ENOTDIR(_)
      | Vfs_defs.EISDIR(_) | Vfs_defs.ENOTEMPTY(_) | Vfs_defs.EINVAL(_)
      | Vfs_defs.EBADF(_) | Vfs_defs.EXDEV(_,_) | Vfs_defs.EBUSY(_)
      | Vfs_defs.EACCES(_) | Vfs_defs.EPERM(_)
	    as e ->
	  (* Get VFS exception here until there are catched in File.cmds *)
	  Printf.printf "Uncatched VFS exception %s\n%!" (Printexc.to_string e)
  done
