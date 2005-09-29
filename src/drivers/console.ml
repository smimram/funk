(* console.ml [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : console module
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
  * Functions for handling consoles.
  *
  * @author Nicolas Guenot, Samuel Mimram, Samuel Thibault *)

let kprintf s = Utils.kprintf "Console" s

let serial_console = ref None
                  
let len_x = 80
let len_y = 25

let tab_length = 8

(** String pointing at video memory. *)
let video = Memory.to_string (Memory.of_addr 0xb8000)

(** Video board registers *)
let port_reg = ref 0x3d4
let port_val = ref 0x3d5

type color =
  | Black
  | Blue
  | Green
  | Cyan
  | Red
  | Magenta
  | Brown
  | Light_gray
  | Dark_gray
  | Light_blue
  | Light_green
  | Light_cyan
  | Light_red
  | Light_magenta
  | Yellow
  | White

type t =
    {
      id : int; (** unique identifier *)
      mutable buffer : (char * color * color) DynArray.t;
      mutable x_pos : int;
      mutable y_pos : int; (** position in from the begining of the buffer in lines *)
      mutable scroll_lines : int; (** offset from the begining of the buffer in lines *)
      mutable received : string
    }

let null_char = ('\000', Light_gray, Black)

exception Ctrl_D

let get_empty_buffer () =
  DynArray.init (len_x * len_y) (fun _ -> null_char)

let make =
  let id_count = ref (-1) in
    fun () ->
      incr id_count;
      {
        id = !id_count;
        buffer = get_empty_buffer ();
        x_pos = 0;
        y_pos = 0;
        scroll_lines = 0;
        received = "";
      }

let get_nb_lines csl =
  (DynArray.length csl.buffer) / len_x

let current_console = ref (make ())

let get_current_console ()  = !current_console

let is_current_console csl =
  csl.id = (get_current_console ()).id

let int_of_color = function
  | Black -> 0
  | Blue -> 1
  | Green -> 2
  | Cyan -> 3
  | Red -> 4
  | Magenta -> 5
  | Brown -> 6
  | Light_gray -> 7
  | Dark_gray -> 8
  | Light_blue -> 9
  | Light_green -> 10
  | Light_cyan -> 11
  | Light_red -> 12
  | Light_magenta -> 13
  | Yellow -> 14
  | White -> 15

let char_of_color fg bg =
  let cfg = int_of_color fg in
  let cbg = int_of_color bg in
    char_of_int (cfg + (cbg lsl 4))

(* Quick hack not to have ansi codes at each displayed character. *)
let aoc_last_fg = ref Red
let aoc_last_bg = ref Red
                    
let ansi_of_color fg bg =
  if fg = !aoc_last_fg && bg = !aoc_last_bg then ""
  else
    (
      let ansi_of_color = function
        | Black -> 1, 30
        | Blue -> 1, 34
        | Green -> 1, 32 
        | Cyan -> 1, 36
        | Red -> 1, 31
        | Magenta -> 1, 35
        | Brown -> 0, 33
        | Light_gray -> 0, 37
        | Dark_gray -> 0, 37
        | Light_blue -> 0, 34
        | Light_green -> 0, 32
        | Light_cyan -> 0, 36
        | Light_red -> 0, 31
        | Light_magenta -> 0, 35
        | Yellow -> 1, 33
        | White -> 1, 37
      in
      let fg_attr, fg_col = ansi_of_color fg in
      let _, bg_col = ansi_of_color bg in
      let bg_col = bg_col + 10 in
        aoc_last_fg := fg;
        aoc_last_bg := bg;
        "\027[" ^
        (string_of_int fg_attr) ^
        ";" ^ (string_of_int fg_col) ^
        ";" ^ (string_of_int bg_col) ^ "m"
    )

let set_color x y fg bg =
  video.[2 * (y * len_x + x) + 1] <- char_of_color fg bg

let write_vga reg value =
  Ports.outb !port_reg reg;
  Ports.outb !port_val value

let read_vga reg =
  Ports.outb !port_reg reg;
  Ports.inb !port_val

(* TODO: store in csl *)
let set_cursor_size csl s e =
  let ss = read_vga 0xa in
  let ee = read_vga 0xb in
  write_vga 0xa ((ss land 0xc0) lor s);
  write_vga 0xb ((ee land 0xe0) lor e)

(* TODO: store the pos in csl *)
let set_cursor csl =
  let x, y = csl.x_pos, (csl.y_pos - csl.scroll_lines) in
    if is_current_console csl then
      (
        let offs = y * len_x + x in
          write_vga 0xe ((offs lsr 8) land 0xff);
          write_vga 0xf (offs land 0xff);
          set_cursor_size (get_current_console ()) 13 15;
      )

let display csl =
  let offs = csl.scroll_lines * len_x in
    for i = 0 to (len_x * len_y - 1)
    do
      let c, fg, bg = DynArray.get csl.buffer (i + offs) in
        video.[2 * i] <- c;
        video.[2 * i + 1] <- char_of_color fg bg
    done;
    current_console := csl;
    set_cursor csl

let clear csl =
  (match !serial_console with
     (*| Some port -> Serial.send_string port "\027[2J\027[0;0H"*)
     | None -> ()
     |_     -> ());
  (* TODO: improve this, only add a clear page *)
  csl.buffer <- get_empty_buffer ();
  csl.scroll_lines <- 0;
  csl.x_pos <- 0;
  csl.y_pos <- 0;
  if is_current_console csl then
    for i = 0 to (len_x * len_y - 1)
    do
        video.[2 * i] <- '\000';
        video.[2 * i + 1] <- '\007'
    done

(* x and y positions are relative to the virtual screen *)
let set_char csl x y c fg bg =
  DynArray.set csl.buffer (y * len_x + x) (c, fg, bg);
  if is_current_console csl then
    let real_y = y - csl.scroll_lines in
      if real_y >= 0 & real_y < len_y then
        (
          video.[2 * (real_y * len_x + x)] <- c;
          video.[2 * (real_y * len_x + x) + 1] <- char_of_color fg bg
        )

let newline csl =
  csl.x_pos <- 0;
  csl.y_pos <- csl.y_pos + 1;
  if csl.y_pos >= get_nb_lines csl then
    (
      DynArray.append (DynArray.init len_x (fun _ -> null_char)) csl.buffer;
      csl.scroll_lines <- csl.scroll_lines + 1;
      if is_current_console csl & csl.scroll_lines = get_nb_lines csl - len_y then
	(
	  String.unsafe_blit video (len_x*2) video 0 ((len_y-1)*len_x*2);
	  for i = (len_y-1)*len_x to len_y*len_x-1
	  do
	    video.[2 * i] <- '\000';
	    video.[2 * i + 1] <- '\007'
	  done;
	)
    )

(* TODO: update with bufferization *)
let del_char csl =
  if csl.x_pos > 0 then
    csl.x_pos <- csl.x_pos - 1
  else
    if csl.y_pos > 0 then
      (
        csl.x_pos <- len_x - 1;
        csl.y_pos <- csl.y_pos - 1;
      )
    else
      csl.x_pos <- 0;
  let c, fg, bg = null_char in
    set_char csl csl.x_pos csl.y_pos c fg bg;
    set_cursor csl

let put_char_c csl c fg bg =
  (match !serial_console with
     (*| Some port ->
         Serial.send_string port (ansi_of_color fg bg);
         Serial.send_char port c*)
     | None -> ()
     |_     -> ());
  if c = '\n' then
    newline csl
  else if c = '\r' then
    (
      (* TODO: handle received cleanly on \r *)
      csl.x_pos <- 0;
      set_cursor csl
    )
  else if c = '\b' then
    (* TODO: rewrite the entire line if previous was \t *)
    del_char csl
  else if c = '\t' then
    (
      let new_pos = (csl.x_pos + tab_length) land (lnot (tab_length - 1))
      in
	if new_pos >= len_x then
	  (
            newline csl;
	    csl.x_pos <- new_pos - len_x
	  )
	else
	  csl.x_pos <- new_pos;
	set_cursor csl
    )
  else
    (
      set_char csl csl.x_pos csl.y_pos c fg bg;
      csl.x_pos <- csl.x_pos + 1;
      if csl.x_pos >= len_x then
        newline csl;
      set_cursor csl
    )

let put_char csl c =
  let _, fg, bg = null_char in
    put_char_c csl c fg bg

let receive_char csl c =
  if c = '\b' then
    (
      if csl.received <> "" && csl.received.[(String.length csl.received) - 1] <> '\n' then
        (
          csl.received <- String.sub csl.received 0 ((String.length csl.received) - 1);
          put_char csl c
        )
    )
  else
    (
      csl.received <- csl.received ^ (String.make 1 c);
      put_char csl c
    )

(* TODO: use a condition here instead of looping like a plane *)
let wait_for_input csl =
  let loop = ref true in
    while !loop
    do
      try
        let n = String.index csl.received '\n' in
          loop := false
      with
	| Ctrl_D -> loop := false;
        | Not_found ->
            (*KThread.yield*) ()
    done

let input_line csl =
  wait_for_input csl;
  let n = String.index csl.received '\n' in
  let ans = String.sub csl.received 0 n in
    csl.received <- String.sub csl.received (n + 1) ((String.length csl.received) - (n + 1));
    ans

let input_char csl =
  wait_for_input csl;
  let ans = csl.received.[0] in
    csl.received <- String.sub csl.received 1 ((String.length csl.received) - 1);
    ans

let print_string csl s =
  for i = 0 to (String.length s) - 1
  do
    put_char csl s.[i]
  done

let print_string_c csl s fg bg =
  for i = 0 to (String.length s) - 1
  do
    put_char_c csl s.[i] fg bg
  done

(* We drop dependency of keyboard.ml for now : we just want to be able to print things to the screen *)
(*let on_key csl c s =
  if c = Keyboard.Char 'd' && s.Keyboard.ks_ctrl then raise (Ctrl_D);
  match c with
    | Keyboard.Char c ->
	receive_char csl c
    | Keyboard.Special_key k ->
	match k with
          | Keyboard.Page_up ->
              csl.scroll_lines <- csl.scroll_lines - len_y;
              if csl.scroll_lines < 0 then
		csl.scroll_lines <- 0;
              display csl
          | Keyboard.Page_down ->
              csl.scroll_lines <- csl.scroll_lines + len_y;
              if csl.scroll_lines > get_nb_lines csl - len_y then
		csl.scroll_lines <- get_nb_lines csl - len_y;
              display csl
          | _ -> kprintf "Unknown special key\n%!"

let () =
  Keyboard.on_key (fun c s -> on_key (get_current_console ()) c s)*)

(* We drop dependency of serial.ml for now *)
(*let serial_console port =
  serial_console := Some port;
  Serial.capture_keyboard port*)

let () =
  Utils.set_fkprintf (fun s -> ignore (print_string (get_current_console ()) s))
