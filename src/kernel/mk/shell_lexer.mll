{
(* shell_lexer.mll [part of the funk project]
[functional kernel, http://perso.ens-lyon.fr/nicolas.guenot/funk/]
 * contents    : shell lexer module
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
    * Lexer for the shell.
    *
    * @author Brice Goglin, Samuel Mimram
    **)
open Lexing
open Shell_parser
}

let letter = ['a'-'z''A'-'Z''0'-'9''-''_''.''/''<''>''=']

rule token = parse
        | '\"'((letter|' ')* as s)'\"'  { STRING s }
        | letter+ as s                  { STRING s }
        | [' ']+                        { token lexbuf }
        | eof                           { EOL }
