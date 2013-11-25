(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2012, ONERA, Toulouse, FRANCE
 *
 * This file is part of Interlude
 *
 * Interlude is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Prelude is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- *)

{
open Parser

exception Error of Location.t

(* Update line number for location info *)
let incr_line lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

let blank = [' ' '\009' '\012']

rule token = parse
| '#' [^ '\n']*
    { token lexbuf }
| '\n'
    { incr_line lexbuf;
      token lexbuf }
| blank +
    { token lexbuf }

| "TFF-2.0" { MAGIC_V2 }
| "Task" { TASK }
| "Dependency" { DEPENDENCY }
| "ComBuffer" { COMBUFFER }
| "Map" { MAP }

| '-'?['0'-'9']+
    { INT (int_of_string (Lexing.lexeme lexbuf)) }
| '"'[^'"']*'"'
    { STRING (String.sub (Lexing.lexeme lexbuf)
                1 ((String.length (Lexing.lexeme lexbuf))-2)) }

| '(' { LPAR }
| ')' { RPAR }
| ',' { COMMA }
| ':' { COLON }
| ":=" { DEF }
| eof { EOF }
| _ { raise (Error (Location.curr lexbuf)) }
