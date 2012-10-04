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

open Spec
open Util
open Printf

let _ =
  Options.parse;
  let infile = if !Options.inname = "" then stdin else open_in !Options.inname in
  let lexbuf = Lexing.from_channel infile in
  let (tasks,deps) = Parser.task_specs Lexer.token lexbuf in
  if !Options.dot then
    Graphgen.dump_dot !Options.outdir !Options.outname tasks deps;
  let sorted = Topsort.sort tasks deps in
  Spec.print_spec stdout (sorted,deps);
  Topsort.print_indeps tasks deps;
  Topsort.print_equality_classes tasks deps;
  Topsort.print_comparable_classes tasks deps;
  fprintf stdout "EQUALITY PERMUTATIONS\n";
  Topsort.print_sub_permutes Topsort.equality_classes tasks deps;
  fprintf stdout "COMPARABLE PERMUTATIONS\n";
  Topsort.print_sub_permutes Topsort.comparable_classes tasks deps
  (* Topsort.print_permutes tasks deps *)
