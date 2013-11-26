(* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2012, ONERA, Toulouse, FRANCE
 *
 * This file is part of Mince
 *
 * Mince is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Mince is distributed in the hope that it will be useful, but
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
  Spec.print_spec stdout (tasks,deps);
  if !Options.dot then
	begin
      Graphgen.dump_indep_dot !Options.outdir !Options.outname tasks deps;
      Graphgen.dump_dep_dot !Options.outdir !Options.outname tasks deps
	end;
  Grouping.print_indeps tasks deps;
  Grouping.print_indeps_spec !Options.outdir !Options.outname tasks deps;
  Grouping.print_deps tasks deps;
  Grouping.print_deps_spec !Options.outdir !Options.outname tasks deps
