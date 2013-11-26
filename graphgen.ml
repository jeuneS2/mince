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
open Printf
open Util
open Grouping

let dump_dot_nodes out_f tasks deps =
  List.iter (fun t -> fprintf out_f "  \"%s\"\n" t.name) tasks;
  List.iter (fun d -> fprintf out_f "  \"%s\" -> \"%s\"\n" d.dep_src d.dep_dst) deps

let dump_dot_groups out_f groups =
  List.iter (fun c ->
    if (List.length c) > 1 then
      begin
        fprintf out_f "subgraph \"cluster";
        List.iter (fun t -> fprintf out_f "_%s" t.name) c;
        fprintf out_f "\" {\n";
        List.iter (fun t -> fprintf out_f "\"%s\"\n" t.name) c;
        fprintf out_f "}\n";
      end) groups

let dump_dot dir name tasks deps =
  let fname = sprintf "%s.dot" (fname dir name) in
  if !Options.verbose then
    fprintf stderr "Generating dot file %s\n" fname;
  let out_f = open_out fname in
  fprintf out_f "digraph tasks {\n";
  dump_dot_nodes out_f tasks deps;
  fprintf out_f "}\n";
  close_out out_f

let dump_indep_dot dir name tasks deps =
  let fname = sprintf "%s.indep.dot" (fname dir name) in
  if !Options.verbose then
    fprintf stderr "Generating dot file %s\n" fname;
  let out_f = open_out fname in
  fprintf out_f "digraph indep_groups {\n";
  dump_dot_nodes out_f tasks deps;
  let indepgroups = indep tasks deps in
  dump_dot_groups out_f indepgroups;
  fprintf out_f "}\n";
  close_out out_f

let dump_dep_dot dir name tasks deps =
  let fname = sprintf "%s.dep.dot" (fname dir name) in
  if !Options.verbose then
    fprintf stderr "Generating dot file %s\n" fname;
  let out_f = open_out fname in
  fprintf out_f "digraph dep_groups {\n";
  dump_dot_nodes out_f tasks deps;
  let depgroups = dep tasks deps in
  dump_dot_groups out_f depgroups;
  fprintf out_f "}\n";
  close_out out_f
