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
open Printf
open Util
open Grouping

let dump_dot_grps out_f indeps =
  List.iter (fun c ->
    if (List.length c) > 1 then
      begin
        fprintf out_f "subgraph \"cluster";
        List.iter (fun t -> fprintf out_f "_%s" t.name) c;
        fprintf out_f "\" {\n";
        List.iter (fun t -> fprintf out_f "\"%s\"\n" t.name) c;
        fprintf out_f "}\n";
      end) indeps

let dump_indep_dot dir name tasks deps =
  if !Options.verbose then
    fprintf stderr "Generating dot file...\n";
  let out_f = open_out (sprintf "%s.indep.dot" (fname dir name)) in
  fprintf out_f "digraph indep_groups {\n";
  List.iter (fun t -> fprintf out_f "  \"%s\"\n" t.name) tasks;
  List.iter (fun d -> fprintf out_f "  \"%s\" -> \"%s\"\n" d.dep_src d.dep_dst) deps;
  let indepgrps = indep tasks deps in
  dump_dot_grps out_f indepgrps;
  fprintf out_f "}\n";
  flush out_f

let dump_dep_dot dir name tasks deps =
  if !Options.verbose then
    fprintf stderr "Generating dot file...\n";
  let out_f = open_out (sprintf "%s.dep.dot" (fname dir name)) in
  fprintf out_f "digraph dep_groups {\n";
  List.iter (fun t -> fprintf out_f "  \"%s\"\n" t.name) tasks;
  List.iter (fun d -> fprintf out_f "  \"%s\" -> \"%s\"\n" d.dep_src d.dep_dst) deps;
  let depgrps = dep tasks deps in
  dump_dot_grps out_f depgrps;
  fprintf out_f "}\n";
  flush out_f
