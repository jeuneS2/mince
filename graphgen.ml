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
open Topsort

let dump_dot_eq out_f indeps =
  let eq = equality_classes indeps in
  List.iter (fun c ->
    if (List.length c) > 1 then
      begin
        fprintf out_f "subgraph \"cluster";
        List.iter (fun t -> fprintf out_f "_%s" t.name) c;
        fprintf out_f "\" {\nlabel = EQ\n";
        List.iter (fun t -> fprintf out_f "\"%s\"\n" t.name) c;
        fprintf out_f "}\n";
      end) eq

let dump_dot_comp out_f indeps =
  let comp = comparable_classes indeps in
  List.iter (fun c ->
    if (List.length c) > 1 then
      begin
        fprintf out_f "subgraph \"cluster";
        List.iter (fun t -> fprintf out_f "_%s" t.name) c;
        fprintf out_f "\" {\nlabel = COMP\n";
        List.iter (fun t -> fprintf out_f "\"%s\"\n" t.name) c;
        fprintf out_f "}\n";
      end) comp

let dump_dot dir name tasks deps =
  if !Options.verbose then
    fprintf stderr "Generating dot file...\n";

  let out_f = open_out (sprintf "%s.dot" (fname dir name)) in
  fprintf out_f "digraph SCC {\n";
  List.iter (fun t -> fprintf out_f "  \"%s\"\n" t.name) tasks;
  List.iter (fun d -> fprintf out_f "  \"%s\" -> \"%s\"\n" d.dep_src d.dep_dst) deps;
  let nodes = build_nodes tasks deps in
  let indeps = List.map (fun t -> (t, indep nodes tasks t)) tasks in
  dump_dot_comp out_f indeps;
  (* dump_dot_eq out_f indeps; *)
  fprintf out_f "}\n";
  flush out_f
