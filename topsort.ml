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

let find_task tasks name =
  List.find (fun t -> t.name = name) tasks

let find_succs tasks deps name =
  List.map (fun d -> find_task tasks d.dep_dst)
    (List.filter (fun d -> d.dep_src = name) deps)

let find_preds tasks deps name =
  List.map (fun d -> find_task tasks d.dep_src)
    (List.filter (fun d -> d.dep_dst = name) deps)

type node =
    { node_task: task;
      node_preds: task list;
      node_succs: task list }

let find_node nodes task =
  List.find (fun n -> n.node_task.name = task.name) nodes
      
let build_nodes tasks deps =
  List.fold_left 
    (fun r task ->
      let preds = find_preds tasks deps task.name
      and succs = find_succs tasks deps task.name in
      { node_task = task;
        node_preds = preds;
        node_succs = succs } :: r) [] tasks

let rec visit nodes (seen, sorted) recseen node =
  if List.mem node recseen then
    fprintf stderr "Error: Detected cyclic dependency for task %s\n" node.node_task.name;

  if not (List.mem node seen) then
    let (seen',sorted') = 
      List.fold_left 
        (fun s n -> visit nodes s (node :: recseen) (find_node nodes n))
        (node :: seen, sorted)
        node.node_succs in
    (seen', node :: sorted')

  else
    (seen, sorted)
      
let topsort nodes tasks =
  let start = List.filter (fun n -> (List.length n.node_preds) = 0) nodes in
  List.fold_left (fun s n -> visit nodes s [] n) ([], []) start

let sort tasks deps =
  if !Options.verbose then
    fprintf stderr "Topological sort of task set...\n";
  
  let nodes = build_nodes tasks deps in
  let _, sorted = topsort nodes tasks in

  List.map (fun n -> n.node_task) sorted
