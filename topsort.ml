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


let rec succ_set nodes task seen =
  let n = find_node nodes task in
  List.fold_left (fun r t -> 
    if not (List.memq t r) then 
      succ_set nodes t (t :: r)
    else r) seen n.node_succs

let rec pred_set nodes task seen =
  let n = find_node nodes task in
  List.fold_left (fun r t -> 
    if not (List.memq t r) then 
      pred_set nodes t (t :: r)
    else r) seen n.node_preds

let indep nodes tasks task =
  let succs = succ_set nodes task [] and
      preds = pred_set nodes task [] in
  List.filter (fun t ->
    (not (List.memq t preds)) && (not (List.memq t succs)))
    tasks

let print_indeps tasks deps =
  let nodes = build_nodes tasks deps in
  let indeps = List.map (fun t -> (t, indep nodes tasks t)) tasks in
  List.iter (fun (t, l) ->
    fprintf stdout "%s: { " t.name;
    List.iter (fun t -> fprintf stdout "%s, " t.name) l;
    fprintf stdout "}\n") indeps

let rec equality_classes indeps =
  match indeps with    
  | (task, indep) :: tail -> 
      if indep = [] then
        (List.map (fun (t, _) -> t) [(List.hd indeps)]) :: (equality_classes (List.tl indeps))
      else
        let eq, rest = List.partition (fun (t, i) -> i = indep) indeps in
        (List.map (fun (t, _) -> t) eq) :: (equality_classes rest)
  | [] -> []

let print_equality_classes tasks deps =
  let nodes = build_nodes tasks deps in
  let indeps = List.map (fun t -> (t, indep nodes tasks t)) tasks in
  let classes = equality_classes indeps in
  List.iter (fun c ->
    if (List.length c) > 1 then
      begin
        fprintf stdout "EQ { ";
        List.iter (fun t -> fprintf stdout "%s, " t.name) c;
        fprintf stdout "}\n"
      end) classes

let rec comparable_classes indeps =
   match indeps with    
  | (task, indep) :: tail -> 
      if indep = [] then
        (List.map (fun (t, _) -> t) [(List.hd indeps)]) :: (comparable_classes (List.tl indeps))
      else
        let comp a b = List.exists (fun t -> List.mem t b) a in
        let eq, rest = List.partition (fun (t, i) -> comp i indep) indeps in
        (List.map (fun (t, _) -> t) eq) :: (comparable_classes rest)
  | [] -> [] 

let print_comparable_classes tasks deps =
  let nodes = build_nodes tasks deps in
  let indeps = List.map (fun t -> (t, indep nodes tasks t)) tasks in
  let classes = comparable_classes indeps in
  List.iter (fun c ->
    if (List.length c) > 1 then
      begin
        fprintf stdout "COMP { ";
        List.iter (fun t -> fprintf stdout "%s, " t.name) c;
        fprintf stdout "}\n"
      end) classes

let print_seq seq =
  fprintf stdout "< ";
  List.iter (fun n -> fprintf stdout "%s, " n.node_task.name) seq;
  fprintf stdout ">\n"    

let rec build_permutes seq tasks deps =
  if tasks = [] then
    print_seq (List.rev seq)
  else
    let nodes = build_nodes tasks deps in
    let start = List.filter (fun n -> (List.length n.node_preds) = 0) nodes in
    let start_safe = if start = [] then [ List.hd nodes ] else start in
    if start = [] then
      fprintf stderr "Error: Cycle in task graph, break for random task\n";
    List.iter (fun s ->
      let name = s.node_task.name in
      let tasks' = List.filter (fun t -> t <> s.node_task) tasks and
          deps' = List.filter (fun d -> d.dep_src <> name && d.dep_dst <> name) deps in
      build_permutes (s :: seq) tasks' deps') start_safe

let print_permutes tasks deps =
  build_permutes [] tasks deps

let sort tasks deps =
  if !Options.verbose then
    fprintf stderr "Topological sort of task set...\n";
  
  let nodes = build_nodes tasks deps in
  let _, sorted = topsort nodes tasks in

  List.map (fun n -> n.node_task) sorted
