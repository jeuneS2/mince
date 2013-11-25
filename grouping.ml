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
  List.fold_left (fun r d -> try (find_task tasks d.dep_dst) :: r with | Not_found -> r)
    [] (List.filter (fun d -> d.dep_src = name) deps)

let find_preds tasks deps name =
  List.fold_left (fun r d -> try (find_task tasks d.dep_src) :: r with | Not_found -> r)
    [] (List.filter (fun d -> d.dep_dst = name) deps)

let group_period group =
  (List.hd group).period
let group_wcet group =
  List.fold_left (fun r t -> r + t.wcet) 0 group
let group_name group =
  String.concat "_" (List.map (fun t -> t.name) group)

let find_group groups name =
  List.find (fun g -> List.exists (fun t -> t.name = name) g) groups

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


let rec indep tasks deps =
  match tasks with
  | [] -> []
  | _ ->
      let nodes = build_nodes tasks deps in
      let nnoprecs, ninner = List.partition (fun n -> (List.length n.node_preds) = 0) nodes in
      let noprecs = List.map (fun n -> n.node_task) nnoprecs in
      let inner = List.map (fun n -> n.node_task) ninner in
      noprecs :: (indep inner deps)

let print_indeps tasks deps =
  let indeps = indep tasks deps in
  fprintf stdout "Independent groups:\n";
  List.iter (fun l ->
    fprintf stdout "{ ";
    List.iter (fun t -> fprintf stdout "%s, " t.name) l;
    fprintf stdout "}\n") indeps

let print_indeps_spec dir name tasks deps = 
  let out_f = open_out (sprintf "%s.indep.txt" (fname dir name)) in
  let indepgrps = indep tasks deps in  
  fprintf out_f "%s" spec_magic;
  (* groups are already ordered according to dependencies, use
	 offsets/deadlines to encode dependencies *)
  let offset = ref 0 in
  List.iter (fun l ->
	let wcet = group_wcet l in
	List.iter (fun t -> 
	  fprintf out_f "Task\t\"%s\"\t%d\t%d\t%d\t(%d)\n"
		t.name t.period t.wcet !offset (!offset+wcet);
	) l;
	offset := !offset+wcet
  ) indepgrps
  (* let seen = Hashtbl.create 10 in *)
  (* List.iter (fun d -> *)
  (* 	let srcgrp = find_group indepgrps d.dep_src *)
  (* 	and dstgrp = find_group indepgrps d.dep_dst in *)
  (* 	try Hashtbl.find seen (srcgrp,dstgrp) *)
  (* 	with Not_found -> *)
  (* 	  List.iter (fun src -> *)
  (* 		List.iter (fun dst -> *)
  (* 		  print_dependency out_f { dep_src = src.name; dep_dst = dst.name; *)
  (* 								   dep_pref = d.dep_pref; dep_pat = d.dep_pat } *)
  (* 		) dstgrp *)
  (* 	  ) srcgrp; *)
  (* 	  Hashtbl.add seen (srcgrp,dstgrp) () *)
  (* ) deps *)

let rec depchain nodes n =
  if (List.length n.node_succs) = 1 then
    let succ = find_node nodes (List.hd n.node_succs) in
    if (List.length succ.node_preds) = 1 then
      succ.node_task :: (depchain nodes succ)
    else []
  else []
  
let rec dep tasks deps =
  match tasks with
  | [] -> []
  | _ ->
      let nodes = build_nodes tasks deps in
      let nnoprec = List.filter (fun n -> (List.length n.node_preds) = 0) nodes in
      let chains = List.fold_left (fun r n -> (n.node_task :: (depchain nodes n)) :: r) [] nnoprec in
      let remain = List.filter (fun t -> not (List.mem t (List.flatten chains))) tasks in
      List.append chains (dep remain deps)

let print_deps tasks deps =
  let deps = dep tasks deps in
  fprintf stdout "Dependent groups:\n";
  List.iter (fun l ->
    fprintf stdout "{ ";
    List.iter (fun t -> fprintf stdout "%s, " t.name) l;
    fprintf stdout "}\n") deps

let print_deps_spec dir name tasks deps = 
  let out_f = open_out (sprintf "%s.dep.txt" (fname dir name)) in
  let depgrps = dep tasks deps in  
  fprintf out_f "%s" spec_magic;
  List.iter (fun l ->
	let name = group_name l
	and period = group_period l
	and wcet = group_wcet l in
	fprintf out_f "Task\t\"%s\"\t%d\t%d\t%d\t(%d)\n" name period wcet 0 period
  ) depgrps;
  List.iter (fun d ->	
	let srcname = group_name (find_group depgrps d.dep_src)
	and dstname = group_name (find_group depgrps d.dep_dst) in
	if srcname <> dstname then
	  print_dependency out_f { dep_src = srcname; dep_dst = dstname;
							   dep_pref = d.dep_pref; dep_pat = d.dep_pat }
  ) deps

