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

open Printf

type task =
    { name: string;
      period: int;
      wcet: int;
      offset: int;
      deadline: int;
      userfun: string;
      id: int}

let print_task out_f task =
  fprintf out_f "Task \"%s\" := \"%s\" %d %d %d (%d)\n"
    task.name task.userfun task.period task.wcet task.offset task.deadline

module Tasktbl =
struct
  let create n t = Array.make n t
  let copy = Array.copy
  let find tbl t = tbl.(t.id)
  let add tbl t x = tbl.(t.id) <- x
  let replace = add
end

let task_util task =
  (float task.wcet) /. (float task.period)

let utilization tasks =
  List.fold_left (fun r t -> r +. (task_util t)) 0.0 tasks

let task_load task =
  (float task.wcet) /. (float task.deadline)

let load_factor tasks =
  List.fold_left (fun r t -> r +. (task_load t)) 0.0 tasks

let q_max tasks =
  List.fold_left (fun r t -> if t.wcet > r then t.wcet else r) 0 tasks

(* Approximation of demand bound function as in: K. Albers and
   F. Slomka. An event stream driven approximation for the analysis of
   real-time systems. ECRTS 2004 *)
let dbf_star task t =
  if t < task.deadline then
    0.0
  else
    let c = task.wcet and
        d = task.deadline and
        u = task_util task in
    (float c) +. u *. (float (t - d))

(* Equation (7) in the paper cited below *)
let task_partition_condA tasks task =
  let u_i = task_util task and
      sum_u_j = List.fold_left (fun r t_j -> r +. (task_util t_j)) 0.0 tasks in
  1.0 -. sum_u_j >= u_i

(* Equation (26) in the paper cited below *)
let task_partition_condB tasks task =
  List.fold_left (fun r t_l ->
    let d_l = t_l.deadline and
        e_l = float t_l.wcet in
    let dbf_sum = (List.fold_left 
                     (fun r t_j ->
                       if t_j.deadline < d_l then r +. dbf_star t_j d_l
                       else r) 0.0 tasks) and
        q_max = (List.fold_left
                   (fun r t_j ->
                     if t_j.deadline > d_l then
                       if t_j.wcet > r then t_j.wcet else r
                     else r) 0 (task :: tasks)) in
    if (float d_l) -. dbf_sum >= e_l +. (float q_max) then r else false) true (task :: tasks)

(* Partitioning condition for constrained task sets as in: N. Fisher
   and S. Baruah. The partitioned multiprocessor scheduling of
   non-preemptive sporadic task systems. RTNS 2006 *)
let task_partition_cond tasks task =
  (task_partition_condA tasks task) && (task_partition_condB tasks task)

type dword = (int * int)

let print_word out_f dword =
  let (a,b) = dword in
  fprintf out_f "%d:%d" a b

type dependency =
    { dep_src: string;
      dep_dst: string;
      dep_pref: dword list;
      dep_pat: dword list }

let sepiter fun_f sep_f list =
    match list with
    | [] -> ()
    | head::tail ->
        fun_f head; List.iter (fun l -> sep_f (); fun_f l) tail

let print_dependency out_f dep =
  fprintf out_f "Dependency \"%s\" \"%s\" "
    dep.dep_src dep.dep_dst;
  sepiter (fun (s,d) -> fprintf out_f "%d:%d" s d) (fun () -> fprintf out_f ",") dep.dep_pref;
  fprintf out_f "(";
  sepiter (fun (s,d) -> fprintf out_f "%d:%d" s d) (fun () -> fprintf out_f ",") dep.dep_pat;
  fprintf out_f ")\n"

type buffer =
    { buf_src: string;
      buf_dst: string;
      buf_elemsize: int;
      buf_size: int;
      buf_init: string option;
      buf_id: int }

let print_buffer out_f buf =
  fprintf out_f "ComBuffer \"%s\" \"%s\" %d %d"
    buf.buf_src buf.buf_dst buf.buf_elemsize buf.buf_size;
  (match buf.buf_init with
  | Some str -> fprintf out_f " := \"%s\"" str
  | None -> ());
  fprintf out_f "\n"

module Buffertbl = Hashtbl.Make(struct
  type t = buffer
  let equal x y = x.buf_id = y.buf_id
  let hash x = x.buf_id
end)

let buffer_size buffer =
  buffer.buf_elemsize * buffer.buf_size

type mapping =
    { map_task: string;
      map_core: int }

let print_mapping out_f map =
  fprintf out_f "Map \"%s\" %d\n" map.map_task map.map_core

type spec = (task list * dependency list * buffer list * mapping list)

let print_spec out_f spec =
  fprintf stdout "TFF-2.0\n";
  let (tasks,deps,bufs,mapping) = spec in
  List.iter (fun t -> print_task out_f t) tasks;
  List.iter (fun d -> print_dependency out_f d) deps;
  List.iter (fun b -> print_buffer out_f b) bufs;
  List.iter (fun m -> print_mapping out_f m) mapping
