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

open Printf

type task =
    { name: string;
      period: int;
      wcet: int;
      offset: int;
      deadline: int;
      userfun: string }

let print_task out_f task =
  fprintf out_f "Task \"%s\" := \"%s\" %d %d %d (%d)\n"
    task.name task.userfun task.period task.wcet task.offset task.deadline


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


type spec = (task list * dependency list)

let spec_magic = "TFF-2.0\n"

let print_spec out_f spec =
  fprintf out_f "%s" spec_magic;
  let (tasks,deps) = spec in
  List.iter (fun t -> print_task out_f t) tasks;
  List.iter (fun d -> print_dependency out_f d) deps
