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

let version = "0.1"
let outname = ref "main"
let outdir = ref ""
let inname = ref ""
let node = ref ""
let opt = ref 0
let use_cache = ref false
let tff = ref false
let dot = ref false
let eps = ref false
let verbose = ref false

let options =
  [ "-out", Arg.Set_string outname, " Specifies the name for output files";
    "-d", Arg.Set_string outdir, " Specifies the directory for output files";
    "-dot", Arg.Set dot, " Dump task graph to .dot file";
    "-verbose", Arg.Set verbose, " Output detailed messages about what the compiler is doing";
    "-version", Arg.Unit (fun () -> print_endline (Sys.argv.(0) ^ " " ^ version); exit 0), " Display the version" ]

let usage = Sys.argv.(0) ^ " <options> [<file>]"

let parse =
  Arg.parse options
    (fun x ->
      if !inname = "" then
        inname := x
      else
        raise (Arg.Bad "Cannot handle more than one input file"))
    usage
