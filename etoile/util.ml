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

open Big_int

let fname dir name =
  if dir = "" then
    name
  else
    dir^"/"^name

let drop_option optv =
  match optv with
  | Some v -> v
  | None -> failwith "Missing value for option variable"

let list_remove l elem =
  List.filter (fun e -> e <> elem) l
