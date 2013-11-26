/* ----------------------------------------------------------------------------
 * SchedMCore - A MultiCore Scheduling Framework
 * Copyright (C) 2012, ONERA, Toulouse, FRANCE
 *
 * This file is part of Etoile
 *
 * Etoile is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * Etoile is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
 * USA
 *---------------------------------------------------------------------------- */

%{

open Spec

let task_id = ref (0)
let buffer_id = ref (0)

%}

%token <int> INT
%token <string> STRING
%token MAGIC_V2
%token TASK DEPENDENCY COMBUFFER MAP
%token LPAR RPAR COMMA COLON DEF NEWLINE

%token EOF

%start task_specs
%type <Spec.spec> task_specs

%%

task_specs: MAGIC_V2 spec_list EOF {
      let (t,d,b,m) = $2 in ((List.rev t), (List.rev d), (List.rev b), (List.rev m))
    }

spec_list: { ([],[],[],[]) }
    | spec_list task_spec_line {
      let (t,d,b,m) = $1 in ($2::t,d,b,m)
    }
    | spec_list depend_spec_line {
        let (t,d,b,m) = $1 in
        if not (List.mem $2 d) then
          (t,$2::d,b,m)
        else
          $1
    }
    | spec_list combuffer_spec_line {
      let (t,d,b,m) = $1 in (t,d,$2::b,m)
    }
    | spec_list map_spec_line {
      let (t,d,b,m) = $1 in (t,d,b,$2::m)
    }

task_spec_line: TASK STRING INT INT INT deadline_spec {
      task_id := !task_id + 1;
      { name = $2; period = $3; wcet = $4; offset = $5; deadline = $6;
        userfun = $2 ^ "_fun"; id = !task_id }
    }
    | TASK STRING DEF STRING INT INT INT deadline_spec {
      task_id := !task_id + 1;
      { name = $2; period = $5; wcet = $6; offset = $7; deadline = $8;
        userfun = $4; id = !task_id }
    }

deadline_spec: deadline_prefix LPAR deadline_pattern RPAR {
      min $1 $3
    } 

deadline_prefix: { max_int }
    | deadline_prefix INT COMMA {
      min $1 $2
    } 

deadline_pattern: INT {
      $1
    }
    | deadline_pattern COMMA INT {
      min $1 $3
    } 

dword_set: INT COLON INT {
      [($1, $3)]
    }
    | dword_set COMMA INT COLON INT {
      ($3, $5)::$1
    }

depend_spec_line: DEPENDENCY STRING STRING LPAR dword_set RPAR {
      { dep_src = $2; dep_dst = $3; dep_pref = []; dep_pat = (List.rev $5) }
    }

combuffer_spec_line: COMBUFFER STRING STRING INT INT {
      buffer_id := !buffer_id + 1;
      { buf_src = $2; buf_dst = $3; buf_elemsize = $4; buf_size = $5;
        buf_init = None; buf_id = !buffer_id }
    }
    | COMBUFFER STRING STRING INT INT DEF STRING {
      buffer_id := !buffer_id + 1;
      { buf_src = $2; buf_dst = $3; buf_elemsize = $4; buf_size = $5;
        buf_init = Some $7; buf_id = !buffer_id }
    }

map_spec_line: MAP STRING INT {
      { map_task = $2; map_core = $3 }
    }

%%
