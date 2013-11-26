/* ----------------------------------------------------------------------------
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
 *---------------------------------------------------------------------------- */

%{

open Spec
open Printf

let period = ref (-1)

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
      let (t,d) = $2 in ((List.rev t), (List.rev d))
    }

spec_list: { ([],[]) }
    | spec_list task_spec_line {
      let (t,d) = $1 in ($2::t,d)
    }
    | spec_list depend_spec_line {
      let (t,d) = $1 in (t,$2::d)
    }
    | spec_list combuffer_spec_line {
      let (t,d) = $1 in (t,d)
    }
    | spec_list map_spec_line {
      let (t,d) = $1 in (t,d)
    }

task_spec_line: TASK STRING INT INT INT deadline_spec {
      if !period >= 0 && $3 <> !period then
        fprintf stderr "Error: Different periods (%d vs %d) not supported\n" !period $3
      else
        period := $3;
      { name = $2; period = $3; wcet = $4; offset = $5; deadline = $6; userfun = $2 ^ "_fun" }
    }
    | TASK STRING DEF STRING INT INT INT deadline_spec {
      { name = $2; period = $5; wcet = $6; offset = $7; deadline = $8; userfun = $4 }
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
      if $5 <> [(0, 0)] then
        fprintf stderr "Error: Extended precedence (\"%s\" -> \"%s\") not supported\n" $2 $3;
      { dep_src = $2; dep_dst = $3; dep_pref = []; dep_pat = (List.rev $5) }
    }

combuffer_spec_line: COMBUFFER STRING STRING INT INT {
    }
    | COMBUFFER STRING STRING INT INT DEF STRING {
    }

map_spec_line: MAP STRING INT {
    }

%%
