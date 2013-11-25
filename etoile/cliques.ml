(* Written by Pietro Abate, 2010 *)

open Graph

(*
The Bron–Kerbosch algorithm is an algorithm for finding maximal cliques in an undirected graph.
http://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm

   BronKerbosch1(R,P,X):
       if P and X are both empty:
           report R as a maximal clique
       for each vertex v in P:
           BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
           P := P \ {v}
           X := X ⋃ {v}

   BronKerbosch2(R,P,X):
       if P and X are both empty:
           report R as a maximal clique
       choose a pivot vertex u in P ⋃ X
       for each vertex v in P \ N(u):
           BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
           P := P \ {v}
           X := X ⋃ {v}
*)

module V = struct
  type t = Spec.task
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module UG = Persistent.Graph.Concrete(V)
module N = Oper.Neighbourhood(UG)
module S = N.Vertex_Set

let rec bronKerbosch1 gr r p x =
  let n v = N.set_from_vertex gr v in
  if (S.is_empty p) && (S.is_empty x) then [r]
  else
    let (_,_,mxc) =
      S.fold (fun v (p,x,acc) ->
        let r' = S.union r (S.singleton v) in
        let p' = S.inter p (n v) in
        let x' = S.inter x (n v) in
        (S.remove v p, S.add v x, (bronKerbosch1 gr r' p' x') @ acc)
      ) p (p,x,[])
    in mxc

let rec bronKerbosch2 gr r p x =
  let n v = N.set_from_vertex gr v in
  if (S.is_empty p) && (S.is_empty x) then [r]
  else
    let u = S.choose (S.union p x) in
    let (_,_,mxc) =
      S.fold (fun v (p,x,acc) ->
        let r' = S.union r (S.singleton v) in
        let p' = S.inter p (n v) in
        let x' = S.inter x (n v) in
        (S.remove v p, S.add v x,(bronKerbosch2 gr r' p' x') @ acc)
      ) (S.diff p (n u)) (p,x,[])
    in mxc

let cliques tasks edges =
  let gr = List.fold_left (fun g v -> UG.add_vertex g v) UG.empty tasks in
  let gr = List.fold_left (fun g (x,y) -> UG.add_edge g x y) gr edges in
  let r = S.empty 
  and p = List.fold_right S.add tasks S.empty
  and x = S.empty in
  List.map S.elements (bronKerbosch2 gr r p x)
