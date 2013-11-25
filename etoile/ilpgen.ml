open Math
open Spec
open Printf

type job =
    { job_task: task;
      job_id: int;
      job_r: int;
      job_d: int;
      job_r': int ref;
      job_d': int ref }

type jobdep = (job * job)

let rec gen_job hp task time id =
  if time >= hp then [] else
  { job_task = task;
    job_id = id;
    job_r = time;
    job_d = time + task.deadline;
    job_r' = ref (- 1);
    job_d' = ref (- 1)} ::
  gen_job hp task (time + task.period) (id + 1)

let gen_jobs hp task =
  gen_job hp task task.offset 0

let rec gen_dword (src, dst) lcm preds succs =
  match preds with
  | [] -> []
  | pred :: pred_tail ->
      (* Look for matching predecessor job *)
      let pred_id = pred.job_id mod (lcm / pred.job_task.period) in
      if pred_id <> src then
	gen_dword (src, dst) lcm pred_tail succs
      else
	match succs with
	| [] -> []
	| succ :: succ_tail ->
         (* Look for matching successor job *)
	  let succ_id = succ.job_id mod (lcm / succ.job_task.period) in
	  if succ_id <> dst then
	    gen_dword (src, dst) lcm preds succ_tail
	  else
	    (* Found match, continue *)
	    (pred, succ) :: (gen_dword (src, dst) lcm pred_tail succ_tail)

let _dep_srcs = Hashtbl.create 10
let dep_srcs dep jobs =
  try Hashtbl.find _dep_srcs dep
  with Not_found ->
	let srcs = List.filter (fun j -> dep.dep_src = j.job_task.name) jobs in
	Hashtbl.add _dep_srcs dep srcs;
	srcs

let _dep_dsts = Hashtbl.create 10
let dep_dsts dep jobs =
  try Hashtbl.find _dep_dsts dep
  with Not_found ->
	let dsts = List.filter (fun j -> dep.dep_dst = j.job_task.name) jobs in
	Hashtbl.add _dep_dsts dep dsts;
	dsts

let gen_dep dep jobs =
  let preds = dep_srcs dep jobs and succs = dep_dsts dep jobs in
  if preds = [] || succs = [] then [] else
  let lcm = lcm (List.hd preds).job_task.period (List.hd succs).job_task.period in
  List.fold_left (fun r w -> List.append r (gen_dword w lcm preds succs)) [] dep.dep_pat

let gen_deps deps jobs =
  List.fold_left (fun r d -> List.append r (gen_dep d jobs)) [] deps

let _job_preds = Hashtbl.create 100
let job_preds job jobdeps = 
  let id = (job.job_task.id, job.job_id) in
  try Hashtbl.find _job_preds id
  with Not_found ->
    let preds = List.fold_left (fun r (src, dst) ->
	  if (dst.job_task.id, dst.job_id) = id then
		src :: r else r) [] jobdeps in
    Hashtbl.add _job_preds id preds;
    preds    

let _job_succs = Hashtbl.create 100
let job_succs job jobdeps = 
  let id = (job.job_task.id, job.job_id) in
  try Hashtbl.find _job_succs id
  with Not_found ->
    let succs = List.fold_left (fun r (src, dst) ->
	  if (src.job_task.id, src.job_id) = id then
		dst :: r else r) [] jobdeps in
    Hashtbl.add _job_succs id succs;
    succs

let rec resolve_job_r job jobs jobdeps =
  if !(job.job_r') < 0 then
    let preds = job_preds job jobdeps in
    List.fold_left (fun r p ->
      let p_r = resolve_job_r p jobs jobdeps in
      max r (p_r + p.job_task.wcet)) job.job_r preds
  else
    !(job.job_r')

let rec resolve_job_d job jobs jobdeps =
  if !(job.job_d') < 0 then
    let succs = job_succs job jobdeps in
    List.fold_left (fun r s ->
      let s_d = resolve_job_d s jobs jobdeps in
      min r (s_d - s.job_task.wcet)) job.job_d succs
  else
    !(job.job_d')

let refine_jobs jobs jobdeps =
  List.iter (fun j -> j.job_r' := resolve_job_r j jobs jobdeps) jobs;
  List.iter (fun j -> j.job_d' := resolve_job_d j jobs jobdeps) jobs;
  List.iter (fun j -> if !(j.job_r') + j.job_task.wcet > !(j.job_d') then
    failwith "Task set not schedulable (r' + C > d')") jobs

let rec precedes jobdeps j k =
  let succs = job_succs j jobdeps in
  if List.memq k succs then
    true
  else
    List.fold_left (fun r s -> if not r then precedes jobdeps s k else r) false succs

let ordered jobdeps j k =
  (precedes jobdeps j k) || (precedes jobdeps j k)

let _task_jobs = Hashtbl.create 10
let task_jobs jobs task =
  try Hashtbl.find _task_jobs task.id
  with Not_found ->
	let jobs = List.filter (fun j -> j.job_task.id = task.id) jobs in
	Hashtbl.add _task_jobs task.id jobs;
	jobs

let job_conflict j k =
  let r = min !(j.job_r') !(k.job_r')
  and d = max !(j.job_d') !(k.job_d') in
  (j.job_task.wcet + k.job_task.wcet) > (d - r)

let _conflict = Hashtbl.create 100
let conflict jobs t s =
  let id = (t.id, s.id) in
  try Hashtbl.find _conflict id
  with Not_found ->
	if (Hashtbl.length _conflict) > 10000000 then Hashtbl.clear _conflict;
	if (t.wcet > (s.period + s.deadline - 2 * s.wcet)
		|| s.wcet > (t.period + t.deadline - 2 * t.wcet)) then
      begin
		Hashtbl.add _conflict id true;
		true
      end
	else
      begin
		let t_jobs = task_jobs jobs t
		and s_jobs = task_jobs jobs s in
		let conf =
		  List.fold_left (fun res j ->
			if res then true else
			  List.fold_left (fun r k ->
				if r then true else
				  job_conflict j k) res s_jobs) false t_jobs in
		Hashtbl.add _conflict id conf;
		conf
      end

let overlap j k =
  (!(j.job_r') >= !(k.job_r') && !(j.job_r') < !(k.job_d'))
   || (!(k.job_r') >= !(j.job_r') && !(k.job_r') < !(j.job_d'))

let before j k =
  !(j.job_d') - j.job_task.wcet < !(k.job_r') + k.job_task.wcet

let subseteq x y =
  List.fold_left (fun r e -> if r then List.memq e y else r) true x

let equiv_subsets jobdeps j k =
  if (!(j.job_r') > !(k.job_r')
    || !(j.job_d') > !(k.job_d')
    || j.job_task.wcet <> k.job_task.wcet) then
    false
  else
    let j_preds = job_preds j jobdeps
    and j_succs = job_succs j jobdeps
    and k_preds = job_preds k jobdeps
    and k_succs = job_succs k jobdeps in
    if (subseteq j_preds k_preds) && (subseteq k_succs j_succs) then
      true
    else
      false

let buffer_taskname str =
  String.sub str 0 (String.rindex str '.')

let util t = (float t.wcet) /. (float t.period)

let dump out_f tasks deps buffers mapping =
  let util_ceil = (int_of_float (ceil (List.fold_left (fun r t -> r +. (util t)) 0.0 tasks))) in
  if util_ceil > 1 then
    failwith "Task set not schedulable (not enough cores for utilization)";

  let (maxoff,hyper) = 
    List.fold_left (fun (o,p) t -> (max o t.offset, lcm p t.period)) (0, 1) tasks in
  let hp = maxoff + 2*hyper in

  let tasks = List.sort (fun t s -> compare (task_util s) (task_util t)) tasks in

  let initjobs = List.concat (List.map (fun t -> gen_jobs hp t) tasks) in
  let initjobdeps = gen_deps deps initjobs in
  
  refine_jobs initjobs initjobdeps;

  let jobs = List.sort (fun j k -> compare !(j.job_d') !(k.job_d'))
      (List.filter (fun j -> !(j.job_r') < hp) initjobs) in
  let jobdeps = List.filter (fun (j, k) -> !(j.job_r') < hp && !(k.job_r') < hp) initjobdeps in

  refine_jobs jobs jobdeps;

  (* let min_prod = ref (List.fold_left (fun r t ->  *)
  (* 	r *. (float (t.wcet + 1))) *)
  (* 	1.0 tasks) in *)
  (* fprintf stderr "maximum min_prod: %f\n" !min_prod; *)
  (* for i = maxoff to maxoff+hyper do *)
  (* 	let crossing = List.fold_left (fun r j -> *)
  (* 	  if !(j.job_r') < i && !(j.job_d') > i then j :: r else r) [] jobs in *)
  (* 	let prod = List.fold_left (fun r j -> *)
  (* 	  r *. (float ((min j.job_task.wcet (min (i - !(j.job_r')) (!(j.job_d') - i))) + 1))) *)
  (* 	  1.0 crossing in *)
  (* 	if prod < !min_prod then *)
  (* 	  min_prod := prod; *)

  (* 	fprintf stderr "prod[%d](%d): %f // %f\n" i (List.length crossing) prod !min_prod *)
  (* done; *)

  fprintf out_f "maxOff = %d;\n" maxoff;
  fprintf out_f "hyper = %d;\n" hyper;

  fprintf out_f "Tasks = {\n";
  List.iter (fun t -> fprintf out_f "  \"%s\",\n" t.name) tasks;
  fprintf out_f "};\n";
  fprintf out_f "TaskProps = #[\n";
  List.iter (fun t -> fprintf out_f "  \"%s\":<%d, %d>,\n" t.name t.period t.wcet) tasks;
  fprintf out_f "]#;\n";
  
  fprintf out_f "Jobs = {\n";
  List.iter (fun j -> fprintf out_f "  \"%s_%d\",\n" j.job_task.name j.job_id) jobs;
  fprintf out_f "};\n";
  fprintf out_f "JobProps = #[\n";
  List.iter (fun j -> fprintf out_f "  \"%s_%d\":<\"%s\", %d, %d>,\n"
      j.job_task.name j.job_id j.job_task.name !(j.job_r') !(j.job_d')) jobs;
  fprintf out_f "]#;\n";

  fprintf out_f "Deps = {\n";
  List.iter (fun (src, dst) -> fprintf out_f "  <\"%s_%d\", \"%s_%d\">,\n"
      src.job_task.name src.job_id dst.job_task.name dst.job_id) jobdeps;
  fprintf out_f "};\n";

  fprintf out_f "Bufs = {\n";
  List.iter (fun b -> fprintf out_f "  \"%s_%s\",\n"
      b.buf_src b.buf_dst) buffers;
  fprintf out_f "};\n";

  if (List.length buffers) = 0 then
    fprintf out_f "BufProps = [ ];\n"
  else
    begin
      fprintf out_f "BufProps = #[\n";
      List.iter (fun b -> fprintf out_f "  \"%s_%s\":<\"%s\", \"%s\", %d>,\n"
          b.buf_src b.buf_dst
          (buffer_taskname b.buf_src) (buffer_taskname b.buf_dst)
          (b.buf_elemsize * b.buf_size)) buffers;
      fprintf out_f "]#;\n";
    end;
  flush out_f;

  fprintf out_f "Hypers = {\n";
  List.iter (fun j -> 
    if !(j.job_r') < maxoff && !(j.job_d') > maxoff then
      let l = List.find (fun l -> l.job_task.id = j.job_task.id && !(l.job_r') < hp && !(l.job_d') > hp) jobs in
      fprintf out_f "<\"%s_%d\", \"%s_%d\">,\n"
        j.job_task.name j.job_id l.job_task.name l.job_id
  ) jobs;
  fprintf out_f "};\n";
  flush out_f;

  let confl_edges = ref [] in
  List.iter (fun t ->
    List.iter (fun s ->
      if t.id < s.id then
		if conflict jobs t s then
          confl_edges := (t, s) :: !confl_edges
  ) tasks) tasks;

  let clqs = Cliques.cliques tasks !confl_edges in
  let max_clq =
    if clqs = [] then [] else
    List.hd (List.sort (fun x y -> compare (List.length y) (List.length x)) clqs) in
  if (List.length max_clq) > 1 then
    failwith "Task set not schedulable (too few cores for maximum clique)";
  confl_edges := [];

  fprintf out_f "Cliques = {\n";
  List.iter (fun c ->
    if List.length c > 1 then
      begin
	    fprintf out_f "<{ ";
        List.iter (fun t ->
	      fprintf out_f "\"%s\", " t.name
            ) c;
	    fprintf out_f "}>,\n"
      end
  ) clqs;
  fprintf out_f "};\n";
  flush out_f;

  fprintf out_f "Unordered = {\n";
  let ord = ref [] in
  List.iter (fun j ->
    List.iter (fun k ->
      if j.job_task.id < k.job_task.id
		&& not (conflict jobs j.job_task k.job_task)
        && (overlap j k)
		&& not (ordered jobdeps j k) then
		begin
		  if (before j k) || (equiv_subsets jobdeps j k) then
		    ord := (j, k) :: !ord
	      else if (before k j) || (equiv_subsets jobdeps k j) then
		    ord := (k, j) :: !ord
          else
            fprintf out_f "  <\"%s_%d\", \"%s_%d\">,\n"
              j.job_task.name j.job_id k.job_task.name k.job_id
        end
    ) jobs;
  ) jobs;
  fprintf out_f "};\n";

  fprintf out_f "Ordered = {\n";
  List.iter (fun (j, k) -> 
  	fprintf out_f "  <\"%s_%d\", \"%s_%d\">, // %s\n"
  	  j.job_task.name j.job_id k.job_task.name k.job_id
      (if before j k then "implied" else "imposed")) !ord;
  fprintf out_f "};\n";

  fprintf out_f "// Tasks: %d, Jobs: %d, Precs: %d, O_max: %d, H: %d, Util: %f\n"
    (List.length tasks) (List.length jobs) (List.length jobdeps)
    maxoff hyper (utilization tasks);
 
  flush out_f
