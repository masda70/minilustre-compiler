open Ast
open Typed_ast
open Imp_ast

let rec untulify el =
  List.fold_right
    (fun e acc -> 
      match e.texpr_desc with
      | TE_tuple el -> (untulify el @ acc)
      | _ -> e :: acc)
    el []

let empty_mem = { fby_mem = []; node_mem = [] }

let empty_init = { fby_init = []; node_init = [] }

let gen_next_id =
  let cpt = ref 0 in
  fun n -> incr cpt; n^"_next"^(string_of_int !cpt)

let gen_mem_id =
  let cpt = ref 0 in
  fun n -> incr cpt; n^"_mem"^(string_of_int !cpt)

let compile_patt {tpatt_desc = d ; tpatt_type = t} = 
  try List.combine d t with Invalid_argument _ -> assert false
    
let rec compile_base_expr e = 
  let desc = 
    match e.texpr_desc with
    | TE_const c -> ME_const c
    | TE_ident x -> ME_ident x
    | TE_unop (op, e) -> ME_unop(op, compile_base_expr e)
    | TE_binop (op, e1, e2) ->  
	let ce1 =  compile_base_expr e1 in
	let ce2 =  compile_base_expr e2 in
	ME_binop (op, ce1, ce2)
    | TE_if (e1, e2, e3) -> 
	let ce1 =  compile_base_expr e1 in
	let ce2 =  compile_base_expr e2 in
	let ce3 =  compile_base_expr e3 in
	ME_if (ce1, ce2, ce3)
    | TE_tuple el -> ME_tuple (List.map compile_base_expr el)
    | TE_print el -> ME_print (List.map compile_base_expr el)
    | TE_fby _ -> assert false (* impossible car en forme normale *)
    | TE_app _ -> assert false (* impossible car en forme normale *)
    | TE_prim(f, el) -> 
	let _, f_out_ty = 
	  try List.assoc f Typing.Delta.prims 
	  with Not_found -> 
	    Printf.fprintf stderr "not a prim : %s" f; 
	    assert false
	in
	ME_prim(f, List.map compile_base_expr el, List.length f_out_ty)
  in
  { mexpr_desc = desc; mexpr_type = e.texpr_type; }

let compile_atom a = 
  match a.texpr_desc with
  | TE_const c -> Const c
  | TE_ident x -> Ident x
  | _ -> assert false

let compile_atoms a = 
  match a.texpr_desc with
  | TE_tuple el -> List.map compile_atom el
  | TE_const _ | TE_ident _ -> [compile_atom a]
  | _ -> assert false

let compile_equation 
    {teq_patt = p; teq_expr = e} (mem_acc,init_acc,compute_acc,update_acc) =
  let tvars = compile_patt p in 
  match e.texpr_desc with
  | TE_fby(e1,e2) ->
      let next_ids = List.map gen_next_id p.tpatt_desc in
      let tnext_ids = List.combine next_ids p.tpatt_type in
      let fby_init = List.combine next_ids e1 in
      let compute =
	let cel = 
	  List.map2
	    (fun x t -> { mexpr_desc = ME_mem x; mexpr_type = [t]; })
	    next_ids p.tpatt_type
	in 
	let expr = 
	  match cel with 
	    [ce] -> ce
	  | l -> { mexpr_desc = ME_tuple l; 
		   mexpr_type = e.texpr_type }
	in
	{ meq_patt = tvars ; meq_expr = expr }
      in
      let update = List.combine next_ids (compile_atoms e2) in      
      { mem_acc with fby_mem = tnext_ids@mem_acc.fby_mem } , 
      { init_acc with fby_init = fby_init@init_acc.fby_init } , 
      compute::compute_acc , update@update_acc

  | TE_app(n,el) ->
      let mem_id = gen_mem_id n in
      let node_mem = [mem_id, n] in
      let step_in = (untulify el) in
      let node_init = [mem_id, n] in
      let compute = 
	let expr = 
	  { mexpr_desc = 
	      ME_app (n, mem_id, 
		      List.map compile_base_expr step_in); 
	    mexpr_type = 
	      List.map 
	        (function { texpr_type = [t] } -> t
		  | _ -> assert false) 
	        step_in; }
	in
	{ meq_patt = tvars ; meq_expr = expr }
      in
      { mem_acc with node_mem = node_mem@mem_acc.node_mem } , 
      { init_acc with node_init = node_init@init_acc.node_init } , 
      compute::compute_acc , update_acc

  | _ ->
      let eq = {meq_patt = tvars; meq_expr = compile_base_expr e} in
      mem_acc, init_acc, eq::compute_acc, update_acc


let compile_equations l = 
  List.fold_right compile_equation l (empty_mem,empty_init,[],[])

let compile_node n = 
  let input_step = n.tn_input in
  let output_step = n.tn_output in
  let (mem , init , compute , update) = compile_equations n.tn_equs in
  { mn_name = n.tn_name;
    mn_input_step = input_step;
    mn_output_step = output_step;
    mn_local = n.tn_local;
    mn_mem = mem; 
    mn_init = init;
    mn_compute = compute;
    mn_update = update }

let compile = List.map compile_node 

let gen_node_id =
  let cpt = ref 0 in
  fun s -> incr cpt ; s^"'_"^(string_of_int !cpt)

let rename_expr env e =
  match e.mexpr_desc with
  | ME_app(f,mem,args) -> 
      { e with mexpr_desc = ME_app(List.assoc f env, mem, args) }
  | _ -> e

let rename_equation env eq =
  { eq with meq_expr = rename_expr env eq.meq_expr; }

let rename_node env n =
  let id = gen_node_id n.mn_name in
  let mem =
    { n.mn_mem with 
      node_mem = 
        List.map (fun (x,t) -> (x, List.assoc t env)) n.mn_mem.node_mem; }
  in
  let init =
    { n.mn_init with
      node_init = 
        List.map 
	  (fun (x,f) -> (x, List.assoc f env)) 
	  n.mn_init.node_init; }
  in
  let compute =
    List.map (rename_equation env) n.mn_compute
  in
  ((n.mn_name, id)::env), 
  { n with mn_name = id;
           mn_mem = mem; 
           mn_init = init;
           mn_compute = compute; }

let rename_nodes f main =
  let env , f' =
    List.fold_left 
      (fun (env,f) n -> let env', n' = rename_node env n in (env', n'::f))
      ([],[]) f
  in
  main := (try List.assoc !main env with Not_found -> "");
  List.rev f'
