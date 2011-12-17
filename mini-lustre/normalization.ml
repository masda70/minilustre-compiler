open Typed_ast

let new_local = 
  let cpt = ref 0 in fun () -> incr cpt; "aux'"^(string_of_int !cpt)

(** [new_pat e] prend en argument une expression [e] et retourne une
    variable (ou une liste de variable si [e] est un tuple) du même
    type que [e]. 

    Plus précisément, cette fonction retourne un triplet [(decl, patt, expr)], 
    où
    [decl] est la déclaration de la variable (ou une liste de déclaration 
    si [e] est un tuple),
    [patt] est la variable (ou le tuple de variables) vue comme un motif et
    [expr] est la variable (ou le tuple de variables) vue comme une expression.
*)  
let new_pat ({ texpr_type= ty; texpr_loc = loc } as e) = 
  let lr , pat , ex = 
    match ty with
    | [t] -> 
	let r = new_local() in 
	let ne = { e with texpr_desc = TE_ident r } in
	[r,t] , [r] , ne
    | lt -> 
	let l = List.map (fun _ -> new_local()) lt in
	let lr = List.combine l lt in
	let vars = List.combine l lt in
	let ne = 
	  List.map 
	    (fun (x,t) ->  
	      { texpr_desc = TE_ident x;
		texpr_type = [t];
		texpr_loc = loc} ) vars 
	in
	lr , l , { e with texpr_desc = TE_tuple ne }
  in 
  lr , { tpatt_desc = pat; tpatt_type = ty; tpatt_loc = loc } , ex

(** [normalize ctx e] met l'expression [e] en forme normale et ajoute
    à [ctx] les équations introduites lors de la normalisation. 
*)
let rec normalize ctx e = 
  match e.texpr_desc with
  | TE_const _ | TE_ident _ -> ctx , e
	
  | TE_unop(op,e1) -> 
      let ctx , e'1 = normalize ctx e1 in
      ctx , { e with texpr_desc = TE_unop(op,e'1) }
	
  | TE_binop(op,e1,e2) ->
      let ctx , e'1 = normalize ctx e1 in
      let ctx , e'2 = normalize ctx e2 in
      ctx , { e with texpr_desc = TE_binop(op,e'1,e'2) }
        
  | TE_app(n,le) ->
      let (new_vars,new_eqs) , le1 = normalize_list ctx le in
      let lv, p, e' = new_pat e in
      let app = { e with texpr_desc = TE_app(n,le1) } in
      (lv@new_vars, {teq_patt = p; teq_expr = app}::new_eqs), e'
        
  | TE_prim(n,le) ->
      let (new_vars,new_eqs) , le1 = normalize_list ctx le in
      let lv, p, e' = new_pat e in
      let app = { e with texpr_desc = TE_prim(n,le1) } in
      (lv@new_vars, {teq_patt = p; teq_expr = app}::new_eqs), e'
        
  | TE_print le ->
      let (new_vars,new_eqs) , le1 = normalize_list ctx le in
      let lv, p, e' = new_pat e in
      let app = { e with texpr_desc = TE_print(le1) } in
      (lv@new_vars, {teq_patt = p; teq_expr = app}::new_eqs), e'
        
  | TE_if(e1,e2,e3) ->
      let ctx , e'1 = normalize ctx e1 in
      let ctx , e'2 = normalize ctx e2 in
      let ctx , e'3 = normalize ctx e3 in
      ctx , { e with texpr_desc = TE_if(e'1,e'2,e'3) }
	
  | TE_tuple l ->
      let ctx , l' = normalize_list ctx l in 
      ctx , { e with texpr_desc = TE_tuple l'}
	
  | TE_fby(c,e1) -> 
      let (new_vars,new_eqs) , e'1 = normalize ctx e1 in
      let lr1 , pat1 , ex1 = new_pat e'1 in
      let ne1 = { teq_patt = pat1; teq_expr = e'1 } in
      let e'2 = { e with texpr_desc = TE_fby(c,ex1) } in
      let lr2 , pat2 , ex2 = new_pat e'2 in
      let ne2 = { teq_patt = pat2; teq_expr = e'2 } in
      (lr1@lr2@new_vars , ne1::ne2::new_eqs) , ex2
        

and normalize_list ctx l = 
  let ctx , l = 
    List.fold_left 
      (fun (ctx,l) e -> 
	let ctx , e' = normalize ctx e in
	ctx , e'::l ) (ctx,[]) l 
  in ctx , List.rev l
  
let normalize_equation node e = 
  let (locals, new_eqs) , e' = normalize ([],[]) e.teq_expr in 
  { node with 
    tn_local = locals@node.tn_local;
    tn_equs = { e with teq_expr = e' } :: (List.rev new_eqs) @ node.tn_equs }
    
let file =  
  List.map 
    (fun n -> 
      let n = 
	List.fold_left normalize_equation { n with tn_equs=[] } n.tn_equs 
      in 
      { n with tn_equs = List.rev n.tn_equs })

