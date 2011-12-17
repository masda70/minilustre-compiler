open Asttypes
open Ast
open Typed_ast
open Format


module S = Set.Make(String)
module M = Map.Make(String)

type error =
	| ExpectedClockIdentifier
	| ExpectedClock of clock * clock
	| ExpectedBaseType of aty * base_ty
	| ExpectedType of aty * aty
	| ExpectedPattern of ty
	| ExpectedSingleType of ty
	| ExpectedNum of ty
	| UnboundVar of string
	| UnboundNode of string
	| TooFewArguments
	| TooManyArguments
	| Clash of string
	| ConstantExpected
	| Other of string
	| FlatTuple
	| UndefinedOutputs of string list
	| InputVar of string
	| Causality
	| BadMain of ty * ty

exception Error of location * error
let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let error loc e = raise (Error (loc, e))
let errors loc s = error loc (Other s)

let rec print_list f sep fmt l = match l with
  | [] -> ()
  | [x] -> f fmt x
  | h :: t -> fprintf fmt "%a%s@ %a" f h sep (print_list f sep) t

let rec print_list_eol f sep fmt l = match l with
  | [] -> ()
  | [x] -> fprintf fmt "%a%s" f x sep
  | h :: t -> fprintf fmt "%a%s@\n%a" f h sep (print_list_eol f sep) t

let print_const fmt c = match c with
  | Cunit -> fprintf fmt "()"
  | Cbool b -> fprintf fmt "%b" b
  | Cint i -> fprintf fmt "%d" i
  | Cfloat f -> fprintf fmt "%f" f
  | Cstring s -> fprintf fmt "\"%s\"" s

let print_unop fmt op = match op with
  | Unot -> fprintf fmt "~"
  | Uminus -> fprintf fmt "-"
  | Uminus_f -> fprintf fmt "-."

let print_binop fmt op = match op with
  | Beq -> fprintf fmt "eq"
  | Bneq -> fprintf fmt "neq"
  | Blt -> fprintf fmt "lt"
  | Ble -> fprintf fmt "le"
  | Bgt -> fprintf fmt "gt"
  | Bge -> fprintf fmt "ge"
  | Badd -> fprintf fmt "add"
  | Bsub -> fprintf fmt "sub"
  | Bmul -> fprintf fmt "mul"
  | Bdiv -> fprintf fmt "div"
  | Bmod -> fprintf fmt "mod"
  | Badd_f -> fprintf fmt "add_f"
  | Bsub_f -> fprintf fmt "sub_f"
  | Bmul_f -> fprintf fmt "mul_f"
  | Bdiv_f -> fprintf fmt "div_f"
  | Band -> fprintf fmt "and"
  | Bor -> fprintf fmt "or"

let print_name fmt name = fprintf fmt "%s" name

let rec print_exp fmt e = match e.pexpr_desc with
  | PE_const c -> print_const fmt c
  | PE_ident s -> fprintf fmt "%s" s
  | PE_unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_exp e
  | PE_binop (op, l, r) ->
      fprintf fmt "(@[%a %a@ %a@])" print_binop op print_exp l print_exp r
  | PE_app (name, e_list)  ->
      fprintf fmt "%a(@[%a@])" print_name name print_arg_list e_list
  | PE_if (cond, cons, altr) ->
      fprintf fmt "@[if %a@ then %a@ else %a@]"
        print_exp cond
        print_exp cons
        print_exp altr
  | PE_fby (l, r) ->
      fprintf fmt "@[(@[%a@]) fby %a@]" print_exp l print_exp r
  | PE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list
	| PE_when (e1,e2) ->
      fprintf fmt "@[%a@ when %a@]" print_exp e1 print_exp e2
	| PE_merge (e1,e2,e3) ->
			fprintf fmt "@[merge %a@ %a@ %a@]" print_exp e1 print_exp e2 print_exp e3

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t


let print_base_type fmt = function
	| Tbool -> fprintf fmt "bool"
	| Tint -> fprintf fmt "int"
	| Tfloat -> fprintf fmt "float"
	| Tstring -> fprintf fmt "string"
	| Tunit -> fprintf fmt "unit"

let print_clock fmt = function
	| (BaseClock) -> fprintf fmt "base clock"
	| Clock(b,id) ->	
		fprintf fmt "on ";
		if b then fprintf fmt "not ";
		fprintf fmt "%s" id

let rec print_single_type fmt = function 
	| (ty,BaseClock)-> 
		print_base_type fmt ty;
	| (ty,Clock(b,id))->
		print_base_type fmt ty;
		fprintf fmt " ";
		print_clock fmt (Clock(b,id))

		
let rec print_annotated_single_type fmt = function 
	| id,s -> print_single_type fmt s

let print_annotated_type fmt = function
	| ([]) -> fprintf fmt "empty tuple"
	| [t] -> print_annotated_single_type fmt t
	| (t:: tl) ->
			fprintf fmt "(";
			print_annotated_single_type fmt t;
			List.iter (fun t -> fprintf fmt " * %a" print_annotated_single_type t) tl;
			fprintf fmt ")"

let print_type fmt = function
	| ([]) -> fprintf fmt "empty tuple"
	| [t] -> print_clocked_type fmt t
	| (t:: tl) ->
			fprintf fmt "(";
			print_single_type fmt t;
			List.iter (fun t -> fprintf fmt " * %a" print_single_type t) tl;
			fprintf fmt ")"

let report fmt = function
	| ExpectedClockIdentifier -> fprintf fmt "expected a clock identifier" id
	| UnboundVar id -> fprintf fmt "unbound variable %s" id
	| UnboundNode id -> fprintf fmt "unbound node %s" id
	| ExpectedClock (c1,c2) ->
			fprintf fmt
				"this expression has clock %a but is expected to have clock %a"
				print_clock c1 print_clock c2
	| ExpectedBaseType (at1, t2) ->
			fprintf fmt
				"this expression has type %a but is expected to have base type %a"
				print_annotated_type t1 print_base_type t2
	| ExpectedType (at1, at2) ->
			fprintf fmt
				"this expression has type %a but is expected to have type %a"
				print_annotated_type t1 print_annotated_type t2
	| ExpectedPattern ty ->
			fprintf fmt "this pattern is expected to have type %a"
				print_type ty
	| ExpectedSingleType ty ->
			fprintf fmt
				"this expression has type %a but is expected to have a type single type"
				print_type ty
	| ExpectedNum ty ->
			fprintf fmt
				"this expression has type %a but is expected to have type int or float"
				print_type ty
	| Clash id -> fprintf fmt "The variable %s is defined several times" id
	| TooFewArguments -> fprintf fmt "too few arguments"
	| TooManyArguments -> fprintf fmt "too many arguments"
	| ConstantExpected -> fprintf fmt "this expression sould be a constant"
	| Other s -> fprintf fmt "%s" s
	| FlatTuple -> fprintf fmt "nested tuples are forbidden"
	| UndefinedOutputs l ->
			fprintf fmt "those output variables are undefined:%a"
				(fun fmt -> List.iter (fun x -> fprintf fmt "%s " x)) l
	| InputVar s -> fprintf fmt "%s is an input variable" s
	| Causality -> fprintf fmt "problem of causality"
	| BadMain (t_in, t_out) ->
			fprintf fmt "The main node has type %a -> %a but is expected to have type () -> unit"
				print_type t_in print_type t_out

module Delta = struct
	
	let prims = [
		"int_of_float", ([Some("x"),(Tfloat,BaseClock)] , [Some("y"),(Tint,BaseClock)]) ;
		"float_of_string", ([Some("x"),(Tstring,BaseClock)] , [Some("y"),(Tfloat,BaseClock)]) ;
		"int_of_string", ([Some("x"),(Tstring,BaseClock)] , [Some("y"),(Tint,BaseClock)]) ;
		"bool_of_string", ([Some("x"),(Tstring,BaseClock)] , [Some("y"),(Tbool,BaseClock)]) ;
		"float_of_int", ([Some("x"),(Tint,BaseClock)] , [Some("y"),(Tfloat,BaseClock)]) ;
		"read", ([Some("x"),(Tunit,BaseClock)] , [Some("y"),(Tstring,BaseClock)]) ;
		"random_int", ([Some("x"),(Tint,BaseClock)] , [Some("y"),(Tint,BaseClock)]) ;
		"random_float", ([Some("x"),(Tfloat,BaseClock)] , [Some("y"),(Tfloat,BaseClock)]) ;
		"cos", ([Some("x"),(Tfloat,BaseClock)], [Some("y"),(Tfloat,BaseClock)]) ;
		"sin", ([Some("x"),(Tfloat,BaseClock)], [Some("y"),(Tfloat,BaseClock)]) ;
		"draw_point", ([Some("x"),(Tint,BaseClock); Some("y"),(Tint,BaseClock)], [Some("z"),(Tunit,BaseClock)]) ;
		"draw_line",
		([Some("x"),(Tint,BaseClock); Some("y"),(Tint,BaseClock); Some("z"),(Tint,BaseClock); Some("a"),(Tint,BaseClock)], [Some("b"),(Tunit,BaseClock)]) ;
		"draw_circle", ([Some("x"),(Tint,BaseClock);Some("y") ,(Tint,BaseClock); Some("z"),(Tint,BaseClock)], [Some("a"),(Tunit,BaseClock)]);
		"draw_rect",
		([Some("x"),(Tint,BaseClock); Some("y"),(Tint,BaseClock); Some("z"),(Tint,BaseClock);Some("a"), (Tint,BaseClock)],[Some("b"),(Tunit,BaseClock)]);
		"fill_rect",
		([Some("x"),(Tint,BaseClock); Some("y"),(Tint,BaseClock); Some("z"),(Tint,BaseClock); Some("a"),(Tint,BaseClock)],[Some("b"),(Tunit,BaseClock)]);
		"get_mouse", ([Some("x"),(Tunit,BaseClock)], [Some("y"),(Tint,BaseClock); Some("z"),(Tint,BaseClock)]) ]
	
	let nodes = Hashtbl.create 97
	
	let generate_print_vars n =
		if(n<0) then assert false
		else
			begin
				let l = ref [] in
				for i = 1 to n do
					[(string_of_int i),(Tunit,BaseClock)]::(!l)
				done;
				(!l,[(string_of_int (n+1)),(Tunit,BaseClock)])
			end
		 
	
	let is_primitive f = List.mem_assoc f prims
	
	let is_print f = (f = "print")
	
	let find n =
		try Hashtbl.find nodes n , false with
			Not_found -> List.assoc n prims , true
	
	let add = Hashtbl.replace nodes
	
	let save () = Hashtbl.fold (fun key a_ty env -> (key, a_ty):: env) nodes []
end

type io = Vinput | Vpatt | Vfresh
(* Vgeneral = general type *)

module Gamma = struct
	
	type t = (single_ty * io) M.t
	
	let empty = M.empty
	
	let add loc env x t io =
		if M.mem x env then error loc (Clash x);
		M.add x (t, io) env

	let update env x t io =
		M.add x (t, io) env
		
	let getfresh env t =
		let cpt = ref 0 in fun () -> incr cpt; 
			let x = "%gen"^(string_of_int !cpt) in
			(M.add x (t, Vfresh) env),x
	
	let adds loc io =
		List.fold_left (fun env (x, t) -> add loc env x t io)
	
	let find loc env x = try
			M.find x env
		with Not_found -> error loc (UnboundVar x)
	
	let patts_vars env =
		M.fold (fun x (_, io) s -> if io = Vpatt then S.add x s else s) env S.empty
	
end

module ClockMap = struct
	type t = (clock_id) Hashtbl.t 
	
	let empty = Hashtbl.create 10
		
	let add map x y=
		Hashtbl.add map x y

	let add_map_vars =
		List.fold_left (fun map x -> add map x x)
		
	let solve map x = try
			let rec f y = 
				let t = Hashtbl.find map y in if(t=y) then t else f t 
			in
			let result = f x in
			add map x result;
			result
		with Not_found -> assert false (* Use this mapper with caution! *)			 
	let find map x = try
			Hashtbl.find map x
		with Not_found -> assert false (* Use this mapper with caution! *)
end

let single_aty_of_aty loc t =
	match t with
	| [t'] -> t'
	| _ -> error loc (ExpectedSingleType t)

let compatible_single_base_type actual_ty expected_ty =
	let (_,(actual,_)) = actual_ty in
	let (_,(expected,_)) = expected_ty in
	actual = expected

let compatible_base_types actual_ty expected_ty =
	try
		List.fold_left2
			(fun well_t ac_t ex_t ->
						let well_t' = compatible_single ac_t ex_t in
						(well_t && well_t'))
			true actual_ty expected_ty
	with Invalid_argument _ -> false

let compatible_clocks loc clock1 clock2=
	if clock1 <> clock2 then error loc ExpectedClock(clock2,clock1)
	
	(*
let compatible_args env fenv loc args_clocked_ty params_annotated_clocked_ty =
	try
		let node_base_clock_id = ref None in
		List.fold_left2
			(fun arg param ->
					match arg,param with
						| (a_ty,clock),id,(p_ty,BaseClock) ->
								if (compatible a_ty p_ty) then (error loc ExpectedBaseType(a_ty,p_ty));
								begin match (!node_base_clock_id) with
											|  None -> node_base_clock_id := clock
											|  Some(internal_clock) ->  if (internal_clock <> clock) then (error loc ExpectedClock(clock, internal_clock))
								end
								Gamma.add loc
					| (a_ty,Clock(arg_clock_bool,arg_clock_ident)),id,(p_ty,Clock(par_clock_bool,par_clock_ident)) ->

								
							
							Gamma(param.
						*)


	

let float_expr_of_expr te =
	match te.texpr_type with
	| [(Tfloat,_)] -> te
	| [(Tint,clock)] ->
			{ texpr_desc =
					TE_prim ("float_of_int",[te]);
				texpr_type = [(Tfloat,clock)];
				texpr_loc = (Lexing.dummy_pos, Lexing.dummy_pos);
			}
	| _ -> assert false

let float_op_of_int_op op =
	match op with
	| Badd -> Badd_f
	| Bsub -> Bsub_f
	| Bmul -> Bmul_f
	| Bdiv -> Bdiv_f
	| _ -> op

let not_a_nested_tuple e loc =
	match e with
	| PE_tuple el ->
			List.iter
				(fun e ->
							match e.pexpr_desc with
								PE_tuple _ -> error loc FlatTuple;
							| _ -> ()) el
	| _ -> assert false

let rec is_constant env e =
	match e.texpr_desc with
	| TE_const _ -> true
	| TE_tuple el -> List.for_all (is_constant env) el
	| _ -> false

let rec const_of_expr e =
	match e.texpr_desc with
	| TE_const c -> [c]
	| TE_tuple el ->
			List.fold_right (fun e acc -> const_of_expr e @ acc) el []
	| _ -> assert false

let type_constant = function
	| Cunit -> Tunit
	| Cbool _ -> Tbool
	| Cint _ -> Tint
	| Cfloat _ -> Tfloat
	| Cstring _ -> Tstring


let unify_typ loc cmap function_internal_clock param_typ in_typ =
	let (param_id,(_,param_clock))= param_typ in
	let (in_id,(_,in_clock)) = in_typ in
	begin match param_clock, in_clock with
		| BaseClock, _ -> 
			if (function_internal_clock <> in_clock) then 
				error loc (ExpectedClock (in_clock,function_internal_clock))
			else
				begin
					begin match param_id,in_id with
						| Some(param_clock_id),Some(in_clock_id) -> ClockMap.add cmap param_clock_id in_clock_id
						| _ -> ()
					end;
				end
		| (Clock(_,_)),(BaseClock) ->  error loc (ExpectedClock (in_clock,param_clock))
		| (Clock(b1,id1)),(Clock(b2,id2)) ->  
			if(b1<>b2) then error loc (ExpectedClock (in_clock,param_clock))
			else 
				begin
					let s_id2 = ClockMap.find cmap id2 in
					if (id1 <> s_id2) then
						 error loc (ExpectedClock ((Clock(b2,s_id2)),param_clock))
					else
						begin match param_id,in_id with
							| Some(param_clock_id),Some(in_clock_id) -> ClockMap.add cmap param_clock_id in_clock_id
							| _ -> ()
						end;
				end
	end
	
	
let unify_types loc cmap function_internal_clock param_types in_types =
		try
			List.iter2 (unify_typ loc cmap function_internal_clock) param_types in_types
		with Invalid_argument _ -> assert false
		
		
let unify_types_hidden loc type1 type2 =
	let cmap = ClockMap.empty in
	unify_types loc cmap BaseClock type1 type2
	
	
let unify_types_hidden loc cmap type1 type2 =
		try
			List.iter2 (unify_typ loc cmap function_internal_clock) param_types in_types
		with Invalid_argument _ -> assert false


let convert_fresh_types cmap main_env ty = 
	let f s typ =
		let fenv,res = s in
		let id,(base_ty,clock)= typ in
			let new_clock =
				match clock with
					| BaseClock ->  BaseClock
					| Clock(b,cid) -> Clock(b,ClockMap.find cmap cid)  (* assumption: clocks are declared before they are used *)
			in
			let new_fenv,new_id = (Gamma.getfresh env (base_ty,new_clock)) in
			ClockMap.add cmap id new_id;
			new_fenv,(new_id,(base_ty,new_clock))::res
	in 
	(List.fold_left f (main_env,[]) ty)


let rec type_expr env e =
	let env,(desc, t) = type_expr_desc env e.pexpr_loc e.pexpr_desc in
		env,{ texpr_desc = desc; texpr_type = t; texpr_loc = e.pexpr_loc; }

and type_expr_desc env loc = function
	| PE_const c ->
			env,(TE_const c , [None,(type_constant c,BaseClock)])
	
	| PE_ident x ->
			let _,gamma_env = env in
			let (ty,cl), _ = Gamma.find loc gamma_env x in 
         env,(TE_ident x , [Some(x),(ty,cl)])

	| PE_unop (Unot, e) ->
			let tt = Tbool in
			let env,te,(id,clock) = expected_type env e tt in
			env,(TE_unop (Unot, te) , [None,(tt,clock)])
	
	| PE_unop (Uminus, e) ->
			let tt = Tint in
			let env,te,(id,clock) = expected_type env e tt in
			env,(TE_unop (Uminus, te) , [None,(tt,clock)])
	
	| PE_unop (Uminus_f, e) ->
			let tt = Tfloat in
			let env,te,(id,clock) = expected_type env e tt in
			env,(TE_unop (Uminus_f, te) , [None,(tt,clock)])
	
	| PE_binop ((Band | Bor as op), e1, e2) ->
			let tt = Tbool in
			let env,te1,(id1,clock1) = expected_type env e1 tt in
			let env,te2,(id2,clock2) = expected_type env e2 tt in
			if compatible_clocks loc clock1 clock2 then
				env,(TE_binop (op, te1, te2) , [None,(tt,clock)])
	
	| PE_binop ((Badd | Bsub | Bmul | Bdiv as op), e1, e2) ->
			let env,te1 = type_expr env e1 in
			let env,te2 = type_expr env e2 in
			begin match te1.texpr_type, te2.texpr_type with
				| [_,(Tint,clock1)], [_,(Tint,clock2)] ->
						if compatible_clocks loc clock1 clock2 then
							env,(TE_binop (op, te1, te2), [None,(Tint,clock1)])
				| [_,((Tint | Tfloat),clock1)], [_,((Tint | Tfloat),clock2)] ->
						if compatible_clocks loc clock1 clock2 then
							env,(TE_binop(float_op_of_int_op op,
								float_expr_of_expr te1,
								float_expr_of_expr te2),
							[None,(Tfloat,clock)])
				| [_,((Tint | Tfloat), _)], ty -> error e2.pexpr_loc (ExpectedNum (ty))
				| ty, _ -> error e1.pexpr_loc (ExpectedNum (ty))
			end
			
	
	| PE_binop (Bmod, e1, e2) ->
			let tt = Tint in
			let env,te1,(id1,clock1) = expected_type env e1 tt in
			let env,te2,(id2,clock2) = expected_type env e2 tt in
			if compatible_clocks loc clock1 clock2 then
					env,(TE_binop(Bmod, te1, te2) , [None,(tt,clock1)])

	
	| PE_binop ((Bdiv_f | Bmul_f | Bsub_f | Badd_f as op), e1, e2) ->
			let tt = Tfloat in
			let env,te1,(id1,clock1) = expected_type env e1 tt in
			let env,te2,(id2,clock2) = expected_type env e2 tt in
			if compatible_clocks loc clock1 clock2 then
					env,(TE_binop (op, te1, te2) , [None,(tt,clock1)])


	| PE_binop (Beq | Bneq as op, e1, e2) ->
			let env,te1 = type_expr env e1 in
			let env,te2 = type_expr env e2 in
			begin match te1.texpr_type, te2.texpr_type with
				| [_,(t1,clock1)], [_,(t2,clock2)] when t1 = t2 ->
						if compatible_clocks loc clock1 clock2 then
							env,(TE_binop (op, te1, te2), [None,(Tbool,clock)])
				| _ ->
						error loc (Other "invalid operands to equality")
			end
	| PE_binop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
			let env,te1 = type_expr env e1 in
			let env,te2 = type_expr env e2 in
			begin match te1.texpr_type, te2.texpr_type with
				| [_,(Tint,clock1)], [_,(Tint,clock2)]
				| [_,(Tfloat,clock1)], [_,(Tfloat,clock2)] ->
						if compatible_clocks loc clock1 clock2 then
							  env,(TE_binop (op, te1, te2), [None,(Tbool,clock1)])
				| _ ->
								error loc (Other "invalid operands to comparison")
			end
	
	| PE_app (f, el) ->
				let ((t_in, t_out) , is_prim), is_print = 
					begin try
						(Delta.find f),is_print 
					with Not_found ->
							if Delta.is_print f then
								generate_print_vars (List.length el),false,false
							else
								error loc (UnboundNode f)
					end
				in
				let fcmap = ClockMap.empty in
				let env,tel,typ_tel = type_expr_list env el in
				let cmap,gamma_env = env in
				let gamma_env,finternal_clock,typ_tel = type_args (fcmap) cmap gamma_env loc t_in typ_tel in
				let app_node = if is_prim then (if is_print then (TE_print(finternal_clock,tel))	else
						TE_prim(f,finternal_clock, tel)) else TE_app(f,finternal_clock, tel) in
				let gamma_env, n_out = (convert_fresh_types fcmap env t_out) in
				(cmap,gamma_env),(app_node , nout)
	
	| PE_if (e1, e2, e3) ->
					let env,te1,(id,clock) = expected_type env e1 Tbool in
					let env,te2 = type_expr env e2 in
					let env,te3 = type_expr env e3 in
					let well_typed = compatible_clocks te2.texpr_type te3.texpr_type in
					if well_typed then
						let tt = te2.texpr_type in
						TE_if (te1, te2, te3), tt
					else
						error loc (ExpectedType (te3.texpr_type, te2.texpr_type))
	
	| PE_fby (e1, e2) ->
			let te1 = type_expr env e1 expected_clock in
			if not (is_constant env te1) then error e1.pexpr_loc ConstantExpected;
			let ty1 = te1.texpr_type in
			let te2 = type_expr env e2 expected_clock in
			let ty2 = te2.texpr_type in
			let well_typed = compatible ty1 ty2 in
			if well_typed then
				TE_fby (const_of_expr te1, te2), ty2
			else error te2.texpr_loc (ExpectedType (ty2, ty1))
	
  | PE_tuple el as n ->
      (*not_a_nested_tuple n loc; *) (* why? *)
		let env,tel,typ_tel = type_expr_list env el in
      env,(TE_tuple tel,typ_tel) 
  | PE_when (e1,e2) ->
		let x,clock =
			match e2 with
				| PE_ident x -> x,Clock(true,x) 
				| PE_unop (Unot, PE_ident x) -> x,Clock(false,x)
				| _ -> (error loc ExpectedClockIdentifier)
		in
		let env,te1 = type_expr env e1 in
		let cmap,gamma_env = env in
		let (ty,cl), _ = Gamma.find loc gamma_env x in
		let unify_types_hidden
		let single_aty_of_aty ty
		let compatible_clocks cl 

		
	(*| PE_merge (e1,e2,e3) -> 
			let te1 = type_expr env e1 in
			let te2 = type_expr env e2 in
			let te3 = type_expr env e3 in
			TE_merge(te1,te2,te3) *)

and type_expr_list env e_list=
	let env,rtel = List.fold_left (fun (_env,_tel) x -> let new_env, te = type_expr _env x in (new_env,te::_tel)) (env,[]) e_list in
	let typ_tel = List.rev (List.fold_left (fun l te -> (List.rev_append te.texpr_type l)) [] tel) in
	env, (List.rev rtel), typ_tel
				 	

and type_args function_cmap cmap gamma_env loc params_annot_ty typ_tel =
   (*let fenv = List.fold_left (
		fun e (id,single_ty) -> 
			match id with 
				| None -> assert false
				| Some(clockid) -> Gamma.add loc e clockid single_ty Vpatt) Gamma.empty params_annot_ty in *) 
	let gamma_env,params = convert_fresh_types function_cmap gamma_env params_annot_ty in
	(if (not (compatible_base_types actual_types params_ty)) then
		error loc (ExpectedType (actual_types, params_ty)));
	let function_internal_clock =
		match typ_tel with
			| (id,single_ty)::tl -> single_ty
			| [] -> asser false
	in
	unify_types cmap function_internal_clock params typ_tel;
	(cmap,gamma_env),function_internal_clock,typ_tel

	

and expected_type env e tt =
	let new_env,te = type_expr env e in
	let ty = te.texpr_type in
	match ty with
		| [id,(typ,clock)] ->
				if typ = tt then new_env,te,(id,clock)
				else error e.pexpr_loc (ExpectedBaseType (ty, tt))
		| _ -> error e.pxepr_loc (ExpectedBaseType (ty,tt))
	
and expected_simple_type env e =
	let te = type_expr env e in
	match te.texpr_type with
	| [_] -> te
	| _ -> error e.pexpr_loc (ExpectedSingleType (te.texpr_type))

let rec type_patt env p =
	let desc, t = type_patt_desc env p.ppatt_loc p.ppatt_desc in
	{ tpatt_desc = desc; tpatt_type = t; tpatt_loc = p.ppatt_loc; }

and type_patt_desc env loc patt =
	match patt with
	| PP_ident x -> begin
				let ty =
					match Gamma.find loc env x with
						t, Vpatt -> t
					| _ -> error loc (InputVar x)
				in
				[x], [ty]
			end
	| PP_tuple pl ->
			let tyl =
				List.map
					(fun x ->
								match Gamma.find loc env x with
									ty, Vpatt -> ty
								| _ -> error loc (InputVar x)
					) pl
			in
			pl , tyl

let type_equation env eq =
	let patt = type_patt env eq.peq_patt in
	let expr = type_expr env eq.peq_expr in
	let well_typed = compatible expr.texpr_type patt.tpatt_type in
	if well_typed then
		{ teq_patt = patt; teq_expr = expr; }
	else
		error
			eq.peq_expr.pexpr_loc (ExpectedType (expr.texpr_type, patt.tpatt_type))

let add_vars_of_patt loc s { teq_patt = { tpatt_desc = p }} =
	let add x s =
		if S.mem x s then error loc (Clash x);
		S.add x s
	in
	List.fold_left (fun s x -> add x s) s p

let check_outputs loc env equs =
	let s = List.fold_left (add_vars_of_patt loc) S.empty equs in
	let not_defined = S.diff (Gamma.patts_vars env) s in
	if not (S.is_empty not_defined)
	then error loc (UndefinedOutputs (S.elements not_defined))

let check_causality loc inputs equs =
	begin try ignore (Scheduling.schedule_equs inputs equs)
	with Scheduling.Causality -> error loc Causality
	end

let type_node n =
	let env = Gamma.adds n.pn_loc Vpatt Gamma.empty (n.pn_output@n.pn_local) in
	let env = Gamma.adds n.pn_loc Vinput env n.pn_input in
	let equs = List.map (type_equation env) n.pn_equs in
	check_outputs n.pn_loc env equs;
	check_causality n.pn_loc n.pn_input equs;
	(* keep identifiers!! *)
	Delta.add n.pn_name (n.pn_input , n.pn_output);
	{ tn_name = n.pn_name;
		tn_input = n.pn_input;
		tn_output = n.pn_output;
		tn_local = n.pn_local;
		tn_equs = equs;
		tn_loc = n.pn_loc; }

let check_main ft main =
	let ty =
		try Delta.find main with Not_found -> error dummy_loc (UnboundNode main)
	in
	match ty with
	| ([Tunit], [Tunit]), false -> ()
	| (t_in, t_out), false ->
			let n = List.find (fun n -> n.tn_name = main) (List.rev ft) in
			error n.tn_loc (BadMain (t_in, t_out))
	| _ -> errors dummy_loc "The main node cannot be a primitive function"

let type_file f main =
	let ft = List.map type_node f in
	if main <> "" then check_main ft main;
	ft

