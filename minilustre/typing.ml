open Asttypes
open Ast
open Typed_ast
open Format

module S = Set.Make(String)
module M = Map.Make(String)


type error =
  | ExpectedType of ty * ty
  | ExpectedPattern of ty
  | ExpectedBase of ty
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

let print_base_type fmt = function
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Tfloat -> fprintf fmt "float"
  | Tstring -> fprintf fmt "string"
  | Tunit -> fprintf fmt "unit"

let print_type fmt = function
  | ([]) -> fprintf fmt "empty tuple"
  | [t] -> print_base_type fmt t
  | (t::tl) ->
      fprintf fmt "(";
      print_base_type fmt t;
      List.iter (fun t -> fprintf fmt " * %a" print_base_type t) tl;
      fprintf fmt ")" 

let report fmt = function
  | UnboundVar id -> fprintf fmt "unbound variable %s" id
  | UnboundNode id -> fprintf fmt "unbound node %s" id
  | ExpectedType (t1,t2) -> 
      fprintf fmt 
      "this expression has type %a but is expected to have type %a" 
      print_type t1 print_type t2
  | ExpectedPattern ty ->
      fprintf fmt "this pattern is expected to have type %a" 
	print_type ty
  | ExpectedBase ty -> 
      fprintf fmt 
     "this expression has type %a but is expected to have a type simple type"
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
    "int_of_float", ([Tfloat] , [Tint]) ;
    "float_of_string", ([Tstring] , [Tfloat]) ;
    "int_of_string", ([Tstring] , [Tint]) ;
    "bool_of_string", ([Tstring] , [Tbool]) ;
    "float_of_int", ([Tint] , [Tfloat]) ;
    "read", ([Tunit] , [Tstring]) ;
    "random_int", ([Tint] , [Tint]) ;
    "random_float", ([Tfloat] , [Tfloat]) ;
    "cos", ([Tfloat], [Tfloat]) ;
    "sin", ([Tfloat], [Tfloat]) ;
    "draw_point", ([Tint; Tint], [Tunit]) ;
    "draw_line",
      ([Tint; Tint; Tint; Tint], [Tunit]) ;
    "draw_circle", ([Tint; Tint; Tint], [Tunit]);
    "draw_rect", 
      ([Tint; Tint; Tint; Tint],[Tunit]);
    "fill_rect",
      ([Tint; Tint; Tint; Tint],[Tunit]);
    "get_mouse", ([Tunit], [Tint;Tint]) ]
    
  let nodes = Hashtbl.create 97
      
  let is_primitive f = List.mem_assoc f prims

  let is_print f = (f = "print")

  let find n = 
    try Hashtbl.find nodes n , false with 
	Not_found -> List.assoc n prims , true

  let add = Hashtbl.replace nodes

  let save () = Hashtbl.fold (fun key ty env -> (key,ty)::env) nodes []
end

type io = Vinput | Vpatt
module Gamma = struct

  type t = (base_ty * io) M.t

  let empty = M.empty

  let add loc env x t io = 
    if M.mem x env then error loc (Clash x);
    M.add x (t,io) env

  let adds loc io = 
    List.fold_left (fun env (x,t) -> add loc env x t io)

  let find loc env x = try 
    M.find x env
  with Not_found ->  error loc (UnboundVar x)

  let patts_vars env = 
    M.fold (fun x (_,io) s -> if io=Vpatt then S.add x s else s) env S.empty

end

let base_ty_of_ty loc t =
  match t with
  | [t'] -> t'
  | _ -> error loc (ExpectedBase t)



let compatible_base actual_ty expected_ty =
  actual_ty = expected_ty

let compatible actual_ty expected_ty =
  try
    List.fold_left2
      (fun well_t ac_t ex_t -> 
	let well_t' = compatible_base ac_t ex_t in
	(well_t && well_t'))
      true actual_ty expected_ty
  with Invalid_argument _ -> false 


let float_expr_of_expr te =
  match te.texpr_type with
  | [Tfloat] -> te
  | [Tint] -> 
      { texpr_desc =
	  TE_prim ("float_of_int",[te]);
	texpr_type = [Tfloat];
	texpr_loc = (Lexing.dummy_pos,Lexing.dummy_pos);
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
  | Cunit -> [Tunit]
  | Cbool _ -> [Tbool]
  | Cint _ -> [Tint]
  | Cfloat _ -> [Tfloat]
  | Cstring _ -> [Tstring]

let rec type_expr env e = 
  let desc,t = type_expr_desc env e.pexpr_loc e.pexpr_desc in
  { texpr_desc = desc; texpr_type = t; texpr_loc = e.pexpr_loc; }

and type_expr_desc env loc = function
  | PE_const c ->
      TE_const c , type_constant c

  | PE_ident x ->
      let ty, _ = Gamma.find loc env x in
      TE_ident x , [ty]

  | PE_unop (Unot, e) ->
      let tt = [Tbool] in
      let te = expected_type env e tt in
      TE_unop (Unot, te) , tt

  | PE_unop (Uminus, e) ->
      let tt = [Tint] in
      let te = expected_type env e tt in
      TE_unop (Uminus, te) , tt

  | PE_unop (Uminus_f, e) ->
      let tt = [Tfloat] in
      let te = expected_type env e tt in
      TE_unop (Uminus_f, te) , tt

  | PE_binop ((Band | Bor as op), e1, e2) ->
      let tt = [Tbool] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_binop (op, te1, te2) , tt 

  | PE_binop ((Badd | Bsub | Bmul | Bdiv as op), e1, e2) ->
      let te1 = type_expr env e1 in
      let te2 = type_expr env e2 in
      begin match te1.texpr_type, te2.texpr_type with
      | [Tint], [Tint] ->
	  TE_binop (op, te1, te2), [Tint]
      | [(Tint | Tfloat)], [(Tint | Tfloat)] ->
	  TE_binop(float_op_of_int_op op,
		   float_expr_of_expr te1,
		   float_expr_of_expr te2), 
	  [Tfloat]
      | [(Tint | Tfloat)], ty -> error e2.pexpr_loc (ExpectedNum (ty))
      | ty, _ -> error e1.pexpr_loc (ExpectedNum (ty))
      end

  | PE_binop (Bmod, e1, e2) ->
      let tt = [Tint] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_binop(Bmod,te1,te2) , tt

  | PE_binop ((Bdiv_f | Bmul_f | Bsub_f | Badd_f as op), e1, e2) ->
      let tt = [Tfloat] in
      let te1 = expected_type env e1 tt in
      let te2 = expected_type env e2 tt in
      TE_binop (op, te1, te2), tt

  | PE_binop (Beq | Bneq as op, e1, e2) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      begin match ty1, ty2 with
      | [t1], [t2] when t1 = t2 ->
	  TE_binop (op, te1, te2), [Tbool]
      | _ ->
	  error loc (Other "invalid operands to equality")
      end

  | PE_binop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
      let te1 = type_expr env e1 in
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      begin match ty1, ty2 with
      | [Tint], [Tint]
      | [Tfloat], [Tfloat] ->
	  TE_binop (op, te1, te2), [Tbool]
      | _ ->
	  error loc (Other "invalid operands to comparison")
      end

  | PE_app (f, el) ->
      begin try 
	let (t_in,t_out) , is_prim = Delta.find f in 
	let tel = type_args env loc t_in el in
	let app_node = if is_prim then TE_prim(f, tel) else TE_app(f, tel) in
	app_node , 
	begin match t_out with
	| [] -> assert false
	| _ -> t_out
	end
      with Not_found -> 
	if Delta.is_print f then
	  let tel = List.map (expected_base_type env) el in
	  (TE_print(tel), [Tunit])
	else
	  error loc (UnboundNode f)
      end

  | PE_if (e1, e2, e3) ->
      let te1 = expected_type env e1 ([Tbool]) in
      let te2 = type_expr env e2 in
      let te3 = type_expr env e3 in
      let well_typed = compatible te2.texpr_type te3.texpr_type in
      if well_typed then
	let tt = te2.texpr_type in
	TE_if (te1, te2, te3), tt
      else
	error loc (ExpectedType (te3.texpr_type, te2.texpr_type))

  | PE_fby (e1, e2) ->
      let te1 = type_expr env e1 in
      if not (is_constant env te1) then error e1.pexpr_loc ConstantExpected;
      let ty1 = te1.texpr_type in
      let te2 = type_expr env e2 in
      let ty2 = te2.texpr_type in
      let well_typed = compatible ty1 ty2 in
      if well_typed then 
	TE_fby (const_of_expr te1, te2), ty2
      else error te2.texpr_loc (ExpectedType (ty2, ty1))
	
  | PE_tuple el as n ->
      not_a_nested_tuple n loc;
      let tel = List.map (type_expr env) el in
      TE_tuple tel, 
      (List.map (fun e -> base_ty_of_ty e.texpr_loc e.texpr_type) tel)

and type_args env loc params_ty el =
  let tel = List.map (type_expr env) el in
  let actual_types = 
    List.rev
      begin
	List.fold_left 
	  (fun res te -> List.rev_append te.texpr_type res)
	  [] tel
      end
  in
  let well_typed = 
    compatible actual_types params_ty
  in
  if well_typed then tel
  else error loc (ExpectedType (actual_types, params_ty));


and expected_type env e tt = 
  let te = type_expr env e in
  let ty = te.texpr_type in
  if ty = tt then te
  else error e.pexpr_loc (ExpectedType (ty, tt))

and expected_base_type env e =
  let te = type_expr env e in
  match te.texpr_type with
  | [_] -> te
  |  _ ->  error e.pexpr_loc (ExpectedBase (te.texpr_type))

let rec type_patt env p = 
  let desc, t = type_patt_desc env p.ppatt_loc p.ppatt_desc in
  { tpatt_desc = desc; tpatt_type = t; tpatt_loc = p.ppatt_loc; }

and type_patt_desc env loc patt =
  match patt with 
  | PP_ident x -> begin
      let ty = 
	match Gamma.find loc env x with
	  t, Vpatt -> t
	| _  -> error loc (InputVar x)
      in
      [x], [ty]
    end
  | PP_tuple pl ->
      let tyl = 
	List.map 
	  (fun x -> 
	     match Gamma.find loc env x with
		 ty, Vpatt -> ty
	       | _  -> error loc (InputVar x)
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


let add_vars_of_patt loc s {teq_patt = {tpatt_desc=p}} = 
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
  let t_in = List.map (fun (_, ty) -> ty) n.pn_input in
  let t_out = List.map (fun (_, ty) -> ty) n.pn_output in
  Delta.add n.pn_name (t_in,t_out);
  { tn_name = n.pn_name;
    tn_input =  n.pn_input;
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

