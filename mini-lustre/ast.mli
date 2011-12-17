(* Arbres de syntaxe abstraite *)

open Asttypes
	
type p_expr =
  { pexpr_desc: p_expr_desc;
    pexpr_loc: location; }

and p_expr_desc =
  | PE_const of const
  | PE_ident of ident
  | PE_unop of unop * p_expr
  | PE_binop of binop * p_expr * p_expr
  | PE_app of ident * p_expr list
  | PE_if of p_expr * p_expr * p_expr
  | PE_fby of p_expr * p_expr
	| PE_when of p_expr * p_expr (* when operator *)
	| PE_merge of p_expr * p_expr * p_expr (* merge operator *)
  | PE_tuple of p_expr list

type p_patt =
  { ppatt_desc: p_patt_desc;
    ppatt_loc: location; }

and p_patt_desc =
  | PP_ident of ident
  | PP_tuple of ident list

type p_equation =
    { peq_patt: p_patt;
      peq_expr: p_expr; }

type p_node =
    { pn_name: ident;
      pn_input: annotated_ty;
      pn_output: annotated_ty; 
      pn_local: annotated_ty; 
      pn_equs: p_equation list;
      pn_loc: location; }

type p_file = p_node list




