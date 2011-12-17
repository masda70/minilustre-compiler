(* Arbres de syntaxe abstraite typés *)

open Asttypes




type t_expr = 
    { texpr_desc: t_expr_desc;
      texpr_type:  annotated_ty;
      texpr_loc: location; }

and t_expr_desc =
  | TE_const of const
  | TE_ident of string
  | TE_unop of unop * t_expr
  | TE_binop of binop * t_expr * t_expr
  | TE_app of string * clock * t_expr list (* internal clock is also present ! :D *)
  | TE_if of t_expr * t_expr * t_expr 
  | TE_fby of const list * t_expr
  | TE_tuple of t_expr list
	| TE_when of t_expr * clock_expr  (* when operator *)
	| TE_merge of clock_expr * t_expr * t_expr (* merge operator *)
	| TE_prim of string * clock * * t_expr list  (* primitive function *)
	| TE_print of clock * t_expr list (* print function *)

type t_patt = 
    { tpatt_desc: string list;
      tpatt_type: ty;
      tpatt_loc: location; }

type t_equation = 
    { teq_patt: t_patt;
      teq_expr: t_expr; }

type t_node = 
    { tn_name: string;
      tn_input: annotated_ty;
      tn_output: annotated_ty;
      tn_local: annotated_ty;
      tn_equs: t_equation list;
      tn_loc: location; }

type t_file = t_node list
