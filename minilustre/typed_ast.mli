(* Arbres de syntaxe abstraite typés *)

open Asttypes 

type typed_var = string * base_ty
      
type t_expr = 
    { texpr_desc: t_expr_desc;
      texpr_type:  ty;
      texpr_loc: location; }

and t_expr_desc =
  | TE_const of const
  | TE_ident of string
  | TE_unop of unop * t_expr
  | TE_binop of binop * t_expr * t_expr
  | TE_app of string * t_expr list
  | TE_prim of string * t_expr list
  | TE_if of t_expr * t_expr * t_expr 
  | TE_fby of const list * t_expr
  | TE_tuple of t_expr list
  | TE_print of t_expr list

type t_patt = 
    { tpatt_desc: string list;
      tpatt_type: ty;
      tpatt_loc: location; }

type t_equation = 
    { teq_patt: t_patt;
      teq_expr: t_expr; }

type t_node = 
    { tn_name: string;
      tn_input: typed_var list;
      tn_output: typed_var list; 
      tn_local: typed_var list; 
      tn_equs: t_equation list;
      tn_loc: location; }

type t_file = t_node list
