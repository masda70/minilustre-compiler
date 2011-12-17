(* code d'Adrien Guatto *)

open Format
open Asttypes
open Typed_ast

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

let rec print_exp fmt e = match e.texpr_desc with
  | TE_const c -> print_const fmt c
  | TE_ident s -> fprintf fmt "%s" s
  | TE_unop (op, e) -> fprintf fmt "%a(%a)" print_unop op print_exp e
  | TE_binop (op, l, r) ->
      fprintf fmt "(@[%a %a@ %a@])" print_binop op print_exp l print_exp r
  | TE_app (name, e_list) | TE_prim (name, e_list) ->
      fprintf fmt "%a(@[%a@])" print_name name print_arg_list e_list
  | TE_if (cond, cons, altr) ->
      fprintf fmt "@[if %a@ then %a@ else %a@]"
        print_exp cond
        print_exp cons
        print_exp altr
  | TE_fby (l, r) ->
      fprintf fmt "@[(@[%a@]) fby %a@]" print_const_exp l print_exp r
  | TE_tuple e_list ->
      fprintf fmt "(@[%a@])" print_tuple_arg_list e_list
  | TE_print e_list ->
      fprintf fmt "print (@[%a@])" print_arg_list e_list

and print_arg_list fmt e_list = match e_list with
  | [] -> ()
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_tuple_arg_list fmt e_list = match e_list with
  | [] -> assert false
  | [x] -> fprintf fmt "%a" print_exp x
  | h :: t -> fprintf fmt "%a,@ %a" print_exp h print_arg_list t

and print_const_exp fmt ce_list = match ce_list with
  | [] -> assert false
  | [c] -> fprintf fmt "%a" print_const c
  | h :: t -> fprintf fmt "%a,@ %a" print_const h print_const_exp t

let print_eq fmt eq =
  fprintf fmt "@[(%a) = @[%a@]@]"
    (print_list print_name ",") eq.teq_patt.tpatt_desc
    print_exp eq.teq_expr

let print_base_type fmt bty = match bty with
  | Tunit -> fprintf fmt "unit"
  | Tbool -> fprintf fmt "bool"
  | Tint -> fprintf fmt "int"
  | Tfloat -> fprintf fmt "float"
  | Tstring -> fprintf fmt "string"

(* let print_type = print_list print_cbase_type "*" *)

let print_var_dec fmt (name, ty) =
  fprintf fmt "%s : %a" name print_base_type ty

let rec print_var_dec_list = print_list print_var_dec ";"

let print_node fmt nd =
  fprintf fmt
    "@[node %s(@[%a@]) returns (@[%a@])@\nvar @[%a;@]@\n@[<v 2>let@ @[%a@]@]@\ntel@]"
    nd.tn_name
    print_var_dec_list nd.tn_input
    print_var_dec_list nd.tn_output
    print_var_dec_list nd.tn_local
    (print_list_eol print_eq ";") nd.tn_equs

let print_node_list_std ndl =
  List.iter (fun nd -> Format.printf "%a@\n@." print_node nd) ndl
