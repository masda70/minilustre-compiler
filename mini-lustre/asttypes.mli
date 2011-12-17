type location = Lexing.position * Lexing.position

type ident = string

type base_ty =
  | Tunit
  | Tbool
  | Tint
  | Tfloat
  | Tstring


type const =
  | Cunit
  | Cbool of bool
  | Cint of int
  | Cfloat of float
  | Cstring of string

type unop = 
  | Unot | Uminus | Uminus_f

type binop = 
  | Beq | Bneq | Blt | Ble | Bgt | Bge 
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Badd_f | Bsub_f | Bmul_f | Bdiv_f 
  | Band | Bor

type clock_id = ident

type clock_expr = bool * clock_id

type clock = BaseClock | Clock of clock_expr 

type single_ty = base_ty * clock

type annotated_single_ty = (clock_id option) * single_ty

type ty = single_ty list

type annotated_ty = annotated_single_ty list  