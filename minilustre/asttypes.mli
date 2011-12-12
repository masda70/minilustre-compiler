type location = Lexing.position * Lexing.position

type base_ty =
  | Tunit
  | Tbool
  | Tint
  | Tfloat
  | Tstring

type ty = base_ty list

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

