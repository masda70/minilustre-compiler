open Asttypes

type atom = Const of const | Ident of string
type tvar = string * base_ty

(** Enregistrement représentant la déclaration de la mémoire interne
    d'un noeud. *)
type mem = 
    { fby_mem: tvar list; 
      (** liste des mémoires associées aux fby. 
          Par exemple, la valeur [ [("next_x", Tbool)] ] 
          correspond au code généré [ "next_x : bool;" ]
       *)
      node_mem: (string * string) list;
      (** liste des mémoires associées aux appels de noeuds
          Par exemple, la valeur [ [("mem1", "f")] ] 
          correspond au code généré [ "mem1 : f_mem;" ]
       *)
    }

(** Enregistrement représentant le corps des fonctions d'initialisation. *)
type init = 
    { fby_init : (string * const) list;
      (** initialisation des fby
          Par exemple, la valeur [ [("next_x", Cbool false)] ]
          correspond au code généré [ "next_x = false;" ]
       *)
      node_init : (string * string) list; 
      (** initialisation des mémoires associées aux appels de noeuds
          Par exemple, la valeur [ [("mem1", "f")] ]
          correspond au code généré [ "mem1 = f_init();" ]
       *)
    }
  

type m_expr = 
    { mexpr_desc: m_expr_desc;
      mexpr_type: base_ty list; }

and m_expr_desc =
  | ME_const of const
  | ME_ident of string
  | ME_mem of string
  | ME_unop of unop * m_expr
  | ME_binop of binop * m_expr * m_expr
  | ME_app of string * string * m_expr list
  | ME_prim of string * m_expr list * int
  | ME_if of m_expr * m_expr * m_expr 
  | ME_tuple of m_expr list
  | ME_print of m_expr list

type m_equation = 
    { meq_patt: tvar list;
      meq_expr: m_expr; }

(** Enregistrement représentant la compilation d'un noeud *)
type m_node = 
    { mn_name: string; (** nom du noeud *)
      mn_input_step: tvar list; (** liste des entrées *)
      mn_output_step: tvar list;  (** liste des sorties *)
      mn_local: tvar list;  (** liste des variables locales *)
      mn_mem: mem; (** déclaration de la mémoire du noeud *)
      mn_init: init; (** initialisation de la mémoire du noued *)
      mn_compute: m_equation list; (** calcul de la valeur des flots *)
      mn_update: (string * atom) list; (** mise à jour de la mémoire *)
    }

type m_file = m_node list
