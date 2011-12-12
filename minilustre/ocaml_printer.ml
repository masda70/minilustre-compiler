open Format
open Asttypes
open Imp_ast

let string_of_list =
  let rec string_of_list acc f sep l =
    match l with
    | [] -> acc
    | [x] -> acc^(f x)
    | x::xs -> string_of_list (acc^(f x)^sep) f sep xs
  in
  fun f sep l ->
    string_of_list "" f sep l 

let print_list print print_sep l =
  let rec printrec l =
    match l with
      [] -> ()
    | [x] ->
	print x
    | x::l ->
	open_box 0;
	print x;
	print_sep ();
(*	print_space ();*)
	printrec l;
	close_box () in
  printrec l

let string_of_base_ty ty =
  match ty with  
  | Tunit -> "unit"
  | Tbool -> "bool"
  | Tint -> "int"
  | Tfloat -> "float"
  | Tstring ->  "string"

let string_of_unop op =
  match op with
  | Unot -> "not"
  | Uminus -> "-"
  | Uminus_f -> "-."

let string_of_binop op =
  match op with
  | Beq -> " = "
  | Bneq -> " <> "
  | Blt -> " < "
  | Ble -> " <= "
  | Bgt -> " > "
  | Bge -> " >= "
  | Badd -> " + "
  | Bsub -> " - "
  | Bmul -> " * "
  | Bdiv -> " / "
  | Bmod -> " mod "
  | Badd_f -> " +. "
  | Bsub_f -> " -. "
  | Bmul_f -> " *. "
  | Bdiv_f -> " /. "
  | Band -> " && "
  | Bor -> " || "

let string_of_const c =
  match c with
  | Cunit ->  "()"
  | Cbool(b) -> if b then "true" else "false"
  | Cint(i) -> 
      if i < 0 then
	"("^(string_of_int i)^")"
      else string_of_int i
  | Cfloat(f) ->
      if f < 0.0 then
	"("^(string_of_float f)^")"
      else string_of_float f
  | Cstring(s) -> "\""^s^"\""

let string_of_atom a =
  match a with
  | Const c -> string_of_const c
  | Ident x -> x

let rec string_of_expr e = 
  match e.mexpr_desc with
  | ME_const c -> string_of_const c
  | ME_ident x -> x
  | ME_mem x -> "mem'."^x
  | ME_unop(op,e1) ->
      sprintf "(%s %s)" (string_of_unop op) (string_of_expr e1)
  | ME_binop (op, e1, e2) -> 
      sprintf "(%s %s %s)" 
	(string_of_expr e1) (string_of_binop op) (string_of_expr e2)
  | ME_if (e1,e2,e3) -> 
      sprintf "@\n    (if %s then@\n       %s@\n     else@\n       %s)"
	(string_of_expr e1) (string_of_expr e2) (string_of_expr e3)
  | ME_tuple el -> 
      sprintf "(%s)" (string_of_list string_of_expr ", " el)
  | ME_app(f,mem,args) ->
      sprintf "(%s_step mem'.%s (%s))" 
	f mem (string_of_list string_of_expr ", " args)
  | ME_prim(f,args,_) ->
      sprintf "(%s (%s))" f (string_of_list string_of_expr ", " args)
  | ME_print(el) -> 
      sprintf "(@\n%s    flush_all())"
	(string_of_list string_of_print_expr "" el)

and string_of_print_expr e =
  match e.mexpr_type with
  | [t] ->
      begin match t with
      | Tunit -> "    print_string \"()\";@\n"
      | Tbool -> 
	  sprintf "    (print_bool %s);@\n" (string_of_expr e)
      | Tint -> sprintf "    (print_int %s);@\n" (string_of_expr e)
      | Tfloat -> sprintf "    (print_float %s);@\n" (string_of_expr e)
      | Tstring -> sprintf "    (print_string %s);@\n" (string_of_expr e)
      end;
  | _ -> assert false
  

let print_mem n = 
  if n.mn_mem = { fby_mem = []; node_mem = []; } then
    printf "type %s_mem = unit@\n" n.mn_name
  else begin
    printf "type %s_mem = {@\n" n.mn_name;
    print_list 
      (fun (x,t) -> printf "    mutable %s: %s;@\n" x (string_of_base_ty t))
      (fun () -> ())
      n.mn_mem.fby_mem;
    print_list 
      (fun (x,t) -> printf "    %s: %s_mem;@\n" x t)
      (fun () -> ())
      n.mn_mem.node_mem;
    printf "  }@\n"
  end

let print_init n =
  if n.mn_mem = { fby_mem = []; node_mem = []; } then
    printf "let %s_init () = ()@\n" n.mn_name 
  else begin
    printf "let %s_init () = {@\n" n.mn_name;
    print_list 
      (fun (x,c) -> printf "    %s = %s;@\n" x (string_of_const c))
      (fun () -> ())
      n.mn_init.fby_init;
    print_list 
      (fun (x,f) -> printf "    %s = %s_init ();@\n" x f)
      (fun () -> ())
      n.mn_init.node_init;
    printf "  }@\n"
  end

let print_compute { meq_patt = p; meq_expr = e } = 
  printf "  let (%s) = %s in@\n" 
    (string_of_list (fun (x,_) -> x) ", " p)
    (string_of_expr e)

let print_update (x,a) =
  printf "  mem'.%s <- %s;@\n" x (string_of_atom a)
 
let print_step n =
  printf "let %s_step mem' (%s) = @\n" 
    n.mn_name 
    (string_of_list (fun (x,_) -> x) ", " n.mn_input_step);
  List.iter print_compute n.mn_compute;
  List.iter print_update n.mn_update;
  printf "  (%s)@\n" (string_of_list (fun (x,_) -> x) ", " n.mn_output_step)

let output_node n =
  print_mem n;
  force_newline();
  print_init n;
  force_newline();
  print_step n;
  force_newline();
  force_newline()

let print_preamble () =
  printf "let print_bool b = @\n";
  printf "  if b then print_string \"true\" @\n";
  printf "  else print_string \"false\";;@\n"

let print_graphics_preamble () =
  print_string "let read = read_line";
  print_string "\n";
  print_string "let random_int = Random.int";
  print_string "\n";
  print_string "let random_float = Random.float";
  print_string "\n";
  print_string "let draw_point (x,y) = Graphics.plot x y";
  print_string "\n";
  print_string "let draw_line (x0,y0,x,y) =\n";
  print_string "  Graphics.moveto x0 y0;\n";
  print_string "  Graphics.lineto x y;;\n";
  print_string "\n";
  print_string "let draw_circle (x,y,r) = Graphics.draw_circle x y r;;\n";
  print_string "\n";
  print_string "let draw_rect (x,y,w,h) = Graphics.draw_rect x y w h;;\n";
  print_string "\n";
  print_string "let fill_rect (x,y,w,h) = Graphics.fill_rect x y w h;;\n";
  print_string "\n";
  print_string "let get_mouse = Graphics.mouse_pos";
  print_string "\n";
  print_string "let _x' = Graphics.open_graph \"\";;\n";
  print_string "\n";
  print_string "let _x' = Graphics.auto_synchronize false;;\n";
  print_string "\n"

let print_run main =
  printf "let wait () = try Thread.delay 0.01 with _ -> ()\n";
  printf "\n";
  printf "let run f_init f_step =\n";
  printf "  let mem = f_init () in\n";
  printf "  while true do\n";
  printf "    Graphics.clear_graph ();\n";
  printf "    f_step mem ();\n";
  printf "    Graphics.synchronize();\n";
  printf "    wait()\n";
  printf "  done\n";
  printf "\n";
  printf "let _ = run %s_init %s_step" main main
  
let _ = set_max_boxes max_int 

let output_ocaml oc f main = 
  set_formatter_out_channel oc;
  print_preamble ();
  printf "@\n";
  print_graphics_preamble ();
  printf "@\n";
  List.iter output_node f;
  if main <> "" then print_run main;
  print_flush ()
