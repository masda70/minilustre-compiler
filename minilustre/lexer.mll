{

  open Lexing
  open Parser
  open Asttypes
  open Ast

  exception Lexical_error of string

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
      [ 
	"and", AND;
	"bool", BOOL;
	"const", CONST;
	"else", ELSE;
	"end", END;
	"false", CONST_BOOL(false); 
	"fby", FBY;
	"float", FLOAT;
	"if", IF;
	"int", INT;
	"let", LET;
	"node", NODE;
	"not", NOT;
	"or", OR;
	"returns", RETURNS;
	"string", STRING;
	"tel", TEL;
	"then", THEN;
	"true", CONST_BOOL(true);
	"unit", UNIT; 
	"var", VAR;
      ];
    fun s -> 
      try Hashtbl.find h s with Not_found -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let exponent = ('e' | 'E') ('+' | '-')? digit+
let float = digit+ '.' digit* exponent?
          | digit* '.'digit+ exponent?
	  | digit+ exponent
let ident = alpha (alpha | '_' | digit)*

rule token = parse
  | '\n' 
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ 
      { token lexbuf }
(*   | "--" [^ '\n']* ['\n'] *)
(*       { newline lexbuf; token lexbuf } *)
  | "/*"   
      { comment lexbuf; token lexbuf }
  | ident  
      { id_or_keyword (lexeme lexbuf) }
  | digit+ 
      { CONST_INT (int_of_string (lexeme lexbuf)) } 
  | float  
      { CONST_FLOAT (float_of_string (lexeme lexbuf)) } 
  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }
  | "-."
      { MINUS_DOT }
  | "+."
      { PLUS_DOT }
  | "*."
      { STAR_DOT }
  | "/."
      { SLASH_DOT }
  | ">" 
      { COMP Bgt }
  | ">=" 
      { COMP Bge }
  | "<" 
      { COMP Blt }
  | "<=" 
      { COMP Ble }
  | "<>"
      { NEQ }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | ":"
      { COLON }
  | ";"
      { SEMICOL }
  | "="
      { EQUAL }
  | ","
      { COMMA }
  | '"'
      { let buf = Buffer.create 512 in
	string buf lexbuf;
	CONST_STRING (Buffer.contents buf) }
  | _ 
      { raise (Lexical_error (lexeme lexbuf)) }
  | eof
      { EOF }

and string buf = parse
  | '"' { () }
  | '\\' 'n' 
      { Buffer.add_string buf "\\n";
	string buf lexbuf }
  | '\\' '\\' 
      { Buffer.add_string buf "\\\\";
	string buf lexbuf }
  | '\\' '"' 
      { Buffer.add_string buf "\\\"";
	string buf lexbuf }
  | [^ '\\' '"' '\n']+ 
      { Buffer.add_string buf (lexeme lexbuf);
	string buf lexbuf }
  | '\\' 
      { raise (Lexical_error "illegal escape character") }
  | '\n' | eof
      { raise (Lexical_error "unterminated string") }
  | _ 
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

and comment = parse
  | "*/" { () }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Lexical_error "unterminated comment") }

