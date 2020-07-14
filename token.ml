open Format

type token =
  | Tok_Print        of string
  | Tok_Str          of string
  | Tok_Num          of int
  | Tok_Bool         of bool
  | Tok_Div
  | Tok_Mul
  | Tok_Add
  | Tok_Sub
  | Tok_Eof          of string
  | Tok_Semi
  | Tok_Id           of string
  | Tok_Var         
  | Tok_Equal        
  | Tok_EqualEqual  
  | Tok_LessEqual    
  | Tok_GreaterEqual 
  | Tok_NotEqual     
  | Tok_Greater      
  | Tok_Less
  | Tok_If
  | Tok_End
  | Tok_Do

let token_to_str token =
  match token with
  | Tok_Print s        -> s
  | Tok_Str s          -> s
  | Tok_Num l          -> string_of_int l
  | Tok_Div            -> "/"
  | Tok_Mul            -> "*"
  | Tok_Add            -> "+"
  | Tok_Sub            -> "-"
  | Tok_Eof s          -> s
  | Tok_Semi           -> ";"
  | Tok_Id s           -> s
  | Tok_Var           -> "var"
  | Tok_Equal        -> "="
  | Tok_EqualEqual    -> "=="
  | Tok_LessEqual     -> "<="
  | Tok_GreaterEqual -> ">="
  | Tok_NotEqual    -> "!="
  | Tok_Less          -> "<"
  | Tok_Greater       -> ">"
  | Tok_Bool s    ->  string_of_bool s
  | Tok_End       -> "end"
  | Tok_If      -> "if"
  | Tok_Do        -> "do";

(* let print_token t =
 *   match t with
 *   | Tok_Str l          -> printf "token: str,          lexeme: %s\n" l
 *   | Tok_Num l          -> printf "token: num,          lexeme: %d\n" l
 *   | Tok_Print l        -> printf "token: print,        lexeme: %s\n" l
 *   | Tok_Add            -> printf "token: add,          lexeme: +\n"
 *   | Tok_Sub            -> printf "token: sub,          lexeme: -\n"
 *   | Tok_Mul            -> printf "token: mul,          lexeme: *\n"
 *   | Tok_Div            -> printf "token: div,          lexeme: /\n"
 *   | Tok_Eof l          -> printf "token: eof,          lexeme: %s\n" l
 *   | Tok_Semi           -> printf "token: semi,         lexeme: ;\n"
 *   | Tok_Id l           -> printf "token: Id,           lexeme: %s\n" l
 *   | Tok_Var           -> printf "token: Var,          lexeme: var\n" 
 *   | Tok_Equal         -> printf "token: Equal,        lexeme: =\n" 
 *   | Tok_EqualEqual    -> printf "token: EqualEqual,   lexeme: ==\n" 
 *   | Tok_LessEqual     -> printf "token: LessEqual,    lexeme: <=\n" 
 *   | Tok_GreaterEqual  -> printf "token: GreaterEqual, lexeme: >=\n" 
 *   | Tok_NotEqual      -> printf "token: NotEqual      lexeme: !=\n" 
 *   | Tok_Less          -> printf "token: Less,         lexeme: <\n" 
 *   | Tok_Greater       -> printf "token: Greater,      lexeme: >\n" 
 *   | Tok_Bool l         -> printf "token: Bool,         lexeme: %B\n" l *)
