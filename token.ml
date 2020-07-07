open Format

type token =
  | Tok_Print        of string
  | Tok_Str          of string
  | Tok_Num          of string * int
  | Tok_Div          of string
  | Tok_Mul          of string
  | Tok_Add          of string
  | Tok_Sub          of string
  | Tok_Eof          of string
  | Tok_Semi         of string
  | Tok_Id           of string
  | Tok_Var          of string
  | Tok_Equal        of string
  | Tok_EqualEqual   of string
  | Tok_LessEqual    of string
  | Tok_GreaterEqual of string
  | Tok_NotEqual     of string
  | Tok_Greater      of string
  | Tok_Less         of string
  | Tok_Bool         of string * bool

let lexeme token =
  match token with
  | Tok_Print s        -> s
  | Tok_Str s          -> s
  | Tok_Num (s, _)     -> s
  | Tok_Div s          -> s
  | Tok_Mul s          -> s
  | Tok_Add s          -> s
  | Tok_Sub s          -> s
  | Tok_Eof s          -> s
  | Tok_Semi s         -> s
  | Tok_Id s           -> s
  | Tok_Var s          -> s
  | Tok_Equal s        -> s
  | Tok_EqualEqual s   -> s
  | Tok_LessEqual s    -> s
  | Tok_GreaterEqual s -> s
  | Tok_NotEqual s     -> s
  | Tok_Less s         -> s
  | Tok_Greater s      -> s
  | Tok_Bool (s, _)    -> s

let print_token t = 
  match t with
  | Tok_Str l          -> printf "token: str,          lexeme: %s\n" l
  | Tok_Num (l, _)     -> printf "token: num,          lexeme: %s\n" l
  | Tok_Print l        -> printf "token: print,        lexeme: %s\n" l
  | Tok_Add l          -> printf "token: add,          lexeme: %s\n" l
  | Tok_Sub l          -> printf "token: sub,          lexeme: %s\n" l
  | Tok_Mul l          -> printf "token: mul,          lexeme: %s\n" l
  | Tok_Div l          -> printf "token: div,          lexeme: %s\n" l
  | Tok_Eof l          -> printf "token: eof,          lexeme: %s\n" l
  | Tok_Semi l         -> printf "token: semi,         lexeme: %s\n" l
  | Tok_Id l           -> printf "token: Id,           lexeme: %s\n" l
  | Tok_Var l          -> printf "token: Var,          lexeme: %s\n" l
  | Tok_Equal l        -> printf "token: Equal,        lexeme: %s\n" l
  | Tok_EqualEqual l   -> printf "token: EqualEqual,   lexeme: %s\n" l
  | Tok_LessEqual l    -> printf "token: LessEqual,    lexeme: %s\n" l
  | Tok_GreaterEqual l -> printf "token: GreaterEqual, lexeme: %s\n" l
  | Tok_NotEqual l     -> printf "token: NotEqual      lexeme: %s\n" l
  | Tok_Less l         -> printf "token: Less,         lexeme: %s\n" l
  | Tok_Greater l      -> printf "token: Greater,      lexeme: %s\n" l
  | Tok_Bool (l, _)    -> printf "token: Bool,         lexeme: %s\n" l
