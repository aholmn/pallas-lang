type token =
  | Print of string
  | Str   of string
  | Num   of int
  | Bool  of bool
  | Id    of string
  | Div
  | Mul
  | Add
  | Sub
  | Eof
  | Semi
  | Var
  | Equal
  | EqualEqual
  | LessEqual
  | GreaterEqual
  | NotEqual
  | Greater
  | Less
  | If
  | End
  | Do
  | Else

let token_to_str = function
  | Print s      -> s
  | Str s        -> s
  | Num l        -> string_of_int l
  | Div          -> "/"
  | Mul          -> "*"
  | Add          -> "+"
  | Sub          -> "-"
  | Eof          -> "eof"
  | Semi         -> ";"
  | Id s         -> s
  | Var          -> "var"
  | Equal        -> "="
  | EqualEqual   -> "=="
  | LessEqual    -> "<="
  | GreaterEqual -> ">="
  | NotEqual     -> "!="
  | Less         -> "<"
  | Greater      -> ">"
  | Bool s       ->  string_of_bool s
  | End          -> "end"
  | If           -> "if"
  | Do           -> "do";
  | Else         -> "else"

(* let print_token t =
 *   match t with
 *   | Str l        -> Format.printf "token: str,          lexeme: %s\n" l
 *   | Num l        -> Format.printf "token: num,          lexeme: %d\n" l
 *   | Print l      -> Format.printf "token: print,        lexeme: %s\n" l
 *   | Add          -> Format.printf "token: add,          lexeme: +\n"
 *   | Sub          -> Format.printf "token: sub,          lexeme: -\n"
 *   | Mul          -> Format.printf "token: mul,          lexeme: *\n"
 *   | Div          -> Format.printf "token: div,          lexeme: /\n"
 *   | Eof l        -> Format.printf "token: eof,          lexeme: %s\n" l
 *   | Semi         -> Format.printf "token: semi,         lexeme: ;\n"
 *   | Id l         -> Format.printf "token: Id,           lexeme: %s\n" l
 *   | Var          -> Format.printf "token: Var,          lexeme: var\n"
 *   | Equal        -> Format.printf "token: Equal,        lexeme: =\n"
 *   | EqualEqual   -> Format.printf "token: EqualEqual,   lexeme: ==\n"
 *   | LessEqual    -> Format.printf "token: LessEqual,    lexeme: <=\n"
 *   | GreaterEqual -> Format.printf "token: GreaterEqual, lexeme: >=\n"
 *   | NotEqual     -> Format.printf "token: NotEqual      lexeme: !=\n"
 *   | Less         -> Format.printf "token: Less,         lexeme: <\n"
 *   | Greater      -> Format.printf "token: Greater,      lexeme: >\n"
 *   | Bool l       -> Format.printf "token: Bool,         lexeme: %B\n" l *)
