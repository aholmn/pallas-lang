type token =
  | Str   of string
  | Num   of float
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
  | LeftParen
  | RightParen
  | Comma
  | Def
  | Return

let str = function
  | Str s        -> s
  | Num l        -> string_of_float l
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
  | Do           -> "do"
  | Else         -> "else"
  | LeftParen    -> "("
  | RightParen   -> ")"
  | Comma        -> ","
  | Def          -> "def"
  | Return       -> "return"
