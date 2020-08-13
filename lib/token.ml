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
  | LeftBracket
  | RightBracket

let str = function
  | Str v        -> v
  | Num v        -> string_of_float v
  | Div          -> "/"
  | Mul          -> "*"
  | Add          -> "+"
  | Sub          -> "-"
  | Eof          -> "eof"
  | Semi         -> ";"
  | Id v         -> v
  | Var          -> "var"
  | Equal        -> "="
  | EqualEqual   -> "=="
  | LessEqual    -> "<="
  | GreaterEqual -> ">="
  | NotEqual     -> "!="
  | Less         -> "<"
  | Greater      -> ">"
  | Bool v       ->  string_of_bool v
  | End          -> "end"
  | If           -> "if"
  | Do           -> "do"
  | Else         -> "else"
  | LeftParen    -> "("
  | RightParen   -> ")"
  | Comma        -> ","
  | Def          -> "def"
  | Return       -> "return"
  | LeftBracket  -> "["
  | RightBracket -> "]"
