type value =
  | Bool   of bool
  | Int    of float
  | String of string
  | Null
  | Callable of string * (value list -> value)

and expr =
  | Value        of value
  | Sum          of expr * expr
  | Diff         of expr * expr
  | Frac         of expr * expr
  | Prod         of expr * expr
  | Greater      of expr * expr
  | Lesser       of expr * expr
  | GreaterEqual of expr * expr
  | LesserEqual  of expr * expr
  | Equal        of expr * expr
  | NotEqual     of expr * expr
  | Id           of string
  | Call         of expr * expr list
  | Assign       of string * expr

and stmt =
  | Declaration of string * expr
  | If of expr * stmt list * stmt list
  | Function of string * string list * stmt list
  | ExprStmt of expr
  | Return of expr option
