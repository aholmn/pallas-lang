type value =
  | Bool   of bool
  | Int    of int
  | String of string

type expr =
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

type stmt =
  | Print of expr
  | Declaration of string * expr
  | Assign of string * expr
  | If of expr * stmt list * stmt list
