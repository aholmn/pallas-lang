type value =
  | Bool   of bool
  | Number of float
  | String of string
  | Null
  | Callable of string * (value list -> value)
  | Values of value list

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
  | Array        of expr list
  | IndexExpr    of expr * expr

and stmt =
  | Declaration of string * expr
  | If of expr * stmt list * stmt list
  | Function of string * string list * stmt list
  | ExprStmt of expr
  | Return of expr option
  | While of expr * stmt list
  | For of expr * expr * stmt list

let rec value_to_str = function
  | Bool v -> Format.sprintf "%B" v
  | Number v -> number_to_str v
  | String v -> v
  | Null -> "null"
  | Callable (v, _) -> Format.sprintf "function <%s>" v
  | Values v ->
     Format.sprintf "[%s]" (value_to_str_aux v)

and value_to_str_aux = function
  | x::xs::xss ->
     Format.sprintf "%s, " (value_to_str x) ^ value_to_str_aux (xs::xss)
  | x::xs ->
     value_to_str x ^ value_to_str_aux xs
  | [] ->
     ""

(* Return a given float as string if the float's decimal is zero
 * the dot is removed. E.g. if input is 2.0 then "2" is returned. *)
and number_to_str f =
  let str = string_of_float f in
  let rec convert i =
    match str.[i] = '.' with
    | true ->
       if i + 1 >= String.length str then String.sub str 0 i
       else str
    | false ->
       convert (i + 1)
  in
  convert 0

(* Tests *)
let%test _ = "hello, world!" =  value_to_str (String "hello, world!")
let%test _ = "0" =  value_to_str (Number 0.0)
let%test _ = "1" =  value_to_str (Number 1.0)
let%test _ = "1" =  value_to_str (Number 1.00)
let%test _ = "1.1" =  value_to_str (Number 1.1)
let%test _ = "1.25" =  value_to_str (Number 1.25)
let%test _ = "[]" =  value_to_str (Values [])
let%test _ = "[1]" =  value_to_str (Values [Number 1.])
let%test _ = "[1, hello, true]" = value_to_str (Values [Number 1.; String "hello" ; Bool true])
