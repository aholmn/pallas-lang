open Pallas_exn

let check_args name arity args =
  let l = List.length args in
  if l > arity then
    raise (RuntimeError (Format.sprintf
                           "%s expected %d arguments but got %d" name arity l))
  else
    None

(* Converts a given float to string if the float's decimal is zero
   the dot is removed, e.g. with 2.0 returns 2.
 *)
let float_to_str f =
  let str = string_of_float f in
  let rec parse' i =
    match str.[i] = '.' with
    | true ->
       if i + 1 >= String.length str then
         String.sub str 0 i
       else
         str
    | false ->
       parse' (i + 1)
  in
  parse' 0

let println =
  let f args = (
      ignore (check_args "println" 1 args);
      let value = List.nth args 0 in
      begin match value with
      | Ast.Int x          ->
         let s = float_to_str x in
         Format.printf "%s\n" s
      | Ast.Bool x         -> Format.printf "%B\n" x
      | Ast.String x       -> Format.printf "%s\n" x
      | Ast.Null           -> Format.printf "null\n"
      | Ast.Callable (x,_) -> Format.printf "function: %s\n" x
      end;
      Ast.Null
    )
  in
  Ast.Callable ("println", f)
