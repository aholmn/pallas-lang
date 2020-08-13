open Pallas_exn

let check_args name arity args =
  let l = List.length args in
  if l > arity then
    raise (RuntimeError (Format.sprintf
                           "%s expected %d arguments but got %d" name arity l))
  else
    None

let print =
  let f args = (
      ignore (check_args "println" 1 args);
      let value = List.nth args 0 in
      Format.printf "%s" (Ast.value_to_str value);
      Ast.Null
    )
  in
  Ast.Callable ("print", f)

let println =
  let f args = (
      ignore (check_args "println" 1 args);
      let value = List.nth args 0 in
      Format.printf "%s\n" (Ast.value_to_str value);
      Ast.Null
    )
  in
  Ast.Callable ("println", f)
