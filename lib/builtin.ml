open Pallas_exn

let check_args name arity args =
  let l = List.length args in
  if l > arity then
    raise (RuntimeError (Format.sprintf
                           "%s expected %d arguments but got %d" name arity l))
  else
    None

let println =
  let f args = (
      ignore (check_args "println" 1 args);
      let value = List.nth args 0 in
      begin match value with
      | Ast.Int x          -> Format.printf "%d\n" x
      | Ast.Bool x         -> Format.printf "%B\n" x
      | Ast.String x       -> Format.printf "%s\n" x
      | Ast.Null           -> Format.printf "null\n"
      | Ast.Callable (x,_) -> Format.printf "function: %s\n" x
      end;
      Ast.Null
    )
  in
  Ast.Callable ("println", f)
