open Pallas_exn

let function_factory f arity name =
  let fn args =
    let len = List.length args in
    match len > arity with
    | true ->
       raise (RuntimeError (Format.sprintf "%s expected %d arguments but got %d" name arity len))
    | false ->
       f args;
  in
  Ast.Callable (name, fn)

let print =
  let f args = (
      let value = List.nth args 0 in
      Format.printf "%s" (Ast.value_to_str value);
      Ast.Null
    )
  in
  function_factory f 1 "println"

let println =
  let f args = (
      let value = List.nth args 0 in
      Format.printf "%s\n" (Ast.value_to_str value);
      Ast.Null
    )
  in
  function_factory f 1 "println"

let length =
  let f args =
    let value = List.nth args 0 in
    match value with
    | Ast.Values values ->
       Ast.Number (float_of_int (List.length values))
    | Ast.String str ->
       Ast.Number (float_of_int (String.length str))
    | _ ->
       raise (RuntimeError "length/1 is only supported on string or array")
  in
  function_factory f 1 "length"
