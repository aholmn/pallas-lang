open Format
open Token
open Lexer
open Parser

exception RuntimeError of string

let rec eval_expr (env: Env.t) (ast: expr) : value =
  let eval_expr' expr = eval_expr env expr in
  match ast with
  | Sum (expr1, expr2) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (x + y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Diff (expr1, expr2) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (x - y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Frac (expr1, expr2) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (x / y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Prod (expr1, expr2) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (x * y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Greater (expr1, expr2) ->
     Bool (eval_relation (>)  (eval_expr' expr1) (eval_expr' expr2))
  | GreaterEqual (expr1, expr2) ->
     Bool (eval_relation (>=) (eval_expr' expr1) (eval_expr' expr2))
  | LesserEqual (expr1, expr2) ->
     Bool (eval_relation (<=) (eval_expr' expr1) (eval_expr' expr2))
  | Lesser (expr1, expr2) ->
     Bool (eval_relation (<)  (eval_expr' expr1) (eval_expr' expr2))
  | NotEqual (expr1, expr2) ->
     Bool (eval_not_equal (eval_expr' expr1) (eval_expr' expr2))
  | Equal (expr1, expr2) ->
     Bool (eval_equal (eval_expr' expr2) (eval_expr' expr2))
  | Id name   ->
     Env.lookup env name
  | Value v ->
     v

and truthy (x : value) : bool =
  match x with
  | Bool b -> b
  | Int  i -> if i > 0 then true else false
  | String _ -> false

and eval_relation (op : 'a) (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | Int i1, Int i2 ->
     op i1 i2
  | _ ->
     raise (RuntimeError "Cannot use '<' '<=' '>' '>=' on anything else than integers")

and eval_equal (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | String s1, String s2 ->
     s1 == s2
  | Int i1, Int i2 ->
     i1 == i2
  |  _ ->
     (truthy v1) == (truthy v2)

and eval_not_equal v1 v2 =
  match v1, v2 with
  | Int x, Int y ->
     x != y
  | String x, String y ->
     x != y
  |  _ ->
     truthy v1 == truthy v2


let rec eval_stmt (env: Env.t) (s: stmt) : unit =
  match s with
  | Print e ->
     begin match eval_expr env e with
     | Int x    -> printf "%d\n" x
     | Bool x   -> printf "%B\n" x
     | String x -> printf "%s\n" x
     end
  | Declaration (s, e) ->
     let x = eval_expr env e in
     Env.add env s x
  | Assign (s, e) ->
     Env.replace env s (eval_expr env e)
  | If (expr, if_stmt, else_stmt) ->
     begin match eval_expr env expr with
     | Bool s ->
        let inner = Env.make (Some env) in
        if s then List.iter (eval_stmt inner) if_stmt
        else List.iter (eval_stmt inner) else_stmt
     | _ ->
        raise (RuntimeError ("cannot compar"));
     end

let eval (statements: stmt list) : unit =
  let env = Env.make None in
  List.iter (eval_stmt env) statements

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  try
    let program = read_file Sys.argv.(1)  in
    let tokens = lexer program in
    (* List.iter printf tokens; *)
    let statements = parse tokens in
    eval(statements)
  with
  | RuntimeError s ->
     printf "RuntimeError: %s\n" s
  | Env.NotfoundError s ->
     printf "NotfoundError: %s\n" s
