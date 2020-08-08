open Format

exception RuntimeError of string
exception ReturnException of Ast.value

let rec eval_expr (env: Ast.env) (ast: Ast.expr) : Ast.value =
  let eval_expr' expr = eval_expr env expr in
  match ast with
  | Call (callee, params) ->
     let args = List.map eval_expr' params in
     begin match eval_expr' callee with
     | Ast.Callable (_name, fn) ->
        begin try
            fn args
          with ReturnException return_value ->
            return_value
        end
     | _ ->
        failwith("not a function")
     end
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
  | Frac (expr2, expr1) ->
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
     Ast.lookup env name
  | Value v ->
     v
  | Assign (s, e) ->
     let v = eval_expr env e in
     Ast.replace env s v;
     v

and truthy (x : Ast.value) : bool =
  match x with
  | Bool b -> b
  | Int  i -> if i > 0 then true else false
  | String _ -> false
  | Null -> false
  | Callable (_,_) -> false

and eval_relation (op : 'a) (v1 : Ast.value) (v2 : Ast.value) : bool =
  match v1, v2 with
  | Int i1, Int i2 ->
     op i1 i2
  | _ ->
     raise (RuntimeError "Cannot use '<' '<=' '>' '>=' on anything else than integers")

and eval_equal (v1 : Ast.value) (v2 : Ast.value) : bool =
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
      truthy v1 = truthy v2

let rec eval (statements: Ast.stmt list) : unit =
  let env = Ast.make None in
  List.iter (eval_stmt env) statements

and eval_stmt (env: Ast.env) (s: Ast.stmt) : unit =
  match s with
  | Print e ->
     begin match eval_expr env e with
     | Int x    -> printf "%d\n" x
     | Bool x   -> printf "%B\n" x
     | String x -> printf "%s\n" x
     | Null     -> printf "null\n"
     | Callable (x,_ )-> printf "function: %s\n" x
     end
  | Declaration (s, e) ->
     let x = eval_expr env e in
     Ast.add env s x
  | If (expr, if_stmt, else_stmt) ->
     begin match eval_expr env expr with
     | Bool s ->
        let inner = Ast.make (Some env) in
        if s then List.iter (eval_stmt inner) if_stmt
        else List.iter (eval_stmt inner) else_stmt
     | _ ->
        raise (RuntimeError ("cannot compar"));
     end
  | Function (name, params, stmts) ->
     let fn args = (
         List.iter2 (Ast.add env) params args;
         List.iter (eval_stmt env) stmts;
         Ast.Null
       ) in
     Ast.add env name (Ast.Callable (name, fn))
  | ExprStmt expr ->
     ignore (eval_expr env expr)
  | Return value ->
     begin match value with
     | None ->
       raise (ReturnException Ast.Null)
     | Some expr ->
        let value = eval_expr env expr in
        raise (ReturnException value)
     end


let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  try
    let program = read_file Sys.argv.(1)  in
    let tokens = Lexer.scan program in
    let statements = Parser.parse tokens in
    eval(statements)
    with
    | RuntimeError s ->
       printf "RuntimeError: %s\n" s
    | Ast.NotfoundError s ->
       printf "NotfoundError: %s\n" s
