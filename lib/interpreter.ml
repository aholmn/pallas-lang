open Format
open Pallas_exn

exception ReturnException of Ast.value

let rec eval_expr (env: Env.env) (ast: Ast.expr) : Ast.value =
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
     | Int x, Int y -> Int (Float.add x y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Diff (expr1, expr2) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (Float.sub x y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Frac (expr2, expr1) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (Float.div y x)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Prod (expr1, expr2) ->
     begin match eval_expr' expr1, eval_expr' expr2 with
     | Int x, Int y -> Int (Float.mul x y)
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
     Bool (eval_equal (eval_expr' expr1) (eval_expr' expr2))
  | Id name   ->
     Env.lookup env name
  | Value v ->
     v
  | Assign (s, e) ->
     let v = eval_expr env e in
     Env.replace env s v;
     v

and truthy (x : Ast.value) : bool =
  match x with
  | Bool b -> b
  | Int  i -> if i > 0. then true else false
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
     s1 = s2
  | Int i1, Int i2 ->
     i1 = i2
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

and eval stmts env =
  match stmts with
  | stmt::t ->
     eval t (eval_stmt env stmt)
  | [] ->
     ()

(* Evaluates a statement and returns an environment.
   If the statment is a ’Declaration’ a new environment is created, with the
   the input environment as its parent, and returned.
 *)
and eval_stmt (env: Env.env) (s: Ast.stmt) : Env.env =
  match s with
  | Declaration (s, e) ->
     let x = eval_expr env e in
     let inner = Env.make (Some env) in
     Env.add inner s x;
     inner
  | If (expr, if_stmt, else_stmt) ->
     begin match eval_expr env expr with
     | Bool s ->
        let inner = Env.make (Some env) in
        if s then eval if_stmt inner
        else eval else_stmt inner
     | _ ->
        raise (RuntimeError ("cannot compar"));
     end;
     env
  | Function (name, params, stmts) ->
     let fn args = (
         let closure = Env.make (Some env) in
         List.iter2 (Env.add closure) params args;
         eval stmts closure;
         Ast.Null
       ) in
     Env.add env name (Ast.Callable (name, fn));
     env
  | ExprStmt expr ->
     ignore (eval_expr env expr);
     env
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

let interpreter file =
  try
    let program = read_file file in
    let tokens = Lexer.scan program in
    let statements = Parser.parse tokens in
    let env = Env.make None in
    Env.add env "println" Builtin.println;
    eval statements env
    with
    | RuntimeError s ->
       printf "RuntimeError: %s\n" s
    | Env.NotfoundError s ->
       printf "NotfoundError: %s\n" s
