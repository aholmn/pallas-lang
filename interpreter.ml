open Format
open Token
open Lexer
open Parser

exception RuntimeError of string

let environment = Hashtbl.create 1024

let rec eval_expr (ast: expr) : value =
  match ast with
  | Sum (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y -> Int (x + y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Diff (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y -> Int (x - y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Frac (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y -> Int (x / y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Prod (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y -> Int (x * y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Greater (expr1, expr2) ->
     Bool (eval_relation (>)  (eval_expr expr1) (eval_expr expr2))
  | GreaterEqual (expr1, expr2) ->
     Bool (eval_relation (>=) (eval_expr expr1) (eval_expr  expr2))
  | LesserEqual (expr1, expr2) ->
     Bool (eval_relation (<=) (eval_expr expr1) (eval_expr expr2))
  | Lesser (expr1, expr2) ->
     Bool (eval_relation (<)  (eval_expr expr1) (eval_expr expr2))
  | NotEqual (expr1, expr2) ->
     Bool (eval_not_equal (eval_expr expr1) (eval_expr expr2))
  | Equal (expr1, expr2) ->
     Bool (eval_equal (eval_expr expr2) (eval_expr expr2))
  | Id id   ->
     Hashtbl.find environment id
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


let rec eval_stmt (s: stmt) : unit =
  match s with
  | Print e ->
     begin match eval_expr e with
     | Int x    -> printf "%d\n" x
     | Bool x   -> printf "%B\n" x
     | String x -> printf "%s\n" x
     end
  | Declaration (s, e) ->
     let x = eval_expr e in
     Hashtbl.add environment s x
  | Assign (s, e) ->
     if Hashtbl.mem environment s then
       Hashtbl.replace environment s (eval_expr e)
     else
       raise (RuntimeError (sprintf "Cannot assign undeclared variable: %s" s))
  | If (expr, if_stmt, else_stmt) ->
     begin match eval_expr(expr) with
     | Bool s ->
        if s then List.iter eval_stmt if_stmt
        else List.iter eval_stmt else_stmt

     | _ ->
        raise (RuntimeError ("cannot compar"));
     end

let eval (statements: stmt list) : unit =
  List.iter eval_stmt statements

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let () =
  let program = read_file Sys.argv.(1)  in
  let tokens = lexer program in
  (* List.iter printf tokens; *)
  let statements = parse tokens in
  eval(statements);
