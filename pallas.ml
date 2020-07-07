open Format
open Token
open Lexer
open Parser

(* TODO: comparison *)
(* TODO: run file from main *)

exception RuntimeError of string

let environment = Hashtbl.create 1024

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


let rec eval_expr ast =
  match ast with
  | Sum (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y       -> Int (x + y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Diff (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y       -> Int (x + y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Frac (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y       -> Int (x + y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Prod (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y       -> Int (x + y)
     | String x, String y -> String (x ^ y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Comparison (token, expr1, expr2) ->        
     begin match token, eval_expr expr1, eval_expr expr2 with
      | Tok_EqualEqual _, Int x, Int y       -> Bool (x == y)
      | Tok_EqualEqual _, Bool x, Bool y     -> Bool (x == y)
      | Tok_EqualEqual _, String x, String y -> Bool (x == y)
      | Tok_NotEqual _, Int x, Int y         -> Bool (x != y)
      | Tok_NotEqual _, Bool x, Bool y       -> Bool (x != y)
      | Tok_NotEqual _, String x, String y   -> Bool (x != y)
      | Tok_GreaterEqual _, Int x, Int y     -> Bool (x >= y)
      | Tok_LessEqual _, Int x, Int y        -> Bool (x <= y)
      | Tok_Less _, Int x, Int y             -> Bool (x < y)
      | Tok_Greater _, Int x, Int y          -> Bool (x > y)
      | _, String x, Int y ->
         raise (RuntimeError "Cannot compare string with int")
      | _, String x, Bool y ->
         raise (RuntimeError "Cannot compare string with bool")
      | _, Int x, String y ->
        raise (RuntimeError "Cannot compare int with string")
      | _, Int x, Bool y ->
         raise (RuntimeError "Cannot compare int with bool")
      | _, Bool x, Int y ->
         raise (RuntimeError "Cannot compare bool with int")
      | _, Bool x, String y ->
         raise (RuntimeError "Cannot compare bool with string")
      | _ ->
         raise (RuntimeError "Could not evaluate ast")
     end
  | Id id   -> Hashtbl.find environment id
  | Value v -> v

let eval_stmt s =
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

let eval (statements) =
  List.iter eval_stmt statements


let () =
  let program = read_file "test.hi" in
  let tokens = lexer program in
  let statements = parse tokens in
  List.iter print_token tokens;
  eval(statements);

(*
  STATEMENTS

  Stmt     -> Print | Decl | Assign
  Decl     -> var Id "=" Comparison;
  Print    -> "print" Comparison ";"
  Assign   -> Id = Comparison;
 
  EXPRESSIONS
  
  Comparison -> Add ("!="|"=="|">"|">="|"<"|"<=") Add | Add

  Add        -> Mul ("+"|"-") Add | Mul
  Mul        -> Prim ("*"|"/") Mul | Prim
 

  Prim  -> Num | Id | Boolean
  Num   -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  Id    -> [A-Za-z]
  Boolean -> true | false
 *)
