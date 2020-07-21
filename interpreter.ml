open Format
open Token
open Lexer
open Parser

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
     | Int x, Int y       -> Int (x - y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Frac (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y       -> Int (x / y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Prod (expr1, expr2) ->
     begin match eval_expr expr1, eval_expr expr2 with
     | Int x, Int y       -> Int (x * y)
     | _, _ ->
        raise (RuntimeError "Cannot eval expression")
     end
  | Comparison (token, expr1, expr2) ->        
     begin match token, eval_expr expr1, eval_expr expr2 with
      | Tok_EqualEqual,  Int x, Int y       -> Bool (x == y)
      | Tok_EqualEqual, Bool x, Bool y     -> Bool (x == y)
      | Tok_EqualEqual,  String x, String y -> Bool (x == y)
      | Tok_NotEqual,  Int x, Int y         -> Bool (x != y)
      | Tok_NotEqual,  Bool x, Bool y       -> Bool (x != y)
      | Tok_NotEqual,  String x, String y   -> Bool (x != y)
      | Tok_GreaterEqual, Int x, Int y     -> Bool (x >= y)
      | Tok_LessEqual, Int x, Int y        -> Bool (x <= y)
      | Tok_Less, Int x, Int y             -> Bool (x < y)
      | Tok_Greater,  Int x, Int y          -> Bool (x > y)
      | _, String _, Int _ ->
         raise (RuntimeError "Cannot compare string with int")
      | _, String _, Bool _ ->
         raise (RuntimeError "Cannot compare string with bool")
      | _, Int _, String _ ->
        raise (RuntimeError "Cannot compare int with string")
      | _, Int _, Bool _ ->
         raise (RuntimeError "Cannot compare int with bool")
      | _, Bool _, Int _ ->
         raise (RuntimeError "Cannot compare bool with int")
      | _, Bool _, String _ ->
         raise (RuntimeError "Cannot compare bool with string")
      | _ ->
         raise (RuntimeError "Could not evaluate ast")
     end
  | Id id   -> Hashtbl.find environment id
  | Value v -> v
     

let rec eval_stmt s =
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
  | If (expr, stmt) ->
     begin match eval_expr(expr) with
     | Bool s ->
        if s then
          List.iter eval_stmt stmt
     | _ ->
        raise (RuntimeError ("cannot compar"));
     end

               
let eval statements =
  List.iter eval_stmt statements

let () =
  let program = read_file Sys.argv.(1)  in
  let tokens = lexer program in
  (* List.iter printf tokens; *)
  let statements = parse tokens in
  eval(statements);

