let tokens = ref []

exception InvalidExpression of string

let rec parse (ts: Token.token list) : Ast.stmt list =
  tokens := ts;
  parse_program []

and  parse_program acc =
  match peek () = Token.Eof with
  | true ->
     List.rev acc
  | false ->
     let stmt =  parse_statement () in
     parse_program(stmt::acc)

and parse_statement () =
  let token = peek() in
  match token with
  | Token.Var ->
     parse_var_statement ()
  | Token.If ->
     parse_if_statement ()
  | Token.Def ->
     parse_def_statement ()
  | Token.Return ->
     parse_return_statement ()
  | Token.While ->
     parse_while_statement ()
  | _ ->
     let expr = parse_expression () in
     consume Token.Semi;
     Ast.ExprStmt (expr);

and parse_while_statement () =
  consume Token.While;
  let condition = parse_expression () in
  consume Token.Do;
  let stmts = parse_statements_until [Token.End] [] in
  consume Token.End;
  Ast.While (condition, stmts)

and parse_return_statement () =
  consume Token.Return;
  let value = match peek () = Token.Semi with
  | true ->
     None
  | false ->
     Some (parse_expression ())
  in
  consume Token.Semi;
  Ast.Return (value)


and parse_var_statement () =
  consume Token.Var;
  let next_token = peek () in
  match next_token with
  | Token.Id id ->
     consume next_token;
     consume Token.Equal;
     let expr = parse_expression () in
     consume Token.Semi;
     Ast.Declaration (id, expr)
  | token ->
     raise (InvalidExpression
              (Format.sprintf
                 "expected variable name after var but got %s"
                 (Token.str token)))

and parse_if_statement () =
  consume Token.If;
  let expr = parse_expression () in
  consume Token.Do;
  let if_stmts = parse_statements_until [Token.End; Token.Else] [] in
  let else_stmts = match peek () with
    | Token.Else ->
       consume Token.Else;
       let s = parse_statements_until [Token.End] [] in
       consume Token.End;
       s
    | Token.End ->
       consume Token.End;
       []
    | token ->
       raise (InvalidExpression
                (Format.sprintf "expected end but got %s" (Token.str token)))
  in
  Ast.If (expr, if_stmts, else_stmts)

and parse_def_statement () =
  consume Token.Def;
  let next = peek () in
  begin match next with
  | Token.Id name ->
     consume next;
     consume Token.LeftParen;
     let args = parse_params [] in
     consume Token.RightParen;
     consume Token.Do;
     let stmts = parse_statements_until [Token.End] [] in
     consume Token.End;
     Ast.Function (name, args, stmts)
  | token ->
     raise (InvalidExpression
              (Format.sprintf
                 "expected function name after def but got end but got %s"
                 (Token.str token)))
  end

and parse_expression () =
  let left = parse_assignment () in
  match peek () = Token.LeftBracket with
  | true ->
     consume Token.LeftBracket;
     let right = parse_expression () in
     consume Token.RightBracket;
     Ast.IndexExpr (left, right)
  | false ->
     left

and parse_assignment () =
  let comparison = parse_comparison () in
  match peek () = Token.Equal with
  | true ->
     consume Token.Equal;
     begin match comparison with
     | Ast.Id id ->
        Ast.Assign (id, parse_comparison ())
     | _ ->
        raise (InvalidExpression "invalid assignment target")
     end
  | false ->
     comparison

and parse_comparison () =
  let left = parse_addition () in
  let token = peek () in
  match token with
  | Token.EqualEqual ->
     consume token;
     Ast.Equal (left, parse_comparison ())
  | Token.NotEqual ->
     consume token;
     Ast.NotEqual (left, parse_comparison ())
  | Token.GreaterEqual ->
     consume token;
     Ast.GreaterEqual (left, parse_comparison ())
  | Token.LessEqual ->
     consume token;
     Ast.LesserEqual (left, parse_comparison ())
  | Token.Less ->
     consume token;
     Ast.Lesser (left, parse_comparison ())
  | Token.Greater ->
     consume token;
     Ast.Greater (left, parse_comparison ())
  | _ ->
     left

and parse_addition () =
  let left = parse_multiplication () in
  let token = peek () in
  match token with
  | Token.Add ->
     consume token;
     Ast.Sum (left, parse_addition ())
  | Token.Sub ->
     consume token;
     Ast.Diff (left, parse_addition ())
  | _ ->
     left

and parse_multiplication () =
  let left  = parse_array () in
  let token = peek () in
  match token with
  | Token.Mul ->
     consume token;
     Ast.Prod (left, parse_multiplication ())
  | Token.Div ->
     consume token;
     Ast.Frac (left, parse_multiplication ())
  | _ ->
     left

and parse_call () =
  let p = parse_primary () in
  match peek () with
  | Token.LeftParen ->
     consume Token.LeftParen;
     let args = parse_expr_list [] Token.RightParen in
     consume Token.RightParen;
     finnish_call (Ast.Call (p, args))
  | _ ->
     p

and parse_array () =
  match peek () = Token.LeftBracket with
  | true ->
     consume Token.LeftBracket;
     let elements = parse_expr_list [] Token.RightBracket in
     consume Token.RightBracket;
     Ast.Array elements
  | false ->
     parse_call ()

and parse_primary () =
  let token = peek () in
  match token with
  | Token.Num value ->
     consume token;
     Ast.Value (Number value)
  | Token.Bool value ->
     consume token;
     Ast.Value (Bool value)
  | Token.Id value ->
     consume token;
     Ast.Id value
  | Token.Str value ->
     consume token;
     Ast.Value (String value)
  | _ ->
     raise (InvalidExpression
              (Format.sprintf "expected value but got %s" (Token.str token)))

and finnish_call callee =
  match peek () = Token.LeftParen with
  | true ->
     consume Token.LeftParen;
     let args = parse_expr_list [] Token.RightParen in
     consume Token.RightParen;
     finnish_call (Ast.Call (callee, args))
  | false ->
     callee


and parse_expr_list acc token =
  match peek () = token with
  | true ->
     List.rev acc
  | false ->
     let expr = parse_expression () in
     match Token.Comma = peek () with
     | true ->
        consume Token.Comma;
        parse_expr_list (expr::acc) token
     | false ->
        parse_expr_list (expr::acc) token

and parse_params params =
  match peek () = Token.RightParen with
  | true ->
     List.rev params
  | false ->
     begin match params with
     | [] ->
        let p = get_param () in
        parse_params (p::params)
     | _::_ ->
        consume Token.Comma;
        let p = get_param () in
        parse_params (p::params)
     end

and get_param () =
  let primary = parse_primary () in
  match primary with
  | Ast.Id id -> id
  | _ ->
     raise (InvalidExpression
              "invalid params in function definition")

and parse_statements_until tokens acc =
  if List.mem (peek ()) tokens then List.rev acc
  else parse_statements_until tokens (parse_statement ()::acc)

and peek () =
  match !tokens with
  | hd::_tl -> hd
  | []     -> raise (InvalidExpression "no tokens left to parse")

and consume token =
  match !tokens with
  | hd::tl -> if hd = token then tokens := tl
              else
                raise (InvalidExpression
                         (Format.sprintf "expected %s but got %s"
                            (Token.str token) (Token.str hd)))
  | [] ->
     raise (InvalidExpression "no tokens left to consume")
