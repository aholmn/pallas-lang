let tokens = ref []

exception InvalidExpression of string

let peek () =
  match !tokens with
  | hd::_tl -> hd
  | []     -> raise (InvalidExpression "no tokens left to parse")

let consume token =
  match !tokens with
  | hd::tl -> if hd = token then tokens := tl
              else
                raise (InvalidExpression
                         (Format.sprintf "expected %s but got %s"
                            (Token.token_to_str token) (Token.token_to_str hd)))
  | [] ->
     raise (InvalidExpression "no tokens left to consume")

let parse_primary () =
  let token = peek () in
  match token with
  | Token.Num value ->
     consume token;
     Ast.Value (Int value)
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
              (Format.sprintf "expected value but got %s" (Token.token_to_str token)))

let rec parse_multiplication () =
  let left  = parse_primary () in
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


let rec parse_addition () =
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

let rec parse_comparison () =
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

let rec parse_statement () =
  let token = peek() in
  match token with
  | Token.Print _ ->
     consume token;
     let expr = parse_comparison () in
     consume Token.Semi;
     Ast.Print expr
  | Token.Id str ->
     consume token;
     consume Token.Equal;
     let expr = parse_comparison () in
     consume Token.Semi;
     Ast.Assign (str, expr)
  | Token.Var ->
     consume token;
     let next_token = peek () in
     begin match next_token with
     | Token.Id str ->
        consume next_token;
        consume Token.Equal;
        let expr = parse_comparison () in
        consume Token.Semi;
        Ast.Declaration (str, expr)
     | _ ->
        raise (InvalidExpression "expected variable name after var")
     end
  | Token.If ->
     consume token;
     let expr, if_stmts, else_stmts = parse_if_statement () in
     Ast.If (expr, if_stmts, else_stmts)
  | _ ->
     raise (InvalidExpression "invalid statement")

and parse_if_statement () =
  let expr = parse_comparison () in
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
                   (Format.sprintf "expected end but got %s" (Token.token_to_str token)))
     in
     (expr, if_stmts, else_stmts)

and parse_statements_until tokens acc =
  if List.mem (peek ()) tokens then List.rev acc
  else parse_statements_until tokens (parse_statement ()::acc)

let rec parse_program acc =
  let token = peek () in
  match token with
  | Token.Eof ->
     List.rev acc
  | _ ->
     let statement =  parse_statement () in
     parse_program(statement::acc)

let parse (ts: Token.token list) : Ast.stmt list =
  tokens := ts;
  parse_program []
