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
  | Token.Tok_Num value ->
     consume token;
     Ast.Value (Int value)
  | Token.Tok_Bool value ->
     consume token;
     Ast.Value (Bool value)
  | Token.Tok_Id value ->
     consume token;
     Ast.Id value
  | Token.Tok_Str value ->
     consume token;
     Ast.Value (String value)
  | _ ->
     raise (InvalidExpression
              (Format.sprintf "expected value but got %s" (Token.token_to_str token)))

let rec parse_multiplication () =
  let left  = parse_primary () in
  let token = peek () in
  match token with
  | Token.Tok_Mul ->
     consume token;
     Ast.Prod (left, parse_multiplication ())
  | Token.Tok_Div ->
     consume token;
     Ast.Frac (left, parse_multiplication ())
  | _ ->
     left


let rec parse_addition () =
  let left = parse_multiplication () in
  let token = peek () in
  match token with
  | Token.Tok_Add ->
     consume token;
     Ast.Sum (left, parse_addition ())
  | Token.Tok_Sub ->
     consume token;
     Ast.Diff (left, parse_addition ())
  | _ ->
     left

let rec parse_comparison () =
  let left = parse_addition () in
  let token = peek () in
  match token with
  | Token.Tok_EqualEqual ->
     consume token;
     Ast.Equal (left, parse_comparison ())
  | Token.Tok_NotEqual ->
     consume token;
     Ast.NotEqual (left, parse_comparison ())
  | Token.Tok_GreaterEqual ->
     consume token;
     Ast.GreaterEqual (left, parse_comparison ())
  | Token.Tok_LessEqual ->
     consume token;
     Ast.LesserEqual (left, parse_comparison ())
  | Token.Tok_Less ->
     consume token;
     Ast.Lesser (left, parse_comparison ())
  | Token.Tok_Greater ->
     consume token;
     Ast.Greater (left, parse_comparison ())
  | _ ->
     left

let rec parse_statement () =
  let token = peek() in
  match token with
  | Token.Tok_Print _ ->
     consume token;
     let expr = parse_comparison () in
     consume Token.Tok_Semi;
     Ast.Print expr
  | Token.Tok_Id str ->
     consume token;
     consume Token.Tok_Equal;
     let expr = parse_comparison () in
     consume Token.Tok_Semi;
     Ast.Assign (str, expr)
  | Token.Tok_Var ->
     consume token;
     let next_token = peek () in
     begin match next_token with
     | Token.Tok_Id str ->
        consume next_token;
        consume Token.Tok_Equal;
        let expr = parse_comparison () in
        consume Token.Tok_Semi;
        Ast.Declaration (str, expr)
     | _ ->
        raise (InvalidExpression "expected variable name after var")
     end
  | Token.Tok_If ->
     consume token;
     let expr, if_stmts, else_stmts = parse_if_statement () in
     Ast.If (expr, if_stmts, else_stmts)
  | _ ->
     raise (InvalidExpression "invalid statement")

and parse_if_statement () =
  let expr = parse_comparison () in
     consume Token.Tok_Do;
     let if_stmts = parse_statements_until [Token.Tok_End; Token.Tok_Else] [] in
     let else_stmts = match peek () with
       | Token.Tok_Else ->
          consume Token.Tok_Else;
          let s = parse_statements_until [Token.Tok_End] [] in
          consume Token.Tok_End;
          s
       | Token.Tok_End ->
          consume Token.Tok_End;
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
  | Token.Tok_Eof ->
     List.rev acc
  | _ ->
     let statement =  parse_statement () in
     parse_program(statement::acc)

let parse (ts: Token.token list) : Ast.stmt list =
  tokens := ts;
  parse_program []
