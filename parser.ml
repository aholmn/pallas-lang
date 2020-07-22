open Token
open Format

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
                         (sprintf "expected %s but got %s"
                            (token_to_str token) (token_to_str hd)))
  | [] ->
     raise (InvalidExpression "no tokens left to consume")

type value =
  | Bool   of bool
  | Int    of int
  | String of string

type expr =
  | Value        of value
  | Sum          of expr * expr
  | Diff         of expr * expr
  | Frac         of expr * expr
  | Prod         of expr * expr
  | Greater      of expr * expr
  | Lesser       of expr * expr
  | GreaterEqual of expr * expr
  | LesserEqual  of expr * expr
  | Equal        of expr * expr
  | NotEqual     of expr * expr
  | Id           of string

type stmt =
  | Print of expr
  | Declaration of string * expr
  | Assign of string * expr
  | If of expr * stmt list * stmt list

let parse_primary () =
  let token = peek () in
  match token with
  | Tok_Num value ->
     consume token;
     Value (Int value)
  | Tok_Bool value ->
     consume token;
     Value (Bool value)
  | Tok_Id value ->
     consume token;
     Id value
  | Tok_Str value ->
     consume token;
     Value (String value)
  | _ ->
     raise (InvalidExpression
              (sprintf "expected value but got %s" (token_to_str token)))

let rec parse_multiplication () =
  let left  = parse_primary () in
  let token = peek () in
  match token with
  | Tok_Mul ->
     consume token;
     Prod (left, parse_multiplication ())
  | Tok_Div ->
     consume token;
     Frac (left, parse_multiplication ())
  | _ ->
     left


let rec parse_addition () =
  let left = parse_multiplication () in
  let token = peek () in
  match token with
  | Tok_Add ->
     consume token;
     Sum (left, parse_addition ())
  | Tok_Sub ->
     consume token;
     Diff (left, parse_addition ())
  | _ ->
     left

let rec parse_comparison () =
  let left = parse_addition () in
  let token = peek () in
  match token with
  | Tok_EqualEqual ->
     consume token;
     Equal (left, parse_comparison ())
  | Tok_NotEqual ->
     consume token;
     NotEqual (left, parse_comparison ())
  | Tok_GreaterEqual ->
     consume token;
     GreaterEqual (left, parse_comparison ())
  | Tok_LessEqual ->
     consume token;
     LesserEqual (left, parse_comparison ())
  | Tok_Less ->
     consume token;
     Lesser (left, parse_comparison ())
  | Tok_Greater ->
     consume token;
     Greater (left, parse_comparison ())
  | _ ->
     left

let rec parse_statement () =
  let token = peek() in
  match token with
  | Tok_Print _ ->
     consume token;
     let expr = parse_comparison () in
     consume Tok_Semi;
     Print expr
  | Tok_Id str ->
     consume token;
     consume Tok_Equal;
     let expr = parse_comparison () in
     consume Tok_Semi;
     Assign (str, expr)
  | Tok_Var ->
     consume token;
     let next_token = peek () in
     begin match next_token with
     | Tok_Id str ->
        consume next_token;
        consume Tok_Equal;
        let expr = parse_comparison () in
        consume Tok_Semi;
        Declaration (str, expr)
     | _ ->
        raise (InvalidExpression "expected variable name after var")
     end
  | Tok_If ->
     consume token;
     let expr, if_stmts, else_stmts = parse_if_statement () in
     If (expr, if_stmts, else_stmts)
  | _ ->
     raise (InvalidExpression "invalid statement")

and parse_if_statement () =
  let expr = parse_comparison () in
     consume Tok_Do;
     let if_stmts = parse_statements_until [Tok_End; Tok_Else] [] in
     let else_stmts = match peek () with
       | Tok_Else ->
          consume Tok_Else;
          let s = parse_statements_until [Tok_End] [] in
          consume Tok_End;
          s
       | Tok_End ->
          consume Tok_End;
          []
       | token ->
          raise (InvalidExpression
                   (sprintf "expected end but got %s" (token_to_str token)))
     in
     (expr, if_stmts, else_stmts)

and parse_statements_until tokens acc =
  if List.mem (peek ()) tokens then List.rev acc
  else parse_statements_until tokens (parse_statement ()::acc)

let rec parse_program acc =
  let token = peek () in
  match token with
  | Tok_Eof ->
     List.rev acc
  | _ ->
     let statement =  parse_statement () in
     parse_program(statement::acc)

let parse (ts: token list) : stmt list =
  tokens := ts;
  parse_program []