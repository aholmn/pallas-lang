let rec scan input =
  let rec scan_aux input pos tokens =
    if pos >= String.length input then
      List.rev (Token.Tok_Eof::tokens)
    else
      if is_digit input.[pos] then
        let (next_pos, digit) = get_digit input pos in
        scan_aux input next_pos (Token.Tok_Num (digit)::tokens)
      else if is_alpha input.[pos] then
        let (next_pos, alpha) = get_alphanumerical input pos in
        scan_aux input next_pos ((keyword alpha)::tokens)
      else
        match input.[pos] with
        | ' '  -> scan_aux input (pos+1) tokens
        | '\t' -> scan_aux input (pos+1) tokens
        | '+'  -> scan_aux input (pos+1) (Token.Tok_Add::tokens)
        | '-'  -> scan_aux input (pos+1) (Token.Tok_Sub::tokens)
        | '*'  -> scan_aux input (pos+1) (Token.Tok_Mul::tokens)
        | '/'  -> scan_aux input (pos+1) (Token.Tok_Div::tokens)
        | ';'  -> scan_aux input (pos+1) (Token.Tok_Semi::tokens)
        | '>'  ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.Tok_GreaterEqual::tokens)
           else
             scan_aux input (pos+1) (Token.Tok_Greater::tokens)
        | '<'  ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.Tok_LessEqual::tokens)
           else
             scan_aux input (pos+1) (Token.Tok_Less::tokens)
        | '!'  ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.Tok_NotEqual::tokens)
           else
             failwith "Unary not yet implemented"
        | '=' ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.Tok_EqualEqual::tokens)
           else
             scan_aux input (pos+1) (Token.Tok_Equal::tokens)
        | '\"' ->
           let str = parse_str input (pos+1) in
           scan_aux input (pos+2 + String.length str) (Token.Tok_Str (str)::tokens)
        | _ ->
           scan_aux input (pos+1) tokens
  in
  scan_aux input 0 []

and keyword s =
  match s with
  | "print" -> Token.Tok_Print s
  | "var"   -> Token.Tok_Var
  | "false" -> Token.Tok_Bool false
  | "true"  -> Token.Tok_Bool true
  | "if"    -> Token.Tok_If
  | "end"   -> Token.Tok_End
  | "do"    -> Token.Tok_Do
  | "else"  -> Token.Tok_Else
  | _       -> Token.Tok_Id s

and is_digit char = let code = Char.code char in
                    code >= Char.code '0' && code <= Char.code '9'

and is_alpha char = let code = Char.code char in
                    code >= Char.code 'A' && code <= Char.code 'z'

and is_alphanumerical char = is_digit char || is_alpha char

and get_alphanumerical str pos =
  let rec get_alpha' i =
    if pos+i < String.length str && is_alphanumerical str.[pos+i] then
      get_alpha' (i + 1)
    else
      (pos+i, String.sub str pos i)
  in
  get_alpha' 0

and get_digit str pos =
  let rec get_digit' i  =
    if pos+i < String.length str && is_digit str.[pos+i] then
      get_digit' (i + 1)
    else
      (pos+i, int_of_string (String.sub str pos i))
  in
  get_digit' 0

and parse_str str pos =
  let rec parse_str' i acc =
    if str.[i] = '\"' then
      acc
    else
      parse_str' (i + 1) (acc ^ String.make 1 str.[i])
  in
  parse_str' pos ""
