let rec scan input =
  let rec scan_aux input pos tokens =
    if pos >= String.length input then
      List.rev (Token.Eof::tokens)
    else
      if is_digit input.[pos] then
        let (next_pos, digit) = get_digit input pos in
        scan_aux input next_pos (Token.Num (digit)::tokens)
      else if is_alpha input.[pos] then
        let (next_pos, alpha) = get_alphanumerical input pos in
        scan_aux input next_pos ((keyword alpha)::tokens)
      else
        match input.[pos] with
        | ' '  -> scan_aux input (pos+1) tokens
        | '\t' -> scan_aux input (pos+1) tokens
        | '+'  -> scan_aux input (pos+1) (Token.Add::tokens)
        | '-'  -> scan_aux input (pos+1) (Token.Sub::tokens)
        | '*'  -> scan_aux input (pos+1) (Token.Mul::tokens)
        | '/'  -> scan_aux input (pos+1) (Token.Div::tokens)
        | ';'  -> scan_aux input (pos+1) (Token.Semi::tokens)
        | '>'  ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.GreaterEqual::tokens)
           else
             scan_aux input (pos+1) (Token.Greater::tokens)
        | '<'  ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.LessEqual::tokens)
           else
             scan_aux input (pos+1) (Token.Less::tokens)
        | '!'  ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.NotEqual::tokens)
           else
             failwith "Unary not yet implemented"
        | '=' ->
           if input.[pos+1] = '=' then
             scan_aux input (pos+2) (Token.EqualEqual::tokens)
           else
             scan_aux input (pos+1) (Token.Equal::tokens)
        | '\"' ->
           if input.[pos+1] = '\"' then
             scan_aux input (pos+2) (Token.Str ("")::tokens)
           else
             let str = parse_str input (pos+1) in
             scan_aux input (pos+2 + String.length str) (Token.Str (str)::tokens)
        | _ ->
           scan_aux input (pos+1) tokens
  in
  scan_aux input 0 []

and keyword s =
  match s with
  | "print" -> Token.Print s
  | "var"   -> Token.Var
  | "false" -> Token.Bool false
  | "true"  -> Token.Bool true
  | "if"    -> Token.If
  | "end"   -> Token.End
  | "do"    -> Token.Do
  | "else"  -> Token.Else
  | _       -> Token.Id s

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
  let rec parse_str' i =
    if str.[pos+i] = '\"' then
      String.sub str pos i
    else
      parse_str' (i + 1)
  in
  parse_str' 0
