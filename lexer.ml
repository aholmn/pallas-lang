open Token

let reg_alpha     = Str.regexp "[A-Za-z]+"
let reg_num       = Str.regexp "[0-9]+"
let reg_add       = Str.regexp "+"
let reg_div       = Str.regexp "/"
let reg_mul       = Str.regexp "*"
let reg_sub       = Str.regexp "-"
let reg_semi      = Str.regexp ";"
let reg_eqeq      = Str.regexp "=="
let reg_noteq     = Str.regexp "!="
let reg_geq       = Str.regexp ">="
let reg_leq       = Str.regexp "<="
let reg_less      = Str.regexp "<"
let reg_greater   = Str.regexp ">"
let reg_eq        = Str.regexp "="

let keyword str =
  match str with
  | "print" -> Tok_Print str
  | "var"   -> Tok_Var 
  | "false" -> Tok_Bool false
  | "true"  -> Tok_Bool true
  | "if"    -> Tok_If
  | "end"   -> Tok_End
  | "do"    -> Tok_Do
  | _       -> Tok_Id str


let lexer (program : string) : token list =
  let rec loop program pos acc =    
    if pos >= String.length program then
      List.rev (Tok_Eof::acc)
    else
      if Str.string_match reg_alpha program pos then
        let value = Str.matched_string program in
        let token = keyword value in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_num program pos then
        let value = Str.matched_string program in
        let token = Tok_Num (int_of_string value) in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_add program pos then
        let value = Str.matched_string program in
        let token = Tok_Add in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_sub program pos then
        let value = Str.matched_string program in
        let token = Tok_Sub in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_mul program pos then
        let value = Str.matched_string program in
        let token = Tok_Mul  in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_div program pos then
        let value = Str.matched_string program in
        let token = Tok_Div in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_semi program pos then
        let value = Str.matched_string program in
        let token = Tok_Semi in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_eqeq program pos then
        let value = Str.matched_string program in
        let token = Tok_EqualEqual in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_noteq program pos then
        let value = Str.matched_string program in
        let token = Tok_NotEqual in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_geq program pos then
        let value = Str.matched_string program in
        let token = Tok_GreaterEqual in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_leq program pos then
        let value = Str.matched_string program in
        let token = Tok_LessEqual in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_less program pos then
        let value = Str.matched_string program in
        let token = Tok_Less in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_greater program pos then
        let value = Str.matched_string program in
        let token = Tok_Greater  in
        loop program (pos + (String.length value)) (token::acc)
      else if Str.string_match reg_eq program pos then
        let value = Str.matched_string program in
        let token = Tok_Equal in
        loop program (pos + (String.length value)) (token::acc)
      else
        loop program (pos + 1) acc
  in
  loop program 0 []  
