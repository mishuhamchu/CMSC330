open TokenTypes
let re_left_paren = Str.regexp "("
let re_right_paren = Str.regexp ")"
let re_left_brace = Str.regexp "{"
let re_right_brace = Str.regexp "}"
let re_equals = Str.regexp "=="
let re_not_equal = Str.regexp "!="
let re_assign = Str.regexp "="
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greater_equal = Str.regexp ">="
let re_less_equal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "!"
let re_semi = Str.regexp ";"
let re_print = Str.regexp "printf"
let re_main = Str.regexp "main"
let re_if = Str.regexp "if"
let re_else = Str.regexp "else"
let re_for = Str.regexp "for"
let re_from = Str.regexp "from"
let re_to = Str.regexp "to"
let re_while = Str.regexp "while"
let re_plus = Str.regexp "+"
let re_minus = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_pow = Str.regexp "\\^"
let re_white_space = Str.regexp "[' '|'\t'|'\n']+"
let re_bool = Str.regexp "true|false"
let re_int = Str.regexp "-?[0-9]+"
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"

let re_bool_type = Str.regexp "bool"
let re_int_type = Str.regexp "int"

let rec tokenize_helper  str pos = 

if pos >= String.length str then
[EOF]
else
if (Str.string_match re_bool_type str pos) then
let token = Str.matched_string str in

Tok_Bool_Type  :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_int_type str pos) then

let token = Str.matched_string str in
 Tok_Int_Type :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_left_paren str pos) then

let token = Str.matched_string str in
 Tok_LParen :: (tokenize_helper  str (pos + String.length(token))) 


else if (Str.string_match re_right_paren str pos) then

let token = Str.matched_string str in
Tok_RParen :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_left_brace str pos) then

 let token = Str.matched_string str in

 Tok_LBrace :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_right_brace str pos) then

 let token = Str.matched_string str in

 Tok_RBrace :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_equals str pos) then

 let token = Str.matched_string str in

 Tok_Equal :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_not_equal str pos) then

 let token = Str.matched_string str in

 Tok_NotEqual :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_assign str pos) then

 let token = Str.matched_string str in

 Tok_Assign :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_greater str pos) then

 let token = Str.matched_string str in

 Tok_Greater :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_pow str pos) then

let token = Str.matched_string str in
Tok_Pow :: (tokenize_helper  str (pos + String.length(token))) 



else if (Str.string_match re_less str pos) then

 let token = Str.matched_string str in

 Tok_Less :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_greater_equal str pos) then
 let token = Str.matched_string str in

 
 Tok_GreaterEqual :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_less_equal str pos) then
 let token = Str.matched_string str in

 
 Tok_LessEqual :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_or str pos) then
 let token = Str.matched_string str in

 
 Tok_Or :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_and str pos) then
 let token = Str.matched_string str in

 
 Tok_And :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_not str pos) then
 let token = Str.matched_string str in

 
 Tok_Not :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_semi str pos) then
 let token = Str.matched_string str in

 
 Tok_Semi :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_print str pos) then
 let token = Str.matched_string str in
 
 Tok_Print :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_main str pos) then
 let token = Str.matched_string str in

 
 Tok_Main :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_if str pos) then
 let token = Str.matched_string str in

 
 Tok_If :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_else str pos) then
 let token = Str.matched_string str in

 

 Tok_Else :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_for str pos) then
 let token = Str.matched_string str in

 

 Tok_For :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_from str pos) then
 let token = Str.matched_string str in

 

 Tok_From :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_to str pos) then
 let token = Str.matched_string str in

 

 Tok_To :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_while str pos) then
 let token = Str.matched_string str in

 

 Tok_While :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_plus str pos) then
 let token = Str.matched_string str in

 

 Tok_Add :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_minus str pos) then
 let token = Str.matched_string str in

 

 Tok_Sub :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_mult str pos) then
 let token = Str.matched_string str in

 Tok_Mult :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_div str pos) then
 
let token = Str.matched_string str in


 
 Tok_Div :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_bool str pos) then
let token = Str.matched_string str in

Tok_Bool(bool_of_string token) :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_int str pos) then


let token = Str.matched_string str in

Tok_Int(int_of_string token) :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_id str pos) then
let token = Str.matched_string str in

Tok_ID(token) :: (tokenize_helper  str (pos + String.length(token))) 

else if (Str.string_match re_white_space str pos) then

let token = Str.matched_string str in

tokenize_helper str (pos + String.length(token))

else

failwith ("Invalid token")





let rec tokenize input = 
tokenize_helper input 0




