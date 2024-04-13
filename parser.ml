open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s\n"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

let rec parse_expr toks : expr_result =
  (*let _ = print_string "in the parse_expr\n" in *)
  let tokens, exp = parse_or_expr toks in
  (tokens, exp)

(*------------------OrExpr -> AndExpr || or_expr | And_expr-----------------*)
and parse_or_expr tokens =
  (*let _ = print_string "in the or_expr\n" in *)
  let (new_tok,and_expr) = parse_and_expr tokens in
  let t = lookahead new_tok in

  match t with 
  |Tok_Or->
    let new_tokens = match_token (new_tok) (Tok_Or) in
    let (new_tok, or_expr) = parse_or_expr new_tokens in
    (new_tok, Or(and_expr, or_expr))
  |_->
    (* let _ = print_string "end of or_expr\n" in *)
    (new_tok, and_expr) 

(*--------------------------------------------------------*)
and parse_and_expr tokens =
  (*let _ = print_string "in the and_expr" in*)
  let (new_tok,equal_expr) = parse_equality_expr tokens in
  let t = lookahead new_tok in
  match t with 
  |Tok_And->
    let new_tokens = match_token(new_tok) (Tok_And) in
    let (new_tok, and_expr2) = parse_and_expr (new_tokens) in
    (new_tok, And(equal_expr, and_expr2))
  |_->
    (new_tok,equal_expr) 
(*--------------------------------------------------------*)
and parse_equality_expr tokens =
  let _ = print_string "-------------[in the equality_expr-------]\n" in 
  let (new_tok, equality) = parse_relational_expr tokens in
  let t = lookahead new_tok in
  match t with
  |Tok_Equal ->
    let new_tokens = match_token(new_tok) (Tok_Equal) in
    let (new_tok, equality2) = parse_equality_expr new_tokens in
    (new_tok,  Equal(equality, equality2))

  |Tok_NotEqual -> 
    let new_tokens = match_token(new_tok) (Tok_NotEqual) in
    let (new_tok, equality2) = parse_equality_expr new_tokens in
    (new_tok, NotEqual(equality,equality2))
  |_-> 
    (new_tok, equality)
(*relationalExpr -> additive_Expr Relational operator(< , >, <=, >= Relational_exp| 
  Additive
*)
(*_------------------------------------------------------*)
and parse_relational_expr tokens = 
  let _ = print_string "entered parse_relational\n" in
  (*let _ = print_string "in the relational_expr\n" in *)
  let (new_tok,relate) = parse_additive_expr tokens in
  (*let _ = print_string "in the relational_expr2\n" in *)
  let t = lookahead new_tok in
  match t with
  |Tok_Greater->
    let new_tokens = match_token(new_tok) (Tok_Greater) in
    let (new_tok, relate_2) = parse_relational_expr new_tokens in
    (new_tok, Greater(relate, relate_2))

  |Tok_GreaterEqual->
    let new_tokens = match_token(new_tok) (Tok_GreaterEqual) in
    let (new_tok,relate_2) = parse_relational_expr new_tokens in
    (new_tok, GreaterEqual(relate,relate_2))

  |Tok_Less->
    let new_tokens = match_token(new_tok) (Tok_Less) in
    let (new_tok, relate_2) = parse_relational_expr new_tokens in
    (new_tok, Less(relate, relate_2))

  |Tok_LessEqual->
    let new_tokens = match_token(new_tok) (Tok_LessEqual) in
    let (new_tok, relate_2) = parse_relational_expr new_tokens in
    (new_tok, LessEqual(relate, relate_2))
  |_->
    (new_tok, relate)
(*------------------------------------------------
 * Additive-> [mult_expr] [additive op(+,-)] [additive_expr] | Multiplicative*)

and parse_additive_expr tokens = 
  (*let _ = print_string "in the additive_expr\n" in*)
  let (new_tok, add) = parse_multiplicative_expr tokens in
  (*TODO: check if t is valid*)
  let t = lookahead new_tok in
  match t with 
  |Tok_Add->
    let new_tokens = match_token(new_tok) (Tok_Add) in
    let t = lookahead new_tokens in
    (*in the case where we have parenthesis
      if (t = Tok_LParen) then
      let new_tokens = match_token (new_tokens)(Tok_LParen) in
      let (new_tok,add2) = parse_additive_expr new_tokens in
      let new_tokens = match_token (new_tok) (Tok_RParen) in
      (new_tokens, Add(add,add2))

      else
    *)
    let (new_tok,add2) = parse_additive_expr new_tokens in
    (new_tok, Add(add,add2))
  |Tok_Sub->
    let new_tokens = match_token(new_tok) (Tok_Sub) in
    let (new_tok, add2) = parse_additive_expr new_tokens in
    (new_tok, Sub(add,add2))
  |_ ->
    (new_tok,add)

(*------------------------------------------------_*)
(*MultiplicativeExpr-> Powerexpr MultiplicativeOperator MultiplicativeExpe | PowerExpr*)
and parse_multiplicative_expr tokens =
  let _ = 
    Printf.printf "the token in multiplicative is %s\n" 
      (string_of_list string_of_token tokens) in


  let (new_tokens_initial, mult) = parse_power_expr tokens in
  let _ = 
    Printf.printf "the tokens after parse_power_expr is %s\n" 
      (string_of_list string_of_token new_tokens_initial) in


  let t = lookahead new_tokens_initial in
  match t with 
  |Tok_Mult->
    let new_tokens = match_token new_tokens_initial (Tok_Mult) in 
    let (new_tok, mult_2) = parse_multiplicative_expr new_tokens in
    (new_tok, Mult(mult, mult_2))

  |Tok_Div->
    let new_tokens = match_token new_tokens_initial (Tok_Div) in
    let (new_tokens,mult_2) = parse_multiplicative_expr new_tokens in
    (new_tokens, Div(mult, mult_2)) 
  |_->
    (new_tokens_initial,mult)

(*-----------------------------------------------
 * parse_power: [unary] ^ [unary] | unary*)
and parse_power_expr tokens = 
  let _ = print_string "entered the power expression" in
  let (new_tokens, unary) = parse_unary_expr tokens in
 let _ =  Printf.printf "The current tokens in parse_power are %s\n" 
     (string_of_list string_of_token new_tokens) in 


  let t = lookahead new_tokens in
  match t with
  |Tok_Pow->
    let _ = print_string "matched with Tok_Pow\n" in
    let new_tokens = match_token new_tokens (Tok_Pow) in
    let (new_tokens,power) = parse_power_expr new_tokens in
    (new_tokens, Pow(unary,power))
  |_-> 
    (new_tokens,unary)

(*-----------------------------------------------
 * UnaryExpr-> [!] [Unary_Expr] | Primary_Expr*)
and parse_unary_expr tokens =
  let _ = print_string "Entered Unary\n" in
  let t = lookahead tokens in
  match t with 
  |Tok_Not->
    let new_tokens = match_token tokens(Tok_Not) in 
    let (new_tokens, new_unary) = parse_unary_expr new_tokens in
     (new_tokens,Not(new_unary))
  |_->
    parse_primary_expr tokens

(*-----------------------------------------------
 * Tok_Int | Tok_Bool | Tok_ID | Expr*)
and parse_primary_expr tokens= 
  let _ = print_string "Entered primary\n" in
  let t = lookahead tokens in 
  let _ = Printf.printf "the tokens after in parse_primary are  %s\n" 
      (string_of_list string_of_token tokens) in
  (*let _ = print_string "entered primary\n" in
    let _ = print_string "tokens length\n" in
    let _ = print_int (List.length(tokens)) in
  *)
  match t with 

  |Tok_Int(num) -> 
    (* let _ = print_int num in*)
    let new_tokens = match_token tokens (Tok_Int(num)) in
    (new_tokens,Int(num))
  |Tok_Bool(value) -> (tokens, Bool(value))
  |Tok_ID(str) ->
    let new_tokens = match_token tokens (Tok_ID(str)) in 
    (new_tokens, ID(str))
  |Tok_LParen->
    let _ = print_string "ENTERED THE LEFT PARENTH CASE" in
    let new_tokens = match_token tokens (Tok_LParen) in
    let (new_tokens, parsed_expr) =  parse_expr new_tokens in  
    let new_tokens = match_token new_tokens Tok_RParen  in
    (new_tokens, parsed_expr)
  
  |_->
    let _ = print_string "entered the exception case" in
    raise  (InvalidInputException (string_of_token t))


;;
(*--------------------------------------------------*)
let rec num_semi tokens = match tokens with
  |[] -> 0
  |(h :: t) ->
    if h = Tok_Semi then
      1 + num_semi t 
    else
      num_semi t
;;

let set_new_tokens tokens t = 
  match_token tokens t 
;;
(*__________________________BEGINNING OF STARTOPTIONS______________________________*)
(*StartOptions stmt | StartOption*)
let rec parse_stmt toks : stmt_result =
  let _ = print_string "ENTERED PARSE_STMT TOKS" in
    let t = lookahead toks in 
   if (t = EOF || t= Tok_RBrace) then   
    (toks, NoOp)
  else
    let (tokens, exp) = parse_stmt_options toks in 
    let (new_toks, new_exp) = parse_stmt tokens in
    (new_toks, Seq(exp,new_exp))


(*StmtOptions->Declare_Stmt | AssignStmt| PrintStmt | IfStmt | ForStmt | WhileStmt*)
and parse_stmt_options tokens =
  let _ = print_string "ENTERED PARSE_STMT_OPTIONS TOKS" in
  let t = lookahead tokens in
  match t with 
  |Tok_Int_Type->
    let _ = print_string "ENTERED THE INT TYPE\n" in
    parse_Declare_Stmt tokens
  |Tok_Bool_Type->
    parse_Declare_Stmt tokens
  |Tok_Print->
    parse_Print_Stmt tokens
  |Tok_If ->
    parse_If_Stmt tokens
  |Tok_For->
    parse_For_Stmt tokens
  |Tok_While->
    parse_While_Stmt tokens
  |Tok_ID(num)->
    parse_Assign_Stmt tokens
  |_-> 
    raise (InvalidInputException (string_of_token t)) 
(*____----------------------------------------------*)
(*DeclareStmt->BasicType ID; where BasicType is int or bool*)
and  parse_Declare_Stmt tokens = 
  let _ = print_string "ENTERED DECLARE_STMT TOKS\n" in
    let _ =  Printf.printf "The current tokens are %s\n" (string_of_list 
    string_of_token tokens) in 

  let t = lookahead tokens in
  match t with 
  |Tok_Int_Type->
    let new_tokens = match_token tokens Tok_Int_Type in

    let t = lookahead new_tokens in
    match t with
    |Tok_ID(str)->
      let new_tokens = match_token new_tokens (Tok_ID(str)) in 
      let declare = Declare(Int_Type, str) in
      let new_tokens = match_token new_tokens (Tok_Semi) in
      (new_tokens, declare)

    |_-> raise (InvalidInputException (string_of_token t)) 


    |Tok_Bool_Type->
      let new_tokens = match_token new_tokens Tok_Bool_Type in
      let t = lookahead new_tokens in
      match t with

      |Tok_ID(str)->
        let new_tokens = match_token new_tokens (Tok_ID(str)) in 
        let new_tokens = match_token new_tokens (Tok_Semi) in
        (new_tokens, Declare(Bool_Type, str))

      |_-> raise (InvalidInputException (string_of_token t))
      |_-> raise (InvalidInputException  (string_of_token t))
(*---------------------------------*)

(*AssignStmt->ID = Expr*)



and parse_Assign_Stmt tokens = 
  let _ = print_string "_____ENTERED ASSIGN_STMT_____\n" in
  let t = lookahead tokens in
  match t with 
  |Tok_ID(str)->
    let new_tokens = match_token tokens (Tok_ID(str)) in
    let _ =  Printf.printf "The current tokens are %s\n" (string_of_list string_of_token tokens) in 
    let new_tokens = match_token new_tokens Tok_Assign in   
    let _ = print_string "entering the expression\n" in
    let (new_tokens, express) = parse_expr new_tokens in
    let _ = print_string "exiting the expression\n" in
    let _ =  Printf.printf "The current tokens are 2 %s\n" 
        (string_of_list string_of_token new_tokens) in

    let assign = Assign(str, express) in
    let new_tokens = match_token new_tokens Tok_Semi in
    (new_tokens, assign)

  |_-> raise(InvalidInputException (string_of_token t))

(*----------------------------------------------------*)
(*printf (Expr) *)
and  parse_Print_Stmt tokens = 
  let _ = print_string "ENTERED PRINT_STMT\n" in 
  let _ =  Printf.printf "The current tokens are %s\n" (string_of_list string_of_token tokens) in 

  let new_tokens = match_token tokens Tok_Print in
  let new_tokens = match_token new_tokens Tok_LParen in
  let (new_tokens_express, express) = parse_expr new_tokens in
  let new_tokens = match_token new_tokens_express Tok_RParen in
  let new_tokens = match_token new_tokens Tok_Semi in
  (new_tokens, Print(express)) 


(*----------------------------------------------------*)
(*if (Expr) { Stmt} ElseBranch*)
and  parse_If_Stmt tokens=
  let _ = print_string "----------ENTERED IF_STMT--------\n-" in
  let new_tokens = match_token tokens Tok_If in
  let new_tokens = match_token new_tokens Tok_LParen in
  let _ = print_string "----------PARSING THE EXPRESSION--------\n" in
  let (new_tokens, express) = parse_expr new_tokens in
  let _ = print_string "----------DONE PARSING THE EXPRESSION-----\n" in
  let _ =  Printf.printf "The CURRENT tokens in if are %s\n" (string_of_list 
    string_of_token new_tokens) in  
  let new_tokens = match_token new_tokens Tok_RParen in    
  let new_tokens = match_token new_tokens Tok_LBrace in
  let (new_tokens, statement) = parse_stmt new_tokens in
  let new_tokens = match_token new_tokens Tok_RBrace in 
  let t = lookahead new_tokens in
 
let (new_tok, else_branch ) = parse_else_Stmt new_tokens in
(new_tok, If(express,statement, else_branch))

(*------------------------------------------------------*)
and parse_else_Stmt tokens = 
    let _ = print_string "ENTERED else_STMT" in
    let t = lookahead tokens in
    match t with 
    |Tok_Else->
      let new_tokens = match_token tokens Tok_Else in
      let new_tokens = match_token new_tokens Tok_LBrace in
      let (new_tokens, statement) = parse_stmt new_tokens in
      let new_tokens = match_token new_tokens Tok_RBrace in
      (new_tokens, statement)
    |_->
      (tokens,NoOp)

(*-------------------------------------------------------*)

(*for (ID from Expr to Expr) {Stmt}*)
and  parse_For_Stmt tokens =
     let _ = print_string "ENTERED FOR_STMT" in
     let new_tokens = match_token tokens Tok_For in
     let new_tokens = match_token new_tokens Tok_LParen in
     let t = lookahead new_tokens in
     match t with 
     |Tok_ID(str) ->

       let new_tokens = match_token new_tokens (Tok_ID(str)) in
       let new_tokens = match_token new_tokens Tok_From in
       let (new_tokens, expr) = parse_expr new_tokens in
       let new_tokens = match_token new_tokens Tok_To in
       let (new_tokens, expression_2) = parse_expr new_tokens in
       let new_tokens = match_token new_tokens Tok_RParen in
       let new_tokens = match_token new_tokens Tok_LBrace in
       let (new_tokens, statement) = parse_stmt new_tokens in
       let new_tokens = match_token new_tokens Tok_RBrace in
       (new_tokens, For(str, expr, expression_2, statement))

(*---------------------------------------------------------*)
(*while(Expr) {Stmt}*)
and  parse_While_Stmt tokens = 
     let _ = print_string "ENTERED WHILE_STMT TOKS" in
     let new_tokens = match_token tokens Tok_While in
     let new_tokens = match_token new_tokens Tok_LParen in
     let (new_tokens, expr) =  parse_expr new_tokens in
     let new_tokens = match_token new_tokens Tok_RParen in
     let new_tokens = match_token new_tokens Tok_LBrace in
     let (new_tokens, stmt) = parse_stmt new_tokens in
     let new_tokens = match_token new_tokens Tok_RBrace in
     (new_tokens, While(expr, stmt) )

(*************PARSE MAIN**************)
let parse_main toks : stmt =

  let new_tokens = match_token toks Tok_Int_Type in
  let new_tokens = match_token new_tokens Tok_Main in
  let new_tokens = match_token new_tokens Tok_LParen in
  let new_tokens = match_token new_tokens Tok_RParen in
  let new_tokens = match_token new_tokens Tok_LBrace in

  let (new_tokens, statement) = parse_stmt new_tokens in
  let _ =  Printf.printf "token list at the end %s\n"( string_of_list string_of_token
                                                         new_tokens) in
  let new_tokens = match_token new_tokens Tok_RBrace in
  let new_tokens = match_token new_tokens EOF in
  statement


;;

