open List
open Nfa

(*********)
(* Types *)
(*********)

type regexp_t =
  | Empty_String
  | Char of char
  | Union of regexp_t * regexp_t
  | Concat of regexp_t * regexp_t
  | Star of regexp_t

(***********)
(* Utility *)
(***********)

let fresh =
  let cntr = ref 0 in
  fun () ->
    cntr := !cntr + 1 ;
    !cntr




(*******************************)
(* Part 3: Regular Expressions *)
(*******************************)
let rec connect_final delta finals new_state = match finals with 
|[] -> delta
|( h :: t) ->
connect_final ((h, None, new_state) :: delta) t new_state
;;

let rec regexp_to_nfa (regexp: regexp_t) : (int, char) nfa_t = match regexp with 
|Empty_String -> 
let new_num = fresh () in
let new_num2 = fresh() in
{ sigma = []; qs = [new_num; new_num2]; q0 = new_num; fs = [new_num2]; 
delta = [(new_num, None, new_num2)] };

|Char(character) ->
let new_state = fresh ()in
let second_new_state = fresh () in
let new_nf = { sigma = ([character]); qs = [ second_new_state; new_state]; 
q0 = new_state; fs = [second_new_state]; delta = [
(new_state, Some (character), second_new_state)];
} in
new_nf
|Star(regex_1) ->
let new_nfa = regexp_to_nfa regex_1  in
let new_q0 = fresh () in
let new_fs = fresh () in
let new_nf = {sigma = new_nfa.sigma; qs = new_q0 :: new_fs ::  new_nfa.qs; q0 = new_q0; 
fs = [new_fs]; 
delta = (new_q0, None, new_nfa.q0) :: (new_q0, None, new_fs) :: (new_fs, None, new_q0) ::  
(connect_final new_nfa.delta new_nfa.fs new_fs) } in
new_nf
|Concat(regex_1, regex_2) ->
let new_nfa_1 = regexp_to_nfa regex_1  in
let new_nfa_2 = regexp_to_nfa regex_2  in
let new_delta = new_nfa_1.delta@new_nfa_2.delta in
let new_nf = {sigma = new_nfa_1.sigma@new_nfa_2.sigma; qs = new_nfa_1.qs@new_nfa_2.qs;
q0 = new_nfa_1.q0; fs = new_nfa_2.fs; delta = (connect_final new_delta new_nfa_1.fs new_nfa_2.q0) } in

new_nf

|Union(regex_1, regex_2) ->
let new_nfa_1 = regexp_to_nfa regex_1  in
let new_nfa_2 = regexp_to_nfa regex_2  in
let new_start = fresh () in
let new_end = fresh () in
let new_delta = new_nfa_1.delta@new_nfa_2.delta in
let new_nf = {sigma = new_nfa_1.sigma@new_nfa_2.sigma; qs = new_nfa_1.qs@new_nfa_2.qs
; q0 = new_start; fs = [new_end]; delta =
((new_start, None, new_nfa_1.q0) :: (new_start, None, new_nfa_2.q0) :: (connect_final new_delta 
(new_nfa_1.fs@new_nfa_2.fs) new_end))
} in
new_nf



;;


(*****************************************************************)
(* Below this point is parser code that YOU DO NOT NEED TO TOUCH *)
(*****************************************************************)

exception IllegalExpression of string

(* Scanner *)
type token =
  | Tok_Char of char
  | Tok_Epsilon
  | Tok_Union
  | Tok_Star
  | Tok_LParen
  | Tok_RParen
  | Tok_END

let tokenize str =
  let re_var = Str.regexp "[a-z]" in
  let re_epsilon = Str.regexp "E" in
  let re_union = Str.regexp "|" in
  let re_star = Str.regexp "*" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let rec tok pos s =
    if pos >= String.length s then [Tok_END]
    else if Str.string_match re_var s pos then
      let token = Str.matched_string s in
      Tok_Char token.[0] :: tok (pos + 1) s
    else if Str.string_match re_epsilon s pos then
      Tok_Epsilon :: tok (pos + 1) s
    else if Str.string_match re_union s pos then Tok_Union :: tok (pos + 1) s
    else if Str.string_match re_star s pos then Tok_Star :: tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else raise (IllegalExpression ("tokenize: " ^ s))
  in
  tok 0 str

let tok_to_str t =
  match t with
  | Tok_Char v -> Char.escaped v
  | Tok_Epsilon -> "E"
  | Tok_Union -> "|"
  | Tok_Star -> "*"
  | Tok_LParen -> "("
  | Tok_RParen -> ")"
  | Tok_END -> "END"

(*
   S -> A Tok_Union S | A
   A -> B A | B
   B -> C Tok_Star | C
   C -> Tok_Char | Tok_Epsilon | Tok_LParen S Tok_RParen

   FIRST(S) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(A) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(B) = Tok_Char | Tok_Epsilon | Tok_LParen
   FIRST(C) = Tok_Char | Tok_Epsilon | Tok_LParen
 *)

let parse_regexp (l : token list) =
  let lookahead tok_list =
    match tok_list with
    | [] -> raise (IllegalExpression "lookahead")
    | h :: t -> (h, t)
  in
  let rec parse_S l =
    let a1, l1 = parse_A l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Union ->
        let a2, l2 = parse_S n in
        (Union (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_A l =
    let a1, l1 = parse_B l in
    let t, n = lookahead l1 in
    match t with
    | Tok_Char c ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_Epsilon ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | Tok_LParen ->
        let a2, l2 = parse_A l1 in
        (Concat (a1, a2), l2)
    | _ -> (a1, l1)
  and parse_B l =
    let a1, l1 = parse_C l in
    let t, n = lookahead l1 in
    match t with Tok_Star -> (Star a1, n) | _ -> (a1, l1)
  and parse_C l =
    let t, n = lookahead l in
    match t with
    | Tok_Char c -> (Char c, n)
    | Tok_Epsilon -> (Empty_String, n)
    | Tok_LParen ->
        let a1, l1 = parse_S n in
        let t2, n2 = lookahead l1 in
        if t2 = Tok_RParen then (a1, n2)
        else raise (IllegalExpression "parse_C 1")
    | _ -> raise (IllegalExpression "parse_C 2")
  in
  let rxp, toks = parse_S l in
  match toks with
  | [Tok_END] -> rxp
  | _ -> raise (IllegalExpression "parse didn't consume all tokens")


let string_to_regexp str = parse_regexp @@ tokenize str

let string_to_nfa str = regexp_to_nfa @@ string_to_regexp str
