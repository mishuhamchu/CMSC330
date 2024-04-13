open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)
(*s is path, q is state*)
let rec find_trans (delta : ('q,'s) transition list) (path : 's option)(q_element: 'q )
= match delta with
 |[] -> 
(*let _  = Printf.printf("ENTERED the empty case") in *)
 []
 |((q_ele,path_check,q_end) :: t) ->
    if q_ele = q_element then
  (* let _ =  Printf.printf("THEY ARE EQUAL") in*)
	if path = path_check then
	    q_end ::  find_trans t path q_element
	    else
	find_trans t path q_element  
	 
	else
	find_trans t path q_element

;;

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
List.fold_left (fun x y -> if find_trans nfa.delta s y <> [] then x @ (find_trans nfa.delta s y) else
x) [] qs;;


let rec get_elements lst num = match lst with
|[] -> [] 
|(h :: t) when num = 0->
 []
|(h :: t) ->
 h :: get_elements lst (num-1)
;;


let rec closure_fixed (nfa: ('q,'s) nfa_t) r : ('q list) r2 : ('q list) =
 let last_element = match List.reverse(r) with 
 |[] -> []
 |(h :: t) -> 
 let new_r2 = r @ (move nfa [h] None) in

 if new_r2 = r then
 new_r2
 else
 closure_fixed nfa r r2
 

;;

let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (new_lst : 'q list) : 'q list = match qs with
|[] -> new_lst
|(h :: t) ->
    new_lst @ (closure_fixed nfa [h] [h]) ;;
;;

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = 
e_closure_helper nfa qs []
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =





;;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  failwith "unimplemented"

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
