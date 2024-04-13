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

let count_occ lst target =
List.fold_left (fun x y -> if (y = target) then 1 + x else x) 0 lst
;;

let uniq lst = 
(*takes in an accumulator and a y*)
let f acc y = if (count_occ acc y == 0) then y :: acc else
acc in
(*calls fold with f function, accumulator, and lst*)

List.fold_left f [] lst
;;


(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

let rec contains element lst = match lst with 
|[] -> false
|(h :: t ) -> if h = element then 
true
else
contains element t
;;
(****************)
(* Part 1: NFAs *)
(****************)
(*s is path, q is state*)
let rec find_trans (delta : ('q,'s) transition list) (trans : 's option)(q_element: 'q )
= match delta with
 |[] -> 
(*let _  = Printf.printf("ENTERED the empty case") in *)
 []
 |((q_ele,trans_check,q_end) :: t) ->
    if q_ele = q_element then
  (* let _ =  Printf.printf("THEY ARE EQUAL") in*)
	if trans = trans_check then
	    q_end ::  find_trans t trans q_element
	    else
	find_trans t trans q_element  
	 
	else
	find_trans t trans q_element

;;
let rec option_lst lst = match lst with
|[] -> []
|(h :: t) -> Some(h) :: option_lst t
;;
(*TODO: check if s is in the list; this includes the none option*)
let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
(*checking if it is empty*)
let sigma_lst = option_lst nfa.sigma in

if s = None || contains  s sigma_lst then
uniq (List.fold_left (fun x y -> if find_trans nfa.delta s y <> [] then x @ (find_trans nfa.delta s y) else
x) [] qs)
else
[]
;;


let rec get_elements lst num = match lst with
|[] -> [] 
|(h :: t) when num = 0->
 []
|(h :: t) ->
 h :: get_elements lst (num-1)
;;


let rec add_lst new_items seen new_lst = match new_items with 
|[] -> (seen,new_lst)
|(h :: t) -> if (contains h seen) = false then
 (*let _ = print_string "have not seen this element" in*)
 add_lst t (h :: seen) (h :: new_lst)
else
add_lst t seen new_lst
;;

let rec combined_lst lst new_lst = match lst with 
[] -> new_lst
|(h :: t) -> 
combined_lst t ( new_lst @ h)
;;



(*TODO: check if the element I am adding is in the seen set*)
let rec closure_fixed (nfa: ('q,'s) nfa_t) r2  seen  = match r2 with 
 |[] -> []
 |(h :: t) -> 
(* let _ = print_string "In the closure fixed" in *)
    
    (*moves all the items in the last lst None*)
    let new_items = (move nfa h None) in
    let (new_seen, added_elements) = add_lst new_items seen [] in
   (*
    let _ = print_string "length of seen" in
    let _ = print_int (List.length(seen)) in
    let _ = print_string "length of added_elements" in
    let _ = print_int (List.length(added_elements)) in
   *) 
    if List.length(added_elements) <> 0 then 
    let new_r2 = (added_elements :: r2 ) in
    closure_fixed nfa new_r2 new_seen
    else
    r2  
;;

let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (new_lst : 'q list)  = match qs with
|[] -> new_lst
|(h :: t) when List.length(new_lst) = 0 ->
let check_lst =  new_lst @ (combined_lst(closure_fixed nfa [[h]] [h]) ([])) in
e_closure_helper nfa t check_lst
|(h :: t) ->
 let check_lst = new_lst @ (combined_lst(closure_fixed nfa [[h]] new_lst) ([])) in 
    e_closure_helper nfa t check_lst

;;

let rec e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = 
let lst = e_closure_helper nfa qs [] in
uniq lst
;;



(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

(*computes all DFA states you can get to from transition out of qs*)
let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
List.fold_right (fun sigma acc -> 
let new_state = move nfa qs (Some(sigma)) in
let closed = (e_closure nfa new_state) in
(closed :: acc)
) nfa.sigma [] 
;;

(*gives a list of transitions for a single element in the DFA*)
let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
 let accum = List.fold_left (fun acc sigma ->  (qs, Some sigma, move nfa qs (Some sigma)) :: acc)
 ([]) nfa.sigma in
List.rev(    let rec add_closure nfa lst = match lst with
    |[] -> []
    |( (start,trans,finish) :: t) -> 
    (start, trans, (e_closure nfa finish))  :: (add_closure nfa t)
    in (add_closure nfa accum)
)
;;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let accum = List.fold_left 
  (fun acc final -> if acc = false && contains final nfa.fs then true else acc) false qs in 
    if accum = true then
    [qs]
    else
    []
;;

let rec each_new_item (nfa : ('q,'s) nfa_t) (dfa : ('q list, 's) nfa_t) (subsets : 'q list list)
(work : 'q list list) = match subsets with 
|[] ->
work
|( h :: t) -> 
if ((contains h dfa.qs) = false) && ((contains h work) = false) then
   each_new_item nfa dfa t (h :: work)
       else
    each_new_item nfa dfa  t work
;;

(*keep doing until unmarked states empty*)

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
match work with 
(* if the working set is empty, then we are done, because the working set keeps track of the 
current items that we have not yet checked*)
 |[]  -> dfa
 |( h :: t) ->
    (*the head of the work list will be the subset we are looking at*)
   let subset = new_states nfa (h) in
   (*new_work will have to reverse*)
  let  new_work = List.rev((each_new_item nfa dfa subset work)) in 
    (*THIS WORKS BECAUSE NEW_WORK WILL NEVER BE EMPTY*)
   let new_new_work = Sets.remove  h new_work in

  let  new_final = new_finals nfa h in
  let new_tran = new_trans nfa h in
  let new_fs = dfa.fs @ (new_finals nfa h) in
  let new_dfa = 
  {sigma = dfa.sigma; qs = if (contains h dfa.qs = false ) then
  h :: dfa.qs else dfa.qs; q0 = dfa.q0; fs = new_fs; delta = dfa.delta@new_tran} in 
  nfa_to_dfa_step nfa new_dfa new_new_work

;;

(*
qs is a set of sets;
q0 is a single set;
fs is a set of sets;
delta is a list of transitions from one set to another set
*)

let rec single_list (lst :'a list list) (single_lst: 'a list) = match lst with
|[] -> single_lst
|(h :: t) ->
 single_list t (single_lst@h)
;;

let rec check_dead_states (nfa_list: 'q list) (dfa_list : 'q list) 
(new_dfa_list:'q list list) = match nfa_list with 
|[] -> new_dfa_list
|(h :: t) ->  
if  (contains h dfa_list ) = false then
check_dead_states t (dfa_list) ([h] :: new_dfa_list)
else
check_dead_states t (dfa_list) (new_dfa_list)
;;


let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
(*
get the e-closure; it will be the initial
new_delta will be the new_transformations from the beginning
dfa_default will be our default dfa structure
unmarked states will initially be set to nfa.qs
*)
  let initial = e_closure nfa [nfa.q0] in 
    let dfa_default = { sigma = nfa.sigma; qs = [initial]; q0 = initial; fs = []; 
    delta =[]} in
    let unmarked_states = dfa_default.qs in 
    (*call the helper function*)
    let new_dfa = nfa_to_dfa_step nfa dfa_default unmarked_states in
    let new_qs = Sets.remove [] new_dfa.qs  in
    let second_new_dfa ={sigma = new_dfa.sigma; qs = new_qs; q0 = new_dfa.q0; fs = new_dfa.fs; 
    delta = new_dfa.delta}
    in
    (*all dfa in a single list*)
    let dfa_list = single_list second_new_dfa.qs [] in

    let new_qs_check = check_dead_states nfa.qs dfa_list second_new_dfa.qs in
 {sigma = new_dfa.sigma; qs = new_qs_check; q0 = new_dfa.q0; fs = new_dfa.fs;
    delta = new_dfa.delta}
  ;;

    

(* returns the terminal state*)
let rec new_state state  lst s = match lst with 
|[] -> []
(*finds the transition with this start state, and this transition*)
|( (start, trans, terminal) :: t) ->
if start = state && trans = s then
terminal
else
new_state state t s
;;

let rec accept_helper dfa state lst
= match lst with
(*if the dfa.fs contains the state, then we are fine*)
|[] -> if contains state dfa.fs then
true
else
false
|( h :: t) ->   
let next_element = new_state (state) (dfa.delta) (Some h) in
accept_helper dfa next_element t
;;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
let string_lst = (explode s) in
let dfa = nfa_to_dfa nfa in
accept_helper dfa (dfa.q0) string_lst 
;;



