open Funs

(***********************)
(* Part 2: Integer BST *)
(***********************)



type int_tree =
  | IntLeaf
  | IntNode of int * int_tree * int_tree

let empty_int_tree = IntLeaf

let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)

let rec int_mem x t =
  match t with
  | IntLeaf -> false
  | IntNode (y, l, r) when x > y -> int_mem x r
  | IntNode (y, l, r) when x = y -> true
  | IntNode (y, l, r) -> int_mem x l

(* Implement the functions below. *)

let rec int_size t = match t with
| IntLeaf -> 0
| IntNode (y,l,r)-> 1 + (int_size l) + (int_size r)

;;

let rec int_max_helper t max = match t with 
|IntLeaf -> 
max
|IntNode (y,l,r) -> 
 int_max_helper r y

;;
let rec int_max t = match t with
(*need to put quotes here*)
|IntLeaf -> invalid_arg "int_max"
|IntNode (y,l,r) -> 
    int_max_helper r y
;;

let rec contains t find = match t with
|IntLeaf -> false
|IntNode(y,l,r) ->
if find = y then
 true
 else

 let left_find = (contains l find) in
 if (left_find = false) then
    (contains r find) 
    else
	left_find
 ;;

let rec int_common_helper t x y = match t with
|IntLeaf -> invalid_arg("int_common")
|IntNode(v,l,r) ->
(*let _ = Printf.printf "entered" in*)

if x = y then 
x
else

let left_contains_y = contains l y in
let right_contains_y = contains r y in
let left_contains_x = contains  l x in
let right_contains_x = contains r x in

if (v = x ) && (left_contains_y || right_contains_y) then
v
else if (v = y ) && (left_contains_x || right_contains_x) then
v
else if (left_contains_x && left_contains_y) then
(*let _ = Printf.printf "going left " in*)

int_common_helper l x y 
else if (right_contains_x && right_contains_y) then
int_common_helper r x y 

else
(*let _ = Printf.printf "the end: printing last value" in *)
v

;;

let rec int_common t x y = match t with
|IntLeaf -> invalid_arg("int_common")
|IntNode(v,l,r) ->
if (contains t x) && (contains t y) then
    int_common_helper t x y
    
    else
    invalid_arg("int_common")

;;
(***************************)
(* Part 3: Polymorphic BST *)
(***************************)

type 'a atree =
    Leaf
  | Node of 'a * 'a atree * 'a atree
type 'a compfn = 'a -> 'a -> int
type 'a ptree = 'a compfn * 'a atree

let empty_ptree f : 'a ptree = (f,Leaf)


let rec int_insert x t =
  match t with
  | IntLeaf -> IntNode(x, IntLeaf, IntLeaf)
  | IntNode (y, l, r) when x > y -> IntNode (y, l, int_insert x r)
  | IntNode (y, l, r) when x = y -> t
  | IntNode (y, l, r) -> IntNode (y, int_insert x l, r)


let rec pinsert x t = match t with 
 | (compfn,Leaf) -> (compfn,Node(x, Leaf, Leaf))
  (*insert into the right when x > y*)
  | (compfn,Node (y, l, r)) when ( (compfn x y) > 0) ->  
    let (compfn, new_tree_right) = pinsert x (compfn, r) in
    (compfn, Node(y,l, new_tree_right))
  | (compfn, Node (y, l, r)) when ((compfn x y) = 0 )-> t
  | (compfn,Node (y, l, r)) when ((compfn x y) < 0) -> 
    
    let (compfn, new_tree_left) = pinsert x (compfn,l) in
    (compfn, Node(y,new_tree_left, r))

;;



let rec pmem x t = match t with 
|(compfn, Leaf) -> false
|(compfn, Node(y,l,r)) -> 
if (compfn x y = 0) then true 
else if (compfn x y < 0) then 
let new_tree = (compfn, l) in 
pmem x new_tree
else
pmem x (compfn, r)
;;

let pinsert_all lst t =
let acc = t in
fold_right pinsert lst acc  
;;

let rec p_as_list t = match t with
|(compfn, Leaf) -> []
|(compfn, Node(y,l,r)) -> 
     (p_as_list (compfn, l)) @ [y] @ (p_as_list (compfn, r)) 
;;
let pmap f t = 
let p_list = p_as_list t in
let mapped = map f p_list in
let (compfn, tree) = t in
let empty_tree = (empty_ptree compfn) in
pinsert_all mapped  (empty_tree)


;;

(***************************)
(* Part 4: Variable Lookup *)
(***************************)


type lookup_table = 
Empty
|Scope of lookup_table  * ( (string * int) list) 

(*TODO: what is empty_table ()?*)
let empty_table () : lookup_table = 
Empty
;;

let push_scope (table: lookup_table) : lookup_table = match table with 
|Empty ->
Scope(Empty, [])
|Scope(parent, lst) -> 
(*let _ = print_string "entered" in *)
Scope(table, lst)
;;

let rec pop_scope (table: lookup_table) : lookup_table = match table with 
|Empty ->
failwith ("No scopes remain!")
|Scope(parent, lst) ->
parent
;;


let rec print_lst lst = match lst with
|[] -> 
let _ = print_string " " in
[]
|((name,value) :: t) ->
    let _ = print_string "(" in
    let _ = print_string name in
    let _ = print_int value in
    let _ = print_string ")" in
    print_lst t
;;

let rec add_item parent_lst lst name new_value = match parent_lst with
|[] -> (name,new_value) :: lst
|(curr_name,value) :: t  when curr_name <> name->
 add_item t ((curr_name, value) :: lst) name new_value
|(curr_name, value) :: t -> add_item t lst name new_value



(*updates the most recent scope to include a new variable name*)
let  add_var name value (table : lookup_table) : lookup_table = match table with
|Empty -> 
 Scope(Empty, [(name, value)])

|Scope(parent, var) ->
(*let _ = print_string "entered the new domain" in*)


let new_list = add_item var [] name value in

(*let _ = print_string "printing the new list" in *)

(*let _ = print_string "done printing the new list" in *)
Scope( parent ,new_list)

;;

let rec check_val current_lst name  = match current_lst with
|[] -> failwith("Variable not found!")
|((named, value) :: t) when named = name ->
    value
|(h :: t) -> 
    check_val t name

;;

let  lookup name (table: lookup_table) = match table with
|Empty -> 
(*let _ = print_string "empty case entered" in *)
failwith("Variable not found!")
|Scope(parent, lst) ->
    check_val lst name
;;
