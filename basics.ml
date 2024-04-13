(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup tup = 
let (a,b,c) = tup in
 (c,b,a)
;;



let abs x = 
if x < 0 then
x * -1
else
x
;;

let area x y = 
 let (c1,c2) = x in
 let (d1, d2) = y in
let e1 = c1 - d1 in
let e2 = c2 - d2 in
let e3 = e1 * e2 in
abs e3
;;

let volume x y = 
let (c1,c2,c3) = x in
let (d1,d2,d3) = y in
let e1 = d1 - c1 in
let e2 = d2 - c2 in
let e3 = d3 - c3 in
let e4 = e1 * e2 * e3 in
abs e4

;;
let equiv_frac (a, b) (x, y) = 
if b = 0 || y = 0 then
false

else
let z1 = float a  /. float b in
let z2 = float x /. float y in
if (z1 = z2) then
true

else
false

;;
(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec factorial x = 
if x = 0 then
1
else
let y = x - 1 in
x * factorial y
;;
let rec pow x y = 
if y = 0  then
 1
 else
 let z = x in
 let new_y = y - 1 in
  z * pow x new_y
 ;;


let rec len x =
let check = x/10 in
if (check = 0) then
1
else
1 + len check
;;


let rec tail_helper x num i = 
if i = num then
0
else
let new_x = x/10 in
let digit = x mod 10 in
let powered = pow 10 i in
let new_i = i + 1 in
digit * powered + tail_helper new_x num new_i
;;

let rec tail x num = 
(*if the length of x is less than num, than we return x*)
if (len x) < num then
x
else 
let i = 0 in
tail_helper x num i
;;

let rec sub_checker x sub len i = 
let digit = x mod 10 in 
let digit2 = sub mod 10 in
if i = len then
true
else if digit = digit2 then
let new_x = x / 10 in
let new_sub = sub / 10 in
let new_i = i + 1 in
sub_checker new_x new_sub len new_i
else 
false
;;


let rec contains_helper sub x i length_x = 
let digit = x mod 10 in
let digit2 = sub mod 10 in
let new_x = x / 10 in
let new_i = i + 1 in

if i = length_x then
false

else if digit = digit2 then
let length = len sub in
let check = sub_checker x sub length 0 in
if check then
true
else
contains_helper sub new_x new_i length_x 
else 
contains_helper sub new_x new_i length_x
;;

let rec contains sub x =
contains_helper sub x 0 (len x)
;;
(*****************)
(* Part 3: Lists *)
(*****************)

let rec get idx lst = match lst with
| [] -> failwith "Out of bounds"
| h :: t ->
    if idx = 0 then
    h
    else
    let new_idx = idx - 1 in
    get new_idx t 
;;

let rec reverse_helper lst lst2 = match lst with
| [] -> lst2
| h :: t ->
let lst3 = h :: lst2 in
    reverse_helper t lst3
;;
let rec reverse lst =  
reverse_helper lst []  
;;


let rec combine lst1 lst2 =
match  lst1  with

| [] -> lst2
| h :: t ->
    h :: combine t lst2
;;

let rec rotate shift lst = 
match lst with
|[] -> []
| h :: t ->
if shift = 0 then
lst
else
rotate (shift-1) (combine t [h])
;;
