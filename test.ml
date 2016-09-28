(* let quadruple x = 
  let double y = y + y in
  let ans = double x + double x in
  ans *)

(* sum returns the sum of elements in l*)
 let rec sum l =
 	match l with
 	[] -> 0
 	| h::t -> h + (sum t)

 (* string list ["ucla ";"is ";"awesome"] *)
 (* "ucla is awesome" *)
 (* ^ -- for concat *)
let rec concat l = 
	match l with
	[] -> ""
	| h::t -> h ^ (concat t)

(* clone 9 5 -> [9;9;9;9;9] *)
(* int int -> int list *)
let rec clone z n = 
	match n with
	0 -> []
	| _ -> z::(clone z (n-1) )

(* every_second [1;2;3;4;5] -> [2;4] *)
let rec every_second l = 
	match l with
	[] | [_]-> []
	| h1::h2::t -> h2::(every_second t)

let rec every_second2 l =
	match l with
	h1::h2::t -> h2::(every_second t)
	| _ -> []

(* contains 1 [1;2;3] --> true *)
let rec contains e l = 
	match l with
	[] -> false
	| h::t when h=e -> true
	| h::t -> (contains e t) 

(* delete all occurences of e *)
let rec delete_element e l =
	match l with 
	[] -> []
	| h::t when h=e -> (delete_element e t)
	| h::t ->h::(delete_element e t)

(* check every element in l1 appears in l2 *)
let rec contains_list l1 l2 = 
	match l1 with
	[] -> true
	| h::t -> (contains h l2) && (contains_list t l2)


