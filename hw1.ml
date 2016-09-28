type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


(* contains 1 [1;2;3] --> true *)
let rec contains e l = 
	match l with
	[] -> false
	| h::t when h=e -> true
	| h::t -> (contains e t) 

(* 1 *)
(* true if a is a subset of b. every subset is a subset of itself. 
takes lists of any type *)
let rec subset a b =
	match a with 
	[] -> true
	| h::t -> (contains h b) && (subset t b)

(* 2 *)
(* true if the represented sets are equal *)
let equal_sets a b =
	(subset a b) && (subset b a)


(* 3 *)
let set_union a b =
	a @ b

(* 4 *)
(* compare each element in a to every element in b. if equal
add to intersection *)
let rec set_intersection a b = 
	match a with
	h::t when (contains h b) ->  ( h::(set_intersection t b) )
	| _ -> []

(* 5 *)
(* a-b *)
let rec set_diff a b = 
	match a with
	h::t when (not (contains h b) )  -> h::(set_diff t b) 
	| _ -> []


(* 6 *)
let rec computed_fixed_point eq f x = 
	(* if f x = x then *)
	if (eq (f x) x) then
		x
	else
		computed_fixed_point eq f (f x)

(* 7 *)
let rec compute f p x =
	match p with
	0 -> x
	| _ -> f (compute f (p-1) x)

let rec computed_periodic_point eq f p x = 
	if (eq x (compute f p x) ) then 
		x
	else
		computed_periodic_point eq f p (f x)

(* 8 *)
let rec while_away s p x =
	if p x then
		x::(while_away s p (s x))
	else 
		[]	

(* 9 *)
let rec expand (n, e) = 
	match n with 
	0 -> []
	| _ -> e::(expand (( n-1), e) ) 

let rec rle_decode lp = 
	match lp with
	h::t -> (expand h)@(rle_decode t)
	| _ -> []

let filter_blind_alleys g = 
	match g with 
	(start, rules) -> 
	(* go through each list an see if each element is one of the nonterminals *)



