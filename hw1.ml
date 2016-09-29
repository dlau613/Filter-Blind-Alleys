type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
(* type ('a, 'b) symbol = N of 'a | T of 'b *)

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


(* returns true if symbol is terminable
symbol is terminable if it is of type T or if it is in list of terminable_symbols *)
let symbol_terminable symbol terminable_symbols = 
	match symbol with 
	T sym -> true
	| N sym -> (contains symbol terminable_symbols)



(* return true if the rhs consists of terminable symbols
rhs is a list of of symbols *)
let rec rhs_terminable rhs terminable_symbols = 
	match rhs with 
	[] -> true
	| h::t when symbol_terminable h terminable_symbols -> rhs_terminable t terminable_symbols
	| _ -> false


(* if rule is terminable (based on current terminable_symbols list)
then add symbol to terminable_symbols
returns list of terminable symbols *)
let rec check_rules rules terminable_symbols = 
	match rules with
	(s,rhs)::t -> if (rhs_terminable rhs terminable_symbols)
						then ((check_rules t ((N s)::terminable_symbols)) )
						else ((check_rules t terminable_symbols) )
	| _ -> (terminable_symbols)

(* made a wrapper that will return a pair of the rules and the terminable symbols
this way the input is the same form as the output and can apply recursively f (f x)
which lets this function be used in computed_fixed_point *)
let check_rules_wrapper (rules, terminable_symbols) = 
	(rules, check_rules rules terminable_symbols)

(* this is a predicate for comparing x and f x where f is check_rules_wrapper and x is (rules,terminable_symbols) 
which is why the two inputs are pairs of rules and terminable symbols*)
let equal_terminable_symbols (rules1, terminable_symbols1) (rules2,terminable_symbols2) =
	if equal_sets terminable_symbols1 terminable_symbols2
		then true
	else false

(* repeatedly check the rules for more terminable symbols until no new ones are added
this condition is determined used computed_fixed_point where x = (rules,[]) 
and f = check_rules_wrapper which will add new terminable symbols 
Returns (rules, terminable_symbols) *)
let repeat_check_rules rules terminable_symbols = 
	computed_fixed_point (equal_terminable_symbols) (check_rules_wrapper) (rules,terminable_symbols) 

(* returns rules with blind alleys removed
check if rhs is terminable *)
let rec remove_rules (rules, terminable_symbols) = 
	match rules with 
	h::t -> if (not (rhs_terminable (snd h) terminable_symbols) )
				then (remove_rules (t, terminable_symbols))
			else h::(remove_rules (t, terminable_symbols))
	| _ -> []
let filter_blind_alleys g = 
	(* check if each symbols rhs is terminable. if it is, add the symbol to a list of terminable symbols
	keep doing this until no new terminable symbols. this is done by repeat_check_rules, which will return 
	the original list of rules with a list of all the terminable symbols.
	remove rules goes through all the rules and checks if the rhs is terminable and removes those that are not
	then go through symbols and if not in terminable symbols then remove from list *)
	match g with 
	(start, rules) -> (start, (remove_rules (repeat_check_rules rules []) ) )


 

