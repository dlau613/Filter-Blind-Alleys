symbol
	A symbol used in a grammar. It can be either a nonterminal symbol or a terminal symbol; each kind of symbol has a value, whose type is arbitrary. A symbol has the following OCaml type:
	type ('nonterminal, 'terminal) symbol =
	  | N of 'nonterminal
	  | T of 'terminal

right hand side
	A list of symbols. It corresponds to the right hand side of a single grammar rule. A right hand side can be empty.

rule
	A pair, consisting of (1) a nonterminal value (the left hand side of the grammar rule) and (2) a right hand side.

grammar
	A pair, consisting of a start symbol and a list of rules. The start symbol is a nonterminal value

Some grammars contain blind-alley rules, that is, grammar rules for which it is impossible to derive a string of terminal symbols. 

filter_blind_alleys g that returns a copy of the grammar g with all blind-alley rules removed. This function should preserve the order of rules: that is, all rules that are returned should be in the same order as the rules in g.