open Tree;;
open Axioms;;
open Mp;;
open Assumption;;

type checked_expression_type = 
	| Assumption of int
	| Axiom of int
	| ModusPonens of int * int
	| NotProofed

let checked_expression expression ind = 
	match (is_assumption expression) with
		| Some (value) -> Assumption (value)
		| _ -> 
		match (check_with_axioms expression) with
			| Some (value)-> Axiom (value)
			| _ -> 
			match (check_mp expression) with
				| Some (i, j) -> ModusPonens (i, j)
				| _ -> NotProofed

let check_expression_tree expression ind = begin 
	let checked_expression = checked_expression expression ind in
		update_mp_table expression ind;
		update_new_mp_table expression ind;
		Hashtbl.replace !exp1 expression ind;
		checked_expression
end;;