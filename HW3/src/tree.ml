type operation = Conj | Disj | Impl;;

type tree_t = 
	| Binop of operation * tree_t * tree_t
    | Neg of tree_t
    | Var of string

module Tree_table = Hashtbl.Make(
struct
	type t = tree_t
	let equal = (=)
	let hash = Hashtbl.hash
end)

let string_of_operation operation = match operation with
	| Conj -> "&"
	| Disj -> "|"
	| Impl -> "->"

let rec string_of_tree = function
	| Var v -> v
	| Neg t -> "!" ^ string_of_tree t
	| Binop (operation, left, right) ->
		"(" ^ 
		string_of_tree left ^ 
		string_of_operation operation ^ 
		string_of_tree right ^ 
		")"

let rec string_of_tree_list = function
	| [] -> ""
	| tree :: tree_list ->
		if (tree_list <> []) then
			string_of_tree (tree) ^ ", " ^ string_of_tree_list (tree_list)
		else
			string_of_tree(tree)