open Tree;;

let (>>) x f = f x;;

let parse_assumptions_from_header_to_tree_list header = header >> Lexing.from_string >> Parser.header Lexer.read;;

let get_assumptions_from_header header = 
	let assumptions_hash_table = Tree_table.create 1000 in
	let (assumptions_tree_list, expression)  = parse_assumptions_from_header_to_tree_list (header) in
	List.iteri (fun i tree -> Tree_table.add assumptions_hash_table tree (i + 1)) (List.tl (List.rev (assumptions_tree_list)));
	(assumptions_hash_table, List.hd (List.rev (assumptions_tree_list)), expression, List.rev (List.tl (List.rev (assumptions_tree_list))))
;;

let is_assumption tree tree_table = 
	if (Tree_table.mem tree_table tree) then 
		(Some (Tree_table.find tree_table tree))
	else (None)