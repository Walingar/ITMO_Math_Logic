open Assumption;;
open Tree;;
open Axiom;;

let (>>) x f = f x;;

let expressions	= Tree_table.create 1000;;
let mp		= Tree_table.create 1000;;
let proof	= Tree_table.create 1000;;
let axioms = List.map (fun line -> line >> Lexing.from_string >> Parser.body Lexer.read)
            ["A-> B->A";
             "(A->B) -> (A->B->C) -> (A->C)";
             "A -> B -> A&B";
             "A&B -> A";  
             "A&B -> B";
             "A -> A|B";
             "B -> A|B";
             "(A->C) -> (B->C) -> (A|B->C)";
             "(A->B) -> (A->!B) -> !A";
             "!!A -> A"
             ]


let checker_a_a = ref (Hashtbl.create 1000);;

let rec check_with_axiom axiom expression_tree = 
	match (axiom, expression_tree) with
	| (Binop (operation_l, l1, r1), Binop (operation_r, l2, r2)) when (operation_l = operation_r) -> (check_with_axiom l1 l2) && (check_with_axiom r1 r2)
	| (Neg a, Neg b) -> (check_with_axiom a b)
	| (Var a, b) ->
		if (Hashtbl.mem !checker_a_a a) then
			(Hashtbl.find !checker_a_a a) = b
		else begin
			Hashtbl.add !checker_a_a a b;
			true
		end
	| (_, _) -> false


type checked_expression_type = 
	| Assumption of int
	| Axiom of int
	| ModusPonens of int * int
	| NotProofed


let string_of_checked_expression checked_expression = match checked_expression with
	| Assumption (value)  -> "Предп. " ^ (string_of_int value)
	| Axiom (value)       -> "Сх. акс. " ^ (string_of_int value)
	| ModusPonens (i, j) -> "M.P. " ^ (string_of_int i) ^ ", " ^ (string_of_int j)
	| NotProofed         -> "Не доказано"



let check_mp expr = 
	if (Tree_table.mem proof expr) then 
		(Some (Tree_table.find proof expr))
	else 
		None;;



let update_mp_table expression_tree ind = match expression_tree with
	| Binop (Impl, a, b) -> 
		if (Tree_table.mem expressions a) then 
			Tree_table.replace proof b (ind,Tree_table.find expressions a)
		else 
			if (Tree_table.mem mp a) then 
				Tree_table.replace mp a ((b, ind)::(Tree_table.find mp a))
			else 
				Tree_table.add mp a [(b, ind)]
	| _ -> ()

let rec find_axiom expression_tree axioms_tail ind = 
   	match axioms_tail with
	    | [] -> None 
	    | h :: t -> 
	    	checker_a_a := Hashtbl.create 1000;
	    	if (check_with_axiom h expression_tree) then 
	    		Some (ind)
	    	else 
	    		find_axiom expression_tree t (ind + 1)

let checked_expression expression_tree ind assumptions_table = match (is_assumption expression_tree assumptions_table) with
		| Some (value) -> Assumption (value)
		| _ -> match (find_axiom expression_tree axioms 1) with
			| Some (value)-> Axiom (value)
			| _ -> match (check_mp expression_tree) with
				| Some (i, j) -> ModusPonens (i, j)
				| _ -> NotProofed



let update_new_mp_table expression_tree ind = 
	if (Tree_table.mem mp expression_tree) then begin
		List.iter (fun (expression, i) -> Tree_table.replace proof expression (i, ind)) (Tree_table.find mp expression_tree);
		Tree_table.replace mp expression_tree [];
	end



let check_expression_tree expression_tree ind assumptions_table = begin 
	let checked_expression = checked_expression expression_tree ind assumptions_table in
		update_mp_table expression_tree ind;
		update_new_mp_table expression_tree ind;
		Tree_table.replace expressions expression_tree ind;
		checked_expression
end;;