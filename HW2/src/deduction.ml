open Tree;;
open Checker;;
open Utils;;

let ind_to_expression = ref (Hashtbl.create 1000: (int, tree_t) Hashtbl.t);;

let deduction_a = read_data_file "data/deduction_a.txt";;
let deduction_axiom = read_data_file "data/deduction_axiom.txt";;
let deduction_mp = read_data_file "data/deduction_mp.txt";;

         

let process_deduction expression ind alpha =
	let expression_tree = parse_body_line (expression) in
	let delta_k = expression_tree in
	Hashtbl.add !ind_to_expression ind delta_k;
	let checked_expression = check_expression_tree expression_tree ind in
	if (expression_tree = alpha) then
		parse_data_lines deduction_a alpha (Var("")) (Var(""))
	else
		match checked_expression with
		 | ModusPonens (i, j) -> begin
		 	let delta_j = Hashtbl.find !ind_to_expression j in 
		 	parse_data_lines deduction_mp alpha delta_j delta_k
		 	end
		 | Axiom _
		 | Assumption _ ->
		 	parse_data_lines deduction_axiom delta_k alpha (Var(""))
		 | _ -> []

let get_deduction_proof alpha proofs = begin
	ind_to_expression := Hashtbl.create 1000;
	let deduction_proof = ref [] in
	List.iter 
		(fun (proof, i) -> deduction_proof := ((process_deduction proof i alpha) @ !deduction_proof))
		proofs;
	List.rev !deduction_proof
end