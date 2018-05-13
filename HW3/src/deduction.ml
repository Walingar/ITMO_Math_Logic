open Tree;;
open Checker;;
open Mp;;
open Assumption;;
open Utils;;

let ind_to_expression = ref (Hashtbl.create 1000: (int, tree_t) Hashtbl.t);;

let deduction_a = read_data_file "data/deduction_a.txt";;
let deduction_axiom = read_data_file "data/deduction_axiom.txt";;
let deduction_mp = read_data_file "data/deduction_mp.txt";;

let ind = ref(0);;

let process_deduction expression alpha = begin
	ind := !ind + 1;
	let expression_tree = expression in
	let delta_k = expression_tree in
	Hashtbl.add !ind_to_expression !ind delta_k;
	let checked_expression = check_expression_tree expression_tree !ind in
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
end

let get_deduction_proof assump proofs alpha = begin
	ind_to_expression := Hashtbl.create 1000;
	ind := 0;
	mp := Hashtbl.create 1000;
	proofing := Hashtbl.create 1000;
	exp1 := Hashtbl.create 1000;
	assumptions := Hashtbl.create 1000;
	add_assumptions assump 0;
	let deduction_proof = ref [] in
	List.iter 
		(fun proof -> deduction_proof := ((List.rev (process_deduction proof alpha)) @ !deduction_proof))
		proofs;
	List.rev !deduction_proof
end