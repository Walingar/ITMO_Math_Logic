open Tree;;
open Printf;;
open Assumption;;
open Checker;;
open Deduction;;
open Utils;;

let vars = (Hashtbl.create 3 : (string, tree_t) Hashtbl.t)
;;

let operation op = match op with
	| Conj -> (&&)
	| Impl -> (fun a b -> (not a) || b)
	| Disj -> (||)

let string_bool x = 
	if (x) then
		"T"
	else
		"F"

let get_lemma file_name a b = parse_data_lines (read_data_file ("data/" ^ file_name ^ ".txt")) a b (Var(""))

let rec deduction_all assump expression =
    match assump with
    | [] -> expression
    | x :: xs -> deduction_all xs (Binop(Impl, x, expression))


let get_vars expr =
	let varibs = (Hashtbl.create 5 : (string, int) Hashtbl.t) in
	let var_list = ref [] in
	let rec get_vars' = function
		| Var s -> if not (Hashtbl.mem varibs s) then begin 
			Hashtbl.add varibs s 0;
			var_list := !var_list @ [s]
		end
		| Neg a -> get_vars' a
		| Binop (_, a, b) -> get_vars' a;
			get_vars' b
	in get_vars' expr;
	!var_list

let get_proof e =
	let get_name a b = function
		| Conj -> 
			"conj_" ^ string_bool a ^ string_bool b
		| Disj -> 
			"disj_" ^ string_bool a ^ string_bool b
		| Impl -> 
			"impl_" ^ string_bool a ^ string_bool b
	in
	let rec get_proof' = function
		| Var s -> 
			(let value = Hashtbl.find vars s in
				match value with
					| Neg a -> 
						(false, [value])
					| _ -> 
						(true, [value])
				)
		| Neg a ->
			let (prooved, proof) = get_proof' a in
			(
				not prooved, 
				proof @ (get_lemma ("not_" ^ string_bool prooved) a (Var("")))
			)
		| Binop (_ as op, a, b) ->
			let (prooved_a, proof_a) = get_proof' a in
			let (prooved_b, proof_b) = get_proof' b in
			(
				operation op prooved_a prooved_b,
				proof_a @ proof_b @ (get_lemma (get_name prooved_a prooved_b op) a b)
			)
	in get_proof' e



let get_answer variables =
	let rec get_string = function
		| x :: [] -> 
			(match x with
				| Neg (Var var) -> var ^ "=Л"
				| Var var       -> var ^ "=И"
				| _ -> ""
			)
		| x :: xs -> 
			(match x with
				| Neg (Var var) -> var ^ "=Л, " ^ (get_string xs)
				| Var var       -> var ^ "=И, " ^ (get_string xs)
				| _ -> ""
			)
		| [] -> ""                
	in "Высказывание ложно при " ^ get_string variables

let prove expression =
	let e' = deduction_all !beautiful_assumptions expression in
    let answer = ref [] in
    let var_list = get_vars e' in
    let rec proofing current = function
        | [] -> let (is_proved, proof) = get_proof e' in
        		if (not is_proved) then 
        			answer := current;
        		(proof, is_proved)
        | x :: xs ->
        			let assump_false = current @ [Neg(Var x)] in
					let assump_true = current @ [Var x] in

					Hashtbl.replace vars x (Neg(Var x));
					let (proof1, ok1) = proofing assump_false xs in
					if (not ok1) then
						([], false)
					else begin
						Hashtbl.replace vars x (Var x);
						let (proof2, ok2) = proofing assump_true xs in
						if (not ok2) then
							([], false)
						else begin
							let deduce_proof1 = get_deduction_proof current proof1 (Neg (Var (x))) in 
							let deduce_proof2 = get_deduction_proof current proof2 (Var (x)) in
							let exclude = get_lemma "exclude" e' (Var x) in
								(deduce_proof1 @ deduce_proof2 @ exclude, true)
						end 
					end
    in
    let (proof, generalize) = proofing [] (var_list) in
    if (not generalize) then 
    	(proof, e', get_answer !answer)
    else (proof, e', "")