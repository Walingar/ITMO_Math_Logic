open Tree;;
open Printf;;
open Assumption;;
open Expression;;
open Checker;;

let (inp, out) = (open_in "input.txt", open_out "output.txt");;

let (assumptions_table, alpha, expression, assumptions_list) = get_assumptions_from_header (input_line inp);;
let ind_to_expression = Hashtbl.create 1000;;

let ind = ref 0;;
let alpha_string = string_of_tree alpha;;

fprintf out "%s|-(%s->%s)\n"
	(string_of_tree_list assumptions_list)
	alpha_string
	(string_of_tree expression)
;;

try
	while true do begin
		let line = input_line inp in
			if (line <> "") then begin
				ind := !ind + 1;
				let expression_tree = parse_expression_to_tree (line) in
				Hashtbl.add ind_to_expression !ind (string_of_tree expression_tree);
				let delta_k = string_of_tree expression_tree in
				let checked_expression = check_expression_tree expression_tree !ind assumptions_table in
				if (expression_tree = alpha) then begin
					fprintf out "%s->(%s->%s)\n"
						alpha_string
						alpha_string
						alpha_string;
					fprintf out "(%s->(%s->%s))->((%s->((%s->%s)->%s))->(%s->%s))\n"
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string;
					fprintf out "%s->((%s->%s)->%s)\n"
						alpha_string
						alpha_string
						alpha_string
						alpha_string;
					fprintf out "(%s->((%s->%s)->%s))->(%s->%s)\n"
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string
						alpha_string;
					fprintf out "%s->%s\n"
						alpha_string
						alpha_string;
				end
				else
					match checked_expression with
					| ModusPonens (i, j) ->
						let delta_j = Hashtbl.find ind_to_expression j in 
						fprintf out "(%s->%s)->((%s->(%s->%s))->(%s->%s))\n"
							alpha_string
							delta_j
							alpha_string
							delta_j
							delta_k
							alpha_string
							delta_k;
						fprintf out "(%s->(%s->%s))->(%s->%s)\n"
							alpha_string
							delta_j
							delta_k
							alpha_string
							delta_k;
						fprintf out "%s->%s\n"
							alpha_string
							delta_k;
					| Axiom _
					| Assumption _ -> 
						fprintf out "%s->%s->%s\n"
							delta_k
							alpha_string
							delta_k;
						fprintf out "%s\n"
							delta_k;
						fprintf out "%s->%s\n"
							alpha_string
							delta_k
					| _ -> ()



			end
		end
	done
with
	| End_of_file -> 
		close_out out;
		close_in inp
;;