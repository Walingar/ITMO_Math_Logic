open Tree;;
open Printf;;
open Assumption;;
open Expression;;
open Checker;;

let (getInp, getOut) = (open_in "input.txt", open_out "output.txt");;

let assumptions_table = get_assumptions_from_header (input_line getInp);;

let ind = ref 0;;
try
	while true do begin
		let line = input_line getInp in
			if (line <> "") then begin
				ind := !ind + 1;
				let expression_tree = parse_expression_to_tree (line) in
				let checked_expression = check_expression_tree expression_tree !ind assumptions_table in
					fprintf getOut "(%d) %s (%s)\n" !ind (string_of_tree (expression_tree)) (string_of_checked_expression (checked_expression))
			end
		end
	done
with
	| End_of_file -> 
		close_out getOut;
		close_in getInp
;;