open Tree;;
open Printf;;
open Assumption;;
open Checker;;
open Deduction;;
open Utils;;

let (getInp, getOut) = (open_in "input.txt", open_out "output.txt");;

let (assumptions, expression) = parse_header (input_line getInp);;

let alpha = List.hd (List.rev (assumptions))

let ind = ref 0;;

let assumptions_list = (List.rev (List.tl (List.rev (assumptions))));;
add_assumptions assumptions_list 0;;

List.iter (fun a -> print_endline(a)) (process_deduction "A" 0 (Var("A")));;

fprintf getOut "%s|-(%s->%s)\n"
	(string_of_tree_list assumptions_list)
	(string_of_tree alpha)
	(string_of_tree expression)
;;

try
	while true do begin
		let line = input_line getInp in
		if (line <> "") then begin
			ind := !ind + 1;
			List.iter
			(fun proof -> (fprintf getOut "%s\n" proof))
			(process_deduction line !ind alpha);
		end
	end
	done
with
	| End_of_file -> 
		close_out getOut;
		close_in getInp
;;