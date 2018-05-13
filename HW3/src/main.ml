open Tree;;
open Printf;;
open Assumption;;
open Checker;;
open Deduction;;
open Utils;;
open Proof;;

let (inp, out) = (open_in "input.txt", open_out "output.txt");;

let (assumptions_readed, expression) = parse_header (input_line inp);;

add_beautiful_assumptions assumptions_readed;;
beautiful_assumptions := List.rev (!beautiful_assumptions);;

let rec print_proof pr = match pr with
    | [] -> ()
    | x :: xs-> fprintf out "%s\n" (string_of_tree x);
                 print_proof xs

let rec print_deduction assumption exp = match assumption with
	| [] -> ()
	| x :: xs -> match exp with
				| Binop (Impl, a, b) -> 
					fprintf out 
						"%s\n%s\n"
						(string_of_tree a)
						(string_of_tree b);
					print_deduction xs b;
				| _ -> ()

let main =
	let (proof, expres, ans) = prove expression in
	if (ans = "") then begin 
		fprintf out "%s|-%s\n" (string_of_tree_list assumptions_readed) (string_of_tree expression);
		print_proof proof;
		print_deduction assumptions_readed expres
	end     
    else 
    	fprintf out "%s" ans;
	close_in inp;
	close_out out