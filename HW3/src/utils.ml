open Tree;;

let (>>) x f = f x;;

let parse_body_line expression = expression >> Lexing.from_string >> Parser.body Lexer.read;;

let parse_header header = header >> Lexing.from_string >> Parser.header Lexer.read;;

let parse_data_line expression a b c = expression >> Lexing.from_string >> Parser.body (Lexer.write a b c);;

let parse_data_lines expressions a b c = List.map (fun expression -> parse_data_line expression a b c) expressions

let read_data_file file_name = 
	let lines = ref [] in
	let inp = open_in file_name in
	try
		while true do
			lines := input_line (inp) :: !lines	
		done;
		!lines
	with 
		|End_of_file -> 
			close_in inp;
			List.rev !lines
;;