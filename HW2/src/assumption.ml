open Tree;;

let assumptions	= ref (Tree_table.create 1000);;
let (>>) x f = f x;;

let rec add_assumptions assumptions_list ind = 
	match assumptions_list with
		| [] -> ()
		| head::tail -> 
			Tree_table.add !assumptions head ind;
			add_assumptions tail (ind + 1)

let is_assumption expression = 
	if (Tree_table.mem !assumptions expression) then 
		(Some (Tree_table.find !assumptions expression))
	else (None)

let clear_assumptions =
	assumptions := Tree_table.create 1000