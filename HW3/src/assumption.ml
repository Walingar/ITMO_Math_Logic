open Tree;;

let assumptions	= ref (Hashtbl.create 1000 : (tree_t, int) Hashtbl.t);;

let beautiful_assumptions = ref ([Var("")]);;
let (>>) x f = f x;;

let rec add_assumptions assumptions_list ind = 
	match assumptions_list with
		| [] -> ()
		| head::tail -> 
			Hashtbl.add !assumptions head ind;
			add_assumptions tail (ind + 1)

let rec add_beautiful_assumptions assumptions_list =
	beautiful_assumptions := assumptions_list

let is_assumption expression = 
	if (Hashtbl.mem !assumptions expression) then 
		(Some (Hashtbl.find !assumptions expression))
	else (None)