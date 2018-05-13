open Tree;;

let exp1	= ref (Hashtbl.create 1000 : (tree_t, int) Hashtbl.t);;
let mp = ref (Hashtbl.create 1000 : (tree_t, 'a list) Hashtbl.t);;
let proofing = ref (Hashtbl.create 1000 : (tree_t, int * int) Hashtbl.t);;

let check_mp expression = 
	if (Hashtbl.mem !proofing expression) then 
		(Some (Hashtbl.find !proofing expression))
	else 
		None
;;

let update_mp_table expression ind = match expression with
	| Binop (Impl, a, b) -> 
		if (Hashtbl.mem !exp1 a) then 
			Hashtbl.replace !proofing b (ind, Hashtbl.find !exp1 a)
		else 
			if (Hashtbl.mem !mp a) then 
				Hashtbl.replace !mp a ((b, ind)::(Hashtbl.find !mp a))
			else 
				Hashtbl.add !mp a [(b, ind)]
	| _ -> ()

let update_new_mp_table expression ind = 
	if (Hashtbl.mem !mp expression) then begin
		List.iter 
			(fun (expression, i) -> 
				Hashtbl.replace !proofing expression (i, ind)) 
			(Hashtbl.find !mp expression);
		Hashtbl.replace !mp expression [];
	end