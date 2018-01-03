let pi = 3.1415;;


let unfold f s = 
	let rec aux f s acc = 
		let opt = f s in 
		match opt with
		| None -> List.rev acc
		| Some (r, s') -> aux f s' (r :: acc) in
	aux f s []