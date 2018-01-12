let pi = 3.1415;;
let max_depth = 3;;
let inf = 10.**9.;;
let bias = 0.1**4.;;
let background_color = Vect.vector_c [0.5; 0.5; 0.5];;

let unfold f s = 
	let rec aux f s acc = 
		let opt = f s in 
		match opt with
		| None -> List.rev acc
		| Some (r, s') -> aux f s' (r :: acc) in
	aux f s []