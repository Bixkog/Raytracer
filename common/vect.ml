type e = float
type vector = e list
type point = e list

let x_v = [1.; 0.; 0.];;
let y_v = [0.; 1.; 0.];;
let z_v = [0.; 0.; 1.];;

let len v = sqrt (List.fold_left (fun s e -> s +. (e**2.)) 0. v)
let neg v = List.map ( ( *.) (-1.)) v
let dot v1 v2 = List.fold_left2 (fun s e1 e2 -> s +. (e1 *. e2)) 0. v1 v2
let add v1 v2 = List.map2 (+.) v1 v2
let add3 v1 v2 v3 = add (add v1 v2) v3
let decr v1 v2 = List.map2 (-.) v1 v2
let mult v1 v2 = List.map2 ( *.) v1 v2
let dist p1 p2 = sqrt (List.fold_left2 (fun s e1 e2 -> s +. (e1 -. e2)**2.) 0. p1 p2)
let direction p1 p2 = List.map2 (fun e1 e2 -> e2 -. e1) p1 p2
let scale v e = List.map (( *.) e) v
let move p v = List.map2 (+.) p v
let norm v = let l = len v in List.map (fun e -> e /. l) v
			 

let cross v1 v2 = let x1::(y1::(z1::_)) = v1 in
				  let x2::(y2::(z2::_)) = v2 in
				  add3 
					  (scale x_v (y1 *. z2 -. y2 *. z1))
					  (scale y_v (x1 *. z2 -. x2 *. z1))
					  (scale z_v (x1 *. y2 -. x2 *. y1))

let set_max m v1 = 
		let max_e = List.fold_left max 0. v1 in
		if max_e > 1. then
			List.map (fun e -> e /. max_e) v1
		else
			v1

let x v = List.hd v
let y v = List.hd (List.tl v)
let z v = List.hd (List.tl (List.tl v))

let px p = List.hd p
let py p = List.hd (List.tl p)
let pz p = List.hd (List.tl (List.tl p))


let e_dists p1 p2 = 
	let distance e1 e2 = abs_float (e1 -. e2) in
	(distance (x p1) (x p2),
	distance (y p1) (y p2),
	distance (z p1) (z p2))

let unpack_v v = v
let unpack_p p = p

let vector_c l = l
let point_c l = l

let print_v v = 
		print_char '[';
		List.iteri (fun i f -> print_float f;
							   if i < 2 then print_string "; ")
					v;
		print_char ']'

let print_p p = 
		print_char '[';
		List.iteri (fun i f -> print_float f;
							   if i < 2 then print_string "; ")
					p;
		print_char ']'


let up_v = [0.; 1.; 0.];;
let zero_v = [0.; 0.; 0.];;
