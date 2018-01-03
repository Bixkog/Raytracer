type e = float
type vector = e list
type point = e list

let x_v = [1.; 0.; 0.];;
let y_v = [0.; 1.; 0.];;
let z_v = [0.; 0.; 1.];;

let len v = sqrt (List.fold_left (fun s e -> s +. (e**2.)) 0. v)
let dot v1 v2 = List.fold_left2 (fun s e1 e2 -> s +. (e1 *. e2)) 0. v1 v2
let add v1 v2 = List.map2 (+.) v1 v2
let add3 v1 v2 v3 = List.map2 (+.) (List.map2 (+.) v1 v2) v3
let decr v1 v2 = List.map2 (-.) v1 v2
let mult v1 v2 = List.map2 ( *.) v1 v2
let dist p1 p2 = List.fold_left2 (fun s e1 e2 -> s +. (e1 -. e2)**2.) 0. p1 p2
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

let unpack_v v = v
let unpack_p p = p

let vector_c l = l
let point_c l = l

let up_v = [0.; 1.; 0.];;
let zero_v = [0.; 0.; 0.];;
