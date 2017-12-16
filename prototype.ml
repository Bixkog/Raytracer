
let inf = 1000;;

type obj_ = Test;;
type ray_ = Test;;
type vect_ = Test;;

let objects = [Test; Test; Test];;
let camera = ();;

let max_i f l comp =
	let rec aux l m m_i i= 
		match l with
			| [] -> m_i
			| h::t -> let fh = f h in
					  if comp fh m then aux t fh i (i+1)
					  else aux t m m_i (i+1)
	in
	match l with
		| [] -> failwith("got empty list in max_i")
		| h::t -> aux t (f h) 0 1
	;;

let max_e f l comp =
	let rec aux l m = 
		match l with
			| [] -> m
			| h::t -> let fh = f h in
					  if comp m fh then aux t fh
					  else aux t m
	in
	match l with
		| [] -> failwith("got empty list in max_e")
		| h::t -> aux t (f h)
	;;


let dist camera = function
	| None -> inf
	| Some _ -> 0;;

let intersect ray obj = None;;

let get_nearest_obj ray objects =
	let hit_points = List.map (intersect ray) objects in
	let m_i = max_i (dist camera) hit_points (<) in
	let hit_point = List.nth hit_points m_i in
	match hit_point with
		| None -> None
		| Some v -> Some (List.nth objects m_i, v)
	;;



let calc_pixel ray =
	let opt_obj = get_nearest_obj ray objects in
	match opt_obj with
		| None -> Test (* blank pixel *)
		| Some (obj, hit_point) -> Test;; 
				(* cast recursive shadow ray *)
				(* with recursion depth counter *)

let make_ray pixel = Test;;

let calc_pixels pixels =
	List.map (fun pixel -> make_ray pixel |> calc_pixel) pixels;