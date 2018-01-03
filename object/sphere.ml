type obj = {
	center : Vect.point;
	radius : float;
	radius2 : float;
	reflectivity : float;
	color : Vect.vector;
	albedo : float
}

let create c r refl color albedo = 
		{center = c; 
		radius = r; 
		radius2 = r*.r; 
		reflectivity = refl;
		color = color;
		albedo = albedo}

let normal s pos = Vect.norm (Vect.direction (s.center) pos) 

let color obj = obj.color
(** sphere -> ray -> hit point *)
let intersect s r = 
	let ctc = Vect.direction (s.center) (r.Ray.center) in
	(* length of vector *)
	let v = Vect.dot ctc (r.Ray.target) in
	(* v - sqrt(d) = length of intersect_vect *)
	let d = s.radius2 -. (Vect.dot ctc ctc) +. (v *. v) in
	if d < 0. then None
	else 
	Some (v -. sqrt(d))

(** sphere -> ray -> (hit point) -> ray *)
let reflect s r hp = 
	let reflect_normal = normal s hp in
	let cos_theta = Vect.dot reflect_normal r.Ray.target in
	let normal' = Vect.scale reflect_normal (cos_theta *. 2.) in
	Ray.create_ray r.Ray.center (Vect.decr r.Ray.target normal')