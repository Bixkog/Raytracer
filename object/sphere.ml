type obj = {
	center : Vect.point;
	radius : float;
	radius2 : float;
	color : Vect.vector;
	ambient : float;
	lambert : float;
	specular : float
}

let create c r color ambient lambert specular= 
		{center = c; 
		radius = r; 
		radius2 = r*.r; 
		color = color;
		ambient = ambient;
		lambert = lambert /. Common.pi;
		specular = specular}

let normal s pos = Vect.norm (Vect.direction (s.center) pos) 

(** sphere -> ray -> hit point *)
let intersect s ray = 
	let ctc = Vect.direction (ray.Ray.center) (s.center) in
	(* length of vector *)
	let v = Vect.dot ctc (ray.Ray.target) in
	(* v - sqrt(d) = length of intersect_vect *)
	let d = s.radius2 -. (Vect.dot ctc ctc) +. (v *. v) in
	if d < 0. then None else
	let t = (v -. sqrt(d)) in
	if t <= 0.0 then None else
	Some t

(** sphere -> ray -> (hit point) -> ray *)
let reflect s ray hp = 
	let s_normal = normal s hp in
	let cos_theta = Vect.dot s_normal ray.Ray.target in
	let normal' = Vect.scale s_normal (cos_theta *. 2.) in
	Ray.create_ray 
			(Vect.move hp (Vect.scale s_normal Common.bias))
			(Vect.decr ray.Ray.target normal')