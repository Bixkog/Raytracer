type obj = 
	{
		origin : Vect.point;
		orientation : Vect.vector;
		color : Vect.vector;
		ambient : float;
		lambert : float;
		specular : float
	}

let create origin orientation color ambient lambert specular =
	{
		origin = origin;
		orientation = Vect.norm orientation;
		color = color;
		ambient = ambient;
		lambert = lambert /. Common.pi;
		specular = specular
	}

let normal pl = pl.orientation

let intersect pl ray = 
	let denom = Vect.dot ray.Ray.target pl.orientation in
	if abs_float denom < 0.0000001 then None else
	let aux = Vect.direction ray.Ray.center pl.origin in
	let t = (Vect.dot aux pl.orientation) /. denom in
	if t < 0. then None else
	Some t

let reflect pl ray hp = 
	let pl_normal' = normal pl in (* need correct direction *)
	let pl_normal  = 
		(if Vect.dot pl_normal' ray.Ray.target > 0. 
			then Vect.neg pl_normal'
			else pl_normal') in
	let cos_theta = Vect.dot pl_normal ray.Ray.target in
	let normal' = Vect.scale pl_normal (cos_theta *. 2.) in
	(* Vect.print_v (Vect.decr normal' ray.Ray.target); print_char '\n'; *)
	let refl_ray = Ray.create_ray 
			(Vect.move hp (Vect.scale pl_normal Common.bias))
			(Vect.decr ray.Ray.target normal') in
	(* Ray.print_r refl_ray; print_char '\n'; *)
	refl_ray
