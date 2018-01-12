(* polimorphic interface *)
type scene_obj = 
	| Sphere_ of Sphere.obj 
	| Plane_ of Plane.obj

let color obj = match obj with
	| Sphere_ s -> s.Sphere.color
	| Plane_ pl -> pl.Plane.color

let intersect ray obj = match obj with
	| Sphere_ s -> Sphere.intersect s ray
	| Plane_ pl -> Plane.intersect pl ray

let normal obj hp = match obj with
	| Sphere_ s -> Sphere.normal s hp
	| Plane_ pl -> Plane.normal pl

let ambient obj = match obj with
	| Sphere_ s -> s.Sphere.ambient
	| Plane_ pl -> pl.Plane.ambient

let lambert obj = match obj with
	| Sphere_ s -> s.Sphere.lambert
	| Plane_ pl -> pl.Plane.lambert

let specular obj = match obj with
	| Sphere_ s -> s.Sphere.specular
	| Plane_ pl -> pl.Plane.specular

let reflect obj ray hp = match obj with
	| Sphere_ s -> Sphere.reflect s ray hp
	| Plane_ pl -> Plane.reflect pl ray hp

let fst_intersection objs ray = 
	let dists = List.map (intersect ray) objs in
	List.fold_left2 (fun s obj dist ->
		match dist with
			| None -> s
			| Some v -> if v < (snd s) then (Some obj, v)
									   else s) 
		(None, Common.inf) objs dists

let lst_intersection objs ray = 
	let dists = List.map (intersect ray) objs in
	List.fold_left2 (fun s obj dist ->
		match dist with
			| None -> s
			| Some v -> if v > (snd s) then (Some obj, v)
									   else s) 
		(None, 0.) objs dists

let calc_light objs obj hp light = 
		let normal_v = normal obj hp in
		let hp' = Vect.move hp (Vect.scale normal_v Common.bias) in
		let light_v = Vect.norm (Vect.direction hp' light.Light.center) in 
		let (collider_o, dist) = fst_intersection objs (Ray.create_ray hp' light_v) in
		match collider_o with
			| Some collider when obj <> collider ->	Vect.zero_v
			| _ -> (
				let light_angle = Vect.dot normal_v light_v in
				(* Printf.printf "%f " dist; *)
				(* Vect.print_v normal_v; Vect.print_v light_v; print_float light_angle;print_char '\n'; *)
				if light_angle <= 0. then 
					Vect.zero_v
				else
					Vect.scale light.Light.color 
							(light_angle *. light.Light.intensity))

(* gather floats, max at 1 *)
let calc_lights objs lights obj hp =
	(* print_float (Vect.dist hp (Vect.point_c [-40.;0.;0.])); print_char '\n'; *)
	if lambert obj = 0. then Vect.zero_v else
	let calc_point_light = calc_light objs obj hp in
	let lights_color = List.fold_left (fun s light -> Vect.add s (calc_point_light light)) 
				   		Vect.zero_v lights in
	Vect.set_max 1. lights_color

let rec calc_refl objs lights obj hp ray depth = 
	if specular obj = 0. then Vect.zero_v else
	let refl_ray = reflect obj ray hp in
	(* Ray.print_r refl_ray; print_char '\n'; *)
	calc_ray objs lights refl_ray (depth + 1)

and hit_ray objs lights obj ray dist depth = 
	let hp = Vect.move ray.Ray.center (Vect.scale (ray.Ray.target) dist) in
	let lights_color = calc_lights objs lights obj hp in (* diffuse *)
	let refl_color = calc_refl objs lights obj hp ray depth in (* specular *)
	let color_r = Vect.scale refl_color (specular obj) in
	let color_a = Vect.scale (color obj) (ambient obj) in (* ambient *)
	let color_l = Vect.scale (Vect.mult (color obj) lights_color) (lambert obj) in
	(* if Vect.len refl_color > 0. then (Vect.print_v color_r;); *)
	Vect.set_max 1. (Vect.add3 color_l color_a color_r)


and calc_ray objs lights ray depth = 
	if depth = Common.max_depth then Vect.zero_v else (* we don't want to reflect background *)
	let (obj_o, dist) = fst_intersection objs ray in
	match obj_o with
		| None -> if depth > 0 then Vect.zero_v else Common.background_color
		| Some obj -> hit_ray objs lights obj ray dist depth

