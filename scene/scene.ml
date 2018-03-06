let objs = ref [];;

let fst_intersection objs ray = 
	let dists = List.map (Object.intersect ray) objs in
	List.fold_left2 (fun s obj dist ->
		match dist with
			| None -> s
			| Some v -> if v < (snd s) then (Some obj, v)
									   else s) 
		(None, Common.inf) objs dists

let intersect ray = 
	(* let objs = Octree.get_objects !(Octree.oct) ray in *)
	fst_intersection !objs ray

let calc_light obj hp light = 
		let normal_v = Object.normal obj hp in
		let light_dist = Vect.dist hp light.Light.center in
		let hp' = Vect.move hp (Vect.scale normal_v Common.bias) in
		let light_v = Vect.norm (Vect.direction hp' light.Light.center) in 
		let (collider_o, dist) = intersect (Ray.create_ray hp' light_v) in
		match collider_o with
			| Some collider when obj <> collider
								 && light_dist > dist
					 ->	Vect.zero_v
			| _ -> (
				let light_angle = Vect.dot normal_v light_v in
				if light_angle <= 0. then 
					Vect.zero_v
				else
					Vect.scale light.Light.color 
							(light_angle *. (Light.intens light hp)))

(* gather floats, max at 1 *)
let calc_lights lights obj hp =
	(* print_float (Vect.dist hp (Vect.point_c [-40.;0.;0.])); print_char '\n'; *)
	if Object.lambert obj = 0. then Vect.zero_v else
	let calc_point_light = calc_light obj hp in
	let lights_color = List.fold_left (fun s light -> Vect.add s (calc_point_light light)) 
				   		Vect.zero_v lights in
	Vect.set_max 1. lights_color

let rec calc_refl lights obj hp ray depth = 
	if Object.specular obj = 0. then Vect.zero_v else
	let refl_ray = Object.reflect obj ray hp in
	(* Ray.print_r refl_ray; print_char '\n'; *)
	calc_ray lights refl_ray (depth + 1)

and hit_ray lights obj ray dist depth = 
	let hp = Vect.move ray.Ray.center (Vect.scale (ray.Ray.target) dist) in
	let lights_color = calc_lights lights obj hp in (* diffuse *)
	let refl_color = calc_refl lights obj hp ray depth in (* specular *)
	let color_r = Vect.scale refl_color (Object.specular obj) in
	let color_a = Vect.scale (Object.color obj) (Object.ambient obj) in (* ambient *)
	let color_l = Vect.scale lights_color (Object.lambert obj) in
	(* if Vect.len refl_color > 0. then (Vect.print_v color_r;); *)
	Vect.set_max 1. (Vect.add3 color_l color_a color_r)


and calc_ray lights ray depth = 
	if depth = Common.max_depth then Vect.zero_v else (* we don't want to reflect background *)
	let (obj_o, dist) = intersect ray in
	match obj_o with
		| None -> if depth > 0 then Vect.zero_v else Common.background_color
		| Some obj -> hit_ray lights obj ray dist depth

