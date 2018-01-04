let max_depth = 3;;
let inf = 10.**9.;;
let bias = 0.1**3.;;
let background_color = Vect.vector_c [0.5; 0.5; 0.5];;

(* polimorphic interface *)
type scene_obj = Sphere_ of Sphere.obj

let color obj = match obj with
	| Sphere_ s -> s.Sphere.color

let intersect ray obj = match obj with
	| Sphere_ s -> Sphere.intersect s ray

let normal obj hp = match obj with
	| Sphere_ s -> Sphere.normal s hp

let ambient obj = match obj with
	| Sphere_ s -> s.Sphere.ambient

let lambert obj = match obj with
	| Sphere_ s -> s.Sphere.lambert

let fst_intersection objs ray = 
	let dists = List.map (intersect ray) objs in
	List.fold_left2 (fun s obj dist ->
		match dist with
			| None -> s
			| Some v -> if v < (snd s) then (Some obj, v)
									   else s) 
		(None, inf) objs dists

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
		let hp' = Vect.move hp (Vect.scale normal_v bias) in
		let light_v = Vect.norm (Vect.direction hp' light.Light.center) in 
		let (collider_o, dist) = lst_intersection objs (Ray.create_ray hp' light_v) in
		match collider_o with
			| Some collider when obj <> collider ->
					Printf.printf "%f " (lambert collider);
					Vect.zero_v
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
	let calc_point_light = calc_light objs obj hp in
	let lights_color = List.fold_left (fun s light -> Vect.add s (calc_point_light light)) 
				   		Vect.zero_v lights in
	Vect.set_max 1. lights_color

let hit_ray objs lights obj ray dist = 
	let hp = Vect.move ray.Ray.center (Vect.scale (ray.Ray.target) dist) in
	let lights_color = calc_lights objs lights obj hp in (* diffuse *)
	let color_l = Vect.scale (Vect.mult (color obj) lights_color) (lambert obj) in
	let color_a = Vect.scale (color obj) (ambient obj) in
	Vect.set_max 1. (Vect.add color_l color_a) 

let calc_ray objs lights ray = 
	let (obj_o, dist) = fst_intersection objs ray in
	match obj_o with
		| None -> background_color
		| Some obj -> hit_ray objs lights obj ray dist
