let max_depth = 3;;
let inf = 10.**9.;;
let bias = 0.1**4.;;
let background_color = Vect.vector_c [0.5; 0.5; 0.5];;

(* polimorphic interface *)
type scene_obj = Sphere_ of Sphere.obj

let color obj = match obj with
	| Sphere_ s -> Sphere.color s

let intersect ray obj = match obj with
	| Sphere_ s -> Sphere.intersect s ray

let normal obj hp = match obj with
	| Sphere_ s -> Sphere.normal s hp

let fst_intersection objs ray = 
	let dists = List.map (intersect ray) objs in
	List.fold_left2 (fun s obj dist ->
		match dist with
			| None -> s
			| Some v -> if v < (snd s) then (Some obj, v)
									   else s) 
		(None, inf) objs dists

let calc_light objs obj hp light = 
		let (collider, dist) = 
			fst_intersection objs 
							(Ray.create_ray hp 
								(Vect.direction hp light.Light.center)) in
		match collider with
			| None -> (
				let normal_v = normal obj hp in
				let light_v = Vect.direction hp light.Light.center in
					Vect.scale light.Light.color 
							((Vect.dot normal_v light_v) *. light.Light.intensity)
				)
			| Some obj -> Vect.zero_v

(* gather floats, only positive, max at 1 *)
let calc_lights objs lights obj hp =
	let calc_point_light = calc_light objs obj hp in
	List.fold_left (fun s light -> Vect.add s (calc_point_light light)) 
				   (Vect.zero_v) lights 

let hit_ray objs lights obj ray l = 
	let hp = Vect.move ray.Ray.center (Vect.scale (ray.Ray.target) l) in
	let lights_color = calc_lights objs lights obj hp in (* diffuse *)
	Vect.mult (color obj) lights_color

let calc_ray objs lights ray = 
	let (obj_o, l) = fst_intersection objs ray in
	match obj_o with
		| None -> background_color
		| Some obj -> hit_ray objs lights obj ray l
