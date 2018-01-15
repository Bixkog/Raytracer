let pixel_of_color v =
	let l = Vect.unpack_v v in
	let l_int = List.map (fun f -> int_of_float (f *. 255.)) l in
	l_int

let render camera objs lights = 
	let rays = Camera.get_rays camera in
	Printf.printf "Created rays\n"; flush_all ();
	let colors = List.rev (List.rev_map (fun ray -> Scene.calc_ray objs lights ray 0) rays) in
	Printf.printf "Calculated colors\n"; flush_all ();
	let pixels = List.rev (List.rev_map pixel_of_color colors) in 
	Printf.printf "Calculated pixels\n"; flush_all ();
	List.flatten pixels