let pixel_of_color v =
	let l = Vect.unpack_v v in
	let l_int = List.map (fun f -> int_of_float (f *. 255.)) l in
	l_int

let gamma_correct gamma colors =
	let gamma' = 1. /. gamma in
	List.map (Vect.map (fun e -> e ** gamma')) colors

let render camera lights gamma = 
	Printf.printf "Creating rays\n"; flush_all ();
	let rays = Camera.get_rays camera in
	Printf.printf "Calculating colors\n"; flush_all ();
	let colors = List.rev (List.rev_map (fun ray -> Scene.calc_ray lights ray 0) rays) in
	let colors_g = gamma_correct gamma colors in
	Printf.printf "Calculating pixels\n"; flush_all ();
	let pixels = List.rev (List.rev_map pixel_of_color colors_g) in 
	List.flatten pixels