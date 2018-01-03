type obj = 
	{
		center : Vect.point;
		direction : Vect.vector;
		width : int;
		height : int;
		fov : float;
	}

let create c d w h fv = 
	{
		center = c;
		direction = Vect.norm d;
		width = w;
		height = h;
		fov = fv;
	}

let ray_generator 
		camera 
		half_width half_height 
		pixel_width pixel_height 
		right_v up_v 
		(x, y) = 
			if y = camera.height then
				None
			else
				let x_offset = Vect.scale right_v 
								((float_of_int x) *. pixel_width -. half_width) in
				let y_offset = Vect.scale up_v 
								((float_of_int y) *. pixel_height -. half_height) in
				Some ((Ray.create_ray camera.center 
						(Vect.add3 camera.direction x_offset y_offset)),
					  (if x + 1 = camera.width then (0, y + 1) else (x + 1, y))
					 )

let get_rays camera = 
	let fov_r = Common.pi *. camera.fov /. 360.0 in
	let aspect_ratio = (float_of_int camera.height) /. (float_of_int camera.width) in
	let half_width = tan fov_r in
	let half_height = aspect_ratio *. half_width in
	let camera_width = half_width *. 2. in
	let camera_height = half_height *. 2. in
	let pixel_width = camera_width /. (float_of_int (camera.width - 1)) in
	let pixel_height = camera_height /. (float_of_int (camera.height - 1)) in
	let right_v = Vect.norm (Vect.cross camera.direction Vect.up_v) in
	let up_v = Vect.norm (Vect.cross right_v camera.direction) in
	Printf.printf "Calculated rays\n";
	Common.unfold 
		(ray_generator camera half_width half_height 
					   pixel_width pixel_height right_v up_v)
		(0, 0)


