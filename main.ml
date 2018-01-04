let get_filename () = 
	if Array.length Sys.argv <> 3 then
		"scene.json", "output.ppm"
	else
		Sys.argv.(1), Sys.argv.(2)

let get_format s = 
	let splited_s = String.split_on_char '.' s in
	if List.length splited_s = 1 then "ppm"
	else
	List.nth splited_s (List.length splited_s - 1)

let ppm_output camera render filename = 
	let out = open_out_bin filename in
	Printf.fprintf out "P6\n%d %d\n255\n" 
					camera.Camera.width camera.Camera.height;
	List.iter (fun b -> output_byte out b) render;
	output_char out '\n';
	close_out out

let () = 
	let input_file, output_file = get_filename () in
	Printf.printf "Input file %s\n" input_file;
	Printf.printf "Output file %s\n" output_file;
	let output_format = get_format output_file in
	Printf.printf "Output format %s\n" output_format;
	let camera, objs, lights = Reader.get_scene input_file in
	Printf.printf "Read file %s\n" input_file;
	let render = Renderer.render camera objs lights in
	Printf.printf "Rendered scene\n";
	match output_format with
		| "ppm" -> ppm_output camera render output_file;
					Printf.printf "Saved render\n"
		| _ -> failwith("invalid output format")

