
let parse_sphere json = 
	let open Yojson.Basic.Util in
	try 
		let center = json |> member "center"
						  |> to_list |> List.map to_float 
						  |> Vect.point_c in
		let radius = json |> member "radius" |> to_float in
		let reflectivity = json |> member "reflectivity" 
								|> to_float in
		let color = json |> member "color"
						 |> to_list |> List.map to_float 
						 |> Vect.vector_c in
		let ambient = json |> member "ambient" |> to_float in
		let lambert = json |> member "lambert" |> to_float in
		Sphere.create center radius reflectivity color ambient lambert
	with
		| e -> Printf.printf "Error while reading sphere\n"; raise e

let parse_object json =
	let open Yojson.Basic.Util in 
	let type_name = json |> member "type" |> to_string in
	match type_name with
		| "sphere" -> Scene.Sphere_ (parse_sphere (json |> member "vals"))
		| _ -> failwith("json contains wrong object type\n")


let parse_camera json = 
	try
		let open Yojson.Basic.Util in 
		let center = json |> member "center" |> to_list 
						  |> List.map to_float |> Vect.point_c in
		let direction = json |> member "direction" |> to_list 
						  |> List.map to_float |> Vect.vector_c in
		let width = json |> member "width" |> to_int in
		let height = json |> member "height" |> to_int in
		let fov = json |> member "fov" |> to_float in
		Camera.create center direction width height fov
	with
		| e -> Printf.printf "Error while reading camera\n"; raise e


let parse_light json = 
	try
		let open Yojson.Basic.Util in 
		let center = json |> member "center" |> to_list 
						  |> List.map to_float |> Vect.point_c in
		let color = json |> member "color"
						 |> to_list |> List.map to_float 
						 |> Vect.vector_c in
		let intensity = json |> member "intensity" |> to_float in
		Light.create center color intensity
	with
		| e -> Printf.printf "Error while reading light\n"; raise e


let get_scene file_name = 
	let scene_json = Yojson.Basic.from_file file_name in
	let open Yojson.Basic.Util in
	let camera = scene_json |> member "camera" |> parse_camera in
	let objects = scene_json |> member "objects" 
							 |> to_list |> List.map parse_object in
	let lights = scene_json |> member "lights" |> to_list
							|> List.map parse_light in
	(camera, objects, lights)