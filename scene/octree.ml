let max_q = 4;;

type node = 
	| EmptyNode
	| TerminalNode of Object.scene_obj list
	| ParentNode of node array

type octree = Octree of node * Vect.point * float

let print_octree oct = 
	let Octree (node, center, size) = oct in
	Vect.print_p center; print_newline ();
	print_float size; print_newline ();
	let rec print_node node = 
		match node with
		| EmptyNode -> print_endline "EmptyNode";
		| TerminalNode objs -> print_endline "TerminalNode";
				List.iter (fun obj -> Vect.print_p (Object.center obj)) objs; 
				print_newline ();
		| ParentNode nodes -> print_endline "ParentNode (";
				Array.iter (fun n -> print_node n; print_newline ()) nodes;
				print_endline ")";
	in
	print_node node

let gen_corners center size = 
	let size_2 = size /. 2. in
	let x_move = (Vect.scale Vect.x_v size_2) in
	let y_move = (Vect.scale Vect.y_v size_2) in
	let z_move = (Vect.scale Vect.z_v size_2) in
	let corners = [center; center; center; center; center; center; center; center] in
	List.mapi (fun i c ->
				let c' = ref c in
				(if i land 4 > 0 then c' := Vect.move !c' x_move
					else c' := Vect.move !c' (Vect.neg x_move));
				(if i land 2 > 0 then c' := Vect.move !c' y_move
					else c' := Vect.move !c' (Vect.neg y_move));
				(if i land 1 > 0 then c' := Vect.move !c' z_move
					else c' := Vect.move !c' (Vect.neg z_move));
				!c') 
			corners

let is_in size center obj =
	match obj with
		| Object.Sphere_ sphere ->
			let max_dist = size /. 2. +. sphere.Sphere.radius in
			let x_dist, y_dist, z_dist = Vect.e_dists center sphere.Sphere.center in
			(* is completly out of the box *)
			if x_dist > max_dist || y_dist > max_dist || z_dist > max_dist then
				false
			else
				true
		| Object.Plane_ plane ->
			let corners = gen_corners center size in
			let angles = List.map (fun c -> 
										Vect.dot 
											plane.Plane.orientation 
											(Vect.direction plane.Plane.origin c)) corners in
			let neg, pos = List.fold_left (fun (n, p) a -> if a > 0. 
											then (n, a::p) else (a::n, p)) ([], []) angles in
			if neg = [] || pos = [] then
				false
			else
				true
			

let gen_centers center size = 
	let size_4 = size /. 4. in
	let x_move = (Vect.scale Vect.x_v size_4) in
	let y_move = (Vect.scale Vect.y_v size_4) in
	let z_move = (Vect.scale Vect.z_v size_4) in
	let centers = [center; center; center; center; center; center; center; center] in
	List.mapi (fun i c ->
				let c' = ref c in
				(if i land 4 > 0 then c' := Vect.move !c' x_move
					else c' := Vect.move !c' (Vect.neg x_move));
				(if i land 2 > 0 then c' := Vect.move !c' y_move
					else c' := Vect.move !c' (Vect.neg y_move));
				(if i land 1 > 0 then c' := Vect.move !c' z_move
					else c' := Vect.move !c' (Vect.neg z_move));
				!c') 
			centers


let rec create_node objs size depth center = 
	let objs' = List.filter (is_in size center) objs in
	if objs' = [] then
		EmptyNode
	else
	if depth = 0 || List.length objs' < max_q then 
		TerminalNode objs'
	else
	let subnodes_centers = gen_centers center size in
	(* List.iter Vect.print_p subnodes_centers; *)
		ParentNode (Array.of_list (
						List.map 
							(create_node objs' (size /. 2.) (depth - 1)) 
							subnodes_centers
						))


let create objs = 
	let start_size = 100. in
	let center = Vect.point_c [0.; 0.; 0.] in
	let depth = 8 in
	Octree (create_node objs start_size depth center,
			center, start_size)


(* searching *)

let first_node tx0 ty0 tz0 txm tym tzm = 
	let output = ref 0 in
	let entry_plane, max_t = List.fold_left 
						(fun (i, m) (ei, e) -> 
							if e > m then (ei, e) else (i, m))
						(0, tx0)
						(List.tl (List.combine [0; 1; 2] [tx0; ty0; tz0])) in
	(match entry_plane with
	| 0 -> (* YZ *)
		begin
			if tym < tx0 then output := !output + 2;
			if tzm < tx0 then output := !output + 4;
		end
	| 1 -> (* XZ *)
		begin
			if txm < ty0 then output := !output + 1;
			if tzm < ty0 then output := !output + 4;
		end
	| 2 -> (* XY *)
		begin
			if txm < tz0 then output := !output + 1;
			if tym < tz0 then output := !output + 2;
		end
	| _ -> ());
	!output

let new_node ts is =
	let exit_plane, _ = List.fold_left 
						(fun (i, m) (ei, e) -> 
							if e < m then (ei, e) else (i, m))
						(0, List.hd ts)
						(List.tl (List.combine [0; 1; 2] ts))
					in
	List.nth is exit_plane


let rec proc_subtree tx0 ty0 tz0 tx1 ty1 tz1 n mask =
	if tx1 < 0. || ty1 < 0. || tz1 < 0. then [] else
	match n with 
	| EmptyNode -> (* print_endline "EmptyNode"; *) []
	| TerminalNode objs ->(*  print_endline "TerminalNode"; *) objs
	| ParentNode subnodes -> 
		let txm = (tx0 +. tx1) /. 2. in
		let tym = (ty0 +. ty1) /. 2. in
		let tzm = (tz0 +. tz1) /. 2. in		
		(* print_endline "Parent"; *)
		(* print_int (first_node tx0 ty0 tz0 txm tym tzm);print_newline (); *)
		(* List.iter (fun f -> print_float f; print_newline ()) [txm; tym; tzm]; *)
		(*print_int (new_node [txm; tym; tzm] [4; 2; 1]);print_newline ();
		print_int (new_node [txm; tym; tz1] [5; 3; 8]);print_newline ();
		print_int ((new_node [tx1; tym; tz1] [8; 7; 8]));print_newline ();*)
		let rec pick_node = function 
			| 0 -> 
				begin
					let res = proc_subtree tx0 ty0 tz0 txm tym tzm subnodes.(mask) mask in
					res @ (pick_node (new_node [txm; tym; tzm] [4; 2; 1]))
				end
			| 1 -> 
				begin
					let res = proc_subtree tx0 ty0 tzm txm tym tz1 subnodes.(1 lxor mask) mask in
					res @ (pick_node (new_node [txm; tym; tz1] [5; 3; 8]))
				end
			| 2 -> 
				begin
					let res = proc_subtree tx0 tym tz0 txm ty1 tzm subnodes.(2 lxor mask) mask in
					res @ (pick_node (new_node [txm; ty1; tzm] [6; 8; 3]))
				end
			| 3 -> 
				begin
					let res = proc_subtree tx0 tym tzm txm ty1 tz1 subnodes.(3 lxor mask) mask in
					res @ (pick_node (new_node [txm; ty1; tz1] [7; 8; 8]))
				end
			| 4 -> 
				begin
					let res = proc_subtree txm ty0 tz0 tx1 tym tzm subnodes.(4 lxor mask) mask in
					res @ (pick_node (new_node [tx1; tym; tzm] [8; 6; 5]))
				end
			| 5 -> 
				begin
					let res = proc_subtree txm ty0 tzm tx1 tym tz1 subnodes.(5 lxor mask) mask in
					res @ (pick_node (new_node [tx1; tym; tz1] [8; 7; 8]))
				end
			| 6 -> 
				begin
					let res = proc_subtree txm tym tz0 tx1 ty1 tzm subnodes.(6 lxor mask) mask in
					res @ (pick_node (new_node [tx1; ty1; tzm] [8; 8; 7]))
				end
			| 7 -> 
				begin
					let res = proc_subtree txm tym tzm tx1 ty1 tz1 subnodes.(7 lxor mask) mask in
					res @ (pick_node 8)
				end
			| _ -> []
		in
		pick_node (first_node tx0 ty0 tz0 txm tym tzm)

let fix_ray ray size = 
	let index_mask = ref 0 in
	let bits = Array.of_list [4; 2; 1] in
	let center_l = Vect.unpack_p ray.Ray.center in
	let target_l = Vect.unpack_v ray.Ray.target in
	let center_l', target_l' = 
		List.split (
			List.mapi 
				(fun i (ci, ti) -> if ti < 0. then 
									(index_mask := !index_mask lor bits.(i);
									(-.ci, -.ti)) (* origin *2 - ci when center is not 0, 0, 0*)
								   else
								   	(ci, ti);
				) 
				(List.combine center_l target_l)
			) in
	(* print_int !index_mask; print_newline (); *)
	(Ray.create_ray (Vect.point_c center_l') (Vect.vector_c target_l')), !index_mask

let node_max oct = 
	let Octree (_, center, size) = oct in
	let size_h = size /. 2. in
	Array.of_list [Vect.px center +. size_h;
				   Vect.py center +. size_h;
				   Vect.pz center +. size_h]

let node_min oct = 
	let Octree (_, center, size) = oct in
	let size_h = size /. 2. in
	Array.of_list [Vect.px center -. size_h;
				   Vect.py center -. size_h;
				   Vect.pz center -. size_h]

let get_objects oct ray = 
	let Octree (root, _, size) = oct in
	let ray', mask = fix_ray ray size in
	(* Ray.print_r ray'; *)
	let maxs = node_max oct in
	let mins = node_min oct in 
	let tx0 = (mins.(0) -. Vect.px ray'.Ray.center) /. (Vect.x ray'.Ray.target) in
	let tx1 = (maxs.(0) -. Vect.px ray'.Ray.center) /. (Vect.x ray'.Ray.target) in
	let ty0 = (mins.(1) -. Vect.py ray'.Ray.center) /. (Vect.y ray'.Ray.target) in
	let ty1 = (maxs.(1) -. Vect.py ray'.Ray.center) /. (Vect.y ray'.Ray.target) in
	let tz0 = (mins.(2) -. Vect.pz ray'.Ray.center) /. (Vect.z ray'.Ray.target) in
	let tz1 = (maxs.(2) -. Vect.pz ray'.Ray.center) /. (Vect.z ray'.Ray.target) in
	(* List.iter (fun f -> print_float f; print_newline ()) [tx0; ty0; tz0; tx1; ty1; tz1]; *)
	if List.fold_left max tx0 [ty0; tz0] <
	   List.fold_left min tx1 [ty1; tz1] then
		proc_subtree tx0 ty0 tz0 tx1 ty1 tz1 root mask
	else
		[]
(* 
let sphere1 = Object.Sphere_ (Sphere.create (Vect.point_c [10.; 5.; 5.]) 1. (Vect.vector_c [1.;1.;0.]) 0.1 0.9 0.);;
let sphere2 = Object.Sphere_ (Sphere.create (Vect.point_c [12.; 5.; -5.]) 1. (Vect.vector_c [1.;1.;0.]) 0.2 0.8 0.);;
let sphere3 = Object.Sphere_ (Sphere.create (Vect.point_c [-10.; -5.; 5.]) 1. (Vect.vector_c [1.;1.;0.]) 0.3 0.9 0.);;
let sphere4 = Object.Sphere_ (Sphere.create (Vect.point_c [30.; 7.; -5.]) 1. (Vect.vector_c [1.;1.;0.]) 0.4 0.8 0.);;
let sphere5 = Object.Sphere_ (Sphere.create (Vect.point_c [-3.; 4.; 5.]) 1. (Vect.vector_c [1.;1.;0.]) 0.5 0.9 0.);;
let sphere6 = Object.Sphere_ (Sphere.create (Vect.point_c [12.; 10.; -5.]) 1. (Vect.vector_c [1.;1.;0.]) 0.6 0.8 0.);;
let plane1 = Object.Plane_ (Plane.create (Vect.point_c [10.;0.;0.]) 
										 (Vect.vector_c [1.;0.;0.]) 
										 (Vect.vector_c [1.;1.;0.])
										 0.7 0.9 0. );;

let oct = create [sphere1; sphere2; sphere3; sphere4; sphere5; sphere6; plane1];; (* should have 1 node*)
print_octree oct;;
let ray = Ray.create_ray (Vect.point_c [0.; 0.; 0.]) (Vect.vector_c [10.; 5.; 5.]);;

let objs = get_objects oct ray;;
List.map (fun obj -> print_float (Object.ambient obj); print_newline ()) objs;;
 *)
(* initialized by main *)
(* let oct = ref (Octree (EmptyNode, Vect.point_c [0.;0.;0.], 0.));; *)




