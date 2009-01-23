open Vec2
open Printf
open Tree

type tree_figure = {
	ps : Vec2.t array; (* points *)
	w : float; (* width  *)
	h : float; (* height *)
	tree : Tree.t
}

let pi = 4. *. atan 1.

let size tree_fig =
	Array.length tree_fig.ps

(* Operations on points array *)

let width = function
	| [||] -> invalid_arg "width : empty array"
	| xs ->
		  let n = Array.length xs in
		  let min_x = ref xs.(0).x in
		  let max_x = ref xs.(0).x in
		  for i=1 to n-1 do
			  if xs.(i).x < !min_x then
				  min_x := xs.(i).x;
			  if xs.(i).x > !max_x then
				  max_x := xs.(i).x
		  done;
		  !max_x -. !min_x

let height = function
	| [||] -> invalid_arg "width : empty array"
	| xs ->
		  let n = Array.length xs in
		  let min_y = ref xs.(0).y in
		  let max_y = ref xs.(0).y in
		  for i=1 to n-1 do
			  if xs.(i).y < !min_y then
				  min_y := xs.(i).y;
			  if xs.(i).y > !max_y then
				  max_y := xs.(i).y
		  done;
		  !max_y -. !min_y

let bottom_left_corner = function
	| [||] -> invalid_arg "bottom_left_corner : empty array"
	| xs ->
		  let n = Array.length xs in
		  let minx = ref xs.(0).x in
		  let miny = ref xs.(0).y in
		  for i=1 to n-1 do
			  if xs.(i).x < !minx then
				  minx := xs.(i).x;
			  if xs.(i).y < !miny then
				  miny := xs.(i).y
		  done;
		  !minx,!miny

let dim xs =
	width xs, height xs

let unsafe_reframe (x0,y0) xs =
	let minx,miny = bottom_left_corner xs in
	Array.iter
		(fun p ->
			 p.x <- p.x -. minx +. x0;
			 p.y <- p.y -. miny +. y0)
		xs

let unsafe_crop_width w' xs =
	let x0,y0 = bottom_left_corner xs in
	let w = width xs in
	let c = w' /. w in
	Array.iter
		(fun p ->
			 p.x <- c *. (p.x -. x0) +. x0;
			 p.y <- c *. (p.y -. y0) +. y0)
		xs

(* Creates a tree figure *)

let create_figure points w h tree =
	{ ps = points;
	  w = w;
	  h = h;
	  tree = tree }

let radial_layout ?(margin= (10.,10.)) w tree =
	let n = Tree.size tree in
	let foi = float_of_int in

	(* number of leaves in the subtree of i *)
	let ls = Tree.leaves_nb tree in

	(* layout : array of coordinates *)
	let xs = Array.init n (fun _ -> Vec2.null()) in 

	(* wedge sizes *)
	let ws = Array.create n (2.*.pi) in

	(* angles of right wedge borders *)
	let bs = Array.create n 0. in             

	let rec iter p i =

		if i <> 0 then (
			let d = DistMat.get tree.Tree.dist_mat i p in
			let angle = bs.(i) +. ws.(i) /. 2. in
			(*let dx = foi(d) *| unit_vec angle in
			let x' = xs.(p) +| dx in*)
			let x = xs.(p) in
			let x' = { x = x.x +. foi(d) *. cos(angle);
					   y = x.y +. foi(d) *. sin(angle) } in
			xs.(i) <- x'
		);

		let b = ref bs.(i) in
		List.iter
			(fun j ->
				 ws.(j) <- foi(ls.(j)) /. foi(ls.(0)) *. 2.*.pi;
				 bs.(j) <- !b;
				 b := !b +. ws.(j);
				 iter i j)
			tree.children.(i) in

	iter 0 0;

	(* Reframe *)
	
	let x0,y0 = margin in

	(* unsafe_reframe margin xs;
	unsafe_crop_width (w-.2.*.x0) xs;*)
	
	let h = height xs +. 2. *. y0 in
	create_figure xs w h tree


(*
let reframe ?(margin=0.,0.) xs =
	let x0, y0 = margin in
	let minx,miny = bottom_left_corner xs in
	Array.map
		(fun {x=x;y=y} ->
			 { x = x -. minx +. x0;
			   y = y -. miny +. y0 })
		xs

let crop_height h' xs =
	let x0,y0 = bottom_left_corner xs in
	let h = height xs in
	let c = h' /. h in
	Array.map
		(fun {x=x;y=y} ->
			 { x = c *. (x -. x0) +. x0;
			   y = c *. (y -. y0) +. y0 })
		xs

let crop w' h' xs =
	let w,h = dim xs in
	if w/.h > w'/.h' then
		crop_width w' xs
	else
		crop_height h' xs
*)

let print_points ps =
	Array.iter (fun {x=x; y=y} -> printf "%f,%f " x y) ps;
	printf "\n"

(*let writeLines fig parents dmat =*)

let put_points out fig =
	let printPoint {x=x; y=y} =
		Svg.putCircle out
			~fill:"lightsteelblue"
			~stroke:"midnightblue"
			~width:1.
			3. (x, y) in
	Array.iter printPoint fig.ps

let put_lines out fig =
	let ps = fig.ps in
	let tree = fig.tree in

	let print_line i m =
		if i <> 0 then
			let p = tree.parents.(i) in
			let d = DistMat.get tree.dist_mat p i in
			if d < 3 then (
				let w = if d=1 then 2. else 1. in
				Svg.putLine out
					~width:w
					(m.x, m.y)
					(ps.(p).x, ps.(p).y)
			) else (
				Svg.putDottedLine out
					~width:1.
					~color:"grey"
					(m.x, m.y)
					(ps.(p).x, ps.(p).y);
				Svg.putText out
					~anchor:"middle"
					~fill:"black"
					(string_of_int d)
					( (m.x +. ps.(p).x) /. 2.,
				      (m.y +. ps.(p).y) /. 2. +. 4.)) in

	Array.iteri print_line ps

let write_svg out fig =
	(*let x0, y0 = margin in
	let fig = reframe ~margin:margin fig in
	let fig = crop_width (float_of_int(w) -. 2.*.x0) fig in*)

	let w = int_of_float fig.w in
	let h = int_of_float fig.h in
	Svg.put out (Svg.header w h);

	put_lines out fig;
	put_points out fig;

	Svg.close out
	
let write_svg_file fig file =
	let out = IO.output_channel (open_out file) in
	write_svg out fig


(*let test =
	let t = {
		children = [|[1;2;3];[];[];[]|];
		ls = [|3;1;1;1|]
	} in
	let dmat = [|[||];[|1;|];[|1;1|];[|1;1;1|]|] in
	radial_layout t dmat*)

(*let write_svg_file (?border = 10) tree dmat file =
	let xs = radial_layout tree dmat in*)
	
(*

  I have to put this in Unit tests.

# fig;;
- : Phylomel.Vec2.t array = [|{x = 1.; y = 1.}; {x = 4.; y = 2.}|]
# crop 1. 1. fig;;
- : Phylomel.Vec2.t array =
[|{x = 1.; y = 1.}; {x = 2.; y = 1.33333333333333326}|]
*)

(*
# let fig = [|make 1. 2.; make 1. 1.; make 2. 1.|];;
val fig : Phylomel.Vec2.t array =
  [|{x = 1.; y = 2.}; {x = 1.; y = 1.}; {x = 2.; y = 1.}|]
# reframe fig;;
- : Phylomel.Vec2.t array =
[|{x = 0.; y = 1.}; {x = 0.; y = 0.}; {x = 1.; y = 0.}|]
# 
*)

(*
# let fig = [|make 1. 2.; make 1. 1.; make 2. 1.; make (-.1.) (-.1.)|];;
val fig : Phylomel.Vec2.t array =
  [|{x = 1.; y = 2.}; {x = 1.; y = 1.}; {x = 2.; y = 1.};
    {x = -1.; y = -1.}|]
# reframe fig;;
- : Phylomel.Vec2.t array =
[|{x = 2.; y = 3.}; {x = 2.; y = 2.}; {x = 3.; y = 2.}; {x = 0.; y = 0.}|]
# *)

(*
let fig = [|make 1. 2.; make 1. 1.; make 2. 1.|];;
val fig : Phylomel.Vec2.t array =
  [|{x = 1.; y = 2.}; {x = 1.; y = 1.}; {x = 2.; y = 1.}|]
# crop 2. 2. fig;;
- : Phylomel.Vec2.t array =
[|{x = 1.; y = 3.}; {x = 1.; y = 1.}; {x = 3.; y = 1.}|]
*)
