open Sdl
open Video
open Event
open Glcaml
open Printf

open Utils

open Phylomel
open Genotypes

open Tree
open Phylogram
open BarnesHut

open Vec2

type state = {
	mutable height : float;
	mutable dx : float;
	mutable dy : float;
	mutable elapsed_time : int;
	mutable last_time    : int;
	spring_factor : float;
	repulse_factor : float;
	bs : BarnesHut.body array;
	fs : Vec2.t array;
	fig : Phylogram.tree_figure;
	n : int;
}

type loop_state = Continue | InPause
type event = Pause | Up | Down | Move of float * float | QuitE | Else

let (+|) = Vec2.add
let (-|) = Vec2.sub

let switch_loop_state = function
	| Continue -> InPause
	| InPause -> Continue

let draw_bodies state =
	for i=0 to state.n - 1 do
		let p = state.bs.(i).p in
		draw_circle p.x p.y 0.15
	done

let draw_springs state =
	for i=1 to state.n - 1 do
		let j = state.fig.tree.parents.(i) in
		let p = state.bs.(i).p in
		let p' = state.bs.(j).p in
		draw_line p p'
	done

(* The main drawing function. *)
let draw_state state =
	glClear (gl_color_buffer_bit lor gl_depth_buffer_bit); 
	glLoadIdentity ();
	glTranslatef state.dx state.dy state.height;
	draw_bodies state;
	draw_springs state
	(*let ps = Array.map (fun p -> p.c) state.ps in
	let area = BarnesHut.calcArea ps in
	let tree = BarnesHut.buildTree area ps in
	BarnesHut.drawTree tree;
	BarnesHut.drawArea area*)

let redraw_state state =
	draw_state state;
	SDLGL.swap_buffers ()

(*
let averageSpeed ps =
	let v = ref 0. in
	let n = Array.length ps in
	for i=0 to n - 1 do
		let p = Array.unsafe_get ps i in
		v := !v +. Vec2.norm p.v
	done;
	!v /. float_of_int(n)

let eq = ref false*)

let update_state_start state =
	let delta = 0.5 in (*0.005*)
	let time = Timer.get_ticks () in
	let real_delta = time - state.last_time in
	if real_delta > 0 then ( (*5*)
		state.last_time <- time;
		state.elapsed_time <- state.elapsed_time + 5;
		ForceDirectedLayout.do_calc_forces_n2 state.fs state.bs state.fig;
		for i=0 to state.n - 1 do
			let b = state.bs.(i) in
			let f = state.fs.(i) in
			b.p.x <-
				b.p.x +. delta *. b.v.x +. 1./.2. *. delta *. delta *. f.x;
			b.p.y <-
				b.p.y +. delta *. b.v.y +. 1./.2. *. delta *. delta *. f.y;
			b.v.x <- b.v.x +. delta *. f.x;
			b.v.y <- b.v.y +. delta *. f.y;
			f.x <- 0.;
			f.y <- 0.
		done
	)

let update_state state =
	let delta = 0.05 in (*0.005*)
	let time = Timer.get_ticks () in
	let real_delta = time - state.last_time in
	if real_delta > 0 then ( (*5*)
		state.last_time <- time;
		state.elapsed_time <- state.elapsed_time + 5;
	(* ForceDirectedLayout.do_calc_forces state.fs state.bs state.fig; *)
		ForceDirectedLayout.do_calc_forces state.fs state.bs state.fig;
		for i=0 to state.n - 1 do
			let b = state.bs.(i) in
			let f = state.fs.(i) in
			b.p.x <-
				b.p.x +. delta *. b.v.x +. 1./.2. *. delta *. delta *. f.x;
			b.p.y <-
				b.p.y +. delta *. b.v.y +. 1./.2. *. delta *. delta *. f.y;
			b.v.x <- b.v.x +. delta *. f.x;
			b.v.y <- b.v.y +. delta *. f.y;
			f.x <- 0.;
			f.y <- 0.
		done
	)
	
	(*let average = averageSpeed state.ps in
	if (state.elapsed_time > 100) && (not !eq) && (average < 0.05) then (
		printf "equilibrium at %d\n" (state.elapsed_time/5);
		(*exit 0;*)
		eq := true
	)*)

let get_event () =
	match poll_event () with
		| Key k when (k.sym = K_SPACE && k.keystate = PRESSED) -> Pause
		| Key k when (k.sym = K_Q && k.keystate = PRESSED) -> QuitE
		| Button b -> (
			match b.mousebutton with
				| WHEELUP -> Up
				| WHEELDOWN -> Down
				| _ -> Else
			)
		| Motion m when m.mousestate = PRESSED ->
			let dx = float_of_int(m.mxrel) /. 40. in
			let dy = float_of_int(m.myrel) /. 40. in
			Move (dx, dy)
		| Quit -> QuitE
		| _ -> Else

let rec loop state =
	redraw_state state;
	let rec loop' last_loop_state =
		(match last_loop_state with
			| Continue ->
				update_state state;
				redraw_state state
			| _ -> ()
		);
		match get_event () with
			| Else ->
				loop' last_loop_state
			| Pause ->
				loop' (switch_loop_state last_loop_state)
			| (Up | Down) as x ->
				let op = if x = Up then (+.) else (-.) in
				state.height <- op state.height 0.3;
				redraw_state state;
				loop' last_loop_state
			| Move (dx,dy) ->
				state.dx <- state.dx +. dx;
				state.dy <- state.dy -. dy;
				redraw_state state;
				loop' last_loop_state
			| QuitE ->
				()
	in loop' InPause

let readArgs () =
	let file = ref "data/geno" in
	let spring_factor = ref 30. in
	let repulse_factor = ref 1. in
	let set_spring_factor x = spring_factor := x in
	let set_repulse_factor x = repulse_factor := x in
	let set_file f = file := f in
	let speclist =
		[("-s", Arg.Float set_spring_factor,  "spring factor (default 30)");
		 ("-r", Arg.Float set_repulse_factor, "repulse factor (default 1)")] in
	let usage_msg = "usage : genoballs -s [spring factor] [file]" in
	Arg.parse speclist set_file usage_msg;
	!file, !spring_factor, !repulse_factor

(* let read_args () = *)
(* 	let file = ref "data/geno" in *)
(* 	let spring_factor = ref 30. in *)
(* 	let repulse_factor = ref 1. in *)
(* 	let setSpringFactor x = spring_factor := x in *)
(* 	let setRepulseFactor x = repulse_factor := x in *)
(* 	let setFile f = file := f in *)
(* 	let speclist = *)
(* 		[("-s", Arg.Float setSpringFactor,  "spring factor (default 30)"); *)
(* 		 ("-r", Arg.Float setRepulseFactor, "repulse factor (default 1)")] in *)
(* 	let usage_msg = "usage : genoballs -s [spring factor] [file]" in *)
(* 	Arg.parse speclist setFile usage_msg; *)
(* 	!file, !spring_factor, !repulse_factor *)

let main () =
	init [VIDEO];
	let w = 1280 and h = 1024 and bpp = 32 in
	let _ = set_video_mode w h bpp [OPENGL;HWSURFACE;DOUBLEBUF;HWACCEL] in
	Window.set_caption "Phylogram" "Phylogram";
	init_gl w h;
	(*show_cursor false;*)
	SDLGL.swap_buffers ();

	let file, spring_factor, repulse_factor = readArgs () in
	let genos = Genotypes.read_file file in
	let dist_mat = GenoMat.create genos in
	let tree = Tree.prim_complete genos.geno_size dist_mat in
	let fig = Phylogram.radial_layout 800. tree in

	let n = Phylogram.size fig in
	let fs = Array.init n (fun _ -> Vec2.null ()) in
	let bs = Array.map ForceDirectedLayout.body_of_pos fig.ps in

	(*experimental*)
	(*Array.iter (fun b -> b.v.x <- rand_float 5.;
					     b.v.y <- rand_float 5.) bs;*)

	let state = {
		height = -35.60;
		dx = 0.;
		dy = 0.;
		elapsed_time = 0;
		last_time   = Timer.get_ticks();
		spring_factor = 30.;
		repulse_factor = 1.;
		bs = bs;
		fs = fs;
		fig = fig;
		n = n;
	} in

	(* for i=0 to 100 do *)
	(* 	update_state_start state; *)
	(* 	redraw_state state *)
	(* done; *)

	loop state;
	quit ()

(* Program entry point *)
let _ = 
	try
		main ()
	with
		SDL_failure m -> failwith m

(*
	let xs = Phylogram.crop 40. 40. xs in
	printf "width : %f, height %f\n" (Phylogram.width xs) (Phylogram.height xs);
	let x,y = Phylogram.bottomLeftCorner xs in
	printf "bottom left corner : %f,%f\n" x y;*)

	(*let xs = Phylogram.crop 800. 600. xs in*)
