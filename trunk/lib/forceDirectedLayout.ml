open ExtLib
open Tree
open Phylogram
open BarnesHut
open Vec2

let drag_coeff = 2.
let spring_coeff = 1.

let body_of_pos p =
	{ p = p;
	  v = Vec2.null () }

let do_calc_drag_force f b =
	f.x <- f.x -. drag_coeff *. b.v.x;
	f.y <- f.y -. drag_coeff *. b.v.y

let do_calc_drag_forces =
	Array.iter2 do_calc_drag_force

let do_calc_spring_forces fs bs parents dmat =
	for i=1 to Array.length parents - 1 do
		let p = parents.(i) in
		let d = float_of_int (DistMat.get dmat p i) in
		let dx = bs.(p).p.x -. bs.(i).p.x in
		let dy = bs.(p).p.y -. bs.(i).p.y in
		let r = sqrt (square dx +. square dy) in
		let factor = spring_coeff *. (r -. d) in
		let fx = factor *. dx in
		let fy = factor *. dy in
		let f = fs.(i) in
		let f' = fs.(p) in
		f.x <- f.x +. fx;
		f.y <- f.y +. fy;
		f'.x <- f'.x -. fx;
		f'.y <- f'.y -. fy
	done

let do_calc_forces fs bs tree_fig =
	(*let dmat = tree_fig.tree.dist_mat in
	let parents = tree_fig.tree.parents in*)
	let tree = BarnesHut.tree_of_bodies (Array.to_list bs) in
	BarnesHut.do_calc_forces fs bs tree;
	do_calc_drag_forces fs bs;
	(*doCalcSpringForces fs bs parents dmat;*)
	()

