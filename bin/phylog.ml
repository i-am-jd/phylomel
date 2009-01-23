open Phylomel
open Printf
open Phylogram

let () =
	if (Array.length Sys.argv) < 3 then
		printf "usage : geno2svg input_file output_file\n"
	else
		let geno_file = Sys.argv.(1) in
		let svg_file = Sys.argv.(2) in

		(* We create four things :
		     - genotypes
		     - distance matrix
		     - minimum spanning tree
		     - figure (graphical tree) *)

		let genos, gsize = Genotype.read_file geno_file in
		let dist_mat = GenoMat.create genos in
		let tree = Tree.prim_complete gsize dist_mat in
		let fig = Phylogram.radial_layout 800. tree in

		Phylogram.write_svg_file fig svg_file

