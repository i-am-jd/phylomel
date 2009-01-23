open Phylomel
open Printf

let writeSvg geno_file svg_file =
    let genos, geno_size = Genotype.read_file geno_file in
    let mat = GenoMat.createF genos in
    let tree, leaves = Clustering.mkUpgma mat geno_size in
    ignore (Clustering.writeSvgFile genos None tree leaves svg_file)

let () =
    if (Array.length Sys.argv) < 3 then
		printf "usage : geno2svg input_file output_file\n"
    else
		writeSvg Sys.argv.(1) Sys.argv.(2)
