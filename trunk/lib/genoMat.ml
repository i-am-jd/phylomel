open Printf

(*DIFFERENCE INT MATRICES*)

(*Build a matrix of the differences*)
let create genos =
	DistMat.mk_dist Genotype.diff genos

(*Retrieves the differences from a matrix*)
let get m i j =
	if i = j then 0
	else DistMat.get m i j

(*Builds an empty difference matrix*)
let empty length =
	DistMat.create length 0

(*DIFFERENCE FLOAT MATRICES*)

(*Builds a float matrix of the differences*)
let createF genos =
	let n = Array.length genos in
	DistMat.init n (fun i j -> float_of_int (Genotype.diff genos.(i) genos.(j)))

(*Builds an empty difference float matrix*)
let emptyF length =
	DistMat.create length 0.
	
(*Retrieves the number of differences from a difference float matrix*)
let getF m i j =
	if i = j then 0.0
	else DistMat.get m i j

let toFloat m =
	DistMat.map float_of_int m

(*Builds a float matrix of percentages*)
let createP (gtab, gsize) =
	let m =  createF gtab in
	DistMat.map_in_place (fun x -> x *. 100. /. float_of_int(gsize)) m;
	m

(*Prints a matrix*)
let print matrix =
	let printLine line =
		Array.iter (printf "-%3d ") line;
		printf "\n"
	in Array.iter printLine matrix

(*Prints a matrix of floats*)
let printF matrix =
	let length = Array.length matrix in
	for i=0 to length - 1 do
		for j=0 to i-1  do
			printf "%-3.1f " matrix.(i).(j)
		done;
		printf "\n"
	done
