open Printf
open ExtLib

let printLine line =
	DynArray.iter (printf "%3.2f ") line;
	printf "\n"
		
let print mat =
	DynArray.iter printLine mat
			
let get mat i j =
	DynArray.get (DynArray.get mat i) j

let unsafeGet mat i j =
	DynArray.unsafe_get (DynArray.unsafe_get mat i) j
		
let remCol mat j =
	for i = j+1 to (DynArray.length mat) - 1 do
		DynArray.delete (DynArray.get mat i) j
	done

let ofMat mat =
	let dyn_mat = DynArray.create () in
	Array.iter (fun x -> DynArray.add dyn_mat (DynArray.of_array x)) mat;
	dyn_mat

(*Retrieves the difference beetween nodes of index i and j in a dynamic matrix*)
let getDiff m i j =
	if i = j then 0.0 else
		if i > j then get m i j
		else		  get m j i
