open ExtArray

type t = {
    id : string;
    markers : float array;
	infos : string array;
}

let size g = Array.length g.markers

let diff g1 g2 =
    let count = ref 0 in
    try
		Array.iter2 (fun x y -> if x <> y then incr count)
			g1.markers g2.markers;
		!count
    with Invalid_argument(_) -> invalid_arg "Genotypes.diff"

let markers_nb = function
	| [||] -> None
	| genos ->
	      let first_size = size genos.(0) in
	      let wrong_size g = (size g) <> first_size in
	      if Array.exists wrong_size genos then
			  None
		  else Some first_size

let description g =
	let b = Buffer.create 30 in
	let concat s =
		Buffer.add_char b ' ';
		Buffer.add_string b s in
	concat g.id;
	Array.iter concat g.infos;
	Buffer.contents b

let compare_markers g1 g2 =
	let compare (x:float) (y:float) = compare x y in
	let n1 = Array.length g1.markers in
	let n2 = Array.length g2.markers in
	if n1 <> n2 then
		invalid_arg "Genotype.compare_markers : different genotype sizes"
	else
		let rec loop i =
			if i=n1-1 then compare g1.markers.(i) g2.markers.(i)
			else
				let c = compare g1.markers.(i) g2.markers.(i) in
				if c <> 0 then c
				else loop (i+1)
		in loop 0

let equal_markers g1 g2 =
	try compare_markers g1 g2 = 0
	with Invalid_argument
	     "Genotype.compare_markers : different genotype sizes" ->
			 invalid_arg
				 "Genotype.equal_markers : different genotype sizes"

type genotype = t
module Geno = struct
	type t = genotype
	let equal = equal_markers
	let hash g = Hashtbl.hash g.markers
end

module GenoHash =
	Hashtbl.Make(Geno)

module GenoSet = Set.Make(struct
	type t = genotype
	let compare = compare_markers
end)
