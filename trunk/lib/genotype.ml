open ExtLib

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
