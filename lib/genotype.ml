open Printf
open ExtLib

type t = {
    id : string;
    info : string;
    tab : float array;
}

exception Wrong_size

type collection = t array * int

exception Bad_collection
exception Bad_file

let (|>) x f = f x

let size g =
    Array.length g.tab
	
let diff g1 g2 =
    let count = ref 0 in
    try
	Array.iter2 (fun x y -> if x <> y then incr count) g1.tab g2.tab;
	!count
    with Invalid_argument(_) -> raise Wrong_size
	
let print g =
    printf "%s = " g.info;
    Array.iter (printf "-%3.1f ") g.tab;
    printf "\n"

let check_genos_size gtab =
    match gtab with
	| [||] -> raise Bad_collection
	| _ ->
	      let first_size = size gtab.(0) in
	      let hasWrongSize g = (size g) <> first_size in
	      let consistent = not (Array.exists hasWrongSize gtab) in
	      if consistent then first_size
			else raise Bad_collection
			    
let split_and_map s sep f =
    String.nsplit s sep
	|> Array.of_list
	|> Array.map f
		  
let parse_line line =
    let parse_tab str =
		split_and_map str "," float_of_string in
    let fields = String.nsplit line ";" in
    match fields with
	| [id;info;tab_str] ->
	      let tab = parse_tab tab_str in
	      { id = id; info = info; tab = tab }
	| _ ->
	      raise Bad_file

let is_comment = function
    | "" -> false
    | s -> s.[0] = '#'

let notComment s = not (is_comment s)

let read_file file_name =
    let chan = open_in file_name in
    let gtab = chan
	|> Std.input_lines
	|> Enum.filter notComment
	|> Array.of_enum
	|> Array.map parse_line in
    try
		let gsize = check_genos_size gtab in
		(gtab, gsize)
	with Bad_collection ->
		raise Bad_file
