(**Genotype manipulation : collections, printing, parsing*)

type t = {
	id : string;
	info : string;
	tab : float array; (** Markers array *)
}

exception Wrong_size

(** A genotype collection is a pair consisting of an array of genotypes
	and their length *)
type collection = t array * int

exception Bad_collection
exception Bad_file

(** @return size of a genotype *)
val size : t -> int

(** @return number of different markers between 2 genotypes *)
val diff : t -> t -> int

(** [print g] prints [g] on the standard output *)
val print : t -> unit

(** [checkGenosSize genos]
	@raise Bad_collection if the genotypes are not
	all of the same length
    @return size of the genotypes in [genos]*)
val check_genos_size : t array -> int

(** @return a genotype read from a string *)
val parse_line : string -> t

(** [readFile file_name]
	@return a genotypes collection read from [file_name]
	@raise Bad_file if the file is invalid
*)

(*
	A valid file would be : \\
	g0;france;1,2,3.5,9 \\
	g1;niger,2,5,6,3
*)
val read_file : string -> collection
