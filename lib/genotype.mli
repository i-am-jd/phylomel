(** Genotype functions*)

(** This module implements a genotype and its common functions *)

type t = {
    id : string;
    markers : float array;
	infos : string array;
	  (** exemple : [|France; Paris; 2009|] *)
}

(** [size g] @return number of markers *)
val size : t -> int

(** [diff g1 g2] @return number of different markers *)
val diff : t -> t -> int

(** [markers_nb gs]
	@return
	 - [Some size] where size if the common number of markers
	   of the genotypes
	 - [None] if the genotypes have different sizes *)
val markers_nb : t array -> int option

(** [description g] @return the concatenation of [g.id] and [g.infos] *)
val description : t -> string
