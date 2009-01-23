(** Genetic distance matrices *)

(** @return a distance matrix inferred from an array of genotypes *)
val create : Genotype.t array -> int array array

(** [get matrix i j]
	@return genetic distance between the genotypes of index [i] and [j] *)
val get : int array array -> int -> int -> int

(** [empty dim]
	@return a distance matrix of size [dim] filled with 0s *)
val empty : int -> int array array

(** [createF genos]
	@return same matrix as [create genos] but filled with floats *)
val createF : Genotype.t array -> float array array

(** [emptyF dim]
	@return a distance matrix of size [dim] filled with 0.0 *)
val emptyF : int -> float array array

(** [getF matrix i j]
	@return floating genetic distance between the genotypes of index [i] and [j] *)
val getF : float array array -> int -> int -> float

(** [toFloat m] maps [float_of_int] on [m] *)
val toFloat : int array array -> float array array

(** [createP (genos, gsize)]
	@param genos genotype array
	@param gsize size of the genotypes in [genos]
	@return a distance matrix filled with difference percentages *)
val createP : Genotype.t array * int -> float array array

val print : int array array -> unit
val printF : float array array -> unit
