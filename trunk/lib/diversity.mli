(**
   Diversity indices
*)


(** [simpson genos]
	@return a tuple :
	 - number of distinct genotypes
	 - simpson diversity index
*)
val simpson : Genotype.t array -> float
val simpsonConfidenceInterval : Genotype.t array -> float * (float * float)
