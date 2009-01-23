open ExtLib

type t = {
	adj_mat : bool DistMat.t;
	dist_mat : int DistMat.t;
	children : int list array;
	  (* children.(i) : children list of the ith vertex *)
	parents : int array;
}

val create : bool DistMat.t -> int DistMat.t -> t

val size : t -> int

val leaves_nb : t -> int array

val prim_clique_adj_mat : int DistMat.t -> int -> bool array array

val prim_clique : int DistMat.t -> int -> t
