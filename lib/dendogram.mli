(**
 Types and common functions for dendograms *)

(**
   A dendogram is an unrooted tree for visual classification of similarity.
   Here we define its type and common basic functions.

   The homology is the percentage of similarity.
*)

(** {4 Types} *)

type leaf = {
	value : int;         (** Each leaf maps the genotype of index value *)
	mutable index : int; (** Position of the genotype in the dendogram  *)
}

type node = {
	t1 : t;
	t2 : t;
	height : int;
	leaves_nb : int;     (** Number of leaves in the subtree *)
	homology : float;    (** Homology between the t1 and t2  *)
	mutable pos : float; 
	  (** Vertical coordinate of the node in the dendogram *)
}

and t =
	| Leaf of leaf
	| Node of node

(** {5 Constructors} *)

(** [mkLeaf i]
	@return a new leaf pointing to the genotype of index i
*)
val mkLeaf : int -> t

(** [mkNode t1 t2 h]
	@return a new node of subtrees [t1] and [t2],
	h being the homology between [t1] and [t2]
*)
val mkNode : t -> t -> float -> node

(** {4 Accessors} *)

(** [height tree]
	@return the height of a tree (0 for leaves) *)
val height : t -> int

(** [leavesNb tree]
	@return the number of leaves in a tree *)
val leavesNb : t -> int

(** [getPos tree]
	@return the vertical position of a node *)
val getPos : t -> float

(** [getHomology tree]
	@return the homology between the subtrees of a node *)
val getHomology : t -> float

(**[homology geno_size diff]
   @return
   the homology between two genotypes of length [geno_size] with [diff]
   differences  *)
val homology : float -> int -> float
