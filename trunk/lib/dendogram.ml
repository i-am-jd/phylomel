(**
   A dendogram is an unrooted tree for visual classification of similarity.
   Here we define its type and common basic functions.
*)

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
	  (** Position of the node along the y-axis in the dendogram *)
}

and t =
	| Leaf of leaf
	| Node of node

(*TREE, NODES AND LEAVES*)

(** [height tree]
	@return height of the tree *)
let height = function
	| Leaf(l) -> 0
	| Node(n) -> n.height

(** [leavesNb tree]
	@return Number of leaves in a tree *)
let leavesNb = function
	| Leaf(l) -> 1
	| Node(n) -> n.leaves_nb

(** [pos tree]
	@return y position of a node *)
let getPos = function
	| Leaf(l) -> float_of_int l.index
	| Node(n) -> n.pos

(** [getHomology tree]
	@return homology percentage between the subtrees of a node,
            or 100. for a leaf *)
let getHomology = function
	| Leaf(l) -> 100.0
	| Node(n) -> n.homology

(** [homology diff geno_size]
    @return homology between two genotypes of length [geno_size] with [diff]
           differences  *)
let homology diff geno_size =
	let size = float_of_int geno_size in
	(size -. diff) /. size *. 100.0

(** [mkLeaf value]
    @return a new leaf mapping to the [genotype_array.(value)] *)
let mkLeaf value =
	let l = { value = value; index = 0; } in
	Leaf(l)
	
(** [mkNode t1 t2 h]
	@return a new node of subtrees t1,t2 with a homology percentage of h*)
let mkNode t1 t2 homology =
	{ t1 = t1;
	  t2 = t2;
	  height = (max (height t1) (height t2) ) + 1;
	  leaves_nb = (leavesNb t1) + (leavesNb t2);
	  homology = homology;
	  pos = 0.0 }
