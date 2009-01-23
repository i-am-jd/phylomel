open Printf
open ExtLib

(** Phylogenetic trees with clustering methods (UPGMA, WPGMA) *)

(**
   We use the data types provided by the [Unrooted] module.
*)

type tree = Dendogram.t
type leaves = Dendogram.leaf list

(** [leavesOfTree tree]
	@return the leaves of the tree in a list,
	sorted to make sure that leaves belonging to the same node are
	contiguous in the tree. *)
val leaves_of_tree : tree -> leaves

(** [fillTreePos tree]
	fills the vertical positions of each node in the tree in order
	to prepare its rendering. More information in the implementation.
	Beware, this unsafe function modifies the tree in place.
*)
val fill_tree_pos : tree -> leaves

(** [mkTree distance_matrix genotype_size]
	@return an unrooted tree built with the UPGMA method,
	         and the list of the leaves. *)
val mk_upgma : (float array array) -> int -> tree * leaves

(** [minHomology tree]
	@return the minimum homology in the tree
	(allowing us to tune the graphical output) *)
val min_homology : tree -> float

(** A transform function allows us to transform 2D coordinates *)
type transform = float * float -> float * float

(** 
	The [link_info] type allows us to specify links on the genotypes of the
	graphical output.

	 - [string array] : hypertext links
	 - [string] : target field in each hypertext link *)
type link_info = string array * string

(**
   [writeGenos out transform genotypes leaves link_info]
   Writes the genotypes on an svg output
   (using higher order IO from the extlib library)
 *)
val write_genos : 'a IO.output -> transform -> Genotype.t array -> leaves -> link_info option -> unit

(**
   [header width height]
   @return an Svg header
*)
val header : int -> int -> string

(**
   [writeSvgFile genotypes link_info tree leaves filename]
   prints the tree as an svg picture in [filename]
*)
val write_svg_file : Genotype.t array -> link_info option -> tree -> leaves -> string -> int * int
