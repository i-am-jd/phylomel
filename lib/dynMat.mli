(** Resizable matrices *)
open ExtLib

val printLine : float DynArray.t -> unit
val print : float DynArray.t DynArray.t -> unit
val get : 'a DynArray.t DynArray.t -> int -> int -> 'a
val unsafeGet : 'a DynArray.t DynArray.t -> int -> int -> 'a
val remCol : 'a DynArray.t DynArray.t -> int -> unit
val ofMat : 'a array array -> 'a DynArray.t DynArray.t
val getDiff : float DynArray.t DynArray.t -> int -> int -> float
