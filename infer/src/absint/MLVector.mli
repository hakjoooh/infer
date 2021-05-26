type t = int list

val pp : Format.formatter -> t -> unit

val concat: t -> t -> t
val equal: t -> t -> bool

val vector : int list -> t
val to_list : t -> int list

module Set : Caml.Set.S with type elt = t
module Map : Caml.Map.S with type key = t
