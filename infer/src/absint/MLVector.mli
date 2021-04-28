type t = Vector of float list
       | LazyVector of int lazy_t list

val pp : Format.formatter -> t -> unit

(** matrix multiplication of two vectors *)
val mult : t -> t -> float
val concat: t -> t -> t
val equal: t -> t -> bool

val vector : float list -> t
val lazy_vector : int lazy_t list -> t

module Set : Caml.Set.S with type elt = t
