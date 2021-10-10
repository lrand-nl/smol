module type S = sig
  (** The type of literals, which are abstract representation of values. *)
  type t

  (** Returns the string representation of a literal *)
  val to_string : t -> string

  (** A total ordering on the literals, which is the lexicographical ordering on their names *)
  val compare : t -> t -> int

  (** Equality on the literals. 
    Two literals are equal iff their name is the same.
 *)
  val equal : t -> t -> bool
end
