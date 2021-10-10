module type S = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool
end
