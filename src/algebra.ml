(* TODO: doc, assumtpions (commutative etc) *)

module type Basic_S = sig
  type t

  val equal : t -> t -> bool

  val to_string : t -> string
end

module type Mul_Monoid_S = sig
  type t

  include Basic_S with type t := t

  val one : t

  val mul : t -> t -> t
end

module type Add_Monoid_S = sig
  type t

  include Basic_S with type t := t

  val zero : t

  val add : t -> t -> t
end

module type Add_Group_S = sig
  type t

  include Basic_S with type t := t

  include Add_Monoid_S with type t := t

  val neg : t -> t

  val sub : t -> t -> t
end

module type Semiring_S = sig
  type t

  include Basic_S with type t := t

  include Mul_Monoid_S with type t := t

  include Add_Monoid_S with type t := t
end

module type Ring_S = sig
  type t

  include Basic_S with type t := t

  include Mul_Monoid_S with type t := t

  include Add_Group_S with type t := t
end

module type Field_S = sig
  type t

  include Basic_S with type t := t

  include Ring_S with type t := t

  val inv : t -> t

  val div : t -> t -> t
end
