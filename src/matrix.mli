module Make (Literal : Literal.S) : sig
  type 'a m

  module Make_SR (K : Algebra.Semiring_S) : sig
    (** We make sure the diagonal is never empty on the support of a matrix *)
    type t = K.t m

    val zero : Literal.t list -> t

    val for_all : (Literal.t -> Literal.t -> 'a -> bool) -> 'a m -> bool

    val is_zero : t -> bool

    val one : Literal.t list -> t

    val diagonal : 'a Vector.Make(Literal).v -> 'a m

    val get_support : 'a m -> Literal.t list

    val get_entry : Literal.t -> Literal.t -> t -> K.t

    val set_entry : Literal.t -> Literal.t -> K.t -> t -> t

    val singleton : Literal.t -> Literal.t -> K.t -> t

    val remove : Literal.t -> Literal.t -> 'a m -> 'a m

    val merge :
      (Literal.t -> Literal.t -> 'a option -> 'b option -> K.t option) ->
      'a m ->
      'b m ->
      t

    val union :
      (Literal.t -> Literal.t -> K.t -> K.t -> K.t option) -> t -> t -> t

    val equal : t -> t -> bool

    val neq : t -> t -> bool

    val iter : (Literal.t -> Literal.t -> 'a -> unit) -> 'a m -> unit

    val fold : (Literal.t -> Literal.t -> 'a -> 'b -> 'b) -> 'a m -> 'b -> 'b

    val filter : (Literal.t -> Literal.t -> 'a -> bool) -> 'a m -> 'a m

    val bindings : 'a m -> ((Literal.t * Literal.t) * 'a) list

    val map : ('a -> K.t) -> 'a m -> t

    val mapi : (Literal.t -> Literal.t -> 'a -> K.t) -> 'a m -> t

    val get_line : Literal.t -> t -> K.t Vector.Make(Literal).v

    val get_column : Literal.t -> t -> K.t Vector.Make(Literal).v

    val add : t -> t -> t

    val mul : t -> t -> t

    val pow : t -> int -> t

    val mul_vect : t -> K.t Vector.Make(Literal).v -> K.t Vector.Make(Literal).v

    val is_nilpotent : t -> bool

    val to_string : t -> string

    module Infix : sig
      val ( + ) : t -> t -> t

      val ( * ) : t -> t -> t

      val ( *^ ) : t -> int -> t

      val ( *> ) : t -> K.t Vector.Make(Literal).v -> K.t Vector.Make(Literal).v

      val ( = ) : t -> t -> bool

      val ( <> ) : t -> t -> bool
    end
  end

  module Make_R (K : Algebra.Ring_S) : sig
    include module type of Make_SR (K)

    val neg : t -> t

    val sub : t -> t -> t

    module Infix : sig
      include module type of Infix

      val ( ~- ) : t -> t

      val ( - ) : t -> t -> t
    end
  end

  (** Create semiring on matrices from a semiring of values and a finite support *)
  val make_semiring :
    (module Algebra.Semiring_S with type t = 'a) ->
    Literal.t list ->
    (module Algebra.Semiring_S with type t = 'a m)

  (** Create ring on matrices from a ring of values and a finite support *)
  val make_ring :
    (module Algebra.Ring_S with type t = 'a) ->
    Literal.t list ->
    (module Algebra.Ring_S with type t = 'a m)
end
