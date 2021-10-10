module Make (Literal : Literal.S) : sig
  (** The type of vectors. Their basis is the set of literals. *)
  type 'a v = private 'a Map.Make(Literal).t

  (** Make a vector with a single entry *)
  val singleton : Literal.t -> 'a -> 'a v

  val of_map : 'a Map.Make(Literal).t -> 'a v

  val to_map : 'a v -> 'a Map.Make(Literal).t

  val to_seq : 'a v -> (Literal.t * 'a) Seq.t

  val add_seq : (Literal.t * 'a) Seq.t -> 'a v -> 'a v

  val of_seq : (Literal.t * 'a) Seq.t -> 'a v

  val mem : Literal.t -> 'a v -> bool

  val get_entry : Literal.t -> 'a v -> 'a option

  (** Adds an entry to the vector. If the literal was already in the basis, updates the value instead. *)
  val add_entry : Literal.t -> 'a -> 'a v -> 'a v

  val remove_entry : Literal.t -> 'a v -> 'a v

  val update_entry : Literal.t -> ('a option -> 'a option) -> 'a v -> 'a v

  val merge :
    (Literal.t -> 'a option -> 'b option -> 'c option) -> 'a v -> 'b v -> 'c v

  val union : (Literal.t -> 'a -> 'a -> 'a option) -> 'a v -> 'a v -> 'a v

  val iter : (Literal.t -> 'a -> unit) -> 'a v -> unit

  val fold : (Literal.t -> 'a -> 'b -> 'b) -> 'a v -> 'b -> 'b

  val for_all : (Literal.t -> 'a -> bool) -> 'a v -> bool

  val exists : (Literal.t -> 'a -> bool) -> 'a v -> bool

  val filter : (Literal.t -> 'a -> bool) -> 'a v -> 'a v

  val partition : (Literal.t -> 'a -> bool) -> 'a v -> 'a v * 'a v

  val cardinal : 'a v -> int

  val bindings : 'a v -> (Literal.t * 'a) list

  val map : ('a -> 'b) -> 'a v -> 'b v

  val mapi : (Literal.t -> 'a -> 'b) -> 'a v -> 'b v

  (** Returns the support of the vector *)
  val get_support : 'a v -> Literal.t list

  (** Define arithmetic operations on vectors with values in a semiring. *)
  module Make_SR (K : Algebra.Semiring_S) : sig
    type t = K.t v

    exception Vector_incompatible of (t * t)

    (** The zero vector for the given basis *)
    val zero : Literal.t list -> t

    val is_zero : t -> bool

    (** Equality of vectors *)
    val equal : t -> t -> bool

    (** Not equal *)
    val neq : t -> t -> bool

    (** Addition 
      @raise [Vector_incompatible (a,b)] if the vectors do not have the same basis.
   *)
    val add : t -> t -> t

    (** Dot product, used for the matrix product.
      @raise [Vector_incompatible (a,b)] if the vectors do not have the same basis.
   *)
    val mul_dot : t -> t -> K.t

    (** Multiplies every entry in a vector with the given scalar *)
    val mul_scalar : K.t -> t -> t

    (** Returns the string representation of a given vector *)
    val to_string : t -> string

    module Infix : sig
      val ( + ) : t -> t -> t

      val ( *. ) : K.t -> t -> t

      val ( ** ) : t -> t -> K.t

      val ( = ) : t -> t -> bool

      val ( <> ) : t -> t -> bool
    end
  end

  (** Define arithmetic operations on vectors with values in a ring. *)
  module Make_R (K : Algebra.Ring_S) : sig
    include module type of Make_SR (K)

    (** Negation *)
    val neg : t -> t

    (** Substraction
      @raise [Vector_incompatible (a,b)] if the vectors do not have the same basis.
   *)
    val sub : t -> t -> t

    module Infix : sig
      include module type of Infix

      val ( ~- ) : t -> t

      val ( - ) : t -> t -> t
    end
  end

  (** Create an additive monoid on vectors from a semiring of values and a finite support *)
  val make_monoid :
    (module Algebra.Semiring_S with type t = 'a) ->
    Literal.t list ->
    (module Algebra.Add_Monoid_S with type t = 'a v)

  (** Create an additive group on vectors from a ring of values and a finite support *)
  val make_group :
    (module Algebra.Ring_S with type t = 'a) ->
    Literal.t list ->
    (module Algebra.Add_Group_S with type t = 'a v)
end
