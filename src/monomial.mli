(** Monomials are sets of literals with non-negative integer exponents.
    They form a monoid with respect to their multiplication.
 *)

module Make (Literal : Literal.S) : sig
  (** The type of Monomials.
    They are mappings from literals to their non-negative exponent.
    We always ensure that exponents are positive.
 *)
  type t

  (** This exception is raised if a non-positive exponent is found in a monomial. *)
  exception Monomial_has_non_positive_exponent of (Literal.t * int)

  (** This exception is raised when trying to set a negative exponent in a monomial. *)
  exception Monomial_set_negative_exponent of (Literal.t * int)

  (** The unit monomial *)
  val one : t

  (** Check if the given monomial is [one] *)
  val is_one : t -> bool

  (** [of_literal x] returns the monomial [x] *)
  val of_literal : Literal.t -> t

  (** [singleton x n] returns the monomial [xⁿ]
    @raise [Monomial_set_negative_exponent (x,n)] if [n < 0]
 *)
  val singleton : Literal.t -> int -> t

  (** Returns a map from literals to their non-negative exponent *)
  val to_map : t -> int Map.Make(Literal).t

  (** Creates a monomial from a map [m]. 
    @raise [Monomial_set_negative_exponent (x,i)] if [i < 0] and [(x,i)] is a binding in [m]
*)
  val of_map : int Map.Make(Literal).t -> t

  val to_seq : t -> (Literal.t * int) Seq.t

  (** Adds a sequence of bindings [s] to the given monomial. 
    @raise [Monomial_set_negative_exponent (x,i)] if [i < 0] and [(x,i)] is a binding in [s]
*)
  val add_seq : (Literal.t * int) Seq.t -> t -> t

  (** Creates a monomial from a sequence of bindings [s].
    @raise [Monomial_set_negative_exponent (x,i)] if [i < 0] and [(x,i)] is a binding in [s]
*)
  val of_seq : (Literal.t * int) Seq.t -> t

  (** Returns the list of literals occuring in the object, sorted in
    lexicographical order *)
  val get_support : t -> Literal.t list

  (** Comparison bewteen monomials.
    Uses the lexicographical ordering on monomials, with lexicographical ordering 
    on the literals. This order ensures that the unit monomial is the global minimum.
    This order is used in the implementation of polynomials.
 *)
  val compare : t -> t -> int

  (** Equality between monomials *)
  val equal : t -> t -> bool

  (** [deg x m] returns the largest integer [n] such that [xⁿ] divides [m]. In particular,
    it returns 0 is [x] is not present in [m].
 *)
  val deg : Literal.t -> t -> int

  (** Commutative multiplication between two monomials. *)
  val mul : t -> t -> t

  (** [set_exponent] ensures that, for all [n >= 0], 
    [deg x (set_exponent x n m) = n].
    @raise [Monomial_set_negative_exponent(x,n)] if [n < 0]
*)
  val set_exponent : Literal.t -> int -> t -> t

  (** [update x f m] applies [f] to the exponent of [x] in [m].
    @raise [Monomial_set_negative_exponent(x,f (deg x m))] if [f] returns a negative exponent.
 *)
  val update : Literal.t -> (int -> int) -> t -> t

  (** @raise [Monomial_set_negative_exponent(x,f x exp_a exp_b)] if [f] returns a negative exponent. *)
  val union : (Literal.t -> int -> int -> int) -> t -> t -> t

  (** Removes a literal from a monomial. Equivalent to [set_exponent x 0] *)
  val remove : Literal.t -> t -> t

  (** Derivation of [m] wrt [x]
    If [x] is not in the support of [m], returns None (an "empty" monomial).
    Otherwise, returns [Some (i,r)] such that [∂m/∂x = i×r], where [r] is a 
    monomial.
 *)
  val deriv : Literal.t -> t -> (int * t) option

  (** Partial application in a monomial.
    [apply (module K) m v] returns [(c,r)] such that the substitution of [v] in [m] 
    equals [c×r], where [r] is a monomial.
 *)
  val apply :
    (module Algebra.Mul_Monoid_S with type t = 'a) ->
    t ->
    'a Map.Make(Literal).t ->
    'a * t

  (** Conversion to printable strings. 
    @raise [Monomial_has_non_positive_exponent (x,i)] if [i <= 0] and [(x,i)] is 
    a binding in the monomial.
 *)
  val to_string : t -> string

  (** Infix operators *)
  module Infix : sig
    val ( * ) : t -> t -> t

    val ( = ) : t -> t -> bool

    val ( <> ) : t -> t -> bool
  end

  (** Mapping functions *)

  val iter : (Literal.t -> int -> unit) -> t -> unit

  val fold : (Literal.t -> int -> 'b -> 'b) -> t -> 'b -> 'b

  val for_all : (Literal.t -> int -> bool) -> t -> bool

  val exists : (Literal.t -> int -> bool) -> t -> bool

  val filter : (Literal.t -> int -> bool) -> t -> t

  val partition : (Literal.t -> int -> bool) -> t -> t * t

  val cardinal : t -> int

  val bindings : t -> (Literal.t * int) list

  val map : (int -> 'b) -> t -> 'b Map.Make(Literal).t

  val mapi : (Literal.t -> int -> 'b) -> t -> 'b Map.Make(Literal).t
end
