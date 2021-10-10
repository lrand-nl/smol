exception Polynomial_is_not_scalar

module Make (Literal : Literal.S) : sig
  (** The type of polynomials.
      They map monomials to their coefficients, which we make sure is not zero.
      The type of polynomials is parametrized by the type of their coefficients.
   *)
  type 'a p

  (** Polynomials with coefficients in a semiring, which forms a semiring *)
  module Make_Semiring (K : Algebra.Semiring_S) : sig
    type t = K.t p

    (** The zero polynomial *)
    val zero : 'a p

    (** Check if the given polynomial is zero *)
    val is_zero : t -> bool

    (** The unit polynomial *)
    val one : t

    val of_scalar : K.t -> t

    (** @raise [Polynomial_is_not_scalar] if the given polynomial is not a scalar *)
    val to_scalar : t -> K.t

    val to_scalar_opt : t -> K.t option

    (** The "variable" polynomial *)
    val of_literal : Literal.t -> t

    (** Builds a polynomial with a single monomial of coefficient [K.one] *)
    val of_monomial : Monomial.Make(Literal).t -> t

    (** Builds a polynomial with a single monomial multiplied by a coefficient *)
    val singleton : Monomial.Make(Literal).t -> K.t -> t

    (** K[x][x] ≡ K[x] *)
    val flatten : t p -> t

    (** Returns the sorted list of variables occuring in the object *)
    val get_support : t -> Literal.t list

    (** Equality between polynomials.
      Assuming monomials only contain positive exponents, 
      and the polynomials do not contain zero coefficients, 
      the polynomial representation is unique,
      so equality is the equality of the coefficients for each of their monomial.
   *)
    val equal : t -> t -> bool

    (** Returns the maximum degree of the given variable in the given polynomial.
      Returns a negative integer if the polynomial is zero.
   *)
    val deg : Literal.t -> t -> int

    (** Adds two polynomials *)
    val add : t -> t -> t

    (** Multiplies a polynomial by a scalar *)
    val mul_scalar : K.t -> t -> t

    (** Polynomial multiplication *)
    val mul : t -> t -> t

    (** Returns the leading coefficient of [p] in the variable [x], with the degree of [x] *)
    val leading_coef : Literal.t -> t -> t * int

    (** Partial application for polynomials.
      [apply_raw A B op v p] substitutes the literals in [v] with their mappings in [p].
      [op] describes a multiplicative operation between [K.t] and [A.t] which results in [B.t],
      it is used to compute the intermediate coefficients of the monomials.
   *)
    val apply_raw :
      (module Algebra.Mul_Monoid_S with type t = 'a) ->
      (module Algebra.Semiring_S with type t = 'b) ->
      (K.t -> 'a -> 'b) ->
      'a Map.Make(Literal).t ->
      t ->
      'b p

    (** Partial application for polynomials.
      [apply A] is equivalent to [apply_raw A A].
   *)
    val apply :
      (module Algebra.Semiring_S with type t = 'a) ->
      (K.t -> 'a -> 'a) ->
      'a Map.Make(Literal).t ->
      t ->
      'a p

    (** Replace literals with values in [K.t]. Equivalent to [apply (module K) K.mul] *)
    val eval : K.t Map.Make(Literal).t -> t -> t

    (** [substitution p x q] replaces variables with polynomials in [p] *)
    val substitution : t Map.Make(Literal).t -> t -> t

    (** Extract the constant value of the given polynomial *)
    val apply_at_0 : t -> K.t

    (** Derivation of [p] wrt [var] *)
    val deriv : Literal.t -> t -> (int * K.t) p

    (** Conversion to printable strings *)
    val to_string : t -> string

    (** Prefix and infix operators *)
    module Infix : sig
      val ( + ) : t -> t -> t

      val ( * ) : t -> t -> t

      val ( *. ) : K.t -> t -> t

      val ( = ) : t -> t -> bool

      val ( <> ) : t -> t -> bool
    end

    (** Mapping functions 
      Note: setting K.zero as a coefficient for a monomial is equivalent to removing its
      binding in the map. *)

    val map : ('a -> K.t) -> 'a p -> t

    val mapi : (Monomial.Make(Literal).t -> 'a -> K.t) -> 'a p -> t

    val merge :
      (Monomial.Make(Literal).t -> 'a option -> 'b option -> K.t) ->
      'a p ->
      'b p ->
      t

    val mem : Monomial.Make(Literal).t -> 'a p -> bool

    val set_coef : Monomial.Make(Literal).t -> K.t -> t -> t

    val update : Monomial.Make(Literal).t -> (K.t -> K.t) -> t -> t

    val remove : Monomial.Make(Literal).t -> 'a p -> 'a p

    val union : (Monomial.Make(Literal).t -> K.t -> K.t -> K.t) -> t -> t -> t

    val compare : ('a -> 'a -> int) -> 'a p -> 'a p -> int

    val iter : (Monomial.Make(Literal).t -> 'a -> unit) -> 'a p -> unit

    val fold : (Monomial.Make(Literal).t -> 'a -> 'b -> 'b) -> 'a p -> 'b -> 'b

    val for_all : (Monomial.Make(Literal).t -> 'a -> bool) -> 'a p -> bool

    val exists : (Monomial.Make(Literal).t -> 'a -> bool) -> 'a p -> bool

    val filter : (Monomial.Make(Literal).t -> 'a -> bool) -> 'a p -> 'a p

    val partition :
      (Monomial.Make(Literal).t -> 'a -> bool) -> 'a p -> 'a p * 'a p

    val cardinal : 'a p -> int

    val bindings : 'a p -> (Monomial.Make(Literal).t * 'a) list

    val get_coef : Monomial.Make(Literal).t -> t -> K.t

    val to_seq : 'a p -> (Monomial.Make(Literal).t * 'a) Seq.t

    val add_seq : (Monomial.Make(Literal).t * K.t) Seq.t -> t -> t

    val of_seq : (Monomial.Make(Literal).t * K.t) Seq.t -> t
  end

  (** Polynomials with coefficients in a ring, which forms a ring.
    We also define partial euclidian division and gcd
 *)
  module Make_Ring (K : Algebra.Ring_S) : sig
    include module type of Make_Semiring (K)

    (** Negation of a polynomial *)
    val neg : t -> t

    (** Substraction of two polynomials *)
    val sub : t -> t -> t

    (** [partial_div_euclid y p1 p2] returns [(q,r,d,n)] st:
      - dⁿ.p₁ = q.p₂ + r
      - if p1 and p2 are polynomials in Q[x_i,y], where the x_i are other variables, then q and r are polynomials in y, with coefficients in Q[x_i], and d is a polynomial in Q[x_i].
      - deg_y(r) < deg_y(p2)
   *)
    val partial_div_euclid : Literal.t -> t -> t -> t * t * t * int

    (** [partial_gcd y p1 p2] returns the gcd of p1 and p2 wrt the variable y *)
    val partial_gcd : Literal.t -> t -> t -> t

    module Infix : sig
      include module type of Infix

      val ( ~- ) : t -> t

      val ( - ) : t -> t -> t
    end
  end
end
