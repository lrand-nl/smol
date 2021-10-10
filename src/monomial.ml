module Make (Literal : Literal.S) = struct
  module LiteralMap = Map.Make (Literal)

  type t = int LiteralMap.t

  exception Monomial_has_non_positive_exponent of (Literal.t * int)

  exception Monomial_set_negative_exponent of (Literal.t * int)

  let iter = LiteralMap.iter

  let fold = LiteralMap.fold

  let for_all = LiteralMap.for_all

  let exists = LiteralMap.exists

  let filter = LiteralMap.filter

  let partition = LiteralMap.partition

  let cardinal = LiteralMap.cardinal

  let bindings = LiteralMap.bindings

  let map = LiteralMap.map

  let mapi = LiteralMap.mapi

  let to_seq = LiteralMap.to_seq

  (** Check if exponent is non-negative, fail if negative *)
  let is_exp_nonneg var exp =
    if exp < 0 then raise (Monomial_set_negative_exponent (var, exp)) ;
    exp > 0

  let filter_seq = Seq.filter (fun (var, exp) -> is_exp_nonneg var exp)

  let add_seq s = filter_seq s |> LiteralMap.add_seq

  let of_seq s = filter_seq s |> LiteralMap.of_seq

  let of_map = LiteralMap.filter is_exp_nonneg

  let to_map m = m

  let one = LiteralMap.empty

  let is_one = LiteralMap.is_empty

  let singleton var exp =
    if is_exp_nonneg var exp then LiteralMap.singleton var exp else one

  let set_exponent var exp =
    if is_exp_nonneg var exp then LiteralMap.add var exp
    else LiteralMap.remove var

  let update var f =
    (* For convenience, we want [f] to be [int -> int], so we create a function
       [g : int option -> int option] to pass to [update]. [None] is 0 in that case.
    *)
    let g x =
      let fx = f (Option.value x ~default:0) in
      if is_exp_nonneg var fx then Some fx else None
    in
    LiteralMap.update var g

  let union f a b =
    let g var x y =
      let fx = f var x y in
      if is_exp_nonneg var fx then Some fx else None
    in
    LiteralMap.union g a b

  let remove = LiteralMap.remove

  let of_literal var = singleton var 1

  let get_support m = List.map fst (bindings m)

  let compare m1 m2 =
    let rec aux lb1 lb2 =
      match (lb1, lb2) with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | ((k1, e1) :: t1, (k2, e2) :: t2) -> (
          match Literal.compare k1 k2 with
          | 0 -> if e1 = e2 then aux t1 t2 else e1 - e2
          | c -> c)
    in
    aux (bindings m1) (bindings m2)

  let equal = LiteralMap.equal Int.equal

  let neq a b = not (equal a b)

  let deg var m = Option.value (LiteralMap.find_opt var m) ~default:0

  let mul : t -> t -> t =
    let aux_union _ e1 e2 = Some (e1 + e2) in
    LiteralMap.union aux_union

  let deriv var m : (int * t) option =
    match deg var m with
    | 0 -> None
    | 1 -> Some (1, remove var m)
    | exp -> Some (exp, set_exponent var (exp - 1) m)

  (* Fast exponentiation for K *)
  let pow (type a) (module K : Algebra.Mul_Monoid_S with type t = a) (x : a)
      (exp : int) : a =
    let rec pow_aux x exp =
      if exp = 0 then K.one
      else if exp = 1 then x
      else
        let exp_2 = exp / 2 in
        let px = pow_aux x exp_2 in
        let px = K.mul px px in
        if exp mod 2 = 0 then px else K.mul px x
    in
    pow_aux x exp

  let apply (type a) (module K : Algebra.Mul_Monoid_S with type t = a) m spec :
      a * t =
    let apply_single m var value =
      match deg var m with
      | 0 -> (K.one, m)
      | exp -> (pow (module K) value exp, remove var m)
    in
    (* A fold on [spec]. Its bindings are applied sequentially *)
    LiteralMap.fold
      (fun var value (coef, m_acc) ->
        let (a_coef, m_acc) = apply_single m_acc var value in
        (K.mul coef a_coef, m_acc))
      spec
      (K.one, m)

  (** Pretty (?) printing of integer exponents *)
  let digit_to_string = function
    | 0 -> "⁰"
    | 1 -> "¹"
    | 2 -> "²"
    | 3 -> "³"
    | 4 -> "⁴"
    | 5 -> "⁵"
    | 6 -> "⁶"
    | 7 -> "⁷"
    | 8 -> "⁸"
    | 9 -> "⁹"
    | _ -> ""

  let rec exp_to_string n =
    if n <= 0 then "" else exp_to_string (n / 10) ^ digit_to_string (n mod 10)

  let to_string m =
    if is_one m then "1ₘ"
    else
      let aux_map (var, exp) =
        if exp < 1 then raise (Monomial_has_non_positive_exponent (var, exp))
        else if exp = 1 then Literal.to_string var
        else Printf.sprintf "%s%s" (Literal.to_string var) (exp_to_string exp)
      in
      String.concat "" (List.map aux_map (bindings m))

  module Infix = struct
    let ( * ) = mul

    let ( = ) = equal

    let ( <> ) = neq
  end
end
