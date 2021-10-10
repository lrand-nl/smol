exception Polynomial_is_not_scalar

module Make (Literal : Literal.S) = struct
  module Monomial = Monomial.Make (Literal)
  module MonomialMap = Map.Make (Monomial)
  module LiteralMap = Map.Make (Literal)

  type 'a p = 'a MonomialMap.t

  module Make_Semiring (K : Algebra.Semiring_S) = struct
    type t = K.t p

    let k_is_zero = K.(equal zero)

    let remove_zero_coeff = MonomialMap.filter (fun _ k -> not (k_is_zero k))

    let mem = MonomialMap.mem

    let remove = MonomialMap.remove

    let set_coef m k p =
      if k_is_zero k then remove m p else MonomialMap.add m k p

    let update m f =
      let g x =
        let fx = f (Option.value x ~default:K.zero) in
        if k_is_zero fx then None else Some fx
      in
      MonomialMap.update m g

    let union f =
      let g m x y =
        let f_ = f m x y in
        if k_is_zero f_ then None else Some f_
      in
      MonomialMap.union g

    let compare = MonomialMap.compare

    let iter = MonomialMap.iter

    let fold = MonomialMap.fold

    let for_all = MonomialMap.for_all

    let exists = MonomialMap.exists

    let filter = MonomialMap.filter

    let partition = MonomialMap.partition

    let cardinal = MonomialMap.cardinal

    let bindings = MonomialMap.bindings

    let get_coef m p = Option.value (MonomialMap.find_opt m p) ~default:K.zero

    let to_seq = MonomialMap.to_seq

    let filter_seq = Seq.filter (fun (_, x) -> not (k_is_zero x))

    let add_seq s p = MonomialMap.add_seq (filter_seq s) p

    let of_seq s = MonomialMap.of_seq (filter_seq s)

    let map f p = MonomialMap.map f p |> remove_zero_coeff

    let mapi f p = MonomialMap.mapi f p |> remove_zero_coeff

    let merge f =
      let g m x y =
        let fx = f m x y in
        if k_is_zero fx then None else Some fx
      in
      MonomialMap.merge g

    let zero = MonomialMap.empty

    let is_zero = MonomialMap.is_empty

    let singleton m k = if k_is_zero k then zero else MonomialMap.singleton m k

    let one = singleton Monomial.one K.one

    let of_scalar k = if k_is_zero k then zero else singleton Monomial.one k

    let of_literal var = singleton (Monomial.of_literal var) K.one

    let of_monomial m = singleton m K.one

    let get_support p =
      let ls = fold (fun m _ ac -> Monomial.get_support m :: ac) p [] in
      List.sort_uniq Literal.compare (List.flatten ls)

    let equal = MonomialMap.equal K.equal

    let neq a b = not (equal a b)

    let deg var p = fold (fun m _ acc -> max acc (Monomial.deg var m)) p (-1)

    let add = union (fun _ -> K.add)

    let mul_scalar : K.t -> t -> t =
     fun k p ->
      if k_is_zero k then zero
      else if K.(equal k one) then p
        (* Faster than using [map], we know the resulting coefficients are not zero *)
      else MonomialMap.map (K.mul k) p

    let mul : t -> t -> t =
     fun p1 p2 ->
      let fold_aux m1 sc1 p_acc =
        add
          (fold
             (fun m2 sc2 acc ->
               if k_is_zero sc1 || k_is_zero sc2 then acc
               else MonomialMap.add (Monomial.mul m1 m2) (K.mul sc1 sc2) acc)
             p2
             zero)
          p_acc
      in
      fold fold_aux p1 zero

    let flatten (pp : t p) =
      fold (fun m p acc -> add acc (mul p (of_monomial m))) pp zero

    let leading_coef var p =
      fold
        (fun m c (acc, d) ->
          let dm = Monomial.deg var m in
          if dm < d then (acc, d)
          else if dm = d then
            (add acc (mul_scalar c (of_monomial (Monomial.remove var m))), d)
          else (mul_scalar c (of_monomial (Monomial.remove var m)), dm))
        p
        (zero, -1)

    let apply_raw (type a b) (module M_a : Algebra.Mul_Monoid_S with type t = a)
        (module K_b : Algebra.Semiring_S with type t = b)
        (mul_ak : K.t -> a -> b) (spe : a LiteralMap.t) (p : t) : b p =
      let fold_aux m sc p_acc =
        let (a_applied, m_applied) = Monomial.apply (module M_a) m spe in
        let b_mul = mul_ak sc a_applied in
        if K_b.(equal b_mul zero) then p_acc
        else
          MonomialMap.update
            m_applied
            (function
              | None -> Some b_mul
              | Some b_old ->
                  let b = K_b.add b_old b_mul in
                  if K_b.(equal b zero) then None else Some b)
            p_acc
      in
      fold fold_aux p zero

    let apply (type a) (module K_a : Algebra.Semiring_S with type t = a) =
      apply_raw (module K_a) (module K_a)

    let eval = apply (module K) K.mul

    module Poly_as_semiring : Algebra.Semiring_S with type t = t = struct
      type t = K.t p

      let one = one

      let mul = mul

      let zero = zero

      let add = add

      let equal = equal

      let to_string _ = ""
    end

    let substitution lm p =
      apply (module Poly_as_semiring) mul_scalar lm p |> flatten

    let apply_at_0 = get_coef Monomial.one

    let to_scalar p =
      let x = apply_at_0 p in
      let q = MonomialMap.remove Monomial.one p in
      if is_zero q then x else raise Polynomial_is_not_scalar

    let to_scalar_opt p =
      try Some (to_scalar p) with Polynomial_is_not_scalar -> None

    let deriv : Literal.t -> t -> (int * K.t) p =
     fun var p ->
      let fold_aux m sc p_acc =
        match Monomial.deriv var m with
        | None -> p_acc
        | Some (coef, m_app) -> MonomialMap.add m_app (coef, sc) p_acc
      in
      fold fold_aux p zero

    let to_string p : string =
      if is_zero p then "0ₚ"
      else if equal p one then "1ₚ"
      else
        let aux_map (m, coef) =
          let ms = Monomial.to_string m in
          if K.(equal coef one) then ms
          else Printf.sprintf "%s.%s" (K.to_string coef) ms
        in
        String.concat " + " (List.map aux_map (MonomialMap.bindings p))

    module Infix = struct
      let ( + ) = add

      let ( * ) = mul

      let ( *. ) = mul_scalar

      let ( = ) = equal

      let ( <> ) = neq
    end
  end

  module Make_Ring (K : Algebra.Ring_S) = struct
    include Make_Semiring (K)

    (* Faster than [map]: [K.zero] is not in [ℑ(K.neg)] *)
    let neg = MonomialMap.map K.neg

    let sub a b = add a (neg b)

    let partial_div_euclid : Literal.t -> t -> t -> t * t * t * int =
     fun y p1 p2 ->
      let (l2, n2) = leading_coef y p2 in
      if n2 < 0 then (zero, p1, one, 1)
      else if n2 = 0 then (p1, zero, l2, 1)
      else
        let rec aux q r d_n =
          let (_l1, n1) = leading_coef y r in
          if n1 < n2 then (q, r, d_n)
          else
            let tmp =
              fold
                (fun m c acc ->
                  let nm = Monomial.deg y m in
                  if nm > n2 then
                    MonomialMap.add (Monomial.set_exponent y (nm - n2) m) c acc
                  else if nm = n2 then
                    MonomialMap.add (Monomial.remove y m) c acc
                  else acc)
                r
                zero
            in
            let q = add (mul l2 q) tmp in
            let r = sub (mul l2 r) (mul tmp p2) in
            aux q r (Int.succ d_n)
        in
        let (a, b, c) = aux zero p1 0 in
        if Int.equal c 0 then (a, b, one, 1) else (a, b, l2, c)

    let rec partial_gcd : Literal.t -> t -> t -> t =
     fun y p1 p2 ->
      if is_zero p2 then p1
      else if deg y p2 = 0 then one
      else
        let (_, r, _, _) = partial_div_euclid y p1 p2 in
        partial_gcd y p2 r

    module Infix = struct
      include Infix

      let ( ~- ) = neg

      let ( - ) = sub
    end
  end
end
