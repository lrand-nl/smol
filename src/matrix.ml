module Make (Literal : Literal.S) = struct
  module LiteralPair = struct
    type t = Literal.t * Literal.t

    let compare (x0, y0) (x1, y1) =
      match Literal.compare x0 x1 with 0 -> Literal.compare y0 y1 | c -> c
  end

  module MatrixMap = Map.Make (LiteralPair)
  module LiteralMap = Map.Make (Literal)

  type 'a m = 'a MatrixMap.t

  module Make_SR (K : Algebra.Semiring_S) = struct
    module Vector = Vector.Make (Literal)
    module Vector_SR = Vector.Make_SR (K)

    let k_is_zero = K.(equal zero)

    let none_to_zero x = Option.value x ~default:K.zero

    let is_good_entry v1 v2 k = Literal.equal v1 v2 || not (k_is_zero k)

    (** We make sure the diagonal is never empty on the support of a matrix *)
    type t = K.t m

    let zero' = MatrixMap.empty

    let zero support =
      List.fold_left
        (fun acc s -> MatrixMap.add (s, s) K.zero acc)
        zero'
        support

    let for_all f = MatrixMap.for_all (fun (a, b) -> f a b)

    let is_zero = for_all (fun _ _ -> k_is_zero)

    let one support =
      List.fold_left (fun acc s -> MatrixMap.add (s, s) K.one acc) zero' support

    let diagonal vect =
      Vector.fold (fun s k acc -> MatrixMap.add (s, s) k acc) vect zero'

    let get_entry v1 v2 m = none_to_zero (MatrixMap.find_opt (v1, v2) m)

    let set_entry v1 v2 k =
      if is_good_entry v1 v2 k then MatrixMap.add (v1, v2) k
      else MatrixMap.remove (v1, v2)

    let singleton v1 v2 k =
      MatrixMap.singleton (v1, v1) K.zero
      |> MatrixMap.add (v2, v2) K.zero
      |> set_entry v1 v2 k

    let remove v1 v2 = MatrixMap.remove (v1, v2)

    let merge f =
      let g (v1, v2) x y =
        match f v1 v2 x y with
        | None -> if Literal.equal v1 v2 then Some K.zero else None
        | Some s -> if is_good_entry v1 v2 s then Some s else None
      in
      MatrixMap.merge g

    (** We keep the matrix as sparse as possible, while keeping the diagonal non-empty *)
    let union f =
      let g (v1, v2) x y =
        match f v1 v2 x y with
        | None -> if Literal.equal v1 v2 then Some K.zero else None
        | Some s -> if is_good_entry v1 v2 s then Some s else None
      in
      MatrixMap.union g

    let equal m1 m2 =
      let p =
        MatrixMap.merge
          (fun _ r1 r2 ->
            match (r1, r2) with
            | (None, None) -> None
            | (Some r, None) | (None, Some r) -> Some (r, K.zero)
            | (Some r1, Some r2) -> Some (r1, r2))
          m1
          m2
      in
      MatrixMap.for_all (fun _ (a, b) -> K.equal a b) p

    let neq m1 m2 = not (equal m1 m2)

    let iter f = MatrixMap.iter (fun (v1, v2) -> f v1 v2)

    let fold f = MatrixMap.fold (fun (v1, v2) -> f v1 v2)

    let filter f = MatrixMap.filter (fun (v1, v2) -> f v1 v2)

    let bindings = MatrixMap.bindings

    let remove_zero_coeff = filter is_good_entry

    let map f m = MatrixMap.map f m |> remove_zero_coeff

    let mapi f m =
      MatrixMap.mapi (fun (v1, v2) -> f v1 v2) m |> remove_zero_coeff

    let get_support mat =
      fold (fun a b _ acc -> if Literal.equal a b then a :: acc else acc) mat []

    let get_line var mat =
      MatrixMap.to_seq (filter (fun a _ _ -> Literal.equal a var) mat)
      |> Seq.map (fun ((_, b), c) -> (b, c))
      |> LiteralMap.of_seq |> Vector.of_map
      |> Vector.union (fun _ _ x -> Some x) (Vector_SR.zero (get_support mat))

    let get_column var mat =
      MatrixMap.to_seq (filter (fun _ b _ -> Literal.equal b var) mat)
      |> Seq.map (fun ((a, _), c) -> (a, c))
      |> LiteralMap.of_seq |> Vector.of_map
      |> Vector.union (fun _ _ x -> Some x) (Vector_SR.zero (get_support mat))

    let add = union (fun _ _ a b -> Some (K.add a b))

    let mul m1 m2 =
      let support_m1 = get_support m1 in
      let support_m2 = get_support m2 in
      let aux y acc x =
        let rf = Vector_SR.mul_dot (get_line x m1) (get_column y m2) in
        if (not (Literal.equal x y)) && k_is_zero rf then acc
        else MatrixMap.add (x, y) rf acc
      in
      let aux2 acc2 y = List.fold_left (aux y) acc2 support_m1 in
      List.fold_left aux2 zero' support_m2

    let pow mat k =
      let module St = struct
        include String

        let to_string x = x
      end in
      let module M = Monomial.Make (St) in
      let module SMap = Map.Make (St) in
      let var = "m" in
      let mono = M.singleton var k in
      (* We use [Monomial]'s fast exponentiation because why not *)
      let (res, _) =
        M.apply
          (module struct
            type t = K.t m

            let one = one (get_support mat)

            let mul = mul

            let equal = equal

            let to_string _ = ""
          end)
          mono
          (SMap.singleton var mat)
      in
      res

    let mul_vect m v =
      Vector.mapi (fun var _ -> Vector_SR.mul_dot (get_line var m) v) v

    let is_nilpotent mat =
      pow mat (List.length (get_support mat)) |> equal zero'

    let to_string mat : string =
      (* It is a sparse matrix, only print the non-zero entries *)
      String.concat
        "\n"
        (List.map
           (fun ((v1, v2), rf) ->
             Printf.sprintf
               "( %s ; %s ) := %s"
               (Literal.to_string v1)
               (Literal.to_string v2)
               (K.to_string rf))
           (bindings mat))

    module Infix = struct
      let ( + ) = add

      let ( * ) = mul

      let ( *^ ) = pow

      let ( *> ) = mul_vect

      let ( = ) = equal

      let ( <> ) = neq
    end
  end

  module Make_R (K : Algebra.Ring_S) = struct
    include Make_SR (K)

    let neg = map K.neg

    let sub m1 m2 = add m1 (neg m2)

    module Infix = struct
      include Infix

      let ( ~- ) = neg

      let ( - ) = sub
    end
  end

  let make_semiring (type a) (module K : Algebra.Semiring_S with type t = a) l =
    (module struct
      include Make_SR (K)

      let zero = zero l

      let one = one l
    end : Algebra.Semiring_S
      with type t = a m)

  let make_ring (type a) (module K : Algebra.Ring_S with type t = a) l =
    (module struct
      include Make_R (K)

      let zero = zero l

      let one = one l
    end : Algebra.Ring_S
      with type t = a m)
end
