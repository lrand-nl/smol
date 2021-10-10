module Make (Literal : Literal.S) = struct
  module LiteralMap = Map.Make (Literal)

  type 'a v = 'a LiteralMap.t

  let of_map x = x

  let to_map x = x

  let singleton = LiteralMap.singleton

  let merge = LiteralMap.merge

  let for_all = LiteralMap.for_all

  let map = LiteralMap.map

  let mapi = LiteralMap.mapi

  let union = LiteralMap.union

  let fold = LiteralMap.fold

  let cardinal = LiteralMap.cardinal

  let partition = LiteralMap.partition

  let filter = LiteralMap.filter

  let exists = LiteralMap.exists

  let iter = LiteralMap.iter

  let bindings = LiteralMap.bindings

  let to_seq = LiteralMap.to_seq

  let add_seq = LiteralMap.add_seq

  let of_seq = LiteralMap.of_seq

  let mem = LiteralMap.mem

  let get_entry = LiteralMap.find_opt

  let add_entry = LiteralMap.add

  let remove_entry = LiteralMap.remove

  let update_entry = LiteralMap.update

  let get_support v = List.map fst (bindings v)

  module Make_SR (K : Algebra.Semiring_S) = struct
    (** Vectors of a finite basis of literals *)
    type t = K.t v

    exception Vector_incompatible of (t * t)

    let zero l = List.map (fun x -> (x, K.zero)) l |> List.to_seq |> of_seq

    let is_zero = for_all (fun _ -> K.(equal zero))

    let equal v1 v2 =
      let p =
        merge
          (fun _ r1 r2 ->
            match (r1, r2) with
            | (None, None) -> None
            | (Some _r, None) | (None, Some _r) -> Some false
            | (Some r1, Some r2) -> Some (K.equal r1 r2))
          v1
          v2
      in
      for_all (fun _ x -> x) p

    let neq a b = not (equal a b)

    let add v1 v2 =
      merge
        (fun _ rf1 rf2 ->
          match (rf1, rf2) with
          | (None, None) -> None
          | (Some a, Some b) -> Some (K.add a b)
          | _ -> raise (Vector_incompatible (v1, v2)))
        v1
        v2

    let mul_dot v1 v2 : K.t =
      let v =
        merge
          (fun _ a b ->
            match (a, b) with
            | (Some ra, Some rb) -> Some (K.mul ra rb)
            | (None, None) -> None
            | _ -> raise (Vector_incompatible (v1, v2)))
          v1
          v2
      in
      fold (fun _ -> K.add) v K.zero

    let mul_scalar s v = map (K.mul s) v

    let to_string v : string =
      String.concat
        "\n"
        (List.map
           (fun (var, rf) ->
             Printf.sprintf "%s := %s" (Literal.to_string var) (K.to_string rf))
           (bindings v))

    module Infix = struct
      let ( + ) = add

      let ( *. ) = mul_scalar

      let ( ** ) = mul_dot

      let ( = ) = equal

      let ( <> ) = neq
    end
  end

  module Make_R (K : Algebra.Ring_S) = struct
    (** Vectors of a finite basis of literals *)
    include Make_SR (K)

    let neg = map K.neg

    let sub v1 v2 = add v1 (neg v2)

    module Infix = struct
      include Infix

      let ( ~- ) = neg

      let ( - ) = sub
    end
  end

  let make_monoid (type a) (module K : Algebra.Semiring_S with type t = a) l =
    (module struct
      include Make_SR (K)

      let zero = zero l
    end : Algebra.Add_Monoid_S
      with type t = a v)

  let make_group (type a) (module K : Algebra.Ring_S with type t = a) l =
    (module struct
      include Make_R (K)

      let zero = zero l
    end : Algebra.Add_Group_S
      with type t = a v)
end
