open Smol

module LString : Literal.S with type t = string = struct
  include String

  let to_string x = x
end

module Monomial = Monomial.Make (LString)
module LiteralMap = Map.Make (LString)

(** (Int∪{+∞}, min, +) is the tropical semiring on integers *)
module Int_semiring = struct
  type t = I of int | Infty

  let zero = Infty

  let one = I 0

  let add a b =
    match (a, b) with
    | (I a, I b) -> I (min a b)
    | (I a, Infty) | (Infty, I a) -> I a
    | _ -> Infty

  let mul a b = match (a, b) with (I a, I b) -> I (a + b) | _ -> Infty

  let to_string = function I a -> Int.to_string a | Infty -> "+∞"

  let equal a b =
    match (a, b) with
    | (I a, I b) -> Int.equal a b
    | (Infty, Infty) -> true
    | _ -> false
end

let mono_sampler l max_exp () =
  List.fold_left
    (fun m x -> LiteralMap.add x (Random.int (max_exp + 1)) m)
    LiteralMap.empty
    l
  |> Monomial.of_map

let int_sampler () = Random.int 20 - 10

let int_tropical_sampler () =
  if Random.int 8 = 0 then Int_semiring.Infty
  else Int_semiring.I (int_sampler ())
