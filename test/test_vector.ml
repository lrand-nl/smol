open Smol
open Smol_helpers
module K = Int
module Vector = Vector.Make (Helpers.LString)
module V = Vector.Make_R (K)
include Test_helpers.Make (V)

let ampli = 256

let x = "x"

let y = "y"

let z = "z"

let i = Helpers.int_sampler

let s () =
  let a =
    List.fold_left
      (fun acc v -> Helpers.LiteralMap.add v (i ()) acc)
      Helpers.LiteralMap.empty
      [x; y; z]
    |> Vector.of_map
  in
  Format.printf "%a\n" pp a ;
  a

let (name_tests, tests_algebra) =
  Test_helpers.make_tests_add_group
    ~ampli
    ~name:"Vectors"
    ~commutative:true
    (Vector.make_group (module K) [x; y; z])
    s

let test_singleton () = ()

let test_mul_dot () =
  let (a, b, c, d, e, f) = (i (), i (), i (), i (), i (), i ()) in
  let v1 =
    Vector.singleton x a |> Vector.add_entry y b |> Vector.add_entry z c
  in
  let v2 =
    Vector.singleton x d |> Vector.add_entry y e |> Vector.add_entry z f
  in
  Alcotest.(check int)
    "test_mul_dot"
    ((a * d) + (b * e) + (c * f))
    (V.mul_dot v1 v2)

let vect_tests =
  let open Test_helpers in
  [(Rand, "Test dot product", test_mul_dot)]

let tests =
  [(name_tests, tests_algebra @ Test_helpers.get_tests ampli vect_tests)]
