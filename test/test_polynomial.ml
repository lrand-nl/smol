open Smol
open Smol_helpers
module K = Int
module Poly = Polynomial.Make (Helpers.LString)
module Polynomial = Poly.Make_Ring (K)
module Monomial = Monomial.Make (Helpers.LString)
include Test_helpers.Make (Polynomial)

let ampli = 256

let x = "x"

let y = "y"

let z = "z"

let i = Helpers.int_sampler

let sampler l max_exp num_mono () =
  List.fold_left
    (fun acc _ ->
      Polynomial.add
        acc
        (Polynomial.singleton (Helpers.mono_sampler l max_exp ()) (i ())))
    Polynomial.zero
    (List.init num_mono (fun _ -> ()))

let s () =
  let a = sampler [x; y; z] 10 (Random.int 6) () in
  Format.printf "%a\n" pp a ;
  a

let (name_tests, tests_algebra) =
  Test_helpers.make_tests_ring
    ~ampli
    ~name:"Polynomials"
    ~commutative:true
    (module Polynomial)
    (sampler [x; y; z] 3 3)

let test_of_literal () =
  let p = Polynomial.of_literal x in
  Alcotest.(check int) "test_of_literal_1" 1 (Polynomial.deg x p) ;
  Alcotest.(check int) "test_of_literal_0" 0 (Polynomial.deg y p) ;
  Alcotest.(check int)
    "test_of_literal_k"
    1
    (Polynomial.get_coef (Monomial.of_literal x) p) ;
  Alcotest.(check int) "test_of_literal_card" 1 (Polynomial.cardinal p)

let test_of_monomial () =
  let m = Test_monomial.s () in
  let p = Polynomial.of_monomial m in
  Alcotest.(check int) "test_of_monomial_coef" 1 (Polynomial.get_coef m p) ;
  Alcotest.(check int) "test_of_monomial_card" 1 (Polynomial.cardinal p)

let test_of_scalar () =
  let k = i () in
  let p = Polynomial.of_scalar k in
  Alcotest.(check int)
    "test_of_scalar_coef"
    k
    (Polynomial.get_coef Monomial.one p) ;
  if k = 0 then
    Alcotest.(check bool) "test_of_monomial_zero" true (Polynomial.is_zero p)
  else Alcotest.(check int) "test_of_monomial_card" 1 (Polynomial.cardinal p)

let test_singleton () =
  let k = i () in
  let m = Test_monomial.s () in
  let p = Polynomial.singleton m k in
  if k = 0 then
    Alcotest.(check bool) "test_of_monomial_zero" true (Polynomial.is_zero p)
  else (
    Alcotest.(check int) "test_of_monomial_coef" k (Polynomial.get_coef m p) ;
    Alcotest.(check int) "test_of_monomial_card" 1 (Polynomial.cardinal p))

let test_card_zero () =
  Alcotest.(check int) "test_card_zero" 0 (Polynomial.cardinal Polynomial.zero)

let test_support () =
  let p = s () in
  if Polynomial.is_zero p then (
    Alcotest.(check bool) "test_support_zero_deg" false (Polynomial.deg x p >= 0) ;
    Alcotest.(check int)
      "test_support_zero"
      0
      (List.length (Polynomial.get_support p)))
  else if List.mem x (Polynomial.get_support p) then
    Alcotest.(check (neg int)) "test_support_deg" 0 (Polynomial.deg x p)
  else Alcotest.(check int) "test_support_not" 0 (Polynomial.deg x p)

let test_mul_scalar () =
  let (k1, k2) = (i (), i ()) in
  let m = Test_monomial.s () in
  let p = Polynomial.singleton m k1 in
  check
    ~msg:"test_mul_scalar"
    ~expected:(Polynomial.singleton m (k1 * k2))
    ~actual:(Polynomial.mul_scalar k2 p)

let test_mul_scalar_distrib () =
  let k = i () in
  let (p1, p2) = (s (), s ()) in
  check
    ~msg:"test_mul_scalar_distrib"
    ~expected:Polynomial.Infix.((k *. p1) + (k *. p2))
    ~actual:Polynomial.Infix.(k *. (p1 + p2))

let test_eval () =
  let k = i () in
  let p0 = sampler [y; z] 10 (Random.int 6) () in
  let p1 = sampler [y; z] 10 (Random.int 6) () in
  let p2 = sampler [y; z] 10 (Random.int 6) () in
  let x' = Polynomial.of_literal x in
  let px = Polynomial.Infix.(p0 + (x' * p1) + (x' * x' * p2)) in
  let e = Polynomial.eval (Helpers.LiteralMap.singleton x k) px in
  let result = Polynomial.Infix.(p0 + (k *. p1) + (Int.mul k k *. p2)) in
  Alcotest.check testable "test_eval" result e

let test_substitution () =
  let px = s () in
  let py = s () in
  let p0 = sampler [z] 10 (Random.int 6) () in
  let p1 = sampler [z] 10 (Random.int 6) () in
  let p2 = sampler [z] 10 (Random.int 6) () in
  let p3 = sampler [z] 10 (Random.int 6) () in
  let x' = Polynomial.of_literal x in
  let y' = Polynomial.of_literal y in
  let p = Polynomial.Infix.(p0 + (x' * p1) + (y' * p2) + (x' * y' * p3)) in
  let sub =
    Polynomial.substitution
      (Helpers.LiteralMap.singleton x px |> Helpers.LiteralMap.add y py)
      p
  in
  let result = Polynomial.Infix.(p0 + (px * p1) + (py * p2) + (px * py * p3)) in
  check ~msg:"test_substitution" ~expected:result ~actual:sub

let test_deriv () =
  let e = Int.abs (i ()) + 1 in
  let p0 = sampler [y; z] 10 (Random.int 6) () in
  let p1 = sampler [y; z] 10 (Random.int 6) () in
  let p2 = sampler [y; z] 10 (Random.int 6) () in
  let x' = Polynomial.of_literal x in
  let m = Polynomial.of_monomial (Monomial.singleton x e) in
  let m' = Polynomial.of_monomial (Monomial.singleton x (e - 1)) in
  let px = Polynomial.Infix.(p0 + (x' * p1) + (m * p2)) in
  let d = Polynomial.deriv x px |> Polynomial.map (fun (a, b) -> a * b) in
  let result = Polynomial.Infix.(p1 + (e *. (m' * p2))) in
  check ~msg:"test_deriv" ~expected:result ~actual:d

let poly_tests =
  let open Test_helpers in
  [
    (Unit, "Test polynomial from literal", test_of_literal);
    (Rand, "Test polynomial from monomial", test_of_monomial);
    (Rand, "Test polynomial from scalar", test_of_scalar);
    (Rand, "Test polynomial singleton", test_singleton);
    (Unit, "Test cardinality of zero polynomial", test_card_zero);
    (Rand, "Test support of polynomial", test_support);
    (Rand, "Test scalar multiplication", test_mul_scalar);
    (Rand, "Test scalar multiplication distributivity", test_mul_scalar_distrib);
    (Rand, "Test evaluation", test_eval);
    (Rand, "Test substitution", test_substitution);
    (Rand, "Test derivation", test_deriv);
  ]

let tests =
  [(name_tests, tests_algebra @ Test_helpers.get_tests ampli poly_tests)]
