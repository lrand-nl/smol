open Smol
module Monomial = Monomial.Make (Helpers.LString)

let pp fmt x = Format.pp_print_string fmt (Monomial.to_string x)

let testable = Alcotest.testable pp Monomial.equal

let ampli = 256

let x = "x"

let y = "y"

let z = "z"

let sampler = Helpers.mono_sampler

let s () =
  let a = sampler [x; y; z] 10 () in
  Format.printf "%a\n" pp a ;
  a

let i = Helpers.int_sampler

let (name_tests, tests_algebra) =
  Test_helpers.make_tests_mul_monoid
    ~ampli
    ~name:"Monomials"
    ~commutative:true
    (module Monomial)
    (sampler [x; y; z] 3)

let test_of_literal () =
  let m = Monomial.of_literal x in
  Alcotest.(check int) "test_of_literal_1" 1 (Monomial.deg x m) ;
  Alcotest.(check int) "test_of_literal_0" 0 (Monomial.deg y m)

let test_singleton () =
  let i = Int.abs (i ()) in
  let m = Monomial.singleton x i in
  Alcotest.(check int) "test_singleton_x" i (Monomial.deg x m) ;
  Alcotest.(check int) "test_singleton_y" 0 (Monomial.deg y m)

let test_set_exponent () =
  let e = Int.abs (i ()) in
  try
    let m = s () |> Monomial.set_exponent x e in
    Alcotest.(check int) "test_set_exponent" e (Monomial.deg x m)
  with Monomial.Monomial_set_negative_exponent _ ->
    Alcotest.(check bool) "test_set_exponent_fail_on_negative" true (e < 0)

let test_remove () =
  let m = s () |> Monomial.remove x in
  Alcotest.(check int) "test_remove" 0 (Monomial.deg x m)

let test_deriv () =
  let m = s () in
  match Monomial.deriv x m with
  | Some (i, m') ->
      Alcotest.(check int) "test_deriv_exp" (Monomial.deg x m) i ;
      Alcotest.check
        testable
        "test_deriv_remain"
        (Monomial.update x (fun x -> x - 1) m)
        m'
  | None -> Alcotest.(check int) "test_deriv_none" 0 (Monomial.deg x m)

let test_apply () =
  let e = i () in
  let rec pow i j =
    if j = 0 then 1 else if j <= 1 then i else i * pow i (j - 1)
  in
  let m = sampler [x; y; z] 6 () in
  let (v, rem) =
    Monomial.apply (module Int) m (Helpers.LiteralMap.singleton x e)
  in
  Alcotest.(check int) "test_apply_value" (pow e (Monomial.deg x m)) v ;
  Alcotest.check testable "test_apply_remain" (Monomial.remove x m) rem

let mono_tests =
  let open Test_helpers in
  [
    (Unit, "Test monomial from literal", test_of_literal);
    (Rand, "Test monomial from singleton", test_singleton);
    (Rand, "Test set exponent in monomial", test_set_exponent);
    (Rand, "Test remove variable from monomial", test_remove);
    (Rand, "Test derivation", test_deriv);
    (Rand, "Test application (Int)", test_apply);
  ]

let tests =
  [(name_tests, tests_algebra @ Test_helpers.get_tests ampli mono_tests)]
