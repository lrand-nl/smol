let () =
  Random.self_init () ;
  Alcotest.run
    "tezos-math"
    (Test_monomial.tests @ Test_polynomial.tests @ Test_vector.tests
   @ Test_matrix.tests)
