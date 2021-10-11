open Smol

type kind = Unit | Rand

let map_tests f (kind, desc, test) = (desc, `Quick, f (kind, test))

let rec repeat n f () =
  if n < 0 then ()
  else
    let t = Sys.time () in
    f () ;
    Format.printf "Time: %fs\n========\n" (Sys.time () -. t) ;
    repeat (n - 1) f ()

let repeat n (kind, f) =
  match kind with Unit -> repeat 1 f | Rand -> repeat n f

let get_tests n = List.map (map_tests (repeat n))

module Make (A : Algebra.Basic_S) = struct
  let pp fmt x = Format.pp_print_string fmt (A.to_string x)

  let testable = Alcotest.testable pp A.equal

  let check ~msg ~expected ~actual =
    Format.printf "Expected:\n\t%a\nActual:\n\t%a\n\n" pp expected pp actual ;
    Alcotest.check' testable ~msg ~expected ~actual
end

module Test_Basics (A : Algebra.Basic_S) = struct
  include Make (A)

  let test_equal_self s () =
    let a = s () in
    Alcotest.(check' bool)
      ~msg:"test_equal_self"
      ~expected:true
      ~actual:(A.equal a a)

  let test_to_string s () =
    let a = s () in
    Alcotest.(check' pass)
      ~msg:"test_to_print"
      ~expected:""
      ~actual:(A.to_string a)

  let tests s =
    [
      (Rand, "∀ a. a = a", test_equal_self s);
      (Rand, "Test to_string", test_to_string s);
    ]
end

let make_tests_basic (type a) ?(ampli = 1) ?(name = "Basic tests")
    (module A : Algebra.Basic_S with type t = a) (sampler : unit -> a) =
  let module T = Test_Basics (A) in
  (name, List.map (map_tests (repeat ampli)) (T.tests sampler))

module Test_Mul_Monoid (A : Algebra.Mul_Monoid_S) = struct
  include Make (A)

  let test_mul_one s () =
    let a = s () in
    check ~msg:"test_mul_one_right" ~expected:a ~actual:(A.mul a A.one) ;
    check ~msg:"test_mul_one_left" ~expected:a ~actual:(A.mul A.one a)

  let test_mul_commute s () =
    let (a, b) = (s (), s ()) in
    check ~msg:"test_mul_commute" ~expected:(A.mul a b) ~actual:(A.mul b a)

  let test_mul_assoc s () =
    let (a, b, c) = (s (), s (), s ()) in
    check
      ~msg:"test_mul_assoc"
      ~expected:(A.mul a (A.mul b c))
      ~actual:(A.mul (A.mul a b) c)

  let tests s =
    [
      (Rand, "∀ a. a×1 = 1×a = a", test_mul_one s);
      (Rand, "∀ a b c. a×(b×c) = (a×b)×c", test_mul_assoc s);
    ]

  let tests_commutative s = [(Rand, "∀ a b. a×b = b×a", test_mul_commute s)]

  let tests_non_commutative _s = []
end

let make_tests_mul_monoid (type a) ?(ampli = 1)
    ?(name = "Multiplicative monoid") ~(commutative : bool)
    (module A : Algebra.Mul_Monoid_S with type t = a) (sampler : unit -> a) =
  let module T1 = Test_Basics (A) in
  let module T2 = Test_Mul_Monoid (A) in
  let tests = T1.tests sampler @ T2.tests sampler in
  let other_tests =
    if commutative then T2.tests_commutative sampler
    else T2.tests_non_commutative sampler
  in
  let tests = tests @ other_tests in
  (name, get_tests ampli tests)

module Test_Add_Monoid (A : Algebra.Add_Monoid_S) = struct
  include Make (A)

  let test_add_zero s () =
    let a = s () in
    check ~msg:"test_add_zero_right" ~expected:a ~actual:(A.add a A.zero) ;
    check ~msg:"test_add_zero_left" ~expected:a ~actual:(A.add A.zero a)

  let test_add_commute s () =
    let (a, b) = (s (), s ()) in
    check ~msg:"test_add_commute" ~expected:(A.add a b) ~actual:(A.add b a)

  let test_add_assoc s () =
    let (a, b, c) = (s (), s (), s ()) in
    check
      ~msg:"test_add_assoc"
      ~expected:(A.add a (A.add b c))
      ~actual:(A.add (A.add a b) c)

  let tests s =
    [
      (Rand, "∀ a. a+0 = 0+a = a", test_add_zero s);
      (Rand, "∀ a b c. a+(b+c) = (a+b)+c", test_add_assoc s);
    ]

  let tests_commutative s = [(Rand, "∀ a b. a+b = b+a", test_add_commute s)]

  let tests_non_commutative _s = []
end

let make_tests_add_monoid (type a) ?(ampli = 1) ?(name = "Additive monoid")
    ~commutative (module A : Algebra.Add_Monoid_S with type t = a)
    (sampler : unit -> a) =
  let module T1 = Test_Basics (A) in
  let module T2 = Test_Add_Monoid (A) in
  let tests = T1.tests sampler @ T2.tests sampler in
  let other_tests =
    if commutative then T2.tests_commutative sampler
    else T2.tests_non_commutative sampler
  in
  let tests = tests @ other_tests in
  (name, get_tests ampli tests)

module Test_Add_Group (A : Algebra.Add_Group_S) = struct
  include Make (A)

  let test_neg s () =
    let a = s () in
    check ~msg:"test_neg" ~expected:A.zero ~actual:(A.add a (A.neg a))

  let test_sub s () =
    let (a, b) = (s (), s ()) in
    check ~msg:"test_sub" ~expected:(A.sub a b) ~actual:(A.add a (A.neg b))

  let tests s =
    [
      (Rand, "∀ a. a+(-a) = 0", test_neg s);
      (Rand, "∀ a b. a-b = a+(-b)", test_sub s);
    ]
end

let make_tests_add_group (type a) ?(ampli = 1) ?(name = "Additive group")
    ~commutative (module A : Algebra.Add_Group_S with type t = a)
    (sampler : unit -> a) =
  let module T1 = Test_Basics (A) in
  let module T2 = Test_Add_Monoid (A) in
  let module T3 = Test_Add_Group (A) in
  let tests = T1.tests sampler @ T2.tests sampler @ T3.tests sampler in
  let other_tests =
    if commutative then T2.tests_commutative sampler
    else T2.tests_non_commutative sampler
  in
  let tests = tests @ other_tests in
  (name, get_tests ampli tests)

module Test_Semiring (A : Algebra.Semiring_S) = struct
  include Make (A)

  let test_mul_zero s () =
    let a = s () in
    check ~msg:"test_mul_zero_right" ~expected:A.zero ~actual:(A.mul a A.zero) ;
    check ~msg:"test_mul_zero_left" ~expected:A.zero ~actual:(A.mul A.zero a)

  let test_mul_distrib_left s () =
    let (a, b, c) = (s (), s (), s ()) in
    check
      ~msg:"test_mul_distrib_left"
      ~expected:(A.mul a (A.add b c))
      ~actual:(A.add (A.mul a b) (A.mul a c))

  let test_mul_distrib_right s () =
    let (a, b, c) = (s (), s (), s ()) in
    check
      ~msg:"test_mul_distrib_right"
      ~expected:(A.mul (A.add b c) a)
      ~actual:(A.add (A.mul b a) (A.mul c a))

  let tests s =
    [
      (Rand, "∀ a. a×0 = 0×a = 0", test_mul_zero s);
      (Rand, "∀ a b c. a×(b+c) = a×b + a×c", test_mul_distrib_left s);
      (Rand, "∀ a b c. (b+c)×a = b×a + c×a", test_mul_distrib_right s);
    ]
end

let make_tests_semiring (type a) ?(ampli = 1) ?(name = "Semiring") ~commutative
    (module A : Algebra.Semiring_S with type t = a) (sampler : unit -> a) =
  let module T1 = Test_Basics (A) in
  let module T2 = Test_Mul_Monoid (A) in
  let module T3 = Test_Add_Monoid (A) in
  let module T4 = Test_Semiring (A) in
  let tests =
    T1.tests sampler @ T2.tests sampler @ T3.tests sampler @ T4.tests sampler
  in
  let other_tests =
    if commutative then T2.tests_commutative sampler
    else T2.tests_non_commutative sampler
  in
  let tests = tests @ other_tests in
  (name, get_tests ampli tests)

let make_tests_ring (type a) ?(ampli = 1) ?(name = "Ring") ~commutative
    (module A : Algebra.Ring_S with type t = a) (sampler : unit -> a) =
  let module T1 = Test_Basics (A) in
  let module T2 = Test_Mul_Monoid (A) in
  let module T3 = Test_Add_Monoid (A) in
  let module T4 = Test_Add_Group (A) in
  let module T5 = Test_Semiring (A) in
  let tests =
    T1.tests sampler @ T2.tests sampler @ T3.tests sampler @ T4.tests sampler
    @ T5.tests sampler
  in
  let other_tests =
    if commutative then T2.tests_commutative sampler
    else T2.tests_non_commutative sampler
  in
  let tests = tests @ other_tests in
  (name, get_tests ampli tests)

module Test_Field (A : Algebra.Field_S) = struct
  include Make (A)

  let test_two_elements () =
    Alcotest.(check' bool)
      ~msg:"test_two_elements"
      ~expected:false
      ~actual:A.(equal one zero)

  let test_inv s () =
    let a = s () in
    if A.equal a A.zero then Alcotest.(check pass) "test_inv_zero" () ()
    else check ~msg:"test_inv" ~expected:A.one ~actual:(A.mul a (A.inv a))

  let test_div s () =
    let (a, b) = (s (), s ()) in
    if A.equal b A.zero then Alcotest.(check pass) "test_div_zero" () ()
    else check ~msg:"test_div" ~expected:(A.div a b) ~actual:(A.mul a (A.inv b))

  let tests s =
    [
      (Unit, "1 ≠ 0", test_two_elements);
      (Rand, "∀ a, a≠0. a×a⁻¹ = 1", test_inv s);
      (Rand, "∀ a b, b≠0. a/b = a×b⁻¹", test_div s);
    ]
end

let make_tests_field (type a) ?(ampli = 1) ?(name = "Field")
    (module A : Algebra.Field_S with type t = a) (sampler : unit -> a) =
  let (_, l) = make_tests_ring ~ampli ~commutative:true (module A) sampler in
  let module T = Test_Field (A) in
  let tests = l @ get_tests ampli (T.tests sampler) in
  (name, tests)
