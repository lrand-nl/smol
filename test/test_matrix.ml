(* TODO finish tests *)

open Smol
module K = Int
module Matrix = Matrix.Make (Helpers.LString)
module Vector = Vector.Make (Helpers.LString)
module M = Matrix.Make_R (K)
include Test_helpers.Make (M)

let ampli = 256

let x = "x"

let y = "y"

let z = "z"

let i = Helpers.int_sampler

let supp =
  List.map (fun a -> List.map (fun b -> (a, b)) [x; y; z]) [x; y; z]
  |> List.flatten

let s () =
  let a =
    List.fold_left
      (fun acc (a, b) -> M.set_entry a b (i ()) acc)
      (M.zero [x; y; z])
      supp
  in
  Format.printf "%a\n" pp a ;
  a

let (name_tests, tests_algebra) =
  Test_helpers.make_tests_ring
    ~ampli
    ~name:"Matrix"
    ~commutative:false
    (Matrix.make_ring (module K) [x; y; z])
    s

let test_diagonal () = ()

(** We must ensure the support does not change without our consent. The support is encoded in the diagonal. *)
let test_get_support () = ()

let test_get_line () =
  let m = s () in
  let v = M.get_line x m in
  Alcotest.(check (option int))
    "test_get_line_x"
    (Some (M.get_entry x x m))
    (Vector.get_entry x v) ;
  Alcotest.(check (option int))
    "test_get_line_y"
    (Some (M.get_entry x y m))
    (Vector.get_entry y v) ;
  Alcotest.(check (option int))
    "test_get_line_z"
    (Some (M.get_entry x z m))
    (Vector.get_entry z v)

let test_get_column () =
  let m = s () in
  let v = M.get_column x m in
  Alcotest.(check (option int))
    "test_get_column_x"
    (Some (M.get_entry x x m))
    (Vector.get_entry x v) ;
  Alcotest.(check (option int))
    "test_get_column_y"
    (Some (M.get_entry y x m))
    (Vector.get_entry y v) ;
  Alcotest.(check (option int))
    "test_get_column_z"
    (Some (M.get_entry z x m))
    (Vector.get_entry z v)

let mat_tests =
  let open Test_helpers in
  [
    (Rand, "Test get line", test_get_line);
    (Rand, "Test get column", test_get_column);
  ]

let tests =
  [(name_tests, tests_algebra @ Test_helpers.get_tests ampli mat_tests)]
