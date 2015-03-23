open OUnit2
open QCheck

(* OUnit tests *)
let _ = run_test_tt_main ( "All tests" >::: [
  Getting_started_test.tests;
])

(* QCheck properties *)
let test = mk_test ~n:10
  ~name:"add"
  Arbitrary.(pair small_int small_int)
  (fun (x, y) -> (+) x y = x + y)

let _ = run_tests [ test ]
