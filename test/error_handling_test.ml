open OUnit2
open QCheck
open Error_handling.Option

let xxx_prop = mk_test ~name:"xxx"
  Arbitrary.(small_int)
  (fun x -> x = x)

let props = [
]

(* -------------------------------------------------------------------------- *)

let tests = "Chapter 4" >::: [
  "mean" >::
    (fun _ ->
      assert_equal None (mean []);
      assert_equal ~printer:(string_of string_of_float)
        (Some 3.)
        (mean [1.; 2.; 3.; 4.; 5.]));

  "ex 4.2" >::
    (fun _ ->
      assert_equal None (variance []);
      assert_equal ~printer:(string_of string_of_float)
        (Some (10. /. 5.))
        (variance [1.; 2.; 3.; 4.; 5.]));
]
