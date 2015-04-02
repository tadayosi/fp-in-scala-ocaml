open OUnit2
open QCheck
open Data_structures.List
open Error_handling.Either

let props = []

(* ------------------------------------------------------ *)

let tests =
  let string_of_float_either =
    string_of_either (fun x -> x) string_of_float in
  "Chapter 4 - Either" >::: [
    "mean" >::
    (fun _ ->
       assert_equal (Left "mean of empty list!") (mean Nil);
       assert_equal ~printer:string_of_float_either
         (Right 3.)
         (mean (list [1.; 2.; 3.; 4.; 5.])));

    "safeDive" >::
    (fun _ ->
       assert_equal (Left Division_by_zero) (safe_div 1 0);
       assert_equal (Right 2) (safe_div 4 2));
  ]
