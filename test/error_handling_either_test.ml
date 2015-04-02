open OUnit2
open QCheck
open Data_structures.List
open Error_handling.Either

let props = []

(* ------------------------------------------------------ *)

let tests =
  let string_of_float_either =
    string_of_either (fun x -> x) string_of_float in
  let string_of_int_list_either =
    string_of_either (fun x -> x) (string_of_list string_of_int) in
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

    "ex 4.7 - sequence" >::
    (fun _ ->
       assert_equal ~printer:string_of_int_list_either
         (Right (list [1; 2; 3]))
         (sequence (list [Right 1; Right 2; Right 3]));
       assert_equal ~printer:string_of_int_list_either
         (Left "2")
         (sequence (list [Right 1; Left "2"; Left "3"])));

    "ex 4.7 - traverse" >::
    (fun _ ->
       let parse_ints ss = traverse ss (fun s ->
           try Right (int_of_string s)
           with e -> Left (Printexc.to_string e)) in
       assert_equal ~printer:string_of_int_list_either
         (Right (list [1; 2; 3]))
         (parse_ints (list ["1"; "2"; "3"]));
       assert_equal ~printer:string_of_int_list_either
         (Left "Failure(\"int_of_string\")")
         (parse_ints (list ["1"; "a"; "3"])));
  ]
