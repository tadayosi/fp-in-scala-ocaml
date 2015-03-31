open OUnit2
open QCheck
open Data_structures.List
open Error_handling.Option

let xxx_prop = mk_test ~name:"xxx"
  Arbitrary.(small_int)
  (fun x -> x = x)

let props = [
]

(* ------------------------------------------------------ *)

let tests =
  let string_of_float_option =
    string_of_option string_of_float in
  let string_of_int_list_option =
    string_of_option (string_of_list string_of_int) in
  "Chapter 4" >::: [
  "mean" >::
    (fun _ ->
      assert_equal None (mean Nil);
      assert_equal ~printer:string_of_float_option
        (Some 3.)
        (mean (list [1.; 2.; 3.; 4.; 5.])));

  "ex 4.2" >::
    (fun _ ->
      assert_equal None (variance Nil);
      assert_equal ~printer:string_of_float_option
        (Some (10. /. 5.))
        (variance (list [1.; 2.; 3.; 4.; 5.])));

  "ex 4.3" >::
    (fun _ ->
      assert_equal (Some 3) (map2 (Some 1) (Some 2) (+));
      assert_equal None (map2 (Some 1) None (+)));

  "ex 4.4" >::
    (fun _ ->
      assert_equal ~printer:string_of_int_list_option
        (Some (list [1; 2; 3]))
        (sequence (list [Some 1; Some 2; Some 3])));

  "ex 4.5" >::
    (fun _ ->
      let parse_ints ss = traverse ss (fun s ->
        try Some (int_of_string s) with _ -> None) in
      assert_equal ~printer:string_of_int_list_option
        (Some (list [1; 2; 3]))
        (parse_ints (list ["1"; "2"; "3"]));
      assert_equal ~printer:string_of_int_list_option
        None
        (parse_ints (list ["1"; "a"; "3"])));
]
