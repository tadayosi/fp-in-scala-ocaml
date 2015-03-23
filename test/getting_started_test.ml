open OUnit2
open Getting_started

let tests = "Chapter 2" >::: [
  "abs" >::
    (fun _ -> assert_equal 42 (abs (-42)));

  "factorial" >::
    (fun _ -> assert_equal 120 (factorial 5));

  "fib" >::
    (fun _ -> assert_equal [0; 1; 1; 2; 3; 5]
      (List.map fib [0; 1; 2; 3; 4; 5]));

  "find_first" >::
    (fun _ -> assert_equal 1
      (find_first [|7; 9; 13|] (fun x -> x = 9)));

  "is_sorted true" >::
    (fun _ -> assert_equal true
      (is_sorted [|1; 2; 3; 4; 5|] (fun x y -> x <= y)));
  "is_sorted false" >::
    (fun _ -> assert_equal false
      (is_sorted [|3; 1; 2; 5; 4|] (fun x y -> x <= y)));

  "partial1" >::
    (fun _ -> assert_equal 3 ((partial1 1 (+)) 2));

  "curry" >::
    (fun _ -> assert_equal 3
      (curry (fun (a, b) -> a + b) 1 2));

  "uncurry" >::
    (fun _ -> assert_equal 3
      (uncurry (fun a b -> a + b) (1, 2)));

  "compose" >::
    (fun _ -> assert_equal "2"
      ((compose string_of_int (fun x -> x + 1)) 1));
]
