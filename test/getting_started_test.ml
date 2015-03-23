open OUnit2
open Getting_started

let tests = "Chapter 2" >::: [
  "fib" >::
    (fun _ -> assert_equal [0; 1; 1; 2; 3; 5]
      (List.map fib [0; 1; 2; 3; 4; 5]));

  "is_sorted true" >::
    (fun _ -> assert_equal true
      (is_sorted [|1; 2; 3; 4; 5|] (fun x y -> x <= y)));
  "is_sorted false" >::
    (fun _ -> assert_equal false
      (is_sorted [|3; 1; 2; 5; 4|] (fun x y -> x <= y)));

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
