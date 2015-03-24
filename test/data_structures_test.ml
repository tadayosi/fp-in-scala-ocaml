open OUnit2
open QCheck
open Data_structures.List

let list_prop = mk_test ~name:"list"
  Arbitrary.(triple small_int small_int small_int)
  (fun (a, b, c) -> list [a; b; c] = Cons (a, Cons (b, Cons (c, Nil))))

let tail_prop = mk_test ~name:"tail" ~pp:PP.(list int)
  Arbitrary.(list small_int)
  (fun xs -> match xs with
    | [] -> tail (list xs) = Nil
    | _ -> tail (list xs) = list (List.tl xs))

let drop_prop = mk_test ~name:"drop" ~pp:PP.(list int)
  Arbitrary.(list small_int)
  (fun xs -> drop (list xs) 3 = tail (tail (tail (list xs))))

let drop_while_prop = mk_test ~name:"drop_while" ~pp:PP.(list int)
  Arbitrary.(list small_int)
  Prop.(
    (fun xs -> drop_while (list xs) (fun _ -> true) = Nil) &&&
    (fun xs -> drop_while (list xs) (fun _ -> false) = (list xs)))

let append_prop = mk_test ~name:"append" ~pp:PP.(pair (list int) (list int))
  Arbitrary.(pair (list small_int) (list small_int))
  (fun (xs, ys) -> append (list xs) (list ys) = list (xs @ ys))

let set_head_prop = mk_test ~name:"set_head" ~pp:PP.(list int)
  Arbitrary.(list small_int)
  (fun xs -> match xs with
    | [] -> set_head (list xs) 1 = Cons (1, Nil)
    | _ -> set_head (list xs) 1 = list (1 :: List.tl xs))

let init_prop = mk_test ~name:"init" ~pp:PP.(pair (list int) int)
  Arbitrary.(pair (list small_int) small_int)
  (fun (xs, x) -> init (list (xs @ [x])) = list xs)

let length_prop = mk_test ~name:"length" ~pp:PP.(list int)
  Arbitrary.(list small_int)
  (fun xs -> length (list xs) = List.length xs)

let reverse_prop = mk_test ~name:"reverse" ~pp:PP.(list int)
  Arbitrary.(list small_int)
  (fun xs -> reverse (list xs) = list (List.rev xs))

let fold_right_tailrec_prop = mk_test ~name:"fold_right_tailrec" ~pp:PP.(list string)
  Arbitrary.(list string)
  (fun xs ->
    fold_right_tailrec (list xs) "" (fun x y -> x ^ y) =
    fold_right (list xs) "" (fun x y -> x ^ y))

let concat_prop = mk_test ~name:"concat" ~pp:PP.(list (list int))
  Arbitrary.(list (list small_int))
  (fun xss ->
    concat (list (List.map (fun xs -> list xs) xss)) =
    list (List.concat xss))

let sum_prop = mk_test ~name:"sum"
  Arbitrary.(triple small_int small_int small_int)
  (fun (a, b, c) -> sum(Cons (a, Cons (b, Cons (c, Nil)))) = a + b + c)

let product_prop = mk_test ~name:"product"
  Arbitrary.(triple (float 10.0) (float 10.0) (float 10.0))
  (fun (a, b, c) ->
    let res = product(Cons (a, Cons (b, Cons (c, Nil)))) in
    let act = a *. b *. c in
    act -. 0.01 < res && res < act +. 0.01)

let props = [
  list_prop;
  tail_prop;
  drop_prop;
  drop_while_prop;
  append_prop;
  set_head_prop;
  init_prop;
  length_prop;
  reverse_prop;
  fold_right_tailrec_prop;
  concat_prop;
  sum_prop;
  product_prop;
]

let tests = "Chapter 3" >::: [
  "ex 3.1" >::
    (fun _ -> assert_equal 3
      (match list [1; 2; 3; 4; 5] with
        | Cons (x, Cons (2, Cons (4, _))) -> x
        | Nil -> 42
        | Cons (x, Cons (y, Cons (3, Cons (4, _)))) -> x + y
        | Cons (h, t) -> h + sum t
        | _ -> 101));

  "ex 3.8" >::
    (fun _ -> let xs = list [1; 2; 3] in
      assert_equal xs
      (fold_right xs Nil (fun x y -> Cons (x, y))));
]
