open OUnit2
open QCheck
open Data_structures

let sum_prop = mk_test ~n:100 ~name:"sum"
  Arbitrary.(triple small_int small_int small_int)
  (fun (a, b, c) -> sum(Cons(a, Cons(b, Cons(c, Nil)))) = a + b + c)

let product_prop = mk_test ~n:100 ~name:"product"
  Arbitrary.(triple (float 10.0) (float 10.0) (float 10.0))
  (fun (a, b, c) ->
    let res = product(Cons(a, Cons(b, Cons(c, Nil)))) in
    let act = a *. b *. c in
    act -. 0.01 < res && res < act +. 0.01)

let list_prop = mk_test ~n:100 ~name:"list"
  Arbitrary.(triple small_int small_int small_int)
  (fun (a, b, c) -> list [a; b; c] = Cons(a, Cons(b, Cons(c, Nil))))

let props = [
  sum_prop;
  product_prop;
  list_prop;
]

let tests = "Chapter 3" >::: [
  "ex 3.1" >::
    (fun _ -> assert_equal 3
      (match list [1; 2; 3; 4; 5] with
        | Cons(x, Cons(2, Cons(4, _))) -> x
        | Nil -> 42
        | Cons(x, Cons(y, Cons(3, Cons(4, _)))) -> x + y
        | Cons(h, t) -> h + sum t
        | _ -> 101));
]
