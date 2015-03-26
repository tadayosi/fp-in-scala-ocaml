module List = struct

  type 'a list = Nil | Cons of 'a * 'a list

  let rec list xs = match xs with
    | [] -> Nil
    | x :: xs' -> Cons (x, list xs')

  let tail xs = match xs with
    | Nil -> Nil
    | Cons (_, xs') -> xs'

  let rec drop xs n =
    if n <= 0 then xs
    else match xs with
      | Nil -> Nil
      | Cons (_, xs') -> drop xs' (n - 1)

  let rec drop_while xs f = match xs with
    | Cons (x, xs') when f x -> drop_while xs' f
    | _ -> xs

  let set_head xs h = match xs with
    | Nil -> Cons (h, Nil)
    | Cons (_, xs') -> Cons (h, xs')

  let rec init xs = match xs with
    | Nil | Cons (_, Nil) -> Nil
    | Cons (x, xs') -> Cons (x, init xs')

  let rec fold_left xs z f = match xs with
    | Nil -> z
    | Cons (x, xs') -> fold_left xs' (f z x) f

  let rec fold_right xs z f = match xs with
    | Nil -> z
    | Cons (x, xs') -> f x (fold_right xs' z f)

  let length xs =
    fold_left xs 0 (fun len _ -> len + 1)

  let reverse xs =
    fold_left xs Nil (fun xs' x -> Cons (x, xs'))

  let fold_right_tailrec xs z f =
    fold_left (reverse xs) z (fun x y -> f y x)

  let append xs ys =
    fold_right_tailrec xs ys (fun x ys' -> Cons (x, ys'))

  let concat xss =
    fold_right_tailrec xss Nil append

  let map xs f =
    fold_right_tailrec xs Nil (fun x xs' -> Cons (f x, xs'))

  let flat_map xs f =
    concat (map xs f)

  let filter xs f =
    flat_map xs (fun x -> if f x then list [x] else Nil)

  let rec zip_with xs1 xs2 f = match (xs1, xs2) with
    | (Nil, _) -> Nil
    | (_, Nil) -> Nil
    | (Cons (x1, xs1'), Cons (x2, xs2')) -> Cons (f x1 x2, zip_with xs1' xs2' f)

  let rec has_subsequence sup sub =
    let rec starts_with xs p = match (xs, p) with
      | (_, Nil) -> true
      | (Cons (x', xs'), Cons (p1, p')) when x' = p1 ->
          starts_with xs' p'
      | _ -> false in
    match sup with
    | Nil -> sub = Nil
    | _ when starts_with sup sub -> true
    | Cons (h, t) -> has_subsequence t sub

  (* ------------------------------------------------------------------------ *)

  let sum ns =
    fold_left ns 0 (fun x y -> x + y)

  let product ns =
    fold_left ns 1.0 (fun x y -> x *. y)

  let add1 ns =
    fold_right_tailrec ns Nil (fun n ns' -> Cons (n + 1, ns'))

  let float_to_string ns =
    fold_right_tailrec ns Nil (fun n ss -> Cons (string_of_float n, ss))

  let rec add_pairwise ns1 ns2 =
    zip_with ns1 ns2 (fun x y -> x + y)

end
