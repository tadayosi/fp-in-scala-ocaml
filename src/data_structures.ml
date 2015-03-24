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

  let rec concat xss =
    fold_right_tailrec xss Nil append

  let rec sum ns =
    fold_left ns 0 (fun x y -> x + y)

  let rec product ns =
    fold_left ns 1.0 (fun x y -> x *. y)
end
