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

  let rec append xs ys = match xs with
    | Nil -> ys
    | Cons (x, xs') -> Cons (x, append xs' ys)

  let set_head xs h = match xs with
    | Nil -> Cons (h, Nil)
    | Cons (_, xs') -> Cons (h, xs')

  let rec init xs = match xs with
    | Nil | Cons (_, Nil) -> Nil
    | Cons (x, xs') -> Cons (x, init xs')

  let rec sum ints = match ints with
    | Nil -> 0
    | Cons (x, xs) -> x + sum xs

  let rec product fs = match fs with
    | Nil -> 1.0
    | Cons (0.0, _) -> 0.0
    | Cons (x, xs) -> x *. product xs
end
