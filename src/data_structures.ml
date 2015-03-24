type 'a list = Nil | Cons of 'a * 'a list

let rec sum ints = match ints with
  | Nil -> 0
  | Cons (x, xs) -> x + sum xs

let rec product fs = match fs with
  | Nil -> 1.0
  | Cons (0.0, _) -> 0.0
  | Cons (x, xs) -> x *. product xs

let rec list xs = match xs with
  | [] -> Nil
  | x :: xs' -> Cons(x, list xs')
