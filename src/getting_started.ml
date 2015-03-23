let abs n =
  if n < 0 then -n
  else n

let factorial n =
  let rec go n acc =
    if n <= 0 then acc
    else go (n - 1) n * acc in
  go n 1

(* ex 2.1 *)
let fib n =
  let rec f curr next = function
    | n' when n' < 0 -> raise (Invalid_argument "n < 0")
    | 0 -> curr
    | n' -> f next (curr + next) (n' - 1) in
  f 0 1 n

let find_first xs p =
  let rec loop n =
    if n >= Array.length xs then -1
    else if p xs.(n) then n
    else loop (n + 1) in
  loop 0

(* ex 2.2 *)
let is_sorted xs ordered =
  let rec f n =
    if n >= (Array.length xs) - 1 then true
    else if not (ordered xs.(n) xs.(n + 1)) then false
    else f (n + 1) in
  f 0

let partial1 a f =
  fun b -> f a b

(* ex 2.3 *)
let curry f = fun a -> fun b -> f (a, b)

(* ex 2.4 *)
let uncurry f = fun (a, b) -> f a b

(* ex 2.5 *)
let compose f g = fun a -> f (g a)
