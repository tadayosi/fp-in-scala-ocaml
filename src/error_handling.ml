open Data_structures

module Option  = struct

  type 'a t = Some of 'a | None

  let map o f = match o with
    | None -> None
    | Some x -> Some (f x)

  let get_or_else o default = match o with
    | None -> Lazy.force default
    | Some x -> x

  let flat_map o f =
    get_or_else (map o f) (lazy None)

  let or_else o ob =
    get_or_else (map o (fun x -> Some x)) ob

  let filter o f =
    flat_map o (fun x -> if f x then Some x else None)

  let lift f = fun x -> map x f

  let map2 o1 o2 f =
    flat_map o1 (fun x -> map o2 (f x))

  let traverse xs f =
    List.fold_right_tailrec xs (Some List.Nil)
      (fun x lo -> map2 (f x) lo (fun x xs -> List.Cons (x, xs)))

  let sequence os =
    traverse os (fun x -> x)


  let string_of_option f o =
    get_or_else
      (map o (fun x -> "Some " ^ f x))
      (lazy "None")

  (* ---------------------------------------------------- *)

  let mean xs =
    if List.is_empty xs then None
    else Some (
        (List.fold_left xs 0. (+.))
        /.
        float_of_int (List.length xs))

  let variance xs =
    flat_map (mean xs)
      (fun m -> mean (List.map xs (fun x -> (x -. m) ** 2.)))

  let abs_o = lift abs_float

end

module Either = struct

  type ('a, 'b) t = Left of 'a | Right of 'b

  let try_ x =
    try Right (Lazy.force x)
    with e -> Left e

  let map e f = match e with
    | Left x -> Left x
    | Right x -> Right (f x)

  let flat_map e f = match e with
    | Left x -> Left x
    | Right x -> f x

  let or_else e1 e2 = match e1 with
    | Left _ -> Lazy.force e2
    | Right x -> Right x

  let map2 e1 e2 f =
    flat_map e1 (fun x -> map e2 (f x))

  let traverse xs f =
    List.fold_right_tailrec xs (Right List.Nil)
      (fun x le -> map2 (f x) le (fun x xs -> List.Cons (x, xs)))

  let sequence es =
    traverse es (fun x -> x)


  let string_of_either f g e = match e with
    | Left x -> "Left " ^ f x
    | Right x -> "Right " ^ g x

  (* ---------------------------------------------------- *)

  let mean xs =
    if List.is_empty xs then Left "mean of empty list!"
    else Right (
        (List.fold_left xs 0. (+.))
        /.
        float_of_int (List.length xs))

  let safe_div x y = try_ (lazy (x / y))

end
