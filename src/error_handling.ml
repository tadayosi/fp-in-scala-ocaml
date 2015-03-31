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
    if List.length xs = 0 then None
    else Some (
      (List.fold_left xs 0. (+.))
      /.
      float_of_int (List.length xs))

  let variance xs =
    flat_map (mean xs)
      (fun m -> mean (List.map xs (fun x -> (x -. m) ** 2.)))

  let abs_o = lift abs_float

end
