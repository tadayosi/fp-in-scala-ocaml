module Option  = struct

  type 'a t = Some of 'a | None

  let string_of f o = match o with
    | None -> "None"
    | Some x -> "Some (" ^ f x ^ ")"

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

  (* ------------------------------------------------------------------------ *)

  let mean xs =
    if List.length xs = 0 then None
    else Some (
      (List.fold_left (+.) 0. xs)
      /.
      float_of_int (List.length xs))

  let variance xs =
    flat_map (mean xs)
      (fun m -> mean (List.map (fun x -> (x -. m) ** 2.) xs))

end
