open QCheck

let test = mk_test ~n:10
  ~name:"add"
  Arbitrary.(pair small_int small_int)
  (fun (x, y) -> (+) x y = x + y)

let _ = run_tests [ test ]
