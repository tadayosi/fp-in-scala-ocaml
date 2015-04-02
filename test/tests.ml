open OUnit2
open QCheck

(* QCheck properties *)
let _ = run_tests (
    Data_structures_test.props @
    Error_handling_option_test.props @
    Error_handling_either_test.props
  )

(* OUnit tests *)
let _ = run_test_tt_main ( "All tests" >::: [
    Getting_started_test.tests;
    Data_structures_test.tests;
    Error_handling_option_test.tests;
    Error_handling_either_test.tests;
  ])
