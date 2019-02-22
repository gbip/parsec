open OUnit2
open Parsec

let test_parsec_or text ctx =
    ()


let integration_tests =
  "Integration test">:::
  ["Graph1">:: (test_parsec_or "aabbaa");
   "Graph13">:: (test_parsec_or "b")
  ]
;;

let () =
  OUnit2.run_test_tt_main integration_tests
;;

