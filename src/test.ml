open OUnit2
open Parsec


let char_parser c text = match text with 
    | a :: rest -> if a == c then Some(rest) else None
    | _ -> None

let test_parsec_or text result_expected ctx =
    let parse_a = char_parser 'a' in
    let parse_b = char_parser 'b' in
    let result = (parse_a |:| parse_b) text in
    let () = OUnit2.assert_equal result result_expected in
    ()


let integration_tests =
  "Integration test">:::
      ["Graph1">:: (test_parsec_or ['a'] (Some [] ));
   "Graph13">:: (test_parsec_or ['b'] (Some [] ));
   "Graph13">:: (test_parsec_or ['a';'b'] (Some ['b'] ));
   "Graph13">:: (test_parsec_or ['b'; 'a'] (Some ['a'] ))
  ]
;;

let () =
  OUnit2.run_test_tt_main integration_tests
;;

