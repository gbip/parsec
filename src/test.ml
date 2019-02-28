open OUnit2
open Parsec
open Printf

(*
Format général des parsers de test :
Un parser prend un tableau de caractères (le texte à analyser)
Il retourne :
-> None si il n'a pas reconnu de mot
-> Some [] si il a reconnu toute la séquence
-> Some reste si il a reconnu un mot tel que le texte en entrée = mot@reste
(ie il a reconnu un mot en début de séquence et retourne ce qui vient après)
*)
let char_parser c text = match text with 
  | a :: rest -> if a == c then Some(rest) else None
  | _ -> None

let test_parsec_or text result_expected ctx =
  let parse_a = char_parser 'a' in
  let parse_b = char_parser 'b' in
  let result = (parse_a |:| parse_b) text in
  let () = OUnit2.assert_equal result result_expected in
    ()

let parser_combinator_or_test =
  "Integration test">:::
  ["Parsec">:: (test_parsec_or ['a'] (Some [] ));
   "Parsec">:: (test_parsec_or ['b'] (Some [] ));
   "Parsec">:: (test_parsec_or ['a';'b'] (Some ['b'] ));
   "Parsec">:: (test_parsec_or ['b'; 'a'] (Some ['a'] ));
   "Parsec">:: (test_parsec_or ['c'; 'a'] None);
   "Parsec">:: (test_parsec_or ['a'; 'c'] (Some ['c'] ));
   "Parsec">:: (test_parsec_or ['c'; 'c'] None);
   "Parsec">:: (test_parsec_or [] None)
  ]
;;

(* retourne Some(ce qui n'a pas été reconnu) *)
let parser_abc text (* un parser qui ne reconnait que la séquence "abc" *) =
  let rec loop expected rest =
    match rest with
      | (a::suite) -> if a == expected then 
            (
              if expected == 'a' then loop 'b' suite else
              if expected == 'b' then loop 'c' suite else
              if expected == 'c' then 
                Some suite (* mot reconnu, on retourne la suite de la séquence ! :D *)
              else
                failwith "unexpected situation"
            )
          else
            None (* mot non reconnu *)
      | _ -> None
  in
    loop 'a' text
;;

(* retourne Some(ce qui n'a pas été reconnu) *)
let parser_def text (* un parser qui ne reconnait que la séquence "def" *) =
  let rec loop expected rest =
    match rest with
      | (a::suite) -> if a == expected then 
            (
              if expected == 'd' then loop 'e' suite else
              if expected == 'e' then loop 'f' suite else
              if expected == 'f' then 
                Some suite (* mot reconnu, on retourne la suite de la séquence ! :D *)
              else
                failwith "unexpected situation"
            )
          else
            None (* mot non reconnu *)
      | _ -> None
  in
    loop 'd' text
;;

let test_parsec_concat text result_expected ctx =
  let result = (parser_abc |.| parser_def) text in
  let () = OUnit2.assert_equal result result_expected in
    ()


let parser_combinator_concat_test =
  "Integration test">:::
  ["Parsec">:: (test_parsec_concat ['a'] (None));
   "Parsec">:: (test_parsec_concat ['b'] (None));
   "Parsec">:: (test_parsec_concat ['a';'b'] (None));

   "Parsec">:: (test_parsec_concat ['a'; 'b';'c';'d';'e';'f'] (Some []));

   "Parsec">:: (test_parsec_concat ['a'; 'b';'k';'d';'e';'f';'g'] (None));
   "Parsec">:: (test_parsec_concat ['a'; 'b';'c';'d';'e';'f';'g'] (Some ['g']));

   "Parsec">:: (test_parsec_concat ['a'; 'b';'c';'d';'k';'f';'g'] None);
   "Parsec">:: (test_parsec_concat ['a'; 'b';'c';'d';'e';'f';'g';'h'] (Some ['g' ;'h']));
   "Parsec">:: (test_parsec_concat [] None)
  ]
;;

let () =
  OUnit2.run_test_tt_main parser_combinator_or_test;
  OUnit2.run_test_tt_main parser_combinator_concat_test
;;

