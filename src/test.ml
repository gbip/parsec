open OUnit2
open Parsec
open Printf
open Text

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

let test_parsec_predicat f text result_expected ctx=
	let result=predicat f text in
	let () = OUnit2.assert_equal result result_expected in
		()

let parser_predicat_test=
	(*let f =(Random.int 2) mod 2 = 0 in*)
	let f_1 () = (2 mod 2 )= 0 in
	let f_2 ()= (1 mod 2 ) = 0 in
	"Integration test">:::
	["Parsec">:: (test_parsec_predicat f_1 ['a'] (Some(['a'])));
	 "Parsec">:: (test_parsec_predicat f_1 ['a','b'] (Some(['a','b'])));
	 "Parsec">:: (test_parsec_predicat f_1 [] (Some([])));
	 "Parsec">:: (test_parsec_predicat f_2 ['a'] None);
	 "Parsec">:: (test_parsec_predicat f_2 ['a','b'] None);
	 "Parsec">:: (test_parsec_predicat f_2 [] None)
	]
(* Test de "star parse_a" ( qui doit reconnaître a(star) ) *)
let test_parsec_star_a text result_expected ctx =
  let parse_a = char_parser 'a' in
  let result = star parse_a text in
  let () = OUnit2.assert_equal result result_expected in
    ()

(* Test de "star parser_abc" ( qui doit reconnaître (abc)(star) ) *)
let test_parsec_star_abc text result_expected ctx =
  let result = star parser_abc text in
  let () = OUnit2.assert_equal result result_expected in
    ()

let parser_combinator_star_test =
  "Integration test">:::
  ["Parsec">:: (test_parsec_star_a ['a'] (Some []) );
   "Parsec">:: (test_parsec_star_a ['b'] None);
   "Parsec">:: (test_parsec_star_a ['a';'b'] (Some ['b']) );
   "Parsec">:: (test_parsec_star_a ['a'; 'a'; 'b'] (Some ['b']) );
   "Parsec">:: (test_parsec_star_a ['a'; 'a'; 'a'; 'a'; 'a'; 'a'; 'a'; 'a'; 'b'] (Some ['b']) );
   "Parsec">:: (test_parsec_star_a ['b'; 'a'; 'a'; 'a'; 'a'; 'a'; 'a'; 'a'; 'a'; 'b'] (None) );
   "Parsec">:: (test_parsec_star_abc ['a'; 'b'; 'c'] (Some []) );
   "Parsec">:: (test_parsec_star_abc ['a'; 'a'; 'b'] None );
   "Parsec">:: (test_parsec_star_abc ['a'; 'b'; 'c'; 'a'; 'b'; 'c'; 'a'; 'b'; 'c'] (Some []) );
   "Parsec">:: (test_parsec_star_abc ['a'; 'b'; 'd'; 'a'; 'b'; 'c'; 'a'; 'b'; 'c'; 'a'; 'b'; 'c'] None );
   "Parsec">:: (test_parsec_star_abc ['a'; 'b'; 'c'; 'a'; 'b'; 'c'; 'a'; 'b'; 'd'; 'a'; 'b'; 'c'] (Some ['a'; 'b'; 'd'; 'a'; 'b'; 'c']) );
  ]
;;
(* Ne marche pas car signature différente 
(* Test de not parse_a" *)
let test_parsec_not_a text result_expected ctx =
  let parse_a = char_parser 'a' in
  let result = not parse_a text in
  let () = OUnit2.assert_equal result result_expected in
    ()

(* Test de not parser_abc"  *)
let test_parsec_not_abc text result_expected ctx =
  let result = not parser_abc text in
  let () = OUnit2.assert_equal result result_expected in
    ()

let parser_combinator_not_test =
  "Integration test">:::
  ["Parsec">:: (test_parsec_not_a ['a'] (None) );
   "Parsec">:: (test_parsec_not_a ['b'] (Some []));
   "Parsec">:: (test_parsec_not_a ['a';'b'] (Some []) );
   "Parsec">:: (test_parsec_not_a ['b'; 'a'] (Some []) );
   "Parsec">:: (test_parsec_not_a ['a'; 'l'; 'a'; 't'; 'a'] (Some []) );
   "Parsec">:: (test_parsec_not_a ['k'; 't'; 'u'; 'n'; 'a'; 's'; 'm'; 'r'] (Some []) );
   "Parsec">:: (test_parsec_not_abc ['a'; 'b'; 'c'] (None) );
   "Parsec">:: (test_parsec_not_abc ['a'; 'a'; 'b'] (Some []));
   "Parsec">:: (test_parsec_not_abc ['a'; 'b'; 'c'; 'a'] (Some []) );
   "Parsec">:: (test_parsec_not_abc ['a'; 'b'; 'd'; 'a'; 'b'] (Some []));
   "Parsec">:: (test_parsec_not_abc ['k'; 't'; 'u'; 'n'; 'a'; 's'; 'm'; 'r'] (Some[]) );
  ]
;;
*)





let test_parser prs text result_expected ctx =
    let result = prs text in
    let () = OUnit2.assert_equal result result_expected in
    ()



let parser_combinator_integration_test = 
  "Integration test">:::
    [
        (* "ab" | "def" *)
        "Parsec">:: (test_parser ((char_parser 'a' |.| char_parser 'b') |:| parser_def) ['a';'b';'k'] (Some ['k']));
        "Parsec">:: (test_parser ((char_parser 'a' |.| char_parser 'b') |:| parser_def) ['d';'e';'f';'k'] (Some ['k']));
        "Parsec">:: (test_parser ((char_parser 'a' |.| char_parser 'b') |:| parser_def) ['c';'e';'f';'k'] (None));
        "Parsec">:: (test_parser ((char_parser 'a' |.| char_parser 'b') |:| parser_def) ['f';'b'] (None));

        (* ( "a" | "b" ).( "def" | "c")  *)
        "Parsec">:: (test_parser ((char_parser 'a' |:| char_parser 'b') |.| (parser_def |:| char_parser 'c')) ['a';'c'] (Some []));
        "Parsec">:: (test_parser ((char_parser 'a' |:| char_parser 'b') |.| (parser_def |:| char_parser 'c')) ['b';'c'] (Some []));
        "Parsec">:: (test_parser ((char_parser 'a' |:| char_parser 'b') |.| (parser_def |:| char_parser 'c')) ['a';'d';'e';'f'] (Some []));
        "Parsec">:: (test_parser ((char_parser 'a' |:| char_parser 'b') |.| (parser_def |:| char_parser 'c')) ['b';'d';'e';'f'] (Some []));

        (* ( "a" | "b"* ).( "def"* | "c")  *)
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['a';'c'] (Some []));
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['a'; 'a'; 'c'] None);
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['b'; 'b'; 'c'] (Some []));
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['b';'b';'a';'c'] None );
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['a';'d';'e';'f';'d';'e';'f';'d';'e';'f'] (Some []));
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['a';'d';'e';'f';'d';'e';'f';'d';'e';'f';'c'] (Some ['c']));
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['b';'b';'d';'e';'f';'d';'e';'f';'c'] (Some ['c']));
        "Parsec">:: (test_parser ((char_parser 'a' |:| star (char_parser 'b')) |.| (star parser_def |:| char_parser 'c')) ['b';'b';'a';'d';'e';'f';'c'] None );
        
    ]
;;


let () =
  OUnit2.run_test_tt_main parser_combinator_or_test;
  OUnit2.run_test_tt_main parser_combinator_concat_test;
  OUnit2.run_test_tt_main parser_combinator_star_test;
  OUnit2.run_test_tt_main parser_combinator_integration_test;
	OUnit2.run_test_tt_main parser_predicat_test;
;;

