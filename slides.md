<!-- $theme: gaia -->
<!-- $size: 16:9 -->

<!-- *template: invert -->

# Combinaisons de parser

---

# Parser

```ocaml
(* Signature d'un parser *)
val char_parser : (char list -> char list option)

let char_parser_generator (c:char) (text:char list)  = match text with 
  | a :: rest -> if a == c then Some(rest) else None
  | _ -> None
  
 let char_parser = char_parser_generator 'a'
```
---

# Exemple

Dans cet exemple on génére un parser qui reconnaît le caractère `'a'`.
On utilise ce parser pour reconnaître le mot `"abc"`

```ocaml
(* Génération du parser *)
let parse_a = char_parser_generator 'a' in
(* Parse "abc" *)
let result = parse_a ['a', 'b', 'c'] in
(* Vérification du résultat *)
let () = assert_equals result (Some ['b', 'c'])
```
---

# Combinaisons de parser

On a défini dans le cours les opérations suivantes :
* étoile : `*`
* concaténation : `.`
* ou : `|`
* not : `!`

On s'intéresse aussi au cas du parser "prédicat"


---

# Combinaison par *

```ocaml
(* Définition *)
val star : ('a -> 'a option) -> 'a -> 'a option
```
--- 

# Combinaison par *

```ocaml
(* Défnition *)
val star : ('a -> 'a option) -> 'a -> 'a option
```

## Exemple

```ocaml
(* Création *)
let parse_a = char_parser 'a' in
let prs = star parse_a text in
(* Utilisation *)
let result = prs ['a'; 'a'; 'a'; 'b'; 'b'] in
let () = assert_equals result (Some ['b'; 'b']) in
let result = prs ['b', 'a'; 'a'] in
let () = assert_equals result (Some ['b'; 'a'; 'a'])
```


--- 


# Combinaison par le OU

```ocaml
(* Défnition *)
val (|:|) : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
```
--- 


# Combinaison par le OU

```ocaml
(* Défnition *)
val (|:|) : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
```


## Exemple

```ocaml
(* Création *)
let prs = (parse_a |:| parse_b) text in
(* Utilisation *)
let result = prs ['a', 'b'] in
let () = assert_equals result (Some ['b']) in
let result = prs ['b', 'c'] in
let () = assert_equals result (Some ['a']) i
```


--- 
# Combinaison par le NOT

```ocaml
val not : ('a -> 'a list option) -> 'a -> 'a list option
```


--- 
# Combinaison par le NOT

```ocaml
val not : ('a -> 'a list option) -> 'a -> 'a list option
```
## Exemple

```ocaml
(* Création *)
let prs = (! parse_a) text in
(* Utilisation *)
let result = prs ['a', 'b'] in
let () = assert_equals result (Some []) in
let result = prs ['a'] in
let () = assert_equals result None
```
---
# Combinaison par concaténation
```ocaml
(* définition *)
val (|.|) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option
```


--- 
# Combinaison par concaténation

```ocaml
(* définition *)
val (|.|) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option
```

## Exemple

```ocaml
(* Création *)
  let pars = (parser_abc |.| parser_def) in
(* Utilisation *)
  let result = pars ['a';'b';'c';'d';'e';'f';'g'] in
  let () = assert_equals result (Some ['g']) in
  let result = pars ['a';'b';'c';'e'] in
  let () = assert_equals result (None)
```

---
# Création d'un parser avec un prédicat

```ocaml
val predicat : (unit->bool) -> string -> string option
```
---

# Création d'un parser avec un prédicat

```ocaml
val predicat : (unit->bool) -> string -> string option
```

## Exemple

```ocaml
(* Fonction qui alterne vrai et faux *)
let f=	let count = ref 0 in
	fun () -> count := !count + 1 ;
		(!count mod 2) = 0 

let result_1= predicat f ['a','b'] in
	let () = assert_equals result_1 None

let result_2= predicat f ['a','b'] in
	let () = assert_equals result_2 (Some(['a','b']))
```

---

# Grammaire
On veut reconnaître la grammaire suivante :
```text
( a | b* ).( (def)* | c) 
```
Pour cela, on défini le parser suivant :
```ocaml
let prs =
(char_parser 'a' |:| star (char_parser 'b')) |.| 
(star parser_def |:| char_parser 'c')
```

---

# Exemple
On utilise le parser pour parser l'expression "adefdefdefc"
```ocaml
let result = prs ['a';'d';'e';'f';'d';'e';'f';'d';'e';'f';'c'] 
```
`result` vaut :
```
(Some ['c']));
```