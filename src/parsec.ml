let combine_or a b text = match a text with
  | Some(t) -> Some(t)
  | None -> b text

let predicat (f:(unit->bool)) text= 
	if f () then 
		Some(text)
	else 
		None
;;

let (|:|) a b = combine_or a b


let concat a b text =
  let rest_b = a text in
    match rest_b with
      | Some seq -> (
          match (b seq) with
            | Some i -> Some i
            | None -> None
        )
      | None -> None
;;
let (|.|) a b = concat a b
;;


let star a text =

  let rec star_loop a following = match (a following) with
    | Some t -> star_loop a t
    | None -> Some following
  in

  match a text with
    | Some t -> star_loop a t
    | None -> None
;;

(* Reconnais le texte s'il n'est pas ENTIEREMENT reconnu par le parser de toute la séquence*)
let not a text =
  match a text with
  | None -> Some []
  | Some y -> ( 
    match y with
    |[] -> None 
    | _ -> Some []
  )  
;;

