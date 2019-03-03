let combine_or a b text = match a text with
  | Some(t) -> Some(t)
  | None -> b text

let predicat (f:(unit->bool)) (text:string)=
	if f () then
		Some(String.sub text 1 ((String.length text)-1))
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