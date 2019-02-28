let combine_or a b text = match a text with
  | Some(t) -> Some(t)
  | None -> b text

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
