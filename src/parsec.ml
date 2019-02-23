let combine_or a b text = match a text with
    | Some(t) -> Some(t)
    | None -> b text

let (|:|) a b = combine_or a b
