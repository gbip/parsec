
(* ------ Infix Operators ------- *)
val (|:|) : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option

val(|.|) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option

