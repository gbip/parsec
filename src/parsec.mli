val predicat : (unit->bool) -> string -> string option

val star : ('a -> 'a option) -> 'a -> 'a option

(* ------ Infix Operators ------- *)
val (|:|) : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option

val (|.|) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option

val not : ('a -> 'a list option) -> 'a -> 'a list option

