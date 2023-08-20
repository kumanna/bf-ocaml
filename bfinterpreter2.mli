type t

val create : int -> string -> t
val interpret : t -> unit
val tape_as_string : t -> string
