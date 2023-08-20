type t

val create : int -> string -> t

(* val increment_data_pointer : t -> t *)
(* val decrement_data_pointer : t -> t *)
(* val increment_value_at_pointer : t -> t *)
(* val decrement_value_at_pointer : t -> t *)
(* val print_current_value : t -> unit *)
val tape_as_string : t -> string
val interpret : t -> unit
