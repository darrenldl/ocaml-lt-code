type state

val make : int -> state

val gen_int : state -> int -> int

val gen_int64 : state -> int64 -> int64

val gen_int_global : int -> int

val gen_int64_global : int64 -> int64
