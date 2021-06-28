type rng

val create_rng : int -> rng

val gen_int : rng -> int -> int

val gen_int_global : int -> int

val hash_int : int -> int
