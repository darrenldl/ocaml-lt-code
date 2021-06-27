type rng

val create_rng : init_state:int -> stream_id:int -> rng

val gen_int : rng -> int -> int

val gen_int_global : int -> int

val hash_int : ?inc:int -> int -> int
