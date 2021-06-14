type t

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  ]

val systematic : t -> bool

val data_block_count : t -> int

val drop_count : t -> int

val unsolved_data_blocks_init : t -> Int_set.t

val make :
  systematic:bool -> data_block_count:int -> drop_count:int -> (t, error) result
