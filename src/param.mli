type t

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  ]

val systematic : t -> bool

val data_block_count : t -> int

val max_drop_count : t -> int

val dist : t -> Dist.t

val make :
  systematic:bool ->
  data_block_count:int ->
  max_drop_count:int ->
  (t, error) result
