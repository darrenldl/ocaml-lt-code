type t

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  ]

val systematic : t -> bool

val data_block_count : t -> int

val drop_count_limit : t -> int

val dist : t -> Dist.t

val make :
  systematic:bool ->
  data_block_count:int ->
  drop_count_limit:int ->
  (t, error) result
