module Ctx : sig
  type t

  type error =
    [ `Invalid_data_block_count
    | `Invalid_drop_count
    ]

  val systematic : t -> bool

  val data_block_count : t -> int

  val drop_count : t -> int

  val make :
    systematic:bool ->
    data_block_count:int ->
    drop_count:int ->
    (t, error) result
end

module Drop : sig
  type t

  type error =
    [ `Invalid_index
    | `Invalid_degree
    | `Invalid_data
    ]

  val make : index:int -> degree:int -> data:bytes -> (t, error) result

  val make_exn : index:int -> degree:int -> data:bytes -> t

  val index : t -> int

  val degree : t -> int

  val data : t -> bytes
end

module Drop_set : Set.S with type elt = Drop.t

type encode_error =
  [ `Inconsistent_data_block_size
  | `Invalid_drop_count
  | `Invalid_data_block_count
  ]

val encode :
  ?systematic:bool ->
  drop_count:int ->
  bytes array ->
  (Ctx.t * Drop.t array, encode_error) result

type decode_error =
  [ `Inconsistent_drop_size
  | `Invalid_drop_index
  | `Invalid_drop_count
  | `Insufficient_drops
  | `Invalid_data_block_buffer
  ]

val decode :
  ?data_block_buffer:bytes array ->
  Ctx.t ->
  Drop_set.t ->
  (bytes array, decode_error) result
