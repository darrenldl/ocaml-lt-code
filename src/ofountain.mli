val max_data_block_count : int

val max_drop_count : int

module Param : sig
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

type drop

val data_of_drop : drop -> Cstruct.t

module Drop_set : Set.S with type elt = drop

type encode_error =
  [ `Inconsistent_data_block_size
  | `Invalid_drop_count
  | `Invalid_data_block_count
  | `Invalid_drop_data_buffer
  ]

val encode :
  ?systematic:bool ->
  ?drop_data_buffer:Cstruct.t array ->
  drop_count:int ->
  Cstruct.t array ->
  (Param.t * drop array, encode_error) result

val encode_lazy :
  ?systematic:bool ->
  ?drop_data_buffer:Cstruct.t array ->
  drop_count:int ->
  Cstruct.t array ->
  (Param.t * drop Seq.t, encode_error) result

val encode_with_param :
  ?drop_data_buffer:Cstruct.t array ->
  Param.t ->
  Cstruct.t array ->
  (drop array, encode_error) result

val encode_with_param_lazy :
  ?drop_data_buffer:Cstruct.t array ->
  Param.t ->
  Cstruct.t array ->
  (drop Seq.t, encode_error) result

type decode_error =
  [ `Invalid_drop_index
  | `Invalid_drop_count
  | `Invalid_data_block_buffer
  | `Invalid_data_block_size
  | `Invalid_drop_size
  | `Cannot_recover
  ]

val decode :
  ?data_block_buffer:Cstruct.t array ->
  Param.t ->
  Drop_set.t ->
  (Cstruct.t array, decode_error) result

type decode_ctx

val param_of_decode_ctx : decode_ctx -> Param.t

val data_block_size_of_decode_ctx : decode_ctx -> int

val drop_fill_count_of_decode_ctx : decode_ctx -> int

val data_blocks_of_decode_ctx : decode_ctx -> Cstruct.t array option

type decode_status =
  [ `Success of Cstruct.t array
  | `Ongoing
  ]

val make_decode_ctx :
  ?data_block_buffer:Cstruct.t array ->
  data_block_size:int ->
  Param.t ->
  (decode_ctx, decode_error) result

val decode_drop : decode_ctx -> drop -> (decode_status, decode_error) result
