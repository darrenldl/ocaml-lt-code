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

module Drop : sig
  type t

  type error =
    [ `Invalid_index
    | `Invalid_degree
    | `Invalid_data
    ]

  val make : index:int -> degree:int -> data:Cstruct.t -> (t, error) result

  val make_exn : index:int -> degree:int -> data:Cstruct.t -> t

  val index : t -> int

  val degree : t -> int

  val data : t -> Cstruct.t
end

module Drop_set : Set.S with type elt = Drop.t

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
  (Param.t * Drop.t array, encode_error) result

val encode_lazy :
  ?systematic:bool ->
  ?drop_data_buffer:Cstruct.t array ->
  drop_count:int ->
  Cstruct.t array ->
  (Param.t * Drop.t Seq.t, encode_error) result

val encode_with_param :
  ?drop_data_buffer:Cstruct.t array ->
  Param.t ->
  Cstruct.t array ->
  (Drop.t array, encode_error) result

val encode_with_param_lazy :
  ?drop_data_buffer:Cstruct.t array ->
  Param.t ->
  Cstruct.t array ->
  (Drop.t Seq.t, encode_error) result

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
