val max_data_block_count : int

val max_drop_count : int

module Param : sig
  type t

  type error =
    [ `Invalid_data_block_count
    | `Invalid_drop_count
    | `Invalid_layer_step_ratio
    ]

  val make :
    systematic:bool ->
    data_block_count:int ->
    drop_count:int ->
    layer_step_ratio:float ->
    (t, error) result

  val systematic : t -> bool

  val data_block_count : t -> int

  val drop_count : t -> int

  val layer_step_ratio : t -> float
end

(** {1 Basic types} *)

type drop

val data_of_drop : drop -> Cstruct.t

(** {1 Encoding} *)

type encode_error =
  [ `Inconsistent_data_block_size
  | `Invalid_drop_count
  | `Invalid_data_block_count
  | `Invalid_drop_data_buffer
  ]

(* val encode : *)
  (* ?systematic:bool -> *)
  (* ?drop_data_buffer:Cstruct.t array -> *)
  (* max_drop_count:int -> *)
  (* Cstruct.t array -> *)
  (* (Param.t * drop array, encode_error) result *)

type encoder

val create_encoder :
  data_blocks:Cstruct.t array ->
  drop_data_buffer:Cstruct.t array ->
  Param.t ->
  (encoder, encode_error) result

val reset_encoder : encoder -> unit

val param_of_encoder : encoder -> Param.t

val data_block_count_of_encoder : encoder -> int

val data_block_size_of_encoder : encoder -> int

val drop_count_of_encoder : encoder -> int

val data_blocks_of_encoder : encoder -> Cstruct.t array

val encode : encoder -> drop array

(** {1 Decoding} *)

type decode_error =
  [ `Invalid_drop_index
  | `Invalid_drop_size
  | `Invalid_data_blocks
  | `Cannot_recover
  ]

type decoder

val create_decoder :
  data_blocks:Cstruct.t array ->
  Param.t ->
  (decoder, decode_error) result

val reset_decoder : decoder -> unit

val param_of_decoder : decoder -> Param.t

val data_block_count_of_decoder : decoder -> int

val drop_count_of_decoder : decoder -> int

type decode_status =
  [ `Success of Cstruct.t array
  | `Ongoing
  ]

val decode_one : decoder -> drop -> (decode_status, decode_error) result
