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
end = struct
  type t = {
    systematic : bool;
    data_block_count : int;
    drop_count : int;
    layer_step_ratio : float;
  }

  type error =
    [ `Invalid_data_block_count
    | `Invalid_drop_count
    | `Invalid_layer_step_ratio
    ]

  let make ~systematic ~data_block_count ~drop_count ~layer_step_ratio =
    if
      data_block_count <= 0 || data_block_count > Constants.max_data_block_count
    then Error `Invalid_data_block_count
    else if drop_count <= 0 || data_block_count > Constants.max_data_block_count
    then Error `Invalid_drop_count
    else if layer_step_ratio <= 0.1 || layer_step_ratio > 1.0 then
      Error `Invalid_layer_step_ratio
    else Ok { systematic; data_block_count; drop_count; layer_step_ratio }

  let systematic x = x.systematic

  let layer_step_ratio x = x.layer_step_ratio

  let data_block_count x = x.data_block_count

  let drop_count x = x.drop_count
end

let calc_step_size (param : Param.t) =
  float_of_int (Param.data_block_count param) *. Param.layer_step_ratio param

let calc_layer_count param =
  let drop_count = Param.drop_count param in
  let step_size = calc_step_size param in
  int_of_float @@ Float.ceil (float_of_int drop_count /. step_size)

let calc_data_block_layer_size (param : Param.t) i =
  let data_block_count = Param.data_block_count param in
  let drop_count = Param.drop_count param in
  let step_size = calc_step_size param in
  let layer_count = calc_layer_count param in
  assert (0 <= i && i < layer_count);
  if i < layer_count - 1 then
    data_block_count + ((i + 1) * int_of_float step_size)
  else drop_count

let calc_drop_data_buffer_layer_size (param : Param.t) i =
  calc_data_block_layer_size param (i + 1)

module Encode = struct
  type encoder = {
    param : Param.t;
    lt_encoders : Lt_code.encoder array;
    data_block_layers : Cstruct.t array array;
    drop_data_buffer_layers : Cstruct.t array array;
  }

  type error =
    [ `Inconsistent_data_block_size
    | `Invalid_drop_count
    | `Invalid_data_block_count
    | `Invalid_drop_data_buffer
    ]

  let create_encoder ~data_blocks ~drop_data_buffer param :
      (encoder, error) result =
    if Array.length data_blocks <> Param.data_block_count param then
      Error `Invalid_data_block_count
    else if not (Utils.cstruct_array_is_consistent data_blocks) then
      Error `Inconsistent_data_block_size
    else
        if
          not (
          Array.length drop_data_buffer = Param.drop_count param
          && Cstruct.length drop_data_buffer.(0)
             = Cstruct.length data_blocks.(0)
          && Utils.cstruct_array_is_consistent drop_data_buffer
    )
        then
        Error `Invalid_drop_data_buffer
        else (
          Utils.zero_cstruct_array drop_data_buffer;
          let layer_count = calc_layer_count param in
          let data_block_size = Cstruct.length data_blocks.(0) in
          let data_block_layers, drop_data_buffer_layers =
            if Param.systematic param then
              let data_block_layers =
                let prev_layer = ref data_blocks in
                Array.init layer_count (fun i ->
                    if i = 0 then data_blocks
                    else
                      let layer_size = calc_data_block_layer_size param i in
                      let res =
                        Array.init layer_size (fun i ->
                            if i < Array.length !prev_layer then !prev_layer.(i)
                            else Cstruct.create data_block_size)
                      in
                      prev_layer := res;
                      res)
              in
              let drop_data_buffer_layers =
                Array.init layer_count (fun i ->
                    if i = layer_count - 1 then drop_data_buffer
                    else data_block_layers.(i + 1))
              in
              (data_block_layers, drop_data_buffer_layers)
            else
              let data_block_layers =
                Array.init layer_count (fun i ->
                    let layer_size = calc_data_block_layer_size param i in
                    Array.init layer_size (fun _ ->
                        Cstruct.create data_block_size))
              in
              let drop_data_buffer_layers =
                Array.init layer_count (fun i ->
                    let layer_size = calc_drop_data_buffer_layer_size param i in
                    Array.init layer_size (fun _ ->
                        Cstruct.create data_block_size))
              in
              (data_block_layers, drop_data_buffer_layers)
          in
          let lt_encoders =
            Array.init layer_count (fun i ->
                let data_block_count = calc_data_block_layer_size param i in
                let max_drop_count = calc_drop_data_buffer_layer_size param i in
                let param =
                  Lt_code.Param.make ~systematic:(Param.systematic param)
                    ~data_block_count
                    ~max_drop_count
                in
                Lt_code.create_encoder ~data_blocks:data_block_layers.(i)
                  ~drop_data_buffer:drop_data_buffer_layers.(i)
                  param)
          in
          Ok { param; lt_encoders; data_block_layers; drop_data_buffer_layers }
        )

  let encode (encoder : encoder) : unit =
    Array.iter Lt_code.encode_all encoder.lt_encoders
end

module Decode = struct
  type decoder = {
    param : Param.t;
    lt_decoders : Lt_code.decoder array;
    data_block_layers : Cstruct.t array array;
  }

  type error =
    [ `Invalid_drop_size
    | `Invalid_data_blocks
    ]

  let create_decoder ~data_blocks param : (decoder, error) result =
        if
          not (
          Array.length data_blocks = Param.drop_count param
          && Utils.cstruct_array_is_consistent data_blocks
    )
        then
          Error `Invalid_data_blocks
        else (
          Utils.zero_cstruct_array data_blocks;
          let layer_count = calc_layer_count param in
          let data_block_size = Cstruct.length data_blocks.(0) in
          let data_block_layers =
            if Param.systematic param then
              let prev_layer = ref data_blocks in
              Array.init layer_count (fun i ->
                  if i = 0 then data_blocks
                  else
                    let layer_size = calc_data_block_layer_size param i in
                    let res =
                      Array.init layer_size (fun i ->
                          if i < Array.length !prev_layer then !prev_layer.(i)
                          else Cstruct.create data_block_size)
                    in
                    prev_layer := res;
                    res)
            else
              Array.init layer_count (fun i ->
                  let layer_size = calc_data_block_layer_size param i in
                  Array.init layer_size (fun _ ->
                      Cstruct.create data_block_size))
          in
          let lt_decoders =
            Array.init layer_count (fun i ->
                let data_block_count = calc_data_block_layer_size param i in
                let max_drop_count = calc_drop_data_buffer_layer_size param i in
                let param =
                  Lt_code.Param.make ~systematic:(Param.systematic param)
                    ~data_block_count
                    ~max_drop_count
                in
              Lt_code.create_decoder ~data_blocks:(data_block_layers.(i)) param
              )
          in
          Ok {
  param;
  lt_decoders;
  data_block_layers;
  }
          )
end
