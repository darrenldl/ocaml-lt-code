module Param = Param
module Drop_set = Drop_set

let get_data_block_indices (param : Param.t) (t : Drop.t) : Int_set.t =
  let systematic = Param.systematic param in
  let data_block_count = Param.data_block_count param in
  let rec aux cur degree acc =
    if cur < degree then
      let x = Random.int data_block_count in
      aux (succ cur) degree (Int_set.add x acc)
    else acc
  in
  if systematic && Drop.index t < data_block_count then
    Int_set.add (Drop.index t) Int_set.empty
  else (
    Random.init (Drop.index t);
    aux 0 (Drop.degree t) Int_set.empty)

module Encode = struct
  type error =
    [ `Inconsistent_data_block_size
    | `Invalid_drop_count
    | `Invalid_data_block_count
    | `Invalid_drop_data_buffer
    ]

  let gen_degrees (param : Param.t) : int array =
    let data_block_count = Param.data_block_count param in
    let drop_count = Param.drop_count_limit param in
    let systematic = Param.systematic param in
    let degrees =
      if systematic then (
        let degrees = Array.make drop_count 1 in
        let n = drop_count - data_block_count in
        let degrees' = Dist.choose_n (Param.dist param) n in
        Array.blit degrees' 0 degrees data_block_count n;
        degrees)
      else Dist.choose_n (Param.dist param) drop_count
    in
    (* fix a random drop to be degree 1 to ensure decoding is at least possible *)
    if not systematic then degrees.(Random.int drop_count) <- 1;
    degrees

  type encoder = {
    param : Param.t;
    degrees : int array;
    data_blocks : Cstruct.t array;
    drop_data_buffer : Cstruct.t array;
    mutable cur_drop_index : int;
  }

  let make_encoder ?(drop_data_buffer : Cstruct.t array option)
      (param : Param.t) data_blocks : (encoder, error) result =
    if Array.length data_blocks <> Param.data_block_count param then
      Error `Invalid_data_block_count
    else
      let data_block_len = Cstruct.length data_blocks.(0) in
      if not (Utils.cstruct_array_is_consistent data_blocks) then
        Error `Inconsistent_data_block_size
      else
        let drop_count = Param.drop_count_limit param in
        let degrees = gen_degrees param in
        let drop_data_buffer =
          match drop_data_buffer with
          | None ->
              Ok
                (Array.init drop_count (fun _ -> Cstruct.create data_block_len))
          | Some buffer ->
              if
                Array.length buffer = Param.drop_count_limit param
                && Cstruct.length buffer.(0) = Cstruct.length data_blocks.(0)
                && Utils.cstruct_array_is_consistent buffer
              then (
                Utils.zero_cstruct_array buffer;
                Ok buffer)
              else Error `Invalid_drop_data_buffer
        in
        match drop_data_buffer with
        | Error e -> Error e
        | Ok drop_data_buffer ->
            Ok
              {
                param;
                degrees;
                drop_data_buffer;
                data_blocks;
                cur_drop_index = 0;
              }

  let reset_encoder (encoder : encoder) : unit =
    Utils.zero_cstruct_array encoder.drop_data_buffer;
    encoder.cur_drop_index <- 0

  let encode_one_drop (encoder : encoder) : Drop.t option =
    let drop_count = Param.drop_count_limit encoder.param in
    let index = encoder.cur_drop_index in
    if index < drop_count then (
      let degree = encoder.degrees.(index) in
      let drop_data = encoder.drop_data_buffer.(index) in
      let drop = Drop.make_exn ~index ~degree ~data:drop_data in
      Int_set.iter
        (fun i -> Utils.xor_onto ~src:encoder.data_blocks.(i) ~onto:drop_data)
        (get_data_block_indices encoder.param drop);
      encoder.cur_drop_index <- encoder.cur_drop_index + 1;
      Some drop)
    else None

  let encode ?(systematic = true) ?drop_data_buffer ~drop_count_limit
      (data_blocks : Cstruct.t array) : (Param.t * Drop.t array, error) result =
    match
      Param.make ~systematic ~data_block_count:(Array.length data_blocks)
        ~drop_count_limit
    with
    | Error e -> (
        match e with
        | (`Invalid_data_block_count | `Invalid_drop_count) as e ->
            Error (e :> error))
    | Ok param -> (
        match make_encoder ?drop_data_buffer param data_blocks with
        | Error e -> Error (e :> error)
        | Ok encoder ->
            let arr =
              Array.init drop_count_limit (fun _ ->
                  Option.get @@ encode_one_drop encoder)
            in
            Ok (param, arr))
end

module Decode = struct
  type error =
    [ `Invalid_drop_index
    | `Invalid_drop_count
    | `Invalid_data_block_buffer
    | `Invalid_data_block_size
    | `Invalid_drop_size
    | `Cannot_recover
    ]

  module Graph = struct
    type bucket = Int_set.t

    type t = {
      param : Param.t;
      data_block_is_solved : bool array;
      mutable data_block_solved_count : int;
      mutable drop_fill_count : int;
      data_edges : bucket array;
      drop_edges : bucket array;
    }

    let make param =
      let data_block_count = Param.data_block_count param in
      {
        param;
        data_block_is_solved = Array.make data_block_count false;
        data_block_solved_count = 0;
        drop_fill_count = 0;
        data_edges = Array.make data_block_count Int_set.empty;
        drop_edges = Array.make (Param.drop_count_limit param) Int_set.empty;
      }

    let reset (g : t) : unit =
      Utils.fill_array false g.data_block_is_solved;
      g.data_block_solved_count <- 0;
      g.drop_fill_count <- 0;
      Utils.fill_array Int_set.empty g.data_edges;
      Utils.fill_array Int_set.empty g.drop_edges

    let remove_edge ~drop_index ~data_index (g : t) : unit =
      g.drop_edges.(drop_index) <-
        Int_set.remove data_index g.drop_edges.(drop_index);
      g.data_edges.(data_index) <-
        Int_set.remove drop_index g.data_edges.(data_index)

    let add_drop (drop : Drop.t) (g : t) : unit =
      let drop_index = Drop.index drop in
      let data_indices = get_data_block_indices g.param drop in
      g.drop_edges.(drop_index) <- data_indices;
      Int_set.iter
        (fun data_index ->
          g.data_edges.(data_index) <-
            Int_set.add drop_index g.data_edges.(data_index))
        data_indices;
      g.drop_fill_count <- g.drop_fill_count + 1

    let mark_data_as_solved ~data_index (g : t) : unit =
      g.data_block_is_solved.(data_index) <- true;
      g.data_block_solved_count <- g.data_block_solved_count + 1

    let degree_of_drop ~drop_index (g : t) : int =
      Int_set.cardinal g.drop_edges.(drop_index)
  end

  type decoder = {
    param : Param.t;
    graph : Graph.t;
    data_block_size : int;
    data_blocks : Cstruct.t array;
    drops : Cstruct.t option array;
  }

  let make_decoder ?data_block_buffer ~data_block_size param :
      (decoder, error) result =
    let data_block_count = Param.data_block_count param in
    if data_block_size < 0 then Error `Invalid_data_block_size
    else
      let data_blocks =
        match data_block_buffer with
        | None ->
            Ok
              (Array.init data_block_count (fun _ ->
                   Cstruct.create data_block_size))
        | Some buffer ->
            if
              Array.length buffer = data_block_count
              && Cstruct.length buffer.(0) = data_block_size
              && Utils.cstruct_array_is_consistent buffer
            then (
              Utils.zero_cstruct_array buffer;
              Ok buffer)
            else Error `Invalid_data_block_buffer
      in
      match data_blocks with
      | Error e -> Error e
      | Ok data_blocks ->
          Ok
            {
              param;
              graph = Graph.make param;
              data_block_size;
              data_blocks;
              drops = Array.make (Param.drop_count_limit param) None;
            }

  let reset_decoder (decoder : decoder) : unit =
    Graph.reset decoder.graph;
    Utils.zero_cstruct_array decoder.data_blocks;
    Utils.fill_array None decoder.drops

  let add_drop (drop : Drop.t) (decoder : decoder) : unit =
    decoder.drops.(Drop.index drop) <- Some (Drop.data drop);
    Graph.add_drop drop decoder.graph

  let remove_solved_drop_edges ~drop_index (decoder : decoder) : unit =
    (* this essentially catches up the missed data propagation *)
    let data_indices = decoder.graph.drop_edges.(drop_index) in
    Int_set.iter
      (fun data_index ->
        if decoder.graph.data_block_is_solved.(data_index) then (
          Utils.xor_onto
            ~src:decoder.data_blocks.(data_index)
            ~onto:(Option.get decoder.drops.(drop_index));
          Graph.remove_edge ~drop_index ~data_index decoder.graph))
      data_indices

  let propagate_data_xor ~data_index (decoder : decoder) : unit =
    let data = decoder.data_blocks.(data_index) in
    Int_set.iter
      (fun drop_index ->
        Option.iter
          (fun onto ->
            Utils.xor_onto ~src:data ~onto;
            Graph.remove_edge ~drop_index ~data_index decoder.graph)
          decoder.drops.(drop_index))
      decoder.graph.data_edges.(data_index)

  type reduction_one_step_status =
    [ `Success
    | `Ongoing
    | `Need_more_drops
    ]

  let reduce_one_step (decoder : decoder) : reduction_one_step_status =
    let degree_1_found = ref false in
    Array.iteri
      (fun drop_index drop ->
        match drop with
        | None -> ()
        | Some drop_data ->
            if Graph.degree_of_drop ~drop_index decoder.graph = 1 then (
              degree_1_found := true;
              let data_index =
                Int_set.choose decoder.graph.drop_edges.(drop_index)
              in
              if not decoder.graph.data_block_is_solved.(data_index) then (
                Cstruct.blit drop_data 0
                  decoder.data_blocks.(data_index)
                  0 decoder.data_block_size;
                Graph.mark_data_as_solved ~data_index decoder.graph;
                propagate_data_xor ~data_index decoder)))
      decoder.drops;
    if !degree_1_found then `Ongoing
    else if
      decoder.graph.data_block_solved_count
      = Param.data_block_count decoder.param
    then `Success
    else `Need_more_drops

  type reduction_status =
    [ `Success
    | `Need_more_drops
    ]

  let reduce (decoder : decoder) : reduction_status =
    let rec aux () =
      match reduce_one_step decoder with
      | (`Success | `Need_more_drops) as s -> (s :> reduction_status)
      | `Ongoing -> aux ()
    in
    aux ()

  type status =
    [ `Success of Cstruct.t array
    | `Ongoing
    ]

  let data_is_ready (decoder : decoder) : bool =
    decoder.graph.data_block_solved_count = Param.data_block_count decoder.param

  let max_tries_reached (decoder : decoder) : bool =
    decoder.graph.drop_fill_count = Param.drop_count_limit decoder.param

  let decode_one_drop (decoder : decoder) (drop : Drop.t) :
      (status, error) result =
    if Cstruct.length (Drop.data drop) <> decoder.data_block_size then
      Error `Invalid_drop_size
    else
      let drop_index = Drop.index drop in
      if data_is_ready decoder then Ok (`Success decoder.data_blocks)
      else if max_tries_reached decoder then Error `Cannot_recover
      else
        match decoder.drops.(drop_index) with
        | Some _ -> Ok `Ongoing
        | None -> (
            add_drop drop decoder;
            remove_solved_drop_edges ~drop_index decoder;
            match reduce decoder with
            | `Success -> Ok (`Success decoder.data_blocks)
            | `Need_more_drops ->
                if max_tries_reached decoder then Error `Cannot_recover
                else Ok `Ongoing)

  let decode ?data_block_buffer (param : Param.t) (drops : Drop_set.t) :
      (Cstruct.t array, error) result =
    if Drop_set.cardinal drops = 0 then Error `Cannot_recover
    else
      match
        make_decoder
          ~data_block_size:(Cstruct.length @@ Drop.data @@ Drop_set.choose drops)
          ?data_block_buffer param
      with
      | Error e -> Error e
      | Ok decoder -> (
          let x = Drop_set.choose drops in
          let drops = Drop_set.remove x drops in
          Drop_set.iter
            (fun drop -> decode_one_drop decoder drop |> ignore)
            drops;
          match decode_one_drop decoder x with
          | Error e -> Error e
          | Ok `Ongoing -> Error `Cannot_recover
          | Ok (`Success arr) -> Ok arr)
end

let max_drop_count = Constants.max_drop_count

let max_data_block_count = Constants.max_data_block_count

type drop = Drop.t

let data_of_drop = Drop.data

type encode_error = Encode.error

type encoder = Encode.encoder

let make_encoder = Encode.make_encoder

let reset_encoder = Encode.reset_encoder

let param_of_encoder (encoder : Encode.encoder) = encoder.param

let data_block_count_of_encoder (encoder : Encode.encoder) =
  Param.data_block_count encoder.param

let drop_count_limit_of_encoder (encoder : Encode.encoder) =
  Param.drop_count_limit encoder.param

let data_block_size_of_encoder (encoder : Encode.encoder) =
  Cstruct.length encoder.data_blocks.(0)

let data_blocks_of_encoder (encoder : Encode.encoder) = encoder.data_blocks

let encode_one_drop = Encode.encode_one_drop

let encode = Encode.encode

type decode_error = Decode.error

type decoder = Decode.decoder

type decode_status = Decode.status

let make_decoder = Decode.make_decoder

let reset_decoder = Decode.reset_decoder

let param_of_decoder (decoder : Decode.decoder) = decoder.param

let data_block_count_of_decoder (decoder : Decode.decoder) =
  Param.data_block_count decoder.param

let drop_count_limit_of_decoder (decoder : Decode.decoder) =
  Param.drop_count_limit decoder.param

let data_block_size_of_decoder (decoder : Decode.decoder) =
  decoder.data_block_size

let drop_fill_count_of_decoder (decoder : Decode.decoder) =
  decoder.graph.drop_fill_count

let data_blocks_of_decoder (decoder : Decode.decoder) : Cstruct.t array option =
  if Decode.data_is_ready decoder then Some decoder.data_blocks else None

let decode_one_drop = Decode.decode_one_drop

let decode = Decode.decode
