module Param : sig
  type t

  val systematic : t -> bool

  val data_block_count : t -> int

  val max_drop_count : t -> int

  val dist : t -> Dist.t

  val make : systematic:bool -> data_block_count:int -> max_drop_count:int -> t
end = struct
  type t = {
    systematic : bool;
    data_block_count : int;
    max_drop_count : int;
    dist : Dist.t;
  }

  let systematic t = t.systematic

  let data_block_count t = t.data_block_count

  let max_drop_count t = t.max_drop_count

  let dist t = t.dist

  let make ~systematic ~data_block_count ~max_drop_count : t =
    assert (
      0 < data_block_count && data_block_count <= Constants.max_data_block_count);
    assert (
      data_block_count <= max_drop_count
      && max_drop_count <= Constants.max_drop_count);
    {
      systematic;
      data_block_count;
      max_drop_count;
      dist = Dist.robust_soliton_dist ~k:data_block_count;
    }
end

module Drop_set = Drop_set

let get_data_block_indices_onto (param : Param.t) (drop : Drop.t)
    (onto : int array) : unit =
  let systematic = Param.systematic param in
  let data_block_count = Param.data_block_count param in
  let rec aux rng degree set =
    if Hash_int_set.cardinal set < degree then (
      Hash_int_set.add set (Rand.gen_int_bounded rng);
      aux rng degree set)
  in
  let degree = Drop.degree drop in
  let drop_index = Drop.index drop in
  if systematic && drop_index < data_block_count then (
    assert (degree = 1);
    onto.(0) <- drop_index)
  else
    let set = Hash_int_set.create degree in
    let rng =
      Rand.create_bounded_rng ~bound:data_block_count (Drop.index drop)
    in
    aux rng degree set;
    let c = ref 0 in
    Hash_int_set.iter
      (fun i ->
        onto.(!c) <- i;
        c := !c + 1)
      set

let get_data_block_indices (param : Param.t) (drop : Drop.t) : int array =
  let arr = Array.make (Drop.degree drop) 0 in
  get_data_block_indices_onto param drop arr;
  arr

module Encode = struct
  let gen_degrees_onto (param : Param.t) onto : unit =
    let data_block_count = Param.data_block_count param in
    let max_drop_count = Param.max_drop_count param in
    assert (Array.length onto = max_drop_count);
    let systematic = Param.systematic param in
    if systematic then (
      for i = 0 to data_block_count - 1 do
        onto.(i) <- 1
      done;
      let n = max_drop_count - data_block_count in
      if n > 0 then (
        Dist.choose_onto ~offset:data_block_count (Param.dist param) onto;
        (* we amplify the coverage of the parity drops *)
        let parity_to_data_ratio = (data_block_count + n - 1) / n in
        let multiplier = parity_to_data_ratio * 2 in
        for i = data_block_count to max_drop_count - 1 do
          onto.(i) <- min data_block_count (onto.(i) * multiplier)
        done))
    else Dist.choose_onto (Param.dist param) onto;
    (* fix a random drop to be degree 1 to ensure decoding is at least possible *)
    if not systematic then onto.(Rand.gen_int_global max_drop_count) <- 1

  let gen_degrees (param : Param.t) : int array =
    let max_drop_count = Param.max_drop_count param in
    let arr = Array.make max_drop_count 0 in
    gen_degrees_onto param arr;
    arr

  type encoder = {
    param : Param.t;
    mutable degrees : int array;
    data_blocks : Cstruct.t array;
    drop_data_buffer : Cstruct.t array;
    mutable cur_drop_index : int;
  }

  let create_encoder ~data_blocks ~(drop_data_buffer : Cstruct.t array)
      (param : Param.t) : encoder =
    assert (Array.length data_blocks = Param.data_block_count param);
    assert (Utils.cstruct_array_is_consistent data_blocks);
    assert (Array.length drop_data_buffer = Param.max_drop_count param);
    assert (Cstruct.length drop_data_buffer.(0) = Cstruct.length data_blocks.(0));
    assert (Utils.cstruct_array_is_consistent drop_data_buffer);
    Utils.zero_cstruct_array drop_data_buffer;
    let degrees = gen_degrees param in
    { param; degrees; drop_data_buffer; data_blocks; cur_drop_index = 0 }

  let reset_encoder (encoder : encoder) : unit =
    Utils.zero_cstruct_array encoder.drop_data_buffer;
    encoder.cur_drop_index <- 0;
    gen_degrees_onto encoder.param encoder.degrees

  let get_drop (encoder : encoder) index : Drop.t =
    let drop_count = Param.max_drop_count encoder.param in
    assert (index < drop_count);
    let degree = encoder.degrees.(index) in
    let drop_data = encoder.drop_data_buffer.(index) in
    Drop.make_exn ~index ~degree ~data:drop_data

  let encode_one (encoder : encoder) : unit =
    let drop_count = Param.max_drop_count encoder.param in
    let index = encoder.cur_drop_index in
    if index < drop_count then (
      let drop = get_drop encoder index in
      let data_indices = get_data_block_indices encoder.param drop in
      if Array.length data_indices = 1 then
        let data_index = data_indices.(0) in
        Utils.blit_onto
          ~src:encoder.data_blocks.(data_index)
          ~onto:(Drop.data drop)
      else
        Array.iter
          (fun i ->
            Utils.xor_onto ~src:encoder.data_blocks.(i) ~onto:(Drop.data drop))
          data_indices;
      encoder.cur_drop_index <- encoder.cur_drop_index + 1)

  let encode_all (encoder : encoder) : unit =
    let drops_left =
      Param.max_drop_count encoder.param - encoder.cur_drop_index
    in
    for _ = 0 to drops_left - 1 do
      encode_one encoder
    done

  let get_remaining_drops (encoder : encoder) : Drop.t array =
    Array.init (Param.max_drop_count encoder.param) (fun i ->
        get_drop encoder i)
end

module Decode = struct
  type error = [ `Cannot_recover ]

  module Graph = struct
    type bucket = Hash_int_set.t

    type t = {
      param : Param.t;
      data_block_is_solved : bool array;
      mutable data_block_solved_count : int;
      mutable drop_fill_count : int;
      data_edges : bucket array;
      drop_edges : bucket array;
    }

    let create param =
      let data_block_count = Param.data_block_count param in
      let max_drop_count = Param.max_drop_count param in
      {
        param;
        data_block_is_solved = Array.make data_block_count false;
        data_block_solved_count = 0;
        drop_fill_count = 0;
        data_edges =
          Array.init data_block_count (fun _ ->
              Hash_int_set.create (max 1 (max_drop_count / 10)));
        drop_edges =
          Array.init max_drop_count (fun _ ->
              Hash_int_set.create (max 1 (data_block_count / 10)));
      }

    let reset (g : t) : unit =
      Utils.fill_array false g.data_block_is_solved;
      g.data_block_solved_count <- 0;
      g.drop_fill_count <- 0;
      Array.iter Hash_int_set.reset g.data_edges;
      Array.iter Hash_int_set.reset g.drop_edges

    let remove_edge ~data_index ~drop_index (g : t) : unit =
      Hash_int_set.remove g.drop_edges.(drop_index) data_index;
      Hash_int_set.remove g.data_edges.(data_index) drop_index

    let add_drop (drop : Drop.t) (g : t) : unit =
      let drop_index = Drop.index drop in
      let data_indices = get_data_block_indices g.param drop in
      Array.iter
        (fun data_index ->
          Hash_int_set.add g.drop_edges.(drop_index) data_index)
        data_indices;
      Array.iter
        (fun data_index ->
          Hash_int_set.add g.data_edges.(data_index) drop_index)
        data_indices;
      g.drop_fill_count <- g.drop_fill_count + 1

    let mark_data_as_solved ~data_index (g : t) : unit =
      g.data_block_is_solved.(data_index) <- true;
      g.data_block_solved_count <- g.data_block_solved_count + 1

    let degree_of_drop ~drop_index (g : t) : int =
      Hash_int_set.cardinal g.drop_edges.(drop_index)
  end

  type decoder = {
    param : Param.t;
    graph : Graph.t;
    data_block_size : int;
    data_blocks : Cstruct.t array;
    drops : Cstruct.t option array;
  }

  let create_decoder ~data_block_buffer param : decoder =
    let data_block_count = Param.data_block_count param in
    assert (Array.length data_block_buffer = data_block_count);
    assert (Utils.cstruct_array_is_consistent data_block_buffer);
    Utils.zero_cstruct_array data_block_buffer;
    {
      param;
      graph = Graph.create param;
      data_block_size = Cstruct.length data_block_buffer.(0);
      data_blocks = data_block_buffer;
      drops = Array.make (Param.max_drop_count param) None;
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
    Hash_int_set.iter
      (fun data_index ->
        if decoder.graph.data_block_is_solved.(data_index) then (
          Utils.xor_onto
            ~src:decoder.data_blocks.(data_index)
            ~onto:(Option.get decoder.drops.(drop_index));
          Graph.remove_edge ~data_index ~drop_index decoder.graph))
      data_indices

  let propagate_data_xor ~data_index (decoder : decoder) : unit =
    let data = decoder.data_blocks.(data_index) in
    Hash_int_set.iter
      (fun drop_index ->
        Option.iter
          (fun onto ->
            Utils.xor_onto ~src:data ~onto;
            Graph.remove_edge ~data_index ~drop_index decoder.graph)
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
                Hash_int_set.choose decoder.graph.drop_edges.(drop_index)
              in
              if not decoder.graph.data_block_is_solved.(data_index) then (
                Utils.blit_onto ~src:drop_data
                  ~onto:decoder.data_blocks.(data_index);
                Graph.mark_data_as_solved ~data_index decoder.graph;
                Graph.remove_edge ~data_index ~drop_index decoder.graph;
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
    [ `Success
    | `Ongoing
    ]

  let data_is_ready (decoder : decoder) : bool =
    decoder.graph.data_block_solved_count = Param.data_block_count decoder.param

  let max_tries_reached (decoder : decoder) : bool =
    decoder.graph.drop_fill_count = Param.max_drop_count decoder.param

  let decode_one (decoder : decoder) (drop : Drop.t) : (status, error) result =
    assert (Cstruct.length (Drop.data drop) = decoder.data_block_size);
    let drop_index = Drop.index drop in
    if data_is_ready decoder then Ok `Success
    else if max_tries_reached decoder then Error `Cannot_recover
    else
      match decoder.drops.(drop_index) with
      | Some _ -> Ok `Ongoing
      | None -> (
          add_drop drop decoder;
          remove_solved_drop_edges ~drop_index decoder;
          match reduce decoder with
          | `Success -> Ok `Success
          | `Need_more_drops ->
              if max_tries_reached decoder then Error `Cannot_recover
              else Ok `Ongoing)

  let decode_all (decoder : decoder) (drops : Drop.t array)
      (drop_present : bool array) : error option =
    assert (Array.length drops = Array.length drop_present);
    let rec aux decoder drops drop_present cur =
      if cur < Array.length drops then
        if drop_present.(cur) then
          let x = drops.(cur) in
          match decode_one decoder x with
          | Error e -> Some e
          | Ok `Ongoing ->
              if max_tries_reached decoder then Some `Cannot_recover
              else aux decoder drops drop_present (succ cur)
          | Ok `Success -> None
        else aux decoder drops drop_present (succ cur)
      else None
    in
    aux decoder drops drop_present 0
end

let max_drop_count = Constants.max_drop_count

let max_data_block_count = Constants.max_data_block_count

type drop = Drop.t

let data_of_drop = Drop.data

type encoder = Encode.encoder

let create_encoder = Encode.create_encoder

let reset_encoder = Encode.reset_encoder

let param_of_encoder (encoder : Encode.encoder) = encoder.param

let encoder_is_systematic (encoder : Encode.encoder) =
  Param.systematic encoder.param

let data_block_count_of_encoder (encoder : Encode.encoder) =
  Param.data_block_count encoder.param

let max_drop_count_of_encoder (encoder : Encode.encoder) =
  Param.max_drop_count encoder.param

let data_block_size_of_encoder (encoder : Encode.encoder) =
  Cstruct.length encoder.data_blocks.(0)

let data_blocks_of_encoder (encoder : Encode.encoder) = encoder.data_blocks

let encode_one = Encode.encode_one

let encode_all = Encode.encode_all

let remaining_drops_of_encoder = Encode.get_remaining_drops

type decoder = Decode.decoder

type decode_error = Decode.error

type decode_status = Decode.status

let create_decoder = Decode.create_decoder

let reset_decoder = Decode.reset_decoder

let param_of_decoder (decoder : Decode.decoder) = decoder.param

let decoder_is_systematic (decoder : Decode.decoder) =
  Param.systematic decoder.param

let data_block_count_of_decoder (decoder : Decode.decoder) =
  Param.data_block_count decoder.param

let max_drop_count_of_decoder (decoder : Decode.decoder) =
  Param.max_drop_count decoder.param

let data_block_size_of_decoder (decoder : Decode.decoder) =
  decoder.data_block_size

let drop_fill_count_of_decoder (decoder : Decode.decoder) =
  decoder.graph.drop_fill_count

let data_blocks_of_decoder (decoder : Decode.decoder) : Cstruct.t array option =
  if Decode.data_is_ready decoder then Some decoder.data_blocks else None

let decode_one = Decode.decode_one

let decode_all = Decode.decode_all
