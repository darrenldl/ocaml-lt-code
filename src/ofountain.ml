module Param = Param
module Drop = Drop
module Drop_set = Drop_set

let get_data_block_indices (param : Param.t) (t : Drop.t) : int list =
  let systematic = Param.systematic param in
  let data_block_count = Param.data_block_count param in
  let rec aux cur degree acc =
    if cur < degree then
      let x = Random.int data_block_count in
      aux (succ cur) degree (x :: acc)
    else acc
  in
  if systematic && Drop.index t < data_block_count then [ Drop.index t ]
  else (
    Random.init (Drop.index t);
    aux 0 (Drop.degree t) [])

let array_of_seq ~drop_count s =
  let s = ref s in
  Array.init drop_count (fun _ ->
    match !s () with
              | Seq.Nil -> failwith "Unexpected"
              | Seq.Cons (x, s') -> (
                s := s';
                  x
              )
  )

module Encode = struct
  type error =
    [ `Inconsistent_data_block_size
    | `Invalid_drop_count
    | `Invalid_data_block_count
    | `Invalid_drop_data_buffer
    ]

  let gen_degrees (param : Param.t) : int array =
    let data_block_count = Param.data_block_count param in
    let drop_count = Param.drop_count param in
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

  let encode_with_param_lazy ?(drop_data_buffer : Cstruct.t array option) (param : Param.t)
      data_blocks : (Drop.t Seq.t, error) result =
    if Array.length data_blocks <> Param.data_block_count param then
      Error `Invalid_data_block_count
    else
      let data_block_len = Cstruct.length data_blocks.(0) in
      if not (Utils.cstruct_array_is_consistent data_blocks) then
        Error `Inconsistent_data_block_size
      else
        let drop_count = Param.drop_count param in
        let degrees = gen_degrees param in
        let data_buffer =
          match drop_data_buffer with
          | None ->
              Ok
                (Array.init drop_count (fun _ -> Cstruct.create data_block_len))
          | Some buffer ->
              if
                Array.length buffer = Param.drop_count param
                && Cstruct.length buffer.(0) = Cstruct.length data_blocks.(0)
                && Utils.cstruct_array_is_consistent buffer
              then (
                Utils.zero_cstruct_array buffer;
                Ok buffer)
              else Error `Invalid_drop_data_buffer
        in
        match data_buffer with
        | Error e -> Error e
        | Ok data_buffer ->
            OSeq.(0 -- Param.drop_count param)
        |> Seq.map (fun index ->
                  let degree = degrees.(index) in
                  let data = data_buffer.(index) in
                  let drop = Drop.make_exn ~index ~degree ~data in
                  List.iter
                    (fun i -> Utils.xor_onto ~src:data_blocks.(i) ~onto:data)
                    (get_data_block_indices param drop);
                  drop
            )
        |> Result.ok

  let encode_with_param ?drop_data_buffer param data_blocks =
        match encode_with_param_lazy ?drop_data_buffer param data_blocks with
        | Error e -> Error e
        | Ok s ->
          Ok (array_of_seq ~drop_count:(Param.drop_count param) s)

  let encode_lazy ?(systematic = true) ?drop_data_buffer ~drop_count
      (data_blocks : Cstruct.t array) : (Param.t * Drop.t Seq.t, error) result =
    let data_block_count = Array.length data_blocks in
    match Param.make ~systematic ~data_block_count ~drop_count with
    | Error e -> (
        match e with
        | (`Invalid_data_block_count | `Invalid_drop_count) as e ->
            Error (e :> error))
    | Ok param -> (
        match encode_with_param_lazy ?drop_data_buffer param data_blocks with
        | Error e -> Error e
        | Ok drops -> Ok (param, drops))

  let encode ?systematic ?drop_data_buffer ~drop_count
      (data_blocks : Cstruct.t array) : (Param.t * Drop.t array, error) result =
        match encode_lazy ?systematic ?drop_data_buffer ~drop_count data_blocks with
        | Error e -> Error e
        | Ok (param, s) ->
          Ok (param, array_of_seq ~drop_count s)
end

module Decode = struct
  type error =
    [ `Inconsistent_drop_size
    | `Invalid_drop_index
    | `Invalid_drop_count
    | `Insufficient_drops
    | `Invalid_data_block_buffer
    ]

  type status = [
    | `Success of Cstruct.t array
    | `Ongoing
    ]

  module Graph = struct
    type bucket = Int_set.t

    type t = {
      data_len : int;
      data_blocks : Cstruct.t array;
      mutable unsolved_data_blocks : Int_set.t;
      drops : Cstruct.t option array;
      data_edges : bucket array;
      drop_edges : bucket array;
    }

    let make ?(data_block_buffer : Cstruct.t array option) (param : Param.t)
        (available_drops : Drop_set.t) : (t, error) result =
      let drop_count = Param.drop_count param in
      if Drop_set.cardinal available_drops > drop_count then
        Error `Invalid_drop_count
      else if
        not
          (Drop_set.for_all
             (fun x -> Drop.index x < drop_count)
             available_drops)
      then Error `Invalid_drop_index
      else
        let drop_size =
          Cstruct.length (Drop.data @@ Drop_set.choose available_drops)
        in
        if
          not
            (Drop_set.for_all
               (fun x -> Cstruct.length (Drop.data x) = drop_size)
               available_drops)
        then Error `Inconsistent_drop_size
        else
          let data_blocks =
            match data_block_buffer with
            | None ->
                Ok
                  (Array.init (Param.data_block_count param) (fun _ ->
                       Cstruct.create drop_size))
            | Some buffer ->
                if
                  Array.length buffer = Param.data_block_count param
                  && Cstruct.length buffer.(0) = drop_size
                  && Utils.cstruct_array_is_consistent buffer
                then (
                  Utils.zero_cstruct_array buffer;
                  Ok buffer)
                else Error `Invalid_data_block_buffer
          in
          match data_blocks with
          | Error e -> Error e
          | Ok data_blocks ->
              let data_block_count = Param.data_block_count param in
              let drops = Array.make drop_count None in
              let data_edges = Array.make data_block_count Int_set.empty in
              let drop_edges = Array.make drop_count Int_set.empty in
              Drop_set.iter
                (fun drop ->
                  let drop_index = Drop.index drop in
                  drops.(drop_index) <- Some (Drop.data drop);
                  let data_indices =
                    Int_set.of_list @@ get_data_block_indices param drop
                  in
                  drop_edges.(drop_index) <- data_indices;
                  Int_set.iter
                    (fun data_index ->
                      data_edges.(data_index) <-
                        Int_set.add drop_index data_edges.(data_index))
                    data_indices)
                available_drops;
              Ok
                {
                  data_len = drop_size;
                  data_blocks;
                  unsolved_data_blocks = Param.unsolved_data_blocks_init param;
                  drops;
                  data_edges;
                  drop_edges;
                }

    let degree_of_drop ~drop_index (g : t) : int =
      Int_set.cardinal g.drop_edges.(drop_index)

    let propagate_data_xor ~data_index (g : t) : unit =
      let data = g.data_blocks.(data_index) in
      Int_set.iter
        (fun drop_index ->
          Utils.xor_onto ~src:data ~onto:(Option.get g.drops.(drop_index));
          g.drop_edges.(drop_index) <-
            Int_set.remove data_index g.drop_edges.(drop_index))
        g.data_edges.(data_index)

    type reduction_res =
      | Success
      | Ongoing
      | Insufficient_drops

    let reduce_single_step (g : t) : reduction_res =
      let degree_1_found = ref false in
      Array.iteri
        (fun drop_index drop ->
          match drop with
          | None -> ()
          | Some drop_data ->
              if degree_of_drop ~drop_index g = 1 then (
                degree_1_found := true;
                let data_index = Int_set.choose g.drop_edges.(drop_index) in
                if Int_set.mem data_index g.unsolved_data_blocks then (
                  Utils.xor_onto ~src:drop_data ~onto:g.data_blocks.(data_index);
                  g.unsolved_data_blocks <-
                    Int_set.remove data_index g.unsolved_data_blocks;
                  propagate_data_xor ~data_index g)))
        g.drops;
      if !degree_1_found then Ongoing
      else if Int_set.is_empty g.unsolved_data_blocks then Success
      else Insufficient_drops

    let reduce (g : t) : error option =
      let rec aux g =
        match reduce_single_step g with
        | Success -> None
        | Ongoing -> aux g
        | Insufficient_drops -> Some `Insufficient_drops
      in
      aux g
  end

  let decode ?data_block_buffer (param : Param.t) (drops : Drop_set.t) :
      (Cstruct.t array, error) result =
    match Graph.make ?data_block_buffer param drops with
    | Error e -> Error e
    | Ok g -> (
        match Graph.reduce g with Some e -> Error e | None -> Ok g.data_blocks)
end

let max_drop_count = Constants.max_drop_count

let max_data_block_count = Constants.max_data_block_count

type encode_error = Encode.error

let encode = Encode.encode

let encode_lazy = Encode.encode_lazy

let encode_with_param = Encode.encode_with_param

let encode_with_param_lazy = Encode.encode_with_param_lazy

type decode_error = Decode.error

let decode = Decode.decode
