module Ctx = Ctx
module Drop = Drop
module Drop_set = Drop_set

let get_data_block_indices (ctx : Ctx.t) (t : Drop.t) : int list =
  let systematic = Ctx.systematic ctx in
  let data_block_count = Ctx.data_block_count ctx in
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

module Encode = struct
  type error =
    [ `Inconsistent_data_block_size
    | `Invalid_drop_count
    | `Invalid_data_block_count
    ]

  let gen_degrees_uniform (ctx : Ctx.t) : int array =
    let systematic = Ctx.systematic ctx in
    let data_block_count = Ctx.data_block_count ctx in
    let drop_count = Ctx.drop_count ctx in
    let degrees = Array.make drop_count 0 in
    Random.self_init ();
    for i = 0 to drop_count - 1 do
      if systematic && i < data_block_count then degrees.(i) <- 1
      else degrees.(i) <- Random.int data_block_count
    done;
    (* fix a random drop to be degree 1 to ensure decoding is at least possible *)
    degrees.(Random.int drop_count) <- 1;
    degrees

  let encode_with_ctx (ctx : Ctx.t) data_blocks : (Drop.t array, error) result =
    if Array.length data_blocks <> Ctx.data_block_count ctx then
      Error `Invalid_data_block_count
    else
      let data_block_len = Cstruct.length data_blocks.(0) in
      if
        not
          (Array.for_all (fun x -> Cstruct.length x = data_block_len) data_blocks)
      then Error `Inconsistent_data_block_size
      else
        let degrees = gen_degrees_uniform ctx in
        let drops =
          Array.init (Ctx.drop_count ctx) (fun index ->
              let degree = degrees.(index) in
              let data = Cstruct.create data_block_len in
              let drop = Drop.make_exn ~index ~degree ~data in
              List.iter
                (fun i -> Utils.xor_onto ~src:data_blocks.(i) ~onto:data)
                (get_data_block_indices ctx drop);
              drop)
        in
        Ok drops

  let encode ?(systematic = true) ~drop_count (data_blocks : Cstruct.t array) :
      (Ctx.t * Drop.t array, error) result =
    let data_block_count = Array.length data_blocks in
    match Ctx.make ~systematic ~data_block_count ~drop_count with
    | Error e -> (
        match e with
        | `Invalid_data_block_count
        | `Invalid_drop_count as e -> Error (e :> error))
    | Ok ctx -> (
        match encode_with_ctx ctx data_blocks with
        | Error e -> Error e
        | Ok drops -> Ok (ctx, drops))
end

module Decode = struct
  type error =
    [ `Inconsistent_drop_size
    | `Invalid_drop_index
    | `Invalid_drop_count
    | `Insufficient_drops
    | `Invalid_data_block_buffer
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

    let make ?(data_block_buffer : Cstruct.t array option) (ctx : Ctx.t)
        (available_drops : Drop_set.t) : (t, error) result =
      let drop_count = Ctx.drop_count ctx in
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
                  (Array.init (Ctx.data_block_count ctx) (fun _ ->
                       Cstruct.create drop_size))
            | Some buffer ->
                if
                  Array.length buffer = Ctx.data_block_count ctx
                  && Array.for_all (fun b -> Cstruct.length b = drop_size) buffer
                then Ok buffer
                else Error `Invalid_data_block_buffer
          in
          match data_blocks with
          | Error e -> Error e
          | Ok data_blocks ->
              let data_block_count = Ctx.data_block_count ctx in
              let drops = Array.make drop_count None in
              let data_edges = Array.make data_block_count Int_set.empty in
              let drop_edges = Array.make drop_count Int_set.empty in
              Drop_set.iter
                (fun drop ->
                  let drop_index = Drop.index drop in
                  drops.(drop_index) <- Some (Drop.data drop);
                  let data_indices =
                    Int_set.of_list @@ get_data_block_indices ctx drop
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
                  unsolved_data_blocks = Ctx.unsolved_data_blocks_init ctx;
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

  let decode ?data_block_buffer (ctx : Ctx.t) (drops : Drop_set.t) :
      (Cstruct.t array, error) result =
    match Graph.make ?data_block_buffer ctx drops with
    | Error e -> Error e
    | Ok g -> (
        match Graph.reduce g with Some e -> Error e | None -> Ok g.data_blocks)
end

    let max_drop_count = Constants.max_drop_count

    let max_data_block_count = Constants.max_data_block_count

type encode_error = Encode.error

let encode = Encode.encode

type decode_error = Decode.error

let decode = Decode.decode

