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
    let data_block_count_int = Ctx.data_block_count ctx in
    let drop_count = Ctx.drop_count ctx in
    let degrees = Array.make 0 drop_count in
    (* fix first drop to be degree 1 to ensure decoding is at least possible *)
    degrees.(0) <- 1;
    Random.self_init ();
    for i = 1 to drop_count - 1 do
      if systematic && i <= data_block_count_int then degrees.(i) <- 1
      else degrees.(i) <- Random.int data_block_count
    done;
    degrees

  let encode_with_ctx (ctx : Ctx.t) data_blocks : (Drop.t array, error) result =
    if Array.length data_blocks <> Ctx.data_block_count ctx then
      Error `Invalid_data_block_count
    else
      let data_block_len = Bytes.length data_blocks.(0) in
      if
        not
          (Array.for_all (fun x -> Bytes.length x = data_block_len) data_blocks)
      then Error `Inconsistent_data_block_size
      else
        let degrees = gen_degrees_uniform ctx in
        let drops =
          Array.init (Ctx.drop_count ctx) (fun index ->
              let degree = degrees.(index) in
              let data = Bytes.make data_block_len '\x00' in
              let drop = Drop.make_exn ~index ~degree ~data in
              List.iter
                (fun i -> Utils.xor_onto ~src:data_blocks.(i) ~onto:data)
                (get_data_block_indices ctx drop);
              drop)
        in
        Ok drops

  let encode ?(systematic = true) ~drop_count (data_blocks : bytes array) :
      (Ctx.t * Drop.t array, error) result =
    let data_block_count = Array.length data_blocks in
    match Ctx.make ~systematic ~data_block_count ~drop_count with
    | Error e -> (
        match e with
        | `Invalid_data_block_count -> failwith "Unexpected case"
        | `Invalid_drop_count as e -> Error (e :> error))
    | Ok ctx -> (
        match encode_with_ctx ctx data_blocks with
        | Error e -> Error e
        | Ok drops -> Ok (ctx, drops))
end

module Decode = struct
  type error =
    [ `Inconsistent_drop_size
    | `Invalid_drop_count
    | `Insufficient_drops
    | `Invalid_data_block_buffer
    ]

  module Graph = struct
    type bucket = Int_set.t

    type t = {
      data_len : int;
      data_blocks : bytes array;
      mutable unsolved_data_blocks : Int_set.t;
      drops : bytes option array;
      data_edges : bucket array;
      drop_edges : bucket array;
    }

    let make_unsolved_data_blocks ~data_block_count : Int_set.t =
      let rec aux c (acc : Int_set.t) =
        if c < data_block_count then aux (succ c) (Int_set.add c acc) else acc
      in
      aux 0 Int_set.empty

    let make ?(data_block_buffer : bytes array option) (ctx : Ctx.t)
        (available_drops : Drop_set.t) : (t, error) result =
      if Drop_set.cardinal available_drops <> Ctx.drop_count ctx then
        Error `Invalid_drop_count
      else
        let drop_size =
          Bytes.length (Drop.data @@ Drop_set.choose available_drops)
        in
        if
          not
            (Drop_set.for_all
               (fun x -> Bytes.length (Drop.data x) = drop_size)
               available_drops)
        then Error `Inconsistent_drop_size
        else
          let data_blocks =
            match data_block_buffer with
            | None ->
                Ok
                  (Array.init (Ctx.data_block_count ctx) (fun _ ->
                       Bytes.make drop_size '\x00'))
            | Some buffer ->
                if
                  Array.length buffer = Ctx.data_block_count ctx
                  && Array.for_all (fun b -> Bytes.length b = drop_size) buffer
                then Ok buffer
                else Error `Invalid_data_block_buffer
          in
          match data_blocks with
          | Error e -> Error e
          | Ok data_blocks ->
              let data_block_count = Ctx.data_block_count ctx in
              let drop_count = Ctx.drop_count ctx in
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
                  unsolved_data_blocks =
                    make_unsolved_data_blocks ~data_block_count;
                  drops;
                  data_edges;
                  drop_edges;
                }

    let degree_of_data ~data_index (g : t) : int =
      Int_set.cardinal g.data_edges.(data_index)

    let degree_of_drop ~drop_index (g : t) : int =
      Int_set.cardinal g.drop_edges.(drop_index)

    let propagate_data_xor ~data_index (g : t) : unit =
      let data = g.data_blocks.(data_index) in
      Int_set.iter
        (fun drop_index ->
          Utils.xor_onto ~src:data ~onto:(Option.get g.drops.(drop_index)))
        g.data_edges.(data_index)

    let process_degree_1_drops (g : t) : error option =
      let degree_1_found = ref false in
      Array.iteri
        (fun drop_index drop ->
          match drop with
          | None -> ()
          | Some drop_data ->
              if degree_of_drop ~drop_index g = 1 then (
                degree_1_found := true;
                let data_index = Int_set.choose g.drop_edges.(drop_index) in
                Bytes.blit drop_data 0 g.data_blocks.(data_index) 0 g.data_len;
                g.unsolved_data_blocks <-
                  Int_set.remove data_index g.unsolved_data_blocks;
                propagate_data_xor ~data_index g))
        g.drops;
      if !degree_1_found then None else Some `Insufficient_drops
  end

  let decode (ctx : Ctx.t) (drops : Drop_set.t) : (bytes array, error) result =
    if Drop_set.cardinal drops <> Ctx.drop_count ctx then
      Error `Invalid_drop_count
    else
      let drop_size = Bytes.length (Drop.data @@ Drop_set.choose drops) in
      if
        not
          (Drop_set.for_all
             (fun x -> Bytes.length (Drop.data x) = drop_size)
             drops)
      then Error `Inconsistent_drop_size
      else failwith "Unimplemented"
end
