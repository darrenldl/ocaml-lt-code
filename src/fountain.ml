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
    (* fix first drop to be degree 1 to ensure decoding is possible *)
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
      if not (Array.for_all (fun x -> Bytes.length x = data_block_len) data_blocks) then
        Error `Inconsistent_data_block_size
    else
      let degrees = gen_degrees_uniform ctx in
      let drops = Array.init (Ctx.drop_count ctx) (fun index ->
        let degree = degrees.(index) in
        let data = Bytes.make data_block_len '\x00' in
        let drop = Drop.make_exn ~index ~degree ~data in
        List.iter (fun i ->
          Utils.xor_onto ~src:data_blocks.(i) ~onto:data
          )
        (get_data_block_indices ctx drop);
            drop
        )
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
    | Ok ctx ->
        match encode_with_ctx ctx data_blocks with
        | Error e -> Error e
        | Ok drops ->
            Ok (ctx, drops)
end

module Decode = struct
  type error =
    [ `Inconsistent_drop_size
    | `Invalid_drop_count
    | `Insufficient_drops
    ]

  let decode (ctx : Ctx.t) (drops : Drop.t array) : (bytes array, error) result =
    if Array.length drops <> Ctx.drop_count ctx then
      Error `Invalid_drop_count
    else
      let drop_size = Bytes.length (Drop.data drops.(0)) in
      if not (Array.for_all (fun x -> Bytes.length (Drop.data x) = drop_size) drops) then
        Error `Inconsistent_drop_size
      else
end
