type setup = {
  param : Ofountain.Param.t;
  data_block_size : int;
  data_loss_rate : float;
  rounds : int;
}

type stats = {
  drops_used : int;
  success : bool;
}

let empty_stats = { drops_used = 0; success = false }

let make_setup ~systematic ~data_block_count ~drop_count ~data_block_size
    ~data_loss_rate ~rounds =
  let param =
    Result.get_ok
    @@ Ofountain.Param.make ~systematic ~data_block_count ~drop_count
  in
  assert (0.0 <= data_loss_rate && data_loss_rate < 1.0);
  { param; data_block_size; data_loss_rate; rounds }

let run (setup : setup) : stats =
  let rec aux (original_data_blocks : Cstruct.t array)
      (decode_ctx : Ofountain.decode_ctx) (stats : stats)
      (drops : Ofountain.drop Seq.t) =
    match drops () with
    | Seq.Nil -> stats
    | Seq.Cons (x, xs) -> (
        let stats = { stats with drops_used = stats.drops_used + 1 } in
        match Ofountain.decode_drop decode_ctx x with
        | Ok (`Success arr) ->
            for
              i = 0
              to Ofountain.Param.data_block_count
                   (Ofountain.param_of_decode_ctx decode_ctx)
                 - 1
            do
              assert (Cstruct.equal arr.(i) original_data_blocks.(i))
            done;
            { stats with success = true }
        | Ok `Ongoing -> aux original_data_blocks decode_ctx stats xs
        | Error `Cannot_recover -> stats
        | Error _ -> failwith "Unexpected case")
  in
  let data_blocks =
    Array.init (Ofountain.Param.data_block_count setup.param) (fun _ ->
        Cstruct.create setup.data_block_size)
  in
  Array.iter
    (fun block ->
      for i = 0 to setup.data_block_size - 1 do
        Cstruct.set_uint8 block i (Random.int 256)
      done)
    data_blocks;
  let data_block_buffer =
    Array.init (Ofountain.Param.data_block_count setup.param) (fun _ ->
        Cstruct.create setup.data_block_size)
  in
  let drop_data_buffer =
    Array.init (Ofountain.Param.drop_count setup.param) (fun _ ->
        Cstruct.create setup.data_block_size)
  in
  let drops =
    Result.get_ok
    @@ Ofountain.encode_with_param_lazy ~drop_data_buffer setup.param
         data_blocks
  in
  let decode_ctx =
    Result.get_ok
    @@ Ofountain.make_decode_ctx ~data_block_buffer
         ~data_block_size:setup.data_block_size setup.param
  in
  aux data_blocks decode_ctx empty_stats drops
