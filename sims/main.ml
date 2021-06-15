type setup = {
  param : Ofountain.Param.t;
  data_block_size : int;
  data_loss_rate : float;
  rounds : int;
  encoder : Ofountain.encoder;
  decoder : Ofountain.decoder;
}

type stats = {
  drops_used : int;
  success : bool;
}

type stats_sum = {
  total_overhead : float;
  total_success_count : int;
}

type combined_stats = {
  average_overhead : float;
  success_rate : float;
}

let empty_stats = { drops_used = 0; success = false }

let empty_stats_sum = { total_overhead = 0.0; total_success_count = 0 }

let make_setup ~systematic ~data_block_count ~max_redundancy ~data_block_size
    ~data_loss_rate ~rounds =
  let drop_count_limit =
    data_block_count
    + int_of_float (max_redundancy *. float_of_int data_block_count)
  in
  let param =
    Result.get_ok
    @@ Ofountain.Param.make ~systematic ~data_block_count ~drop_count_limit
  in
  assert (0.0 <= data_loss_rate);
  let drop_data_buffer =
    Array.init (Ofountain.Param.drop_count_limit param) (fun _ ->
        Cstruct.create data_block_size)
  in
  let data_blocks =
    Array.init (Ofountain.Param.data_block_count param) (fun _ ->
        Cstruct.create data_block_size)
  in
  let encoder =
    Result.get_ok @@ Ofountain.make_encoder ~drop_data_buffer param data_blocks
  in
  let data_block_buffer =
    Array.init (Ofountain.Param.data_block_count param) (fun _ ->
        Cstruct.create data_block_size)
  in
  let decoder =
    Result.get_ok
    @@ Ofountain.make_decoder ~data_block_buffer ~data_block_size param
  in
  { param; data_block_size; data_loss_rate; rounds; encoder; decoder }

let run_once (setup : setup) : stats =
  let rec aux (data_blocks_copy : Cstruct.t array) (stats : stats) : stats =
    match Ofountain.encode_one_drop setup.encoder with
    | None -> stats
    | Some x -> (
        Random.self_init ();
        if Random.float 1.0 < setup.data_loss_rate then
          aux data_blocks_copy stats
        else
          let stats = { stats with drops_used = stats.drops_used + 1 } in
          match Ofountain.decode_one_drop setup.decoder x with
          | Ok (`Success arr) ->
              for
                i = 0 to Ofountain.data_block_count_of_decoder setup.decoder - 1
              do
                let same = Cstruct.equal arr.(i) data_blocks_copy.(i) in
                if not same then (
                  Fmt.pr "recovered: %a\n" Cstruct.hexdump_pp arr.(i);
                  Fmt.pr "original:  %a\n" Cstruct.hexdump_pp
                    data_blocks_copy.(i));
                assert same
              done;
              { stats with success = true }
          | Ok `Ongoing -> aux data_blocks_copy stats
          | Error `Cannot_recover -> stats
          | Error _ -> failwith "Unexpected case")
  in
  Ofountain.reset_encoder setup.encoder;
  Ofountain.reset_decoder setup.decoder;
  let data_block_count = Ofountain.data_block_count_of_encoder setup.encoder in
  let data_block_size = Ofountain.data_block_size_of_encoder setup.encoder in
  let data_blocks = Ofountain.data_blocks_of_encoder setup.encoder in
  Array.iter
    (fun block ->
      for i = 0 to setup.data_block_size - 1 do
        Cstruct.set_uint8 block i (Random.int 256)
      done)
    data_blocks;
  let data_blocks_copy =
    Array.init data_block_count (fun i ->
        let x = Cstruct.create data_block_size in
        Cstruct.blit data_blocks.(i) 0 x 0 data_block_size;
        x)
  in
  aux data_blocks_copy empty_stats

let run (setup : setup) : combined_stats =
  let stats_collection = Array.init setup.rounds (fun _ -> run_once setup) in
  let data_block_count =
    float_of_int @@ Ofountain.Param.data_block_count setup.param
  in
  let sum =
    Array.fold_left
      (fun sum stats ->
        if stats.success then (
          let overhead =
            (float_of_int stats.drops_used -. data_block_count)
            /. data_block_count
          in
          assert (overhead >= 0.0);
          {
            total_overhead = sum.total_overhead +. overhead;
            total_success_count = sum.total_success_count + 1;
          })
        else sum)
      empty_stats_sum stats_collection
  in
  let rounds = float_of_int setup.rounds in
  {
    average_overhead = sum.total_overhead /. rounds;
    success_rate = float_of_int sum.total_success_count /. rounds;
  }

let () =
  let setup =
    make_setup ~systematic:false ~data_block_count:100 ~max_redundancy:2.0
      ~data_block_size:1300 ~data_loss_rate:0.10 ~rounds:100
  in
  let stats = run setup in
  Printf.printf
    "success rate: % 3.3f%%, avg. overhead for successful cases: % 3.3f%%\n"
    (100.0 *. stats.success_rate)
    (100.0 *. stats.average_overhead)
