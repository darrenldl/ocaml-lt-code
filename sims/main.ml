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

let make_setup ~systematic ~data_block_count ~redundancy ~data_block_size
    ~data_loss_rate ~rounds =
  let drop_count =
    int_of_float ((1.0 +. redundancy) *. float_of_int data_block_count)
  in
  let param =
    Result.get_ok
    @@ Ofountain.Param.make ~systematic ~data_block_count ~drop_count
  in
  assert (0.0 <= data_loss_rate);
  { param; data_block_size; data_loss_rate; rounds }

let run_once ~data_block_buffer ~drop_data_buffer (setup : setup) data_blocks :
    stats =
  let rec aux (original_data_blocks : Cstruct.t array)
      (decode_ctx : Ofountain.decode_ctx) (stats : stats)
      (drops : unit -> Ofountain.drop option) : stats =
    match drops () with
    | None -> stats
    | Some x -> (
        Random.self_init ();
        if Random.float 1.0 < setup.data_loss_rate then
          aux original_data_blocks decode_ctx stats drops
        else
          let stats = { stats with drops_used = stats.drops_used + 1 } in
          match Ofountain.decode_drop decode_ctx x with
          | Ok (`Success arr) ->
              for
                i = 0
                to Ofountain.Param.data_block_count
                     (Ofountain.param_of_decode_ctx decode_ctx)
                   - 1
              do
                let same = Cstruct.equal arr.(i) original_data_blocks.(i) in
                if not same then (
                  Fmt.pr "recovered: %a\n" Cstruct.hexdump_pp arr.(i);
                  Fmt.pr "original:  %a\n" Cstruct.hexdump_pp
                    original_data_blocks.(i));
                assert same
              done;
              { stats with success = true }
          | Ok `Ongoing -> aux original_data_blocks decode_ctx stats drops
          | Error `Cannot_recover -> stats
          | Error _ -> failwith "Unexpected case")
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

let run (setup : setup) : combined_stats =
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
  let stats_collection =
    Array.init setup.rounds (fun _ ->
        run_once ~data_block_buffer ~drop_data_buffer setup data_blocks)
  in
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
    make_setup ~systematic:false ~data_block_count:1600 ~redundancy:2.0
      ~data_block_size:1300 ~data_loss_rate:0.05 ~rounds:100
  in
  let stats = run setup in
  Printf.printf "success rate: % 3.3f%%, avg. overhead for successful cases: % 3.3f%%\n"
    (100.0 *. stats.success_rate)
    (100.0 *. stats.average_overhead)
