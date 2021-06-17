type setup = {
  param : Ofountain.Param.t;
  data_block_size : int;
  data_loss_rate : float;
  rounds : int;
  encoder : Ofountain.encoder;
  encoder_setup_time : float;
  decoder : Ofountain.decoder;
  decoder_setup_time : float;
}

type stats = {
  encoding_time : float;
  decoding_time : float;
  drops_used : int;
  success : bool;
}

type stats_sum = {
  total_encoding_time : float;
  total_decoding_time : float;
  total_drops_used : int;
  total_overhead : float;
  total_success_count : int;
}

type combined_stats = {
  encoder_setup_time : float;
  decoder_setup_time : float;
  average_encoding_time : float;
  average_decoding_time : float;
  average_drops_used : float;
  average_overhead : float;
  success_rate : float;
}

let time_function (f : unit -> 'a) : float * 'a =
  let start = Unix.gettimeofday () in
  let res = f () in
  let end_exc = Unix.gettimeofday () in
  (end_exc -. start, res)

let empty_stats =
  { encoding_time = 0.0; decoding_time = 0.0; drops_used = 0; success = false }

let empty_stats_sum =
  {
    total_encoding_time = 0.0;
    total_decoding_time = 0.0;
    total_drops_used = 0;
    total_overhead = 0.0;
    total_success_count = 0;
  }

let make_setup ~systematic ~data_block_count ~max_redundancy ~data_block_size
    ~data_loss_rate ~rounds =
  let max_drop_count =
    data_block_count
    + int_of_float (max_redundancy *. float_of_int data_block_count)
  in
  let param =
    Result.get_ok
    @@ Ofountain.Param.make ~systematic ~data_block_count ~max_drop_count
  in
  assert (0.0 <= data_loss_rate);
  let drop_data_buffer =
    Array.init (Ofountain.Param.max_drop_count param) (fun _ ->
        Cstruct.create data_block_size)
  in
  let data_blocks =
    Array.init (Ofountain.Param.data_block_count param) (fun _ ->
        Cstruct.create data_block_size)
  in
  let encoder_setup_time, encoder =
    time_function (fun () ->
        Result.get_ok
        @@ Ofountain.make_encoder ~drop_data_buffer param data_blocks)
  in
  let data_block_buffer =
    Array.init (Ofountain.Param.data_block_count param) (fun _ ->
        Cstruct.create data_block_size)
  in
  let decoder_setup_time, decoder =
    time_function (fun () ->
        Result.get_ok
        @@ Ofountain.make_decoder ~data_block_buffer ~data_block_size param)
  in
  {
    param;
    data_block_size;
    data_loss_rate;
    rounds;
    encoder;
    encoder_setup_time;
    decoder;
    decoder_setup_time;
  }

let run_once (setup : setup) : stats =
  let rec aux (data_blocks_copy : Cstruct.t array) (stats : stats) : stats =
    let encoding_time, encode_res =
      time_function (fun () -> Ofountain.encode_one_drop setup.encoder)
    in
    let stats =
      { stats with encoding_time = stats.encoding_time +. encoding_time }
    in
    match encode_res with
    | None -> stats
    | Some x -> (
        Random.self_init ();
        let stats = { stats with drops_used = stats.drops_used + 1 } in
        if Random.float 1.0 < setup.data_loss_rate then
          aux data_blocks_copy stats
        else
          let decoding_time, decode_res =
            time_function (fun () -> Ofountain.decode_one_drop setup.decoder x)
          in
          let stats =
            { stats with decoding_time = stats.decoding_time +. decoding_time }
          in
          match decode_res with
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
        let overhead =
          (float_of_int stats.drops_used -. data_block_count)
          /. data_block_count
        in
        assert (overhead >= 0.0);
        {
          total_encoding_time = sum.total_encoding_time +. stats.encoding_time;
          total_decoding_time = sum.total_decoding_time +. stats.decoding_time;
          total_drops_used = sum.total_drops_used + stats.drops_used;
          total_overhead = sum.total_overhead +. overhead;
          total_success_count =
            (sum.total_success_count + if stats.success then 1 else 0);
        })
      empty_stats_sum stats_collection
  in
  let rounds = float_of_int setup.rounds in
  {
    encoder_setup_time = setup.encoder_setup_time;
    decoder_setup_time = setup.decoder_setup_time;
    average_encoding_time = sum.total_encoding_time /. rounds;
    average_decoding_time = sum.total_decoding_time /. rounds;
    average_drops_used = float_of_int sum.total_drops_used /. rounds;
    average_overhead = sum.total_overhead /. rounds;
    success_rate = float_of_int sum.total_success_count /. rounds;
  }

let calc_redundancy (setup : setup) : float =
  let data_block_count = Ofountain.data_block_count_of_encoder setup.encoder in
  let max_drop_count = Ofountain.max_drop_count_of_encoder setup.encoder in
  100.0
  *. (float_of_int (max_drop_count - data_block_count)
     /. float_of_int data_block_count)

let print_setup (setup : setup) =
  let data_block_count = Ofountain.data_block_count_of_encoder setup.encoder in
  let max_drop_count = Ofountain.max_drop_count_of_encoder setup.encoder in
  let redundancy = calc_redundancy setup in
  Printf.printf "  setup:\n";
  Printf.printf "    systematic:       %b\n"
    (Ofountain.encoder_is_systematic setup.encoder);
  Printf.printf "    data block count: %d\n" data_block_count;
  Printf.printf "    max drop count:   %d\n" max_drop_count;
  Printf.printf "    data block size:  %d\n" setup.data_block_size;
  Printf.printf "    redundancy:       %7.3f%%\n" redundancy;
  Printf.printf "    data loss rate:   %7.3f%%\n" (100.0 *. setup.data_loss_rate);
  Printf.printf "    rounds:           %d\n" setup.rounds

let print_stats (setup : setup) (stats : combined_stats) =
  let s_to_us_multiplier = 1_000_000.0 in
  Printf.printf "  stats:\n";
  Printf.printf "    encoder setup time:              %10.3fus\n"
    (s_to_us_multiplier *. stats.encoder_setup_time);
  Printf.printf "    decoder setup time:              %10.3fus\n"
    (s_to_us_multiplier *. stats.decoder_setup_time);
  Printf.printf "    average encoding time per round: %10.3fus\n"
    (s_to_us_multiplier *. stats.average_encoding_time);
  Printf.printf "    average decoding time per round: %10.3fus\n"
    (s_to_us_multiplier *. stats.average_decoding_time);
  Printf.printf "    average encoding time per drop:  %10.3fus\n"
    (s_to_us_multiplier
    *. stats.average_encoding_time
    /. stats.average_drops_used);
  Printf.printf "    average decoding time per drop:  %10.3fus\n"
    (s_to_us_multiplier
    *. stats.average_decoding_time
    /. stats.average_drops_used);
  let data_byte_count_per_round =
    float_of_int
      (setup.data_block_size * Ofountain.Param.data_block_count setup.param)
  in
  Printf.printf "    average encoding data Mbytes/s:  %10.3f\n"
    (data_byte_count_per_round
    /. 1024.0
    /. 1024.0
    /. stats.average_encoding_time);
  Printf.printf "    average decoding data Mbytes/s:  %10.3f\n"
    (data_byte_count_per_round
    /. 1024.0
    /. 1024.0
    /. stats.average_decoding_time);
  Printf.printf "    success rate:                    %10.3f%%\n"
    (100.0 *. stats.success_rate);
  Printf.printf "    average overhead:                %10.3f%%\n"
    (100.0 *. stats.average_overhead)

let run_and_print (setup : setup) =
  let stats = run setup in
  let redundancy = calc_redundancy setup in
  Printf.printf "Simulation at data loss rate of %.1f%%, redundancy at %.1f%%, %s\n"
    (100.0 *. setup.data_loss_rate)
    redundancy
    (if Ofountain.Param.systematic setup.param then "systematic"
    else "non-systematic");
  print_setup setup;
  print_stats setup stats

let () =
  let setups =
    [
      make_setup ~systematic:false ~data_block_count:1600 ~max_redundancy:0.30
        ~data_block_size:1000 ~data_loss_rate:0.01 ~rounds:100;
    ]
  in
  List.iter run_and_print setups
