open Core

let encode_all_upfront = true

let rounds = 100

let setups =
  [
    make_setup ~systematic:true ~encode_all_upfront ~data_block_count:1000
      ~max_redundancy:0.30 ~data_block_size:490 ~data_loss_rate:0.30 ~rounds;
  ]

let () =
  Random.self_init ();
  List.iter run_and_print setups
