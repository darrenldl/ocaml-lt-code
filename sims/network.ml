open Core

let encode_all_upfront = false

let rounds = 1000

let setups =
  [
    make_setup ~systematic:false ~encode_all_upfront ~data_block_count:1000
      ~max_redundancy:1.00 ~data_block_size:1300 ~data_loss_rate:0.05 ~rounds;
  ]

let () =
  Random.self_init ();
  List.iter run_and_print setups
