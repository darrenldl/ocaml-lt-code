open Core

let () =
  Random.self_init ();
  let rounds = 100 in
  let setups =
    [
      make_setup ~systematic:false ~data_block_count:1000 ~max_redundancy:0.30
        ~data_block_size:1300 ~data_loss_rate:0.00 ~rounds;
    ]
  in
  List.iter run_and_print setups
