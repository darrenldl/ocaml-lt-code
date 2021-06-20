type state = int64 ref

let modulus = 0x7FFF_FFFFL

let create seed : state =
  let seed = Int64.of_int seed in
  ref (if Int64.equal seed 0L then Int64.succ 0L else Int64.logand seed modulus)

let hash' (x : int64) : int64 =
  let x = Int64.(mul x 48271L) in
  Int64.(unsigned_rem x modulus)

let hash_int (x : int) : int = Int64.to_int @@ hash' (Int64.of_int x)

let gen' (state : state) (bound : int64) : int64 =
  let rec aux state bound retry_start =
    let x = hash' !state in
    state := x;
    (* let x = if x = Int64.max_int then Int64.(pred max_int) else x in *)
    if x < retry_start then Int64.unsigned_rem x bound
    else aux state bound retry_start
  in
  aux state bound Int64.(sub modulus (unsigned_rem modulus bound))

let global =
  Random.self_init ();
  let seed = Random.int 0x0FFF_FFFF in
  create seed

let gen_int (state : state) (bound : int) : int =
  let r = Int64.to_int @@ gen' state (Int64.of_int bound) in
  r

let gen_int_global bound = gen_int global bound
