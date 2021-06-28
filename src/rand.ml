type rng = { mutable state : int64 }

let modulus = 0x7FFF_FFFFL

let create_rng seed : rng =
  let seed = Int64.of_int seed in
  { state = (if seed = 0L then 1L else Int64.(logand seed modulus)) }

let hash' (x : int64) : int64 =
  let x = Int64.(mul x 48271L) in
  Int64.(unsigned_rem x modulus)

let hash_int (x : int) : int = Int64.to_int @@ hash' (Int64.of_int x)

let gen' (rng : rng) (bound : int64) : int64 =
  let rec aux rng bound threshold =
    let x = hash' rng.state in
    rng.state <- x;
    if Int64.unsigned_compare x threshold >= 0 then Int64.unsigned_rem x bound
    else aux rng bound threshold
  in
  aux rng bound Int64.(unsigned_rem modulus bound)

let global =
  Random.self_init ();
  let seed = Random.int 0x0FFF_FFFF in
  create_rng seed

let gen_int (rng : rng) (bound : int) : int =
  let r = Int64.to_int @@ gen' rng (Int64.of_int bound) in
  r

let gen_int_global bound = gen_int global bound
