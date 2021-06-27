type rng = {
  mutable state : int64;
  inc : int64;
}

let create_rng ~init_state ~stream_id : rng =
  let state = Int64.of_int init_state in
  let inc = Int64.of_int stream_id in
  let inc = Int64.(shift_left inc 1) in
  let inc = Int64.(logor inc 1L) in
  { state; inc }

let hash' ?(inc : int64 = 1L) (x : int64) : int64 =
  let x = Int64.mul x 6364136223846793005L in
  let x = Int64.add x inc in
  let xorshifted = Int64.shift_right x 18 in
  let xorshifted = Int64.logxor xorshifted x in
  let xorshifted = Int64.shift_right xorshifted 27 in
  let rot = Int64.(to_int (shift_right_logical x 59)) in
  let res =
    Int64.(
      logor
      (shift_right xorshifted rot)
      (shift_left xorshifted ((-rot) land 31))
      ) 
  in
  res

let hash_int ?inc (x : int) : int =
  Int64.to_int @@ hash' ?inc:(Option.map Int64.of_int inc) (Int64.of_int x)

let gen' (rng : rng) (bound : int64) : int64 =
  let rec aux rng bound threshold =
    let x = hash' ~inc:rng.inc rng.state in
    rng.state <- x;
    (* let x = if x = Int64.max_int then Int64.(pred max_int) else x in *)
    if x < threshold then Int64.unsigned_rem x bound
    else aux rng bound threshold 
  in
  let threshold = Int64.(unsigned_rem (neg bound) bound) in
  aux rng bound threshold

let global =
  Random.self_init ();
  let init_state = Random.int 0x0FFF_FFFF in
  let stream_id = Random.int 0x0FFF_FFFF in
  create_rng ~init_state ~stream_id

let gen_int (rng : rng) (bound : int) : int =
  let r = Int64.to_int @@ gen' rng (Int64.of_int bound) in
  r

let gen_int_global bound = gen_int global bound
