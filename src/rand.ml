type state = int64 ref

let make seed : state =
  let seed = Int64.of_int seed in
  ref
    (if Int64.equal seed 0L then Int64.succ 0L
    else if Int64.equal seed Int64.max_int then Int64.(pred max_int)
    else seed)

let hash_int64 (x : int64) : int64 =
  let x = Int64.logxor x (Int64.shift_left x 13) in
  let x = Int64.logxor x (Int64.shift_right_logical x 7) in
  Int64.logxor x (Int64.shift_left x 17)

let hash_int (x : int) : int =
  Int64.to_int 
  @@
  hash_int64 (Int64.of_int x)

let gen_int64 (state : state) (bound : int64) : int64 =
  let x = hash_int64 !state in
  assert (x <> 0L);
  state := x;
  let x = if x = Int64.max_int then Int64.(pred max_int) else x in
  let x = Int64.logand x 0x7FFFFFFF_FFFFFFFFL in
  let r = Int64.rem x bound in
  r

let global =
  Random.self_init ();
  let seed = Random.int 0x0FFFFFFF in
  make seed

let gen_int64_global bound = gen_int64 global bound

let gen_int (state : state) (bound : int) : int =
  let r = Int64.to_int @@ gen_int64 state (Int64.of_int bound) in
  r

let gen_int_global bound = gen_int global bound
