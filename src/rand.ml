type state = int32 ref

let make seed : state =
  let seed = Int32.of_int seed in
  ref
    (if Int32.equal seed 0l then Int32.succ 0l
    else if Int32.equal seed Int32.max_int then Int32.(pred max_int)
    else seed)

let gen_int32 (state : state) (bound : int32) : int32 =
  assert (bound > 0l);
  let x = Int64.(logand 0xFFFF_FFFFL (of_int32 !state)) in
  let x = Int64.(rem (mul x 48271L) 0x7FFF_FFFFL)in
  let x = Int64.to_int32 x in
  assert (x <> 0l);
  state := x;
  let x = Int32.logand x 0x7FFF_FFFFl in
  let r = Int32.rem x bound in
  r

let global =
  Random.self_init ();
  let seed = Random.int 0x0FFFFFFF in
  make seed

let gen_int32_global bound = gen_int32 global bound

let gen_int (state : state) (bound : int) : int =
  assert (bound <= Int32.(to_int max_int));
  let r = Int32.to_int @@ gen_int32 state (Int32.of_int bound) in
  r

let gen_int_global bound = gen_int global bound
