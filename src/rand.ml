type state = int32 ref

let mask = 0x7FFFFFFFl

let create seed : state =
  let seed = Int32.of_int seed in
  ref (if Int32.equal seed 0l then Int32.succ 0l else Int32.logand seed mask)

let hash_int32 (x : int32) : int32 =
  let x = Int64.of_int32 x in
  let x = Int64.(mul x 48271L) in
  let x = Int64.(unsigned_rem x 0x7FFF_FFFFL) in
  Int64.to_int32 x

let hash_int (x : int) : int = Int32.to_int @@ hash_int32 (Int32.of_int x)

let gen_int32 (state : state) (bound : int32) : int32 =
  let rec aux state bound retry_start =
    let x = hash_int32 !state in
    assert (x <> 0l);
    state := x;
    (* let x = if x = Int64.max_int then Int64.(pred max_int) else x in *)
    let x = Int32.logand x mask in
    if x < retry_start then Int32.rem x bound else aux state bound retry_start
  in
  aux state bound Int32.(sub mask (rem mask bound))

let global =
  Random.self_init ();
  let seed = Random.int 0x0FFFFFFF in
  create seed

let gen_int (state : state) (bound : int) : int =
  let r = Int32.to_int @@ gen_int32 state (Int32.of_int bound) in
  r

let gen_int_global bound = gen_int global bound
