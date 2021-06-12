let xor_onto ~(src : bytes) ~(onto : bytes) : unit =
  assert (Bytes.length src = Bytes.length onto);
  let rec aux i len src onto =
    if i < len - 4 then
      let old = Bytes.get_int64_ne onto i in
      let x = Bytes.get_int64_ne src i in
      let new_val = Int64.logxor x old in
      Bytes.set_int64_ne onto i new_val;
      aux (i + 4) len src onto
    else
      if i < len then
        let old = Bytes.get_uint8 onto i in
        let x = Bytes.get_uint8 src i in
        let new_val = x lxor old in
        Bytes.set_uint8 onto i new_val;
        aux (succ i) len src onto
    else
      ()
    in
  let len = Bytes.length src in
  aux 0 len src onto

let data_block_indices (ctx : Ctx.t) (t : Drop.t) : int list =
  let systematic = Ctx.systematic ctx in
  let data_block_count = Ctx.data_block_count ctx in
  let rec aux cur degree acc =
    if cur < degree then
      let x = Random.int data_block_count in
      aux (succ cur) degree (x :: acc)
    else
      acc
      in
    if systematic && t.index < data_block_count then
      [t.index]
    else (
      Random.init t.index;
      aux 0 t.degree []
      )

