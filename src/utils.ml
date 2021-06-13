let xor_onto ~(src : bytes) ~(onto : bytes) : unit =
  assert (Bytes.length src = Bytes.length onto);
  let rec aux i len src onto =
    if i < len - 4 then (
      let old = Bytes.get_int64_ne onto i in
      let x = Bytes.get_int64_ne src i in
      let new_val = Int64.logxor x old in
      Bytes.set_int64_ne onto i new_val;
      aux (i + 4) len src onto)
    else if i < len then (
      let old = Bytes.get_uint8 onto i in
      let x = Bytes.get_uint8 src i in
      let new_val = x lxor old in
      Bytes.set_uint8 onto i new_val;
      aux (succ i) len src onto)
    else ()
  in
  let len = Bytes.length src in
  aux 0 len src onto
