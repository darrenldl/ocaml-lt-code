let zero_cstruct (x : Cstruct.t) : unit = Cstruct.memset x 0

let zero_cstruct_array (arr : Cstruct.t array) : unit =
  Array.iter zero_cstruct arr

let xor_onto ~(src : Cstruct.t) ~(onto : Cstruct.t) : unit =
  assert (Cstruct.length src = Cstruct.length onto);
  let len = Cstruct.length src in
  (* let get_uint64 = Cstruct.LE.get_uint64 in *)
  (* let set_uint64 = Cstruct.LE.set_uint64 in *)
  (* let rec aux i len src onto = *)
  (* if i < len - 8 then ( *)
  (* let old = get_uint64 onto i in *)
  (* let x = get_uint64 src i in *)
  (* let new_val = Int64.logxor x old in *)
  (* set_uint64 onto i new_val; *)
  (* aux (i + 8) len src onto) *)
  (* else if i < len then ( *)
  (* let old = Cstruct.get_uint8 onto i in *)
  (* let x = Cstruct.get_uint8 src i in *)
  (* let new_val = x lxor old in *)
  (* Cstruct.set_uint8 onto i new_val; *)
  (* aux (succ i) len src onto) *)
  (* else () *)
  (* in *)
  (* let len = Cstruct.length src in *)
  (* aux 0 len src onto *)
  let src = Ctypes.(bigarray_start array1 (Cstruct.to_bigarray src)) in
  let onto = Ctypes.(bigarray_start array1 (Cstruct.to_bigarray onto)) in
  Lt_code_c.xor_onto len src onto

let memcpy ~src ~dst =
  assert (Cstruct.length src = Cstruct.length dst);
  let len = Cstruct.length src in
  let src = Ctypes.(bigarray_start array1 (Cstruct.to_bigarray src)) in
  let dst = Ctypes.(bigarray_start array1 (Cstruct.to_bigarray dst)) in
  Lt_code_c.memcpy dst src len

(* let blit_onto ~(src : Cstruct.t) ~(onto : Cstruct.t) : unit = *)
(* assert (Cstruct.length src = Cstruct.length onto); *)
(* Cstruct.blit src 0 onto 0 (Cstruct.length src) *)

let cstruct_array_is_consistent (arr : Cstruct.t array) : bool =
  Array.length arr = 0
  ||
  let len = Cstruct.length arr.(0) in
  Array.for_all (fun x -> Cstruct.length x = len) arr

let fill_array (v : 'a) (arr : 'a array) : unit =
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- v
  done
