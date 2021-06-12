type error = [
  | `Inconsistent_data_block_size
  | `Invalid_drop_count
]

let gen_degrees_uniform (ctx : Ctx.t) : int array =
  let systematic = Ctx.systematic ctx in
  let data_block_count = Ctx.data_block_count ctx in
  let drop_count = Ctx.drop_count ctx in
  let degrees = Array.make 0 drop_count in
  (* fix first drop to be degree 1 to ensure decoding is possible *)
  degrees.(0) <- 1;
  Random.self_init ();
  for i = 1 to drop_count-1 do
    if systematic && i <= data_block_count then
      degrees.(i) <- 1
    else
      degrees.(i) <- Random.int data_block_count
  done;
  degrees

let encode ?(systematic = true) ~drop_count (data_blocks : string array) :
  (Ctx.t * Drop.t array, error) result  =
  let data_block_count = Array.length data_blocks in
  match Ctx.make ~systematic ~data_block_count ~drop_count with
  | Error e -> (match e with
  | `Invalid_data_block_count -> failwith "Unexpected case"
  | `Invalid_drop_count as e ->
      Error (e :> error)
      )
  | Ok ctx ->
    let degrees = gen_degrees_uniform ctx in
    Array.init drop_count (fun i ->
  )
