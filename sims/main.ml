type setup = {
  param : Ofountain.Param.t;
  data_block_size : int;
}

type stats = {
  drops_needed : int;
}


let run ~systematic ~data_block_count ~drop_count ~data_block_size =
  let param = Result.get_ok @@
  Ofountain.Param.make ~systematic ~data_block_count
  ~drop_count in
  { param; data_block_size }
