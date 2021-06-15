type t = {
  systematic : bool;
  data_block_count : int;
  drop_count : int;
  dist : Dist.t;
}

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  ]

let systematic t = t.systematic

let data_block_count t = t.data_block_count

let drop_count t = t.drop_count

let dist t = t.dist

let make ~systematic ~data_block_count ~drop_count : (t, error) result =
  if data_block_count <= 0 || data_block_count >= Constants.max_data_block_count
  then Error `Invalid_data_block_count
  else if
    drop_count < data_block_count || drop_count >= Constants.max_drop_count
  then Error `Invalid_drop_count
  else
    Ok
      {
        systematic;
        data_block_count;
        drop_count;
        dist = Dist.robust_soliton_dist ~k:data_block_count;
      }
