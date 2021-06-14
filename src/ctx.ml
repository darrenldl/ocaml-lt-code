type t = {
  systematic : bool;
  data_block_count : int;
  drop_count : int;
  unsolved_data_blocks_init : Int_set.t;
}

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  ]

let systematic t = t.systematic

let data_block_count t = t.data_block_count

let drop_count t = t.drop_count

let unsolved_data_blocks_init t = t.unsolved_data_blocks_init

let make_unsolved_data_blocks ~data_block_count : Int_set.t =
  let rec aux c (acc : Int_set.t) =
    if c < data_block_count then aux (succ c) (Int_set.add c acc) else acc
  in
  aux 0 Int_set.empty

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
        unsolved_data_blocks_init = make_unsolved_data_blocks ~data_block_count;
      }
