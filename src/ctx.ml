type t = {
  systematic : bool;
  data_block_count : int;
  drop_count : int;
}

type error = [
  | `Invalid_data_block_count
  | `Invalid_drop_count
]

let systematic t = t.systematic

let data_block_count t = t.data_block_count

let drop_count t = t.drop_count

let make ~systematic ~data_block_count
~drop_count : (t, error) result =
  if data_block_count <= 0 then
    Error `Invalid_data_block_count
  else
  if drop_count < data_block_count then
    Error `Invalid_drop_count
  else
    Ok { systematic; data_block_count; drop_count}
