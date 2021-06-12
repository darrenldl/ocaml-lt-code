type t = {
  index : int;
  degree : int;
  data : bytes;
}

type error = [
  | `Invalid_index
  | `Invalid_degree
  | `Empty_data
]

let make ~index ~degree ~data : (t, error) result =
  if index < 0 then
    else if 

let serialize (t : t) : string =
  failwith "Unimplemented"

let deserialize (s : string) : t option =
  failwith "Unimplemented"
