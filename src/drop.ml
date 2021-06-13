type t = {
  index : int;
  degree : int;
  data : bytes;
}

type error =
  [ `Invalid_index
  | `Invalid_degree
  | `Invalid_data
  ]

exception Error_exn of error

let make ~index ~degree ~data : (t, error) result =
  if index < 0 || index > Constants.max_index then Error `Invalid_index
  else if degree < 0 || degree >= Constants.max_index then Error `Invalid_degree
  else if Bytes.length data <= 0 then Error `Invalid_data
  else Ok { index; degree; data }

let make_exn ~index ~degree ~data : t =
  match make ~index ~degree ~data with
  | Ok x -> x
  | Error e -> raise (Error_exn e)

let index t = t.index

let degree t = t.degree

let data t = t.data
