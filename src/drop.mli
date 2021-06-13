type t

type error =
  [ `Invalid_index
  | `Invalid_degree
  | `Invalid_data
  ]

val make : index:int -> degree:int -> data:bytes -> (t, error) result

val make_exn : index:int -> degree:int -> data:bytes -> t

val index : t -> int

val degree : t -> int

val data : t -> bytes
