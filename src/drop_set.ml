include Set.Make (struct
  type t = Drop.t

  let compare = Drop.compare
end)
