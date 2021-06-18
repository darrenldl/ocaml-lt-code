type t = Int_set.t array

let hash (t : t) (x : int) =
  let r =
  abs (
  (Rand.gen_int_global max_int)
  lxor x
)
  land
  max_int
  in
  assert (r >= 0);
  r mod Array.length t

let create size =
  Array.make size Int_set.empty

let add (t : t) (x : int) : unit =
  let i = hash t x in
  t.(i) <-
    Int_set.add x t.(i)

let mem (t : t) (x : int) : bool =
  let i = hash t x in
  Int_set.mem x t.(i)

let remove (t : t) x =
  let i = hash t x in
  t.(i) <-
    Int_set.remove x t.(i)

let cardinal t =
  Array.fold_left (fun c s ->
    c + Int_set.cardinal s
    )
  0
  t

let iter (f : int -> unit) t : unit =
  Array.iter (fun s ->
    Int_set.iter f s
) t

let reset t =
  for i=0 to Array.length t-1 do
    t.(i) <- Int_set.empty
  done

let choose_opt t =
  let rec aux cur len t =
    if cur < len then
      match Int_set.choose_opt t.(cur) with
    | None -> aux (succ cur) len t
    | Some x -> Some x
    else
      None
  in
  aux 0 (Array.length t) t

let choose t =
  Option.get (choose_opt t)
