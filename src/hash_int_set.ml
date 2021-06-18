let seed = Rand.gen_int_global max_int

let hash (x : int) =
  let r =
  abs (seed lxor x)
  land
  max_int
  in
  assert (r >= 0);
  r

module H = Hashtbl.Make (struct type t = int let equal = Int.equal let hash = hash end)

type t = unit H.t

let create size : t =
  H.create size

let add (t : t) (x : int) : unit =
  H.add t x ()

let mem (t : t) (x : int) : bool =
  H.mem t x

let remove (t : t) x =
  H.remove t x

let cardinal (t : t) =
  H.length t

let iter (f : int -> unit) (t : t) : unit =
  H.iter (fun x () -> f x) t

let reset (t : t) =
  H.reset t

let choose_opt t =
  match H.to_seq_keys t () with
  | Seq.Nil -> None
  | Seq.Cons (x, _) -> Some x

let choose t =
  Option.get (choose_opt t)
