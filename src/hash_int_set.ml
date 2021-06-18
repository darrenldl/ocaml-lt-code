type t = {
  seed : int;
  buckets : Int_set.t array;
}

let hash (t : t) (x : int) =
  let r =
  abs (t.seed lxor x)
  land
  max_int
  in
  assert (r >= 0);
  r mod Array.length t.buckets

let create size : t =
  {
    seed = Rand.gen_int_global max_int;
  buckets = Array.make size Int_set.empty;
}

let add (t : t) (x : int) : unit =
  let i = hash t x in
  t.buckets.(i) <-
    Int_set.add x t.buckets.(i)

let mem (t : t) (x : int) : bool =
  let i = hash t x in
  Int_set.mem x t.buckets.(i)

let remove (t : t) x =
  let i = hash t x in
  t.buckets.(i) <-
    Int_set.remove x t.buckets.(i)

let cardinal (t : t) =
  Array.fold_left (fun c s ->
    c + Int_set.cardinal s
    )
  0
  t.buckets

let iter (f : int -> unit) (t : t) : unit =
  Array.iter (fun s ->
    Int_set.iter f s
) t.buckets

let reset (t : t) =
  for i=0 to Array.length t.buckets -1 do
    t.buckets.(i) <- Int_set.empty
  done

let choose_opt t =
  let rec aux cur len buckets =
    if cur < len then
      match Int_set.choose_opt buckets.(cur) with
    | None -> aux (succ cur) len buckets
    | Some x -> Some x
    else
      None
  in
  aux 0 (Array.length t.buckets) t.buckets

let choose t =
  Option.get (choose_opt t)
