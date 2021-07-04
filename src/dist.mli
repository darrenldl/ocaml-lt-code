type t

val robust_soliton_dist : k:int -> t

val choose_n : ?seed:int -> t -> int -> int array

val choose_onto : ?seed:int -> ?offset:int -> t -> int array -> unit
