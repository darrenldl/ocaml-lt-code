open Ctypes
open Foreign

external xor_onto : int -> Cstruct.buffer -> Cstruct.buffer -> unit = "xor_onto_stub"

(* let xor_onto = *)
  (* foreign "xor_onto_stub" (int @-> ptr char @-> ptr char @-> returning void) *)

let memcpy = foreign "memcpy" (ptr char @-> ptr char @-> int @-> returning void)
