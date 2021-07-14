open Ctypes
open Foreign

let xor_onto =
  foreign "xor_onto"
  (int @-> ptr char @-> ptr char @-> returning void)

let memcpy =
  foreign "memcpy"
  (ptr char @-> ptr char @-> int @-> returning void)
