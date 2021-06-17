open Ctypes
open Foreign

let xor_onto =
  foreign "xor_onto"
  (int @-> ptr uchar @-> ptr uchar @-> returning void)
