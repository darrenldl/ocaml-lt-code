module Ctx = Ctx
module Drop = Drop
module Drop_set = Drop_set

type encode_error = Fountain.Encode.error

let encode = Fountain.Encode.encode

type decode_error = Fountain.Decode.error

let decode = Fountain.Decode.decode
