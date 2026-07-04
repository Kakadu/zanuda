let xs : (module LINT.TYPED) list ref = ref []
let all () = !xs
let add x = xs := x :: !xs
