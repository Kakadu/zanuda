(* identity monad *)
let (>>=) x f = f x
let return x = x

let foo x = x >>= fun y -> return y
