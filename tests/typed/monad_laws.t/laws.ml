(* identity monad *)
let (>>=) x f = f x
let return x = x

let foo x = x >>= fun y -> return y
let _should_be_reported x = x >>= function x -> return x [@@warning "-27"]

[@@@warning "-unused-value-declaration"]

let not_to_be_reported x = x >>= fun y -> return x [@@warning "-27"]
let not_to_be_reported x = x >>= function y -> return x [@@warning "-27"]

