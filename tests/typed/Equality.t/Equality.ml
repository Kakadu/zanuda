let __ x = if x = None then 1 else 1
let __ x = if x = [] then 2 else 2
let __ x = if x = true then 3 else 3

type b = Boolean of bool
let __ xx y (>>=) =
  xx >>= fun x ->
  (match x = Boolean true with
        | true -> y
        | false -> y)


let suffix_is_always_false x ~suffix =
  x == (1::suffix)